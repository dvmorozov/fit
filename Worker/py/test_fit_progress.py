# SPDX-License-Identifier: GPL-3.0-or-later
"""A fit on this sidecar reporting its progress while it runs.

WHY. The desktop shows a live loss curve, or the model moving, for every fit -
whichever engine runs it. This sidecar used to say nothing until its reply came
back, so a fit on it showed an empty chart for its whole length. Given a
``progressId``, a fit now publishes the best parameters it has reached, and
``GET /fit/progress?id=`` answers with them while the POST is still out.

The shape published is the outcome's own, so the Pascal relay reads a progress
reply exactly as it reads the final one.
"""

from __future__ import annotations

import json
import threading
import urllib.request

import numpy as np
import pytest
from http.server import ThreadingHTTPServer

import fit_backend
import fitting
import progress_store
from fit_backend import Handler

EXPR_GAUSSIAN = "A*exp(-(x-x0)**2/(2*sigma**2))"


def _problem(progress_id=None):
    x = np.linspace(0, 20, 81)
    y = 100 * np.exp(-(x - 10) ** 2 / (2 * 1.5 ** 2))
    problem = {
        "profileX": x.tolist(),
        "profileY": y.tolist(),
        "expression": EXPR_GAUSSIAN,
        "curves": [{"params": [
            {"name": "A", "value": 80.0},
            {"name": "x0", "value": 9.5},
            {"name": "sigma", "value": 1.0},
        ]}],
        "weighting": "none",
    }
    if progress_id is not None:
        problem["progressId"] = progress_id
    return problem


# ------------------------------------------------------------------- the store

def test_nothing_is_known_about_an_id_never_published():
    assert progress_store.latest("never-published") is None


def test_the_latest_publication_is_what_is_read():
    progress_store.publish("s1", {"rFactor": 0.5})
    progress_store.publish("s1", {"rFactor": 0.25})
    try:
        assert progress_store.latest("s1") == {"rFactor": 0.25}
    finally:
        progress_store.discard("s1")


def test_a_discarded_id_is_forgotten():
    progress_store.publish("s2", {"rFactor": 0.5})
    progress_store.discard("s2")
    assert progress_store.latest("s2") is None


def test_discarding_what_was_never_there_is_harmless():
    progress_store.discard("nobody")


def test_one_fits_progress_is_not_anothers():
    progress_store.publish("a", {"rFactor": 0.5})
    progress_store.publish("b", {"rFactor": 0.9})
    try:
        assert progress_store.latest("a") == {"rFactor": 0.5}
    finally:
        progress_store.discard("a")
        progress_store.discard("b")


# ---------------------------------------------------------------- the reporter

class _Clock:
    def __init__(self):
        self.now = 0.0

    def __call__(self):
        return self.now


def _params(A=1.0, x0=2.0, sigma=3.0):
    from lmfit import Parameters
    p = Parameters()
    p.add("c0_A", value=A)
    p.add("c0_x0", value=x0)
    p.add("c0_sigma", value=sigma)
    return p


#  As _seed_curves produces them: every spec says whether it is shared.
SEEDS = [[{"name": "A", "shared": False}, {"name": "x0", "shared": False},
          {"name": "sigma", "shared": False}]]


def test_the_first_improvement_is_published():
    published = []
    report = fitting._progress_reporter(SEEDS, published.append, clock=_Clock())
    assert report(_params(A=5.0), 1, np.array([1.0, 1.0])) is None
    assert len(published) == 1
    assert published[0]["curves"][0]["params"][0] == {"name": "A", "value": 5.0,
                                                     "error": -1.0}


def test_only_an_improvement_is_published():
    published = []
    clock = _Clock()
    report = fitting._progress_reporter(SEEDS, published.append, clock=clock)
    report(_params(), 1, np.array([1.0]))
    clock.now = 10.0
    report(_params(), 2, np.array([2.0]))
    assert len(published) == 1


def test_improvements_are_rationed():
    #  A least-squares solver evaluates thousands of times a second; the
    #  desktop reads four times a second.
    published = []
    clock = _Clock()
    report = fitting._progress_reporter(SEEDS, published.append, clock=clock)
    report(_params(), 1, np.array([3.0]))
    clock.now = fitting.PROGRESS_INTERVAL_S / 2
    report(_params(), 2, np.array([2.0]))
    assert len(published) == 1
    clock.now = fitting.PROGRESS_INTERVAL_S * 2
    report(_params(), 3, np.array([1.0]))
    assert len(published) == 2


def test_a_non_finite_residual_is_not_an_improvement():
    published = []
    report = fitting._progress_reporter(SEEDS, published.append, clock=_Clock())
    report(_params(), 1, np.array([np.nan]))
    assert published == []


def test_what_is_published_is_strict_json():
    published = []
    report = fitting._progress_reporter(SEEDS, published.append, clock=_Clock())
    report(_params(A=float("inf")), 1, np.array([1.0]))
    json.dumps(published[0], allow_nan=False)


# --------------------------------------------------------------------- the fit

def test_a_fit_given_somewhere_to_report_reports():
    published = []
    out = fitting.fit_problem(_problem(), progress=published.append)
    assert published, "the fit published nothing along the way"
    assert len(published[-1]["curves"]) == len(out["curves"])


def test_a_fit_given_nowhere_to_report_runs_as_before():
    out = fitting.fit_problem(_problem())
    assert out["errorCode"] == 0


def test_the_endpoint_publishes_under_the_fits_id_and_forgets_it_after(monkeypatch):
    seen = []
    real = progress_store.publish
    monkeypatch.setattr(progress_store, "publish",
                        lambda pid, o: (seen.append(pid), real(pid, o)))
    fit_backend._fit(_problem(progress_id="fit-7"))
    assert seen and set(seen) == {"fit-7"}
    assert progress_store.latest("fit-7") is None


def test_the_endpoint_forgets_the_id_even_when_the_fit_fails():
    progress_store.publish("fit-8", {"rFactor": 1.0})
    bad = _problem(progress_id="fit-8")
    bad["expression"] = ""
    with pytest.raises(Exception):
        fit_backend._fit(bad)
    assert progress_store.latest("fit-8") is None


# ------------------------------------------------------------------ over HTTP

@pytest.fixture
def server():
    srv = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    t = threading.Thread(target=srv.serve_forever, daemon=True)
    t.start()
    yield "http://127.0.0.1:%d" % srv.server_address[1]
    srv.shutdown()
    srv.server_close()


def _get(url):
    with urllib.request.urlopen(url, timeout=5) as r:
        return r.status, json.loads(r.read())


def test_an_id_with_nothing_yet_is_answered_not_refused(server):
    #  Not 404: the first poll usually arrives before the first improvement, and
    #  404 is how a sidecar WITHOUT this route answers, which stops the polling.
    status, body = _get(server + "/fit/progress?id=nothing-yet")
    assert status == 200
    assert body["ok"] is True and body["found"] is False and body["curves"] == []


def test_a_published_outcome_is_answered(server):
    progress_store.publish("h1", {"errorCode": 0, "rFactor": 0.5, "curves": [
        {"params": [{"name": "A", "value": 1.0, "error": -1.0}]}]})
    try:
        status, body = _get(server + "/fit/progress?id=h1")
    finally:
        progress_store.discard("h1")
    assert status == 200
    assert body["found"] is True
    assert body["curves"][0]["params"][0]["value"] == 1.0


def test_no_id_is_nothing_found(server):
    status, body = _get(server + "/fit/progress")
    assert status == 200 and body["found"] is False


def test_a_route_without_a_query_is_still_given_none(server):
    #  What every GET handler was given before queries were read, and what a
    #  module's handler may still expect.
    status, body = _get(server + "/health")
    assert status == 200 and body["ok"] is True
