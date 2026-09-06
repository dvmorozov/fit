# SPDX-License-Identifier: GPL-3.0-or-later
"""Tests for the Python compute sidecar.

The sidecar is model-agnostic: the curve formula arrives on the wire as a text
`expression` in `x` and the curve's parameter names, and the seed parameter
values arrive per curve. These tests drive built-in and multi-branch shapes
purely through that generic path - no curve type is hard-coded in the backend.
"""

import json
import logging
import os
import sys
import threading
import urllib.error
import urllib.request

import numpy as np
import pytest

import fit_backend
import routes
from fit_backend import Handler
import fitting
from fitting import fit_problem
from lineshapes import Model
from http.server import ThreadingHTTPServer

#  Canonical built-in expressions, matching the native Pascal engine
#  (fitminimizers/SimpMath.pas). The Pascal side is the source of truth
#  (see the *_points_set.pas GetCurveExpression overrides); these copies let the
#  generic engine be exercised without a Pascal build.
EXPR_GAUSSIAN = "A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))"
EXPR_LORENTZIAN = "A*(1/(pi*sigma/2))*(1/(1+((x-x0)/(sigma/2))**2))"
EXPR_PSEUDO_VOIGT = (
    "A*((1-eta)*(2*sqrt(log(2))/(sigma*sqrt(pi))*exp(-4*log(2)*(x0-x)**2/sigma**2))"
    "+eta*((2/(pi*sigma))*(1/(1+(2*(x-x0)/sigma)**2))))"
)
EXPR_ASYM_PSEUDO_VOIGT = (
    "where(x>=x0,"
    "A*((1-eta)*exp(-4*log(2)*(x0-x)**2/(sigma+deltasigma)**2)"
    "+eta*(1/(1+(2*(x-x0)/(sigma+deltasigma))**2))),"
    "A*((1-eta)*exp(-4*log(2)*(x0-x)**2/(sigma-deltasigma)**2)"
    "+eta*(1/(1+(2*(x-x0)/(sigma-deltasigma))**2))))"
)
EXPR_TWO_BRANCHES_PV = (
    "where(x>=x0,"
    "A*((1-etaright)*exp(-4*log(2)*(x0-x)**2/sigmaright**2)"
    "+etaright*(1/(1+(2*(x-x0)/sigmaright)**2))),"
    "A*((1-eta)*exp(-4*log(2)*(x0-x)**2/sigma**2)"
    "+eta*(1/(1+(2*(x-x0)/sigma)**2))))"
)


def _curve(**params):
    """A seed curve for the wire: parameter names and starting values (all varied,
    none shared)."""
    return {"params": [{"name": n, "value": float(v)} for n, v in params.items()]}


def _p(name, value, vary=True, shared=False):
    return {"name": name, "value": float(value), "vary": vary, "shared": shared}


def _problem(x, y, expression, curves, **extra):
    p = {
        "profileX": list(x),
        "profileY": list(y),
        "expression": expression,
        "curves": curves,
    }
    p.update(extra)
    return p


def _synthetic(expression, curves, xmax=20.0, step=0.2, noise=0.0, seed=0):
    """Sample a model over a grid so a test can then recover its parameters."""
    x = np.arange(0.0, xmax + 1e-4, step)
    model = Model(expression)
    y = np.zeros_like(x)
    for c in curves:
        y = y + model(x, {p["name"]: p["value"] for p in c["params"]})
    if noise:
        y = y + np.random.default_rng(seed).normal(0.0, noise, size=y.shape)
    return x, y


def _params(curve):
    return {p["name"]: p["value"] for p in curve["params"]}


def test_recovers_a_clean_gaussian():
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth)
    seed = [_curve(A=80.0, x0=9.5, sigma=1.0)]
    out = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed))

    assert out["errorCode"] == 0
    p = _params(out["curves"][0])
    assert p["A"] == pytest.approx(100.0, rel=1e-3)
    assert p["sigma"] == pytest.approx(1.5, rel=1e-3)
    assert p["x0"] == pytest.approx(10.0, rel=1e-4)


def test_reports_statistics_and_errors():
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth, noise=2.0, seed=1)
    seed = [_curve(A=80.0, x0=9.5, sigma=1.0)]
    out = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed))

    stats = out["statistics"]
    assert stats["reducedChiSquare"] > 0
    assert 0.9 < stats["rSquared"] <= 1.0
    assert "aic" in stats and "bic" in stats
    assert stats["dataPoints"] == len(x)
    for p in out["curves"][0]["params"]:
        assert p["error"] >= 0.0


def test_two_overlapping_peaks_are_separated():
    truth = [_curve(A=80.0, x0=12.0, sigma=1.2), _curve(A=120.0, x0=16.0, sigma=1.0)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth, xmax=30.0, step=0.1)
    seed = [_curve(A=70.0, x0=12.3, sigma=1.5), _curve(A=110.0, x0=15.7, sigma=1.5)]
    out = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed))
    x0s = sorted(_params(c)["x0"] for c in out["curves"])
    assert x0s[0] == pytest.approx(12.0, abs=0.05)
    assert x0s[1] == pytest.approx(16.0, abs=0.05)


def test_lorentzian_and_pseudovoigt_are_supported():
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5, eta=0.5)]
    for expr, seed in (
        (EXPR_LORENTZIAN, [_curve(A=80.0, x0=9.5, sigma=1.0)]),
        (EXPR_PSEUDO_VOIGT, [_curve(A=80.0, x0=9.5, sigma=1.0, eta=0.4)]),
    ):
        x, y = _synthetic(expr, truth if "eta" in expr else
                          [_curve(A=100.0, x0=10.0, sigma=1.5)])
        out = fit_problem(_problem(x, y, expr, seed))
        assert out["errorCode"] == 0
        assert out["curves"]


def test_two_branches_pseudo_voigt_is_supported():
    """The curve from the bug report: independent left/right sigma and eta."""
    truth = [
        _curve(A=100.0, x0=10.0, sigma=1.5, eta=0.5, sigmaright=2.5, etaright=0.3)
    ]
    x, y = _synthetic(EXPR_TWO_BRANCHES_PV, truth)
    seed = [
        _curve(A=80.0, x0=9.8, sigma=1.2, eta=0.5, sigmaright=2.0, etaright=0.5)
    ]
    out = fit_problem(_problem(x, y, EXPR_TWO_BRANCHES_PV, seed))

    assert out["errorCode"] == 0
    p = _params(out["curves"][0])
    assert p["sigma"] == pytest.approx(1.5, rel=1e-2)
    assert p["sigmaright"] == pytest.approx(2.5, rel=1e-2)
    assert p["x0"] == pytest.approx(10.0, abs=0.05)


def test_more_parameters_than_data_points_still_fits():
    """M<N: many curves in a short window give more free params than residuals.
    MINPACK leastsq rejects this; the native Downhill Simplex does not, so the
    Python backend must handle it too (trf solver)."""
    #  3 curves x 6 params = 18 free params over a 13-point window (M<N), kept
    #  small so the trf solve stays fast; leastsq rejects it regardless of size.
    truth = [
        _curve(A=50.0, x0=float(c), sigma=1.5, eta=0.4, sigmaright=2.0, etaright=0.3)
        for c in (2, 3, 4)
    ]
    x, y = _synthetic(EXPR_TWO_BRANCHES_PV, truth, xmax=6.0, step=0.5)  # 13 points
    assert len(x) < 6 * len(truth)  # genuinely underdetermined
    seed = [
        _curve(A=40.0, x0=float(c), sigma=1.2, eta=0.5, sigmaright=1.8, etaright=0.5)
        for c in (2, 3, 4)
    ]
    out = fit_problem(_problem(x, y, EXPR_TWO_BRANCHES_PV, seed))
    assert out["errorCode"] == 0
    assert len(out["curves"]) == len(truth)


#  Reference values from the native Pascal engine (SimpMath.pas), one row per x
#  in 6..14 step 1, columns: Gaussian, Lorentzian, Pseudo-Voigt, Asym PV, 2-br PV.
#  Params: A=100, sigma=1.5, eta=0.4, x0=10; asym deltasigma=0.5; 2-br
#  sigmaright=2.5, etaright=0.3. Regenerate by evaluating the *Point functions.
_SIMPMATH_REFERENCE = [
    (6.0, 0.75973240, 1.44140326, 0.57656141, 0.61538462, 1.35849073),
    (7.0, 3.59939777, 2.49654813, 0.99919264, 1.08108108, 2.35385670),
    (8.0, 10.93400498, 5.23249128, 2.36480939, 2.35385670, 5.36551062),
    (9.0, 21.29653370, 15.27887454, 17.07035848, 11.75000000, 31.89793559),
    (10.0, 26.59615203, 42.44131816, 54.55401841, 100.00000000, 100.00000000),
    (11.0, 21.29653370, 15.27887454, 17.07035848, 50.00000000, 63.21258934),
    (12.0, 10.93400498, 5.23249128, 2.36480939, 11.75000000, 20.29725416),
    (13.0, 3.59939777, 2.49654813, 0.99919264, 4.11718750, 5.72958055),
    (14.0, 0.75973240, 1.44140326, 0.57656141, 2.35385670, 2.72692213),
]


def test_expressions_match_the_native_engine():
    """Every built-in expression must reproduce SimpMath.pas numerically - this is
    what guarantees a Python fit and a native fit describe the same curve."""
    ref = np.array(_SIMPMATH_REFERENCE)
    x = ref[:, 0]
    common = {"A": 100.0, "sigma": 1.5, "x0": 10.0}
    cases = [
        (EXPR_GAUSSIAN, common, 1),
        (EXPR_LORENTZIAN, common, 2),
        (EXPR_PSEUDO_VOIGT, {**common, "eta": 0.4}, 3),
        (EXPR_ASYM_PSEUDO_VOIGT, {**common, "eta": 0.4, "deltasigma": 0.5}, 4),
        (
            EXPR_TWO_BRANCHES_PV,
            {**common, "eta": 0.4, "sigmaright": 2.5, "etaright": 0.3},
            5,
        ),
    ]
    for expr, params, col in cases:
        got = Model(expr)(x, params)
        assert np.allclose(got, ref[:, col], atol=1e-6), expr


def test_parameter_bounds_are_honored():
    """A max bound must cap the fit, so it stays in the physical region the native
    engine enforces (e.g. eta<=1) instead of wandering out and being clamped on
    readback."""
    x = np.arange(0.0, 10.0, 0.5)
    y = 5.0 * x  # best unbounded slope is 5
    seed = [{"params": [{"name": "A", "value": 1.0, "min": 0.0, "max": 2.0}]}]
    out = fit_problem(_problem(x, y, "A*x", seed))
    a = _params(out["curves"][0])["A"]
    assert a == pytest.approx(2.0, abs=1e-6)  # pinned at the max bound, not 5


def test_degenerate_bounds_hold_the_parameter_fixed():
    """min == max means the native window allows no movement (as the placement
    window often does for x0), so the parameter is effectively fixed. lmfit
    rejects min == max, so it must be held instead."""
    x = np.arange(0.0, 10.0, 0.5)
    y = 5.0 * x + 2.0
    #  'A' is pinned by a degenerate bound; 'B' stays free so there is a fit to do.
    seed = [{"params": [
        {"name": "A", "value": 3.0, "min": 3.0, "max": 3.0},
        {"name": "B", "value": 0.0},
    ]}]
    out = fit_problem(_problem(x, y, "A*x+B", seed))
    p = _params(out["curves"][0])
    assert p["A"] == pytest.approx(3.0, abs=1e-12)   # held at the degenerate bound
    assert p["B"] != pytest.approx(0.0, abs=1e-6)    # the free one did move


def test_fit_with_no_free_parameters_is_rejected():
    x = np.arange(0.0, 10.0, 0.5)
    seed = [{"params": [{"name": "A", "value": 1.0, "vary": False}]}]
    with pytest.raises(ValueError):
        fit_problem(_problem(x, 5.0 * x, "A*x", seed))


def test_fixed_parameter_stays_at_its_seed():
    """A vary=False parameter must be held exactly, like the native engine."""
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth)
    #  Seed x0 off the true peak but pin it: the fit must not move it.
    seed = [{"params": [_p("A", 80.0), _p("x0", 9.0, vary=False), _p("sigma", 1.0)]}]
    out = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed))
    assert out["errorCode"] == 0
    assert _params(out["curves"][0])["x0"] == pytest.approx(9.0, abs=1e-9)


def test_shared_parameter_is_tied_across_curves():
    """A shared parameter yields one value common to every curve instance."""
    #  Two Gaussians generated with the SAME sigma; fit sigma as shared.
    truth = [_curve(A=100.0, x0=8.0, sigma=1.3), _curve(A=60.0, x0=14.0, sigma=1.3)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth, xmax=22.0, step=0.1)
    seed = [
        {"params": [_p("A", 90.0), _p("x0", 8.2), _p("sigma", 1.0, shared=True)]},
        {"params": [_p("A", 70.0), _p("x0", 13.8), _p("sigma", 1.0, shared=True)]},
    ]
    out = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed))
    assert out["errorCode"] == 0
    s0 = _params(out["curves"][0])["sigma"]
    s1 = _params(out["curves"][1])["sigma"]
    assert s0 == pytest.approx(s1, abs=1e-9)   # tied: identical
    assert s0 == pytest.approx(1.3, rel=1e-2)  # and recovered


def _load_fidelity_cases():
    """The Pascal->numpy translation cases shared with testcase_expr_fidelity.pas.
    Each row: pascal ;; numpy ;; name=val,... ;; expected_native_value."""
    path = os.path.join(os.path.dirname(__file__), "..", "..", "tests",
                        "expr_fidelity_cases.txt")
    cases = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            pascal, numpy, params, expected = (p.strip() for p in line.split(" ;; "))
            values = {}
            if params:
                for pair in params.split(","):
                    name, val = pair.split("=")
                    values[name.strip()] = float(val)
            cases.append((numpy, values, float(expected)))
    return cases


_FIDELITY_CASES = _load_fidelity_cases()


@pytest.mark.parametrize("numpy_expr,values,expected", _FIDELITY_CASES,
                         ids=[c[0] for c in _FIDELITY_CASES])
def test_translated_expression_evaluates_to_the_native_value(numpy_expr, values,
                                                             expected):
    """The third leg of the translation guarantee: the sidecar's Model evaluates the
    numpy string to the same value the native engine produced for the Pascal source.
    Combined with testcase_expr_fidelity.pas (native == expected, transpile == numpy)
    this proves native(pascal) == numpy(translate(pascal)) for the whole surface."""
    x = np.zeros(1)  # scalar model; x is bound but only used if the expr names it
    got = Model(numpy_expr)(x, values)
    assert float(np.ravel(got)[0]) == pytest.approx(expected, abs=1e-6)


def test_fidelity_fixture_is_not_empty():
    assert len(_FIDELITY_CASES) >= 20   # guard against a silently empty fixture


def test_transpiled_user_expression_matches_native_engine():
    """A user formula transpiled from native_math_expr (`^`→`**`, `ln`→`log`,
    `log`→`log10`, `sqr`→`square`) must evaluate exactly as the Pascal engine.
    Reference values are from native_math_expr.ParseAndCalcExpression on
    `A*exp(-((x-x0)/w)^2)+B*ln(sqr(x)+1)` at x=6..14, A=100 x0=10 w=1.5 B=3."""
    transpiled = "A*exp(-((x-x0)/w)**2)+B*log(square(x)+1)"
    x = np.arange(6.0, 14.0 + 1e-9, 1.0)
    native = np.array([
        10.91435252, 13.56763291, 29.42449335, 77.33819658, 113.84536155,
        78.53010198, 31.83153277, 17.23895920, 15.93120997,
    ])
    got = Model(transpiled)(x, {"A": 100.0, "x0": 10.0, "w": 1.5, "B": 3.0})
    assert np.allclose(got, native, atol=1e-6)


def test_outcome_is_strict_json_even_when_underdetermined():
    """With more parameters than points the statistics can be NaN/Infinity. Those
    are not valid strict JSON and the Pascal client rejects them ("unreadable
    result"), so the outcome must carry only finite numbers."""
    truth = [
        _curve(A=50.0, x0=float(c), sigma=1.5, eta=0.4, sigmaright=2.0, etaright=0.3)
        for c in (2, 3, 4)
    ]
    x, y = _synthetic(EXPR_TWO_BRANCHES_PV, truth, xmax=6.0, step=0.5)
    seed = [
        _curve(A=40.0, x0=float(c), sigma=1.2, eta=0.5, sigmaright=1.8, etaright=0.5)
        for c in (2, 3, 4)
    ]
    out = fit_problem(_problem(x, y, EXPR_TWO_BRANCHES_PV, seed))
    #  allow_nan=False is exactly what a strict parser (fpjson) enforces.
    json.dumps(out, allow_nan=False)


def test_saturated_fit_is_bounded_and_returns():
    """A saturated model (free parameters ~ data points) must not grind forever:
    the solver budget is capped, so the fit always comes back. This is the
    regression for the 'app hangs with the Python minimizer' report."""
    from fitting import MAX_NFEV_PER_PARAM

    #  6 curves x 6 params over 25 points: more parameters than data.
    centers = [2.0, 4.0, 6.0, 8.0, 10.0, 12.0]
    truth = [
        _curve(A=50.0, x0=c, sigma=1.0, eta=0.4, sigmaright=1.2, etaright=0.3)
        for c in centers
    ]
    x, y = _synthetic(EXPR_TWO_BRANCHES_PV, truth, xmax=12.0, step=0.5)
    seed = [
        _curve(A=40.0, x0=c, sigma=0.8, eta=0.5, sigmaright=0.8, etaright=0.5)
        for c in centers
    ]
    out = fit_problem(_problem(x, y, EXPR_TWO_BRANCHES_PV, seed))
    assert out["errorCode"] == 0
    assert MAX_NFEV_PER_PARAM > 0   # the cap exists and is applied in fit_problem


def test_curves_with_different_parameter_names_still_fit():
    """Curves are normally evaluated in one broadcast call, which needs identical
    parameter names. When they differ the per-curve fallback must still fit."""
    x = np.arange(0.0, 10.0, 0.5)
    y = 3.0 * x
    #  Both curves evaluate the same expression, but the second carries an extra
    #  parameter, so the name sets differ and the broadcast path cannot be used.
    seed = [
        {"params": [{"name": "A", "value": 1.0}]},
        {"params": [{"name": "A", "value": 0.5}, {"name": "spare", "value": 2.0}]},
    ]
    out = fit_problem(_problem(x, y, "A*x", seed))
    assert out["errorCode"] == 0
    total = sum(_params(c)["A"] for c in out["curves"])
    assert total == pytest.approx(3.0, rel=1e-3)   # the two amplitudes sum to 3


def test_empty_expression_is_rejected():
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    with pytest.raises(ValueError):
        fit_problem(_problem(x, y, "", [_curve(A=1.0, x0=10.0, sigma=1.0)]))


def test_expression_with_unknown_symbol_is_rejected():
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    with pytest.raises(ValueError):
        fit_problem(_problem(x, y, "A*wobble(x)", [_curve(A=1.0)]))


def test_unsupported_function_gives_a_clear_actionable_message():
    """An fpexprpars function with no numpy equivalent (frac/int/round/...) fits
    natively but not here; the message must name it and point to the way out."""
    with pytest.raises(ValueError) as ei:
        Model("A*frac(x)")(np.zeros(1), {"A": 1.0})
    msg = str(ei.value)
    assert "'frac'" in msg                       # names the offending function
    assert "does not support" in msg
    assert "native minimizer" in msg             # actionable
    assert "sin, cos" in msg                      # lists supported functions


def test_non_name_evaluation_error_is_reported():
    """A runtime failure that is not an unknown name (here a scalar divide by zero)
    still becomes a clean ValueError, not a crash."""
    with pytest.raises(ValueError) as ei:
        Model("A/0")(np.zeros(1), {"A": 1.0})
    assert "cannot evaluate curve expression" in str(ei.value)


def test_http_unsupported_function_is_reported(server):
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    out = _post(server + "/fit", _problem(x, y, "A*frac(x)", [_curve(A=1.0)]))
    assert out["ok"] is False
    assert "frac" in out["error"] and "does not support" in out["error"]


def test_missing_parameter_in_seed_is_rejected():
    """The expression needs `sigma` but the seed curve omits it."""
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    with pytest.raises(ValueError):
        fit_problem(_problem(x, y, EXPR_GAUSSIAN, [_curve(A=1.0, x0=10.0)]))


def test_no_curves_is_rejected():
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    with pytest.raises(ValueError):
        fit_problem(_problem(x, y, EXPR_GAUSSIAN, []))


def test_model_constant_expression_broadcasts():
    """A parameter-only expression still returns a full-length array."""
    m = Model("A")
    out = m(np.array([1.0, 2.0, 3.0]), {"A": 5.0})
    assert out.shape == (3,)
    assert np.all(out == 5.0)


def test_syntactically_invalid_expression_is_rejected():
    with pytest.raises(ValueError):
        Model("A*(")


def test_unweighted_fit_is_supported():
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth)
    seed = [_curve(A=80.0, x0=9.5, sigma=1.0)]
    out = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed, weighting="none"))
    assert out["errorCode"] == 0
    assert _params(out["curves"][0])["A"] == pytest.approx(100.0, rel=1e-3)


def test_curve_without_parameters_is_rejected():
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    with pytest.raises(ValueError):
        fit_problem(_problem(x, y, EXPR_GAUSSIAN, [{"params": []}]))


def test_malformed_profile_is_rejected():
    with pytest.raises(ValueError):
        fit_problem(
            _problem([1.0, 2.0], [1.0], EXPR_GAUSSIAN, [_curve(A=1.0, x0=1.0, sigma=1.0)])
        )


def test_json_float_sanitizes_non_finite_and_non_numeric():
    from fitting import _json_float

    assert _json_float(1.5) == 1.5
    assert _json_float(float("nan")) == 0.0
    assert _json_float(float("inf"), -1.0) == -1.0
    assert _json_float("not a number", -1.0) == -1.0   # defensive path


def test_r_factor_is_zero_for_empty_signal():
    """sum|y| == 0 must not divide by zero."""
    from fitting import _r_factor

    zeros = np.zeros(5)
    assert _r_factor(zeros, zeros) == 0.0


@pytest.fixture
def server():
    srv = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    t = threading.Thread(target=srv.serve_forever, daemon=True)
    t.start()
    yield "http://127.0.0.1:%d" % srv.server_address[1]
    srv.shutdown()
    srv.server_close()


def _post(url, payload):
    req = urllib.request.Request(
        url, data=json.dumps(payload).encode(), headers={"Content-Type": "application/json"}
    )
    try:
        with urllib.request.urlopen(req, timeout=30) as r:
            return json.loads(r.read())
    except urllib.error.HTTPError as e:
        return json.loads(e.read())


def test_http_health(server):
    with urllib.request.urlopen(server + "/health", timeout=5) as r:
        health = json.loads(r.read())
    assert health["ok"] and health["backend"] == "python-lmfit"


def test_http_fit_end_to_end(server):
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth)
    seed = [_curve(A=80.0, x0=9.5, sigma=1.0)]
    out = _post(server + "/fit", _problem(x, y, EXPR_GAUSSIAN, seed))
    p = _params(out["curves"][0])
    assert p["A"] == pytest.approx(100.0, rel=1e-3)
    assert out["statistics"]["rSquared"] > 0.99


def test_http_bad_expression_reports_error(server):
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    out = _post(server + "/fit", _problem(x, y, "", [_curve(A=1.0)]))
    assert out["ok"] is False
    assert out["errorCode"] != 0
    assert "error" in out


def _get_status(url):
    try:
        with urllib.request.urlopen(url, timeout=5) as r:
            return r.status, json.loads(r.read())
    except urllib.error.HTTPError as e:
        return e.code, json.loads(e.read())


def test_http_unknown_get_route_is_404(server):
    status, body = _get_status(server + "/nope")
    assert status == 404
    assert body["ok"] is False


def test_http_unknown_post_route_is_404(server):
    out = _post(server + "/wrong", {})
    assert out["ok"] is False


def test_http_empty_post_body_is_rejected(server):
    #  No Content-Length body -> empty -> not valid JSON -> reported, not a crash.
    req = urllib.request.Request(server + "/fit", data=b"",
                                 headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=5) as r:
            out = json.loads(r.read())
    except urllib.error.HTTPError as e:
        out = json.loads(e.read())
    assert out["ok"] is False


#  --- fit_backend process-lifecycle helpers (not exercised by the HTTP server) ---


@pytest.fixture
def restore_root_logging():
    """Snapshot and restore the root logger, since _configure_logging reconfigures
    it globally (force=True) and would otherwise leak handlers into other tests."""
    root = logging.getLogger()
    saved_handlers, saved_level = root.handlers[:], root.level
    yield
    for h in root.handlers[:]:
        if h not in saved_handlers:
            h.close()
    root.handlers[:] = saved_handlers
    root.setLevel(saved_level)


def test_configure_logging_writes_to_a_file(tmp_path, restore_root_logging):
    log_file = tmp_path / "sidecar.log"
    fit_backend._configure_logging(str(log_file))
    logging.getLogger("fit").info("hello")
    for h in logging.getLogger().handlers:
        h.flush()
    assert log_file.exists()
    assert "hello" in log_file.read_text()


def test_configure_logging_without_a_file_uses_stderr_only(restore_root_logging):
    fit_backend._configure_logging("")
    kinds = [type(h) for h in logging.getLogger().handlers]
    assert logging.StreamHandler in kinds
    assert logging.FileHandler not in kinds


def test_configure_logging_survives_an_unopenable_file(tmp_path, capsys,
                                                       restore_root_logging):
    #  A directory path can't be opened as a log file; it must warn, not raise.
    fit_backend._configure_logging(str(tmp_path))
    assert "cannot open log file" in capsys.readouterr().err
    assert logging.FileHandler not in [type(h) for h in logging.getLogger().handlers]


def test_parent_alive_true_false_and_permission(monkeypatch):
    #  Our own process is alive.
    assert fit_backend._parent_alive(os.getpid()) is True

    def raise_(exc):
        def _f(*_a):
            raise exc
        return _f

    monkeypatch.setattr(fit_backend.os, "kill", raise_(ProcessLookupError))
    assert fit_backend._parent_alive(999999) is False
    #  Exists but not ours to signal -> treated as alive.
    monkeypatch.setattr(fit_backend.os, "kill", raise_(PermissionError))
    assert fit_backend._parent_alive(1) is True


def test_exit_when_orphaned_exits_once_parent_is_gone(monkeypatch):
    monkeypatch.setattr(fit_backend.time, "sleep", lambda _s: None)
    monkeypatch.setattr(fit_backend, "_parent_alive", lambda _pid: False)

    def fake_exit(code):
        raise SystemExit(code)

    monkeypatch.setattr(fit_backend.os, "_exit", fake_exit)
    with pytest.raises(SystemExit):
        fit_backend._exit_when_orphaned(4242, poll_seconds=0.0)


def test_main_starts_orphan_watch_and_shuts_down_cleanly(
        tmp_path, monkeypatch, restore_root_logging):
    started = {"orphan_watch": False}
    closed = {"server": False}

    class FakeThread:
        def __init__(self, target=None, args=(), daemon=None):
            pass

        def start(self):
            started["orphan_watch"] = True

    class FakeServer:
        def __init__(self, addr, handler):
            pass

        def serve_forever(self):
            raise KeyboardInterrupt   #  as Ctrl-C / shutdown would

        def server_close(self):
            closed["server"] = True

    monkeypatch.setattr(fit_backend.threading, "Thread", FakeThread)
    monkeypatch.setattr(fit_backend, "ThreadingHTTPServer", FakeServer)

    rc = fit_backend.main([
        "--host", "127.0.0.1", "--port", "0",
        "--parent-pid", str(os.getpid()),
        "--log-file", str(tmp_path / "log.txt"),
    ])

    assert rc == 0
    assert started["orphan_watch"] is True   #  --parent-pid -> watcher started
    assert closed["server"] is True          #  finally: server_close ran


def test_special_functions_come_from_scipy():
    """The special functions the native engine hand-writes (erf for EMG, the
    Faddeeva/Voigt profile for true Voigt) are provided by scipy - the trusted
    implementation, and the golden reference the Pascal side is pinned to."""
    from scipy import special

    x = np.linspace(-4.0, 4.0, 17)
    assert np.allclose(Model("erf(x)")(x, {}), special.erf(x))
    assert np.allclose(Model("erfc(x)")(x, {}), special.erfc(x))
    #  Voigt(x; sigma, gamma) == scipy.special.voigt_profile.
    v = Model("voigt(x-x0, sigma, gamma)")(x, {"x0": 0.5, "sigma": 1.2, "gamma": 0.7})
    assert np.allclose(v, special.voigt_profile(x - 0.5, 1.2, 0.7))
    #  gamma over a pole-free (positive) domain.
    xp = np.linspace(0.5, 5.0, 10)
    assert np.allclose(Model("gamma(x)")(xp, {}), special.gamma(xp))


#  ------------------------------------------------------------------ objectives
#
#  THE POINT OF THESE. Selecting an engine must change only the speed and the
#  uncertainties, never the question being asked. The Pascal side decides which
#  objectives may come here (LossIsLeastSquares in Server/fit_loss.pas); these
#  pin the other half of that contract, so the two cannot drift apart silently.


def test_least_squares_losses_are_accepted_and_agree():
    """R-factor and sum-of-squares differ only by the constant sum(obs)^2, so
    they must not merely both succeed - they must find the SAME minimum."""
    truth = [_curve(A=100.0, x0=10.0, sigma=1.5)]
    x, y = _synthetic(EXPR_GAUSSIAN, truth)

    got = []
    for kind in (fitting.LOSS_RFACTOR, fitting.LOSS_SUMSQ):
        out = fit_problem(
            _problem(x, y, EXPR_GAUSSIAN, [_curve(A=80.0, x0=9.5, sigma=1.0)],
                     lossKind=kind))
        assert out["errorCode"] == 0
        got.append(_params(out["curves"][0]))

    for name in ("A", "x0", "sigma"):
        assert got[0][name] == pytest.approx(got[1][name], rel=1e-6), (
            "a constant factor on the objective must not move the minimum")


def test_a_loss_this_solver_cannot_express_is_refused():
    """Refused, not approximated. Quietly minimising a different objective than
    the one asked for is the failure mode this whole contract exists to stop -
    it would look like a successful fit and be a different answer."""
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    for kind in (fitting.LOSS_RFACTOR_LEGACY, fitting.LOSS_RELATIVE):
        with pytest.raises(ValueError, match="least-squares"):
            fit_problem(
                _problem(x, y, EXPR_GAUSSIAN,
                         [_curve(A=80.0, x0=9.5, sigma=1.0)], lossKind=kind))


def test_an_absent_loss_field_means_the_corrected_r_factor():
    """A peer that predates the field must land on the objective we would have
    chosen, not on whatever happens to be numbered zero historically."""
    x, y = _synthetic(EXPR_GAUSSIAN, [_curve(A=100.0, x0=10.0, sigma=1.5)])
    seed = [_curve(A=80.0, x0=9.5, sigma=1.0)]
    absent = fit_problem(_problem(x, y, EXPR_GAUSSIAN, seed))
    explicit = fit_problem(
        _problem(x, y, EXPR_GAUSSIAN, seed, lossKind=fitting.LOSS_RFACTOR))
    assert absent["errorCode"] == 0
    assert _params(absent["curves"][0]) == pytest.approx(
        _params(explicit["curves"][0]), rel=1e-9)


def test_every_loss_code_is_named_and_classified():
    """Self-enforcing: a code added on the Pascal side without being mirrored
    here shows up as an unnamed loss, and this fails rather than the sidecar
    reporting it as 'unknown' at runtime."""
    codes = {fitting.LOSS_RFACTOR, fitting.LOSS_RFACTOR_LEGACY,
             fitting.LOSS_SUMSQ, fitting.LOSS_RELATIVE}
    assert set(fitting.LOSS_NAMES) == codes
    assert fitting.LEAST_SQUARES_LOSSES <= codes
    #  The refusal message names the loss, so a user can act on it.
    assert all(fitting.LOSS_NAMES[c] for c in codes)


#  ---------------------------------------------------------- the route table


def test_a_route_must_be_a_path():
    #  Registered at import time, so a malformed path would otherwise surface as
    #  a route nobody can reach rather than as an error where it was written.
    with pytest.raises(ValueError, match="must start with"):
        routes.post("decompose")(lambda body: {})


def test_a_route_cannot_be_claimed_twice():
    #  Which handler wins would otherwise depend on import order, and the loser
    #  would be a route that looks installed and never runs.
    with pytest.raises(ValueError, match="already registered"):
        routes.post("/fit")(lambda body: {})


def test_naming_no_module_loads_nothing():
    #  The public build passes an empty --modules, and that is an ordinary
    #  configuration rather than an error.
    fit_backend.load_module_routes("")
    fit_backend.load_module_routes(None)


def test_a_named_module_has_its_route_package_imported(tmp_path, monkeypatch):
    #  How a module reaches the sidecar at all: nothing here names one, so the
    #  proof has to be built by the test. A package called <name>_routes is
    #  imported for each name given, and registration is a side effect of that
    #  import - exactly what a real pack relies on.
    (tmp_path / "sample_routes.py").write_text(
        "import routes\n"
        "@routes.post('/sample/echo')\n"
        "def _echo(body):\n"
        "    return body\n",
        encoding="utf-8",
    )
    monkeypatch.syspath_prepend(str(tmp_path))
    fit_backend.load_module_routes("sample")
    assert "/sample/echo" in routes.POST_ROUTES
    #  Leave the table as it was found: it is process-global, and a stray entry
    #  would make a later test's "unknown route" list wrong.
    del routes.POST_ROUTES["/sample/echo"]


def test_a_module_is_found_in_a_repository_beside_this_one(tmp_path, monkeypatch):
    #  The development layout, and the reason this search exists: a module keeps
    #  no file in this repository, so its route package sits in a sibling
    #  checkout's Worker/py - never beside fit_backend.py.
    beside = tmp_path / "fit-sample" / "Worker" / "py"
    beside.mkdir(parents=True)
    (beside / "sibling_routes.py").write_text(
        "import routes\n"
        "from sibling_detail import PATH\n"
        "@routes.post(PATH)\n"
        "def _echo(body):\n"
        "    return body\n",
        encoding="utf-8",
    )
    #  Beside the route package, as a real pack's parser is: finding the package
    #  is only half the job if what it imports does not resolve too.
    (beside / "sibling_detail.py").write_text("PATH = '/sibling/echo'\n", encoding="utf-8")
    #  fit_backend pretends to live in the umbrella's own Worker/py.
    monkeypatch.setattr(
        fit_backend, "__file__", str(tmp_path / "fit" / "Worker" / "py" / "fit_backend.py")
    )
    saved = list(sys.path)
    try:
        fit_backend.load_module_routes("sibling")
        assert "/sibling/echo" in routes.POST_ROUTES
    finally:
        del routes.POST_ROUTES["/sibling/echo"]
        sys.modules.pop("sibling_routes", None)
        sys.modules.pop("sibling_detail", None)
        sys.path[:] = saved


def test_extra_directories_are_searched_when_given(tmp_path, monkeypatch):
    #  The escape hatch (--module-path / FIT_MODULE_PATH), for a layout that is
    #  neither "installed beside the script" nor "checked out beside the repo".
    (tmp_path / "elsewhere_routes.py").write_text(
        "import routes\n"
        "@routes.post('/elsewhere/echo')\n"
        "def _echo(body):\n"
        "    return body\n",
        encoding="utf-8",
    )
    saved = list(sys.path)
    try:
        fit_backend.load_module_routes("elsewhere", str(tmp_path))
        assert "/elsewhere/echo" in routes.POST_ROUTES
    finally:
        del routes.POST_ROUTES["/elsewhere/echo"]
        sys.modules.pop("elsewhere_routes", None)
        sys.path[:] = saved


def test_several_modules_are_loaded_from_one_list(tmp_path):
    #  --modules is a pathsep-separated LIST: a build can carry more than one
    #  pack, and each one's routes must arrive.
    for name in ("first", "second"):
        (tmp_path / f"{name}_routes.py").write_text(
            "import routes\n"
            f"@routes.post('/{name}/echo')\n"
            "def _echo(body):\n"
            "    return body\n",
            encoding="utf-8",
        )
    saved = list(sys.path)
    try:
        fit_backend.load_module_routes(os.pathsep.join(["first", "second"]), str(tmp_path))
        assert "/first/echo" in routes.POST_ROUTES
        assert "/second/echo" in routes.POST_ROUTES
    finally:
        for name in ("first", "second"):
            del routes.POST_ROUTES[f"/{name}/echo"]
            sys.modules.pop(f"{name}_routes", None)
        sys.path[:] = saved


def test_a_module_that_is_nowhere_says_where_it_looked(monkeypatch):
    #  Loud, not a 404 later: the caller asked for a build that has the module,
    #  and the message has to be actionable - hence the directories searched.
    with pytest.raises(ModuleNotFoundError, match="is in none of"):
        fit_backend.load_module_routes("definitely_absent")


def test_a_broken_import_inside_a_module_is_reported_as_itself(tmp_path):
    #  A pack whose route package IS there but whose own import is broken must
    #  not be reported as a missing pack - that would send the reader hunting
    #  for a file that exists.
    (tmp_path / "broken_routes.py").write_text("import no_such_library\n", encoding="utf-8")
    saved = list(sys.path)
    try:
        with pytest.raises(ModuleNotFoundError, match="no_such_library"):
            fit_backend.load_module_routes("broken", str(tmp_path))
    finally:
        sys.modules.pop("broken_routes", None)
        sys.path[:] = saved


def test_the_search_survives_an_unreadable_umbrella(monkeypatch, tmp_path):
    #  An installed sidecar is not in a working tree at all, so the walk up to a
    #  sibling checkout lands on nothing readable. That is an ordinary layout,
    #  not a failure.
    monkeypatch.setattr(fit_backend.os, "listdir", _raise_oserror)
    dirs = fit_backend.module_search_dirs(str(tmp_path))
    assert str(tmp_path) in dirs


def _raise_oserror(_path):
    raise OSError("no such directory")


def test_a_search_directory_is_never_listed_twice(tmp_path):
    #  The directories are printed when a module is not found; a repeated one
    #  would only make that message harder to read.
    twice = fit_backend.module_search_dirs(os.pathsep.join([str(tmp_path), str(tmp_path)]))
    assert twice.count(str(tmp_path)) == 1


#  ------------------------------------------------------------ unknown routes


def test_an_unknown_route_says_what_the_build_offers(server):
    #  A module's route exists only when that module is loaded, so "no such
    #  route" is an ordinary answer here rather than a defect - and for a typo
    #  in a script, which is where this is read, naming the alternatives is the
    #  other half of the answer.
    body = _post(server + "/no-such-thing", {})
    assert body["ok"] is False
    assert "/fit" in body["error"]
