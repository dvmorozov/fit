# SPDX-License-Identifier: GPL-3.0-or-later
"""The best outcome each running fit has reached, by the id its caller gave it.

Written by the thread running a fit, read by the thread answering
``GET /fit/progress`` - the HTTP server is threaded, so the POST that runs the fit
and the polls that watch it are served at the same time. Hence the lock, held for
a dict operation and nothing more.

An entry lives exactly as long as its fit: ``/fit`` discards it when the fit
returns, however it returns, so the store cannot grow for as long as the sidecar
runs.
"""

from __future__ import annotations

import threading

_lock = threading.Lock()
_latest: dict = {}


def publish(progress_id: str, outcome: dict) -> None:
    """Replaces what the fit ``progress_id`` has reached."""
    with _lock:
        _latest[progress_id] = outcome


def latest(progress_id: str):
    """What the fit has reached, or None when nothing has been published."""
    with _lock:
        return _latest.get(progress_id)


def discard(progress_id: str) -> None:
    """Forgets the fit; harmless when it was never there."""
    with _lock:
        _latest.pop(progress_id, None)
