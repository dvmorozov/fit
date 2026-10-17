# SPDX-License-Identifier: GPL-3.0-or-later
"""Which routes this sidecar answers, and who answers them.

WHAT THIS REPLACES: two if-chains in the request handler, one per method, with
every route's name written into them. A module's route therefore had to be added
to a file the module does not own - and `fit_backend` had to import that module
just to declare it, so the generic sidecar could not start without it.

A handler takes the decoded request body and returns the dict to send back. It
raises to reject; the caller turns that into a 400 with the message, so a
handler never formats an error envelope itself.
"""

from typing import Callable, Dict

#  Path -> handler. Plain dicts: the registry is small, and a route table that
#  can be printed is worth more here than any abstraction over it.
GET_ROUTES: Dict[str, Callable] = {}
POST_ROUTES: Dict[str, Callable] = {}


def _register(table: Dict[str, Callable], path: str, handler: Callable) -> None:
    if not path.startswith("/"):
        raise ValueError(f"route {path!r} must start with '/'")
    if path in table:
        #  Two handlers for one path would be resolved by import order, and the
        #  loser would be a route that looks installed and never runs.
        raise ValueError(f"route {path!r} is already registered")
    table[path] = handler


def get(path: str):
    """Registers a GET handler:  @routes.get("/health")."""
    def decorate(handler: Callable) -> Callable:
        _register(GET_ROUTES, path, handler)
        return handler
    return decorate


def post(path: str):
    """Registers a POST handler:  @routes.post("/fit")."""
    def decorate(handler: Callable) -> Callable:
        _register(POST_ROUTES, path, handler)
        return handler
    return decorate


def known() -> str:
    """Every route, for an error that says what could have been asked instead."""
    names = sorted(list(GET_ROUTES) + list(POST_ROUTES))
    return ", ".join(names) if names else "(none)"
