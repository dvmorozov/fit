#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""The Python compute sidecar: an HTTP server exposing the same stateless fit
endpoint as the native ``fit_server``, backed by lmfit.

    GET  /health -> {"ok": true, "backend": "python-lmfit", "protocol": N}
    POST /fit    -> body is a fit-problem JSON; reply is the outcome JSON,
                    extended with per-parameter errors and fit statistics.
    GET  /fit/progress?id=<progressId>
                 -> while a /fit that carried that progressId runs, the best
                    outcome it has reached; {"found": false} before the first.

It is an independent process, started separately (a drop-in network worker under
decision D7), and needs no desktop or Lazarus. Run:

    python3 fit_backend.py [--host H] [--port N]      # defaults 127.0.0.1:8788
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sys
import importlib
import threading
import urllib.parse
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

#  Keep the numeric libraries single-threaded. A curve fit works on tiny matrices
#  (tens of parameters, tens of points), where BLAS/OpenMP spawning a thread per
#  core does not speed anything up but does saturate the machine - a runaway fit
#  was seen at 1277% CPU. Must be set before numpy is first imported, so it stays
#  above the `fitting` import.
for _var in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
             "NUMEXPR_NUM_THREADS", "VECLIB_MAXIMUM_THREADS"):
    os.environ.setdefault(_var, "1")

import lifeline  # noqa: E402

#  BEFORE THE SLOW IMPORTS BELOW. A sidecar that dies importing numpy, scipy or
#  lmfit closes this connection, and fit_server stops waiting at once instead of
#  waiting out its whole budget for a process that is already gone.
_EARLY_PARENT = lifeline.connect_from_argv(sys.argv)

from fitting import fit_problem  # noqa: E402
import progress_store  # noqa: E402
import routes  # noqa: E402


@routes.get("/health")
def _health(_body):
    return {"ok": True, "backend": BACKEND_NAME, "protocol": PROTOCOL_VERSION}


@routes.post("/fit")
def _fit(body):
    progress_id = body.get("progressId") if isinstance(body, dict) else None
    if not progress_id:
        return fit_problem(body)
    try:
        return fit_problem(body, progress=lambda outcome:
                           progress_store.publish(progress_id, outcome))
    finally:
        #  However the fit ends: an entry per failed fit would be a leak for as
        #  long as the sidecar runs.
        progress_store.discard(progress_id)


@routes.get("/fit/progress")
def _fit_progress(query):
    #  NEVER 404 FOR AN ID NOT SEEN: the first poll usually arrives before the
    #  first improvement, and 404 is how a sidecar without this route answers -
    #  which is what tells the caller to stop asking.
    progress_id = (query or {}).get("id", "")
    reached = progress_store.latest(progress_id) if progress_id else None
    if reached is None:
        return {"ok": True, "found": False, "curves": []}
    return dict(reached, ok=True, found=True)


#  Where a module's sidecar files may sit. A module keeps NO file in this
#  repository - that is the whole arrangement - so its route package is not
#  beside this script in a working tree, and the import needs to be told where
#  to look. Two layouts answer it without configuration, and FIT_MODULE_PATH
#  (or --module-path) is the escape hatch for a third.
def module_search_dirs(extra: str = "") -> list[str]:
    """The directories searched for a module's ``<name>_routes.py``, in order.

    This script's own directory first - an installed layout puts every sidecar
    file in one place - then the directories named explicitly, then each module
    repository checked out beside this one, which is the development layout.
    """
    here = os.path.dirname(os.path.abspath(__file__))
    dirs = [here]
    for d in (extra or "").split(os.pathsep):
        d = d.strip()
        if d:
            dirs.append(os.path.abspath(d))
    #  Worker/py -> Worker -> the repository -> the umbrella holding them all.
    umbrella = os.path.dirname(os.path.dirname(os.path.dirname(here)))
    try:
        siblings = sorted(os.listdir(umbrella))
    except OSError:
        siblings = []   # not a working tree; the layouts above still apply
    for name in siblings:
        candidate = os.path.join(umbrella, name, "Worker", "py")
        if os.path.isdir(candidate):
            dirs.append(candidate)
    #  Duplicates would only make the diagnostic below repeat itself.
    seen: set[str] = set()
    return [d for d in dirs if not (d in seen or seen.add(d))]


#  A module's routes are imported from the packages FIT_MODULES names, so this
#  file declares only what the framework itself answers. Absent or empty - the
#  public build - simply means no module routes exist, which is not an error.
def load_module_routes(spec: str, module_path: str = "") -> None:
    """Imports each named pack's route package.

    Registration happens as a side effect of the import: a route package
    decorates its handlers with routes.get/post. Nothing named means no module
    routes, which is the public build rather than an error.

    A name that IS given and cannot be found is fatal, and says where it looked:
    the caller asked for a build that has the module, and a sidecar answering
    404 for its routes would be that build failing silently.
    """
    dirs: list[str] | None = None
    for name in (spec or "").split(os.pathsep):
        name = name.strip()
        if not name:
            continue
        package = f"{name}_routes"
        if dirs is None:
            dirs = module_search_dirs(module_path)
        #  The package's own directory, not just the file: a route package
        #  imports the rest of its pack (its parser, its model) from beside
        #  itself, and those must resolve too.
        for d in dirs:
            if os.path.isfile(os.path.join(d, f"{package}.py")) and d not in sys.path:
                sys.path.append(d)
        try:
            importlib.import_module(package)
        except ModuleNotFoundError as e:
            if e.name != package:
                raise
            raise ModuleNotFoundError(
                f"module {name!r} was asked for, but its sidecar route package "
                f"{package}.py is in none of: " + ", ".join(dirs)
            ) from e


def _configure_logging(log_file: str) -> None:
    """Send the compute log to *log_file* (fit_server points at it), plus stderr.
    Comprehensive logging of every fit is a project requirement - the sidecar's
    stderr is detached from fit_server, so a file is the durable record."""
    handlers: list[logging.Handler] = [logging.StreamHandler(sys.stderr)]
    if log_file:
        try:
            handlers.append(logging.FileHandler(log_file, encoding="utf-8"))
        except OSError as e:
            sys.stderr.write(f"fit_backend: cannot open log file {log_file}: {e}\n")
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
        handlers=handlers,
        force=True,
    )


#  Matches WORKER_PROTOCOL_VERSION in Worker/fit_worker_protocol.pas.
PROTOCOL_VERSION = 1
BACKEND_NAME = "python-lmfit"


class Handler(BaseHTTPRequestHandler):
    def _send(self, code: int, payload: dict) -> None:
        body = json.dumps(payload).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):  # noqa: N802 (stdlib naming)
        #  The route is the path; a query string is the handler's to read.
        parts = urllib.parse.urlsplit(self.path)
        handler = routes.GET_ROUTES.get(parts.path)
        if handler is None:
            self._send(404, {
                "ok": False,
                #  Names what could have been asked instead. For a typo in a
                #  script - where this error is actually read - "what is wrong"
                #  without "what is valid" is half an answer.
                "error": f"no such route: {self.path}. This build offers: "
                         f"{routes.known()}",
            })
            return
        #  None without a query, which is what every GET handler was given
        #  before queries were read - a module's handler may still expect it.
        query = dict(urllib.parse.parse_qsl(parts.query)) if parts.query else None
        self._send(200, handler(query))

    def do_POST(self):  # noqa: N802
        handler = routes.POST_ROUTES.get(self.path)
        if handler is None:
            self._send(404, {
                "ok": False,
                "error": f"no such route: {self.path}. This build offers: "
                         f"{routes.known()}",
            })
            return
        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length) if length else b""
        what = self.path.lstrip("/")
        try:
            body = json.loads(raw.decode("utf-8"))
            self._send(200, handler(body))
        except Exception as e:  # noqa: BLE001 - report anything as a 400/500
            #  Record why it was rejected (bad/unsupported expression, malformed
            #  problem, ...) so it is diagnosable from the sidecar log, then return
            #  the same message for the desktop to show the user.
            logging.getLogger("fit").warning("%s rejected: %s", what, e)
            self._send(400, {"ok": False, "errorCode": -1, "error": str(e)})

    #  Quiet by default; the desktop's logging is the record of record.
    def log_message(self, fmt, *args):
        sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description="Fit Python compute sidecar")
    ap.add_argument("--host", default="127.0.0.1")
    ap.add_argument("--port", type=int, default=8788)
    ap.add_argument(
        "--lifeline-port",
        type=int,
        default=0,
        help="the owning fit_server's readiness port: say when this sidecar is "
             "listening, and exit when that connection ends",
    )
    ap.add_argument(
        "--log-file",
        default="",
        help="append the compute log here (in addition to stderr)",
    )
    ap.add_argument(
        "--modules",
        default=os.environ.get("FIT_MODULES", ""),
        help="module packages whose routes to load, separated by os.pathsep",
    )
    ap.add_argument(
        "--module-path",
        default=os.environ.get("FIT_MODULE_PATH", ""),
        help="extra directories to search for those packages, separated by "
             "os.pathsep (the usual layouts are found without this)",
    )
    args = ap.parse_args(argv)

    _configure_logging(args.log_file)

    #  Made at import time when --lifeline-port was given (see _EARLY_PARENT);
    #  connected here only when main is called directly, as the tests do.
    parent = _EARLY_PARENT
    if parent is None and args.lifeline_port:
        parent = lifeline.connect(args.lifeline_port)

    #  Before the server starts answering, so a route is never missing for the
    #  first request. A module that fails to import is fatal here rather than a
    #  404 later: the caller asked for a build that has it.
    load_module_routes(args.modules, args.module_path)

    server = ThreadingHTTPServer((args.host, args.port), Handler)
    if parent is not None:
        #  Bound now, so a request can be answered: ready.
        lifeline.announce_ready(parent)
        #  And never outlive fit_server: when it ends, killed or not, the
        #  operating system closes the connection and this read returns.
        threading.Thread(
            target=lifeline.wait_for_parent_to_end,
            args=(parent, lambda: os._exit(0)), daemon=True,
        ).start()
    sys.stderr.write(
        f"fit_backend ({BACKEND_NAME}, protocol {PROTOCOL_VERSION}) "
        f"serving http://{args.host}:{args.port}\n"
    )
    sys.stderr.flush()
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


if __name__ == "__main__":  # pragma: no cover - process entry point
    raise SystemExit(main())
