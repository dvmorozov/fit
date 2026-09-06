<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Python compute sidecar (lmfit)

An alternative compute backend for Fit. It is an **independent HTTP process** that
serves the **same stateless fit endpoint** as the native `fit_server`, but runs the
optimization with [lmfit](https://lmfit.github.io/lmfit-py/) and returns what the
native engine cannot: **per-parameter standard errors** and **fit statistics**
(weighted reduced χ², R², AIC, BIC).

```
GET  /health -> {"ok": true, "backend": "python-lmfit", "protocol": 1}
POST /fit    -> fit-problem JSON  ->  outcome JSON (curves + errors + statistics)
```

The wire format is the same one `Worker/fit_problem_json.pas` marshals, so the
native and Python backends are interchangeable behind the `IFitBackend` seam. The native engine remains the default and needs no Python.

The sidecar is **model-agnostic**, exactly like the native engine: it does not
implement any curve type. Each curve arrives on the wire as a text `expression`
in `x` and its parameter names (e.g.
`A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))`) together with, per placed
curve, its seed parameter values. The backend evaluates whatever formula it is
sent (compiled once, evaluated over numpy) and optimizes it. Adding or changing
a curve type is a change to the Pascal model that owns the formula
(`*_points_set.pas` `GetCurveExpression`), never to this backend. `curveTypeId`
still travels for logging but the fit uses `expression`.

## Setup and use

Everything goes through the pinned `requirements.txt` — never `pip install` these
by hand:

```
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
.venv/bin/python -m pytest .             # its own tests
.venv/bin/python fit_backend.py          # serve on http://127.0.0.1:8788
```

`fit_server` starts and stops the sidecar itself, so serving it by hand is only
for debugging it. Choose the lmfit engine in the client to use it.

## Layout

| File | What it is |
|------|-----------|
| `lineshapes.py` | `Model`: compiles the wire `expression` once to a code object and evaluates it over numpy in a builtins-free namespace (no curve type hard-coded) |
| `fitting.py` | the weighted multi-peak fit; consumes the fit-problem dict, returns curves + errors + statistics |
| `fit_backend.py` | the HTTP server (`/health`, `/fit`) |
| `test_fit_backend.py` | pytest: parameter recovery, statistics, overlapping peaks, HTTP round-trip, and expression fidelity to `SimpMath.pas` |
| `requirements.txt` | pinned numpy / scipy / lmfit |

## Supported curves

Any curve whose model can supply a closed-form `expression` — every built-in
analytic type reaches the backend this way (Gaussian, Lorentzian, Pseudo-Voigt,
asymmetric Pseudo-Voigt, two-branch Pseudo-Voigt) **and user-defined curves**,
whose formula is transpiled to numpy syntax. Because the backend never names a
curve type, a new analytic curve needs no change here — only its Pascal
`GetCurveExpression`. A curve with no closed-form expression (empty
`GetCurveExpression`) is reported as a fit error rather than silently mis-fit.

The problem also carries each parameter's `vary` and `shared` flags plus its
physical bounds (`min`/`max`), so the fit holds fixed parameters, ties shared ones
across curves, and stays inside the same feasible region — exactly as the native
engine does. The bounds are essential: an unbounded fit reaches a lower residual in
an unphysical region (`eta > 1`, `sigma < 0`) that the native side then clamps on
writeback, so the recomputed model no longer matches what was fitted.

## Logging

`--log-file` (set by `fit_server`) records every fit: the problem shape and the
solver's convergence record (`success`, `nfev`, `redchi`, unweighted R-factor,
termination message). Start there when a fit looks wrong.
