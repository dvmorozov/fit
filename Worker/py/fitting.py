# SPDX-License-Identifier: GPL-3.0-or-later
"""Weighted multi-peak fitting with lmfit, returning uncertainties and statistics.

This is the numerics of the Python compute sidecar, kept free of any HTTP so it
can be tested directly. It consumes the same fit-problem JSON the native
``fit_server`` accepts on ``POST /fit`` (see ``Worker/fit_problem_json.pas``) and
returns the same outcome shape extended with what the native engine cannot give:
per-parameter standard errors, weighted reduced chi-squared, R^2, AIC and BIC.

The backend is model-agnostic: the problem carries the curve ``expression`` and,
per placed curve, its seed parameter values. Nothing here knows one curve type
from another - it optimizes whatever formula it is handed, unconstrained, like
the native Downhill Simplex engine.
"""

from __future__ import annotations

import logging

import numpy as np
from lmfit import Parameters, minimize

from lineshapes import Model

#  Comprehensive compute logging: every fit records its shape and how the solver
#  converged, so a bad result (e.g. the DHS-vs-Python divergence on 2.dat) is
#  diagnosable from the sidecar log rather than a black box. Configured in
#  fit_backend.main() to a file fit_server points at.
log = logging.getLogger("fit")

#  Evaluation budget per free parameter. Bounds the wall time of a fit so a
#  saturated model can never look like a hung application; generous enough that
#  realistic fits still converge (a 40-free-parameter fit of Data/2.dat converges
#  in ~6900 evaluations, well inside 300*(40+1)).
MAX_NFEV_PER_PARAM = 300

#  Objective codes, mirroring Server/fit_loss.pas. Kept as plain integers on the
#  wire so neither side has to parse the other's vocabulary.
LOSS_RFACTOR = 0
LOSS_RFACTOR_LEGACY = 1
LOSS_SUMSQ = 2
LOSS_RELATIVE = 3

#  What this solver can honour. scipy's least_squares is handed a residual VECTOR
#  and minimises the sum of its squares, so:
#
#    RFACTOR  = sum(resid^2) / sum(obs)^2  - a positive CONSTANT multiple of the
#               sum of squares, so exactly the same minimiser;
#    SUMSQ    = sum(resid^2)               - the solver's native form.
#
#  The other two cannot be written as a residual vector at all: RFACTOR_LEGACY
#  divides by a quantity that moves with the parameters (which is what makes it
#  gameable), and RELATIVE sums absolute deviations, an L1 problem. The caller
#  falls back to the native engine for those; this list is the second half of
#  that contract, so a request that slips through is refused rather than
#  silently minimising something else.
LEAST_SQUARES_LOSSES = frozenset({LOSS_RFACTOR, LOSS_SUMSQ})

LOSS_NAMES = {
    LOSS_RFACTOR: "R-factor",
    LOSS_RFACTOR_LEGACY: "R-factor (legacy)",
    LOSS_SUMSQ: "Sum of squares",
    LOSS_RELATIVE: "Relative deviation",
}


def _weights(y: np.ndarray, kind: str) -> np.ndarray:
    """Residual weights. 'poisson' = 1/sqrt(max(y,1)) (counting data, the XRD
    default); 'none' = unweighted. Weighting is what turns a curve into a
    result with meaningful uncertainties."""
    if kind == "none":
        return np.ones_like(y)
    #  sqrt(counts) counting statistics; floor at 1 so empty channels do not
    #  divide by zero or dominate.
    return 1.0 / np.sqrt(np.maximum(y, 1.0))


def _seed_curves(problem: dict) -> list[list[dict]]:
    """The placed curves as lists of parameter dicts (name/value/vary/shared);
    validated. Absent vary/shared default to varied/not-shared."""
    curves = problem.get("curves") or []
    if not curves:
        raise ValueError("no curves given")
    out: list[list[dict]] = []
    for c in curves:
        specs = [
            {
                "name": p["name"],
                "value": float(p["value"]),
                "vary": bool(p.get("vary", True)),
                "shared": bool(p.get("shared", False)),
                #  Physical bounds mirroring the native parameter clamps (A>=0,
                #  sigma>0, eta in [0,1]); absent => unbounded. Keeping the fit
                #  inside them is what stops it wandering into an unphysical minimum
                #  that the native engine would clamp away on readback.
                "min": float(p.get("min", -np.inf)),
                "max": float(p.get("max", np.inf)),
            }
            for p in c.get("params", [])
        ]
        if not specs:
            raise ValueError("a curve has no parameters")
        out.append(specs)
    return out


def _json_float(value, default: float = 0.0) -> float:
    """A JSON-safe float. NaN/Infinity are legal in Python's json output but not in
    strict JSON, and the Pascal client's parser rejects them - which would turn a
    finished fit into "returned an unreadable result". Non-finite values (e.g. a
    reduced chi-square with no degrees of freedom) become *default*."""
    try:
        v = float(value)
    except (TypeError, ValueError):
        return default
    return v if np.isfinite(v) else default


def _lmfit_key(curve_index: int, spec: dict) -> str:
    """The lmfit parameter name for a spec: shared params collapse to one key
    (``shared__<name>``) tied across all curves; the rest are per-curve."""
    if spec["shared"]:
        return "shared__" + spec["name"]
    return f"c{curve_index}_{spec['name']}"


def fit_problem(problem: dict) -> dict:
    """Run the fit described by *problem* and return the outcome dict.

    problem: {profileX, profileY, expression, curves:[{params:[{name,value}]}],
              begIndex?, endIndex?, weighting?}
    """
    x = np.asarray(problem["profileX"], dtype=float)
    y = np.asarray(problem["profileY"], dtype=float)
    if x.size == 0 or x.size != y.size:
        raise ValueError("profile is empty or malformed")

    #  The fitting window (defaults to the whole profile).
    beg = int(problem.get("begIndex", 0) or 0)
    end = int(problem.get("endIndex", x.size - 1) or (x.size - 1))
    beg = max(0, min(beg, x.size - 1))
    end = max(beg, min(end, x.size - 1))
    xw, yw = x[beg : end + 1], y[beg : end + 1]

    model = Model(problem.get("expression", ""))
    seeds = _seed_curves(problem)

    #  One lmfit parameter per distinct key: shared params collapse across curves,
    #  the rest are per-curve. A vary=False param is added but held fixed. Seeding
    #  a shared param from its first occurrence is enough (all seeds agree).
    params = Parameters()
    for i, specs in enumerate(seeds):
        for spec in specs:
            key = _lmfit_key(i, spec)
            if key not in params:
                lo, hi = spec["min"], spec["max"]
                #  Keep the seed inside its bounds (lmfit rejects a value outside),
                #  in case a seed sits exactly on the clamp.
                value = min(max(spec["value"], lo), hi)
                if lo >= hi:
                    #  A degenerate bound (the native window allows no movement)
                    #  means the parameter is effectively fixed; lmfit also rejects
                    #  min == max, so hold it rather than bound it.
                    params.add(key, value=value, vary=False)
                else:
                    params.add(key, value=value, vary=spec["vary"], min=lo, max=hi)

    #  Default 0 = the corrected R-factor, matching the Pascal side, so a peer
    #  that omits the field lands on the right objective rather than a historical
    #  one.
    loss_kind = int(problem.get("lossKind", LOSS_RFACTOR))
    if loss_kind not in LEAST_SQUARES_LOSSES:
        raise ValueError(
            "loss %r (%s) cannot be expressed as a least-squares residual, which "
            "is the only form this backend can minimise; fit it with the native "
            "engine instead"
            % (loss_kind, LOSS_NAMES.get(loss_kind, "unknown"))
        )

    weighting = problem.get("weighting", "poisson")
    w = _weights(yw, weighting)

    #  Evaluate every curve in ONE expression call by broadcasting, instead of
    #  looping in Python once per curve. The model evaluation is the innermost loop
    #  of the fit (thousands of calls), so paying the interpreter/numpy per-call
    #  overhead once instead of once-per-curve is what keeps a multi-curve fit
    #  comparable to the native engine. Only possible when every curve exposes the
    #  same parameter names (always true for one curve type); otherwise fall back.
    names = [s["name"] for s in seeds[0]]
    batched = all([s["name"] for s in specs] == names for specs in seeds)

    if batched:
        #  Per parameter name, the lmfit key of each curve, in curve order.
        keys_by_name = {
            name: [_lmfit_key(i, seeds[i][j]) for i in range(len(seeds))]
            for j, name in enumerate(names)
        }
        xb = xw.reshape(1, -1)              #  (1, npoints)

        def model_sum(ps) -> np.ndarray:
            values = {
                name: np.fromiter((ps[k].value for k in keys), float,
                                  len(keys)).reshape(-1, 1)   # (ncurves, 1)
                for name, keys in keys_by_name.items()
            }
            return model(xb, values).sum(axis=0)
    else:
        def model_sum(ps) -> np.ndarray:
            total = np.zeros_like(xw)
            for i, specs in enumerate(seeds):
                values = {s["name"]: ps[_lmfit_key(i, s)].value for s in specs}
                total = total + model(xw, values)
            return total

    def residual(ps):
        return (model_sum(ps) - yw) * w

    if not any(p.vary for p in params.values()):
        raise ValueError("no free parameters to fit")

    log.info(
        "fit: points=%d curves=%d params(free=%d shared=%d fixed=%d) "
        "loss=%s weighting=%s expr=%r",
        xw.size, len(seeds), sum(1 for p in params.values() if p.vary),
        sum(1 for k in params if k.startswith("shared__")),
        sum(1 for p in params.values() if not p.vary),
        LOSS_NAMES.get(loss_kind, loss_kind), weighting, model.expression,
    )

    #  A saturated problem (free parameters ~ data points, e.g. 10 curves over 51
    #  points) can otherwise grind for many minutes: lmfit's default budget here is
    #  ~2000*(n+1) evaluations, which looks like a hung application. Cap the effort
    #  so a fit always returns in bounded time; hitting the cap is reported as
    #  success=False and logged, and the best point so far is still returned.
    n_free = sum(1 for p in params.values() if p.vary)
    max_nfev = MAX_NFEV_PER_PARAM * (n_free + 1)

    #  scipy Trust Region Reflective (bounded): it tolerates more free parameters
    #  than data points (M<N) - as the native Downhill Simplex does - honours the
    #  physical parameter bounds so the fit matches what the native engine would
    #  accept, and still returns a covariance, hence per-parameter uncertainties.
    result = minimize(residual, params, method="least_squares", max_nfev=max_nfev)
    fit = model_sum(result.params)

    #  Log how the solve went, incl. the unweighted R-factor (the native engine's
    #  own metric) so the sidecar log is directly comparable to a DHS run.
    log.info(
        "done: success=%s nfev=%d redchi=%.4g rFactor=%.4g msg=%s",
        result.success, result.nfev, result.redchi, _r_factor(fit, yw),
        result.message,
    )

    #  Fitted curves, parameters in the order the seeds gave them, with errors.
    #  A shared parameter reports its single tied value on every curve.
    curves = []
    for i, specs in enumerate(seeds):
        pout = []
        for spec in specs:
            p = result.params[_lmfit_key(i, spec)]
            pout.append(
                {
                    "name": spec["name"],
                    "value": _json_float(p.value),
                    #  -1 when lmfit could not estimate it (fixed param, or a
                    #  non-finite covariance).
                    "error": _json_float(p.stderr, -1.0)
                    if p.stderr is not None else -1.0,
                }
            )
        curves.append({"params": pout})

    return {
        "errorCode": 0,
        "rFactor": _json_float(_r_factor(fit, yw)),
        "curves": curves,
        "statistics": _statistics(result, yw, fit),
    }


def _r_factor(fit: np.ndarray, y: np.ndarray) -> float:
    """The engine's R-factor: sum|y-model| / sum|y|."""
    denom = float(np.sum(np.abs(y)))
    if denom == 0:
        return 0.0
    return float(np.sum(np.abs(y - fit)) / denom)


def _statistics(result, y: np.ndarray, fit: np.ndarray) -> dict:
    """The publishable numbers the native engine does not produce."""
    ss_res = float(np.sum((y - fit) ** 2))
    ss_tot = float(np.sum((y - np.mean(y)) ** 2))
    r_squared = 1.0 - ss_res / ss_tot if ss_tot > 0 else 0.0
    return {
        #  Weighted reduced chi-squared: ~1 means the model fits to the noise.
        #  All JSON-sanitised: with more parameters than points these can be
        #  non-finite, which strict JSON cannot carry.
        "reducedChiSquare": _json_float(result.redchi),
        "rSquared": _json_float(r_squared),
        "aic": _json_float(result.aic),
        "bic": _json_float(result.bic),
        "degreesOfFreedom": int(result.nfree),
        "dataPoints": int(result.ndata),
    }
