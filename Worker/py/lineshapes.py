# SPDX-License-Identifier: GPL-3.0-or-later
"""Generic curve evaluation for the Python compute sidecar.

The sidecar is model-agnostic, exactly like the native engine: it does not know
Gaussians from Lorentzians. A curve arrives on the wire as a text ``expression``
in ``x`` and the curve's parameter names (e.g.
``A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))``); :class:`Model` compiles it
once and evaluates it over numpy arrays. Adding or changing a curve type is a
change to the Pascal model that owns the formula, never to this backend.

**Why compiled and not interpreted.** This is the innermost loop of every fit: a
50-parameter fit evaluates the model ~8500 times. Walking the expression's syntax
tree per call (as ``asteval`` does) cost ~97 us per curve, which made a fit that
the native engine finishes in ~1 s take ~9 s. Compiling the expression once to a
code object and evaluating it against a fixed namespace is ~4x faster, and the
namespace is deliberately minimal: only the numpy functions below, with builtins
removed, so an expression can compute but cannot reach the interpreter, import,
or touch attributes. The expressions themselves are ours (the built-in curves) or
transpiled from the user's own formula by ``native_math_expr``, and arrive only
over loopback from the fit_server that owns this process.

:class:`Model` also broadcasts, so a whole set of curves can be evaluated in one
call: pass ``x`` shaped ``(1, npoints)`` and each parameter shaped
``(ncurves, 1)`` and the result is ``(ncurves, npoints)``.
"""

from __future__ import annotations

import numpy as np
from scipy import special as _special

def _emg(u, sigma, tau):
    """Area-normalised exponentially modified Gaussian, evaluated branch-wise so it
    stays finite for every parameter (see special_functions.EmgProfile)."""
    u = np.asarray(u, dtype=float)
    z = (sigma / tau - u / sigma) / np.sqrt(2.0)
    with np.errstate(over="ignore", invalid="ignore"):
        rising = (1.0 / (2.0 * tau)) * np.exp(-u * u / (2.0 * sigma * sigma)) \
            * _special.erfcx(z)
        falling = (1.0 / (2.0 * tau)) \
            * np.exp(sigma * sigma / (2.0 * tau * tau) - u / tau) * _special.erfc(z)
    return np.where(z >= 0, rising, falling)


#  The only names an expression may use. Builtins are removed at evaluation, so
#  this is the whole vocabulary available to a received expression.
#
#  The special functions come straight from scipy (the same vetted code lmfit's
#  models wrap) - they are the trusted implementation for shapes the native engine
#  hand-writes: erf (Exponentially Modified Gaussian), and the Faddeeva function
#  wofz / voigt_profile (true Voigt). The native Pascal side must implement the
#  matching function and is pinned to these as the golden reference (see the
#  Pascal<->numpy differential tests).
_SYMBOLS = {
    "exp": np.exp,
    "log": np.log,
    "log10": np.log10,
    "sqrt": np.sqrt,
    "square": np.square,
    "abs": np.abs,
    "sin": np.sin,
    "cos": np.cos,
    "tan": np.tan,
    "arctan": np.arctan,
    "where": np.where,
    "pi": float(np.pi),
    "erf": _special.erf,
    "erfc": _special.erfc,
    "erfcx": _special.erfcx,
    "gamma": _special.gamma,
    #  Voigt profile and the underlying Faddeeva real part; wofz is complex, so a
    #  real-valued helper keeps an expression's arithmetic real.
    "voigt": _special.voigt_profile,
    "wofz_real": lambda z: np.real(_special.wofz(z)),
    #  Area-normalised exponentially modified Gaussian (matches special_functions.EmgProfile
    #  on the native side). Uses the erfcx form on the rising side and the exp*erfc form on
    #  the falling side; each is overflow-free where selected, so the result is finite for
    #  every parameter (a single-formula EMG is not).
    "emg": _emg,

    #  THE KEYPAD'S OWN VOCABULARY, mirrored from native_math_expr.
    #
    #  The user-curve dialog draws a keypad whose buttons insert tg, ctg, sh, ch,
    #  th, cth, sch, csch, arcsin, arccos, arctg, arcctg, arsh, arch, arth and
    #  arcth.  ExpressionToNumpy renames the ten that have a numpy name; the six
    #  below that do not keep the engine's own spelling and are provided here, so
    #  a formula the native engine accepts fits under this backend too.
    #
    #  The definitions are the load-bearing part, because both sides have to agree
    #  to the last bits: ctg is 1/tan rather than a cotangent primitive, arcctg is
    #  pi/2 - arctan rather than arctan(1/x) (which is discontinuous at zero), and
    #  arcth is arctanh(1/x).  tests/expr_fidelity_cases.txt evaluates all sixteen
    #  through both engines and compares.
    "sinh": np.sinh,
    "cosh": np.cosh,
    "tanh": np.tanh,
    "arcsin": np.arcsin,
    "arccos": np.arccos,
    "arcsinh": np.arcsinh,
    "arccosh": np.arccosh,
    "arctanh": np.arctanh,
    "ctg": lambda v: 1.0 / np.tan(v),
    "cth": lambda v: 1.0 / np.tanh(v),
    "sch": lambda v: 1.0 / np.cosh(v),
    "csch": lambda v: 1.0 / np.sinh(v),
    "arcctg": lambda v: np.pi / 2.0 - np.arctan(v),
    "arcth": lambda v: np.arctanh(1.0 / v),
}

#  Nothing from the interpreter is reachable from an expression.
_NO_BUILTINS = {"__builtins__": {}}

#  Named in the error when an expression uses something unsupported, in the user's
#  (fpexprpars) vocabulary rather than the numpy names.
SUPPORTED_HINT = ("exp, ln, log, sqrt, sqr, abs, sin, cos, tg, ctg, "
                  "arcsin, arccos, arctan, arctg, arcctg, "
                  "sh, ch, th, cth, sch, csch, arsh, arch, arth, arcth, pi, "
                  "erf, erfc, erfcx, gamma, voigt, wofz_real, emg")


class Model:
    """A curve compiled from a text expression; call it with x and a params dict.

    A malformed or non-evaluable expression raises :class:`ValueError`, which the
    sidecar reports as a fit error rather than a crash.
    """

    def __init__(self, expression: str):
        expr = (expression or "").strip()
        if not expr:
            raise ValueError("empty curve expression")
        self.expression = expr
        try:
            self._code = compile(expr, "<curve expression>", "eval")
        except (SyntaxError, ValueError) as e:
            raise ValueError(f"invalid curve expression: {e}") from e

    def __call__(self, x: np.ndarray, params: dict) -> np.ndarray:
        namespace = dict(_SYMBOLS)
        namespace["x"] = x
        namespace.update(params)
        try:
            value = eval(self._code, _NO_BUILTINS, namespace)  # noqa: S307
        except NameError as e:
            #  A function or name the backend does not provide - e.g. an fpexprpars
            #  function (frac/int/round/...) the native engine accepts but that has
            #  no numpy equivalent here. Name it, so the message is actionable.
            #  NameError.name is always set for a failed name lookup (Python 3.10+).
            raise ValueError(
                f"the curve expression uses '{e.name}', which the Python minimizer "
                f"does not support; use the native minimizer or a supported "
                f"function ({SUPPORTED_HINT})"
            ) from e
        except Exception as e:  # noqa: BLE001 - any other failure is a bad expression
            raise ValueError(f"cannot evaluate curve expression: {e}") from e
        #  Broadcast a constant (parameter-only) result to the shape of x.
        return np.asarray(value, dtype=float) * np.ones_like(x)
