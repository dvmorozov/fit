<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Compute backends: the native engine and the Python option

Fit does its number-crunching in a **compute server** — a separate program the
desktop talks to. There are two:

| Backend | What you get | Setup |
|---------|--------------|-------|
| **Native** (`fit_server`) | The built-in **unconstrained Downhill-Simplex** engine. Fast, interactive, **needs nothing extra**. Reports the R-factor and goodness-of-fit statistics (reduced χ², R², AIC, BIC). | Ships with the app. |
| **Python** (lmfit) | A **Trust Region (trf)** least-squares fit that additionally reports **per-parameter uncertainties**. Fits every curve type the native engine does, including user-defined ones. | One-time setup, below. |

Both minimizers fit the **same** models and, on the same data, converge to the
same result — they differ only in the algorithm and in the extra uncertainties the
Python one reports. The Python backend uses scipy's **Trust Region Reflective**
solver (rather than classic Levenberg–Marquardt) so it copes with fits that have
more free parameters than data points, exactly as the native Downhill Simplex does.

The native backend is the default and always works. The Python backend is
**optional** — you only set it up if you want parameter error bars.

## What you need for the Python backend

Python 3 and three libraries: **numpy**, **scipy** and **lmfit**. You do **not**
install these by hand — the build script installs the exact, tested versions into
a self-contained environment next to the app, so nothing touches your system
Python.

### One-time setup

From the project folder, run:

```
cd Worker/py
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

That installs the pinned
[`Worker/py/requirements.txt`](../../Worker/py/requirements.txt) — numpy, scipy
and lmfit — into a virtual environment beside the app.

**On Debian and Ubuntu**, `python3 -m venv` needs the separate `python3-venv`
package. Without it the command leaves an empty `.venv` behind that has no `pip`
in it, and that husk is worse than no venv at all: the app prefers
`.venv/bin/python` over the system interpreter, so every fit that asks for the
Python engine then dies with `ModuleNotFoundError: No module named 'numpy'`.
Install `python3-venv` and delete the broken `.venv` before retrying.

Verify it:

```
.venv/bin/python -c "import numpy, scipy, lmfit; print('ok')"
.venv/bin/python -m pytest .          # the sidecar's own tests
```

## Using the Python backend in the app

That one-time setup is all you do — you do **not** start the Python worker
yourself. It is a **sidecar of the compute server**: `fit_server` starts it on
first use and stops it when it exits. The desktop only ever talks to
`fit_server`.

The Python engine is selected the same way as the native one: **Fit → Minimizer →
Python (Trust Region)**. Then fit exactly as usual (place curves, Minimize)
— the compute server runs the optimization on the sidecar instead of the native
engine, and the results come back through the same parameter grid and statistics.

If the Python libraries aren't installed, the fit says so (run the setup above);
switch back to **Minimizer → Downhill Simplex** and the native engine keeps
working.

### Weighting (Python only)

**Fit → Weighting** chooses how the Python backend weights the residuals. It only
appears while the Python minimizer is selected — the native engine always fits
unweighted, so the menu is hidden under Downhill Simplex.

| Option | What it does |
|--------|--------------|
| **Poisson (counting statistics)** — default | Divides each residual by √counts. Correct for photon-counting data: it fits the *relative* error, so faint features carry as much weight as tall peaks. This is also the basis of the reduced χ² shown in the status bar. |
| **None (unweighted)** | Every point counts equally, the same objective the native engine minimizes. Use it when you want the Python result to match the native one term-for-term. |

Both minimizers fit within the same physical parameter limits (amplitude ≥ 0,
width > 0, mixing 0…1, position within its placement window), so the two engines
agree on the same data rather than drifting to different answers.

**Help → Compute Backends…** shows the setup step inside the app.

## Which should I use?

- Just fitting and reading positions/widths/R-factor → **native**, no setup.
- Publishing numbers that need **parameter uncertainties** → **Python**
  (Fit → Fit with Python).

The native fit shows the reduced χ², R² and AIC/BIC in the status bar. With the
Python minimizer the parameter grid additionally shows each fitted value with its
uncertainty (`value ± error`); the native engine leaves the uncertainty blank.
