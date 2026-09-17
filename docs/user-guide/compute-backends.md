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

## What the server can be asked to do

The compute server documents itself: point a browser at
`http://127.0.0.1:8787/docs` while it is running and you get its complete HTTP
API, with every call runnable from the page. The list is produced by the server
you are asking, so it describes the version you actually have. If nothing is
running yet, `./scripts/build-app.ps1 -Task api-docs` builds it, starts it and
opens that page for you.

## What you need for the Python backend

Python 3.10 to 3.13 and three libraries: **numpy**, **scipy** and **lmfit**.
They go into a self-contained virtual environment at the exact, tested versions,
so nothing touches your system Python.

### One-time setup

From the project folder, build the environment and install the pinned
[`Worker/py/requirements.txt`](../../Worker/py/requirements.txt) into it. On
macOS and Linux:

```
python3 -m venv ~/.local/share/fit/py/sidecar
~/.local/share/fit/py/sidecar/bin/python -m pip install --only-binary=:all: -r Worker/py/requirements.txt
```

On Windows, in PowerShell:

```
py -3.12 -m venv "$env:LOCALAPPDATA\Fit\py\sidecar"
& "$env:LOCALAPPDATA\Fit\py\sidecar\Scripts\python.exe" -m pip install --only-binary=:all: -r Worker/py/requirements.txt
```

`--only-binary=:all:` stops pip compiling numpy or scipy from source when your
Python is newer than the pins have wheels for; it fails instead, and a Python in
the range above is the fix.

**The environment is per machine, not per checkout.** It goes in
`%LOCALAPPDATA%\Fit\py\sidecar` on Windows and
`~/.local/share/fit/py/sidecar` elsewhere (`$XDG_DATA_HOME` is honoured), so
every clone and worktree on the machine shares one copy. Set `FIT_PY_HOME` to
put it somewhere else - the environment then goes in its `sidecar` directory;
both the build script and the app read that variable.

It is deliberately **not** installed system-wide. Debian, Fedora and Homebrew
mark their Python "externally managed" and refuse such an install outright
(PEP 668), and the pins exist so fitted numbers reproduce — which the next
unrelated `pip install` into a shared interpreter would undo.

**On Debian and Ubuntu**, creating the environment needs the separate
`python3-venv` package; without it `venv` fails on `ensurepip`. Delete the
half-made directory before trying again.

Verify it by serving the sidecar by hand and asking it for its health:

```
~/.local/share/fit/py/sidecar/bin/python Worker/py/fit_backend.py --port 8788
```

<http://127.0.0.1:8788/health> answers `{"ok": true, ...}`. Stop it with Ctrl+C;
the app starts its own.

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
