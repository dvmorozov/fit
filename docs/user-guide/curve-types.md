<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Built-in curve types (peak and edge shapes)

Fit ships a library of ready-made peak shapes. Pick one with **Model → Curve Type →
Standard**, then
place it on the chart and fit as usual — no formula to type (for a custom shape, see
[user-defined curves](user-defined-curves.md)). Every built-in shape fits and plots
identically under **both** minimizers (**Fit → Minimizer →** Downhill Simplex or Python /
Trust Region), so you can switch engines freely.

Composite models are built by placing **several** curves at once — the fit optimises them
together, so overlapping peaks are resolved as a sum.

## Choosing a shape

| Curve | Use it for | Parameters |
|-------|------------|------------|
| **Gaussian** | symmetric peaks dominated by random broadening | A (area), x0, σ |
| **Lorentzian** | symmetric peaks with heavier tails (lifetime broadening) | A (area), x0, σ (FWHM) |
| **Pseudo-Voigt** | a fast Gaussian/Lorentzian blend | A, x0, σ, η (mix) |
| **Asym. Pseudo-Voigt** | a Pseudo-Voigt whose two sides have different widths | A, x0, σ, η, Δσ (asymmetry) |
| **2 br. Pseudo-Voigt** | left and right halves each with their own mix and width | A, x0, σ, η per branch |
| **Voigt** | the *exact* Gaussian⊗Lorentzian convolution (diffraction, spectroscopy) | A (area), x0, σ (Gaussian), γ (Lorentzian HWHM) |
| **Pearson VII** | peaks whose "peakiness" is between Lorentzian and Gaussian | A (peak), x0, σ (FWHM), m (shape) |
| **Moffat** | bell peaks with adjustable wing weight (optics, PSFs) | A (peak), x0, σ (core), m (β) |
| **Skewed Gaussian** | mildly asymmetric peaks | A, x0, σ, β (skew) |
| **Exponentially Modified Gaussian** | tailed peaks from a one-sided exponential (chromatography) | A (area), x0, σ, τ (tail) |
| **Doniach-Sunjic** | asymmetric core-level lines (XPS) | A, x0, σ (width), α (asymmetry) |
| **Step (erf)** | a smoothed edge / absorption step, not a peak | A (height), x0 (edge), σ (width) |

A thirteenth type, **User-defined**, is a formula you type in — see
[user-defined curves](user-defined-curves.md). Anything beyond these is added as a
module: see [writing a module](../contributing/writing-a-module.md).

**How the shapes relate** (useful when a fit is unstable — start from the simpler limit):

- **Voigt** becomes a **Gaussian** as γ → 0 and a **Lorentzian** as σ → 0.
- **Pearson VII** is a **Lorentzian** at m = 1 and approaches a **Gaussian** as m grows.
- **Moffat** is a **Lorentzian** at m = 1.
- **Skewed Gaussian** and **EMG** both become a plain **Gaussian** as their asymmetry
  (β, τ) → 0.

## Parameter meanings

- **A** — amplitude. For most shapes it is the peak **area** (Gaussian, Lorentzian, Voigt,
  EMG); for Pearson VII and Moffat it is the **peak height**; for Step it is the step
  **height**. In every case it is a linear scale the fit adjusts, so a rough starting value
  is fine.
- **x0** — the peak position (for Step, the edge centre, where the value is A/2).
- **σ** (sigma) — the width. Its exact meaning is per shape (Gaussian std-dev, Lorentzian
  FWHM, EMG/Voigt Gaussian part, edge width); the fit refines it from a starting value
  derived from the fitting interval.
- **Shape parameters** — **m** (Pearson VII / Moffat), **η** (Pseudo-Voigt mix, 0–1),
  **γ** (Voigt Lorentzian half-width), **τ** (EMG tail), **β** (Skewed-Gaussian skew),
  **α** (Doniach–Šunjić asymmetry, 0–1). Starting values are chosen automatically; you can
  edit or fix any of them in the curve's properties.

## Reading the results

After a fit, each curve's parameters appear in the results grid **with their
uncertainties** (`value ± error`) when the Python minimizer is used, alongside the overall
quality figures (weighted χ², R², AIC/BIC) in the status area. See
[compute backends](compute-backends.md) for choosing and configuring the engine.

## Which minimizer?

Both engines fit every built-in shape. Use the **native Downhill Simplex** for a quick,
dependency-free fit; use the **Python / Trust Region** engine when you want **parameter
uncertainties** and gradient-based convergence. Results agree between the two — that
equivalence is enforced by the project's cross-engine tests.

For the fit → change the model → fit again loop, and what a re-fit keeps,
see [fitting-workflow.md](fitting-workflow.md).
