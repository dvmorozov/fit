<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Argument axes and units

Fit stores your data's x-argument **exactly as loaded** and never changes it. What you see
on the horizontal axis — its name, unit and values — is a **display transform** you choose.
Switching axes changes only the labels and the numbers shown/exported; the stored data and
the fit results (in raw units) are identical either way.

Choose an axis under **Data → Argument Transformation**. By default Fit picks it for you,
from the curve type you are modelling with — see below.

## Available axes

| Axis | When to use | Needs wavelength? |
|------|-------------|-------------------|
| **From Curve Type** | The default: the axis the selected model defines — a diffraction angle for the peak lineshapes, a plain position for user-defined curves and for any module's own model. | Only if the model asks for one |
| **General Position** | Any dataset — energy, time, Raman shift, m/z, or a plain x. | No |
| **Theta** / **2 \* Theta** / **Sin Theta / Lambda** | X-ray / neutron diffraction. The stored argument is 2θ (degrees); these show θ, 2θ, or sin(θ)/λ. | Sin(θ)/λ does |
| **Custom Position…** | A domain transform of your own — e.g. a log axis, an energy↔wavelength conversion. | No |

### From Curve Type (the default)

The model knows what its argument means, so it supplies the axis: choose a Gaussian,
Pseudo-Voigt, Pearson VII or any other peak and the chart shows **2 \* Theta [deg]**; choose an
user-defined curve and it shows **Position**. Change the curve type
and the caption, the plotted values and the positions in the parameters grid follow it.

Picking any other item in the menu overrides this for good: your choice wins over whatever
the model would prefer, on this session and every later one, until you switch back to
**From Curve Type**.

### General Position (identity)

The value shown is the argument as loaded, with no unit assumed and **no wavelength prompt**.
Use this for any non-diffraction signal whose model would otherwise ask for a diffraction
angle — for instance a peak lineshape fitted to a spectrum in energy or time.

### Diffraction angle (Theta / 2·Theta / Sin Theta / Lambda)

Preserves the original diffraction workflow. The stored argument is **2θ in degrees**;
selecting **Theta** shows θ = 2θ/2, and **Sin Theta / Lambda** shows sin(θ)/λ (unit `1/A`).
The last requires a wavelength — set it under **Data → Argument Transformation → Set Rule
Parameters → Wavelength…**. XRD and neutron data behave exactly as before.

### Custom Position… (user-defined)

Define your own axis with a forward formula (shown value as a function of the raw `x`) and
its inverse. Both are formulas in the single variable `x`, evaluated by the same expression
engine as user-defined curves (functions like `ln`, `log`, `exp`, `sqrt`, `sin`, `cos`, and
the operators `+ - * / ^`).

1. **Data → Argument Transformation → Custom Position…**
2. Enter a **Name** and **Unit** (labels only), a **Forward** formula (`display = f(x)`), and
   an **Inverse** formula (`raw = g(display)`).

   Example — a natural-log axis: Forward `ln(x)`, Inverse `exp(x)`.
3. **OK**. The chart relabels and rescales to the custom axis.

The custom axis is **display-only** — the fit still runs on the raw stored argument, so
fitted positions/widths are unaffected. Your definition is saved and restored on the next
start (see below).

## Persistence

An axis you picked yourself is remembered between sessions, and is recorded as *your* choice
so it is never overridden by the model. Until you pick one, Fit starts on **From Curve Type**
— including on an installation that predates it, whose stored axis was only ever the old
hard-coded default. For a custom axis, its name, unit and both formulas are saved too, so it
comes back defined rather than blank; if it cannot be restored, Fit falls back to
**From Curve Type**.

## Notes

- Switching axes **never** alters the fit or the stored data — only the display,
  exported values, and reported positions change.
- The wavelength control is only relevant to the **Sin Theta / Lambda** diffraction mode, and
  is greyed out whenever the axis in force is not a diffraction angle.
