<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# User-defined curves (no-code custom models)

Fit lets you fit peaks with a **formula you type in**, without writing or compiling any
code. This is useful when none of the built-in shapes (Gaussian, Lorentzian, Pseudo-Voigt,
…) matches your data.

## Creating a user-defined curve

1. **Model → Curve Type → User → New User Curve…**.
2. In **Create New Curve Type**, give the curve a **Name** and an **Expression** in terms
   of `x`. A working bell curve is pre-filled as an example:

   ```
   A*exp(-((x-x0)/SIGMA)^2)
   ```

   Use the on-screen buttons or type directly. **Every button on the keypad names a
   function both minimizers understand**: `exp`, `ln` (natural log), `log` (base 10),
   `sqrt`, `sqr`, `abs`; the circular `sin`, `cos`, `tg`, `ctg` and their inverses
   `arcsin`, `arccos`, `arctg`, `arcctg`; the hyperbolic `sh`, `ch`, `th`, `cth`,
   `sch`, `csch` and their inverses `arsh`, `arch`, `arth`, `arcth`. Also the constant
   `pi`, the operators `+ - * / ^`, and brackets. Names are not case-sensitive, so
   `Tg`, `tg` and `TG` are the same function.

   `arctan` is accepted as another spelling of `arctg`.

   The **Python (Trust Region)** minimizer supports the same set. If a formula uses a
   function only the native engine knows, fitting with the Python minimizer stops with a
   message naming that function and suggesting the native minimizer instead — so choose
   **Fit → Minimizer → Downhill Simplex** for it.
3. Click **Continue »**. The **Set Curve Type Properties** dialog opens, listing the
   parameters found in your formula. Here you assign each one a role (see below) and, if you
   like, edit its starting value or mark it fixed.
4. Click **Done**. Your curve now appears in the **Model → Curve Type → User** submenu,
   below **New User Curve…**, and can be selected and fitted like any built-in curve. The
   curve that is being fitted shows a check mark; **New User Curve…** never does — it is
   the action that creates a curve, not a curve you can select.

## Parameter roles

For a fit to start sensibly, Fit needs to know which parameter is the peak amplitude, which
is the width, etc. There are two ways to tell it — they can be combined:

**By name (convention).** Name your parameters using these reserved names and the role is
recognised automatically:

| Name    | Role       | Initialised from            |
|---------|------------|-----------------------------|
| `x`     | argument   | — (runs along the axis)     |
| `x0`    | position   | where the peak is placed    |
| `A`     | amplitude  | the data peak height        |
| `SIGMA` | width      | the fitting interval        |

**By role (explicit).** In the properties dialog, use the **Argument**, **Position
parameter**, **Amplitude parameter** and **Width parameter** drop-downs to designate any
parameter — whatever its name — for each role. Explicit roles take precedence over names.

Every other parameter is a free variable, optimised during the fit, starting at the value
you set in the properties dialog (0 by default).

## What the fit initialises for you

When you run a fit, a user curve is set up from your data just like a built-in curve:

- **amplitude** ← the peak height of the data,
- **position** ← where you placed the curve (or the centre of the interval if you placed
  none),
- **width** ← a value derived from the fitting interval,

and the optimiser refines all free parameters from there.

## Worked example

1. Load `Data/1.dat` (**File → Load Profile…**).
2. **Model → Curve Type → User → New User Curve…**; keep the default formula
   `A*exp(-((x-x0)/SIGMA)^2)`; **Continue »**; leave the roles (`A`, `x0`, `SIGMA` are
   recognised by name); **Done**.
3. Place the curve near a peak (**Model → Curve Positions**), then **Fit → Minimize
   Difference**. The curve converges onto the peak; the R-factor is shown in the status bar.

## Common messages

- **"The formula must use x as its argument."** — Your formula has no `x` (or another
  parameter designated as the argument). A curve needs a variable that runs along the axis.
- **"Enter a formula for the curve."** — The Expression field was empty.
- **"The formula could not be understood."** — A syntax error (typo, unmatched bracket, or an
  unknown function). Check it and try again.
- **"The formula cannot be evaluated at its starting values…"** — With the current starting values the
  formula produces an infinite or undefined result — most often a parameter used as a denominator (a
  width) left at 0. Give it a non-zero starting value in the properties dialog and retry. This is caught
  at creation so the curve cannot silently fit from a degenerate shape.
- **"Calculation error…"** during a fit — the model could not be evaluated with the current
  values; usually a starting value needs adjusting (for example a width that must not be 0).

Curves you create are saved and reappear next time you start Fit. To remove one, use
**Model → Curve Type → User → Delete User Curve**.

Deleting the curve that is **currently being fitted** leaves the model with no curve type,
and Fit says so: select another type under **Model → Curve Type** before fitting again.
Until you do, a fit is refused with a message rather than quietly carried out with the
formula you just deleted. Curves already drawn on the chart were computed from that formula
and stay until the next fit.
