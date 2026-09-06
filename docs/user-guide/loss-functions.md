# Choosing what the fit minimises

A fit works by making one number as small as possible. Which number that is —
the **loss function**, or objective — is yours to choose under
**Fit → Loss Function**.

You do not have to choose. The default is right for most work, and if you never
open this menu the app behaves sensibly.

## The choices

| | What it measures | Choose it when |
|---|---|---|
| **R-factor** *(default)* | Squared deviations, as a fraction of the data | Almost always. Comparable between datasets, so an R-factor of 0.03 means the same thing on two different measurements. |
| **Sum of squares** | Squared deviations, unscaled | You want a raw number rather than a proportion. Finds the *same answer* as the R-factor — only the number printed differs. |
| **Relative deviation** | Absolute deviations, as a fraction of the data | Your data has a few outliers you do not want dominating the fit. Reads directly as "the model is off by this much". |
| **R-factor (legacy)** | Squared deviations, as a fraction of *the model* | Reproducing a result from an older version. Not recommended otherwise — see below. |

### Why "legacy" is not recommended

It divides by the model's own size rather than the data's. When curve scaling is
on, that means the fit can improve the number by making the model **bigger**,
without matching the data any better. For ordinary peak fitting this almost never
happens, because a peak's height starts from the data and stays near it — which
is why it went unnoticed for years. For a model whose amplitude is free, such as
a model whose amplitude is free, it happens immediately, and the app will not let you
pair the two.

## Which points are scored

Whatever you choose, it is measured over your **fit intervals** and nothing else — that is what
they are for. With none marked, the whole profile is one interval, so parts of the series your
model says nothing about are scored too. Several intervals are pooled into one figure.

## When the app overrides your choice

Some combinations cannot be honoured. **The app never does this silently** — it
tells you what it did and why:

- the **status bar** always shows what the fit will actually do;
- a **dialog** appears when a choice you just made cannot be used as selected;
- **greyed-out menu entries** explain themselves when you hover.

Three situations:

**1. The objective is not usable with this curve type.**
Selecting such a model disables *R-factor (legacy)*, for the reason above.
If it was already selected, it switches to the plain R-factor.

**2. Your objective needs the built-in engine.**
*Relative deviation* and *R-factor (legacy)* cannot be expressed in the form the
Python engine solves. If you have selected that engine, the fit runs on the
built-in one instead. **Your objective is still honoured** — only the engine
changes. You lose per-parameter uncertainties, which only the Python engine
produces. Switch to *R-factor* or *Sum of squares* if you would rather keep it.

**3. Your curve type needs the built-in engine.**
Some models compute their points directly instead of from a formula, and the
Python engine fits by evaluating formulas. Same trade-off as above.

## Curve scaling

For a model that sets its own amplitude, curve
scaling is switched off automatically. It fits one overall multiplier for the
whole profile, which duplicates what the model already does; the duplicate lets
the fit flatten the shape while the multiplier absorbs the difference, producing
a plausible-looking number from a meaningless model. This is reported in the
status bar, not as a dialog, because it is not something you selected.

## The statistics do not change

Reduced χ², R², AIC and BIC are always computed the same way, whichever objective
you minimised. So a χ² from one fit stays comparable with a χ² from another, and
AIC/BIC remain valid for choosing between models. Only the *fitted parameters*
respond to your choice of objective.
