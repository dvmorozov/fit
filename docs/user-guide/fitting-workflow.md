<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Fitting, then changing the model, then fitting again

Fitting here is meant to be **iterative**. You are not expected to describe the
whole profile correctly before pressing Fit once: place part of the model, fit it,
look at what is left over, add to the model, fit again. Each round starts from
where the last one finished rather than from scratch.

This page is about that loop — what a re-fit keeps, what it recomputes, and the
one edit that is refused.

## The loop

1. **Load the data.** *File → Open*.
2. **Say what to fit, if not everything.** A *fit interval* is a stretch of the
   profile fitted as one independent problem. If you define none, the whole
   profile is one interval — that is the default, not "nothing to fit".
3. **Place some model.** Depending on the curve type, either by picking **curve
   positions** (one per peak), or, for a type that is placed from its own markup,
   by making that markup instead.
4. **Fit.** The status bar shows the elapsed time and the achieved R-factor;
   *Fit Intervals* and *Summary* show the numbers per interval.
5. **Look at what is left.** The **Difference** series is the profile minus the
   model. A systematic bump in it is a curve you have not placed yet.
6. **Change the model and fit again.** Add a position where the difference says
   one is missing, delete one that turned out not to be needed, adjust a fit
   interval. Then fit again.

Step 6 is the point: **the previous round is not thrown away.**

## Two ways round the loop

Every step above is on the menu bar — *Model → Curve Positions*, *Fit → Minimise
Difference* — and the menus are the full inventory of what the program can do.

They are not the quick way round a loop you repeat. The **Tools** tab, on the
left beside *Data*, carries the same commands as buttons: the curve type as a
list, then *Positions*, *Fit intervals*, *Background* and *Fit*, each heading
counting what the model currently holds. The buttons and the menu entries run
the same commands, so it does not matter which you use — a *Pick* button stays
pressed while its picking mode is running, which is the one thing the menu entry
cannot show you.

## Seeing and changing what the model holds

The **Model** tab, on the right beside *Graphs*, lists the model: one row per
curve, with its type and where it sits. For a curve type placed from its own
markup the same tab shows that structure instead, as a hierarchy.

**Right-click a row to delete that curve.** The pick it was placed from goes with
it — the model is rebuilt from your picks, so a pick left behind would put the
curve straight back. Every other curve keeps the parameters the last fit found,
which is the same promise the table below makes for deleting a position.

You can also select whole rows in *Curve Attributes* and press Delete, which
removes those curves the same way.

## What a re-fit keeps

When you change the model, the program rebuilds it and then re-attaches
everything the last fit found. In practice:

| You do this | What happens to the previous fit |
|---|---|
| Add a curve position | Every existing curve keeps its fitted parameters. The new one starts from the data at that point. |
| Delete a curve position | Its curve goes. Every other curve keeps its fitted parameters. |
| Delete a curve | The same thing from the other end: the curve and the pick it was placed from both go, and every other curve keeps its fitted parameters. |
| **Move** a curve position | The curve keeps the width and shape the fit found, and moves to where you put it. Its height is taken from the data at the new point, like a fresh one. Every other curve is untouched. |
| Add or change a fit interval | Curves keep their parameters; the intervals are re-derived around them. |
| Place more markup (a second pattern, say) | Everything already fitted keeps its parameters; the new part starts fresh. |
| Change the curve type | The model is rebuilt from nothing — the parameters of one shape mean nothing to another. |
| Change the loss function or minimiser | Nothing is discarded; the next fit optimises the same model against a different objective. |
| Run **Minimize Number Of Curves** | The curves it decides you do not need are removed for good — their positions go with them, so the next edit does not bring them back. |

So the ordinary way to improve a fit is to add one curve at a time and re-fit,
watching the R-factor. Each round is cheap, because it is only refining what the
last one left.

## Two kinds of position marker

After a fit the chart carries two marker series, and the difference between them
is deliberate:

- **Curve positions** — where *you* picked. These are your input. A fit never
  moves them, so you can always re-fit from your own starting points, or try a
  different loss function against the same seeds.
- **Fitted positions** — where the curves *ended up*. One marker per curve, at the
  position the fit chose for it.

Before you fit, the second series is empty. After a good fit the two sit close
together; a pick whose curve has moved a long way from it usually means that curve
was seeded on the wrong feature.

For a model placed from its own markup rather than from picks, *Curve positions*
is legitimately empty and *Fitted positions* is where the model reports itself.

## The one edit that is refused

**You cannot move a curve position once its curve has been fitted.** Try it and
the program declines, and says why.

The reason is that the fitted values for a curve are held against the point it
started from. Move that point and they can no longer be found: that one curve
would quietly drop back to a starting guess while every other curve kept its
fitted values, leaving the model half-fitted with nothing on screen to say so.
Rather than do that, the program refuses.

**To place a curve somewhere else:** delete the position and add one at the new
place, then fit again. The curve is fitted afresh there — which is all that moving
it could have given you.

Before any fit there is nothing to lose, so moving a position is ordinary and is
allowed.

## When a fit will not do what you asked

Some combinations cannot be honoured — an out-of-process engine cannot evaluate a
curve that has no formula, and a self-normalising objective is meaningless for a
model that sets its own amplitude. The program corrects these rather than failing,
and **it tells you**: the correction and its reason appear before the fit runs.
See [loss-functions.md](loss-functions.md) and
[compute-backends.md](compute-backends.md).

## Related

- [curve-types.md](curve-types.md) — which shape to choose, and how each is placed
- [loss-functions.md](loss-functions.md) — what is being minimised, and how to read
  the result
- [compute-backends.md](compute-backends.md) — native engine, Python sidecar, or a
  remote compute server
- [user-defined-curves.md](user-defined-curves.md) — fitting a shape the program
  does not ship
