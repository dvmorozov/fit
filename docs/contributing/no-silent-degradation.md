# No silent degradation

**The principle, and it outranks convenience:** this program must state its
problems, never hide them. Code that carries on with a value it did not expect —
substituting a default, skipping a step, returning an empty result — produces an
answer that *looks* like an answer. For a fitting tool that is the worst possible
failure, because a plausible wrong number is indistinguishable from a right one
and gets used.

So: **when something is not as the code requires, say so and stop.**

## Refusing an edit that would silently lose work

Moving a picked curve position after its curve has been fitted is **refused**, not
performed. It is worth recording why, because "do it and warn" looks like the
friendlier option and is not.

The fitted values for a curve are stored against the point it was seeded from. Move
that point and they can no longer be found, so that one curve falls back to its
starting guess while every other curve keeps its fitted values. Nothing raises;
the model is simply half-fitted, and the chart looks plausible.

- *Performing it silently* is the degradation this document exists to remove.
- *Performing it with a warning* still destroys the work. A warning about
  irreversible loss is not a mitigation, it is a notification.
- *Refusing* costs a gesture that could not have produced what the user wanted
  anyway - a moved seed is refitted from scratch at the new place, which is
  exactly what deleting and re-adding does, explicitly.

The decision and its wording live in `Server/fit_advice.pas`, with the rest of
this program's "what will actually happen" logic, so the engine and the UI answer
the question the same way. The message says what did not happen, why, and what to
do instead - the three parts every refusal here owes the reader.

## A refusal is not a fault

Saying "no" correctly includes saying it with the right *kind* of error. Over
REST an engine refusal is `400` and logged at `Warning`; `500` and `Fatal` are
kept for the engine doing something it did not intend, and carry a stack trace.

Every refusal used to be a `500`, which told any consumer that the server had
broken and the call was worth retrying unchanged - both false. It also filed
every user being told "no" among the genuine faults in the log, which is the
expensive half: a log where refusals outnumber faults is a log nobody reads when
a fault finally happens.


## No `Assert`

`Assert` is compiled out of release builds. The checks a developer relies on to
catch a broken invariant are therefore exactly the checks *absent from the build
users run* — a violated invariant that would stop the program on a developer's
machine instead runs on quietly, in the one situation where diagnosing it is
hardest. A check that only runs when it is not needed is not a check.

Use `Common/checks.pas` instead. It is unconditional:

| Use | For |
| --- | --- |
| `CheckThat(cond, 'what was expected')` | any invariant |
| `CheckAssigned(obj, 'the R-factor bounds')` | something that must exist |
| `CheckIndex(i, count, 'the curve list')` | a position in a collection |
| `CheckUnreachable('the loss kind switch')` | a branch that must not happen |

Every one **logs at the point of failure and then raises**. Both, deliberately:
an exception can be caught and reworded several layers up, or swallowed by a
handler written for a different failure, whereas the log entry is written before
any of that can happen and is what reaches a bug report.

### The description is the only thing a bug report carries

It says what was expected *in terms of the domain* — not what the expression
says. `'the fit must have a profile before curves are collected'` is useful in a
log; `'FProfile <> nil'` is what the next line of code already says, and a check
whose description repeats its own condition has no message at all.

| Instead of | Write |
| --- | --- |
| `'fit_service: StartIndex <> -1'` | `'the first background point must fall on a sample of the profile'` |
| `'P'` | `'the parameter whose value the column shows'` |
| `'the curve list'` (a noun restating the field) | `'a task must have built its curves before their parameters are read'` |

Four rules, and `tools/build-tests/no_assert.tests.ps1` fails the build on each:

- **no unit-name prefix.** It names the file the log line already names.
- **no operator, `nil` or `Assigned` in the description.** The *condition* may
  contain them; a description that does is a restatement.
- **no bare identifier**, with or without the leading `F`/`A`.
- **more than one word.** A noun is not a sentence.

The reason this is machine-checked rather than reviewed: a bad description is
invisible until somebody reads a log, which is years later, and by then the
information that would have gone in it is gone.

### Not for user error

`EInternalCheckFailed` means *this program is wrong about itself*. A missing
file, an unreachable server, a number typed wrongly — those are ordinary
outcomes, and they get an ordinary message aimed at the user
(`EUserException`). Keeping the two apart is what lets a catch-all written for
one be found and narrowed rather than silently absorbing the other.

## Status of the conversion

**Finished.** No first-party unit calls `Assert`:

| Area | `Assert` remaining | Converted |
| --- | --- | --- |
| `Server/` (the engine) | **0** | 201 |
| `Worker/`, `Common/`, `tests/` | **0** | — |
| `Desktop/Forms/` | **0** | 135 |
| `Desktop/` (client, viewer, misc) | **0** | 141 |
| `Desktop/ModelCurves/` (+`CurveParameters`, `UserPointsSet`) | **0** | 63 |
| `Desktop/DataLoaders/` | **0** | 10 |
| the private domain repo | **0** | 18 |
| `../fitminimizers/` (the minimizer package) | **0** | 43 |
| `../fitgrids/` | **0** | — |

The engine was taken first on purpose: a skipped check there corrupts a *result*,
which the user then acts on, while the same omission in the UI at worst
mis-draws something the user can see is wrong.

**THE SIBLING PACKAGES COUNT.** They are the same author's units linked into the
same binaries, so a rule that stopped at this repository's boundary was not the
rule: `fitminimizers` held 43 `Assert`s while both checks reported zero, fifteen
of them in `SimpMath`'s lineshape functions, which the engine calls on every
evaluated point. That package cannot use `Common/checks.pas` - it `uses log`, and the package must
build standalone - so `MyExceptions.pas` carries the same three routines and
logs through a **sink the host fills**:

```pascal
// MyExceptions.pas, in the package
var OnCheckFailed: procedure(const AMessage: string) = nil;

// Common/checks.pas, in its initialization - one place, not one per program
MyExceptions.OnCheckFailed := @LogPackageCheckFailure;
```

Copying the logger instead would have put two of them in one process: two size
limits and two rotation policies, which aimed at one file is interleaved writes
and doubled rotation dropping lines, and aimed at two splits a single fault's
trace across both. It would also have relicensed 350 GPL lines into an MPL
package and dragged `Windows`/`Shfolder` into it.

Wired from an initialization section because four binaries link this and a line
in each `.lpr` is a line the fifth forgets - `checks.pas` is already linked into
every one of them, so the wiring happens by the act of linking.

`checks.EInternalCheckFailed` is an **alias** of the package's class, not a
second class of the same name: `on E: EInternalCheckFailed` would otherwise
catch whichever one the uses clause resolved and silently miss the other.

Two things finished it, and the second matters more than the first.

**The flag that was propping it up is gone.** Nine `<IncludeAssertionCode>`
entries and two `-Sa` flags used to force assertions on in every build, which is
why debug and release behaved alike — by configuration, in nine places, any one
of which could be dropped. One had been: the plain-FPC command line was missing
`-Sa`, a dozen precondition tests silently stopped refusing, and 556 guarded
lines vanished from the coverage denominator. Now the checks are unconditional
and the flags are removed, so a stray future `Assert` is inert in *both* builds
— visibly wrong everywhere rather than load-bearing in one.

**The same flag exists in Python, and the sidecar is held to the rule too.**
`python -O`, and `PYTHONOPTIMIZE` in the environment, delete every `assert`
statement from the bytecode - the identical failure, in the identical shape, in
the other language the compute path is written in. No first-party module uses
one, and the build test looks for it. It also stopped naming extensions when it
looks for `-Sa`: a list of six could not see the `.lpi` field the Lazarus
options dialog writes, nor the workflows that build in CI, so it now reads every
text file except this documentation - which discusses the flag on purpose and
builds nothing.

**The descriptions were written, not deferred.** The first pass converted
`Server/` with the expression as the description and recorded that as debt; this
one rewrote those 71 and wrote the 349 new ones as sentences, because the string
is the whole point of the change. The rule above is now a build test.

Three dead handlers went with it: six copies in `Server/fit_server_proxy.pas`
and one in `TFitTask.SetSpecialCurve` caught `EAssertionFailed`, which nothing
raises any more, under comments claiming a behaviour that had not happened since
the conversion. `tests/testcase_task_preconditions.pas` and
`testcase_fit_client_view.AProxyWithNoStubRefusesEveryCallback` now hold the
behaviour those comments misdescribed, and both check *which* exception class
arrives — a refusal reworded as `EUserException` would say the user was at fault
for a defect.

## Fixed under this principle

- **A fit with nothing placed built one pattern per data point.** With no wave
  bounds and no positions, curve creation fell through to the automatic mode,
  which treats every data point as a position — 101 patterns, each fitted, which
  the user experienced as the application hanging. Now refused with a sentence
  saying how to place a pattern.
- **A fit interval containing no wave was fitted anyway.** The same trap as
  above, reached the other way: with several intervals marked, one holding none
  of the wave bounds found nothing to build, fell through to the automatic mode
  and produced ~1200 free parameters. `TFitService.RefuseIntervalsWithNoWave`
  names the offending interval and refuses. It runs when a **fit starts**, not
  when the intervals are set — `CreateTasks` also runs on ordinary state changes,
  so refusing there would forbid marking an interval before drawing the pattern
  in it, which is a normal order of work. `TFitTask.RefuseUnfittable` carries the
  same distinction and defaults to True, so refusing is the default and only the
  preview path opts out.
- **Seeding a curve at every data point was offered for wave patterns.** It is
  the right start for peaks — their amplitudes come from the data and the extras
  are pruned — and meaningless for waves, which are placed by their bounds and
  never pruned. `SelectAllPointsAsCurvePositions` now refuses for wave patterns
  and says how to place one; the behaviour for peak-like curves is unchanged.
- **A clamped optimiser step was invisible to the algorithm.** Not a message but
  the same principle: the minimiser was told a parameter had moved when the model
  had refused the move, so it reasoned about a simplex that did not exist. It now
  records what the parameter actually became — see `minimizer-comparison.md`.
- **A curve type that was not linked into the binary was selected silently.**
  See `curve_type_registration.pas`; start-up now names what is missing.
- **An unregistered curve type is refused** by `SetCurveType` rather than
  leaving the previous selection in place.
- **A candidate wave count that cannot be scored** is marked, not ranked as
  merely poor — "could not be evaluated" and "evaluated and found bad" must
  never look alike.
