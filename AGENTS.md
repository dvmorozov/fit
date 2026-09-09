<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Working on Fit — a guide for AI agents

Most future development on this codebase will be carried out by AI agents, so
this file is a first-class deliverable rather than a courtesy. It
covers what you **cannot** read off the code: the invariants, the reasons behind
the non-obvious decisions, and the direction of travel.

Read this, then [`docs/contributing/architecture.md`](docs/contributing/architecture.md)
for the diagrams.

## What this project is becoming

Not a diffraction peak-fitting app that also does other things. A **framework for
experimentation across application domains**. It ships one domain (diffraction)
and an example module, and it is built so that the next domain costs a directory
and one search-path entry - which is how the domains that are not published here
are carried.

Judge every change by that: *what does this cost the person who adds the next
domain?* An implementation that is slightly more work now and removes an edit
from every future extension is the right trade — that is what the capability
model buys.

## Non-negotiables

Breaking any of these will pass review-by-vibes and fail in production.

1. **The desktop client contains no fitting engine.** `strings Fit | grep -x
   TFitTask` must find nothing.
2. **`fit_server` is the only client-facing endpoint.** New backends go *behind*
   it. The client never talks to the Python sidecar; `fit_server` owns it as a
   child process.
3. **Additive changes by default.** Existing curve types, menus and results must
   behave identically unless the change is explicitly about them.
4. **The app must run with zero Python.** The sidecar is optional; its absence
   degrades cleanly.
5. **Capabilities, not enumeration.** Never write a table of "type X works with
   feature Y". Have each side declare a property and derive the answer once.
6. **Reuse the existing architecture. Extend it only when absolutely necessary.
   Never invent a parallel bypass.** See the next section — this is the rule that
   has been broken most often, and most expensively.
7. **Any override the user can notice must explain itself in the UI**, through
   `Server/fit_advice.pas` — the same function the engine uses to decide.
8. **The build orchestrator runs on Linux, macOS *and* Windows — every module of
   it, always**, including a task you personally only ever run on one of them.
   Its libraries are dot-sourced into a single scope, so *loading* one must not
   assume a platform either: a platform-specific expression at a library's top
   level raises while the script is still starting, and takes every task with it.
   Name no tool that is absent on one of the three — `sh` and `bash` and `id` on
   Windows, `python3` wherever the interpreter is `python.exe` — resolve it
   instead. The contributor build guide carries the standing hazards, the
   resolver for each, and the test that enforces them.

## Before you add a mechanism: read the one that exists

The most expensive mistake made in this codebase, repeatedly, and always the same
shape: building outward from code you have just written instead of reading what is
already there.

**The rule:** before writing any new unit, verb, wire contract or record, find how
the app already does the analogous thing. For anything crossing the client/server
boundary, **read `Desktop/http_fit_service.pas`'s implementation of the nearest
existing verb first** — that is where the truth about what actually reaches the
client lives.

Worked examples, all real:

| Invented | What already existed | The actual gap |
|---|---|---|
| a new wire contract, to carry a module's per-curve metadata | `GET /curves` had **always** sent every curve's parameters, and `GetCurveAttributes` had always rebuilt them client-side | `value` is a JSON number, so a GUID arrived as `0`. **One field**, `kind`, saying what `value` holds — mirroring the existing `GetCurveParameterError` beside it |
| a `decompose-leg` verb | `Get`/`SetWaveBounds` | nothing — read, append, write back |
| a sequential numeric id, "because a curve parameter holds a double" | the design specified a **GUID**, and `Value_` was already a string on disk | the `FloatToStr`/`StrToFloat` pair in one container |
| nothing — but worth the opposite warning: a curve's own identity is **not** a parameter | a parameter is a quantity of the model, and `value` is a number | the handle is its own field beside `params`, and all three curve routes address by it |

**The tell:** if a new contract needs a *join key* back to an existing one, it is
probably a bypass of that existing one. Stop and extend the original.

**A second tell:** a feature that works in tests but not in the app. Every instance
here — identity never issued, identity unstable across rebuilds, the overlay never
drawn — was correct code that the production path never reached, with green tests
throughout. One test through the real surface (REST) beats several through
in-process objects.

## Things that will mislead you

Hard-won; each of these cost real debugging time.

- **Mixed line endings.** Several files are CRLF, some mixed. A scripted
  replacement built with `\n` **silently matches nothing** and reports success.
  Always assert your replacement count, and try both variants.
- **`grep` can lie about non-UTF-8 files.** Use `LC_ALL=C grep -a` when a file
  may be ISO-8859 — plain `grep` reported "no modifications" on a file that was
  demonstrably modified.
- **`Desktop/Fit.lpi` has three build modes** - "Any platform", "Windows
  specific", "Linux Qt6". Patching one leaves the app uncompilable while every
  test passes.
- **The projects compile in DELPHI syntax mode, where `@Routine` is
  assignment-compatible with any procedural variable.** So a seam wired with the
  wrong signature compiles in silence; under `{$mode objfpc}` the same line is
  an error naming both signatures. This shipped: `Fit.lpr` passed `@FileExists`
  as a one-argument existence check, bound the two-argument UnicodeString
  overload, and the last project could never be reopened. Every `.lpr` now
  declares `{$mode objfpc}{$H+}`, and `tools/build-tests/syntax_mode.tests.ps1`
  keeps it that way and forbids taking the address of a routine this codebase
  does not declare wherever the strict mode is not in force. In a Delphi-mode
  unit, wrap an RTL routine in a named function with the signature written out -
  `recent_project.DefaultPathExists` is the shape - and have one test call the
  seam the way the application does. The same trap had already been caught once
  on `DefaultSourceNotice`.
- **Untyped real constants are `extended`, not `double`**, which breaks exact
  comparisons. Type them explicitly.
- **`ICurveTypeIterator.EndCurveType` means "is this the last item"**, not "end
  iteration".
- **`TFitService.AddPoint` is add-or-TOGGLE.** A coordinate that is already in
  the set is REMOVED. That is the interactive gesture - the user clicking the
  same sample twice - and the client mirrors it. A bulk write must use
  `SetPointUnique` instead, which keeps the same one-pick-per-abscissa rule
  without the toggle: routing a bulk set through `AddPoint` makes a duplicated
  input coordinate net to ZERO points rather than one. Do not simplify the two
  onto each other.
- **The window checks itself, and `-Task check-ui` is how you ask.** It
  measures every caption against the control holding it and compares every
  command against the two surfaces that draw it, inside the running
  application - the only place real widgets, real fonts and real model state
  exist. It now builds the client every time: it did not, and measuring a stale
  binary cost three wrong conclusions in one session. The switch is `/CHECK_UI`
  (it was `/CHECK_LAYOUT` while captions were all it looked at); there is
  deliberately ONE of it, so no build can run half the checks.
- **A menu built and never attached is invisible, and nothing reports it.**
  `PopupViewMode` was populated, declared checkable and kept ticked for years
  with no control's `PopupMenu` naming it. Assign it, and assert the assignment
  where you make it.
- **Build artefacts under `tools/lib/` are tracked in git.** Rebuilding dirties
  the tree; commit source directories explicitly rather than `git add -A`, and
  beware that `git stash` will refuse to pop over them.
- **A model may hold curves of DIFFERENT types, so nothing may size itself from
  `Items[0]`.** A module can register several curve types and they can coexist in
  one model. Two types from one module can differ in parameter count (15 against
  19) and, at equal counts, in parameter name (`k5` against `c5`). Anything presenting
  curves side by side must key on the parameter **name**, over the union of every
  curve - the parameters grid did it positionally from the first curve, which
  crashed on a count mismatch and, worse, silently showed one curve's value under
  another's heading when the counts happened to agree.
- **A pick set is model INPUT; a fit may only DELETE from one.** The picked
  curve positions carry unique x values that are real samples of the profile, each
  one the seed its curve is rebuilt from and each one carrying the **handle** that
  curve's fitted values are handed back by. Writing fitted values back into them
  breaks the uniqueness and the grid lookup at once, and reports it as a crash
  several gestures later. What the model was built into belongs in a derived,
  read-only set (`GET /calc-positions`).

  A pick may be added, deleted **or moved**: the handle is issued, not derived
  from the seed, so a move rekeys and the curve keeps the shape the fit found.
  The one deletion a fit itself performs is `AdoptCurveRemovalsFromTasks`, so an
  automatic run's reduction survives the next edit; it never moves or adds.
  A module's markup is still refused a move - it places every instance at once,
  so there is no correspondence to carry (`fit_advice`).
- **`TFitTask` can be constructed through the inherited `TComponent`
  constructor**, which leaves fields zero-initialised. Whatever `0` means is what
  unconfigured code silently gets — choose your constant values accordingly.

## How to work here

**Red-green-refactor, without exception.** Write the failing test first, watch it
fail *for the reason you intend*, then write the smallest code that makes it
pass. A test written after the code tests the code you wrote, not the behaviour
you meant - and this codebase's recurring failure is a green suite over a path
the user never takes. Where a change genuinely cannot be driven by a test, say so
and say why, in the commit; do not skip it quietly.

**Every new non-UI method is covered.** Not "the unit is above the baseline": the
method you just added has a test that fails when you break it. The ratchet gates
a *drop*, and it cannot see new code arriving uncovered underneath a rising
average. Check with `-Task coverage-gaps` before you report.

**Every use case gets one test through the surface the user actually reaches.**
One test through REST beats several through in-process objects: every
"worked in tests, not in the app" defect in [findings.md](docs/contributing/findings.md)
was correct code the production path never ran.

**The RED test must enter where the application enters.** This is what makes the
two rules above strict rather than aspirational, and it is the one that was
missing: a failing test that starts in the middle of the pipeline, or fills a
seam with a stub the application does not use, can go green while the real
wiring is never executed - so the red half proves nothing about the feature. So
before writing the test, name the entry point the user's gesture reaches and
call THAT; if a seam has to be stubbed to reach it, the production argument to
that seam is itself untested, and needs its own test calling it the way the
application does.

That is not a hypothetical. `PlanStartup` was covered by thirteen tests that
passed their own existence check in, `Fit.lpr` passed `@FileExists`, and the
feature - reopening the last project - did not work at all while every test was
green. The failing test was written first, and it entered one layer below the
defect.

**The red test lives in the repository the gesture reaches, not the one the fix
lands in.** A feature that spans the framework and a pack has two suites, and
both can be green while the user cannot do the thing at all - the defect is in
the join, and neither half's own contract is wrong. So the failing test goes
where the gesture goes (`-Task test-pro`, through REST for a pack), even when
almost every line of the fix is framework code.

That is not a hypothetical either. Delete curve was reported greyed three times.
The first two were framework defects with framework tests; the third was a pack's
Model-panel row naming no curve, and the deletion not reaching the markup that
rebuilds the pattern - two halves, both correct alone, and the only test that
could go red was an end-to-end one in the pack's suite.

**Extraction is not exempt.** Moving a decision into a function where it can be
tested is the commonest way the red half gets skipped: the new unit test is
written against code that already behaves, so it passes on the first run and
proves nothing. Run it against the OLD behaviour first - it is one edit away -
or state in the commit that you did not and why.

**Tests first, and prefer a UNIT test.** A decision expressed over plain booleans
(see `Server/fit_advice.pas`, `Server/fit_loss.pas`) can be tested exhaustively in
milliseconds. The same logic reachable only through a live `TFitTask` often cannot
be tested at all. If something is hard to test, that is usually a design signal,
not a testing problem.

A test is a **unit** test when it needs nothing outside its own process; anything
that crosses a process boundary, touches the filesystem, or runs the optimiser to
convergence is an **integration** test, and it must say so:
`RegisterTest('unit', T)` or `RegisterTest('integration', T)`. Not "the light
suite" — that is a build flavour, and it is missing seven unit classes while
carrying four integration ones. Only the unit half is measured.

**Logic does not live in UI classes.** An LCL descendant cannot be instantiated
headlessly — sizing a `TStringGrid` with no parent raises "Canvas does not allow
drawing" — so a decision made inside one is unreachable by any test, and UI
classes are excluded from the coverage target for exactly that reason. Put the
decision in a counted module and leave the UI class reading controls and
forwarding. `Desktop/int_ui_host.pas` and `Desktop/int_fit_viewer.pas` are the
seams; `Desktop/pick_target.pas` is the pattern already applied. Logic moved out
lands **with its tests in the same commit** — the coverage gate fails a commit
that moves it without covering it.

**A test is not a use, and the check runs before tests are written.** Whether a
member earns its place is answered by *which production code calls it* - not by
coverage, which says a line ran and not that anything needs it.
`TFitClient.GetCurveList` had two tests over it and no caller; a sweep over
`TCurveListBase`'s public surface produced fourteen tests one commit before the
reference check deleted the twenty-eight members they covered. Run
`python tools/find-dead-code.py` first. Its header carries the seven ways a live
thing looks dead here - registry self-registration and the module extension
points above all - and dead code is **deleted, not lifted**: five methods
computing the chart's extents had no caller and one of them was wrong, so
extracting them would have given thirty unwanted lines their own tests.

**Widen the real type; never add a member for a test.** In order: delete it if
nothing needs it; otherwise move the class from the implementation section to the
interface section, or the method from `private` to `protected`, so the test
drives the same code path production does; only then, as a last resort, a seam -
and it carries a comment saying what it is for, which is what makes it
re-checkable later. Most of what is private here was made private *before the
tests existed*, so it encodes no considered decision. An accessor, an alias or a
`GetX` beside a field is not respecting encapsulation, it is a second way to
reach the same state; and reaching a protected member by cast is the same debt in
a form nobody can grep for.


**Write self-enforcing tests.** Line coverage is measured (`docs/contributing/testing.md`
says what it counts), but it only reports which lines ran. Write tests that walk a
registry and assert every registered thing has what it needs — so adding the next
curve type, rule or loss function *without* fixtures fails CI. These check the
cases that matter rather than the lines that were executed.

**Mocks are plain objects, and you free them yourself.** Everything compiles
`-SIcorba`: CORBA-style interfaces, so there is no `IUnknown`, no reference
counting and no cast back to `TObject`. A mock descends from `TObject` — never
`TInterfacedObject`, whose refcounting is inert here and whose presence reads as a
lifetime guarantee that does not exist — exposes `AsObject`, and is owned by the
fixture, which holds both references, nils the interface first in `TearDown` and
then frees the object. Getting this wrong is silent.

**Put a seam in front of the socket, not a test around it.** A class that is
mostly marshalling around one network or process call is untestable while that
call is built inline — and four of them were. Each now declares a
`protected virtual` transport method a double overrides (`Fetch`/`Send`,
`Post`, `Get`), or had its decisions lifted into a unit that touches nothing
(`Worker/sidecar_launch.pas`). Follow that shape rather than reaching for an
integration test: the decision goes where a test can call it, the syscall stays
behind the one method the double replaces, and the timeouts stay with the adapter.

**A thread is not a reason something cannot be tested.** `TThread.Synchronize`
called from the main thread runs its method inline, so a thread class can be
created suspended, never started, and driven directly — which is how a dropped
callback assignment in `TMainCalcThread.SetSyncMethods` was found after years.

**Name tests after the invariant they defend.** A failure should read as the
violated rule. `AbsoluteEqualsDeviationPlusChord` beats `TestCalcValue2`.

**Comments explain WHY.** The code says what it does. Record the alternative you
rejected and the reason — that is what stops the next agent re-litigating a
settled decision, and it is the single highest-value thing you can leave behind.

**Diagrams are part of the change, and nothing is drawn by hand twice.**
Architecture-level pictures explaining *why* are Mermaid inside
`docs/contributing/architecture.md` — edit those directly, in the same commit as
the code. Everything on the published site is **generated** by
`scripts/gen-diagrams` from the registry dump and from the sources themselves, and
regenerated by `-Task diagrams`, `-Task preview-sites` and every `-Task publish`.
There is no export step and no copy into `gh-pages` any more: rename a class and
the generator refuses to publish rather than shipping a picture that now lies. If
you add a hand-composed figure there (process or threading structure, which no
declaration reports), every class it names is checked against the sources, so keep
the names real.

**Verify before reporting.** Run `./scripts/build-app.ps1 -Task test -Suite unit`
while you iterate, then `-Task test` and `-Task build` before you report; run the
sidecar's pytest suite too if you touched `Worker/py`. Report failures with their
output; never describe unverified work as done.

## Where the reasoning lives

| Question | Look in |
|---|---|
| Why is the architecture like this? | `docs/contributing/architecture.md` |
| How are tests classified, and what does coverage count? | `docs/contributing/testing.md`, `tests/README.md` |
| What do the class/sequence diagrams say? | the generated site — `./scripts/build-app.ps1 -Task preview-sites` |
| How do I add a module? | `docs/contributing/writing-a-module.md` and `Modules/example-linear/` |
| What is in a project file, and what is deliberately not? | `docs/user-guide/project-files.md`; the reasoning is in the headers of `Common/fit_project_document.pas` and `Desktop/fit_project_restore.pas` |
| How do I build this from source? | `docs/user-guide/building-from-source.md` |
| What is planned, and what is settled? | `docs/internal/roadmap.md` |
| What has already gone wrong here? | `docs/contributing/findings.md` |
| Why does the R-factor divide by the observed integral? | `docs/contributing/loss-functions.md` |

## Terminology: "fit interval" (was "R-factor interval")

What the UI calls a **fit interval** is `FRFactorBounds` / `rfactor-bounds` in the code and on
the wire — a stretch of the profile that becomes one independent `TFitTask`. The user-facing
name was changed because the user picks *what to fit*, not an R-factor; the identifiers and the
REST contract were deliberately **not** renamed, so a rename never becomes a wire-compatibility
event. Do not "finish" the rename in the code without a reason to break the contract.

**No fit interval means the whole profile**, not "nothing to fit". `TFitService.CreateTasks`
materialises that default into the bounds so every consumer (sub-tasks, summary table, chart,
statistics) sees one ordinary interval and the user can see what is being fitted. Requiring an
explicit interval is what once made a just-placed model instance invisible.

**Decisions are recorded, not remembered.** If you make a call that a future
session could reasonably reverse, write it down in the same commit — in the
relevant doc, and as a comment where the code lives. A decision that exists only
in a conversation transcript is already lost.

## What a module may and may not touch

A module is a directory plus one registration unit; a search-path entry is the
only thing that puts it into a build. The rule that keeps that true:

**The framework may not name a module. A module may only use published contracts.**

| A module may | A module may not |
|---|---|
| subclass `TNamedPointsSet` and answer its capability methods | add a branch to `fit_task`, `fit_service` or `form_main` |
| register a loader, backend, minimizer, loss, action, UI module, sidecar route | add a `{$IFDEF}` fork to a shared unit |
| serve `/problems/{id}/modules/{vendor}/{resource}` | add a verb to `int_fit_service` |
| ship its own copy of `app_modules` / `module_tests` | edit the framework's copies |
| bring its own point set through `IModulePointSink` | route paired picks through the shared `AddPoint` |
| declare a menu as data through `IUiModule` | name an LCL type |

If a change seems to require an edit to a framework file, the seam is missing -
add the seam, not the branch. Every hardcoded `if`-chain that used to decide these
things is now a registry, and that is not decoration: it is what lets a module
live in a repository this one has never heard of.

**No file exists in two repositories.** That is what makes "never merge back and
forth" true rather than aspirational.

**A module repository carries its own `AGENTS.md`.** If one is checked out beside
this tree, read that file as well: a module's invariants belong with the module,
and cannot be recorded here without the framework naming it. Nothing in this file
assumes that any module is present.

**This repository is published as a snapshot** - one orphan commit, force-pushed
from a development tree kept privately. The history here is not the history upstream, and an
external pull request cannot be merged directly.


## Direction of development

Do not start these unprompted, but let them shape design:
[roadmap.md](docs/internal/roadmap.md) has what is planned, what already
exists towards it, and - separately - what is settled and must not be reopened.

Before changing anything, read [findings.md](docs/contributing/findings.md). It
records the defects that cost real time and the invariants that look arbitrary
and are not. This codebase's recurring failure is a green suite over a path the
user never takes, and that document is the record of every instance.

**A note on scope.** This codebase has a long memory and a small number of very
deliberate decisions. When something looks wrong, it is worth one round of "why
might this be intentional?" before changing it — the model-normalised R-factor
looked like a plain bug and turned out to be a sound intent with a narrow defect,
and the difference mattered to the fix.
