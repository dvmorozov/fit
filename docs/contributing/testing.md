# Testing, and what coverage counts

This page is the policy. [`tests/README.md`](../../tests/README.md) is the
mechanics — how to run the suites and how to add a test.

## Test first, always

Red-green-refactor is the rule, not a preference: the failing test comes first
and has to fail for the reason you intend before any code is written to pass it.
A test written afterwards tests the code that exists rather than the behaviour
that was wanted, and the failure this project keeps producing is a green suite
over a path the user never takes.

New non-UI code is expected to be **fully covered** - the method you added has a
test that fails when you break it. The ratchet below gates a *drop* in the
figure, so it cannot see new uncovered code arriving under a rising average;
`-Task coverage-gaps` is what answers that question. `AGENTS.md` carries the same
rule for agents working here.

### The red test enters where the application enters

This is the clause that makes the rule strict, and it was learned by breaking it.
A failing test that starts in the middle of a pipeline - or that fills a seam
with a stub the application never uses - can go green while the wiring that
actually runs has never been executed once. The red half then proves nothing
about the feature; it proves the layer under it.

So before the test is written, name the entry point the user's gesture reaches,
and call that one. Where a seam has to be stubbed to get there, the production
argument to that seam is itself untested, and it needs a test that calls it the
way the application does.

**What this cost.** `recent_project.PlanStartup` decides which project opens at
start-up and had thirteen tests, each passing its own existence check in. The
application passed `@FileExists`, which is not a one-argument function, and the
feature - reopening the last project - did not work at all, on any run, with the
whole suite green. See `findings.md`, "The sixth gap".

### A feature that spans two repositories is red in the pack's suite

The framework and an analysis pack are separate repositories with separate
suites, and a defect in the join between them makes neither suite fail: each half
honours its own contract, and the user still cannot do the thing. So the failing
test is written where the **gesture** is - `-Task test-pro`, over REST, in the
pack's tests - however much of the fix turns out to be framework code.

**What this cost.** Delete curve was reported greyed three times. Twice it was a
framework defect, found and fixed with framework tests. The third time a pack's
Model-panel row named no curve - a pack identifies its rows by its own markup -
*and* deleting the curve did not reach the markup a pattern is rebuilt from, so
enabling the entry alone would have shipped a Delete that visibly did nothing.
Both repositories were green throughout. See `findings.md`, "Delete curve, greyed
for the third time".

### Extracting a decision does not exempt it from the red half

Moving a rule out of a place no test can reach is the commonest way red-green
quietly becomes green-green: the new unit test is written against code that
already behaves, passes on its first run, and demonstrates nothing. Make it fail
first - against the old behaviour, which is one edit away - or say in the commit
that you did not, and why.

## Two suites, one rule

Every test class declares which suite it belongs to, and the criterion is
**dependencies, not speed**:

```pascal
RegisterTest('unit', TMyTest);          // needs nothing outside its own process
RegisterTest('integration', TMyTest);   // needs something outside it
```

A test is an **integration** test if it depends on a **process boundary** (a
compute server, HTTP, the Python sidecar), on the **filesystem** (a fixture, a
settings file, a data file), or on **running the optimiser to convergence**.
Everything else is a unit test.

A file counts as an external dependency exactly as a socket does — it can be
missing, stale, or left behind by whatever ran before. An earlier version of this
rule kept fixture-reading tests on the unit side because reading a sample file is
fast; that was the wrong test, and speed is not the criterion.

```sh
./scripts/build-app.ps1 -Task test -Suite unit
./scripts/build-app.ps1 -Task test
```

**Classifying is not optional.** `tests/testcase_suite_split.pas` fails the suite
when a class registers into neither half. It has to, because the failure it
prevents is silent: an unclassified test does not error, it drops out of
`--suite=unit` — the half coverage is measured over — and the number goes on being
reported for a suite that quietly stopped running part of itself.

## What coverage counts

The denominator is **this project's own logic**. Nothing else is in it, and the
exclusions are listed rather than inferred:

| Excluded | Why |
|---|---|
| Third-party and separately published component code carried in the tree | A figure that moves when somebody else's library grows is measuring the wrong thing |
| The LCL, the RTL, LazUtils | Not ours |
| **UI wrapper classes** | The boundary layer of the LCL, excluded on the same grounds as the LCL itself — see below |
| The test code itself | The suite measuring itself |

Everything that remains is in the target, and the target is **100 %**.

### What stands between the figure and 100 %, measured

The target is the aim; it is not a prediction. This section says where the gap
actually is, with numbers, so the next person reads the figure correctly instead
of re-deriving it.

**`tools/coverage/baseline-project.md` is the authority for these numbers, not
this page.** The table below is a snapshot for reading; the baseline is what the
ratchet gates on, and it carries the same per-repository headings. If the two
disagree, the baseline is right and this paragraph is stale — which has happened,
so it is worth checking rather than quoting.

| | covered | of | gated half |
|---|---:|---:|---|
| `fit` — this repository | 10128 | 11336 | **89.34 %** |
| `fitminimizers` | 1388 | 1450 | **95.72 %** |
| the private curve pack | 3022 | 3389 | **89.17 %** |
| **total** | **14538** | **16175** | **89.88 %** |

These are the **unit half**, which is what the ratchet gates on. The union of both
suites — the figure this project reports — is **15168/16175 = 93.77 %**, measured
with the union task over the same sources.


**Judge what is left by the METHOD, not by the line**, and
`tools/coverage/rescan.py` will do it for you over a union report. At the time of
writing it answers: **two** methods have never been entered by any test.
`TServerCallThread.Create` (7 lines) is out of reach by construction — declared in
an implementation section, so no test can name it, and it completes through
`Synchronize`, which needs a message loop no test binary has. A domain module's
session `Kind` (4 lines) returns a constant and would need that module's session
stood up around it.

Everything else uncovered is one of three things, and none of them is answered by
writing another test of the same kind:

- **A branch inside a method the tests already enter** — an error path, an
  alternative condition. This is the bulk of it, and a good deal of it has now
  been done: the settings route's nine fields, the pruning guards, a curve
  dropped when its position is removed, the position and bounds algorithms over a
  profile with a peak in it, the loader's refusals. What remains here has to be
  constructed INSIDE the engine rather than asked of it — a stored-values list
  missing an instance the model says was fitted, a markup document malformed in
  one specific way — and no REST call can express those. They want fixtures that
  build engine objects directly.
- **A syscall deliberately left behind a test double.** `python_sidecar.Fit`'s
  HTTP POST is the clearest case: the decisions around it were extracted precisely
  so the transport could stay unreached.
- **Line-table noise.** Some lines are attributed to code that is not theirs,
  which makes 100.0 % unreachable by construction. [findings](findings.md) carries
  the worked example, in which a method nothing calls has three of its lines
  reported as covered.

What follows is still the shape of the gap, and still where the effort would go.

**`fit_service.pas` and `fit_task.pas` hold 629 of the 1850 missing lines** — a
third of the whole gap in two units. The operations behind their routes run the
optimiser to convergence, which is an integration test by the rule at the top of
this page; what covers them is the route dispatch that does not. Excluding those
two units, this repository stands at **7830/8608 = 90.96 %**.

**The rest is code the doubles replace.** A mock transport stands in for the HTTP
client, a scripted sidecar for `TProcess`, a scripted host for the window. The
real bodies underneath are one syscall each and the integration suite exercises
them.

**AND THE REMAINDER IS NOT A BACKLOG.** Three scans of different shapes — class
methods ranked by how much of their class they touch, free functions in
implementation sections, and the wrapper units ranked by how much they still
decide — agree that no module of substance is left to extract. What holds the
remaining lines is the optimiser, the sidecar process, the sockets, the worker
thread and the filesystem, each of which the integration half exercises and the
measurement deliberately does not. Reaching 100 % from here would mean measuring
the integration suite too, which is a different decision from writing more tests;
`findings.md` records how that conclusion was reached and what it cost to reach
it.

**A WARNING FROM HOW THIS NUMBER LAST MOVED.** It went from 79.25 % to 82.76 % in
one commit that wrote no test at all: the private pack's REST tests were *all*
registered `integration`, and only four of the sixteen ran a fit. Splitting them
by the rule already on this page — a shared fixture, a `unit` half and an
`integration` half — moved twelve tests taking 0.078 s out of a half that takes
33 s, and lifted that pack's session unit from 25.60 % to 63.14 %, its builder
from 12.72 % to 63.60 %, and `fit_task` from 64.87 % to 76.32 %.

None of that code was untested. It was tested in the half nobody measures. So
before concluding that a unit is out of reach, check what is already exercising
it and where that test is registered — the framework's own REST file carries the
same note for the same reason.

### Why UI classes are excluded, and what that obliges

An LCL descendant cannot be instantiated headlessly at all. This is not a
preference: sizing a `TStringGrid` with no parent raises *"Canvas does not allow
drawing"* before a single cell is written, whichever way the default row height
and column width are pinned first. [`findings.md`](findings.md) records the whole
investigation.

So the way to cover logic that lives in a UI class is to **move it out**, into a
counted module — not to find a cleverer harness. A class qualifies as an excluded
wrapper only when **both** hold:

- **it is the boundary itself.** Any of three shapes counts: it descends from an
  LCL type; its interface section names LCL types; or its implementation's only
  work is delegating to an LCL form or dialog, or to the global application
  object — the same boundary reached through a global rather than through
  inheritance. **And**
- it contains **no decisions** — no branching on application state, no arithmetic,
  no parsing, no formatting. A method reads controls, calls one thing, and writes
  controls back.

The third alternative in the first clause is the one that needs explaining. A
class-adapter written so that something else can be tested implements an
**LCL-free interface** — that is its whole purpose — so its interface section
names nothing from the widget set and it descends from `TInterfacedObject`. Its
body shows a modal dialog, or reaches into the main form, or calls `MessageDlg`
on the global application, and none of that runs headlessly. Counting such a unit
put permanently unreachable lines in the denominator and added a unit every time
an adapter was written to make something else testable — penalising exactly the
move this list exists to encourage.

An adapter is **not** exempt from the second condition. One that decides anything
is still counted, and the decision still has to come out. The check on this is
`curve_type_parameters_factory`: it looked like one of the adapter family and
turned out to be pure, so it was tested instead of listed.

The exclusion list is a **debt register, not an amnesty** — a UI class holding
logic owes it to a counted, tested module. Two things keep that honest:

1. **Its total line count may only shrink.** Extraction moves lines out; nothing
   may move lines in. A decision parked behind the exclusion fails the build.
2. **Extracted logic lands with its tests, in the same commit.** That makes
   extraction monotone: moving *N* lines into a counted module and covering all
   *N* gives `(hit+N)/(total+N)`, which is never below `hit/total`. The figure can
   only move toward 100 %, so a commit that lowers it is by construction a commit
   that moved logic without covering it — and that is what the gate reports.

### The one time the wrapper total has grown

The project file added a File menu, two dialogs, a dozen `IProjectHost` members
and three export seams to `form_main`. No decision came with them - the sequences
and rules they serve are in `project_workflow`, `project_commands`,
`project_ui_context`, `recent_project` and `table_export`, none below 92 %. The
running total is in `baseline-project.md` rather than quoted here, because a
number in prose goes stale and this one did within a day.

**Three of those units exist because the gate reported the growth**, and the third
is the one worth remembering. The New/Open/Save/Save-As sequences were methods of
the form until it did. The export name loop had never been reachable by any test -
the questions in `table_export` were tested, the loop around them was not, so
nothing checked that "No" to Overwrite asks again while Cancel does not. And
`project_ui_context` was written after the growth prompted a reading of what was
still in the window, which found three defects nothing had failed on: the selected
interval was never saved, the user-defined formula was never saved, and the
working context was written to every project and read back from none.

The lesson is worth keeping: the rule is about what KIND of line lands in the
excluded group, and enforcing it as a line count is what makes it noticeable. A
new user-facing feature cannot be added to a window without adding boundary lines
to that window, so the total will move again; what must not move is the answer to
"did a decision land here".

### How much of that debt is left

**Thin, and spread out.** The excluded set stands at 3165 lines, of which
`form_main.pas` and `fit_viewer.pas` are about 2600. Both delegate heavily: between
them they call fifteen extracted units.

Counted rather than guessed — decisions (`if`/`case`/`while`, comments stripped)
in the largest wrapper, minus `Assigned` guards, `csDestroying` guards, and
conditions that are calls into rules already extracted — **143 remain, in 82
methods, about 1.8 each**, and they sort into four kinds:

| what is left | roughly |
|---|---:|
| widget state: which tab is active, is a menu open, is this control a grid | 41 |
| calls into rules extracted already, and their results | 22 |
| what the user answered a dialog | 6 |
| range and emptiness guards | 6 |
| the rest: small per-method conditions, each in one place only | 58 |

**A decision-density scan is misleading**, and the mistake is easy to make twice.
It ranks by what a method *reads*, and a method full of calls to tested rules reads
exactly like a method full of untested ones: the top hits have twice now been
methods that already delegate (`SaveTableAsText`, `ApplyViewMode`, `CheckState`,
`AimPickAtActiveSerie`) or that genuinely build widgets and should stay
(`BuildRightPanelTabs`). Read the hits before believing the number.

**And a wrapper's line count is not its debt.** Extracting the whole conversation
about unsaved work out of `FormCloseQuery` — every rule about what "No" means,
what Cancel cancels, and what a failed save must prevent — took *2 lines* off the
wrapper, because two nested helpers took their place. The gain was that the rules
became reachable by a test. Judge an extraction by that, not by the number it
moves; [findings](findings.md) records the arithmetic.

The seams for this already exist and are worth using rather than reinventing:
`Desktop/int_ui_host.pas` (`IUiHost`) and `Desktop/int_fit_viewer.pas`
(`IFitViewer`) were written precisely so the client's logic could be driven
without a form or a chart. `Desktop/pick_target.pas` — plain pixel arithmetic
lifted out of the chart click handler, with a test over it — is the pattern
already applied.

### The transport seam

A class that is mostly marshalling around one network or process call cannot be
tested while that call is built inline. Four of them now declare a
`protected virtual` method a test double overrides - the HTTP client service, both
remote fit backends, and the sidecar launcher, whose decisions were extracted into
a unit of their own. Between them they went from near zero to two thirds covered
without a socket being opened anywhere.

Where to draw the line: the **decision** goes where a test can call it, and the
**syscall** stays behind the one method the double replaces. Which paths to try is
a decision; asking the disk is not. The timeouts belong to the adapter rather than
to its caller, so they stay on the real side.

Threads are not an obstacle either: `TThread.Synchronize` called from the main
thread runs its method inline, so a thread class can be created suspended, never
started, and driven directly. See `tests/README.md`.

### Emptying a UI class: what to take and what to leave

Every extraction so far has had the same three parts, and separating them is most
of the work:

1. **The decision** - a function from values to values, or a small class with its
   own state. It goes into a counted module and lands with its tests.
2. **The widget conversation** - reading a control, writing `Enabled`, adding a
   row to a grid. It stays in the UI class, which becomes a loop over the
   decision's answer.
3. **The accumulator** - a `Tag` bit, a field carrying half a result between two
   methods. It usually disappears: it existed because the decision was split
   across procedures that could not return anything, and a function can.

The third is where the defects live. Bit flags packed into widget `Tag`s were
where six commands failed to be disabled during a fit, where three menu entries'
state was computed and discarded, and where one menu command has been unreachable
for years - none of which any reader could see, because writing an integer to a
`Tag` is always legal. See [findings](findings.md).

**And before writing a test for anything, ask what calls it.**
`python tools/find-dead-code.py` answers that: units nothing reaches from any
project root, and members referenced from no other unit, split into none-anywhere
/ tests-only / own-unit-only. Its header carries the seven ways a live thing
looks dead in this codebase, which is the half worth reading. A test is not a
use, and a sweep over a public surface will happily test whatever it finds - that
is how fourteen tests came to be written one commit before the members they
covered were deleted.


**Read the callers before moving anything.** Five methods computing the chart's
extents turned out to have no caller at all, and one of them was wrong;
extracting them would have added thirty lines nobody wants to the denominator and
given them tests. Dead code is deleted, not lifted.

**Preserve behaviour, and say where it is odd.** An extraction that also fixes
what it moves cannot be reviewed as either. Where the old behaviour is plainly
wrong but the fix changes what the user sees, the test pins it as it is, says so
in as many words, and the finding goes in [findings](findings.md).

### Both halves reported, the unit half gated

**100 % is the target for the union of the two suites** — a line is covered if
any test reaches it. That is the honest question to ask of a suite, and it is the
figure above.

**The ratchet gates on the unit half**, and deliberately does not gate on the
union. The unit half is about 145 billion simulated instructions and eighteen
minutes under callgrind; the integration half is 1.7 **trillion** and nearly two
hours, because it drives real fits to convergence. A gate nobody can afford to
run is not a gate. So the floor that cannot slide is the cheap measurement, and
the union is measured when the reported figure is meant to move.

This page previously claimed the integration half "reaches almost nothing a unit
test does not". That was an assumption, and measuring it showed it was wrong: the
union adds **587 lines, +3.68 points**. It does not land evenly. `fit_service.pas`
and `fit_task.pas` are most of it — the same two units named above as a third of
the gap, because what those routes do *is* run the optimiser. `fitminimizers`
gains nothing at all, which is the same fact from the other side: pure arithmetic
is fully reachable without a process.

**And the measurement is only as good as the environment.** The first union run
reported 91.87 %, because the container could not host what the integration tests
integrate with: no scientific Python stack, so five sidecar tests skipped, and no
compute server built, so the four tests that start one ran a stale binary and
errored. Provisioning both — 162 tests, no errors, nothing skipped — moved the
union by **29 lines, +0.18 points**: real, and much smaller than the nine broken
tests suggest, because the units they touch were already reached by other tests.
Worth knowing in both directions: a skipped test and a covered line look the same
in a percentage, and repairing one need not move it much.

### It is line coverage, not branch coverage

A line counted as executed may have had only one of its branches taken. Do not
report it as if it proved more than that — and do not treat a covered line as a
tested one. A test that runs a line without asserting anything about it moves the
number and defends nothing.

## What coverage is not

This project's recurring failure, recorded case by case in
[`findings.md`](findings.md), is **a green suite over a path the user never
takes**. Coverage cannot see that failure: every one of those bugs was in code the
suite executed.

So the number is a floor, not a goal, and it is checked by things it cannot
replace:

- **Self-enforcing tests.** A test that walks a registry and asserts every
  registered thing has what it needs fails when the next curve type, loss function
  or module arrives without fixtures. Line coverage says which lines ran; this
  says whether the cases that matter exist at all.
- **One test through the real surface.** A feature that works in tests and not in
  the app has been tested through the wrong door. One test through the REST API
  beats several through in-process objects - and the door has to be the one the
  application opens, not the layer below it (see "The red test enters where the
  application enters", above).

## The walk: what only a running window can answer

Every suite here drives objects. The two UI surfaces — the Tools pane on the
left, the Model panel on the right — are generated at run time from one command
table, and what they can get wrong is the coupling *between* widgets: a command
enabled in the pane and disabled in the menu, a button whose pressed state
disagrees with the mode it stands for, a legend row that controls a series other
than the one it names. None of that is a line of code a test can execute; it is
two lines agreeing, and they agree only in a window.

Three defects in this area were found by running the app and by nothing else: a
nil command table during `FormCreate` (which aborts before there is a window, so
it appears as an exit with no message anywhere), 41000 frames of recursion
between the Model panel and its own refresh, and a bulk pick write that
annihilated a duplicate. `-Task check-ui` now answers the first two rows of the
list below by asking the window about itself; the rest is still a person at the
keyboard.

**Run it on the VM, never on this machine.**

- **Both panels exist.** `Tools` and `Data` tabs on the left, `Graphs` and
  `Model` on the right. The `Model` tab is the framework's and is present with no
  module loaded at all — that is the check that the panel does not depend on a
  contributor existing.
- **Nothing open: the two surfaces agree.** Every tool row disabled, and the
  menu entry behind each disabled too. Open a profile and they enable together.
  Disagreement here is the whole reason there is one command table.
- **The curve type is one selection, shown twice.** Pick `Gaussian` in the list;
  `Model → Curve Type` carries the same tick, and choosing from the menu moves
  the list.
- **A picking button holds its state.** `Pick` under Positions stays pressed, the
  menu entry reads `Stop Visual Position Selection`, chart clicks add positions,
  the heading count rises, pressing again releases it.
- **The Model panel fills before any fit** — one row per placed position, not
  only after fitting — and names the fitted parameters after one.
- **Deleting a curve takes its dependents.** Right-click a row → `Delete curve`:
  that curve goes, its series leaves the chart, its legend row goes, its pick
  marker goes, every other curve keeps its fitted parameters and every other
  pick marker stays.
- **Delete from the MIDDLE of several**, then tick one legend row off and watch
  which series disappears. The legend was index-parallel to the chart only by
  luck of construction, and removal is the operation that breaks it — this is the
  one step that would have caught it.
- **Fit, delete, fit again.** The survivors keep their fitted parameters, the
  deleted curve does not return, and the log carries no orphan warning from
  `RestoreCurveValues`.
- **The same sequence with the compute server as the backend.** The in-process
  and HTTP paths must not diverge; that is what the delete route is for.
- **Right-click on the chart adds no point** — check this with a picking mode
  running, which is where the stray point used to land — and opens the axis menu.
  Left-click still picks exactly as before.
- **A row command with nothing selected** is present and disabled, not absent.
- **The renamed dialogs still open and work**: About, Wavelength, Background
  Factor, Max Acceptable Difference, and the keypad's digits.
- **With a module loaded**, the shared panel is the point: an ordinary curve type
  shows the framework's flat list and a type placed from its own markup shows the
  module's hierarchy, in the SAME tab; switching to a markup type with nothing
  marked yet shows the framework's empty text rather than one row per data point;
  and deleting a curve the module drew removes both its curve series and its
  markers while leaving every other pattern's alone.

**Most of these are no longer manual.** `-Task check-ui` starts the client with
`/CHECK_UI` and a data file, and the window checks itself, in four passes with a
verdict line each:

- every caption against the control holding it;
- every command against the two surfaces that draw it - same hint, same
  availability, one button width;
- the Model panel's context menu, over a model the check BUILDS itself: two
  picks and an interval through the same client calls the pane's buttons make,
  the first row that names a curve selected, and the menu asked whether it
  offers anything;
- every legend row against the series it carries, and the picking latch against
  a mode the check enters on purpose - a rule about a running mode is worth
  nothing if no mode ever runs while it is checked.

It builds the client first, runs unattended, and fails on any finding. Every one
of those rules has been verified by breaking it deliberately and watching the
task exit 1 - a hint, a latch, a legend row and the input-order bug that greyed
Delete curve. That last one is why the list is worth extending rather than
trusting: it shipped, and reading the code twice did not find it.

**There is deliberately ONE switch**, and it was called `/CHECK_LAYOUT` while
captions were all it looked at. A second would mean a second task and a second
build step, one of which gets run while the other is forgotten - and nothing
needed separating, since the forms are already built and `/INFILE` has already
opened a profile by the time it runs.

**What stays manual, and why.** Only what needs a pointer: a chart click landing
where it should, a tooltip appearing on hover, the act of right-clicking itself. Driving those with synthetic input would need a tool provisioned
on the machine that runs the app, and would still verify things once rather than
on every commit. The rest of the list is state the window could assert about
itself the same way, and each item added there is one fewer thing that depends
on someone remembering to look.

## Contributing a test

See [`tests/README.md`](../../tests/README.md). In short: register it into a
suite, prefer the unit half, and name it after the invariant it defends rather
than the method it calls — the test name is what a future reader gets instead of
an explanation.
