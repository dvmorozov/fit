<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Findings

What a fresh reader cannot re-derive from the code: defects that cost real time,
invariants that look arbitrary but carry weight, and traps already hit once each.

Every item here is a thing that actually happened. None is hypothetical.

## The failure mode this codebase keeps producing

**Green tests over a path the user never takes.** It has happened often enough to
be the first thing to suspect when something "works" but nobody can see it.

- **A whole feature was absent from the compute server.** A curve type registers
  from its unit's `initialization`, so it registers only if the unit is *linked* —
  and the test binary linked it either way. `SetCurveType` silently kept the
  previous selection, no curve of that type was built, and the fit fell back to
  auto-mode with one curve per data point. The user experienced it as a hang.
  Guards now: `ExpectCurveTypes` raises at start-up naming what did not link, and
  public CI greps the built binary, not the sources.
- **A view computed on the client from data only the server has.** Over HTTP the
  client rebuilds curves from points alone — no parameters, no instances — so a
  cast to the concrete class always failed and the loop always skipped. In-process
  tests passed, because in process the cast works.
- **A module's pick reached the server and the client never refreshed.** Inlining
  a two-line call dropped the `UpdateComputedData` the method had ended with. The
  server recorded everything; the chart could not change. No error, nothing in the
  log. Guarded now by `testcase_pick_refresh`, which asserts the invariant for
  *every* pick mode rather than the one that broke.

- **Every pick after the first was dropped in silence.** The chart's crosshair
  snaps to the nearest point of *any* visible series; the window compared that
  series with the one being marked up and ignored the click when they differed.
  True while the chart showed data alone - and false from the moment a model was
  drawn over it, which is what marking the first bounded pattern does. No pick, no
  message, no log line, and a second pattern could not be marked at all. The
  decision now lives in `Desktop/pick_target.pas` over plain numbers, where
  `testcase_pick_target` asserts both directions, and the window re-aims such a
  click at the active series instead of dropping it.
- **A module's picking mode stayed ticked after it had ended.** Nothing put the
  entry back when the mode ended some other way - another selection mode, a
  profile loaded - so the entry claimed a mode that was off and the next click on
  it read as "leave". Module toggles are now driven from the polled selection
  mode, like every built-in one (`TFormMain.SetSelectionMode`).

- **The published application depended on an unpublished branch of a dependency.**
  `fitminimizers` was developed on `dev` while its default branch stayed `master`,
  three commits behind. Everything built here, and in CI, because both had the
  developer's checkout - but a visitor cloning the documented way got `master`,
  which lacks the API `Server/downhill_simplex_minimizer.pas` calls, and the build
  died on five missing identifiers. Nothing inside this repository could have
  shown that. Both sibling packages now have a single branch, `master`, which is
  also their default.

**The rule:** a test that does not cross the process boundary proves nothing about
linkage or transport, and a build that does not start from a clean clone proves
nothing about what a visitor gets. Anything that can only fail across it belongs in a suite
that starts a real `fit_server` — see `tests/worker_process_harness.pas`.

## The parameters table assumed every curve was the same shape

Marking a second pattern of a *different type* after a fit took the client down
with `data_classes: Grid.ColCount - Grid.FixedCols = CurveParameters.Params.Count - 1`,
from `SetRowContents` on the next repaint.

`TCurveList` treated the grid as a rectangle whose columns were **positional**,
and sized it from `Items[0]`. That is only valid for a model whose curves all
carry the same parameters in the same order - true of any single curve type,
which is why it held for years and why four separate methods had each grown their
own copy of the same `CheckThat`.

A module may register several curve types, and they may coexist in one model. Two
of them break the assumption in two ways, and the quiet one is worse:

- **Different counts.** A corrective pattern carries 15
  parameters where a motive one carries 19. The check fired, the client stopped
  polling, and the dialog said "Server polling has been stopped" - which reads as
  a compute-server fault and is not one.
- **Same count, different names.** A motive pattern's `k5` is a diagonal's `c5`;
  a triangle has `r5`. The counts agree, so **nothing complained** and the grid
  showed one curve's `c5` under the other's `k5` heading - a wrong number under a
  plausible label. Found only by enumerating every pattern type's parameters
  while chasing the crash.

**The rule:** a column means a parameter, and the only identity that is the same
for every row is the **name**. The columns are now the union of parameter names
over every curve, in first-seen order (`CollectColumnNames`), and a curve that
lacks one gets a **blank** cell rather than 0 - which would read as a value it
holds. `SetCaption`, `SetColOptions`, `SetRowContents`, `GetRowContents`,
`GetInfoCols` and `ColumnParameterType` all consult that one answer instead of
re-deriving the mapping, which is what let them drift apart in the first place.

Two things fell out of the rewrite:

- `SetColOptions` had indexed `ColOptions` by `Index` in one branch and by `i` in
  the other, so a curve with the argument anywhere but first assigned the
  calculated and real columns to different places. Unreachable in practice - the
  argument is always first - but it was one reordering away from mattering.
- A column's option is one setting for a whole column, while a parameter may be
  computed in one type and fitted in another. It is disabled only when *every*
  curve that has the parameter computes it; otherwise a fitted value would be
  presented as something the fit never touched.

**Covered now, and the way it got covered is the point.** An LCL `TStringGrid`
still cannot be driven headlessly - sizing one with no parent raises "Canvas does
not allow drawing" before a single cell is written, whichever way the default row
height and column width are pinned first. So the logic left the grid instead:
`CollectColumnNames`, `ColumnParameterType`, `RowCellText`, `ApplyRowCellText` and
`ColumnIsEditable` are all in `Server/curve_list.pas`, which links no widget set,
and `tests/testcase_curve_list.pas` drives every one of them. What is left in
`Desktop/curve_list_grid.pas` is loops over those answers. Move a decision back
into them and it becomes unguarded again - which is why that file is on the
excluded wrapper list.

## The compute server built a desktop client, and a connection to itself

`Server/fit_task.pas` and `Server/fit_service.pas` each carried `uses app;` in
their implementation section. Neither referenced `FitClientApp_` or
`FitService_` - the only two identifiers `Desktop/app.pas` exports - so the
clause bought nothing at all. But `app.pas` has an `initialization` block:

```pascal
initialization
    FitClientApp_ := TFitClientApp.Create;
    FitService_ := THttpFitService.Create(DEFAULT_SERVER_URL);
    FitClientApp_.FitClient.FitService := FitService_;
```

So `fit_server` - the headless compute process - constructed a desktop client
application object and an HTTP client aimed at the default server URL on every
start-up, and freed them again at exit. A server holding a client of itself.

Six more units carried the same dead clause: `fit_server_app`,
`fit_task_with_thread`, `main_calc_thread`, `csv_file_loader`, `fit_client` and
`configurable_user_points_set`. Only the last two have any business linking
`app`, and only one of them uses it.

**Why nothing noticed.** It is not a crash and not a leak - the objects are freed
- and nothing the server does afterwards touches them. It cost start-up work and,
much more expensively, it put `Forms` and therefore the whole LCL on the
dependency path of the fitting engine. That was one of the two reasons roughly
fifteen thousand lines could only be compiled by `lazbuild` with a widget set,
and so could not be reached by the plain-FPC suite where line coverage is
measured.

**The rule.** A `uses` clause is a dependency, not a decoration. In a unit under
`Server/`, `Common/` or `Worker/`, one that reaches the LCL is a defect whether
or not anything is called through it - and a unit with an `initialization` block
does work merely by being linked.

## IGridDataSource was implemented for a caller that did not exist

`Desktop/table_components.pas` declared `TTableCompList = class(TSelfCopiedCompList,
IGridDataSource)` and implemented all forty of that interface's methods. The
interface is the grids library's way of letting a grid pull its own data:
`TDataGrid.SetGridDataSource` hands one over.

Nothing in this application ever called `SetGridDataSource`. The only caller
anywhere is in `fitgrids`' own examples. The grid was never given the data
source; `GridAssign` pushed values into the grid directly, which is what it had
always actually done.

So the declaration cost the curve-list hierarchy - and, through
`int_fit_service`, the engine - a dependency on `Grids`, `Controls`, `Forms` and
`Graphics`, in order to satisfy a contract with nobody. Discovered only when the
hierarchy was split and the question "what has to keep implementing this?"
finally got asked.

**The rule.** An implemented interface with no consumer is not free. Before
declaring one, find the call site that will use it; if there is none, the methods
are dead code wearing a type signature.

## An LCL descendant cannot be tested, so the logic must leave it

The parameters table's rules - which columns exist, what a cell reads, when a
cell is blank, whether a column may be edited - lived inside methods taking a
`TStringGrid`. That made them unreachable by any test: an LCL grid cannot be
driven headlessly, and sizing one with no parent raises "Canvas does not allow
drawing" before a single cell is written, whichever way the default row height and
column width are pinned first. The gap was recorded here as open and stayed open.

Splitting the hierarchy moved every one of those decisions into
`Server/curve_list.pas`, which links no widget set, and
`tests/testcase_curve_list.pas` now covers them - including two that had already
misled a user (a value under the wrong column heading, and a blank cell reported
as a conversion failure) and one that had never been exercised at all: the
display-scaling arithmetic behind the column and row seed sizes, which reads an
injectable `CurveListPixelsPerInch` where it used to read `Screen`.

**The rule, and it is now the coverage policy as well.** Logic does not live in a
UI class. What remains in `Desktop/curve_list_grid.pas` is loops over the model's
answers, and that file is on the excluded wrapper list precisely because it should
never again contain a decision worth testing.

## The one request that opted out of every timeout was the one at shutdown

`THttpFitService.Destroy` released the server-side problem like this:

```pascal
C := TFPHTTPClient.Create(nil);
try
    try
        C.Delete(Format('%s/problems/%d', [FBaseUrl, FProblemId]));
    except
    end;
finally
    C.Free;
end;
```

Every other request in the unit goes through `NewClient`, which exists for one
reason, stated in its own comment: *"a client with timeouts, so a server that is
down or wedged cannot make the application wait forever."* The destructor built
its client by hand and therefore had **no ConnectTimeout at all** - and its
`except` swallowed the failure, so nothing said it was even trying.

Against a dead port that cost three seconds of silent waiting on every teardown.
Against a *wedged* server - one that accepts a connection and then never answers -
it is an unbounded wait during shutdown, with no message and no way for the user
to tell the application is not simply hung.

**How it surfaced.** Not from the application. A new unit test built a service
against a nonexistent server, and every single test in the class took exactly
3.001 seconds while the requests themselves measured 0 ms. The time was all in
the destructor.

**The rule.** One place opens a connection. A call that builds its own client
opts out of every policy the shared one carries - timeouts here, but the same
would go for headers or retries - and an `except` with an empty body hides that
it happened.

## Seven hundred lines were untestable because each caller made its own client

`http_fit_service.pas` is what every desktop action goes through, and almost all
of it is marshalling: building a URL, encoding a body, reading a reply, deciding
what a missing field means. It sat at **2 % coverage**.

The reason was structural rather than neglect. Each of the transport call sites -
`HttpGet`, `HttpSend`, `GetPoints`, `ModuleGet`, `ModulePost`, the curve-points
loop, the destructor - constructed its own `TFPHTTPClient` inline. A test double
could therefore only override the high-level verbs (`GetProfilePointsSet`,
`AddPointToSet`, …), which is precisely the code it wanted to exercise. The
existing stubs - `TSilentPickService`, `TRecordingFitService`, `TSilentFitService`
- all do exactly that, and so none of them runs a line of the marshalling.

Two protected virtual methods, `Fetch` and `Send`, now hold the whole of it.
Overriding them runs every line of the real encoding and decoding against a canned
reply, and the same change removed the duplicated "a rejection is the server
talking, anything else is the transport failing" handling from six call sites.

**The rule.** A seam is not an abstraction for its own sake. Where bytes cross a
process boundary, one method should do it - otherwise every caller is a place a
test cannot reach, and the untestable surface is the marshalling, which is where
the silent mistakes live.

## Twelve unit tests were sitting in the integration half

`fit_service.pas` showed **0 % coverage** while being exercised on every one of
twenty-nine REST tests. `fit_task.pas` likewise. The tests were not missing; they
were in the wrong half.

`testcase_rest_api`'s own header says it drives the API *"directly (no socket)"*
and one of its comments calls it *"this unit test"* - yet the class was registered
`integration`. Seventeen of its tests do run the optimiser to convergence, which
is genuinely an integration criterion; the other twelve create a problem, push a
profile, read positions and handles back and get refused where a refusal is right.
Registering the class as one thing made all of it slow-half, and coverage is
measured over the unit half alone.

Splitting the class moved `fit_service` from 0 % to 46 %, `fit_task` to 64 % and
`fit_server_session` to 96 % - **without writing a single new test.** The same
split applied to `testcase_fit_marshalling` (two of three), and three further
classes - `testcase_axis_defaulting`, `testcase_curve_type_selection`,
`testcase_pick_refresh` - turned out to be unit tests in their entirety: each
drives the client through a `THttpFitService` descendant whose transport is
overridden, so no socket is opened and no server is needed. They were in the slow
half because they are **nogui-only**, which is a property of the binary they link
into rather than of what they depend on. The two had been conflated.

**The rule.** "Needs the LCL" and "needs something outside this process" are
different questions. Classify by dependency, and a class whose tests differ in
their dependencies is two classes.

## A refused pick was fatal on Linux and a balloon on Windows

The chart-click handler caught `EUserException` under `{$ifdef windows}` and did a
bare `raise` under everything else. So on Linux a server declining a pick - with a
correct, carefully worded explanation - escaped the click handler and reached
`TFormMain.OnException`, which is the last-resort handler for FAULTS: it logs at
`Fatal` and **stops the state poll**. The user got their explanation with "Server
polling has been stopped. Use Fit -> Compute Server... to resume it." stapled to
the end, and had to reconnect from the menu after being told "no".

Latent for as long as no pick was ever refused. It surfaced the moment one was.

**The rule:** a refusal is a message on every platform, and it never touches the
timer. The Windows balloon is kept - it is anchored at the click and reads better
than a dialog - and elsewhere the message is queued and shown from the main loop
like every other non-fatal message in the form.

**Why the tests did not catch it, and what now does.** The refusal was covered at
the REST layer (a 400 with the message) and the message itself was right; nothing
exercised what the CLIENT does with it, and the handler that got it wrong lives on
an LCL form that the headless suite cannot drive - see the note on
`CollectColumnNames` for the same limitation. What is pinned instead is the half
that can be: `ARefusedPickRaisesEUserExceptionAndNothingWorse` asserts a refused
pick reaches the caller as `EUserException` and not as some other class, because
that class is the entire contract between a server that declines and a UI that
explains. A platform-conditional handler is still a platform-conditional handler,
so treat one as a defect until proven otherwise.

## Invariants that carry weight

- **Fit intervals are disjoint by design.** Each becomes its own `TFitTask`,
  minimised independently — which is what makes them parallelisable and what
  licenses the sorted-consecutive-pairs encoding of their bounds. Overlapping
  bounds collapsing into different, still-disjoint intervals is intended, not
  corruption. Do not "fix" it, and **do not reuse that encoding for anything that
  can overlap** — a module whose items nest or share endpoints must bring its own
  point set.
- **`AddPoint` deletes on a repeated x.** It is add-or-toggle, which is right for
  independent picks and wrong for pairs. Routing paired picks through it
  annihilates a shared boundary. And wrong for a BULK WRITE, which is the
  direction this entry did not look: `SetCurvePositions` and `SetRFactorBounds`
  loop through it to get the uniqueness rule below, so a set carrying one
  coordinate twice netted to ZERO points at that abscissa rather than one. The
  rule is kept and the mechanism split - `SetPointUnique` upserts without the
  toggle - because the interactive path depends on the toggle and the client
  mirrors it. Predicted while planning, asserted, and the assertion failed
  before the fix, which is the only reason it is written down as a fact.
- **A pick set is model INPUT, and a fit only ever deletes from one.** The picked
  curve positions must hold unique x values that are real samples of the profile,
  because `CreateTasks` looks each one up in the data, every instance is seeded
  from it, and the pick carries the handle that instance's fitted parameters are
  handed back by (`IdentifyCurve` / `RestoreCurveValues`). Writing the built
  curves' fitted `x0` back
  into them broke all three at once, and the report was a crash three gestures
  later: two instances converging on one `x0` made x non-unique, so the next
  redraw asserted inside `TPointsSet.Sort`; a fitted `x0` is off the sample grid,
  so the next `CreateTasks` could not find it; and changing the seed changed the
  restore key, silently discarding the fit it was trying to report.

  The one exception, and why it is safe: `AdoptCurveRemovalsFromTasks` DELETES
  the picks whose curves an automatic run removed. It never moves one and never
  adds one, so both properties above survive — and without it the reduction
  evaporated on the next edit, because the picks seeded every deleted curve
  again. What the
  model was built into is reported by a *separate*, derived, read-only set
  (`FResultedCurvePositions`, `GET /calc-positions`) — which is free to hold an
  off-grid x and two curves at the same one precisely because nothing reads it
  back.

  The near miss worth remembering: the change that introduced this was right in
  its first half (stop auto-seeding a position on every sample for a type placed
  from its own point set) and wrong only in the second (report the result by
  overwriting the input). Two meanings had been given to one object, and the
  crash was the second one arriving.
- **A pick may be moved once its curve is fitted — but only because identity is
  issued rather than derived.** This used to be refused, and the refusal was
  honest at the time: the restore key was computed from the seed, so moving a
  pick orphaned the values stored under it — that curve alone falling back to its
  starting guess while its neighbours kept theirs, which reads as a fit that has
  partly come undone for no reason.

  The key is now a handle issued to the pick, and a move carries it across
  (`TCurveIdentityRegistry.TakeSeedFrom`). The curve keeps everything the
  optimiser found about its SHAPE and is re-seeded at the new position — decided
  by parameter ROLE (`VariablePosition`, `Amplitude`), not by name, so a curve
  type that calls them something else still re-seeds correctly.

  **A module's markup is still refused**, and it is a different case, not a
  leftover: its points are not one per curve, so moving one re-derives every
  instance the markup placed, all with new seeds. There is no correspondence to
  carry — `Server/fit_advice.pas`, `AdviseMoveMarkupPoint`.
- **`SubtractBackground` does not touch the picks** — worth knowing, because it
  looks as though it should. It rewrites `FExpProfile`'s y values in place
  (`SubtractBackgroundLinearly`), so a pick's stored y goes stale, but the pick
  itself — and therefore its handle — survives, and the previous round is not
  discarded. **Established by reading the code, not by a test**: the honest
  statement is that nothing currently guards it, so a change there could break
  the restore silently. A test over the REST surface would close that.
- **`TPointsSet.Sort` does not police uniqueness.** It used to, by assertion, from
  inside a selection sort that consumed one *distinct* x per output slot — so a
  repeated x left its search index at `-1`. In a debug build that was an
  assertion; in a release build it read `FPoints[-1]` and wrote a garbage point
  into the result, reporting nothing. It is now a stable merge sort that is
  correct for any input. Uniqueness is enforced where it can be explained:
  `TFitService.AddPoint` keeps the pick sets free of duplicates, and
  `CreateTasks` says so with a `CheckThat` on the grid lookup.
- **No fit interval means the whole profile.** `CreateTasks` materialises that
  default into the bounds, so every consumer — sub-tasks, summary table, chart,
  statistics — sees one ordinary interval and the user can see what is being
  fitted. Requiring an explicit one made a just-placed model instance invisible.
- **A curve holds only the samples it covers.** It used to hold one point per
  profile sample whatever its shape, so its values could not distinguish "zero
  here" from "not here at all" - and a compactly supported curve, exactly zero
  outside its own stretch, was drawn stepping from the data's level to zero at
  each edge: a vertical line the height of the chart on data that sits far from
  zero. That was patched by having the server STATE an extent
  (`CollectCurveAttributes` → `Start Pos.`/`Finish Pos.` → `CurveExtentOf` →
  `TPointsSet.SetExtent` → every drawing path honouring it), and the patch was
  correct. It is gone, because the points now say it: a curve is built over its
  own stretch by `TFitTask.CreatePointsFor`, drawing all of them is right, and
  there is nothing to state, stamp, or forget.

  What replaced the index arithmetic: a curve records where its first sample sits
  in the profile (`FFirstSampleIndex`) and sums ITSELF into a target
  (`AddTo`/`SubtractFrom`), so the translation exists once instead of at every
  loop that touches both. The offset and the x values are set in the SAME walk in
  `SetWindow`, so they cannot disagree, and the point array is sealed afterwards -
  a curve that grew would be summed into the wrong place from then on, silently,
  because the fit still converges.

  **Which samples are a curve's own is a question for the curve** (`CoversSample`),
  not a range test the framework applies. Two patterns placed end to end share one
  x, and it belongs to whichever STARTS there; the other must not carry it at all,
  rather than carry it as a zero. That also fixes the order the builder works in:
  placement, then the shared-boundary yielding, and only then the points.


- **`GetCurveExpression` and `IsAnalytic` must not contradict each other.** One
  is the formula the out-of-process engines evaluate; the other decides, at class
  level, whether those engines are offered at all. A type that overrode one and
  forgot the other would be offered an engine that cannot fit it, or denied one
  that can — and the fit would then depend on which engine ran it, both answers
  looking plausible.

  **Only one direction is assertable**, and knowing which saves an attempt to
  strengthen it: a type *with* an expression must be analytic, and a type that is
  *not* analytic must have no expression. The converse fails on the type that is
  most obviously analytic — a user-defined curve *is* a formula, yet a freshly
  constructed one has none until the user types it. The registry-walking test
  asserts what is true rather than what is symmetrical.

  This was found by writing the test: the code comment had asserted the
  symmetrical version for some time, and no test existed.
- **What the model allows is decided when the model is built, not when a fit
  starts.** `fit_advice` corrects two things a selection cannot honour — the
  objective, and curve scaling for a model that sets its own amplitude — and
  `TFitTask.EnforceLossCompatibility` applies them. It used to be called from
  `Optimization` alone, so the correction existed only during a fit. Curve
  scaling multiplies the whole model onto the profile, and the model built after
  every pick is also **what the user is shown**: a self-scaling model was
  therefore drawn to one scale before the fit and another after it. A pattern
  placed between two picked points came out about a third above them — off the
  top of the chart, read by the user as a pattern that was not drawn at all, and
  seen to "appear" the moment a fit finished. `InitTasks` now applies the same
  rule to the model it has just rebuilt, so the picture before a fit is the
  picture the fit starts from.
- **The argument axis is display-only.** It converts the stored argument for the
  user and supplies the caption. It never alters stored data or the fit.

## Traps

- **`ICurveTypeIterator.EndCurveType` means "the current item IS the last"**, not
  "past the end". A `while not EndCurveType do … Next` loop silently skips the
  final entry — which, the registry being sorted, is whichever type sorts last.
  Use the project's idiom: process, then `if EndCurveType then Break else
  NextCurveType` (`TFormMain.CreateCurveTypeMenus`). The semantics were left alone
  because existing callers depend on them.
- **Untyped real constants are `extended`**, so a `double` assigned from one can
  compare as strictly less than the constant itself.
- **`TPositionCurveParameter` derives its variation bounds at CONSTRUCTION time.**
  On a curve with no points yet, that pins the position to its seed. The engine's
  generic path builds a registered type through the one-argument constructor and
  assigns `x0` afterwards — so a type using that path must declare the role
  (`Type_ := VariablePosition`) instead of using that class. The built-in peaks are
  unaffected: they take `x0` in their constructor.
- **The curve-type selection is process-global**
  (`TCurveTypesSingleton.FSelectedCurveType`), and every new service seeds from
  it. A test that does not state its curve type inherits whatever the previous
  test selected — which surfaced only when fixtures were reordered.
- **Every project has its own unit search path**, and `Desktop/Fit.lpi` has **two
  build modes with separate paths**. Patching only the first leaves the
  application failing to compile while every test suite passes. A new source
  directory must be added to all of them, and may appear to work at first by
  resolving against stale `.ppu` files.
- **A project file copied to a different directory depth must use forward slashes**
  in its unit filenames. A backslash is a literal character on Linux, and the main
  program file then cannot be found.
- **A project's own directory is searched before its search path.** That is why the
  overridable stubs (`Common/app_modules.pas`, `tests/no-modules/module_tests.pas`)
  deliberately live where no project calls home — a copy beside the program could
  never be overridden.
- **Several source files are CRLF.** Editing them with a text-mode script rewrites
  every line; use byte-preserving I/O so diffs stay reviewable.
- **`TTAChart`'s crosshair is not the user's aim.** `GetPointNextTo` scans every
  series that shows points or lines and keeps the nearest point, ties going to the
  LAST series scanned. Anything drawn over the data therefore captures clicks
  meant for it. Treat `OnDrawReticule`'s series index as "what the crosshair is
  on", never as "what the user clicked".
- **`Packages/TAGraph` is ISO-8859 encoded.** `grep` treats it as binary and
  reports no matches; use `LC_ALL=C grep -a`. This produced one false "no local
  changes" reading about a component that is in fact a local fork.
- **The process tests start a worker on a FIXED port.** A stale `fit_server` from
  an interrupted run then serves every later run, and the symptom is not "cannot
  connect" — it is an unrelated assertion failing with a stable, believable value.
  The harness clears the port first; do not remove that.

## Coverage measures the unit suite, and that is what made it possible

Measuring coverage over the whole suite ran for **two hours** and was killed unfinished.
Split by suite, the same measurement takes **11 seconds**. The numbers behind that:
7 test classes accounted for 98 % of a 132-second run, and the remaining 460 tests
together took 2.6 seconds. Callgrind simulates every instruction, so it multiplies
whatever it is given - and it was being given real fits and real servers.

**An integration test is the wrong thing to measure coverage with anyway.** It drives the
same lines repeatedly to check behaviour; it reaches almost nothing a unit test does not.
Coverage answers "is this line exercised", integration answers "does it work", and
conflating them buys hours of runtime for a number that means less.

So every test class is registered into `unit` or `integration`
(`RegisterTest('unit', TFoo)`), and `testcase_suite_split` fails the build when one is
registered into neither. That guard matters in the direction that hurts: an unclassified
test silently disappears from `--suite=unit`, and the coverage number then describes a
suite that quietly stopped running it.

**The split immediately found a test that passed for the wrong reason.**
`TOpenWithoutServerTest` opens `2.dat`, but never registered the data loaders -
`RegisterAllDataLoaders` is deliberately not called from an `initialization` section, and
the only caller was `testcase_data_loader_registry`. FPCUnit runs tests in registration
order, that unit test is listed fifty lines earlier in `fit_tests.lpr`, and the registry
it populates is global. So the file-opening test inherited state from a test it has no
relationship with, and `--all` always ran both, so the dependency was invisible. Running
the integration half alone produced "no installed reader handles .dat files".

That is why **filesystem access makes a test an integration test**: a file is an external
dependency exactly as a socket is, and the state around it can be left behind by whatever
ran before.

**A corollary worth keeping:** the coverage task builds no compute server and sets no
`FIT_SERVER`, because a unit test does not cross a process boundary - that is what makes
it one. An earlier version did build one, and the ordinary server build steps the build
number, so measuring coverage raised the application's version. A measurement must not
change what it measures.

## The coverage image is pinned; the build toolchain still is not

These look contradictory and are not, so the distinction is recorded rather than
left to be rediscovered and "fixed".

**The toolchain is deliberately unpinned** so an incompatibility shows up as a failed
build within days rather than years later. That is unchanged, and it still governs
`-Task check`, `-Task build` and every test task: they use whatever Lazarus the runner
currently offers.

**The coverage image is pinned** because it is a *measurement* environment, not a build
toolchain. An unpinned one would make the coverage number move for reasons unrelated to
the tests, and a number that drifts on its own cannot gate anything or be compared with a
baseline taken last month. The FPC version inside it must also match the one the project
builds with, or coverage is measured over different generated code than the one that ships.

The rule this leaves: pin what you *measure with*, not what you *build with*.

**Measured, not assumed: the valgrind version does not change the coverage number.**
The concern that a native run and a container run would disagree was checked rather than
argued about. The same suite measured with valgrind 3.22.0 on the host and 3.19.0 in the
image produced *identical* coverage for every unit - 949/1423 either way - while the
instruction counts differed (116,938,173 against 116,539,642). That is the expected shape
once stated plainly: coverage is a control-flow property of the program, whereas the
instruction count is a property of the simulation.

The version check in `Test-NativeCoverageTools` is kept anyway, and deliberately: this is
one data point about two nearby versions, the check costs one container start, and the
whole value of the number is that it can be compared with a baseline committed months
earlier. The FPC version is the half that genuinely matters, since a different compiler
generates different code and therefore a different line table.

## Conventions

- **Invariant tests are named after the invariant they defend**, so a failure
  reads as the broken decision rather than a broken assertion.
- **Write the invariant test within the change that establishes it**, not after.
- **Record a decision in the same commit that makes it** — in the relevant
  document and as a comment where the code lives. A decision that exists only in a
  conversation is already lost.
- **Fail loudly.** `Assert` is compiled out in release builds, so a check that
  must hold in production is a `CheckThat`/`CheckAssigned` that raises. See
  [no-silent-degradation.md](no-silent-degradation.md).

## A fault inside the widget set must not be reported through it

The client froze with an empty error dialog on screen, the Fit menu still
dropped down under it, and no click or keystroke reaching anything - the desktop
session with it. `fit_client.log` ended with two `EAccessViolation`s 53 ms
apart, every frame in the shared-library region and none in the Fit binary.

Resolved by address, against `/proc/<pid>/maps` of the process while it was
still hung:

- the **first** fault was in GTK, entering a menu item with the pointer -
  `gtk_menu_shell_real_select_item` <- `gtk_menu_shell_enter_notify` <-
  `gtk_menu_enter_notify` <- `_gtk_marshal_BOOLEAN__BOXED` <- `g_closure_invoke`;
- the **second**, 53 ms later, was the queued error dialog being mapped -
  `gtk_dialog_map` <- `gtk_window_map` <- `g_object_notify`;
- and `gdb` on the live process showed the main thread blocked **forever** in
  `g_mutex_lock_slowpath`, called from `g_signal_emit_valist`.

That last line is the hang. FPC turns the SIGSEGV into an exception and unwinds
out of GTK's C frames, so the signal emission that was in progress never
released GLib's signal mutex. Every later emission - painting, closing the menu,
mapping a dialog - blocks on it, and the process holds the X pointer grab while
it does. The dialog `OnException` opened made it worse in the most literal way:
it was one more emission, into the same lock.

So `TFormMain.OnException` no longer reports a memory fault at all. It logs the
fault, the stack, and now the **module map** (the addresses above could only be
resolved because the process was still alive to read `/proc/<pid>/maps` from -
which is possible exactly once, and only if nobody closes the window), and then
`client_log.EndProcessAfterFault` kills the process. Not `Halt`: unit
finalization frees the forms, which re-enters the widget set and hangs there.
Which exceptions count is one decision over exception classes -
`FaultLeavesProcessUnsound`, exhaustively covered by `testcase_client_fault` -
and the arithmetic faults are deliberately on the survivable side.

**What is still unknown is why GTK faulted.** The suspects were all measured and
none reproduced: with the client driven by synthetic X events on an Xvfb display
under `G_SLICE=always-malloc MALLOC_PERTURB_=170`, none of ~40 curve-type
selections with the queued menu rebuild landing on a menu being reopened, nor
the polled ticks arriving while a menu was open, nor long dwells on hint-bearing
entries, faulted once. The next occurrence will carry its own module map, which
is what makes it solvable without the hung process.

## The window scaled and everything inside it did not

On a display at 192 dpi the application came up as a postage stamp: readable
title bar, unreadable everything else. Three separate causes, and fixing any one
of them alone changes nothing.

**1. The binary shipped two manifests, and the one that won said nothing about
DPI.** `Fit.lpr` carried `{$R manifest.res}` - a hand-made `RT_MANIFEST`,
resource id 1, identifying the application as
`CompanyName.ProductName.YourApp`. The `.lpi` *also* asks Lazarus for a manifest
(`UseXPManifest`), and that one lands in `Fit.res` under the same resource id.
Two `RT_MANIFEST` id 1 in one binary is not a spare; the linker keeps one, and
nothing in the build says which. Windows read an executable that never declared
`dpiAware`, so it ran the entire GUI through the bitmap stretcher. The hand-made
one is gone. The manifest now comes from exactly one place, the `.lpi`, which
carries the real identity and `<DpiAware Value="True/PM_V2"/>`.

**2. `Application.Scaled` was never set.** It is the master switch: LCL forms
default to `Scaled = True` individually, and every one of those checks is
`if Application.Scaled and Scaled`. With the application flag off, not one form
scaled. It is set in `Fit.lpr` **before** `Application.Initialize` and before any
`CreateForm`, because `TCustomDesignControl.Create` reads it while the form is
being built - setting it afterwards leaves the already-constructed forms at
design size. The `.lpi` carries `<Scaled Value="True"/>` to match.

**3. What the LCL scales, and what it cannot.** The LCL walks the control tree
and multiplies bounds and fonts by the ratio between the design ppi and the
monitor's. That reaches streamed `.lfm` geometry and anything a control exposes
as bounds. It reaches nothing else. Three kinds of code had to be converted by
hand, and new code in any of them has the same obligation:

- **A `Paint` method.** `Packages/TAGraph/Package/TAGraph.pas` is very nearly
  nothing but one: the axis frame, the ticks, the gaps around the mark labels
  and the 13-pixel mark font are literal counts handed to a `Canvas`. They now
  go through `TTAChart.Sc`, which is `Scale96ToFont` with a floor of 1 so a pen
  never rounds to zero and draws nothing. `TFormMain.CheckListBoxLegendDrawItem`
  is the same problem in miniature.
- **Anything that sets geometry after the form is constructed.** Values set in
  `FormCreate` are safe - `TCustomForm.AfterConstruction` calls `DoCreate` and
  *then* scales - but an event handler running later is past that point. The
  five tab `OnShow` handlers each re-laid their table with the literals 8, 4, 16
  and 12, so they did not merely fail to scale: **they overwrote the scaled
  geometry with design pixels every time the user changed tabs.** They are now
  one `TFormMain.InsetGridInPanel`.
- **A form built by hand rather than streamed.** `ShowCustomAxisDialog` builds
  its dialog with `TForm.CreateNew`, whose `AfterConstruction` scales the form
  *before* the first control is added to it. Every constant in it is now quoted
  at 96 dpi and passed through `Scale96ToForm`.

Two more levers, easy to miss. `TImageList.Scaled = True` is what makes a 16-pixel
toolbar icon render at 32 on a 200% display - without it the toolbar grows and
its glyphs do not. And a *data source* can undo the work of a control: the grids
refill their widths from `TTableCompList.GetColWidthByDefault`, which returned a
flat 64, cutting the scaled columns back down on every data change.

**The rule.** Every pixel literal in this application is quoted at 96 dpi and
converted at the point of use. If you write a number that reaches a `Canvas`, a
`SetBounds` outside `FormCreate`, or a hand-built form, convert it - `Scale96ToForm`
for geometry, `Scale96ToFont` for anything sized to sit beside text.

**GTK2 cannot see that the desktop is scaled, and says so with confidence.**
This is the part that made the fix look like it had done nothing. On a Plasma
desktop at 200%, `xsettingsd` publishes `Gdk/WindowScalingFactor 2` and
`Gdk/UnscaledDPI 98304` - the GTK3 scheme. GTK2 predates all of it and
understands only `Xft/DPI`, which that desktop no longer sends. So
`gdk_screen_get_resolution` returns -1, the LCL falls back to dividing the pixel
width of the screen by its millimetre width, and on a 3840-pixel 1016 mm display
that is exactly 96. Nothing reports an error; the application is simply told
that a 200% desktop is a 100% one, and every correct scaling decision above then
multiplies by one.

Whether it bites depends on how far GTK got: `gtk_init` sets up an XSETTINGS
client that falls back to the root window's `RESOURCE_MANAGER` for `Xft.*`, and
where that fallback fires the DPI comes out right. Where it does not, nothing
downstream can tell. `Desktop/ui_dpi.pas` therefore resolves the ppi itself -
`/DPI=` on the command line, then `FIT_UI_DPI`, then `GDK_SCALE` /
`QT_SCALE_FACTOR`, then the `Xft.dpi` X resource read straight off the root
window - and writes it into `ScreenInfo` before the first form exists. One
assignment, because `ScreenInfo` is where `Screen.PixelsPerInch`,
`Monitor.PixelsPerInch` and every form's own scaling all read from; scaling each
form afterwards would be a call to forget on the next form somebody adds.

**It also logs what it decided, and what the main form did with it.** Two lines:

    UI scaling: 192 ppi (the Xft.dpi X resource)
    FormMain: designed at 107 ppi, laid out at 192 ppi (monitor says 192),
              1891x1019, font height -18

Those exist because "it still looks small" is not a diagnosis. It cannot
distinguish a form that never scaled, from a display that really is 96 ppi, from
a binary that was never rebuilt - and one of those three was the answer the first
time this was reported. Read the log before changing any code.

**The resolution: the Linux client is built on Qt, not GTK2.** Reading `Xft.dpi`
makes GTK2 usable, and no more than usable. Two things stay broken under it and
no application code can reach them - one DPI for the whole session, so a second
monitor at another scale is always wrong, and no live rescale, because there is
no `WM_DPICHANGED` equivalent to react to. The build therefore compiles the
client with `--widgetset=qt6` (then `qt5`), which needs two packages installed:
the LCL's Qt **interface units**, which `lazbuild` compiles against, and the
**`qtNpas` binding library**, which the finished binary loads.
They are different packages and one without the other fails in a way that does
not mention the other.

A machine with neither still builds, on GTK2 - and the build says so on every
run, naming the consequence, because a scaled desktop laid out at 100% is
exactly the failure being replaced and must not come back quietly. That is also
why `ui_dpi`'s guesswork is fenced inside `{$IFDEF LCLGtk2}`: Qt already applies
`QT_SCALE_FACTOR` and already knows the display's ppi, so reading the same
signals again and handing the product back to the LCL would scale the
application twice. Where the widget set can answer, its answer is the answer.

**A column measured with the wrong font.** `fitgrids`' `GetMaxTextWidth` sized
every column with the cell font, including the fixed rows, which are drawn with
the larger `TitleFont`. The headers read `Positio` and `Amplitu`. A couple of
pixels short at 96 dpi, twice that on a scaled display - scaling did not cause
it, it made it unreadable. Each row is now measured with the font it is drawn
in, and the padding beside the text is scaled like everything else.

## Scaling correctly is not the same as being readable

After all of the above the application scaled exactly as it should - and the
report came back unchanged: *the text is still too small*. It was, and scaling
was never going to fix it.

`form_main.lfm` set `Font.Name = 'MS Sans Serif'` and `Font.Height = -10` on
**44 controls**, at `DesignTimePPI = 107` - about 6.7 pt. The desktop's own UI
font was Noto Sans 10 pt. So every table, tab and toolbar in Fit rendered at
roughly two-thirds the linear size of every other application on the screen, at
*any* dpi: scaling multiplied both numbers by the same factor and preserved the
gap perfectly. And because GTK draws the menu bar and title bar with the system
font, the parts of the window the eye lands on first never changed at all -
which is exactly why the fix looked like it had done nothing.

The 143 font lines are gone. Controls inherit the form's font, the form uses the
system's, and Fit now reads at the same size as everything else on the desktop.
`MS Sans Serif` does not exist outside Windows anyway, so on Linux this was a
substituted face at a size nobody chose.

**What a font change breaks, and how to tell.** Fixed pixel geometry that was
sized around the old font. All of it surfaced at once, and all of it had the
same shape - a number that should have been a measurement:

- `AutoSize = False` with `Width = 56` on the "Position:" / "Intensity:"
  captions clipped to "Intensit". They autosize now, and the values beside them
  are anchored to their right edge instead of to a hardcoded `Left`.
- The same two rows then slid under the legend box above, because their `Top`
  was a constant. The right-hand column is now an anchor stack from the bottom
  up, so its height follows its text.
- The status bar's four panels (268 + 201 + 557 in the `.lfm`, plus 400 added in
  code) came to more than the window is wide once scaled, and the mode hint
  arrived as `Drag mouse from top-left to bottom-r...`. The two panels with a
  known longest string are now measured with `Canvas.TextWidth`; the two holding
  free prose divide what is left, on every resize.

The rule that falls out: **if a control's size depends on text, measure the
text.** A pixel count is a guess about a font, and this application no longer
chooses its own.

**One trap worth naming.** `AnchorSideLeft.Side = asrRight` does not compile to
anything - `TAnchorSideReference` is `(asrTop, asrBottom, asrCenter)` on *both*
axes, so horizontally `asrTop` is the neighbour's LEFT edge and `asrBottom` its
RIGHT one. `asrLeft` and `asrRight` do not exist. The form refuses to load with
"Invalid value for property", at run time, not at build time - which is how the
same mistake was made twice in one afternoon after being written down once.

## `with` shadows your locals, and nothing says so

`fitgrids`' `GetMaxTextWidth` was written as `with Grid do ...`. Adding a local
called `Width` to it turned every `Width := Canvas.TextWidth(...)` into
`Grid.Width :=` - the `with` record wins over the enclosing locals - so the
routine resized the grid control once per cell and then returned the control's
own width as its answer. Every column came out as wide as the whole table, one
column filled it and the rest were pushed out of sight.

The compiler is happy, both identifiers exist, and the failure is a plausible
layout rather than an error. The rule that follows: **do not add a local to a
`with` block.** Where a routine takes the object as a parameter, qualify it -
`Grid.Canvas`, `Grid.RowCount` - and the trap cannot be sprung.

## Checking every caption without opening every dialog

Most of this application's dialogs are modal and reachable only through a menu,
so "does the text still fit?" could only be answered by opening each one by hand
on a display of each density. That is how a clipped caption survives for years,
and it is why the font change above was reported three times as "still wrong"
before the last of it was found.

The client's `/CHECK_UI` switch measures instead, and it terminates the
application as soon as it has reported so whatever drives it does not sit
waiting for a window to be closed. It walks every form that exists, compares
each caption against the width of the control holding it in that control's own
font, and writes the misfits to the log. Two things had to be right before it
told the truth:

- **`HandleNeeded` first.** A form that has never been shown has not resolved
  its anchors, and its children still carry whatever streaming left them. The
  first run reported a label 20 pixels wide that is really 448.
- **Skip tool buttons on a bar with `ShowCaptions = False`.** A tool button's
  caption is its hint and its action's name, not something drawn. Sixteen of the
  first eighteen hits were icons that fit perfectly well.

What it found once those were fixed: `InputBackFactorDlg.Label1`, 20 px wide for
a caption needing 375, and `CreateUserPointsSetDlg.Label1`, six pixels short.
Both were fixed-width labels beside a fixed-position edit; both are now anchored
to the edit and autosizing. The check reports zero across all seven forms.

Run it after any change to a font, a caption or a dialog layout.

**Not fixed, and deliberate.** `form_main.lfm` carries `DesignTimePPI = 107`; it
was last saved in the IDE on a 107 dpi machine and its stored sizes are 107 dpi
sizes. That is self-consistent, and scaling it is now correct in both directions
- but it does mean the main window renders about 10% smaller on a 96 dpi display
than the raw numbers in the `.lfm` suggest. Renormalising the file to 96 means
rewriting every coordinate in it, which is a worse trade than the 10%.

## An open menu is a grab this application does not own

A dropped-down menu is a modal grab held by the widget set: it owns the pointer
and the keyboard until the user picks something or presses Escape. Two things
break that from behind, and both leave a submenu painted on screen attached to
nothing - no pointer hides it, Escape does not reach it, and the only way to be
rid of it is to click one of its entries, which runs a command nobody asked for:

- **Destroying an entry that is on screen.** A menu entry is one of two widgets,
  plain or checkable, and which one it is is decided when its handle is made. On
  GTK2, ticking a plain entry afterwards makes the widget set destroy it and
  build a checkable one in its place (`TGtk2WSMenuItem.SetCheck` ->
  `TMenuItem.RecreateHandle`). The same applies wholesale to rebuilding a menu
  from the model behind it, which this application does for the curve types.
- **Opening a window over it.** A dialog or a message box takes the grab from a
  menu that is still up.

Neither is something the user asked for at that instant: the ticks arrive from
the state poll (twice a second), the dialogs from a timer or from the calculation
thread through `Synchronize`, and the rebuilds from a queued call. Any of them
can land while the user is standing in a menu. Both times this shipped, it was
found from a screenshot - the state poll had ticked a submenu parent the first
time, and a queued dialog had stolen the grab the second.

So the rule is not "do it carefully" but **do it later**: `ui_menus.AMenuIsOpen`
answers whether a menu is up, and the polled state, the queued dialog and the
queued menu rebuild all wait for it to close. A tick still goes through
`SetMenuEntryChecked`, which names in the log any entry that is ticked without
having been declared checkable - the declaration defect that makes the first
hazard possible at all - and refuses that tick while a menu is open.

Only GTK2 can answer, and only GTK2 needs to; everywhere else `AMenuIsOpen` is
False, which is what the application always effectively assumed.

**This cannot be checked from the Pascal suite.** The nogui widget set the tests
link has no menu support: `TMenuItem.Create` reads
`TWSMenuItemClass(WidgetSetClass)` and faults, so no LCL menu can be built in a
test process at all. What is checkable is the source that decides it, and
`tools/build-tests/menu_entries.tests.ps1` reads it: no designed entry that has
entries of its own may declare itself checkable, no designed entry may be ticked
through the property, and the three deferral points must ask `AMenuIsOpen`. At
run time `MenuEntriesAtRiskOfDangling` walks the menus as actually built -
including the ones a module contributed - and writes any breach to the log.

## A machine provisioned before a requirement never hears about it

The Qt widget set became a prerequisite for the Linux client - GTK2 cannot see a
scaled desktop - and the prerequisites step learned to install it. The test VM
had been provisioned weeks earlier, so nothing there ever ran that step again:
`dnf history` shows the toolchain install and no transaction naming
`lazarus-lcl-qt6` or `qt6pas` at all. Every build since then produced a GTK2
client, and every one of them said so, in a yellow line in the middle of a long
log. Being right in a warning nobody reads is not the same as being installed.

Three things were wrong, and each is a rule of its own:

- **The closing check looked only for executables on `PATH`.** A widget set is
  units plus a binding library, so it was never checked, and the step reported
  "every tool is where it belongs" on a machine that could not build what it was
  provisioned to build. `Confirm-Prerequisites` now checks it as well, and
  `Get-InstalledQtWidgetSet` counts a version installed only when BOTH halves
  answer - the LCL interface units and the `libQtNPas` the finished binary loads.
  A half-installed machine fails twice otherwise: once at `lazbuild`, once at
  start-up.
- **A build reported the gap instead of closing it.** `Build-Client` now repairs
  it: one attempt, Linux only, only when nothing is installed, and never in CI,
  whose images are provisioned by their own workflow. If it cannot, the build
  carries on with the loud GTK2 fallback exactly as before.
- **The step hopped to the VM whole.** With the target set to `vm` it ran only
  there - over the rsync and ssh this machine had to own for the hop to be
  possible. A fresh box set to `vm` therefore could not provision itself, and the
  only way in was to install rsync by hand, which is the one thing the step
  exists to make unnecessary. `prerequisites` now runs on both sides: here first,
  narrowed to what drives the VM (`-DriverOnly`), then in full over there.

**And then it still installed nothing.** With all three fixed, the step on the VM
reported the widget set missing and finished green anyway. Reproducing it printed
the reason above the check: `sudo: a terminal is required to read the password`.
Every package install in the step is a package manager under sudo, and over ssh
without `-t` sudo exits before doing anything - so the toolchain install failed,
the widget-set install failed, and because `lazbuild`, `fpc`, `git` and `rsync`
had been installed months earlier the tool check still passed. Two more rules:

- **Ask whether the run can become root BEFORE installing anything.**
  `Get-ElevationMode` answers root / passwordless / prompt / no-terminal / none
  from `id -u`, `sudo -n true` and whether stdin is redirected, and
  `Confirm-CanInstallPackages` refuses the last two with one clear error instead
  of a column of failures nobody connects to the closing line. A repair asked for
  by a build says the same thing and carries on, because a build must not fail
  over provisioning.
- **A failed install decides the closing line.** `Invoke-PackageInstall` records
  every non-zero exit and the step throws with the list. The tool check can only
  see what is on `PATH`, which says nothing about what this run installed.

**The password was already being asked for.** `Get-VmSudo` asks on this side -
because a prompt written on the VM's tty looks like a run that has stopped - and
`Invoke-VmRootScript` feeds it to one `sudo -S`. The prerequisites hop used
neither: it ran `ssh -t` and left the remote sudo to find a terminal of its own.
A step that installs cannot be fed one password on stdin anyway, since it calls
the package manager several times, so the password now travels the way sudo
itself provides for - `Set-VmAskpass` writes it into a 0600 file with a 0700
script that prints it, `SUDO_ASKPASS` names the script, `Get-SudoPrefix` returns
`sudo -A`, and the helper is removed when the task ends. It is never on a command
line, where `ps` in the VM would show it to everyone.

**And one silent disarming, found while reading it.** `Install-QtWidgetSet @sudo
'dnf'` binds by position, so on a machine where `$sudo` is empty - any root
account - `'dnf'` bound to `$Sudo` and `$Manager` was empty: the switch matched
nothing, `$sets` was empty, and the widget set was never attempted at all, with
no output to say so. Splatting an empty array into a positional `[string[]]`
parameter shifts every argument after it. The calls bind by name now.

**`& @sudo dnf install` does not mean what it reads like.** Everything between `&`
and the first space is the COMMAND NAME, so splatting the prefix there
stringifies the whole array into one name. With a one-word prefix that is `sudo`
and works by accident; the day it became `sudo -A` every install in the step died
with

    The term 'sudo -A' is not recognized as a name of a cmdlet ...

and the script suite was green, because every test asserted what
`Get-SudoPrefix` RETURNS and not one of them ran anything through it. There is
one `Invoke-Elevated` now - the prefix and the command are a single argument list
whose first element is the program - all 23 call sites go through it, and the
tests run a real command through a real multi-word prefix, using `pwsh` in place
of `sudo` because it exists everywhere the suite does and takes arguments of its
own. A test that mocks the thing it is meant to exercise proves the mock.

**And once Qt was finally installed, the build could not pass it on.** The
widget-set argument was built as

    $wsArgs = if ($ws) { @("--widgetset=$ws") } else { @() }

An if-statement unrolls its output, so the ONE-element case comes out as a plain
string, and splatting a string spreads it one CHARACTER per argument: lazbuild was
handed `-`, `-`, `w`, `i`, ... and answered `Invalid option at position 1: "-"`.
The empty case worked by accident, which is why nothing noticed for as long as
every machine fell back to GTK2. Both builders had it, the published one
included. A typed `[string[]]` declaration keeps it an array, and
`tools/build-tests/lazbuild_args.tests.ps1` records the arguments a stand-in
program actually receives - including the broken expression, so the fix cannot be
tidied back into an if.

The general rule: a prerequisite that is not checked at the end of the step that
installs it is a prerequisite that silently expires, and a step that cannot do
its job has to say so rather than end in green. And an argument list is only
tested by something that receives it. If a build can tell that
something is missing, it can install it, and a warning is what is left when
neither is possible.

## The nil guard that stops one line short of its neighbour

`TFitClient.ClearExpProfile` called `HideExpProfile` - which checks
`Assigned(FExperimentalProfile)` - and then dereferenced the same field with no
check of its own. A client asked to refresh before it had ever held a profile
died in `ShowProfile`, one line below a guard for exactly that case.

Nothing in the running application reaches it: data is loaded before anything
long enough to report `Done`. It was found by a unit test built on
`IFitViewer` and a mocked service, which is the first thing able to construct
this class in its initial state and then call into it - the real client is never
seen empty because the form loads a file first.

The interesting part is not the missing `if`. It is that the two adjacent methods
disagreed about whether the field can be nil, and both were right about the
paths they were written for. A guard placed per-path rather than per-invariant
holds only for as long as no new caller appears.

The three tests around it also failed first for a different reason and were
wrong, not the code: `SetSelectionMode` and `SelectEntireProfile` assert their
preconditions deliberately, and a test that trips an `Assert` is a test driving
the class in an order the application does not. Those were fixed by setting up
the state the app sets up, and the assertions left alone.

## A membership test that was a substring search

`RegisterSidecarModule` kept the registered Python packages as one
path-separated string and skipped a repeat with `Pos(APackage, ModulePackages) > 0`.
That is not membership. Registering `pat` after `patterns` finds `pat` inside it
and drops the registration, and the consequence is not an error: the sidecar
starts, reports healthy, and answers 404 for the routes that module was supposed
to add.

Nothing in either repository has two module names sharing a prefix, so this had
never fired. It was found by moving the rule out of `python_sidecar` into
`sidecar_launch`, where it could be called with arguments the application does
not currently produce - which is most of what extracting a decision buys.

The rule now splits on the separator and compares whole names, and the
prefix case is a test. The wider lesson is that a delimited string is not a set,
and every `Pos` against one is a bug waiting for a name long enough to collide.

## Nothing can read back a child process's arguments

The Python sidecar's command line was built inline between a `TProcess` and a
socket, so the only way to exercise it was to start a real interpreter - and
`python_sidecar.pas` measured zero covered lines while owning what the sidecar
is told to do. That matters more than the number: a missing `--modules` produces
a process that starts and reports healthy, and once it is running there is no
way to ask it what it was passed.

Moving the argument list into a function that fills a `TStrings` makes the
decision readable before the child exists. The probing stayed behind - which
paths to try is a decision, asking the disk is not - and that split is the
general shape: the pure half goes where it can be tested, and the syscall stays
where it is.

## Six handlers for an exception that is no longer raised

Every method of `TFitServerProxy` wraps its precondition check in

    except on E: EAssertionFailed do raise EUserException.Create(E.Message) else raise;

and none of those handlers can fire. `checks.CheckAssigned` raises
`EInternalCheckFailed`, which `checks.pas` keeps deliberately distinct from
`EUserException` so that "the user did something we do not support" can be told
apart from "this program is wrong about itself". So what leaves the proxy is the
internal check, by way of the `else raise` - six times over.

The behaviour that survives is arguably the right one: a callback with no client
behind it is the program being wrong about itself. What is wrong is that the code
states the opposite, at length, in six places, and a reader auditing how faults
reach the user would count this as a place where they do.

This is a leftover of the silent-degradation conversion that
`findings.md` already records as unfinished. It is worth noting how it stayed
invisible: the conversion changed what `Fail` raises, and every handler written
against the old class went on compiling. Narrowing an `except` clause is a
change with no compiler consequence and no test consequence either, unless
something exercises the failing path - which nothing did here until the proxy
got its first test.

## An argument checked as required and then dropped

`TMainCalcThread.SetSyncMethods` takes eight callbacks, asserts each one is
assigned, and stores seven of them. `FComputeBackgroundPointsDone` was declared
and used and never written to, so `ComputeBackgroundPointsDone` synchronized a
nil method: the in-process engine computed the background points and the client
was never told, which is the "Please wait" that does not go away.

The shape of the omission is worth more than the fix. The seven correct
assignments and the missing one sit in a block of eight nearly identical lines,
directly under a block of eight nearly identical checks - so the check for the
argument that is dropped reads as evidence that it is used. Nothing could catch
it: the compiler is satisfied (the field is read elsewhere), and the field's only
reader is a method nothing tested.

It was found by writing the first test for the class, which is possible without
starting a thread at all: `TThread.Synchronize` called from the main thread runs
its method inline rather than queueing it, so the whole marshalling layer can be
driven on the test's own thread. That the class had gone untested was not because
threads are hard to test - it is because nobody had tried.

## A freed loader and its replacement at the same address

`TExtensionDataLoaderInjector` holds one data loader at a time and frees the
previous one at the top of `CreateDataLoader`. Writing the first test for it
showed something the code cannot express: the second call frequently returns the
SAME interface pointer as the first, because the first object is freed before the
second is allocated and the allocator hands the address straight back.

So a caller that kept the earlier reference does not get a dangling pointer it
might notice. It gets a live object of the right class holding a different file -
which is the worst of the available failure modes, and one that no pointer
comparison could detect. The rule is that the caller does not keep the reference
across another Open, and nothing in the type system says so.

There is a second edge in the same method: the previous loader is freed BEFORE
the registry is asked whether anything can read the new file, so a refused Open
also takes the working loader with it. No caller depends on that today - the
desktop drops its reference on a failed Open - which is why it is recorded rather
than changed.

Both are pinned in `testcase_loader_injector`, deliberately as the behaviour is
rather than as it should be, so that the next caller to hold a loader across an
Open finds a test rather than a crash.

## One remote backend bounds its wait and the other does not

`python_fit_backend` sets a connect timeout and an I/O timeout on every request,
with a comment saying why: the sidecar bounds its own solver effort, so a reply
that never comes means it is wedged, and failing with a message beats an
application that looks hung.

`server_fit_backend`, which does the same job against a compute server over the
same protocol, sets neither - on the fit and on the availability check both. A
server that accepts the connection and then stops answering hangs the fit
indefinitely, and the availability check is on the desktop's start-up path, so the
same server hangs the application before it draws anything.

Not fixed here, because the right timeout for a fit against an unknown remote
machine is a judgement about how long a legitimate fit may take, and inventing one
under a coverage change is the wrong way to make it. Recorded so the number is
chosen deliberately.

The reason this is worth writing down at all is the pattern: the two classes are
near-copies, one of them learned something, and nothing carried it across. The
same omission was found and fixed in `http_fit_service`'s destructor earlier in
the same pass.

## The first logged exception costs nine seconds

`log.ExceptionTrace` builds its text with `BackTraceStrFunc`, one call per frame.
Measured from a test that did nothing else, the first such call on the test binary
takes about **nine seconds** - the runtime is loading the executable's debug
information to turn addresses into file and line numbers, and these binaries are
built with full line info.

That cost is not a property of the test. It falls on the first exception the real
application logs, on whatever thread logs it, and the desktop logs exceptions from
its error paths - so the first fault a user hits stalls the process for several
seconds before anything is reported. The trace is worth having; paying for it at
the worst possible moment is what is wrong.

It was found by accident: a nine-line unit test for `ExceptionTrace` took the fast
suite from a tenth of a second to nine seconds, which is the kind of thing only
measuring notices. The test was removed - a hundredfold slowdown of the suite that
has to stay worth running is a bad trade for eight lines - and the reason is
recorded where the test was.

Worth considering: warm the symboliser once at start-up, off the path that
matters, or fall back to raw addresses at the point of failure and resolve them
when the log is read.

## The server will not say what formula it is fitting

`GET /problems/{id}/special-params` builds its reply from
`GetSpecialCurveParameters` alone, so it returns the user-defined curve's
PARAMETERS and never its EXPRESSION - although the matching `PUT` accepts both.
The formula is write-only over REST.

Nothing is broken by it today: the client owns the formula, keeps it in its
settings and pushes it to the server, so it never needs to read it back. What is
lost is the ability to ask the server what it is actually fitting - which is
precisely the question asked when a fit produces the wrong shape, and the one
case where the client's copy is not the answer, because the whole hypothesis is
that the two have diverged.

Found by writing a round-trip test for the route and having it fail on the read.
A round trip is the obvious test to write for a PUT/GET pair and it had never
been written, which is how a half-implemented pair stayed unnoticed.

## Five methods that computed the chart's extents, and no caller

`TFitViewer` had `GetMinX`, `GetMaxX`, `GetMinY`, `GetMaxY` and the `GetMinMax`
they all delegated to. Nothing called any of them - not the class itself, not
`IFitViewer`, not the form, not the private module repository. They were removed
rather than extracted, along with the two constants only they used.

They were also wrong. `GetMinMax` compared `PointXCoord[j]` - a RAW abscissa -
against an accumulator it had filled with `DisplayX(...)`, a TRANSFORMED one. On
the identity axis the two coincide and the code looks right; on a diffraction
axis it was taking the extreme of a comparison between two different quantities.
That is the shape of defect dead code specialises in: it cannot be observed, so
it is never wrong until someone calls it.

The lesson for the extraction work: read the callers before moving a method into
the counted half. Extracting these would have added thirty lines nobody wants to
the denominator and given them tests, which is worse than either leaving them or
deleting them.

## One header row for columns that are reused per interval

The summary table gives each fitting interval its own block of rows and starts
its curve columns again at column four. But the curve names are written into row
zero, which the whole table shares - so with two intervals the second one
overwrites the first one's names, and every curve value in the first interval
sits under the name of a curve from the second.

It is not new, and it is not what this pass set out to change: the extraction of
`summary_table` moved the behaviour unaltered and pinned it in a test that says
so in as many words. Fixing it means naming the curve in the interval's own
subheading row, which changes what the user reads and is worth doing on its own
terms rather than inside a coverage commit.

Worth recording because of how it was found. Nobody had ever built this table
twice in one process and looked at the result - the only way to see it was to
mark two intervals in the running application and read the screen - and the first
test that constructed two intervals found it in a second.

## Half the commands a running fit disabled were never disabled

The window's enablement machinery packed bit flags into the `Tag` of every action
and menu entry, then unpacked them into `Enabled` at the end of one long method.
Two methods wrote those flags. `SetOpenState` wrote to the ACTIONS -
`ActionSmoothProfile.Tag`, `ActionSelectIntervalBounds.Tag` - and the unpacking
at the end read the same actions. `SetAsyncState`, which is what disables things
while a fit is running, wrote to the MENU ENTRIES for six of them:
`MenuSmoothProfile`, `MenuSelectIntervalBounds`, `MenuSelectDataInterval`,
`MenuSelectEntireProfile`, `MenuSelectCharacteristicPoints`,
`MenuSelectCurveBounds`.

Nothing ever read those six flags. So during a running fit the user could still
smooth the profile, select interval bounds, select the data interval, go back to
the whole profile, and start either of two picking modes - all of which edit the
data the fit is running on.

It cannot be seen by reading either method: each is internally consistent, and
the two names differ by one word in a file of five thousand lines. It fell out of
rewriting the machinery as a function from a record of inputs to a record of
per-command flags, where "which widget" is not a thing that can be got wrong
because widgets are not mentioned. The new code disables all twelve, which is
what the old code's own comments say it intended.

**And it was not the only dead flag.** The same rewrite found three more sets of
`Tag` writes that nothing read:

- the three `Remove ...` entries (`MenuRemoveBackgroundPoints`,
  `MenuRemoveRFactorBounds`, `MenuRemoveCurvePositions`), which the selection
  mode set a flag on apiece. They are bound to actions of their own and take
  their `Enabled` from the submenu they sit in, so the flags did nothing.
- `ActionSubtractBackgroundBySelectedPoints`, which the selection mode enabled
  or disabled according to whether any background point had been picked. It too
  follows its submenu, so that rule has never been in force - the command is
  offered with no points picked, and subtracts nothing.
- the whole of `TViewState`, which existed to name two values of "the chart is
  drawing something".

Offering each removal only while its own set is being picked, and requiring a
picked point before subtracting by picked points, are both plainly what the dead
code intended - and both are changes to what the user can click. They are
recorded here rather than made silently inside a coverage change.

**Why so many flags could be dead at once.** A `Tag` is an integer on every
widget, and writing one is always legal. There is no such thing as writing to
the wrong `Tag`, or to a `Tag` nobody reads, or to a `Tag` twice - the compiler
sees an integer assignment and is satisfied. The accumulator existed only because
the decision was split across methods that could not return anything; making it
return a record removed the need for it and the room for the mistake in one
move.

## A menu command that has been unreachable for years

`ActionSaveModelAsText` had its enable bit CLEARED at the top of `SetOpenState`
and set nowhere. `SetOpenState` runs from a timer twice a second, so whatever the
form designer left the command at, it is disabled within half a second of the
program starting and stays that way forever. The only export this program has
cannot be reached.

That is very likely what the standing "**There is no export**" note in the open
list is really about: there is one, and it is switched off.

Not fixed here. Enabling it is one line, but nothing has run that code path in
years and whether it still works is a question for someone who can open the
program and try it. The behaviour is pinned in `testcase_action_state` with a
test that says in as many words that it is a defect.

## A module's outline could index a level that was never opened

`ShowModulePanel` turned a module's flattened outline - a depth-first list of
rows, each carrying its own indent - into a tree, by keeping an array of "the
node open at each level" and hanging each row from `Parents[Indent - 1]`.

Nothing checked that level had ever been opened. A row at indent 2 with no
indent-1 row before it read an array slot that was never written, and hung the
node from whatever pointer happened to be there - or from nil, silently promoting
a child to a root. The array was sized from the row count, so the read stayed
inside the allocation and no range check would have caught it either.

It has never fired, because the one module that exists produces well-formed
outlines. That is the whole problem with it: the input comes from a module, the
framework ships none, and the only way to exercise the flattening at all was to
open a window with a module installed. The rule is now in
`Desktop/outline_layout.pas` with twenty-two tests over it, and a row that skips a
level is attached to the deepest level that exists and reported as detached - the
same treatment a row whose parent could not be found already gets, because both
mean the same thing to the user: this data is damaged.

The general shape is worth keeping in mind while emptying the rest of the window:
an algorithm written directly against a widget is an algorithm whose inputs are
whatever the one caller happens to send, and its edge cases have never been tried.

## One invariant, written out four times, asserted nowhere

A user-defined curve's parameters carry roles - which name is the abscissa, which
places the curve, which is its height, which its width - and each role belongs to
at most one parameter. The engine seeds the amplitude from the data peak and the
width from the fitting interval, and it cannot do either for two parameters at
once.

The rule lived in four combo-box change handlers of an LCL dialog. Each walked
the parameter list clearing the role it was about to give away, then gave it.
Four copies, and nothing anywhere asserted the result: a curve type with two
amplitudes is one the fit seeds twice from the same peak, with none it is one
whose height is never estimated - and either way the fit converges on something
and the only sign is that the answer is wrong.

Two things fell out of moving it to `Desktop/parameter_roles.pas`:

**The position is one role wearing two hats.** `InvariablePosition` and
`VariablePosition` are the same role, fixed or varied, and code that searches for
only the fixed one reports no position on a curve whose position the fit may
move. Three of the four handlers got this right by accident, because only the
position handler looked for it.

**Unticking an already-varied position loses it.** `TypeAfterFixing` sends a
ticked `VariablePosition` to `InvariablePosition` and an unticked
`InvariablePosition` back - but an unticked `VariablePosition` becomes a plain
`Variable`, and the curve silently stops having a position. The interface cannot
reach it: the box is only consulted when its state changed, and an unticked box
is what a varied position already shows. It is a trap for the next caller that
asks unconditionally - a "reset all", a settings restore - and it is pinned in
`testcase_parameter_roles` as behaviour with a test named after what it would
cost.

## Four things that were maintained and unreachable

Each was live code, kept in step by hand for years, that nothing could reach -
and in every case what hid it was that nothing reports absence.

**`PopupViewMode` was attached to nothing.** Six argument-axis entries, created
in `FormCreate`, declared checkable in `DeclareCheckableMenuEntries` and ticked
from `ApplyViewMode` on every view change - and no control's `PopupMenu` ever
named it. It could not have been attached either: a right-click placed a point
(below), so opening a menu would have edited the model. Now assigned to the
chart, with a `CheckThat` where it is assigned, because the next one of these
will look exactly the same.

**A right-click on the chart added a point.** Picking is a left-click gesture and
always has been; nothing checked. `TTAChart.MouseDown` and `MouseUp` both take
the button and never read it, the window's handlers ignored it, and
`OnChartClick`'s gate tested whether the crosshair had been drawn - which comes
from `MouseMove`, so it is button-independent. Any button that did not move
between press and release picked.

**`ActionDelete` deleted nothing.** It removed rows from `FCurveList`, which is
what `GetCurveAttributes` answers - `FCurveAttributes.GetCopy` - so the deletion
never left the client and the next refresh restored every row. It also acted on
`GridParameters` whatever grid had focus, while `ucDelete` was enabled from the
FOCUSED grid's selection: it lit up over the data and silently rewrote the
model's parameters.

**`IUiHost.RefreshPanelVisibility` had no caller anywhere** - not in the
framework, not in the module repository, only in the mock. It existed to ask a
module whether its panel was worth a tab, and the Model panel is the
framework's now and always shown, so the question is gone with it.

## Every thirty-second curve was drawn in a colour from outside the palette

The chart holds sixteen colours in an `array[1..16]`, and each curve took the one
at its own position:

    if Index <= 16 then Palette[Index] else Palette[Index mod 16]

`Index` counts from one. Curves 1 to 16 are fine; 17 to 31 wrap onto 1 to 15;
curve 32 gives `32 mod 16` = **0**, and there is no colour zero. The same for 48,
64 and so on. With range checking off - which is how this ships - the series is
drawn in whatever integer happened to sit before the palette in memory.

Thirty-two curves is not hypothetical. "Select all points as curve positions"
seeds one curve per sample, which on a coarse profile is exactly this many.

Fixed, because the intent is not in question and there is no behaviour to
preserve: an out-of-range read has no behaviour. The rule is now
`Desktop/series_palette.pas`, which shifts to a zero-based position, wraps, and
shifts back - doing it the other way round is what maps the multiples of the
count onto zero. It also refuses to produce a negative subscript, since Pascal's
`mod` keeps the sign of its dividend and a wrapping rule fed a negative index is
the other way this same line can read outside an array.

The reason it survived is worth more than the fix. It is one conditional
expression, inside a nested procedure, inside a method that takes a chart - so
the only way to see it was to draw thirty-two curves and look, and a wrong colour
does not look like a defect. Moved out, it is eight tests and one of them is a
sweep.

## A typo in the profile table moves the point to the origin

The profile grid is the one table a value can be typed into, and what is typed
becomes a number through `StrToFloatDef(Text, 0)`. The default exists for a good
reason: a row being typed has empty cells in it, and reading one must not raise
in the middle of an edit.

But an empty cell and a wrong one are the same thing to `StrToFloatDef`. A typo,
a stray letter, a value pasted with its units - `12..5`, `abc`, `12.5 keV` - all
read as zero, and the point moves to the origin. No message, no refusal, and the
chart redraws with a point at (0, 0) that the user has to notice for themselves.

Not fixed here, because refusing an edit needs somewhere to say so and this grid
has no way to report one: the balloon it uses is driven from an exception raised
by the server, and a client-side refusal would need its own path. What has
changed is that the difference is now askable - `grid_edit.EditedValueIsEmpty`
and `EditedValueIsReadable` are there, tested, and unused - so whoever fixes it
starts from the distinction rather than from `StrToFloatDef`.

The behaviour is pinned in `testcase_grid_edit` under a name that says it is a
trap rather than a rule.

## A module's menu has a declaration-order rule nobody documented

A module declares its menu as data - ids, captions, kinds, and the id of the
submenu each entry belongs to. The window resolves those parents by keeping a
list of the submenus it has created SO FAR, which means a submenu declared after
its children cannot be their parent: the children land at the top level instead.

Nothing says so. `int_ui_host` describes `Parent` as "the Id of the submenu it
belongs to" and nothing about order, and the failure is not a failure - the
entries appear, in the wrong place, and a module author looking at their own menu
sees a layout they did not ask for with no error anywhere.

It is now pinned in `testcase_module_menu` and the flattening reports it: a node
whose parent could not be resolved carries `ParentWasMissing`, the same flag an
entry naming a submenu that does not exist gets, because they are the same thing
from the user's side. The window does not yet act on it; the point of the flag is
that it can.

The extraction found nothing else wrong here - the translation from declarations
to widgets was faithful - which is worth saying too. What it changed is that the
contract's load-bearing half now has twenty-three tests over it, in a framework
that ships no module to test it with.

## A typo in the wavelength box disconnected the user from the server

Entering a wavelength went through this:

    SaveDecimalSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    ...SetWaveLength(StrToFloat(Edit.Text));
    DecimalSeparator := SaveDecimalSeparator;

`StrToFloat` raises. So anything that is not a number - a comma-decimal
keyboard's own separator, a value pasted with its units, a slip - did two things
at once, neither of them where the fault was:

- **the process-wide decimal separator was never put back.** Every number the
  application read or wrote for the rest of the session used a full stop it had
  not been asked to use;
- **the exception left the menu handler.** It reached `TFormMain.OnException`,
  which is the last-resort handler for faults: it logs at Fatal and STOPS THE
  STATE POLL. The user got a fault report and "Server polling has been stopped",
  and had to reconnect from the Fit menu - because of a typo in a text box.

`findings.md` already records this exact pattern being fixed in the chart click
handler, where a refused pick escaped as an exception and stopped the poll. The
same shape survived here, in a place nobody had reason to look, because the
raising call is spelled `StrToFloat` and looks like a conversion rather than a
control-flow decision.

Fixed. The reading is now `typed_number.TryTypedNumber`, which parses through a
LOCAL format-settings record - never the global - and refuses instead of raising.
The box also refuses a non-positive wavelength, because zero is what "not set"
already means to the client and accepting it silently did nothing at all.

## Two user curves created in the same millisecond overwrite each other

`WriteUserCurve` names its file after the milliseconds since the epoch:

    CurveType.FFileName := GetConfigDir + IntToStr(...MSecs(Now)) + '.cpr';

Nothing checks the name is free. Two curves created within the same millisecond
get the same file, and the second silently replaces the first - the menu shows
both until the next restart, after which one of them is gone.

Not reachable by hand, and reachable by anything that creates curves in a loop.
Left as it is because changing how these files are named is not a change to make
inside a coverage pass; recorded so that whoever does it knows what to fix.

## Three lineshapes, three conventions, one parameter name

The curve formulas live side by side in `SimpMath`, take arguments with the same
names, and mean different things by them.

**`Sigma` is a standard deviation in one and a full width at half maximum in
another.** `GaussPoint` divides by `Sigma * sqrt(2*pi)` and has
`exp(-(x-x0)^2 / (2*Sigma^2))`; `PseudoVoigtPoint` divides by
`Sigma * sqrt(pi) / (2*sqrt(ln 2))` and has `exp(-4*ln(2)*(x-x0)^2 / Sigma^2)`.
The same number means two widths a factor of **2.355** apart.

**`A` is an area in three of them and a height in the fourth.** The Gaussian, the
Lorentzian and the pseudo-Voigt are area-normalised - the declaration says so, in
a comment: "Integral of function by definition area". `AsymPseudoVoigtPoint` has
no normalising factor at all, so its `A` is the peak height.

**And `DeltaSigma` is the last argument, after `x`** - a shape parameter placed
after the abscissa, which is not where anyone looks for it. Every one of these
was found by a test written from the argument names and failing.

None of this is a defect on its own: each formula is internally consistent and
the fit converges from any starting point. What it costs is everywhere the
parameters are compared, seeded or carried across a change of curve type - a
sigma kept when the user switches between two shapes is a different width, and a
seed computed as a height for a shape whose A is an area is wrong by the width.

Worth a single convention. Worth, at the very least, saying which is which where
the parameters are declared, rather than in one comment on one of the four.

## A large constant in the goal function stops the fit before it starts

`TDownhillSimplexAlgorithm`'s tolerance test compares the SPREAD of the goal
function's values across the simplex against its MAGNITUDE. The unit's own
comment records what that costs when the magnitude is set by something other than
the thing being fitted - "a fit stopping after two cycles with eleven of twelve
parameters still at their initial values" - and the stagnation window was added
to answer it.

Driven directly, at the settings the engine itself uses (`MinRelImprovement`
1e-6 over a window of 12 passes), the worst case is worse than the comment says:
with a constant term nine orders above the variation the algorithm runs **zero
cycles**. It reports convergence, the answer is the starting point, and nothing
anywhere says so.

The stagnation window cannot rescue that shape, and the reason is structural: it
asks whether the best decision is IMPROVING, and a search that never started has
nothing to compare against. It defends the case it was written for - a fit that
begins and then stalls - and not this one.

What would: testing the spread against the RANGE of values seen rather than
against their magnitude, or subtracting the best decision's own value before
comparing. Both change when every existing fit stops, so neither belongs in a
coverage change. The behaviour is pinned in `testcase_simplex` under a name that
says what it costs.

## The simulated-annealing minimiser had never been instantiated

`TDownhillSimplexSAAlgorithm` is a second algorithm class - its own decision
type, its own ranking of the simplex vertices, its own trial step, its own
`AlgorithmRealization` - and nothing in either repository had ever created one.
The suite drove the plain simplex through a single parabola test; the annealing
variant was reachable only by selecting it as the minimiser, and nothing does.

It works. It minimises, and at zero temperature it reduces to the plain simplex,
which is what makes the temperature a dial rather than a different algorithm.
That is now asserted, along with the decision copy that carries the fluctuated
evaluation - a copy that lost it would rank every copy as zero and always prefer
it.

Worth recording as a category rather than a defect: a class offered as an
alternative and never selected is code that ships, compiles, and has never run.
The registry that offers minimisers is the place to notice it - a test that walks
it and instantiates each is the shape that would have.

## Still open

- **`csv_file_loader.pas` raises `ENotImplemented`.** The OHLC loader covers one
  shape of CSV; a general numeric CSV has no loader.
- **There is no export.** Parameters, errors and statistics can be read on screen
  and nowhere else.
- **The automatic background search assumes a concave background**, as neutron
  diffractograms have. It will not work for any other shape, and says so in a
  comment rather than in the UI.
- **Sub-tasks run sequentially** although they are independent by construction —
  see the roadmap (internal) §5.
- **The silent-degradation conversion is unfinished** in `Desktop/`; the checks
  converted so far carry the expression they came from rather than a sentence.
- **The GTK fault behind the freeze above has no known trigger.** The freeze it
  caused is fixed - the process now dies with a readable report instead - but the
  access violation inside `gtk_menu_shell_real_select_item` is unexplained, and
  nothing in the menu code reproduced it.
- **A queued dialog can still be opened while a menu holds the pointer grab.**
  `QueueError` is right for a lost server, and that is the one caller left after
  the change above; it runs from the main loop, which is not the same as running
  with no menu open. Nothing in the LCL reports "a menu is dropped down", which
  is why it is recorded here rather than guarded.
- **Nothing checks that a custom axis's two formulas are inverses of each other.**
  The user gives f(x) to display a value and g(x) to get back from a displayed
  one, and both are required - but whether `g(f(x)) = x` is never sampled. Write
  f(x)=ln(x) with g(x)=log10(x) and the chart maps positions to the wrong place
  in one direction only, which looks like a drawing fault rather than a formula
  one. Sampling the round trip over the profile's own range would settle it in a
  few lines; it would also reject input the program accepts today, which is why
  it is recorded rather than done. See `Desktop/custom_axis.pas`.
- **Nothing checks that the sibling packages' default branches still build this.**
  The clean-clone build is a manual step; until it runs in CI, the dependency can
  drift again without anything saying so.

## A curve list cannot copy itself if it is subclassed

`TMSCRCurveList.GetCopy` constructs `TMSCRCurveList` **by name** rather than by the class
it was called on, and `TSelfCopiedComponent.CopyParameters` then asserts that source and
destination have the same class. So a descendant asking for a copy of itself fails an
assertion instead of getting a copy.

Latent: nothing in either repository derives from the class today. It is pinned by
`tests/testcase_curve_list_axis.pas`'s `ADescendantCannotCopyItself`, so whoever adds the
first real descendant meets a failing test rather than an assertion in the middle of a
table refresh. The fix, when one is wanted, is `TMSCRCurveListClass(ClassType).Create`.

## The light coverage scope was measuring a shrinking denominator

Two flaws in `tests/build.sh` and the light branch of `Invoke-CoverageRun`, both
found by running the plain-FPC binary directly rather than through the coverage
task:

**Assertions were compiled out.** `fit_tests.lpi` sets `IncludeAssertionCode`,
so the Lazarus-built binary has them; the plain-FPC command line did not pass
`-Sa`. A dozen tests assert that a precondition is *refused* - the call then
quietly succeeded and the test failed - and every line guarded by an `Assert`
was unreachable, so it counted as missed.

**Stale units were silently reused.** The coverage build shares its unit
directory (`-FEtests`) with `tests/build.sh`, and FPC reuses a `.ppu` whose
source has not changed even when the compiler options have. A unit left there by
an ordinary run carries no DWARF line table, so it was measured as zero lines and
disappeared from the report altogether - the denominator shrank instead of the
build failing.

Fixed: `-Sa` on both plain-FPC command lines, and the light coverage build now
deletes `tests/*.ppu tests/*.o` first. The light scope went from 5573/7588 =
73.44% to 6583/8144 = 80.83% - the denominator grew by 556 lines that had been
quietly absent.

## Two rules for what may place a curve, and they disagree

`parameter_roles.CanHoldRole(P, prPosition)` says a parameter may place a curve
when it is not the abscissa. `PositionChoices` - the list the properties dialog
actually offers, extracted from `FillComboPosition` - is narrower: it admits only
`Shared`, `Variable` and the two position types, so a parameter already holding
the amplitude or the width is not offered.

The narrow rule is the better one: a curve whose position is also its height
moves whenever the fit changes how tall it is, and nothing refuses it. But
`CanHoldRole` is what a caller reaching for the rule by name would find, and it
would let exactly that through.

Left as it stands, with both pinned by `testcase_parameter_choices.pas`, because
changing `CanHoldRole` changes what the dialog offers and that wants seeing in
the running app first.

## Most of what is left in the main form is not logic, and the debt metric cannot tell

`tools/coverage/wrappers.txt` counts every line of a wrapper as debt, and
`--wrappers-may-not-grow` holds the total down. That is the right gate while
there is logic to extract. It is not a measure of how much is left to extract,
and after several rounds of Phase 5 the two have come apart.

Reading the ten longest methods still in `form_main.pas`: `OnChartClick`,
`GridDataEditingDone`, `CreateCurveTypeMenus`, `AimPickAtActiveSerie` and
`ApplyViewMode` are already reduced to reading widgets, calling one counted
function and writing the answer back - each carries a comment naming where its
rule went. `FormCreate` (207 lines) is start-up wiring. `ShowCustomAxisDialog`
(136) builds a form control by control, with the only decision in it - whether
the formulas are usable - already in `custom_axis.pas`. `BuildParameterLegend`
and `CheckListBoxLegendDrawItem` are anchor arithmetic and painting over
`legend_layout.pas`.

So the residue is dominated by construction and forwarding, which is exactly
what the wrapper definition says a wrapper is - but it is 2000 lines of it, and
the debt figure reads as though 2000 lines of logic were still hiding there.

Two consequences worth stating rather than discovering:

- **The wrapper total will not approach zero**, and a plan that expects it to
  will read every later commit as a failure. What it can do is stop falling,
  which is the honest signal that the extraction is finished.
- **The remaining extractions are small ones.** `UsableViewMode` (13 lines of
  decision) and `chart_panning` (two formulas) are the shape of what is left:
  worth doing, because each was untested and one of them decided whether the
  application could start, but no longer worth measuring by the line.

## A falling total has two causes and the gate could not tell them apart

`--no-total-drop` was built to catch one thing: logic extracted from a UI class
landing without its tests. It fired for the first time on something else.

Adding `testcase_sidecar_startup.pas` made the light binary LINK
`Worker/python_sidecar.pas` for the first time - nothing in that binary had
referenced it before, so its 112 lines had never been in the light denominator
at all. They arrived at 46%, against an average of 81%, and the total fell 0.30
points while nothing anywhere got worse. More is measured than before, which is
the opposite of the regression the gate names in its message.

The gate cannot distinguish the two cases, and should not try to: a drop is
worth a sentence in the commit either way. What it can do is price the new units
out, and it now does - it reports the figure the total WOULD have moved to had
those units already been in the baseline, so the reader is told which case they
are in instead of comparing two reports by hand. Both halves are pinned in
`tools/build-tests/coverage.tests.ps1`, including that the note does NOT appear
when an existing unit lost ground - otherwise an unrelated new unit in the same
commit would explain away the one case the gate exists for.

Worth expecting again: every unit that is only reachable from a test which does
not exist yet is absent from the light denominator, and will join it below the
average on the day somebody writes that test.

## Twenty-eight members of the parameters table answered a caller that does not exist

Phase 2 recorded that `IGridDataSource` was vestigial - nothing in this
application ever handed a data source to a grid - and `curve_list_grid.pas` was
written deliberately not to implement it. What was not done at the time was
removing the members that existed only to satisfy it.

Checked by reference rather than by reading: of everything `TCurveListBase`
exposed, the grid wrapper calls twenty-six members. Twenty-eight others have no
caller anywhere in either repository - the whole cell-editing surface
(`ValueToString`, `StringToValue`, `SetValueByDefault`, `IsDataValid`,
`BeforeStringToValue`, `GetCellEditMask`, `IsCellDisabled`), all nine `MayIDo*`
permissions, the seven row-and-column notifications, `IsDataSourceEmpty`,
`GetColNumFixed`, `GetRowNumFixed`, and two width-item helpers whose row-height
twins ARE called.

Three of them carried a SECOND, CONTRADICTORY MODEL OF WHAT A COLUMN MEANS.
`ValueToString`, `StringToValue` and `SetValueByDefault` mapped column 0..5 onto
a fixed list of names - Intensity, StartPos, PeakPos, FinishPos, IntCorrFactor,
Sigma - from a time when every curve had those parameters, while the live half
of the class derives its columns from `CollectColumnNames`, the union of the
names the curves actually carry. Had anything ever called them, the table would
have read one curve's values under another curve's headings. One of them still
raised with a message naming `data_classes`, a unit deleted in Phase 2.

Deleted: `Server/curve_list.pas` falls from 1197 lines to 869.

WORTH SAYING PLAINLY: some of this was covered - by tests written in this same
effort, one commit earlier, before the reference check was done. Coverage says a
line ran, not that anything needs it, and a sweep that walks a class's public
surface will happily test whatever it finds. The check that pays here is "who
calls this", and it belongs BEFORE the tests, not after.

## Three more no-caller methods, in the client this time

The same reference check run over `TFitClient` found three members with no
production caller: `GetCurveList`, `GetSelectionMode` and
`ReplacePointInSelected`.

`GetSelectionMode` is the more interesting of them: the `SelectionMode` property
reads the FIELD directly, so the getter beside it had never been called by
anything. `ReplacePointInSelected` is the odd one of the five ReplacePointIn*
siblings - it edits the local set and, alone among them, does not tell the
server. That asymmetry is defensible for a set the client owns, but nothing ever
asked for it.

`GetCurveList` had two tests over it and no caller. Worth noticing what that
means: a test is not a use. The check that a member earns its place is "which
production code calls this", and a test written against a public surface answers
nothing about it. Both tests are removed here with the method.

Deleted, and the same check should be run over the other units before more tests
are written against them - see the entry above about `curve_list`.

## Four filer callbacks that nothing can reach, and one deliberately not deleted

`Settings_v1` and `Curve_type` both override `DefineProperties` to do NOTHING:
the single `Filer.DefineProperty` line in each is commented out, with the note
"ne rabotaet s XML-potokami" - it does not work with XML streams. So
`ReadCurveTypes`, `WriteCurveTypes`, `ReadParams` and `WriteParams` are never
registered with a filer, and there is no path by which the streaming system can
call them.

They are dead by the same test as everything else deleted in this effort. They
are NOT deleted, and the difference is worth stating: the commented-out line
names them, so it is a restorable "someday" marker rather than an accident.
Removing the callbacks would leave a comment referring to methods that no longer
exist - dead AND broken rather than dead and revivable.

What the code is actually waiting on is a decision nobody has taken: whether user
curve types should stream through the component filer at all. They are persisted
another way today - `form_main` writes them itself - so the answer is probably
no, and the whole group should go together with the commented line. That is a
product decision rather than a cleanup, which is why it is recorded here instead
of being made in passing.

## Six more no-caller members, and one that was a test seam in disguise

The reference check, now run before writing tests rather than after, found six
more: `TFitService.CurrentCalcProfile`, `GetCurvePoints` and `Integrate`;
`TFitTask.ClearBuiltCurves` and `GetLossValue`; and `TFitViewer.ValToStr`.

`ValToStr` is worth a note of its own. It was reduced to a one-line forward to
`summary_table.CurveValueText` during the extraction work, with a comment saying
it forwards "because the other four grids in this unit still write their cells
directly" - and by the time the extraction finished, those grids had been
rewired too. So the method was left with no callers and a comment describing a
state that no longer existed. A forwarding method is exactly the shape that
survives its own reason for being.

`GetLossValue` was the interesting one: a public one-line alias for the
protected value the optimiser minimises, whose only caller was
`testcase_loss_real_data`. Deleting it broke that test, which is the point - it
was a method kept in a production class for a test to use. The test now reaches
the protected member itself, by cast rather than by construction, because the
task is built by `BuildTaskFromProblem` and there is no point at which a
descendant could be substituted.

Running the check BEFORE writing tests is what stopped this round repeating the
`curve_list` mistake, where a sweep tested twenty-eight members that turned out
to have no caller at all.

## Pressing Stop a moment too late tells the user the fit never started

`POST /actions/stop` is refused with 400 and "This operation is not available
right now. The calculation not started." when nothing is running - and the
client turns a 400 into a message the user reads.

The stop button is live for as long as a fit is, and a fit can finish between
the press and the request arriving. So the user who watched a fit run, pressed
Stop as it was ending, and is then told the calculation was never started has
been given a message that contradicts what they just saw.

Harmless, and pinned as it behaves by
`testcase_rest_api.StoppingWhenNothingIsRunningIsRefused`. The fix is to answer
200 for a stop with nothing to stop - which is what every other idempotent
request does, and what the client already assumes: `TFitClient.StopAsyncOper`
reads nothing back, because the completion comes through the usual callback.
Not made here because it changes server behaviour, and the same state machine
refuses several other operations for reasons that ARE right.

Found while covering the action routes: the assumption in the test was that this
succeeded, and the run said otherwise.

## Deleting a point that is not there faulted, in a method documenting the opposite

`TPointsSet.DeletePoint` opens with "Looks for the point; its absence is not an
error" and ends with an `else` branch that leaves the set alone. That branch was
unreachable.

The rebuilt array was sized `PointsCount - 1` before anything was searched for,
so when nothing matched, the loop copied every point into an array one short and
the assertion inside it fired first. Assertions are compiled into the shipped
builds (`IncludeAssertionCode` in both `Fit.lpi` and `fit_server.lpi`), so this
faulted visibly rather than corrupting the heap - but it faulted, on a call the
method says is ordinary.

**Fixed.** The array is sized for the worst case and trimmed once the answer is
known.

## Moving a point onto another leaves two at one abscissa

`TPointsSet.ReplacePoint` keeps the set single-valued on the path that ADDS a
point: asked to put a point where one already sits, it overwrites that point's
value instead of adding a second. The path that MOVES a point does not check.

When the previous coordinates match an existing point, it is moved to the new
abscissa without asking what is already there - so typing into the parameters
table an abscissa that another row already holds leaves the set with two y
values at one x. The chart then draws a vertical segment and the fit integrates
over both.

Nothing raises: `Sort` was made correct for repeated abscissae after an earlier
defect, so the set survives being in a state its consumers assume it cannot be
in.

Not fixed, because the right answer is a product decision - merge the two points,
refuse the edit, or move the point and drop the one it lands on. Pinned as it
behaves by `testcase_points_set_edit.MovingAPointOntoAnotherLeavesTwoAtOneAbscissa`,
so whoever decides finds a test rather than a surprise.

## Every built-in curve carries a parameter that stands for nothing

`Curve_parameters.Create` adds a placeholder before anything else: a parameter
named `'?'`, typed `Argument`, value 0. The comment beside it says "Collection
should contain at least one item, otherwise is written incorrectly", with a TODO
to check that.

The units that build a parameter list for a purpose clear it - the user-curve
path and the service marshalling both call `Params.Clear` first. The built-in
curve types do not. So a Gaussian declares FOUR parameters and varies three, and
the same is true of every other shape in the framework.

Harmless everywhere it has been looked for: it is typed `Argument`, so the
optimiser's filter drops it and the parameters table excludes it along with
every real abscissa. But two things follow that are worth knowing:
`Parameters.Count` is one more than the curve has, and `SetSpecParamPtr` points
`FArgP` at this placeholder for every built-in type - a real abscissa would have
to be added afterwards to displace it.

Pinned by `testcase_curve_parameters.ButItCarriesAFourthNobodyAskedFor` so the
count is a stated fact rather than a surprise. Not removed: whether the filer
still needs a non-empty collection is the TODO nobody has answered, and the
answer decides whether the placeholder goes or the comment does.

## A curve type's real constructor is the two-argument one

`TGaussPointsSet.Create(AOwner, x0)` builds the amplitude, the position and the
width and wires the role pointers. `Create(AOwner)` is the inherited TComponent
constructor: it compiles, returns a curve, and leaves it with no parameters at
all - so the first thing that reads one faults.

The same shape as `TPositionCurveParameter.Create` recorded above, and the same
hazard: an overload that looks like the obvious constructor and is not. Found by
writing it wrong.

## A second number parser lived in a dialog, and four dialogs used it

`set_maximum_rfactor_dialog.pas` declared `StringToValue`, and the wavelength
box, the background-factor box, the R-factor box and the user-curve parameter
box all called it. It normalised commas and points by hand and read the result
with `StrToFloat` - after assigning the process-wide `DecimalSeparator`, and
restoring it in a `finally`.

The restore was there. What was not guarded is that the whole thing raises on a
typo, which is its documented behaviour: the callers used `try ... except` to
catch it. So a user typing a letter into the R-factor box got the message they
should - and the separator had already been swapped and restored correctly, but
the SAME shape one layer up is what the wavelength box did wrong, and that one is
recorded above.

The deeper problem is that it existed at all. `typed_number.pas` had already been
extracted and tested for exactly this: a full stop whatever the locale, a LOCAL
`TFormatSettings`, and a refusal rather than a guess. Two rules for reading a
number meant two answers - the local copy read `1,500` as `1.5`, a thousandfold
error on a value the user typed.

**Fixed.** All four dialogs read through `typed_number` now, and
`StringToValue` is deleted. The wavelength box uses `TypedNumberIsPositive`,
which also refuses a negative - it did not before - and its message now says what
is wanted rather than only that zero is not.

**A deliberate behaviour change goes with it:** a comma is now refused with a
message instead of being reinterpreted. That is `typed_number`'s documented rule
and the reason is in its test - `1,5` read as fifteen is a plausible wavelength,
and every formula, data file and wire format this program writes uses a full
stop.

`tools/build-tests/number_input.tests.ps1` guards the shape rather than the name:
no unit under `Desktop/` may assign the global separator, no second
text-to-number function may appear, and those four dialogs must go on using the
shared rule.

## A user curve was identified by half a pointer

`CreateUserCurveMenu` stored a curve type's identity in its menu item as
`mi.Tag := LongInt(ct)`, with `(* 32 *)` beside it, and both click handlers
compared with `LongInt(ct) = Tag`.

`TComponent.Tag` is `PtrInt` - 64 bits on this platform. So the assignment threw
away the top half of the address and the comparison threw away the same half,
which is why it worked: both sides were consistently wrong. Two `Curve_type`
objects whose addresses differ only above bit 32 would have matched each other,
and clicking one user curve would have selected another.

Nothing was seen to go wrong, and nothing was likely to on a small heap. It is
recorded because the marker `(* 32 *)` says somebody knew it was a 32-bit
assumption and left it in a build that has not been 32-bit for years.

**Fixed:** `PtrInt` on both sides, and the comparison now carries a note saying
what identity means here - a list that changed since the menu was built fails to
match, rather than selecting whichever curve now sits at that position. Failing
to match is the safe direction; this project's own rule is that a wrong value
under a plausible label is the expensive failure.

The usability rule beside it - a stored curve with no formula is refused with an
explanation instead of failing an assertion inside the optimiser - moved to
`curve_type_menu.UserCurveIsUsable` and has tests.

## Sixteen of the twenty-three keypad buttons built a formula nothing could evaluate

The user-defined curve dialog offers an on-screen keypad of function buttons.
Each inserts its own name into the formula box; the formula is then evaluated by
`Common/native_math_expr.pas`, which is FPC's `fpexprpars` plus five registered
special functions (`erf`, `erfc`, `erfcx`, `voigt`, `emg`).

`fpexprpars` knows `cos`, `sin`, `arctan`, `abs`, `sqr`, `sqrt`, `exp`, `ln`,
`log`, `frac`, `int`, `round`, `trunc` - and nothing else. So of the
twenty-three names the keypad offers, seven evaluate and **sixteen do not**:

`Arccos`, `Arcctg`, `Arch`, `Arcsin`, `Arctg`, `Arcth`, `Arsh`, `Arth`, `Ch`,
`Csch`, `Ctg`, `Cth`, `Sch`, `Sh`, `Tg`, `Th`.

Two of those need no new mathematics at all: the engine spells the tangent `tan`
and its inverse `arctan`, and the buttons say `Tg` and `Arctg`. The other
fourteen are hyperbolic and inverse-trigonometric functions the engine has no
implementation of.

The symptom is a curve that will not evaluate, reported as an invalid
expression, with nothing pointing at the button that produced it - and the user
has no way to know that pressing a labelled key on a keypad the program drew was
the mistake.

**Fixed: all sixteen are registered, in both engines.** The alternative was
relabelling the keypad to the seven names that worked, which loses fourteen
functions from the offer; these are the names the notation uses in this field,
and six of them - the reciprocals and the two inverse cotangents - have no
single name on either side and would otherwise have to be written out by hand in
every formula.

The native engine and the Python sidecar are held to a parity rule
(`ExpressionToNumpy`, `tests/expr_fidelity_cases.txt`, `test_fit_backend.py`),
so each name landed in both or user curves would have stopped fitting under the
Python minimizer - a failure that shows up only when someone switches backend,
and reads as the backend being broken. Ten have a numpy name and
`NumpyFuncName` renames them to it. The six numpy has no name for keep the
engine's own spelling and `Worker/py/lineshapes.py` provides them.

**The definitions are the load-bearing part**, because a reciprocal defined as
the wrong reciprocal still returns a plausible number for a plausible argument.
`ctg` is `1/tan` on both sides rather than FPC's `cotan`; `arcctg` is
`pi/2 - arctan` rather than `arctan(1/x)`, which is discontinuous at zero; and
`arcth` is `arctanh(1/x)`. Twenty-three rows in the fidelity fixture evaluate
all sixteen through both engines, and the Pascal tests check the identities -
`ctg(x)*tg(x) = 1` fails for an inverse where a value comparison at one point
might not.

**Case is the trap in the transpiler.** An unmapped name passes through
UNCHANGED, so `Tg` typed with a capital T would reach Python as `Tg`, and Python
is case-sensitive where fpexprpars is not. Every one of the sixteen therefore
has an entry in `NumpyFuncName` even where the two spellings are identical -
which is also why `arcsin` and `arccos` are listed there.

`tools/build-tests/formula_keypad.tests.ps1` now checks the whole chain: every
button's name is registered, has a numpy spelling, and that spelling is a name
the sidecar provides. Its `$NotImplemented` register is empty and kept, so a
button added later whose name nothing registers fails rather than shipping.

## A parameter cannot be named after a function, and now says so instead of raising

Registering the keypad's sixteen made an existing hazard much easier to reach.
`ParseAndCalcExpression` scans a formula for identifiers, adds the ones the
parser does not already know as variables, and then - on every evaluation -
seeds each with `IdentifierByName(name).AsFloat := 0`.

For a name the parser DOES already know, nothing was added and
`IdentifierByName` handed back the existing definition. When that definition is
a function, setting `AsFloat` on it raises. That raise is in the seeding loop,
which runs on every one of the millions of evaluations a fit makes, and nothing
on that path catches it.

It was always reachable - a parameter called `sin`, `exp` or `pi` does it - but
`th`, `ch`, `sh` and `tg` are plausible parameter names in a way that `sin` is
not, so registering them widened it from a curiosity to something a user could
walk into by naming a parameter after an angle.

**Fixed:** the scan refuses a formula whose parameter collides with a
non-variable identifier, returning the same `0` the caller gets for any formula
it cannot evaluate, and the value-setting loop only writes to variables. A name
that merely STARTS with a function's name is unaffected - only a token
immediately followed by `(` is a function call, so `theta` is still a parameter.

## SimpMath's vector classes had no caller in any repository

`IVector`, `ISpace`, `IComplexVector`, `E3DVector`, `T3DVector` and
`T3DComplexVector` - about ninety lines of crystallography vector machinery in
the exported section of `SimpMath`. Nothing in this repository, in any module
pack, or in `fitminimizers` itself referenced any of them; the examples that
look as if they might declare their own unrelated `T3DVector` record. Both classes are abstract
(`SetNorma` and the three complex accessors), and no descendant exists anywhere,
so as shipped they could not be used without being subclassed first - and
nothing subclasses them.

**Deleted**, along with the `Classes` unit reference that existed only for the
`TComponent` they descended from. It is a breaking change for any consumer
outside these repositories, which is why it was a decision rather than a
cleanup; `fitminimizers` is published as a component library. `TDoubleVector3`
and the free functions over it stay - those are what the affine machinery and
the fitting engine actually use, and they are tested.

## The parameters grid is written to and never read back

`TCurveListGrid` carried `GetDataFromGrid` and `GetRowContents`, the read-back
half of the curve-parameters table. Neither had a caller - and neither did
`TRowCompList.GetDataFromGrid`, the abstract method they were extracted from, at
the tag before this work started. The path has been dead for as long as the
history shows.

It is dead because the table is not editable. `TCurveListGrid.Assign` sets
`Options := StaticOptions`, which carries no `goEditing`; the form gives
`GridParameters` `AutoEdit = False` and an `Options` set without it; and
`TNumericGrid`'s double-click-into-edit path is inside `{$ifndef lazarus}`. The
`OnEditingDone` handler the form wires to it dispatches on `Sender` and has no
branch for that grid.

**Both dead methods are deleted.** `TCurveListBase.ApplyRowCellText` is kept: it
is the inverse of `RecalcParamValue`'s axis transform, which is live, and an
inverse that exists in only one direction states the contract by halves. Whether
the table should become editable - which would need the edited value to reach
the server, not just the local parameter object - is a feature question and is
left open.

## Gauss refused a nil points array differently from its four siblings

`SimpMath` fills a points array through five procedures - `Gauss`, `Lorentz`,
`PseudoVoigt`, `AsymPseudoVoigt`, `TwoBranchesPseudoVoigt`. Four of them begin
with `if not Assigned(PointsArray) then raise EPointsArrayIsNotAssigned`.
`Gauss` began with `Assert(Length(PointsArray) > 0)`.

That made it the odd one out twice. It reported a caller error as an assertion
failure rather than by the name the other four use; and assertions are compiled
out of every build except the test binaries (`-Sa`), so in a release build
`Gauss` silently did nothing where its siblings raised.

**Fixed:** `Gauss` now refuses the same way the other four do. The five array
fillers have tests (`TSimpMathShapeArrayTest`), which is also where the related
surprise is pinned: `SetLength(P, 0)` leaves a dynamic array nil in FPC, so an
empty points array is indistinguishable from a missing one and all five refuse
it.

## A curve's position is clamped to a single value, so no fit can ever move a peak

`TPositionCurveParameter.SetValue` clamps `x0` to `[Fx0Low, Fx0High]`, and the
declaration says what that range is meant to be:

> SetValue clamps x0 to [Fx0Low, Fx0High] (the neighbouring data points), so a
> bounded backend fit keeps each peak in the same window the native engine
> allows.

The two bounds come from `SetBoundaries(x0, PointsSet)`, which walks the points
set looking for the nearest sample below `x0` and the nearest above it. It has
exactly one caller other than itself - `TPositionCurveParameter.Create(x0,
PointsSet)` - and every curve type calls that constructor the same way, from
inside its own constructor:

```pascal
constructor TGaussPointsSet.Create(AOwner: TComponent; x0: double);
begin
    inherited Create(AOwner);
    ...
    Parameter := TPositionCurveParameter.Create(x0, Self);
```

`Self` is the curve, and at that moment the curve holds **no points at all** -
they are added later, when the profile is attached. So the search finds no
sample below `x0` and none above it, both indices stay -1, and the routine's own
fallback collapses the range:

```pascal
    if Lowindex = -1 then Fx0Low := x0;
    if Highindex = -1 then Fx0High := x0;
```

`Fx0Low = Fx0High = x0`. The clamp is to a single point, permanently: nothing
ever calls `SetBoundaries` again.

**What that costs.** Every write to a position goes through this setter:

* the native optimiser's, through `TCurvePointsSet.SetVariableValue` (line 383,
  `Parameter.Value := Value`) - and `x0` IS in the variable set, so the simplex
  spends a dimension on a coordinate that always returns its seed. Every
  perturbation of a position leaves the residual unchanged, which the optimiser
  reads as a flat direction;
* and `ApplyOutcomeToTask`'s, so a position fitted by the Python or the server
  backend is discarded in silence - the outcome reports a moved peak that the
  model never took.

In both cases a peak sits wherever it was seeded, which is wherever the user
clicked. Nothing reports it.

**Characterised, not fixed** - `TOutcomeApplyTest.APositionIsPinnedToItsSeedAndCannotBeMoved`
and `ItsBoundsAreASinglePointRatherThanTheNeighbouringSamples` pin both halves,
the behaviour and the gap between it and the declaration, so repairing one
without the other fails. The repair is a decision about fitting rather than a
correction to a routine: giving the bounds the profile instead of the empty
curve makes every peak position fittable for the first time, which changes the
answer of every fit this application performs.

## A 690-line unit with no caller, invisible to the coverage report

`Packages/utils/vectors.pas` exported twenty-one routines. Not one had a caller
anywhere in either repository. Its only reference in the whole product was a
single line in the desktop program's uses clause:

```pascal
fit/Desktop/Fit.lpr:46:    vectors,
```

`Server/curve_list.pas` looks like a consumer and is not - its own comments say
`TLongArray` is *"DECLARED HERE rather than imported from
Packages/utils/vectors.pas"* and that the three array operations were *"moved
verbatim from Packages/utils/vectors.pas"*. The curve-list extraction copied
what it needed and cut the dependency; the `.lpr` entry stayed behind, and with
it three `<Unit>` entries in project files.

275 implementation lines had no caller against 39 that did. The largest single
piece was `CalculateSimpExpr` plus `CalculateExpr` - **a 177-line second
expression evaluator**, doing the job `Common/native_math_expr.pas` does, with
its own error-code vocabulary and its own decimal-separator handling.

**WHY FIVE PASSES OVER THE COVERAGE REPORT DID NOT FIND IT.** It never appeared
in that report at all - not counted, not vendor, not wrapper, not even under
"not attributed to any repository". Nothing referenced it, so smart linking
dropped it and it contributed no lines to the measured binary. A report that
joins what RAN with what COULD have run is built from the binary's line table,
and a unit that was never linked has no line table to be missing from. Dead code
of this kind is invisible to coverage BY CONSTRUCTION, and the more completely
dead it is, the more invisible it becomes.

The check that finds it is a different one: for each exported routine, does any
non-test file outside its own unit name it? That is the scan that produced this
entry, and it is worth running occasionally rather than reading the percentage.

**Deleted**, with the `Fit.lpr` line and the three project-file entries
(`Fit.lpi`, `FitPro.lpi`, `Fit_example.lpi`, each `<Units Count>` decremented).
Coverage did not move by a single line, which is the point.

It also makes stale the coverage plan's Phase 3 item 8, which listed
`Packages/utils/vectors.pas (0/281)` as *"the array primitives curve_list.pas
now sits on"*. It does not sit on them; it has its own copies.

## The residual weighting had no home, and its rule lives in Python

`'poisson'` appeared as a bare literal in six places, each deciding the default
for itself: `app_settings`, `fit_service`, `fit_task`, `http_fit_service` (as
the value a server too old to carry the field implies) and twice in
`form_main`. There was no named constant anywhere.

The interpretation was written twice, in two languages, and matched only by
coincidence. `Worker/py/fitting.py`:

```python
if kind == "none": return np.ones_like(y)
return 1.0 / np.sqrt(np.maximum(y, 1.0))
```

and `form_main`:

```pascal
if FSettings.Weighting = 'none' then ApplyWeighting('none')
else ApplyWeighting('poisson');
```

Both mean *anything that is not exactly `none` is poisson* - the empty string
included, which is what every settings file older than the setting carries.

**Weighting does not fail when it is wrong.** It answers a different question
and reports the answer with the same confidence, so every failure here is
silent. Today the fall-through is safe by luck: the default it lands on is the
correct one. The dangerous version is one copy away - a site written as `if
Weighting = 'poisson' then poisson else none` inverts the fall-through and turns
every empty or misspelled value into an unweighted fit.

**Fixed:** `Common/fit_weighting.pas` owns the two names, the predicate and
`WeightingOrDefault`, which every site that stores or sends a value now goes
through - so only canonical names reach a settings file or the wire, and the
case-sensitivity can only ever be met by a value this program did not write. It
lives in `Common/` for the reason `rest_polling` does: both processes need the
answer and must not disagree.

The Pascal predicate is exact and case-sensitive **to match Python**, not by
oversight, and `tools/build-tests/weighting_parity.tests.ps1` guards the
correspondence the Pascal suite cannot see: that the sidecar still tests for
`none` rather than for `poisson`, that the Pascal side has not grown a
`LowerCase` or a `Trim` the backend does not share, and that no seventh site
spells the name.

## The window restored the minimizer by naming its one alternative

`TFormMain.ReadSettings` said, in a comment directly above the code:

> Restore the chosen minimizer; an unknown persisted value falls back to the
> always-available Downhill Simplex.

and then implemented it as `if FSettings.MinimizerKind = MIN_KIND_PYTHON_LM then
that else DHS`. `IsKnownMinimizer` already existed in `minimizer_registry` and is
exactly what the comment describes.

Equivalent today, because two engines are registered. Register a third - which
is what the registry exists to allow - and a user who chose it would be silently
given the first one on every start, with the settings file still holding their
real choice.

**Fixed:** `MinimizerKindOrDefault` sits beside `IsKnownMinimizer` and answers
the first REGISTERED engine for an unknown kind, since registration order is the
order the menu shows and the default registers first - so the rule names no
engine. The window asks it. Covered by five tests, including that every
registered kind survives a restart, which is the assertion the old code would
have failed as soon as there were three.

## An extraction was written, documented, and never connected

`action_state.IsManualPickingMode` had no reference anywhere - not in product
code, not in a test - yet it was complete and carried its own explanation:

> Whether a picking mode is one the user leaves by picking the same entry again -
> so its entry says "stop" rather than "start".

The rule it states was written out three times in `TFormMain`, once per entry:

```pascal
if FitClientApp_.FitClient.SelectionMode <> ModeSelectBackground then
begin ... enter ... end
else
    FitClientApp_.FitClient.SelectionMode := ModeSelectNothing;
```

So the rule lived in a counted unit where a test could reach it, the window
ignored it, and three copies sat in a wrapper where nothing can.

**Getting one of the three wrong costs the user a click that appears to do
nothing.** The tick says the mode is on; the click is read as "enter" rather
than "leave", or the reverse; and only a second click seems to work. The unit's
own header already records that exact symptom as a defect this project has had.

**Fixed by connecting it rather than deleting it**, which is the choice worth
recording: deleting the dead function would have removed one unreferenced
routine and left three copies of the rule in the least testable place in the
program. `ModeAfterPicking(ACurrent, AEntry)` now answers what the window
actually needs - the mode in force afterwards - and is built on
`IsManualPickingMode`, so both are live and both are tested. The three call
sites ask instead of restating. `action_state.pas` 96.70% -> 100%.

The tests state the part the three copies could not: that switching straight
from one picking mode to another needs no stop first (a rule that only entered
from `ModeSelectNothing` would make every switch take two clicks, the first
looking like nothing), and that leaving goes to no mode rather than back to the
previous one.

## Two routines deleted, and one that looked like them and is not

`ui_dpi.UiPixelsPerInch` had no reference at all. It answered
`Screen.PixelsPerInch`, which is what its own comment says every caller should
ask directly once `ApplyUiPixelsPerInch` has run - so it was a wrapper around
the thing it told you to use instead.

`minimizer_registry.MinimizerName` was declared "for logs and menus" and used by
neither: the menu reads `Engines[i].Name` straight off the registry array, which
is better, and nothing logs a minimizer name at all. A declaration promising
something the code does not do is worse than no declaration.

A curve pack's own state wrapper looked like the same thing - no product
caller, twenty-one test references - and is NOT. Its declaration says why: the
engine has no verb for setting a pack's markup, because it holds module state it
cannot read, so the pack supplies a wrapper for a caller that builds a task by
hand. The concrete state class is private to that unit's implementation, so
without the wrapper a test cannot construct module state at all and the pack's
server tests could not exist. **"Only tests reference it" is not by itself a reason to delete
anything.** The question is whether the routine exists for a stated reason; a
seam that says what it is for and is used for that is doing its job, and a
leftover is one whose declared purpose no longer matches any caller.

## The compute service stated one rule about thirty times

`Server/fit_service.pas` decided at every entry point which of its states admit
the operation being asked for, and it decided it inline each time. A clone
detector over the counted units put a six-line block at the top of its report,
repeated fourteen times:

```pascal
Result := '';
if State = AsyncOperation then
begin
    AbortAsyncOper;
    Result := CalcAborted;
end;
```

`InadmissibleServerState` appeared 38 times across about twenty raise sites
carrying three reasons; the profile rule alone - *load data before running this
operation* - was spelled out in full at twenty of them. Two more units repeated a
fourth rule, *a calculation has not been started*, three times between them.

**THIRTY COPIES IS NOT A TIDINESS PROBLEM.** It is why none of the rules could be
reached without driving the whole service, which needs the optimiser - so by this
project's own rule every test that touched them was an integration test, and the
service sat at 67 % with its decisions in the uncovered third. Each rule is a
function of the state enum and nothing else.

**AND THE CLIENT ALREADY HAD THE OTHER HALF.** `Desktop/action_state.pas` answers
`FitIsAllowed` over the same enum, because the window must decide whether to
offer the command the service decides whether to accept - extracted and tested
some time ago. Two statements of one rule, in two processes, with nothing linking
them, and they differ: at `BackNotRemoved` the window disables the fit while the
service would accept it and complete the missing data itself. The service says so
only in a commented-out guard:

```pascal
// if State <> ReadyForFit then
//   raise EUserException.Create(InadmissibleServerState + CRLF + NotAllData);
//  Instead of an error, the data that is needed is created.
```

**Extracted to `Server/service_state_rules.pas`** - five predicates over the
state, the four refusal texts, and `RefuseIf`. About thirty-three call sites
across three units now ask instead of restating, and 21 tests state each rule
over all six states at once rather than naming the interesting one, so a seventh
state has to be considered rather than falling into whichever answer the compiler
gives it.

Three distinctions the copies could not express, now written down:

* **aborting is not refusing.** A command that finds a calculation running may
  replace it and tell the caller the previous one was cancelled; another refuses
  instead. Which of the two a command does is the command's own choice, and the
  service makes it differently for different commands on purpose.
* **a pick is stricter than an ordinary operation by exactly one state** - a
  pick arriving mid-calculation would land on a model being rebuilt underneath
  it, and the user did not ask for the fit to stop, they clicked on a chart. The
  test counts the states where the two disagree and asserts that it is one, so a
  divergence anywhere else fails without the test knowing where.
* **an abort refuses every state but one**, the inverse of every other rule here
  and the one most likely to be written back-to-front. It is asserted as the
  inverse relation rather than as its own list.

`RefuseIf` raises `EUserException` specifically, and the tests check the class:
the REST layer maps that to 400 and everything else to 500, and every engine
refusal came back as a 500 once - which told every consumer the server had broken
and the call was worth retrying unchanged.

**One user-visible string was nearly changed by the move.** The extracted
`CalcNotStarted` was first written as *"A calculation has not been started."*
where the service had *"The calculation not started."*; the wording the service
shipped is what the unit carries. The separator is `#13#10` rather than
`LineEnding` for the same reason: the text travels to a client that need not be
on the server's platform.

## An interface nothing implemented

`IFitProblem` - four members declared as `interface(IFitService)`. No class
declared it, and the type name appeared nowhere but its own declaration. Six
lines, deleted, and misleading with it: it suggested a problem-oriented facade
existed when the real API is `THttpFitService` plus the REST session registry.

## The router answered two questions in one chain of twenty-one conjunctions

`TFitRestApi.HandleRoute` was the largest routine in the counted half of this
program - 455 lines - and it answered *which route is this* and *what does that
route do* in the same if-chain. The first is a function of the method and the
path and nothing else; the second needs a session, a service, an engine and
sometimes a Python sidecar.

So the first could only be asked through the second. "Does
`GET /problems/1/curves/7/points` name a route?" required standing up a problem.
"Is PUT accepted here as well as POST?" could only be answered by sending one.
Twenty-one route shapes, each a conjunction of a method, a segment count and one
or two segment names, with no way to see the table as a table.

**`Worker/rest_routes.pas` is now that table**, and the shapes turn out to be
pairwise disjoint - so it is a flat classifier rather than an ordered chain, and
nothing in it depends on being asked in a particular order. 26 tests, and the
useful ones are the near-misses: a leaf that exists with the wrong method, a
known leaf at the wrong depth, five segments with the wrong last one. A route
recognised one segment too loosely answers a request nobody made; one recognised
too strictly returns "unknown endpoint" for a call the client is entitled to
make, which reads as the feature not existing. Both are silent from the server's
side and both are one character away from correct.

**Two vocabularies that differ by four names.** A PUT may replace the four point
sets the user supplies; a GET may read those and the four the engine computes.
The classifier names them once each. Offering to replace a computed answer would
let a caller overwrite the engine's own output and the next fit would start from
it.

**WHAT DELIBERATELY DID NOT MOVE.** The three 404s that guard the session lookup
stay in the router, in their original order, because they are not route
questions. A request naming a problem that does not exist is answered "no such
problem" even when the rest of its path is nonsense, since the id is checked
before the leaf. Classifying first and refusing unknown routes first would change
that answer - a small thing, and a thing a caller may already rely on. The
handler bodies did not move either: what a route DOES needs the engine, and only
what a route IS is a function of two strings.

**One of my own test expectations was wrong before the code was.**
`POST /problems/12/modules/vendor` is four segments, which the original router
accepts as a module route with the single-segment resource `vendor`; the test
asserted it was unrecognised. The classifier was faithful and the test was not.
The real near-miss is three segments - `modules` with no resource named at all -
and that is what the test says now.

Verified by the 54 existing router tests passing unchanged, which is the point of
doing the extraction this way round: the recognition rule moved, the dispatch did
not, so the suite that drives every route end to end is the regression check.

## Half an extraction is a clone detector's top hit

The state-rules extraction replaced the CONDITION of the compute service's
abort-first step and left the ACTION behind, so fourteen commands still began
with six lines apiece:

```pascal
Result := '';
if MustAbortRunningOperation(State) then
begin
    AbortAsyncOper;
    Result := CalcAborted;
end;
```

A clone detector re-run after that commit put it back at the top of its report,
which is how it was noticed. Extracting a condition and leaving the block it
guards is a half-measure that reads as finished: the rule is testable, the
duplication is not gone, and nothing complains.

`TFitService.AbortedToMakeRoom` is the whole step now - abort what is running if
anything is, and answer the note the caller passes back. Fifteen call sites (the
fourteen plus one written without the `Result := ''` prologue, where the result
was already empty), six lines each becoming one: 89 lines out, 36 in.

The name says what the caller needs to know, which the block did not: **aborting
is not refusing.** The command goes ahead either way; what it owes the user is a
sentence saying the previous calculation was cancelled to make room.

## Three dead declarations, and a guard that caught the fourth thing

Found by two scans that had not been run before - classes nothing constructs, and
free routines in an implementation section with no caller.

**`TOpenCLFitTask`** was `class(TComponent) public end;` - an empty class, no
members, no implementation, no reference anywhere, under the comment "the wrapper
for future OpenCL implementation". A placeholder with no content is a comment
pretending to be code: it costs a name in the interface and appears in every
search of the task types while saying nothing its own comment does not. The
intention is kept as a comment; the declaration is gone.

**`ComparePairs` and `TValuePair`** in `gauss_points_set.pas` were dead together.
The comparator had no caller at all, and the class existed only to be cast to
inside it - nothing anywhere ever constructed one, so the comparator could not
have received a `TValuePair` even if something had called it.

`ComparePairs` had survived several passes of the no-caller scan because that
scan read only INTERFACE sections. A routine defined in an implementation can
only be called from its own unit, so the check is purely local and was simply
missing. Run properly over both repositories it finds exactly one thing - this -
which is reassuring, but the earlier claim that the name-level scan was
exhausted was too confident.

**`form_hint.pas`, its `.lfm` and its `.lrs`** - 64 lines and two resources. The
unit's only reference in the tree was inside a `(* ... *)` block in
`set_maximum_rfactor_dialog.pas`, with the `var FH: TFormHint;` declaration
commented out as well. No `uses` clause named it and no project file listed it,
so it was an orphan on disk that nothing compiled. The balloon-tip call above the
commented block is what replaced it.

**AND THE DIAGRAM GUARD EARNED ITS KEEP.** Deleting `TValuePair` failed
`diagrams.tests.ps1`, because `CURVE_DIAGRAM_DROP` in the generator names the
helper types deliberately excluded from the curve hierarchy picture and is
checked against the parse - its own comment says "a rename here fails rather than
silently dropping nothing". A class removed from the sources and left in that
list is a drop of nothing, which is how a generated picture starts describing a
program that no longer exists. The entry went with the class.

**What was NOT deleted, on the same test as before.** `TCSVFileLoader` is a
51-line stub that raises "not implemented" and that nothing constructs - and two
other units document it as the reserved home for that work:
`data_loader_registration` explains why the OHLC loader is registered instead,
and `ohlc_csv_loader`'s header says csv_file_loader "is left untouched as the
home". A declared intention with content behind it is not a leftover. The empty
OpenCL class failed that test on the content; this one passes it.

## No fit could move a peak, and the suite was green

`TPositionCurveParameter` clamps `x0` to `[Fx0Low, Fx0High]`, and its own
declaration says those are "the neighbouring data points". They were not. They
were the seed, twice, for every curve in the program's history.

The boundaries were read in the constructor:

```pascal
constructor TPositionCurveParameter.Create(x0: double; PointsSet: TPointsSet);
begin
    ...
    SetBoundaries(x0, PointsSet);
end;
```

and **every** curve type constructs it from inside its own constructor, passing
itself as the points set - `TPositionCurveParameter.Create(x0, Self)`. A curve
holds no points at that moment. So `SetBoundaries` looped over nothing, found no
sample either side of `x0`, and its own fallback fired:

```pascal
if Lowindex = -1 then
    Fx0Low := x0;
if Highindex = -1 then
    Fx0High := x0;
```

Both bounds collapsed onto the seed. The clamp was to a single value, permanently.

**What it cost, on both fitting paths.** The native simplex spent a dimension on
a coordinate that handed back its seed whatever it was given - so it explored a
space one axis of which did nothing, and every peak stayed exactly where the user
had clicked. A backend fit was worse, because it looked like it worked: the
gradient minimizer was shipped `GetMinValue`/`GetMaxValue` as the parameter's
range - a range of zero width - and whatever position came back was clamped away
on writeback, while the outcome still reported a moved peak. A fitted model that
is not the model that was applied, which is the same failure
`testcase_parameter_bounds`' header was written about.

**Why nothing caught it.** The parameter's own bounds fixture built a profile and
passed it in at construction, so it exercised the branch that works and never the
one every curve in the program takes. The clamp was consistent with the declared
bounds - both were the seed - so the subset invariant that fixture exists to
check held perfectly. Fits converged, R-factors were acceptable, and the peaks
simply never moved.

**The repair is about ordering, not arithmetic.** A curve gets its points before
it gets its position: the engine calls `SetWindow` and only then assigns `x0`. So
there is no moment in the constructor at which the window can be known, and the
boundaries are read on the first assignment instead - once, measured from the
seed the constructor was given rather than from whichever value arrives first,
because the native and backend paths do not agree on what that is. Measured from
the incoming value, the window would bracket it and clamp nothing at all; read
again on each assignment, it would follow the optimiser downhill one sample at a
time while every individual step still looked clamped.

A copy inherits the window and the fact that the window is settled, and not the
points - they belong to another curve.

**The whole difference is in a test whose name had to be inverted.** It used to
be called `APositionIsPinnedToItsSeedAndCannotBeMoved`, characterising the defect
because the behaviour had been decided blind. It is
`APositionTakesAFittedValueWithinItsWindow` now, and the six lifecycle tests next
to the bounds policies pin the order that makes it true.

**The real-data fits still pass**, which is the reassuring half: making positions
fittable did not destabilise the diffraction profiles the integration suite
measures. It does change the answer of every fit that has a position parameter,
which is not something a suite can sign off - it wants a look at real data
through the real surface.

## Two surfaces disagreeing was recorded as a defect, and is not one

The state-rules unit's own header flagged it: the window disables the fit at
`BackNotRemoved` while the compute service would accept one and complete the
missing data itself, and the service says so in a commented-out guard rather
than in code. It read like something to fix.

Reading both sides says it is a workflow choice, deliberately made twice.

The service refuses a fit on `ProfileRefusal` alone - which is `ProfileWaiting`
and nothing else - and the stricter check that would have refused here is
commented out under its own explanation, "Instead of an error, the data that is
needed is created". A caller that asks for a fit before the background has been
subtracted gets one, on a profile that still carries its background.

The window does not offer the two manual fits there. It does offer *do all
automatically*, unconditionally, and that command subtracts and then fits. So
the workflow the window presents is: fit deliberately once the background is
gone, or press the button that does the whole thing. Neither side is wrong, and
neither is a narrowing of the other.

**Which side to change was the wrong question. Where the rule lives was the
right one.** `FitIsAllowed` sat in `Desktop/action_state.pas`, three states named
in a condition with no comment about the state it left out; the service's half
sat behind a commented-out guard four thousand lines away. Two processes, one
subject, and a difference nobody reading either could see.

It is one pair now. `FitIsOffered` moved into `service_state_rules.pas` beside
`ProfileRefusal`, `action_state` asks rather than restates, and four tests walk
the whole enum over both - including one that asserts *both halves* of the
disagreement at `BackNotRemoved`, because the finding is the difference and
either half alone would keep passing if the other side quietly moved.

The count is asserted too, and it is two states rather than one. The second is
`AsyncOperation`, and it is incidental: a fit entry point aborts what is running
and goes ahead, while the window keeps the command dark for the length of the
operation. Same intent, said twice. Worth writing down only because a test
asserting "they differ at exactly one state" would have been wrong, and wrong in
the direction that looks right.

**The general shape.** A rule stated in two processes is not made correct by
picking a winner. It is made correct by having one statement of it, after which
a deliberate difference is a readable pair and an accidental one is a failing
test.

## Two functions of mine, deleted two commits after I wrote them

`RouteIsWrite` had no reference anywhere. `RouteNeedsProblem` had four
assertions in its own fixture and no caller. Both were written while extracting
the REST route table, on the reasoning that the router would want them.

`RouteIsWrite` said so itself: *"Not used to decide anything today: it is here
because the module route accepts PUT as well as POST and that fact was
previously visible only inside the router."* A comment explaining why a function
has no caller is a function with no caller.

`RouteNeedsProblem` is the more useful case, because it looked wireable. The
router does look a problem up, and does answer for a bad id - but the guards
around that are about path *shape*, not about the route enum: a 404 for anything
whose first segment is not `problems`, another for a path with fewer than two
segments, then the lookup. Substituting the predicate would classify before
refusing, and the router's own comment says that changes the answer a client
gets for a malformed path against a problem that does not exist. So its declared
purpose - "so the router must look the session up" - matches no caller and no
place to become one.

Its tests went with it, including one that walked the whole enum. That pattern
earns its keep over a rule something depends on; over an unused function it
pins a fact nothing reads.

This is the same failure I deleted `MinimizerName` for one commit earlier, and I
committed it twice more in between. **Extracting a rule and inventing its
neighbours are different activities**, and the second one produces code that
passes every scan: it has a caller (a test), a comment, and a plausible name.
The test that catches it is the one that catches any leftover - does the declared
purpose match a caller? - and it has to be run against new code, not only old.

## A third of a unit read as untested, and the cause was a file read

The rule engine sat at 67%, with ninety-seven lines missing, and I had written
those off as failure branches the fixture could not drive. Asked to look again, I
listed the actual missing lines. They were 317 to 438 - one function, the static
rule table, and nothing else but two lines of a guard.

The table is called by exactly one fixture. That fixture reads its oracle from a
file, so it is registered `integration` - correctly, by this project's own rule -
and the coverage run measures the unit half. The table was therefore exercised on
every full run and measured on none.

**A CLASSIFICATION IS NOT A CLAIM ABOUT WHAT A UNIT NEEDS.** The fixture needs a
file because its subject is an oracle shared with a second implementation. The
table does not: it is eighteen records of constants, and asking whether it still
describes the engine needs no file, no pattern object and no fit. One fixture's
reason for being slow had been quietly inherited by everything it happened to
touch.

The same shape, in the same session, in another unit: `ReadOutcomeFromTask` is
what the compute server answers a fit request with, and its only caller in the
suite drives the optimiser to convergence. It reads the curves a task already
holds. A task built and never optimised holds its seeds, which is a perfectly
good thing to read - so a quarter of `fit_task_marshalling` was unmeasured for
the same reason and with the same fix.

**What the new tests are, and are not.** Not a second copy of the table: the
useful questions are identities between the table and the engine - the set of ids
each knows, the severity and the limit each reports - because a hand-written
table beside the code it describes drifts in exactly two directions and both are
silent. A rule added to the engine and not the table is enforced with no sentence
to show the user; a rule left in the table after the engine stopped emitting it
reads as a rule still being checked.

One divergence is deliberate and got its own test rather than an exemption inside
a loop: a contracting diagonal's wave-3 guideline is scored against the
reciprocal of the tabulated target, because the wedge rule caps wave 3 below wave
1 when the shape contracts and a guideline no shape can reach is not a guideline.
A spec record holds one number; the engine reads the form off the geometry. That
test asserts the divergence in both directions, so the exemption cannot outlive
the reason for it.

**The general rule.** When a unit's coverage looks bad, list the missing lines
before theorising about why they are missing. I had a story - failure branches
the parameterisation cannot reach - that was plausible, consistent with the code,
and about the wrong lines entirely.

## A guard that cannot fire, one layer below the check that does

`ProfileValueAt` seeds a curve's amplitude from the profile height at its
position, for a problem that named a position and no height. It opens by
answering zero for an empty profile - a sensible guard against indexing an empty
array.

It cannot fire. `BuildTaskFromProblem` sets the profile on the task before it
looks up any seed, and the task checks the profile holds at least two points. A
problem with no profile is refused there, with a different message, at a
different layer.

So the guard is dead in the way that is hardest to see. It is not unreachable
code in the compiler's sense - the call is live and the comparison runs on every
seed lookup - it is a branch whose condition the caller has already made
impossible. It reads as the handling of a case that is in fact refused upstream,
which is worse than no handling at all: a reader looking for what happens to an
empty profile finds an answer here, and it is not the answer the program gives.

Left in place - one comparison, and the function is private to a unit that could
grow another caller - but written down, and the test that would have covered it
now characterises what actually happens instead: a profile of nought or one point
is refused, so "at least two" is pinned rather than "not empty".

## Silence and approval are the same answer, and a boolean loses that

A module that adds its own kind of markup has to answer two questions when a fit
is requested: is there enough of my markup to fit, and does this fit interval
contain any of it. Both were decided inline in the module's session object, which
reads what the user has selected off the compute service that owns it - a
concrete class of some four thousand lines that reaches the widget set. So a
decision over two integers and a string could only be reached by building one,
and it sat in the session's uncovered part.

Extracted, the interesting part turned out not to be either refusal. It is the
third answer.

**Every rule has to open by asking whether this module places the selected curve
type at all**, and when it does not, the answer is indistinguishable from "your
markup is fine": some other module's markup, or none, governs that fit, and this
one must say nothing whatever. Written as `FitIsAllowed: boolean` those two
collapse into `True` and nobody notices, because `True` is right both times. They
come apart the moment a third state is added, or the moment someone tightens the
rule and reasons about the `False` case only.

A module refusing a fit it has no stake in is a defect that gets diagnosed as the
other module's, because the message names a menu the user is not using.

**The second thing worth writing down is an ordering.** The per-interval rule
answers "not refused" when nothing is marked at all, rather than "nothing is
marked" - the whole-fit rule has already said that, once. Said again per
interval, one omission is reported once for every interval the user happens to
have drawn, and each message names an interval, implying the interval was the
problem. That is the sort of rule that is obviously right once stated and is
never stated when the two decisions live in two methods forty lines apart.

**The messages moved with the rules**, because a refusal the user cannot act on
is reported as a bug. One carries a menu path three levels deep, since a user who
has not found the command cannot act on advice assuming they have. The other
names *two* ways out - mark a pattern inside the interval, or remove the interval
- because an interval drawn by accident is more often the thing to remove, and a
message naming only the first reads as an instruction to invent data. Both are
now claims a test checks rather than comments nobody re-reads.

**One cost taken deliberately.** The interval check now counts what falls inside
the interval before asking the rule, so it walks the markup list even when the
answer is already known. That is a list walk per interval at fit setup, over a
handful of patterns, and it is what keeps the rule a function of numbers - which
is the only reason it can be tested at all. Written down here rather than
optimised back into two early exits that would restate the rule's own first two
branches.

## Both ends of a wire tested, and nothing testing the wire

The engine's verbs travel as strings in a URL. The client's side is a dozen
one-line methods, each posting a literal; the server's side is a registry that
can be enumerated. Both had tests. Neither test could fail for the reason that
matters.

The client's fixture swept its methods and asserted each sent the string it
sends. The registry's fixture asserted the server knows the names the server
registered. Each was satisfied by its own end alone, so **a typo in one client
literal was invisible to the whole suite** - and nearly invisible in use: one
menu command answering "unknown action" into a message box, on a build where the
other eleven work.

Two tests now, in the registry's fixture rather than the client's, because this
is the side that can be enumerated - the client's literals cannot be read out of
it, only observed by making the call and seeing where it went, which is what the
transport mock is for.

**The reverse direction found something on its first run.** Every verb the server
registers must be reachable by some client call, and `select-profile-interval`
was not in my sweep. That direction catches the opposite mistake to the typo: a
verb the server grew that no client can reach. Not a broken command - a feature
nobody can use, which arrives as a support question rather than a fault, and
never as a test failure. In this case the client did have the method and my list
was short, which is the test doing its job on the test.

**The limit, stated because it is real:** a client method added and never listed
in the sweep is caught by neither direction. The forward test only checks what it
is given; the reverse only checks the server's list. Closing that would need the
client's verbs to be enumerable, which would mean a table on the client side too
- worth doing when there is a second reason for one, not for this alone.

## One line per function, missing, for eight functions in a row

While chasing the tail I noticed eight missing lines in a client unit at exactly
five-line intervals. They were the `function` header lines of eight one-line
delegating methods - and six of the eight are called by a test that asserts what
each one sends.

So the header line is not attributed to the executed code: for a method whose
body is a single call, the entry appears under the callee. Six of those eight
lines will never be reported as covered no matter what is written.

Two of the eight *were* genuinely untested and are now called, so the look was
not entirely false. But **a per-line report over very small functions carries
noise of the same order as the signal**, and I had been about to write tests
aimed at eight lines of measurement artefact. The report is reliable for blocks;
for a family of one-liners, read what the tests assert instead.

## One public field was the whole reason a flow could not be tested

Defining a curve type of one's own takes two modal windows: a name and a formula,
then which parsed parameter plays which role. The sequence between them is not
linear - a formula that will not parse sends the user back to the first window,
rejecting the roles sends them back too and the draft type has to be removed on
the way, and cancelling either abandons the lot.

Twenty-seven lines, written inline in the class method that opened the windows,
and not one path through it reachable by a test. Including the one that matters:
the draft is stored the moment the formula parses, so rejecting the roles has to
delete it before going back, or a curve the user rejected stays in their list
looking like one they made.

**Four of the five collaborators were already interfaces.** A parser, a factory,
a storage and the formula dialog - all of them injected, all of them mockable. The
fifth was a global dialog object whose public curve-type field the caller set
before showing it:

```pascal
UserPointsSetPropDlg.FCurveType := ct;
case UserPointsSetPropDlg.ShowModal of
```

Two statements that had to stay together, one of them reaching into another
unit's public field. That was the whole of it. Behind an interface whose single
method takes the curve type as an argument, the sequence names no window, and the
five doubles that drive it are ordinary recording objects.

**The modal constants had to go too, and that is where the enum came from.** A
sequence comparing `ShowModal` against `mrOk` and `mrRetry` still names the widget
set. The adapters map a modal result onto one of three answers - accepted, start
again, cancelled - which is a mapping that belongs in the part that knows about
windows, and leaves the sequence over three named answers.

**Three, not two, and the third is the content of the change.** "Cancelled" and
"start again" are both ways of saying no, and they are opposites: one leaves the
program as it was, the other keeps the user in the flow with a draft to clean up.
In the original they were distinguished by which of two labels a `goto` jumped
to, and there is no boolean that holds that difference.

**Seventeen tests, and one of them characterises rather than asserts.**
Cancelling the roles dialog does *not* delete the draft, where rejecting them
does - so a user who abandons the second window finds a type in their list with no
roles assigned. Whether that is right is a question about what a user expects to
find, not about this sequence, and it is what the program has always done. Pinned
so that changing it is a decision rather than an accident.

**The mocks answer past the end of their script on purpose.** A dialog asked more
times than it was scripted for answers "cancelled" and counts the overrun, so a
sequence that fails to terminate fails as a wrong count rather than by hanging the
suite - which for a loop-shaped flow is the difference between a red test and a
CI job killed on time.

## Extracting past an interface orphans it, and I did it twice in a day

Two functions were deleted in the morning for having no caller. By the afternoon
I had orphaned an interface the same way and written a comment claiming
otherwise.

`ICreateUserPointsSetDlg` answered a modal result. Its only caller was the
user-curve definition sequence. When that sequence moved onto three named answers
- so that it could be tested without a window - the interface had no caller at
all, and the adapter I edited in the same commit gained the sentence *"which is
what its other callers want"*. There were none. I had not looked.

**The pattern is specific to extraction, which is why the ordinary scans miss
it.** Adding a function with no caller is easy to notice: nothing calls it, and a
no-caller scan finds it. *Moving a caller off an interface* leaves the interface
with its unit, its adapter, its entries in three project files and its place in a
generated diagram - everything except the one thing that made it load-bearing.
Each surviving reference is a reason it looks alive.

**What caught it was neither a test nor a scan.** The diagram generator parses
the sources and refuses to publish a figure naming something that no longer
exists, and `ICreateUserPointsSetDlg` was a required symbol of the user-curve
figure and a participant in the configure-a-curve-type sequence. Deleting it
failed the build. That is the second time in one session that check has earned
its keep - the first was a helper class deleted out from under
`CURVE_DIAGRAM_DROP` - and both times what it caught was a stale *picture*, which
is the artefact nobody re-reads.

**So the rule to run after an extraction, not before:** for every interface,
class or unit the extracted code used to name, ask what still calls it. The
extraction's own tests pass either way, and the coverage figure improves either
way - the orphan's lines simply stop being reached, which reads as the wrapper
exclusion doing its job.

## What is left, and why it is not the same kind of work

Three sessions of tail-chasing ended with the counted remainder dominated by four
categories, and it is worth writing them down so the next pass does not start by
re-deriving them.

**Genuinely across a boundary.** The compute service and the task are the
optimiser; the sidecar wrapper is `TProcess` and HTTP; the transports are
`TFPHTTPClient`. Between them these are most of the remaining lines. They are
covered by the integration half, which the coverage run does not measure, and
that is the measurement working as designed rather than a gap.

**Filesystem by nature.** Both loggers, the loaders' file-reading entry points,
the settings storage. Each already has a `TStrings`-fed or in-memory twin that IS
a unit test; what remains is the syscall.

**Reached only through a thread or a global.** The client's worker thread and its
`Synchronize`; the curve-type singleton's "nothing selected yet" branches, which
are reachable only before any test has moved a process-wide cursor and therefore
not reachable reliably at all. The first of these yielded to making one method
virtual. The second would need the singleton to be resettable, which is a change
to production shape for a test's benefit and was not made.

**Measurement noise.** A method whose body is a single call has its `function`
line attributed to the callee, so a family of one-line delegations reads as
uncovered however thoroughly it is tested. Eight such lines were nearly the
subject of a test-writing session. **List the missing lines before theorising
about them** - twice in this effort a plausible story about why a unit was
uncovered turned out to be about the wrong lines entirely.

What is NOT on the list, because it kept turning out to be false: "this needs a
mock we do not have". Four of the five collaborators of the hardest sequence were
already behind interfaces; the fifth was one public field. The rule table looked
like failure branches and was a static array. Before concluding that a unit is
out of reach, read what it actually touches.

## The last large block of untested logic was a validator, not an algorithm

Asked once more whether anything was left worth extracting, the honest answer
needed the per-line report rather than a theory. The compute service had five
hundred missing lines - and they were in a hundred and sixty-two separate blocks,
largest seventeen lines. That fragmentation is the signature of scattered
branches, not of a module waiting to come out.

With one exception, and it was sixty-six lines with **none** of them covered: the
validator for a user's own curve formula. Thirteen per cent of the service's
whole gap in a single routine, and it touched nothing the service owns - an
expression parser, a parameter container and five strings.

**What made it worth its own unit was not the arithmetic. It was the five
refusals.** Every one is read by a person mid-way through writing a curve,
looking at their own text, with no other information; each has to say what is
wrong AND what to type instead. Two of the five are near-duplicates that must not
be swapped - "the formula must CONTAIN the variable x" for a formula with no
symbols at all, "the formula must USE x as its argument" for one with symbols and
none of them the axis. Shown the wrong way round, someone who wrote `A*b + c` is
told to add a variable they can see they already have. Nothing had asserted a
word of any of them.

The naming convention it implements is an interface the user programs against and
is documented nowhere they can see, so the near-misses are pinned as hard as the
hits: `x0` is the position and `x1` is not, `sigma` is a width and `sigma2` is
not. Getting one wrong does not fail - the curve fits, and wanders off its peak or
divides by zero.

And two rules that look arbitrary alone are tested together: `sigma` starts at
0.25 rather than 0, and a formula is refused if it cannot be evaluated at its
starting values. Either separately is a detail; together they are why
`A*exp(-sqr((x-x0)/sigma))` is accepted and the same shape written with `w` is
refused, and the difference is the parameter's NAME.

## A malformed formula was accepted the second time it was refused

The extraction's tests found it. Two identical calls returned different
refusals - which is not a thing a pure function does, so it was worth probing
rather than working around.

`ParseAndCalcExpression` caches on the last expression. On a failure it set its
own sentinel to force a fresh attempt, and left the underlying expression parser
holding the failed text. The retry then re-registered the identifiers and assigned
**the same text** to that parser, whose property setter does nothing when the text
has not changed - so nothing was re-parsed, nothing raised, and the formula was
declared valid with no tree behind it.

Measured directly rather than reasoned about: attempt one answers 0, attempts two
and three answer -1, and a different formula in between resets it to the same
pattern.

**What it cost the user.** Type a bad formula: "The formula could not be
understood". Press OK again on the same text: the formula is accepted as
parseable, discovery runs on whatever the scan found, and they get "the formula
cannot be evaluated at its starting values" - which sends them to change numbers
that were never the problem. Deterministic, and one button press away.

The failure path clears the underlying parser as well now. **Three regression
tests, not one:** the fix that comes to mind first - clearing a flag - makes the
second attempt right and leaves the third wrong, so the test walks three. And one
in the other direction, because whatever the failed attempt leaves behind must not
make the NEXT formula unparseable - a user who mistypes once could otherwise never
enter a formula again without restarting.

**The general point.** A cache keyed on an input, over a stateful thing it does
not own, is only correct if a failure invalidates BOTH. This one invalidated its
own key and left the other half primed to agree with it.

## Two copies of an ordering, neither wrong on its own

The background is a shifted quadratic and the optimiser addresses its four
coefficients by number, so there is a mapping from index to coefficient - written
out once to read the current value and once to write a proposed one, forty lines
apart on the same class.

A reordering applied to one copy and not the other has the optimiser reading the
curvature and writing the offset. The fit runs, every step lands on the wrong
coefficient, the background comes out as a shape nobody asked for, and nothing
reports anything. **Neither copy is wrong in isolation**, which is exactly why no
test over either could catch it.

So the test that matters is the round trip, and it has to be asked PER INDEX with
a distinct value each: a mapping that swapped two coefficients round-trips
perfectly if both are written before either is read, and would pass a test that
wrote all four and then checked all four.

The two sides are also deliberately asymmetric - two of the four are stored as
magnitudes, so a negative proposal comes back reflected rather than refused. That
is a property of the parameterisation and is now asserted as one, counted over the
whole range, rather than sitting as an Abs in one of the two copies.

## Guard branches, and the two things the sweep turned up

About sixty lines across four already-pure units: nil guards on the role queries,
divide-by-zero guards in the pooled loss forms, four symmetric name lookups on the
parameter container, the interpolator's tangent branch, the ratio parameter's
open and closed bounds. No extraction needed - cases in fixtures that already
existed.

Two of them were more than coverage.

**Exactly one objective cannot be pooled across intervals at all**, and the walk
discovered it by raising. An objective normalised by the MODEL divides by a
quantity that is not a property of the data, so summing parts across intervals
would divide by a total that means nothing. That is now a counted property tied to
`LossIsSelfNormalising` rather than something a caller finds out by catching -
and there is still no predicate to ask, which the test says out loud.

**The four name lookups end in `Assert(False)`, and what that means depends on
the build.** With assertions on, a mistyped parameter name raises. With them off,
the readers answer zero and **the writers do nothing at all**:
`ValuesByName['sigmaa'] := 2.5` succeeds, silently, and the value the user set is
gone. A test cannot provoke the release behaviour from here, so what these pin is
that the checked build really does refuse - which is what makes the mistake
findable before a release build hides it.

**And the interpolator's weighted harmonic mean had never been computed.** Every
interior pivot of an alternating count is a turning point, so the tangent pass
takes its zero-tangent branch at all of them - and every fixture used an
alternating wave, because that is the shape a motive pattern reduces to. A
monotone run reaches the other branch. What is asserted is not the branch but what
it exists for: the curve never falls back on itself between two rising pivots, on
unequal spacing, at every tension. Mirrored for a falling run, because the branch
condition is the sign of a PRODUCT - both secants negative gives the same positive
product and the same branch, so a rule written for rising data can be right there
and wrong falling with nothing in the condition to show it.

**One expectation of mine was wrong and the code was right.** `RoleChoices`
always opens with a "(none)" row, with no parameters or with a hundred, because
that row is how the user takes a role away; a list without it would make the first
assignment permanent. Asserted as the exception it is, rather than "fixed".

## The extraction programme is spent, and here is how that was established

Ten rounds of "is there anything left worth extracting" ended with the same
answer arrived at three independent ways. Writing down how, because the cheapest
way to waste the next session is to start the search again from the top.

**The scans, and what each is blind to.**

*Class methods ranked by dependency count.* For each method of a class, count the
fields it touches and the sibling methods it calls; a method with a big uncovered
gap and one or two dependencies is a free function wearing a class's name. This
is what found the background finder (82 lines, no dependencies at all) and, in a
weaker form, the formula validator. It is blind to free functions and to blocks
inside large methods.

*Free functions in implementation sections.* Every earlier scan matched
`function T<Class>.<Name>` and so could not see a plain function at all - which
is the easiest thing in the tree to test, and therefore the most surprising place
for a gap. This found the registries' refusal paths.

*Wrapper units ranked by decision density.* Branches, boolean operators,
arithmetic and formatting calls per method, over the excluded UI classes. This is
the one that answers "is there still logic behind the exclusion".

**Three ways my own scans lied to me, all worth knowing.**

Stripping multi-line comments by collapsing them to a space **shifts every line
number after each comment**, so the offsets and the uncovered counts were
nonsense - the first run cheerfully reported "0 uncovered" for everything and I
nearly believed it. Preserve the newlines.

Matching repository roots with `startswith` put every path of the private pack
under the public one, because the public repository's directory name is a prefix
of the pack's. Longest root first.

And counting fields without counting **sibling method calls** offered methods
whose two fields were incidental and whose real dependency was the rest of the
class. Both, or the list is noise.

**The decision-density scan needs reading, not trusting.** Its top two hits over
`form_main` were `SaveTableAsText` and `ApplyViewMode`; both already delegate
their decisions to counted units, and what the scan had counted was
`if Assigned(MenuItem)` nil guards and `Mode = XCM_T` passed as an argument.
String concatenation counts as arithmetic, too. The metric picks what to READ.

**The substantive finding is about the wrapper debt.** `form_main.pas` and
`fit_viewer.pas` are 2660 of the 3216 excluded lines, and the plan describes
reducing them as "the long pole, 4-6 weeks". It is not that any more:
`fit_viewer` contains **no** formatting or parsing calls and `form_main` exactly
one, inside a comment, and between them they call nine extracted units. The
sharpest single probe was `grep -c` for the `FloatToStr`/`StrToFloat`/`Format`
family - a UI class still holding logic almost always still formats or parses
something, and one that has been reduced does neither.

**What the answer looks like when it is genuinely no.** Not "I could not find
anything" but: three scans of different shapes, each with its blind spot covered
by another, agreeing; the top hits of each read rather than counted; and the one
remaining candidate small enough to name precisely. The last one was thirteen
lines.

## A whole repository was invisible to every scan, because of my own tooling

Eleven rounds of "is anything left worth extracting" were answered from scans
that walked two of the three counted repositories. The optimiser package - which
the denominator has always included, and which the committed baseline reports as
its own section - was in none of them.

**The mechanism, because it is silent and reusable.** `cgcov.py` takes `--roots`
and attributes each source file to the root it lives under; anything it cannot
place goes into a section headed "Not attributed to any repository - usually RTL
or LCL code compiled with line info". The real coverage task passes every root.
My ad-hoc `--detail` runs, written by hand to get per-line data, passed two. So
the optimiser's units landed in that section, **with a summary line and no
per-line list at all** - and my scans, which read the per-line lists, saw nothing
to report. No error, no empty result: just silence that looked like absence.

The tell was there and I read past it for eleven rounds: the baseline says
`### fitminimizers - 1362/1450 = 93.93%` and my detail runs said
`- Decisions.pas - 325/354` under "not attributed". The same numbers, filed
differently.

**The lesson is not about this script.** A tool that classifies its input by a
list you supply will quietly report on a subset when the list is short, and a
subset looks exactly like a whole. When a scan says "nothing found", check first
that it looked everywhere the authority looks - and the authority here is the
committed baseline, which had the section headings all along.

## The same shape three more times: the loop that never improved

Once inside the optimiser package, three of its four search loops had the same
gap, and it is the gap this whole exercise has met most often: **the branch that
replaces a running answer had never run.**

`GetMaxDecision` and `GetMinDecision` pick which member of a population the
search keeps. Each is a loop with a first-candidate branch and an improvement
branch. `GetAbsoluteMax` is a plainer version of the same. In three of the four,
the improvement branch was dead.

**One test-data decision did that.** Every existing test built the same
descending list - 10, 7, 4, 1 - which is exactly what `GetMaxDecision`'s own
comment demands: *"Items must be sorted by decreasing of estimation value."* On a
descending list the first eligible candidate IS the answer for the bounded
maximum and for the absolute maximum, so those loops never improve on anything.
The list is sorted as documented, the tests pass, and half of each function is
unreachable.

**And the documented precondition turns out not to be required.** The improvement
branch is a plain linear scan, so the search finds the right answer on an
unsorted list too. Worth knowing rather than guessing: the population is sorted
by a comparator elsewhere, and a change there would otherwise be assumed to break
these. Now asserted, on lists deliberately not in the documented order - and
noting that the maximum needs an ASCENDING list to reach its improvement branch
while the minimum needs a descending one, which is why one list could never
exercise both.

Also pinned: among equal candidates the bounded searches keep the LAST, because
the comparison is `>=` rather than `>`. The decisions are distinct objects with
distinct parameters and equal evaluation, so which one is kept is which solution
the optimiser carries forward.

`CombEnumerator` is the third instance. It exists to map one integer onto one
index per discrete quantity, and **only the one-quantity case had ever run** -
with a single quantity the decomposition loop's bound is `ValuesNumber - 2`, so it
does not execute, and the combination count takes an addition instead of the
multiply. The class was exercised precisely where it does nothing. It had no
fixture; its 94% came from the simplex server driving it with one quantity.

The test for it is a bijection walk rather than a few expected tuples: an
off-by-one shifting every tuple equally would satisfy any hand-computed example
shifted with it, and would still visit one combination twice and another never.

## An off-by-one in an uncalled method, and why it was not fixed

`TTwoDimFloatDecision.CopyBlock` copies a rectangle of a genetic representation
with a wrap-around offset. Forward and backward are separate expressions.

Forward, for an index past the end, it is `Index - N * (Index div N)` - a correct
modular wrap, and a shift of one rotates the genes. Backward, for a negative
index, it is `(N - 1) + (Index + N * (Abs(Index) div N))`, which for `Index = -1`
gives `N - 2` where a rotation needs `N - 1`. **It is off by one.** On a two-gene
decision a backward shift of one sends both rows to gene 0, the second
overwriting the first, and leaves gene 1 untouched: data lost rather than
rotated. On a genetic search that would duplicate one gene and drop another.

**It was not fixed, and that is the judgement worth recording.** `CopyBlock` has
no caller - nothing in either repository invokes it outside its fixture - and the
genetic representation it belongs to is not what the shipped fits use. Rewriting
arithmetic in an uncalled method, on my reading of what it ought to compute rather
than on a failure anyone has seen, is a guess dressed as a fix. So the asymmetry
is asserted as an asymmetry, with the forward rotation beside it, and a future
caller meets a test rather than a surprise.

I had written the test expecting a rotation, and it failed. The code was not
obviously wrong and neither was I - what was wrong was asserting an intention.

## The application side is complete; the reporter is not, and that is deliberate

Where the search finally ended, so that the next reader can start from the answer
rather than re-derive it.

**Every counted Pascal root is scanned.** The framework, the private pack, and the
optimiser package - the last of which was invisible for eleven rounds because of a
mis-rooted invocation, recorded above. Three scan shapes, each blind where another
sees, agree that nothing of substance is left to extract.

**The Python sidecar is gated at 100 %.** `Invoke-TestPy` runs `coverage report -m
--fail-under=100` over `lineshapes, fitting, fit_backend, routes`, plus the pack's
four when it is checked out. There is nothing to find there, and the gate is why.

**What is left untested is the reporter itself, and it was decided to leave it.**
`cgcov.py` is 329 lines and produces every number this whole programme has been
steered by, and no test executes it: the script suite asserts it by reading its
source text. `compare.py`, by contrast, IS run against fixtures - which is why the
ratchet's behaviour can be trusted and the report's rests on inspection. Only
`executable_lines` needs objdump; `classify`, `read_patterns`, `locate_sources`,
`executed_lines` and `basename_key` are ordinary Python.

The part that would matter most is `classify`, because it *is* the denominator: its
ordering rule - tests beats everything, vendor beats wrapper - and its two
membership tests decide what counts as this project's logic.

The decision was that build tooling is not the shipped program's logic and so not
in the denominator this effort is about. Recorded rather than acted on, which is
the point of writing it here.

## Two flaws in the reporter, checked rather than assumed

**`locate_sources`' docstring overstates what it does.** It says a basename found
in more than one repository is "reported as ambiguous rather than assigned to one
of them". It IS assigned - to the first root walked - and counted there, carrying a
`*(name also exists in another repo)*` footnote on its row.

Checked rather than dramatised: exactly two basenames collide across the counted
roots, `app_modules.pas` and `module_tests.pas`, and they are the two the docstring
itself names as deliberate - the module override mechanism, where a unit earlier on
the search path replaces the framework's. Because only ONE copy is ever linked into
a given binary, the DWARF yields one set of executable lines and the callgrind
output one set of executed ones: **the numbers are unaffected.** What is wrong is
the label - in the pro binary the linked unit is the pack's and the row is titled
with the framework's path - on a three-line unit, with a footnote saying why. A
real discrepancy, a small consequence, and worth knowing before someone reads that
footnote as a warning about the totals.

**And the bucket that hid a repository for eleven rounds.** A source whose basename
is found under no root is listed under "Not attributed to any repository - usually
RTL or LCL code compiled with line info". Omit a root and an entire repository
lands there: a summary line per unit, no per-line data, no warning, under a heading
that invites the reader to dismiss the whole section. The wording is accurate for
its intended case and actively misleading for the other one.

If that section is ever made to say something when a large body of code cannot be
placed, this is the reason.

## What the integration half actually adds, measured rather than assumed

`coverage.ps1` measures `--suite=unit` only, and its comment gives the reason: an
integration test "reaches almost nothing a unit test does not, while costing
nearly all of the runtime". The first half of that had never been checked. It has
now been, by measuring the integration half under callgrind as well and giving
`cgcov.py` both outputs - it unions executed sets across every callgrind file it
is handed, so no new tooling was needed.

| | covered | of | |
|---|---:|---:|---|
| unit suite alone | 14086 | 15940 | **88.37 %** |
| integration suite alone | 9071 | 15940 | **56.91 %** |
| **union** | **14644** | **15940** | **91.87 %** |

**The integration half adds 558 lines across 25 units** - three and a half points,
not "almost nothing". And it adds them exactly where every previous round of this
effort said the remaining gap was:

| unit | unit only | union |
|---|---:|---:|
| `Server/fit_task.pas` | 681/884 | **805**/884 |
| `Server/fit_service.pas` | 1183/1609 | **1290**/1609 |
| `Worker/python_sidecar.pas` | 55/112 | **93**/112 |
| `Common/log.pas` | 87/122 | **113**/122 |
| `Server/server_fit_backend.pas` | 31/49 | **47**/49 |
| `Desktop/DataLoaders/data_loader.pas` | 29/54 | **46**/54 |

So the standing claim - that the remainder is the optimiser, the sidecar process,
the sockets and the filesystem, each exercised by the half the measurement does
not see - is true, and this is the number for it.

**THE COST IS THE OTHER HALF OF THE COMMENT, AND IT WAS RIGHT.** 1.53 trillion
simulated instructions against the unit half's 145 billion: 99 minutes against
about 18. A ratchet gating on the union would turn a twenty-minute measurement
into a two-hour one, which is why the split exists and should stay.

**1296 lines remain outside the union**, and they are three different things.
Some is merely unprovisioned - the coverage image has no scipy and the path builds
no worker, so five sidecar tests skip and four `THttpFitServiceTest` cases error;
installing those would raise the union with no new tests. Some needs a message
loop, which no measurement of a test binary reaches: the client's worker thread and
its `Synchronize`. And the wrapper units stay out by the denominator rule whatever
runs.

**A correction to something I had asserted twice.** I claimed `--trace-children`
would be needed to reach the spawned worker's code. It would not: every unit the
worker executes is already in the denominator and already driven in-process by the
unit half, and the only thing unique to the child is `Worker/fit_server.lpr`,
which is not linked into the test binary and so is not counted at all. The right
lever was provisioning, not child instrumentation.

**And the check I skipped.** I launched the 99-minute measurement without first
running the integration suite in that container at all - a thirty-second check
that would have told me it works there (162 tests, ~40 s, four errors, five clean
skips) and that my runtime estimate was drawn from the wrong native baseline: 72
tests and 28 s belong to the framework binary, not the pro one. Measure the cheap
thing before you buy the expensive one.

**AND THEN IT WAS PROVISIONED, AND THE PREDICTION ABOVE WAS RIGHT IN DIRECTION AND
WRONG IN SIZE.** The image now installs the sidecar's pinned wheels and the
measurement path builds the module's compute server and names it through
`FIT_SERVER`; the integration half runs 162 tests with 0 errors and nothing
skipped, where it had 4 errors and 5 skips. The union moved from **91.87 % to
92.05 %** — 29 lines, +0.18 points.

I had written that installing those "would raise the union". It did, and I would
have guessed at several times that. Nine broken tests bought 29 lines because the
units they touch were already reached by other tests: `python_sidecar.pas` had
already gone 55 -> 93 on the integration half alone and provisioning took it to 95;
`fit_service.pas` gained 6. **A repaired test raises coverage only by whatever it
alone reaches**, which is usually far less than its subject's line count — the
same arithmetic that makes a new test on a covered unit worth little, seen from the
other end. The reason to fix those nine tests is that they were not testing
anything, which is a better reason than the number.

**What the 1267 lines still outside the union are** — unchanged in kind from the
paragraph above, less the provisioning slice: code needing a message loop, and the
paths the doubles stand in for. Nothing left in that remainder is reachable by
provisioning; the next line moved there has to be moved by a test.

## A rule that described a program which did not exist

Extracting the picking captions turned up a disagreement nothing could have
noticed. `action_state.ModeAfterPicking` decides what choosing a picking entry
does, and it said: only the three *manual* entries toggle, so choosing any other
one twice enters it twice. Its test said so too, in as many words — "choosing a
non-toggling entry twice enters it twice rather than leaving".

The window did the opposite. `ActionSelectIntervalBoundsExecute`,
`ActionSelectCharacteristicPointsExecute` and `ActionSelectCurveBoundsExecute`
each wrote out `if SelectionMode <> ModeSelectX then enter else ModeSelectNothing`
by hand — a toggle. And that is the behaviour the user sees, because those
entries are ticked menu items and a tick that cannot be un-ticked by the same
click is a mode with no way out.

**Nothing failed, and nothing could have.** Those three handlers never called the
rule; they carried their own copy. So the rule was free to describe a different
program, its test was free to pin that description, and both were green. A test
over an extracted rule proves the rule self-consistent; it says nothing about
whether anyone asks it. The four copies are now one call, the rule follows the
window, and its test carries the correction and the reason.

**The general shape of this, worth watching for:** an extraction that leaves the
original copy in place has not reduced anything — it has added a second opinion,
with a test attached to the one nobody uses.

## An unreachable branch that reads as a feature

`TFormMain.GridDataEditingDone` exits at the top when the sender is
`GridBackground` — manual entry of background points is not implemented, and a
comment says so. Further down, after the edit has been read, it asks:

    if Sender = GridData then ReplacePointInProfile(...)
    else if Sender = GridBackground then ReplacePointInBackground(...)

The second branch cannot run. Left as it is, with this entry rather than deleted:
it is the only statement of where that feature would attach, and removing it would
make the unimplemented half harder to find than it is now. It is a stub, not a
path — do not read the file as evidence that editing background points works.

## What the UI-wrapper debt actually contains, counted rather than assumed

3216 lines were excluded as UI wrappers when Phase 5 began, and it would be easy
to read that as 3216 lines of hidden logic. It is not. Counting decisions —
`if`/`case`/`while`, with comments stripped — over every unit in
`tools/coverage/wrappers.txt`:

| | decisions |
|---|---:|
| `Desktop/Forms/form_main.pas` | 222 |
| `Desktop/fit_viewer.pas` | 41 |
| `Desktop/ui_dpi.pas` | 14 |
| `Desktop/curve_list_grid.pas` | 11 |
| everything else, eight units | 15 |

Then, in the largest one, dropping `Assigned` guards, `csDestroying` guards and
conditions that are calls into rules already extracted and tested leaves **143 in
82 methods, about 1.8 each**. The methods a decision-density ranking puts at the
top — `CheckState`, `AimPickAtActiveSerie`, `UpdateFitAdvice`,
`BuildRightPanelTabs` — turn out on reading to be already delegating, or to be
genuine widget construction that should stay.

So the remaining value in this phase was concentrated, thin and worth finding one
cluster at a time; the line count was never the measure of it. Two consequences
worth keeping in mind:

  * **A wrapper's line count is not its debt.** Extracting `FormCloseQuery`'s
    rules moved the whole conversation about unsaved work into a tested unit and
    took **2 lines** off the wrapper, because two nested helpers took their place.
    The gain was testability, not size.

  * **Decision density points at the wrong methods** once a codebase has been
    through a few rounds of this: it ranks by what a method *reads*, and a method
    full of calls to tested rules reads exactly like a method full of untested
    ones.

## An intermittent 400 in the module suite, and how it was told apart from my own work

While finishing the wrapper extractions, one run of the private module's
end-to-end suite failed: a decompose request that must always succeed on real data
came back **400 instead of 200**. The obvious suspicion was the work in flight, so
it was checked rather than assumed — the whole change set was stashed, the suite
ran green on the commit beneath it, the change set was restored, and the suite ran
green twice on exactly the tree that had failed. Earlier in the same session
another run of that suite had reported one error and the immediate re-run none.

So it is **intermittent and not caused by those extractions**, and it is worth
recording rather than shrugging at, because a 400 on that route is not noise: the
REST layer maps `EUserException` to 400 and everything else to 500, so a 400 means
the engine DELIBERATELY DECLINED. The likeliest candidate is a busy-state refusal
— "a fit while another is running" — from a server shared by every test in the
suite, where an asynchronous fit started by an earlier test can still be running.
That would make the test order-dependent by construction, and green most of the
time.

**The method is the point.** A suite that fails once in three runs will be blamed
on whatever was being written at the time. Stash, run, restore, run twice is
cheap — four minutes here — and it is the difference between a real finding and
a plausible story.

## A verb whose name describes the one state it refuses

`POST /problems/{id}/actions/minimize-difference-again` — "continue fitting from
where the last fit stopped" — had never been called by any test. Calling it turned
up two things.

**It is refused after a fit.** The engine requires `ReadyForFit`; a finished fit
leaves `Finished`, so the verb answers 400 in exactly the situation its name
describes. Before any fit, with a model that is ready, it is accepted.

**And no menu item sends it.** The desktop's command set has one fit verb, and
continuing is what plain `minimize-difference` already does: it resumes from the
parameters in the model rather than restarting, which is what
`ARefitResumesFromTheFittedParameters` pins. So the "again" verb is reachable only
over REST, does the same thing as the plain one where it is accepted, and refuses
where a caller would reach for it.

Left as it is and pinned as it is: the test now asserts both the acceptance and
the refusal, so the contract is written down rather than inferred from the name.
Changing it is a decision about the API, not a bug fix — and anything driving
this server over REST needs to know that the two verbs are not interchangeable.

**One test of mine had to be rewritten twice before this was clear**, which is
worth its own line. The first version drove the automatic sequence over the
hundred-point profile the other tests use; that verb works out the curve positions
itself, seeds roughly one curve per point, and the suite stopped finishing — seven
minutes and counting where the whole class takes seven seconds. Eleven points with
one peak exercise the same path. **A test that hangs is not a slow test, it is a
suite nobody runs**, and an automatic verb over a realistic profile is exactly
where that happens.

## The line table attributes lines to code that is not theirs

Chasing the last of the uncovered lines turned up a limit in the MEASUREMENT, not
in the tests, and it is worth knowing before anyone plans to reach 100.0%.

`Desktop/DataLoaders/data_loader.pas`, `TDataLoader.Reload`:

| line | | reported |
|---|---|---|
| 149 | `procedure TDataLoader.Reload;` | uncovered |
| 150 | `begin` | **covered** |
| 153 | `Assert(FFileName <> '');` | **covered** |
| 154 | `Assert(Assigned(FPointsSet));` | uncovered |
| 156-161 | the body | uncovered |
| 162 | `end;` | **covered** |

The method is never called by any test — and three of its lines are reported as
run. Addresses are shared between adjacent lines, and between functions after the
optimiser has finished with them, so a line-table entry can carry no instruction
of its own or share one with code elsewhere. Counted over the whole remainder,
**105 of the uncovered lines are procedure or function HEADER lines**, most of
them in methods whose bodies do run.

Two consequences:

  * **100.0% line coverage is not a reachable target**, and a run that reported it
    would more likely mean the attribution had drifted than that every line was
    exercised. The number is a floor on what is exercised, not a census of it.

  * **Judge by the method, not by the line.** "Which methods has no test ever
    entered" is unambiguous and answerable; at the time of writing it is **three**
    (`TFitClient.Reload`, `TServerCallThread.Create`,
    and one accessor pair since covered), holding 46 lines. Everything else
    uncovered is a BRANCH inside a method the tests already enter, a syscall
    deliberately left behind a double, or noise of the kind above.

## Starting the client before the compute server killed it, and had for a long time

The first thing the VM validation found, and it is not a regression — that was
checked rather than assumed.

**What happens.** `FormCreate` builds the action states, and one of them asks the
compute server whether background variation is enabled. With no server listening
that raises out of `FormCreate`, so the form never finishes being created. The
widget set then calls `OnDestroy` on it anyway, `FormDestroy` calls
`WriteSettings`, and `WriteSettings` dereferences `FSettings` — which that
aborted `FormCreate` never assigned. An access violation, on the way out, routed
through `client_log.EndProcessAfterFault`: the process ends, deliberately, because
a memory fault cannot be reported through the widget set that raised it.

So a user who launched the app before the server saw it die instead of open, with
the real cause (no server) buried above the fault in the log.

**Proved pre-existing, not introduced.** The revision from before this phase was
built on the VM and run with no server: identical fault, `WRITESETTINGS` line 4995
there against 4911 now, same `FORMDESTROY` caller. Worth the ten minutes — a
crash found immediately after a large refactoring is assumed to belong to it.

**Fixed at the shallow end only.** `WriteSettings` now returns at once when there
are no settings to write: a form that never finished being created has nothing to
persist. Verified on the VM — no server, no access violation, clean exit, and the
window now stays up, opens the file it was given, and reports the unreachable
server as the message it always meant to be.

**The deeper half is left as it stands and stated here instead.** `FormCreate`
reaching over the network at all is what makes the failure possible; an action's
`Update` handler is a poor place to ask a server anything, and it runs before there
is a window to report through. That is a design change rather than a guard, and it
wants its own commit.

## What the VM validation could and could not exercise

Recorded because "verified on the VM" should say what that means.

**Could:** the build on Linux, both suites there (2151 framework, 2614 pro, all
green), the `/CHECK_LAYOUT` self-check — every form constructed, one clipped
caption reported, no faults — and a real launch with `/INFILE`, which opened the
data file, pushed the profile to the server, refreshed the computed data, cleared
the summary table through its no-intervals branch, and repainted the chart. That
last line is the one that matters for this phase: the chart repaint means the
extracted series styles were applied by the real viewer to a real chart.

**Could not:** anything requiring a click. The VM has no `xdotool` and the session
is Wayland with the app on XWayland, so there is no scripted way to open a menu,
pick a curve type, cancel a dialog, or close a window with an edited table. Those
paths — the ones the extractions changed most — still rest on their unit tests
and on a person at the screen.

**A near miss worth noting:** driving a fit over REST against the problem the
client had open did not reach the client. Its state poll was not running in that
session, so nothing came back to redraw. Whether the poll is meant to be off until
the user acts was not chased down; it is noted here rather than left as a
half-formed suspicion.

## A second SubtractBackground that nothing called

`TFitService` carried two methods of that name: `SubtractBackground(Auto: boolean)`,
which the automatic sequence calls and the REST layer exposes, and a parameterless
`SubtractBackground`, which nothing anywhere called — not the engine, not the
desktop, not the pack, not a test. Its own declaration said so in a way that had
stopped being read:

    { Linearly subtracts background in the SelectProfileInterval and recreates
      SelectProfileInterval. TODO: unify with SubtractBackground. }

**It was not a duplicate, which is what made it worth removing rather than
ignoring.** It subtracted over the WHOLE set from first point to last; the live
one works interval by interval, over the peaks it found. So the two would have
given different data, and the dead one was protected — reachable only from
inside the class, where the next person to want "subtract the background" would
have found it first and by name.

Found while looking for something to cover: it sat in the uncovered list, and the
only test one could write for it would prove that an unreachable method still
compiles. Removed instead. `SubtractBackgroundLinearly`, which it wrapped, stays
— the live path uses it.

## Code only the tests call is dead code, and two pieces of it were mine

A scan for routines nothing references turned up a second, sharper question:
which routines are referenced ONLY BY TESTS? A test is not a caller. Production
code whose only mentions are in a fixture is dead in the way that matters — it
does nothing for the user, and its test reports success either way.

**REMOVED, referenced by nothing at all (116 lines):**

| what | why it was dead |
|---|---|
| `TFormMain.ButAddSelectedDataPointToPositionsClick` and two siblings | handlers for buttons no form file has any more |
| `TFormMain.TAChart1Zoom` | a chart zoom handler nothing binds |
| `TFormMain.Chart1MouseMove` | its body was entirely commented out |
| `TFormMain.SaveTable`, `TFormMain.LoadTable` | empty bodies, declared with comments describing XML they never wrote |
| `OFNHookProc`, `OFNHookProcOldStyle` | commented-out Win32 dialog hooks, orphaned when the block that called them went |
| `TFitTask.SubbCurveFromProfile` | its only mention was a commented-out call |

**AND TWO RULES I HAD EXTRACTED THIS SESSION AND THEN NOT CALLED.** Both were
found by the test-only half of the scan, and they are the exact trap recorded
above under "a rule that described a program which did not exist" — an extraction
that leaves the original in place has not reduced anything, it has added a second
opinion with a test attached to the one nobody uses. I wrote that entry and then
did it twice.

  * `series_style.MarkersToggleApplies` stated the rule for which series the
    "View markers" toggle may touch — while `TFitViewer.ViewAllMarkers` went on
    asking the chart's two flags itself. FIXED BY CALLING IT: the function now
    takes the two flags, which is what the caller has, and the view asks instead
    of restating. The rule is load-bearing now rather than decorative.

  * `action_state.IsManualPickingMode` answered which picking modes toggle. It
    lost its last production caller when ModeAfterPicking's special case went,
    and worse, what it claimed was already false: it answered that the
    interval-bounds and characteristic-point entries do NOT toggle, while the
    window has always toggled them by hand in each handler. REMOVED, with the two
    tests that pinned the wrong answer.

**KEPT AND REPORTED INSTEAD OF DELETED: `TCurveListGrid.Release`.** Nothing calls
it — but it is the only code that saves the grid's column widths, scroll position
and selection back into the curve list, and it marks those settings saved. That is
not dead weight, it is a DISCONNECTED FEATURE: the counterpart of `Assign`, which
is called. Deleting it would delete the only implementation of "remember the
table's layout"; wiring it up is a behaviour decision, so it is stated here rather
than made silently.

**The method, worth reusing:** for every implemented routine, count mentions
outside its own declaration and implementation, split by whether the mentioning
file is a test. Skip anything a `.lfm` names, since a form file binds handlers by
name; skip overloads, where the counting cannot tell which one was meant. Then
read every candidate before deleting it — of the first thirty, one was a
feature.

## A re-declared property is not a hook: the curves stopped being drawn

**The symptom.** After every fit the chart drew the profile, the model sum, the
residual and all three pick sets — and none of the model's individual curves. The
"Curve Attributes" and "Summary" tabs vanished with them. The client log recorded
the fit, recorded the refresh, recorded both `GET /curves`, and then nothing. No
error anywhere. Every test was green, including one asserting that a refresh
plots the curves.

**Two defects, and the second is why the first took a session to find.**

**1. THE RESIZING HUNG OFF A RE-DECLARED PROPERTY.** `TNumericGrid` keeps one
option per column and `TColorStringGrid` one colour per cell, and both arrays
have to follow the grid's shape. The grid's own `SetColCount` is private in the
ancestor and cannot be overridden, so `TClipboardGrid` RE-DECLARED `ColCount` and
`RowCount` with virtual setters and the two descendants resized their arrays
there.

A property re-declaration is resolved by the REFERENCE's compile-time type. So
that worked only for a caller holding the object as one of those classes.
`Desktop/curve_list_grid.pas` takes a `TStringGrid` — it exists to keep the LCL
off the model's dependency path, so that parameter type is the point of it — and
through it the ancestor property was written, the virtual setters never ran, and
the arrays kept the length they had. The grid's OWN internals go the same way,
which is worse: pasting a column in would have missed them too.

`SetColOptions` then wrote one option per parameter name into an array still
sized for the previous, narrower table, and `ENumericGrid: 'Invalid option
index'` came out of a line nowhere near the assignment that caused it. It needed
a fit to appear at all: with no curves that loop returns early, so loading a file
was always fine.

It broke when the grid concerns were extracted into `curve_list_grid.pas`. The
old `TTableCompList.GridAssign` had assigned the two counts inside
`with Grid as TColorStringGrid do` — a cast that looks like noise, is not
commented, and was the only thing making the hook fire. The extraction dropped
it.

**FIXED PROPERLY, in the library:** both arrays now follow from
`TCustomGrid.SizeChanged`, which is virtual in the grid itself and fires from the
one place the counts change, whoever changed them. The re-declared properties and
their four accessors are gone, so there is nowhere left to hang work that a
caller can miss. The consumer needs no cast and does not have to know.

**The rule:** a re-declared property is a NEW property that shadows the old one
for some callers and not others. It can carry a default, and nothing else. If
behaviour has to happen, find the virtual method the base class calls — there
usually is one, and here there had been one all along.

**2. AN EXCEPTION IN THE POST-FIT REFRESH WAS SWALLOWED WHOLE.**
`TFitClient.Done` runs inside `Synchronize`, so an exception escaping it is
re-raised in the WORKER thread — after the handler that logs one has already run
— and dies there with the thread. Half-refreshed window, empty log, and a
drawing defect with no error behind it. `TServerCallThread.Finished` now logs it
and reports it through `OnCalcError`. That change is what made the diagnosis
possible: with it in place the cause appeared in the log on the first run.

**Why the green test proved nothing.** `testcase_fit_client_view` stubs `/curves`
with an EMPTY array, so no test had ever refreshed the client with real curves.
The client half was in fact sound — established with a throwaway probe against
the live server's recorded reply — and the failure was entirely below it, in code
no test in this project can enter.

**And it cannot be covered by a unit test.** Not "was not" — cannot. An LCL grid
cannot be CONSTRUCTED without a widget set: `TNumericGrid.Create(nil)` raises
"Canvas does not allow drawing" under nogui, because the grid sizes itself in its
constructor and a default row height is worked out from the height of a line of
text. Eight tests were written against a real grid and every one of them failed
there, which is how that is known rather than assumed.
`tools/build-tests/grid_sizing.tests.ps1` guards the shape of the source instead:
that the resizing hangs off `SizeChanged` and that the re-declared properties
have not come back. It fails on the pre-fix source on three separate assertions.

## Finishing the Assert conversion, and the two things that made it stick

349 live `Assert` call sites, every one of them in `Desktop/`, replaced with
`Common/checks.pas` calls. That number is worth recording because the plan was
written against **364**, counted by `grep`: fifteen of those matches were inside
comments. The scanner that replaced the grep strips comments *by replacing their
bodies with the same number of newlines*, which is also what keeps its reported
line numbers real - collapsing a multi-line comment to a space shifts every line
after it, and this file already records a hand-run scan that reported findings
against the wrong lines for exactly that reason.

**Two build-flag facts the conversion turned up.**

`IncludeAssertionCode` was `True` in all nine project configurations and `-Sa` on
both raw `fpc` lines, so assertions had been on in *every* build - debug and
release behaved alike by configuration rather than by construction. All eleven
are now removed. What that buys is not tidiness: a stray future `Assert` is inert
in both builds instead of load-bearing in one, and the failure mode inverts from
"silently absent for users" to "visibly wrong everywhere".

**249 stale `.ppu` files in `tests/` were being reused, and I did not notice for
an hour.** `tests/build.sh` writes its units with `-FEtests`, so they accumulate
in the source tree; a plain `fpc` run then finds them on the unit path and skips
recompiling. My first "the light suite compiles clean" reading came from a build
that compiled **42** units instead of 250. `coverage.ps1` already deletes them
for the same reason, with a comment explaining that a stale `.ppu` carries no
DWARF line table and so silently shrinks the coverage denominator. The lesson is
narrower than "clean before building": **a build that compiles far fewer units
than the project has is not a fast build, it is a build that did not happen.**
Count them.

**`CheckAssigned` takes `TObject`, and the codebase is `-SIcorba`.** So the 48
`Assert(Assigned(FitService))` in `fit_client.pas` would not convert: corba
interfaces have no `IUnknown`, hence no common ancestor to declare a parameter
as. Those became `CheckThat(Assigned(X), '<sentence>')`, which is what `Server/`
already did 18 times. Worth knowing before designing an overload: there is no
type that accepts an arbitrary corba interface, so the fallback is the API, not a
gap in it. The compiler finds every one of these for you - it is a type error,
not a silent difference.

**The descriptions are the deliverable, and they are now machine-checked.**
`tools/build-tests/no_assert.tests.ps1` fails on a description carrying a
unit-name prefix, an operator, `nil`/`Assigned`, a bare identifier, or a single
word. Writing it found 71 offenders in the *already converted* engine -
`'fit_service: StartIndex <> -1'`, `CheckAssigned(P, 'P')` - which the first pass
had recorded as debt and which would otherwise have stayed. Three notes from
building it:

- the check must run over the **whole file**, not line by line. A call wrapped
  onto two lines was invisible to a line-based scan, which meant the longest
  descriptions were precisely the ones it could not check;
- the description is the **last** string literal in the call, not the first.
  `CheckThat(FFileName <> '', 'only a data set that came from a file...')` has a
  literal in its *condition*, and taking the first one reported an empty
  description and the wrong offence;
- `tests/` is exempt, because `testcase_checks.pas` passes deliberately silly
  descriptions to the helpers - that is its subject.

**`Server/curve_list.pas` is not UTF-8.** Decoding it strictly throws. Every edit
here went through `latin-1`, which round-trips arbitrary bytes, so files whose
encoding is neither known nor uniform come back byte-identical apart from the
edit. This file already warns that `grep` treats `Packages/TAGraph` as binary for
the same reason; the set of affected files is larger than that.

### Three handlers for an exception nothing raises, and what they cost

The six dead `EAssertionFailed` handlers in `fit_server_proxy.pas` this file
already recorded are gone, along with a seventh in `TFitTask.SetSpecialCurve` and
the doctrine comments in `fit_service.pas` that still told the reader to raise
`EAssertionFailed` for an inadmissible state, `TODO` included.

`SetSpecialCurve` is the one that mattered. Its handler logged a `Warning` and
carried on, under a comment calling the failure non-fatal - and could not fire,
so the real behaviour has been to raise ever since the conversion. Both readings
cannot be right, and the code's own stated one is the wrong one: storing an empty
expression leaves a special curve that cannot be evaluated, so "warn and
continue" is precisely the silent degradation the policy refuses. The raise
stays; the comment now says so; `tests/testcase_task_preconditions.pas` holds it.

**Both new tests assert which exception class arrives, not merely that one does.**
That is the whole content of the defect: a refusal reworded as `EUserException`
tells the user they did something wrong when the program is wrong about itself,
and a test that catches `Exception` cannot tell the two apart. This is also why
narrowing an `except` clause is so easy to get wrong - it has no compiler
consequence and no test consequence either, unless something drives the failing
path.

## The reference check, scripted - and what four passes of it had left

`tools/find-dead-code.py` is the check this file records being run by hand at
least six times. Writing it down changed two things: the unit-level half found
641 lines nothing had noticed, and the member-level half found almost nothing -
which is the more useful result, because it says the earlier passes worked.

### A four-unit island in the engine, 641 lines

`fit_server_app` -> `fit_service_multithreaded` -> `fit_service_with_thread` and
`fit_task_with_thread`: `TFitServerApp`, `TFitServiceMultithreaded`,
`TFitServiceWithThread` and `TFitTaskWithThread`. Nothing outside the island
referenced any of them, no project file listed them, and no build compiled them.

They were already half-cleaned: the entry above about a server holding a client
of itself names four of them as carrying the same dead `uses app` clause. **The
clause was removed and the units were left**, which is the specific shape to
watch for - a cleanup that fixes what it was looking at and leaves the thing it
was looking at.

What settled the keep-test was not their own header, which still claims a purpose
("server component performing long-term operation in separate thread"), but
AGENTS.md non-negotiable #1: **the desktop client contains no fitting engine.**
These implement the in-process threaded design that rule forbids. A declaration
whose purpose the architecture has since ruled out is not a reserved home for
future work.

`csv_file_loader.pas` is unreached by the same scan and stays, on the test
recorded earlier: two other units document it as the reserved home for CSV
loading, so it is a declared intention with content behind it.

### The diagram guard earned its keep for the third time

Both a generated figure and `architecture.md` named `TFitServiceWithThread` and
`TFitTaskWithThread` - and the figure's note existed to explain that the REST
session deliberately owns a *plain* `TFitService` instead of them. That is the
orphan pattern this file already describes: the unit, the project entries and the
place in a diagram all survive, and each surviving reference is a reason the code
looks alive. The note now says what stays true - progress reaches the client by
polling because a headless server never pumps `Synchronize` - without naming
classes that are gone.

### Two more pre-existing breakages, both found by building everything

The default build task builds two of the eleven Lazarus projects. Building all
eleven found:

- **A demo project in the private domain repo has not compiled since 8d8656f**,
  which renamed one of its list units and replaced that unit's setter with
  module state. Its `.lpi` also carried seven search paths copied from a project
  one directory up and never re-based, which is why the failure surfaced as a
  missing unit rather than a missing identifier.
- **`fit_tests_example.lpi` was missing `../../tests/fitminimizers`**, so it could
  only build while stale `.ppu` files happened to be lying in `tests/`. It built
  green for me twice before I cleared them.

**249 stale `.ppu` files in `tests/` were being reused for the first hour of this
work.** `tests/build.sh` writes units with `-FEtests`, so they accumulate in the
source tree and a plain `fpc` run finds them on the unit path. My first "the
suite compiles clean" reading came from a build that compiled 42 units instead of
250. `coverage.ps1` already deletes them, with a comment about the coverage
denominator. The transferable rule is narrower: **a build that compiles far fewer
units than the project has is not a fast build, it is a build that did not
happen.** Count them.

### What the member-level scan found, and four ways it lied first

One deletion in this repository: `TOHLCFileLoader.DateLayout`, a read-only
property nothing reads - not even a test. Its value does real work through the
field; the property was a public accessor with no consumer and no stated one,
unlike `SkippedRows` beside it, whose comment names why a caller would want it.

Three findings in `fitminimizers`, recorded rather than acted on because each is
an algorithm decision: `SimplexStartStepMultiplierEnabled` and
`SimplexStartStepRandomEnabled` are properties nothing writes, and both are set
`False` in the constructor - so two restart heuristics are wired, reachable in
code and permanently off. `ExitDerivative` is write-only and never written, so
the value handed to the algorithm is always the default. Deleting the properties
would remove the only way to turn those behaviours on, which is the
`IsManualPickingMode` case: **connect, or leave and record - do not delete.**

The four wrong answers the scan gave before it gave that one are the reason its
header is as long as it is:

1. **Excluding `Object.Method`.** The first version skipped an identifier
   preceded by `.`, to avoid crediting `Foo.Bar` to some other `Bar` - and
   reported **602** candidates, because in Pascal nearly every call is a member
   access. Over-counting is the safe direction for a deletion list.
2. **Comparing use against a flat count.** `own <= 2` was meant to be
   "declaration plus implementation header", and for a free routine it is
   "declaration plus one real use" - so `@ActStop` at `fit_rest_api.pas:618`,
   the registration that makes the REST verb work, read as no use at all. Twenty
   live route handlers and twenty-three live expression functions were reported
   dead. Count the declaration sites per name.
3. **Ignoring the interface/implementation split.** A routine declared only below
   `implementation` is unit-local *by construction*, so every reference from
   another file is a collision. Without that, `checks.pas`'s private `Fail` was
   credited with seventeen references, all of them FPCUnit's own `Fail`.
4. **Requiring a direct import for a bare reference.** This looked right - Pascal
   needs the `uses` clause to name an identifier - and made the output worse,
   183 candidates to 195, because a production reference reaching the unit
   through an intermediate stopped counting and its member moved to "tests
   only". Reverted, and the comment says why.

**177 members are referenced only from tests and 727 only inside their own
unit.** Neither number is a worklist yet: the first is dominated by name
collisions the tool deliberately keeps (`Send`, `Take`, `Split`), and the second
is mostly interface methods reached through a vtable. What turns an entry into a
deletion is the keep-test, one at a time - and the rule that decides it is now in
AGENTS.md rather than only here.

## I said no Assert remained, and 43 did - in the package the engine calls

Both of my checks were scoped to `fit` and the private domain repo, and both
reported zero
while `fitminimizers` held **43**. The scanner had a hardcoded list of directory
names - `Common`, `Desktop`, `Server`, `Worker`, `tests` - none of which exist in
that repository, so it walked nothing and printed `TOTAL 0`. The Pester gate
walked `$RepoRoot` and stopped at the repository boundary.

**Fifteen of them were in `SimpMath.pas`'s lineshape functions** -
`Assert(A >= 0)`, `Assert(Sigma >= 0)`, `Assert((Eta >= 0) and (Eta <= 1))` -
which `Server/fit_task.pas` calls on every evaluated point of every fit. Those
guards have been compiled out of every release build the project has ever
shipped, which is the exact defect the whole conversion existed to remove. The
unit is in the coverage baseline at 97.37%, so it was measured, built and
exercised the entire time; nothing about it looked unfinished.

Two lessons, and the second is the one worth keeping.

**A scanner that reports zero must prove it looked.** `find-dead-code.py` prints
the file count per repository for this reason and the assert scanner did not;
`no_assert.tests.ps1` had the guard in the right shape already ("has sources to
check, so a pass is not an empty sweep") and it passed, because it was counting
the files in the one tree it knew about. A non-empty sweep of the wrong set is
not a non-empty sweep.

**A rule that stops at a repository boundary is not the rule.** These are the
same author's units linked into the same binaries; the boundary is a packaging
decision, not a scope. Both gates now walk `fit`, the private domain repo,
`fitminimizers` and `fitgrids`, and report paths relative to the directory that holds them so a
hit names its repository. Widening them found four more things immediately:
`{$ASSERTIONS ON}` in four source files and two Delphi `.dpk` package files -
a source directive forcing assertions on in one translation unit regardless of
the build, which no project-file check would ever see - and one more description
restating its own expression, in one of the private domain repo's engine units.

### The package could not use the framework's checks unit

`Common/checks.pas` `uses log`, and `fitminimizers` has its own release cycle and
its own licence and must build with nothing beside it. Reaching into the
application that consumes it would invert that dependency, so the three routines
went into `MyExceptions.pas`, which was already linked into everything there and
already owned the question "which kind of error is this".

**They log through a sink the host fills**, which is the answer after asking why
the package has no logger at all. It has none because it is a library: where
diagnostics go is the host's decision. Copying `log.pas` in would have been wrong
twice - two loggers in one process means two size limits and two rotation
policies, so aimed at one file it is interleaved writes and doubled rotation
dropping lines, and aimed at two it splits a single fault's trace across both,
which is the worst outcome for the only reader who matters - and it would have
relicensed 350 GPL lines into an MPL package while dragging `Windows`/`Shfolder`
into a package whose appeal is that it is plain Pascal.

So `MyExceptions.OnCheckFailed` is a procedure variable, filled from
`Common/checks.pas`'s **initialization section** rather than from each `.lpr`.
Four binaries link it - client, compute server, two test suites,
`dump_registries` - and a line in each program is a line the fifth one forgets;
`checks.pas` is already linked into every binary that makes checks, so the wiring
happens by the act of linking and the failure mode of an injected sink cannot
arise from an oversight.

**`checks.EInternalCheckFailed` had to become an alias**, and noticing that was
the near-miss of this change. Adding `MyExceptions` to `checks.pas`'s uses clause
gave the program two distinct classes with the same name, so
`on E: EInternalCheckFailed` would have caught whichever the uses clause resolved
and silently missed the other - the same shape of defect as the six dead handlers
above, arrived at from the opposite direction. One class: a broken invariant is a
broken invariant whichever side of a package boundary stated it.

**The wiring test was verified in both directions**, which for an injected sink
is the only way it means anything: unwiring the initialization line makes
`APackageCheckReachesThisApplicationsLog` fail, rewiring it makes it pass. The
failure mode being tested for is silent - every check still raises, every other
test still passes, and the only loss is the log line that would have reached a
bug report.

And the stale-`.ppu` trap bit once more while proving it: the first "rewired"
run still failed, because clearing `tests/*.ppu` does not clear the `-FE` output
directory the broken build had just written.

`tests/fitminimizers/testcase_my_exceptions.pas` covers both directions and
asserts the class, not just that something was raised -
`EInternalCheckFailed.InheritsFrom(EUserException)` is checked to be false,
because a test catching plain `Exception` would pass while the distinction rotted
away. That is how six handlers in the consuming application came to catch an
exception nothing raised.

Coverage: the light scope went 8011/8830 = 90.72% to 8034/8855 = 90.73%. The
ratchet caught `MyExceptions.pas` entering the denominator at 9/16 and said so
precisely - "without them the figure would be 90.73%, which did NOT fall" - which
is the message that distinguishes a new measurable unit from logic landing
untested. The tests took it to 14/16 and the drop disappeared.

## The one export this program has was never offered, and could not say what it wrote

`ucSaveModelAsText` had its flag cleared on every state change in
`action_state.CommandStates` and set again nowhere, so **File ▸ Save as Text
File… was unreachable for years**. `TActionStateTest.SavingTheModelAsTextIsNeverOffered`
pinned that as behaviour rather than fixing it, on the grounds that whether the
export still worked was a question for someone who could run the application.

It could not have been fixed in place, because the command was two commands. Its
handler wrote the parameter grid or the datasheet according to which tab was in
front, and the `if`/`else if` had **no `else`** — so on any other tab it silently
did nothing. A command whose label cannot say what it will do is a decision made
from state the user cannot see, and its availability cannot be derived either:
"is there something to export" has two different answers depending on a tab.

It also cleared the table's modified flag, claiming the work had been kept — for
a file nothing can open again.

Two commands now, each naming its table, each following that table's own row
count. The pinned test is gone, replaced by ones that assert each is offered
exactly when its own table has rows. The lesson is the first one: a flag that is
only ever cleared reads exactly like a flag that is conditional, and only a test
that asserts it can ever be **True** tells them apart.

## A contract can be asymmetric for years without anyone noticing

`GET /problems/{id}/curves` had always reported each curve's instance handle, and
there was no way to send one back. Nothing failed: every route worked, every test
passed, and the gap only became visible when something needed to *restore* a
model rather than display one — at which point picks pushed back got fresh
handles, every saved value matched nothing, and the fit silently resumed from its
seeds.

The tell, in hindsight: a value the server **issues** and the client **stores**
has to have a way home, or the client's copy is decoration. Worth asking of any
identifier that crosses outward — `GET` exists, does `PUT`?

Two symmetric writes closed it: `ids` beside `x` and `y` on the picks, and
`PUT /curves` as the write side of the read. Both are the same body shape as the
read they mirror, which is what kept them from becoming a parallel contract.

## A module that declares a resource may still have nothing to say

`TFitService.ModuleGet` **raises** when no session answers — correct for a client
asking for a resource by name, and wrong for the framework collecting across
modules. Collecting a project's module state through it meant that a pack which
declared `project-state` but had nothing placed yet made **saving the project
fail**, in exactly the builds the feature exists for.

The declaration and the answer are two different questions. The declaration says
whether to ask at all (and is what keeps a build whose modules keep nothing from
looking for a resource that does not exist); the answer says whether there is
anything to store, and declining is silence rather than an error — as it is for
every other resource. `GetModuleProjectStates` asks the sessions directly for
that reason.

Found by the test that registered a module declaring the resource beside one that
did not, which is the case a single-module fixture cannot produce.

## Five gaps in one feature, all the same shape: tested in the middle, wired to nothing

The project file was built bottom-up, each layer with its own tests, every layer
green. Five separate things were nevertheless missing, and every one of them had
the identical shape: **a mechanism that was correct and covered, reached by
nothing at one end or both.**

| What was wrong | What the tests were doing |
|---|---|
| `ui.json` was written on every save and read on none, so the axis, the tab and the picking mode never restored — while the user guide said they did | the codec's round-trip tests passed; nothing asked who called the reader |
| the selected interval and the user-defined formula were never captured, so a project saved on one peak reopened across the whole profile and one using a user curve reopened empty | the session tests passed a context *in*, so the assembly of that context was never exercised |
| the selected curve was decided by `PlanUiRestore` and neither captured nor applied | four tests exercised the decision against a hand-made document |
| opening a project drew nothing, because the client's own profile copy was never re-read | every project test drove `IFitService` and then asked `IFitService` |
| saving destroyed parts written by a newer build | `fit_project_json` preserves them and four tests prove it — the capture handed it an empty document to preserve |

**The tell, and it is cheap to check:** for every field a format or a contract
declares, ask *what writes this* and *what reads it*, and require a named answer
for both. Four of the five above are visible in a two-column list of exactly that
— which is how the last of them were found, after the first two were found by
accident.

**Why the tests did not help.** Each layer was tested at its own seam, and the
seams were sound. What nothing tested was the *joins*: a test that hands a record
to the middle of a pipeline proves the middle works and says nothing about
whether either end is attached. `testcase_project_ui_context` is the sharpest
case — its tests of "may this curve be selected again?" were correct, thorough,
and sat above a capture that never filled the field in and below an apply that
ignored the answer.

**What actually caught them** was reading the call sites and asking of each field
"who fills this in?". Not coverage: the assembly lived in `form_main`, which is
excluded from the target by design, so the figure was blind to it — and being
blind there is precisely why logic must not live there.

The general form is the one at the top of this file, and this is its most
concentrated instance to date: **a green suite over a path the user never
takes.** Here it was five paths in one feature.

## The sixth gap: `@` on an overloaded RTL routine defeats the seam it fills

The last project was not reopened at start-up, and every part of the chain was
covered: `Settings_v1.LastProjectFile` round-trips through the XML writer
(`testcase_settings_model`), `PlanStartup` decides the precedence
(`testcase_recent_project`, thirteen tests), `TProjectWorkflow.OpenProjectAt`
opens and applies (`testcase_project_workflow`), and `Fit.lpr` calls all three in
order. Reading it top to bottom, nothing was wrong.

The defect was in the one argument no test had ever supplied:

```pascal
Startup := PlanStartup(ProjectParam, InFileParam,
    FormMain.LastProjectFile, @FileExists);   //  wrong, and it compiles
```

**There is no one-argument `FileExists`.** Both RTL overloads are
`(FileName; FollowLink: Boolean = True)` (`filutilh.inc:178,200`). The default
makes the *call* `FileExists(P)` legal; it does nothing for the *address*.

**And the compiler does catch this — in the mode the rest of the codebase is
checked by.** Compiled into the test suite, which declares `{$mode objfpc}`, the
same two lines are an error:

```
Error: (4001) Incompatible types:
  got      "<address of function(const UnicodeString;Boolean=TRUE):Boolean>"
  expected "<procedure variable type of function(const AnsiString):Boolean>"
```

`Desktop/Fit.lpi` compiles in **Delphi syntax mode**, where `@Routine` yields an
untyped pointer that is assignment-compatible with any procedural variable — so
there the same line compiles in silence. `fit_server.lpr` and `fit_tests.lpr`
both declare `{$mode objfpc}{$H+}` over that project setting. `Fit.lpr` was the
one that did not, and it is the one that got it wrong.

Read the error again for what actually ran: the address taken is the
**UnicodeString** overload, and it was called with an AnsiString. This was not
an intermittent misread — the remembered project could never be found, on any
run. The user saw an application that started empty, and the log said the last
project was no longer there, which reads exactly like a file someone had moved.

**The same trap had already been caught once in this feature**, on
`DefaultSourceNotice`, and was fixed there with an explicit overload. It came
back at the next seam because the seam was filled from `Fit.lpr`, which no test
links.

**The shape is the previous entry's, one layer further out.** A seam exists so
the decision can be tested with a stub; the production wiring of that seam is
then the only line nobody exercises — and here it was also the only line that
could be wrong. The fix is not a better `@`: it is `DefaultPathExists` as a
named function in `recent_project` with the exact signature, so the compiler
checks it and a test can call it. `TStartupOnDiskTest` now runs `PlanStartup`
against files that really are and really are not on disk.

**What to do about it generally:** never pass `@SomeRTLRoutine` as a callback.
Wrap it in a named function whose signature is written out, put the wrapper
beside the seam it fills, and let one integration test call the seam the way the
application does. Two things then hold that `@` gave up: the compiler checks the
signature, and the wiring is reachable.

**And two guards, because "remember not to" is not one.** Both were checked by
putting the original line back and watching each fail, naming
`fit/Desktop/Fit.lpr:263`:

  * `Desktop/Fit.lpr` now declares `{$mode objfpc}{$H+}`, so the compiler makes
    this an error in the client's own wiring as it already does everywhere else;
    and
  * `tools/build-tests/syntax_mode.tests.ps1` holds the rule in both directions
    — every `.lpr` declares the strict mode, and no file *without* it takes the
    address of a routine this codebase does not declare. The second half is
    deliberately not "no `@`": `@Buffer` handed to a Windows API is ordinary,
    and so is the address of a routine declared twenty lines above its use. What
    cannot be checked by eye is a routine whose overload set lives in the RTL.

**Why a lint rule and not just the mode.** The mode fixes the program files; the
scan covers everything the project setting still compiles as Delphi — every unit
under `Desktop/Forms`, among others — where the same hole is open and the
compiler will not say so.

**And then the sequence itself came out of the program file**, which is the
third guard and the only one that is a test of the feature rather than of a
language rule. Reading the window's remembered project, calling the decision and
acting on it are now `Desktop/startup_sequence.pas` behind an `IStartupHost` of
five forwarders; `Fit.lpr` creates the host and makes one call.
`testcase_startup_sequence` drives it in both halves — the unit half stubs the
existence check so every branch is reachable, and the integration half calls
**the production entry point, the one that supplies its own check**, against a
file that is really on disk. That last test is the one the defect could not have
survived: the file is there and the project is still not opened.

The general rule this produced is now in AGENTS.md and `testing.md`: **the red
test enters where the application enters**, and where a seam must be stubbed to
get there, the production argument to that seam needs its own test calling it
the way the application does.

## Four defects in one bug report, and what the log line was worth

A user ran the finished project-file feature and reported three things; a fourth
came out of reading the file they attached. They are worth keeping together
because only one of them was a mistake in the code that was written for the
feature.

**1. "The last project still does not open."** It did. The client's own log —
the Debug line added with the previous fix, which exists precisely so that
"nothing was remembered" and "the auto-open is broken" stop looking alike —
answered it in one line:

```
start-up: /PROJECT="" /INFILE=".../Data/2.dat" last project ".../Sample.fitproj"
/INFILE: opening .../Data/2.dat
```

`-Task run` passed `/INFILE=Data/2.dat` on every launch, from a time when the
application had no documents. `/INFILE` outranks the remembered project **by
design**. So on the only machine the application is ever run on, the feature was
unreachable while behaving exactly as specified, and no test could have caught it
— the launcher is not the product, and the product was right.

The lesson is not about tests. **A default in the tooling can hide a feature as
completely as a bug in it**, and the thing that made it a five-minute diagnosis
instead of a day was one Debug line printing the inputs to a decision.

**2. The project file kept no pack model.** The saved file was honest: an
empty `positions` set beside two fitted curves. A pack's pattern is built from
the pack's own markup, never from a picked position — and the pack declared
three resources, none of them the reserved `project-state`. The framework asks
only modules that declare it, and it may not name one, so the file recorded a
model with no inputs at all and reopening rebuilt nothing.

Both repositories were green. **The gap was in neither half but in the
declaration that joins them** — the same shape as the five gaps above, one
repository further out.

**3. A restore then wrote zeroes over what it had just rebuilt.** A pack
pattern's identity is text (`waveId`, `waveLabel`); the project carries values as
doubles, so each was saved as `0.0` — and a restore writes saved values onto the
rebuilt instances. Fixing (2) would have made this visible for the first time:
the markup would come back and the labels would be destroyed on the next line.
The capture now stores quantities only, and what is not a quantity is regenerated
from the module state that (2) restores.

**4. The positions table was empty while the chart drew the model.** The table
was filled from inside the *picked* positions' plot method, and a pack picks
nothing. The window disagreed with itself about whether a model existed.

**What (2), (3) and (4) have in common** is the tell this file already carries:
for every field, ask what writes it and what reads it. Here the answer for the
pack's markup was "nothing writes it"; for a text parameter, "the file writes 0
and the restore reads it"; for the positions table, "only the set that a pack
never fills". Three joins, no crashes, and a feature that worked in every test.

## A fifth defect, found only by asking whether a restored fit is still a fit

The four above were fixed and covered, and one more test was written to close the
last gap in that batch: save a half-converged pack model to a real `.fitproj`,
reopen it, and assert both that the values come back **and** that the instance is
still known to be *fitted* rather than merely placed.

The values came back. The flag did not — and it turned out to have been gone
before the file was ever written, so the project file had nothing to do with it.

`SyncIdentityToPicks` dropped every identity whose seed is not among the picked
positions, keeping only positionless ones. **A module places its instances from
its own markup: they have a seed and no pick**, and a pack's pick set is empty,
so every one of them was pruned on every rebuild. `AddBuiltCurve` re-adopted the
handle a moment later, so the model looked right and nothing ever crashed. What
was lost each time was everything the registry knew about the instance —
including `Fitted`, which is the only thing distinguishing "an optimiser produced
these values" from "this is where it was placed".

So `IsCurveFitted` was **always false for a module-placed model**: every saved
project recorded `fitted=false` for every pattern, `AnyCurveIsFitted` was false
for a fully fitted model, and the distinction did not exist for a pack at all.

**Three things worth keeping from this one.**

*The rule had no test because of where it lived.* The loop was a private method
of the service, so asking it anything needed an engine, a module and a rebuild.
Moved into the registry as `KeepOnlySeeds`, it takes an array of doubles and
answers in microseconds — five tests, one of them the case that was wrong.

*The move settled a tolerance nobody had noticed.* The loop compared seeds with
`TINY` and then removed them with `SameSeed`'s `SEED_EPSILON` — two spellings of
one question, in fourteen lines.

*And the new test immediately caught a bug in the fix.* `Append` fills a **local**
record field by field, and a local record is not zero-initialised, so the new
`PlacedByModule` carried stack garbage: every entry looked module-placed and
nothing was pruned at all. The one test asserting that a deleted pick takes its
instance with it is what failed. `Append` now starts from
`Default(TCurveIdentity)`.

The general lesson is the one this file keeps arriving at from a new direction:
**a question nothing can ask is a question nobody has answered.** Here it had
been wrong for as long as packs have existed.

## Delete curve, greyed for the third time - and the two halves nobody joined

The entry has now been reported greyed three times, and each report was a
different defect with the same symptom. The first two were in the framework's own
half: one input was gathered *after* the decision that reads it, and a
right-click did not select the row it landed on. Both were fixed, both have
tests, and the entry was still dead over the rows a user of an analysis pack
actually right-clicks - because those rows are not the framework's.

**The window asked whose rows these were.** A pack fills the Model panel for its
own curve types, and the window answered "which curve does the selected row
name?" with `if the framework filled this panel then the row id else nothing`. A
pack identifies its rows by its own markup - a wave guid - so the answer was
always "no curve", and every framework command on one curve was disabled over
every pattern in a wave count. The row now carries the handle *beside* its own
identity (`TOutlineRow.CurveId`), and the question is answered from the row, by
`CurveHandleForRowId`, whoever put it there.

**And the other half would have made the command lie.** An instance a pack
placed is rebuilt from that pack's markup on every model edit, so removing the
curve and its handle - which is all `DeleteCurve` did - would have deleted the
pattern for exactly as long as it took the next rebuild to put it back. Enabling
the entry alone would have shipped a Delete that visibly does nothing, which is
worse than one that is greyed.

So the registry now says what a deletion has to take with it (`RemovalOf`: the
pick, the handle alone, or the markup that placed it), and for the third the
service asks the modules. A module that does not claim the instance gets a
refusal in words rather than a deletion that undoes itself.

**The nested pattern is what caught the last piece**, and only through REST. A
pack removes a subtree - a pattern nested in the deleted one has no leg left to
hang from - so more instances go than the one named, and the framework refreshes
its reported curve list only while something still describes the model. Delete
the *last* root pattern and nothing does: the child went on being drawn, hanging
from nothing. `TryRemoveInstance` therefore names every instance that went, and
the service drops each. The one-pattern case passed throughout, because there the
curve deleted by index happened to be the only one.

**The shape, again:** two repositories, both green, and the defect in the join
between them. What made it visible was writing the test as the user's gesture -
right-click a wave row, choose Delete curve, then ask whether the pattern is
*still gone* - rather than as either half's own contract.

