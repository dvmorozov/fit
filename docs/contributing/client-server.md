<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Client / server: running and developing

Fit is a **client/server** application. The two parts are **independent processes**:

| Binary | Project | What it is |
|--------|---------|------------|
| `Fit` | `Desktop/Fit.lpi` | the desktop client (UI) |
| `fit_server` | `Worker/fit_server.lpi` | the compute server (runs the fitting engine) |

The client is a **thin client**: it contains **no fitting engine at all**. Every server call
goes over **HTTP+JSON** to `fit_server`, which must be running. The client **does not start or
supervise** the server - it is launched separately, and may run **on another machine**.

You can verify the split:

```
strings Fit       | grep -x TFitTask   # nothing - no engine in the client
strings fit_server | grep -x TFitTask  # present - the engine lives here
```

## Pointing the client at a server

**Fit → Compute Server…** takes a URL (default `http://127.0.0.1:8787`; it may equally be
`http://some-host:8787`). The setting is persisted. The client warns immediately if nothing
answers - **without a server it cannot fit anything.**

## The API

The REST surface replaces the retired XML-RPC/WST transport and carries the same `IFitService`
verbs. A problem is a resource, keeping the original stateful `ProblemID` model:

| Route | Purpose |
|-------|---------|
| `GET /health` | liveness + protocol version |
| `POST /problems`, `DELETE /problems/{id}` | open / close a problem |
| `PUT|GET /problems/{id}/profile` (also `background`, `positions`, `rfactor-bounds`) | the point sets |
| `POST|PUT /problems/{id}/points/{set}` | add / move a single point. Moving a picked curve position is allowed: the pick carries the handle its curve is identified by, so a move rekeys and the curve keeps the shape the fit found, re-seeded at the new place. **A move is still refused for a module's own markup** - its points are not one per curve, so moving one re-derives every instance the markup placed and there is no correspondence to carry (`Server/fit_advice.pas`) |
| `GET /problems/{id}/calc-positions` | where the built model's curves actually sit: one point per instance with a position parameter, at its own fitted `x0`. The counterpart of `positions`, which is what the user **picked** - the same input/derived distinction as `profile` and `calc-profile`. Derived and read-only, so unlike the picks it may hold an x off the sample grid and two curves that converged on one value |
| `GET /problems/{id}/module-states` | every module's `project-state` document in one answer, collected server-side over the registry the problem's sessions were made from - so the framework names no module, and a client with a different set of modules linked cannot ask for the wrong ones. Not a bypass of the module route below: that addresses ONE module's resource, this is the framework collecting across modules for the project file, on the same reading as `GET /curves` beside `GET /curves/{cid}/points`. Only modules whose declared resources include it are asked, because `ModuleGet` raises when nothing answers and a module with nothing to keep is not an error; a module that declares it and still declines is silence, so a save cannot fail because a pack had nothing to say |
| `PUT /problems/{id}/curves` | the whole model's fitted values at once - the write side of the `GET` below, and the same body shape. One request rather than one per parameter because each parameter write rebuilds the entire model, and because only a whole-model write can carry `fitted`, which says an OPTIMISER produced the values rather than their being seeds. That cannot be derived: every instance has values from the moment it is placed. An unknown handle is `404` and fails the whole request - a restore that silently skipped one curve would draw a model with a peak missing and say nothing |
| `GET|PUT /problems/{id}/settings` | maxRFactor, waveLength, curveType, … |
| `GET|PUT|DELETE /problems/{id}/special-params` | the user-defined curve's formula and parameters. `DELETE` means the user curve was deleted: the problem forgets the formula, and fitting the user-defined type is then refused rather than silently continuing with it |
| `GET /problems/{id}/curves` | fitted curves. Each also carries `fitted`, beside the handle rather than among the parameters and for the same reason the handle is: a parameter is a quantity of the model, and this is a fact about the instance. Each carries `id` - the **handle** to that curve instance, its own field beside `params` because it is a handle to the object and not a quantity of the model - and a `params` array of `{name, value, type, error[, kind]}`. `kind` is emitted only when `value` is not a number: JSON is self-describing, so a text-valued parameter (a module's identity, a label that looks like "3") simply IS a string, and `kind` says so rather than a second field being added beside it |
| `DELETE /problems/{id}/points/{set}/{pid}` | one MEMBER of a point set, by the handle that names it. The member address `/points/{set}` always implied and could not express, because a point used to be only a coordinate pair - so the only way to remove one was to POST the same coordinates again and let `AddPoint`'s toggle do it. `positions` only for now: a curve's identity is issued to the pick it is seeded from, so a pick can be named and a profile sample cannot, and any other set is refused **by name** rather than ignored. Removing a pick removes the curve built from it, which is what makes the deletion stick - the model is rebuilt from its inputs, so dropping only the identity would let the next rebuild put a fresh instance back with a new handle |
| `GET /problems/{id}/curves/{cid}/points`, `PUT /curves/{cid}/params/{j}` | one curve, addressed **by handle**. An unknown handle is 404. Not by ordinal: the model's order is derived - it follows the fit intervals and the picks inside them - so an index held across an edit names a different curve, and both routes used to resolve anything unparseable to curve 0. The handle's wire form carries no braces, because a URL path segment cannot |
| `POST /problems/{id}/actions/{name}` | minimize-difference, do-all-automatically, subtract-background, … |
| `GET /problems/{id}/async`, `/stats`, `/rfactor` | progress and results (`/stats` carries the goodness-of-fit statistics: weighted reduced chi-squared, R^2, AIC, BIC) |

## How much of this surface is actually tested, and at which level

Three levels, and they prove different things. A route that classifies, a handler
that runs, and a reply that survives serialisation over a socket are three
separate claims.

| level | what it proves | where |
|---|---|---|
| routing | the path string classifies to the right enum member | `testcase_rest_routes.pas` |
| in-process | the handler does the right thing given a parsed route | `testcase_rest_api.pas`, through `TFitRestApi.Handle` |
| **end to end** | a real worker answered a real request | `testcase_http_fit_service.pas`, via `worker_process_harness.pas` |

**Every route is tested at the first two levels: 21 of 21 classify, and 21 of 21
run through `HandleRoute`. The gap is the third.**

| route | routing | in-process | e2e |
|---|---|---|---|
| `rtHealth` | yes | yes | yes |
| `rtCreateProblem` | yes | yes | yes |
| `rtDiscardProblem` | yes | yes | yes |
| `rtState` | yes | yes | yes |
| `rtPutPointsSet` | yes | yes | yes |
| `rtGetPointsSet` | yes | yes | yes |
| `rtGetSettings` | yes | yes | yes |
| `rtPutSettings` | yes | yes | yes |
| `rtStats` | yes | yes | yes |
| `rtCurves` | yes | yes | yes |
| `rtCurvePoints` | yes | yes | yes |
| `rtAction` | yes | yes | yes (one verb) |
| `rtDeletePoint` | yes | yes | **yes** |
| `rtAsync` | yes | yes | **no** |
| `rtSelectedInterval` | yes | yes | **no** |
| `rtGetSpecialParams` | yes | yes | **no** |
| `rtPutSpecialParams` | yes | yes | **no** |
| `rtDeleteSpecialParams` | yes | yes | **no** |
| `rtCurveParam` | yes | yes | **no** |
| `rtAddPoint` | yes | yes | **no** |
| `rtMovePoint` | yes | yes | **no** |
| `rtModule` | yes | 404 branch only | **no** |

**Of the fourteen registered actions, only `minimize-difference` is ever invoked
over a socket.** The other thirteen, `minimize-difference-again` included, are
in-process only. The client-side tests prove the client *sends* the right verb
string through a mock transport; nothing proves the server runs it.

Two entries are worth singling out. `rtAddPoint` and `rtMovePoint` are the
picking paths the desktop uses constantly and neither has e2e cover. `rtModule`
has **never returned a success** in any test - only its 404 branch runs - so the
module channel is proven to refuse an unknown resource and nothing more.

**How to read the number.** The process-spawning tests are `integration`, so they
do not gate every commit; and the coverage ratchet measures the `unit` half, so
none of this e2e work moves a coverage figure. This table is the only place this
kind of coverage is visible at all, which is why it is a table and not a number.

**The picks carry their curves' handles, both ways.** `PUT` and `GET
/problems/{id}/positions` both carry an optional `ids` array beside `x` and `y`,
one entry per point and in the same order. Sending one ADOPTS that handle for
that pick, so the instance rebuilt there is the same instance to everything
downstream and values stored under it can be handed back; an empty entry means
"issue one", which is what a project carries for a pick placed since the last
fit. Absent entirely, every message is byte-identical to what it was before the
field existed.

**Only `positions`.** A curve's identity is issued to the pick it is seeded from,
so a pick can be named and a profile sample cannot; `profile`, `background` and
`rfactor-bounds` refuse `ids` **by name** rather than ignoring them, because a
field quietly dropped lets a client believe it restored an identity that was
never established. A list whose length does not match the points is refused
before anything is cleared: nothing can know which pick the missing entry
belonged to, and a wrong guess attaches one curve's values to another
undetectably.

Adoption happens as the picks are written, not in a pass afterwards.
`SetPointUnique` collapses a repeated abscissa to one point, so a later pass
indexing the ids by point number would be off by one for every pick after the
duplicate - and an id that slid by one is not an error anywhere downstream, it is
another curve's shape restored onto this one.

**What a repeated coordinate means, per write path.** One path toggles, and it is
the one the desktop still uses:

| Write path | A coordinate that is already there |
|---|---|
| `DELETE /problems/{id}/points/{set}/{pid}` | removes exactly that member; an id names one thing and nothing is inferred |
| `PUT /problems/{id}/positions` (and `rfactor-bounds`) | **dedupes to one point.** A bulk write says "these are the picks", and the later value wins. It still goes through the uniqueness rule - a pick set holds unique abscissae because every instance is seeded from one - but not through the toggle, which would leave NO point at that abscissa |
| `POST /problems/{id}/points/{set}` | **toggles: the point is removed.** The interactive gesture - the user clicking the same sample twice - and `TFitService.AddPoint`'s own header says so. Superseded for `positions` by the DELETE above, and unchanged because the client depends on it |

Do not "simplify" those onto each other. `AddPoint` is add-or-toggle and is the
interactive primitive; `SetPointUnique` is the bulk one. A test at the service
layer pins each, which is new: the toggle was previously asserted only on
`TFitClient`'s mirror of it, so the copy was covered and the original was not.

**Status codes.** `400` means the request was inadmissible for this problem - a
fit while one is already running, a curve type this build does not have, moving a
pick whose curve has been fitted. `404` is an unknown problem or route. `500` is
reserved for a genuine fault: the engine did something it did not intend, and the
server log carries a stack trace for it. The distinction matters because the two
call for opposite responses - a 400 will fail again identically, a 500 may not.

The desktop client does **not** read the code; it reads the `ok` field and raises
`EUserException` with the `error` message. That is deliberate - it wants the
message, not the class of failure - but it means the codes are only ever checked
by other consumers, so they are covered by tests rather than by use.

## Running the server

```
fit_server [--host H] [--port N] [--log-level L] [--verbose]   # defaults: 127.0.0.1, 8787, debug
```

## Logging (the first place to look when something "does nothing")

Each process writes its **own** file in the config directory (`$HOME/Fit/` on Linux/macOS,
`%APPDATA%\Fit\` on Windows) — two processes must never append to one file:

| Process | File |
|---------|------|
| compute server | `fit_server_log.txt` |
| Python sidecar | `fit_sidecar_log.txt` |
| desktop client | `fit_client.log` |

`--verbose` also echoes the server log to stderr.

Each file is rotated at 32 MB: it is renamed to `<name>.1` (replacing the previous `.1`) and a new
one is started, so the two newest generations survive and nothing older does. A log left to grow is
written until the disk is full, and the session that filled it is unreadable long before that.

### The client's log

The client is instrumented through `Desktop/client_log.pas`, which fixes the tiers (routing to
`log.WriteLog`, exactly as a module's own log unit does for it):

| Tier | Contents | Shown at |
|------|----------|----------|
| UI action | menu picked, file loaded, selection mode entered/left, point picked, long operation started/finished | `notification` |
| State | what the client did as a consequence: data refreshed, picks pushed, table cleared | `debug` |
| Server call | one REST call: verb, path, duration, bytes in/out | `debug` |
| Warning | server unreachable or rejecting, unreadable reply, a state the client repaired | `warning` |
| Fatal | any exception that reached the user: class, message, **and the call stack**, logged before the dialog is even queued | `fatal` |
| Trace | inner loops: per-repaint chart timing, and the REST calls the client polls | `trace` |

The notification tier alone replays what the user did, so a bug report is reproducible from it:

```
ui: loading data file Data/2.dat
ui: selection mode: nothing -> wave bounds
ui: point picked in mode wave bounds: (12, 1030)
ui: long server operation started
```

### Nothing has to be switched on

Both processes start at **`debug`** and log everything but the inner loops, with no argument passed.
This is deliberate: a fault that cannot be reproduced on demand has to be readable from the log the
run already wrote, and a switch nobody passed is a switch that was off during the one run that
mattered. `FIT_QUIET_LOG` builds a quiet binary (back to `notification`) without touching a call
site.

`--log-level` (server) and `/LOG_LEVEL=` (client) therefore only ever turn the log **down**. An
unknown level is reported in the log rather than silently ignored. Both take
`fatal | warning | notification | debug | trace`:

| Level | What it adds |
|-------|--------------|
| `fatal` | failures only: unhandled engine exceptions, a server that cannot start |
| `warning` | + rejected requests (4xx): unknown problem, unknown action, malformed body |
| `notification` | + every request and response with its status and duration **except the polled routes**, the problem lifecycle, and **what each long-running operation left behind** (state, and the point counts of the profile, background, positions, bounds, curves) |
| `debug` | **(default)** + every engine **state transition** and exception stack traces |
| `trace` | + the inner loops: the polled routes, the minimizer's per-iteration progress, per-repaint chart timing |

### Why `trace` is below the default

`trace` is for output whose volume is set by an **iteration count** rather than by anything the user
did. That is a statement about volume, not value — these are diagnostics, kept in full, one switch
away.

Two things live there. The **polled routes** — `/problems/{id}/state`, `/async` and `/rfactor` — which
the client asks for twice a second for as long as it is open; at `debug` a two-hour session buries
every real event under some fifty thousand lines of `state = 5`. And the **minimizer's per-iteration
progress**, which for a single three-second fit is over three hundred lines: measured, that is 86 %
of the file (38.2 KB against 5.3 KB for the same fit). Since the log rotates at 32 MB, that volume
does not merely add noise — it is what evicts the events worth keeping.

Which routes count as polled is decided in one place, `Common/rest_polling.pas`, used by both the
server (choosing the tier for the incoming request) and the client (for the outgoing call). If the
two disagreed, the halves of one call would land at different levels and one of them would vanish.

The operation line is the useful one: an action that reports success while producing nothing is
visible as a completed operation with zero points. That is what a silent no-op looks like:

```
--> POST /problems/1/actions/compute-curve-positions
state BackNotRemoved -> AsyncOperation
state AsyncOperation -> BackNotRemoved
state BackNotRemoved -> ReadyForAutoFit
problem 1: ComputeCurvePositionsDone
operation done in 597 ms; state ReadyForAutoFit; profile 1692, background 99, positions 374, bounds 0, curves 0 points
<-- 200 POST /problems/1/actions/compute-curve-positions  597 ms  { "ok" : true, "message" : "" }
```

Every state change reinitializes data (`TFitService.SetState`), so a wrong transition silently
discards the problem — which is why the transitions are logged at `debug`.

### `PUT /problems/{id}/profile` resets the problem

A profile is not one more field of a problem: everything else the problem holds is expressed in the
profile's own x-values. Background points, curve positions, data intervals and pattern bounds are
picks **on** the data; the curves, the calculated profile and the difference are computed **from**
it. So setting a profile passes through `ProfileWaiting` — the state defined as "what the server had
at start-up" — and only then installs the new points:

```
--> PUT /problems/1/profile
state ReadyForAutoFit -> ProfileWaiting
state ProfileWaiting -> BackNotRemoved
<-- 200 PUT /problems/1/profile  { "ok" : true, "message" : "Now background points should be defined." }
```

A client that keeps its own copies of those sets must therefore re-read them after pushing a profile
(`TFitClient.SendProfileToServer` does), and a client that wants markup on the new data must send it
after the profile, never before. Carrying the old markup over is not a matter of it going stale: a
curve position that names an x the new profile does not contain fails `CreateTasks`' internal check
(`PosIndex <> -1`) as soon as anything rebuilds the sub-tasks — which the next pick does.

Endpoints:

| Route | Purpose |
|-------|---------|
| `GET /health` | liveness + protocol version |
| `POST /fit` | body = the fit problem (JSON); reply = the fitted result (JSON) |

## Building and running both

```
./scripts/build-app.ps1 -Task build     # builds Fit and fit_server
Worker/o/fit_server                     # start the server first...
Desktop/o/*/Fit-*                       # ...then the client
```

The server binds `http://127.0.0.1:8787` by default; `--port` changes it, and the
client points elsewhere through **Fit ▸ Compute Server…**.

## Opening a data file at start-up

The client opens a file given as `/INFILE=<path>`. The Lazarus IDE passes it as a
run parameter (`Fit.lpi` carries `/INFILE=../../../Data/2.dat`), so a run from the
IDE opens the bundled sample.

A relative path is resolved against the executable directory, and a path that does
not exist is logged rather than silently ignored.

## From the Lazarus IDE

A Lazarus **project** produces exactly one binary, so no single project can build both. Use the
project group instead:

1. **Open `fit.lpg`** (Project → Open Project Group…). It contains `Fit.lpi` and
   `fit_server.lpi`. *Requires the "Project Groups" package, which ships with Lazarus but is
   not installed by default.*
2. **Project → Compile all** builds **both** binaries.

### One IDE action to start both

The IDE runs one project per F9, and the two binaries are deliberately independent.
Start the server once as an **External Tool** and keep it running while you work on
the client:

1. **Tools → Configure External Tools… → Add**
   - *Title*: `Start compute server`
   - *Program*: the built `Worker/o/fit_server`
   - *Working directory*: the repository root
2. Assign it a keyboard shortcut (Tools → Options → Editor → Key Mappings).

Then F9 runs the client against it — two independent processes, exactly as in
production. To **debug the server** instead, make `fit_server.lpi` the active
project and start the client by hand.

## Transport failures

The server is a separate process, possibly on another machine, so it can be absent, die, or become
unreachable at any moment. The client treats that as a normal outcome, not a crash:

- every call has a **connect timeout** (5 s) and a **reply timeout** (30 s), so a wedged server can
  never hang the application; only the fitting actions wait indefinitely, and those run on a worker
  thread, never on the UI thread;
- a transport failure is raised as a **user error naming the server** ("The compute server at
  http://... could not be reached"), not as a raw socket error;
- the UI polls the server's state twice a second; if that starts failing it reports the failure
  **once** and stops polling, rather than raising a dialog every tick. **Fit → Compute Server…**
  resumes it.

### Never open a dialog from the code that failed

An error dialog is shown by `TFormMain.QueueError`, which stores the message and hands it to
`Application.QueueAsyncCall`; `ShowPendingError` opens it from the main loop. Nothing reports a
failure with `MessageDlg` directly — not `Application.OnException`, not the poll timer, not the
calculation thread's `Synchronize`d error callback.

This is not style. Those three run at an arbitrary point inside the LCL's event dispatch, which under
X11 may hold an implicit pointer and keyboard grab (a menu, a popup, another modal). A dialog opened
there runs its own message loop *inside* that grab; if the code that faulted never returns, the grab
is never released, and every click and keystroke in the session keeps being delivered to the wedged
process. The machine is healthy and nothing but the power button answers — a client crash taking the
whole desktop with it. Queueing lets the faulting call unwind, and any grab with it, before anything
modal appears.

Two rules go with it: `FErrorDialogPending` admits **one** dialog at a time, so a fault on the
twice-a-second poll path cannot stack them; and any unhandled exception stops the poll timer, because
polling on into an unknown state is what turns one fault into a cascade.

The server is **threaded** (`Server.Threaded := True`), and each problem has a lock: the operations
that touch the engine are serialized, while the progress routes (`/state`, `/async`, `/rfactor`) are
deliberately **not** locked. They exist to be polled *while* an operation runs - if they queued behind
it, the client's UI thread would block for the whole fit, which is exactly what froze the app.

## The Python sidecar

The Python (lmfit) backend is a **subprocess sidecar of `fit_server`**,
not a server the client talks to. The desktop only ever connects to `fit_server`;
Python is integrated the **same way as every compute backend** - as an `IFitBackend`
(`Server/python_fit_backend.pas`, next to `TNativeFitBackend`/`TServerFitBackend`),
selected by the **minimizer kind** (`MIN_KIND_PYTHON_LM`). Nothing about the fit path
is special-cased: the client picks the minimizer with the ordinary `settings` verb and
runs the ordinary `minimize-difference`. When that minimizer is selected, `fit_server`:

1. starts the sidecar the first time it is needed (`Worker/python_sidecar.pas` runs
   `Worker/py/fit_backend.py` with the venv Python), health-checks it, and reuses it,
2. hands its loopback URL to the engine (`TFitService.SetPythonSidecarUrl`), so
   `TFitTask.Optimization` creates a `TPythonFitBackend` that marshals the task to the
   shared fit-problem contract, POSTs it to the sidecar's `/fit`, and writes the fitted
   parameters back into the task - exactly as `TServerFitBackend` does for a remote server.

The sidecar is bound to fit_server's lifetime two ways: fit_server stops it on clean
shutdown, and the sidecar is started with `--parent-pid` so it **exits on its own if
fit_server dies without cleanup** (a `kill -9`). It listens only on loopback; the client
never addresses it.

### Model-agnostic backend (why the sidecar knows no curve types)

Like the native engine, the sidecar is **model-agnostic**: neither optimizer knows a
Gaussian from a Lorentzian. The wire contract (`Worker/fit_problem_json.pas`) carries,
per placed curve:

- the curve's **analytic `expression`** in `x` and its parameter names, in numpy syntax
  (e.g. `A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))`), produced by each curve's
  `GetCurveExpression` (`ModelCurves/*_points_set.pas`; user curves transpile their
  formula via `native_math_expr.ExpressionToNumpy`);
- each parameter's `value`, plus `vary` and `shared` flags.

The sidecar (`Worker/py/lineshapes.py`) **compiles** the expression once and evaluates
it over numpy in a namespace holding only the whitelisted numpy functions, with builtins
removed — it never enumerates curve types. **Decision:** send an evaluable formula rather
than a curve-type GUID the sidecar must dispatch on. The GUID approach coupled the
optimizer to the model and forced the sidecar to re-implement every curve (it silently
supported only three); the expression keeps the optimizer independent of the model, as the
original design intended, and any new analytic curve works with no sidecar change.
`curveTypeId` still travels but is informational only.

**Parameter-variability parity.** `vary`/`shared` make the Python fit hold and tie exactly
the parameters the native engine does. `BuildProblemFromTask` sets them from the native
classification: varied iff `Type_ ∈ {Variable, VariablePosition, Amplitude, Width, Shared}`
and not `VariationDisabled` (mirroring `TCurvePointsSet.InitListOfVariableParameters`);
`shared` iff `Type_ = Shared`. In the sidecar a shared parameter collapses to one lmfit
parameter tied across all curve instances — the same single common value the native engine
keeps in `FCommonVariableParameters`. The abscissa (`Argument`) is not sent; the sidecar
binds `x` to the data.

**Algorithm.** The sidecar fits with scipy **Trust Region Reflective** (`least_squares`,
via lmfit), not MINPACK `leastsq`: it tolerates more free parameters than data points
(M<N) — as the native Downhill Simplex does — while still returning a covariance, hence the
per-parameter uncertainties that are the Python backend's reason to exist.

### Parameter constraints: one range, two enforcement mechanisms

Both engines must respect the same physical limits — `A >= 0`, `sigma > 0`, `eta` in
`[0,1]`, `x0` inside its placement window — but they **enforce them differently, and
deliberately so**. The range is declared once, on the parameter class
(`ModelCurves/CurveParameters/*.pas`), and each engine consumes it in the form it can
actually use.

**Native Downhill Simplex — enforcement by clamping at assignment.** The simplex itself
is *unconstrained*: it will happily propose `eta = 1.5`. The limit is applied by the
parameter object, in `SetValue`, on every write. So the model is only ever *evaluated*
at physical values even though the search is unconstrained. This is safe precisely
because DHS is **derivative-free** — a clamped region is just a plateau it probes past.
(Mild known cost: the simplex can sit on that plateau for a while.)

**Python/trf — enforcement by box bounds handed to the optimizer.** The same range is
marshalled as `min`/`max` per parameter (from
`TSpecialCurveParameter.GetMinValue`/`GetMaxValue`) and given to lmfit. This is *not* a
stylistic difference. Clamping inside the residual would make the objective **flat**
beyond the limit, so the finite-difference **Jacobian column becomes zero**: the step
direction degenerates, convergence stalls, and the covariance — i.e. the per-parameter
uncertainties that are the whole reason this backend exists — is meaningless in that
direction. Trust Region Reflective is *designed* for box constraints; bounds keep the
gradient valid inside the feasible region and handle the boundary properly.

**Why they must stay compatible.** The invariant is not that the bounds equal the clamp.
It is that the bounds are a **subset of the values the clamp leaves untouched**:

> every value inside `[GetMinValue, GetMaxValue]` must survive `SetValue` unchanged

That is what makes a bounds-respecting Python result apply losslessly to the native
curve. A *tighter* bound than the clamp is therefore fine, and sometimes deliberate:
sigma advertises `TINY` (1e-6) although the clamp tolerates smaller, because a
near-zero width blows the model up and the optimizer should never go there.
`tests/testcase_parameter_bounds.pas` pins this invariant for every parameter class.

**Known asymmetry.** The clamps use `Abs`, which *reflects* (`eta -0.3 -> +0.3`), whereas
a box bound *stops* at 0. Immaterial in practice — the bounds stop trf proposing
negatives at all — but tests must assert containment in `[min, max]`, never equality
with the nearest bound.

**Rule for contributors:** declare the range on the parameter class, next to the
`SetValue` it describes. Never hand-write bounds anywhere else, and never derive them
per curve type in the marshalling.

**What happens when they are incompatible** — this is not hypothetical. While the Python
fit was unbounded it drove `eta > 1` and `sigma < 0`, reached a lower residual out there,
and the values were then **clamped on writeback**, so the model the app recomputed was not
the model Python had fitted: weighted chi-square **13,345,660 vs 1,111** for DHS on
`Data/2.dat`. It looked like a bad optimizer; it was a constraint mismatch.

**Degenerate windows.** `min == max` (common for `x0`, whose window is the neighbouring
data points) means the native engine cannot move that parameter at all, so the sidecar
holds it fixed — lmfit also rejects `min == max`.

Two regression gates keep this honest: `tests/testcase_python_real_data.pas` fits
`Data/2.dat` with both engines and requires Python to be no worse than DHS, and
`PythonQualityIsNoWorseThanNative` in `tests/testcase_python_backend_process.pas` makes
the same comparison **per curve type**. For how the two minimizers actually compare in
fit quality (and the objective-vs-metric subtlety behind those gates), see
[minimizer-comparison.md](minimizer-comparison.md).

**Weighting.** `weighting` on the wire selects the Python backend's residual weighting —
`poisson` (`1/sqrt(counts)`, the default; statistically correct for counting data and
matching the reduced chi-square the app displays) or `none` (unweighted, like the native
objective). The native engine always fits unweighted and ignores it, so the desktop hides
the **Fit → Weighting** menu unless the Python minimizer is selected.

**Bounded effort (why a fit can no longer hang).** A saturated model — free
parameters approaching the number of data points, e.g. 10 two-branch curves over the
51 points of `Data/2.dat` — used to run effectively forever.

- **The cause: an unbounded solver budget, amplified by the numerical Jacobian.**
  lmfit's default here is ~2000·(n+1) ≈ 102,000 `nfev`. That number understates the
  work badly: with 50 free parameters scipy estimates the Jacobian by finite
  differences, so *each iteration* costs ~51 extra residual evaluations that `nfev`
  does not count. The real budget is therefore millions of curve evaluations, with no
  point at which the solver gives up. `MAX_NFEV_PER_PARAM` (300 per free parameter)
  caps it, so a fit always returns; hitting the cap surfaces as `success=False` in the
  log with the best point so far, instead of a frozen UI.
- **Secondary: thread oversubscription.** numpy/BLAS spawned a thread per core for the
  tiny matrices a curve fit uses. Measured cost on this fit: 19.7 s unpinned vs 8.7 s
  pinned — a real ~2.3x penalty (and misleading CPU readings, a runaway showed 1277%),
  but *not* what made it hang. `fit_backend.py` pins
  `OMP/OPENBLAS/MKL/NUMEXPR` to one thread *before* numpy is imported.

**Cost per evaluation is what makes or breaks this backend.** The optimizer is not the
problem — Trust Region Reflective converges on the 2.dat 10-curve model in ~8,500
evaluations, which is a perfectly ordinary count and fewer than the native Downhill
Simplex uses. The native engine wins on *cost per evaluation*: compiled Pascal is
~1 us, so DHS finishes in about a second. Two things closed most of that gap:

- **Compile the expression** instead of interpreting its syntax tree per call
  (`asteval` cost ~97 us per curve).
- **Evaluate all curves in one broadcast call** rather than looping in Python once per
  curve: `x` is passed as `(1, npoints)` and each parameter as `(ncurves, 1)`, giving
  `(ncurves, npoints)` which is summed. One expression evaluation per residual instead
  of ten. (`fitting.py` falls back to the per-curve loop when curves do not share the
  same parameter names.)

Net effect on that fit: **9.1 s -> 1.3 s**, same result (R-factor 0.0313). When a fit
feels slow, look at the per-evaluation cost first, not the optimizer.

`TPythonFitBackend` additionally sets `ConnectTimeout`/`IOTimeout`, so even a wedged
sidecar reports an error rather than blocking the application.

**Logging.** The sidecar's stderr is detached from `fit_server`, so it is given
`--log-file` (`fit_sidecar_log.txt`, beside `fit_server_log.txt` in the config dir). Every
fit logs its shape (points, curves, free/shared/fixed parameter counts, weighting,
expression) and how the solve ended (`success`, `nfev`, `redchi`, the unweighted R-factor
and the solver's termination message) — the first place to look when a fit disappoints.
The outcome's floats are sanitised before serialising: NaN/Infinity are valid Python JSON
but not strict JSON, and would reach the client as "returned an unreadable result".

## Architecture notes

- The seam is `IFitBackend` (`Server/interfaces/int_fit_backend.pas`): one call performs one
  whole fit. `TNativeFitBackend` runs it in-process; `TServerFitBackend` POSTs it to a server.
- The wire contract (`Worker/fit_problem_json.pas`) is deliberately **engine-free** plain
  records, so it can be tested in isolation and marshalled across the process boundary.
- `Server/fit_task_marshalling.pas` maps both ways between the wire contract and a live
  `TFitTask`.
- **Careful:** a curve position carries an `(x, y)`; the **y seeds the curve's amplitude**
  (`RecreateCurves`). Dropping it starts the fit from a zero-amplitude curve that never
  converges — so it is carried across the wire.
- **The selected curve type lives on both sides.** The client has its own registry
  (`TCurveTypesSingleton`) for the menu and the legend; the server keeps a `curveType`
  setting that decides what is fitted, defaulting to the alphabetically first registered
  type. Only `TFitClient.SelectCurveType` may change the selection — it updates the registry
  and PUTs the setting. Anything that sets one without the other makes the app fit a
  different model from the one shown as selected, silently.
