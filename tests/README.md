# Tests

Automated tests for Fit, using **FPCUnit** (ships with FPC/Lazarus). The console
runner builds and runs headlessly, which is what lets the whole suite run in CI.

## Running them

```sh
./scripts/build-app.ps1 -Task test -Suite unit   # the fast half, seconds
./scripts/build-app.ps1 -Task test               # both halves
```

Run the unit half while you are changing something, and the whole suite before you
believe a result. A unit run builds no compute server, because a unit test has
nothing to ask one — see the rule below.

## The two suites, and the rule that sorts them

Every test class registers itself into one of two suites, and the criterion is
**dependencies, not speed**:

```pascal
RegisterTest('unit', TMyTest);          // needs nothing outside its own process
RegisterTest('integration', TMyTest);   // needs something outside it
```

A test is an **integration** test if it depends on any of:

- a **process boundary** — it starts a compute server, speaks HTTP, needs the
  Python sidecar;
- the **filesystem** — it reads a fixture, writes a settings file, opens a data
  file. A file is an external dependency exactly as a socket is: it can be
  missing, stale, or left behind by whatever ran before;
- or it **drives the optimiser to convergence**, which is neither cheap nor a
  test of one unit.

Everything else is a unit test. An earlier version of this rule kept
fixture-reading tests on the unit side on the grounds that reading `Data/2.dat`
is fast. Speed is not the criterion — the dependency is.

1812 unit tests run in about fourteen seconds; the 72 integration tests take the rest
of the couple of minutes `-Task test` costs.

**`testcase_suite_split.pas` fails the suite when a class registers into
neither half**, and that check earns its place: an unclassified test does not
error, it quietly disappears from `--suite=unit`. That is the half line coverage
is measured over, so the number would go on being reported for a suite that had
silently stopped running part of itself.

## Which binary carries which tests

`tests/fit_tests.lpi`, built with `lazbuild --widgetset=nogui` (the LCL linked
headlessly), carries **everything**. That is what `-Task test` builds.

`tests/build.sh` builds a smaller binary with plain FPC and no LCL at all, for a
machine that has FPC but not Lazarus:

```sh
sh tests/build.sh                  # everything in that binary
sh tests/build.sh --format=xml     # JUnit-style XML
```

**It is not the unit suite**, and the difference cuts both ways. That binary
carries 1549 unit tests where the Lazarus-built one carries 1812: the classes it
is missing reach the REST surface, the curve-type registry or the user-defined
curve, whose configuration dialog names LCL `Controls`. It also carries 9
*integration* tests, because reading a fixture from disk needs no LCL to do it.
A green run there does not mean the unit half passed.

`build.sh` reproduces the project's compiler settings — **Delphi mode**, **CORBA
interfaces (`-SIcorba`)**, **assertions on (`-Sa`)** and the
`FIT`/`FITCLIENT`/`FITSERVER` defines — plus the unit search paths. The `-Sa`
matters: a dozen tests assert that a precondition is *refused*, and with
assertions compiled out the call quietly succeeds, so those tests fail here while
passing in the Lazarus-built binary. Note that FPC reuses a `.ppu` whose source
has not changed even when the options have — after changing a flag, delete
`tests/*.ppu tests/*.o` before believing the result. Override `FITMINIMIZERS` if that package is installed
elsewhere.

## The Python sidecar

```sh
Worker/py/.venv/bin/python -m pytest Worker/py
```

Kept at an enforced 100 % line coverage gate. Some Pascal integration tests are
**skipped rather than failed** when the sidecar is absent — they report as ignored
and say why.

## Adding a test

- One `testcase_*.pas` unit per area, registering its `TTestCase` in
  `initialization` — **with a suite name**.
- Add the unit to the `uses` clause of `fit_tests.lpr`. The always-compiled half
  is the plain-FPC one; the `{$IFDEF UseNoGUI}` half is for anything that pulls an
  LCL or server unit, and each entry there says why it has to be there.
- Prefer a unit test. A decision table over plain values can be tested
  exhaustively in milliseconds; the same logic reached only through a live
  `TFitTask` usually cannot be tested at all — and only the unit half is measured.
- Test data is the repository `Data/` samples plus small synthetic fixtures with
  known parameters.

## Where a test lives

Most tests sit in `tests/` beside each other. Two directories mean something:

- **`tests/mocks/`** — the test doubles, and `mock_support.pas`, which states
  the rule they all follow.
- **`tests/fitminimizers/`** — tests whose subject is in the **sibling
  `fitminimizers` repository**. That package has no suite of its own, so its
  tests are built and run by this one; the directory is what makes that visible
  rather than something to be discovered from a `uses` clause. See the README
  there.

There is deliberately no `tests/fitgrids/`: that package is excluded from the
coverage target (`tools/coverage/vendor.txt`), because a figure that moves when
a separately published component grows is measuring the wrong thing.

A test's directory changes nothing else about it — it registers into `unit` or
`integration` like any other and `fit_tests.lpr` names it in the same uses
clause.

## Mocks

`tests/mocks/` holds the test doubles, and `mock_support.pas` states the rule they
all follow. It matters more than it looks: everything compiles `-SIcorba`, so
interfaces are CORBA style with **no reference counting**. A mock is a plain
`TObject` - never `TInterfacedObject`, whose refcounting is inert here and whose
presence reads as a lifetime guarantee that does not exist - it exposes
`AsObject`, and the fixture owns it, nils the interface first and frees the object
after.

A mock **records; it does not assert**. A mock that failed inside its own callback
would report from whatever thread the code under test happened to use, and would
name the mock rather than the expectation.

**Beware the process-global registries.** Several of them are append-only by
design, because a module registers once at start-up. A test that registers a mock
into one leaves it there for the rest of the run, so the mock has to behave like a
real thing that simply has nothing in it. Returning `nil` from
`TMockAppModule.CreateSession` once made twenty-seven REST tests fail several
hundred tests away from the cause: `TFitService` asks every registered module for
a session as soon as a problem is created, and then dereferences the answer. See
`mock_module_session.pas` for what "inert" has to mean.

### Transport seams, and why there are four of them

Four classes now declare a `protected virtual` transport method that a mock
overrides - `http_fit_service` (`Fetch`/`Send`), `python_fit_backend` (`Post`),
`server_fit_backend` (`Get`/`Post`), and the process launch decisions extracted
from `python_sidecar` into `sidecar_launch`. Each was written the same way and for
the same reason: the class was almost entirely marshalling, the request was built
inline around a client the test could not reach, and the unit measured near zero
while being what every action of its kind went through.

The pattern is worth naming because it keeps recurring:

- the **decision** goes where a test can call it - a virtual method, or a
  function that fills a `TStrings`;
- the **syscall** stays where it is, in the one method the mock replaces;
- the timeouts and the retry policy belong to the adapter, not to the caller, so
  they stay on the real side of the seam.

`sidecar_launch` is the same move without an object: which paths to try is a
decision and asking the disk is not, so the candidate list is a function and the
`FileExists` loop stayed in `python_sidecar`.

### Driving a thread without starting it

`TThread.Synchronize` called from the main thread runs its method inline instead
of queueing it. So `testcase_calc_thread` creates `TMainCalcThread` suspended,
never starts it, and calls its methods directly - which is the only way to observe
the eight callbacks it marshals, and is how a dropped assignment in
`SetSyncMethods` was found. A class being built on a thread is not a reason it
cannot be tested.

**Self-enforcing tests are the house speciality**, and they are what makes this
scale: a test that walks a registry and asserts every registered thing has what it
needs will fail when somebody adds the next curve type, loss function or module
without fixtures. Coverage says which lines ran; these say whether the cases that
matter are covered at all.

See [testing](../docs/contributing/testing.md) for what coverage counts and why,
and [architecture](../docs/contributing/architecture.md) for where the suites sit
in the system.
