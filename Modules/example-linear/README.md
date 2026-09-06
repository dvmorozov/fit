# Example module: the linear ramp

A complete, working module in six files. Nothing in the framework names it, and
nothing in it edits the framework — putting this directory on a project's unit
search path is the entire difference between a build that has it and one that
does not.

It exists to be read and copied. If you are writing a module, start here.

## What is in it

| File | What it is |
|---|---|
| `linear_points_set.pas` | the curve type — the module's actual content |
| `example_module.pas` | the front door: one procedure that registers everything |
| `app_modules.pas` | overrides `Common/app_modules.pas`; names the front door |
| `module_tests.pas` | overrides `tests/no-modules/module_tests.pas`; names the tests |
| `testcase_linear_ramp.pas` | the module's own tests |
| `*.lpi` | the three projects, each a public one plus this directory |

## Building it

```
lazbuild Modules/example-linear/Fit_example.lpi          # the client, with the module
lazbuild Modules/example-linear/fit_server_example.lpi   # the compute server, with it
lazbuild Modules/example-linear/fit_tests_example.lpi    # the public suite plus its tests
tests/fit_tests_example --all --format=plain
```

The suite runs the public tests **and** this module's — 6 more than the public
suite alone. The client shows a **Curve Type ▸ Example ▸ Linear ramp** entry that
a build without this directory does not have.

Compare `Fit_example.lpi` with `Desktop/Fit.lpi`: the only meaningful difference
is one entry at the front of `OtherUnitFiles`. The rest is the same project
re-rooted one directory deeper.

## How it plugs in

Two units carry the whole mechanism, and both work the same way — a module ships
its own copy of a framework unit and puts its directory **first** on the search
path, so the compiler finds the module's copy instead of the framework's:

- `app_modules.RegisterAppModules` — called once by the client and once by the
  compute server, before any menu is built and before anything can create a
  curve. The framework's copy does nothing; this one calls the front door.
- `module_tests` — the same idea for the test suite. Naming a test unit in its
  `uses` clause is what links it, and linking it is what registers its fixture.

Both stubs deliberately live somewhere no project calls home
(`Common/`, `tests/no-modules/`), because a project's own directory is searched
before its search path and a copy sitting beside the program could never be
overridden.

## Why the front door ends with `ExpectCurveTypes`

A unit that nothing references is not linked, so its `initialization` section
never runs and its curve type is simply **absent** — with nothing to say so. That
is not hypothetical: a whole feature was once missing from the compute server for
exactly this reason, while every test passed, because the test binary linked the
units either way. The symptom was a fit that looked like a hang.

`ExpectCurveTypes('Example', [TLinearPointsSet])` asserts, at start-up, in
whichever binary is running, that the types really did register. It is the reason
a mis-ordered search path fails loudly instead of silently dropping your module.

## Things worth copying

- **Declare the position by ROLE, not by class.** The engine builds any
  registered type through the one-argument constructor and assigns `x0`
  afterwards. `TPositionCurveParameter` derives its variation boundaries at
  construction time, so on a curve that has no points yet it pins the position to
  its seed. The built-in peak types avoid this by taking `x0` in their
  constructor; a module using the generic path declares
  `Type_ := VariablePosition` instead.
- **Keep `GetCurveExpression` and `DoCalc` in agreement**, including the support
  boundary. The expression is what the Python sidecar and a remote compute server
  fit with; `DoCalc` is what fits in process. If they disagree, the same model
  gives different answers depending on which engine ran it, and both look
  plausible.
- **Name every parameter in the expression.** One the expression omits is one a
  formula-based backend cannot vary, so the fit quietly holds it at its seed.
- **Give the type a group** (`GetCurveTypeGroup`) so the Curve Type menu shows
  where it came from instead of growing one flat list.

## What a module can contribute

This one contributes only a curve type, which is the smallest useful module.
The same front-door shape carries the rest — data loaders, minimizers, loss
functions, REST actions, sidecar routes, a picked point set of its own, menus and
a right-panel tab — each through its own registry. See
`docs/contributing/writing-a-module.md`.
