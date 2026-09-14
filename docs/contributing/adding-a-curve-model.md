<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Adding a curve model

Fit supports two kinds of curve (peak) model. Choose by whether the shape needs compiled
code or can be expressed as a formula.

## 1. A user-defined curve (no code)

End users add curves at run time by typing a formula — see the user guide
(`docs/user-guide/user-defined-curves.md`). No contribution is needed. Under the hood the
formula is evaluated by `Common/native_math_expr.pas` (a cross-platform wrapper over FPC's
`TFPExpressionParser`), and the run-time curve is `TUserPointsSet`
(`Desktop/ModelCurves/UserPointsSet/`). Extend that engine if you need new functions or
operators.

## How a curve is computed — two evaluators of one formula

A curve type is, at heart, **one formula**. Neither engine is the "real" model — the formula
is, and both must agree on it:

- the **native engine** evaluates it Pascal-side to fill the plot/calc profile (always rebuilt
  Pascal-side — a shape the native side can't compute could not even be displayed);
- the **Python sidecar** is model-agnostic: it receives the same formula as a text
  `GetCurveExpression` in **numpy syntax** and evaluates it over numpy (see
  `docs/contributing/client-server.md`).

There are two ways a built-in curve provides its native evaluation:

- **formula-driven (§2, recommended for new lineshapes):** the curve owns a single formula
  string; the *same* string drives both engines, so they agree **by construction**.
- **compiled `DoCalc` (§3, the original six):** the curve computes points with a hand-written
  `DoCalc` (calling `SimpMath`) *and* returns a separate hand-written `GetCurveExpression`. Two
  implementations of one formula — kept in step only by tests.

### The parity rule (special functions)

Some shapes need a special function: true Voigt needs the Faddeeva function, EMG needs `erf`.
Such a function must exist on **both** evaluators — added to `Common/native_math_expr.pas`
(Pascal) *and* whitelisted from `scipy.special` in `Worker/py/lineshapes.py` (Python). A
function present on only one side is a bug the test harness must catch. Pin both to the
**scipy/lmfit reference values** (the golden oracle) plus an **analytic limit** check (Voigt →
Gaussian as the Lorentzian width → 0; Pearson VII → Lorentzian at m = 1; EMG → Gaussian as the
rate → 0). Parity of the Pascal↔numpy *translation* is pinned for every formula by the
three-legged differential harness in `tests/expr_fidelity_cases.txt` (see §Tests).

## 2. A formula-driven built-in curve (recommended)

If the shape can be written as a closed-form formula, subclass **`TFormulaPointsSet`**
(`Desktop/ModelCurves/formula_points_set.pas`) — it computes the curve from a single formula
via the same cross-platform engine (`native_math_expr`) that user curves use, so there is no
`DoCalc` and no second formula to drift. `TPearson7PointsSet` and `TMoffatPointsSet` are the
worked examples; copy one.

**Steps**

1. **Subclass `TFormulaPointsSet`.** Implement only:
   - `function GetNativeExpression: string;` — the formula in the **native** engine's syntax
     (`^` for power, `ln` natural log, `sqr`, …) in `x` and the parameter names, e.g.
     `A/(1+(2^(1/m)-1)*(2*(x-x0)/sigma)^2)^m`. The base translates it to numpy for the sidecar
     via `ExpressionToNumpy`, so you do **not** override `GetCurveExpression`.
   - `GetCurveTypeName` (menu label), `GetCurveTypeId` (a fresh GUID), `GetExtremumMode`.
   - a `constructor Create(AOwner: TComponent; x0: double)` that adds the parameters (see
     *Parameter roles* below) and calls `InitListOfVariableParameters`.
2. **Self-register** in `initialization` (see below) and **link** the unit (see below).
3. **Normalisation:** keep the existing curves' convention (amplitude = area) *unless it needs
   a special function you'd have to add on both sides just for the constant* — e.g. Pearson VII
   / Moffat are **peak-height** normalised (A = peak) to avoid pulling `gamma` into the native
   engine. Document the choice in the class comment.

## 3. A compiled `DoCalc` built-in curve (Pascal)

Use this only when the shape can't be a formula (needs iteration, a lookup/sampled table, or
speed that the expression engine can't give). Follow the original curves in
`Desktop/ModelCurves/` (e.g. `gauss_points_set.pas`).

**Steps**

1. **Subclass `TNamedPointsSet`** (via `TCurvePointsSet`). Implement:
   - `class function GetCurveTypeName: string;` — the menu label (Title Case).
   - `class function GetCurveTypeId: TCurveTypeId;` — a fresh GUID (unique per type).
   - `procedure DoCalc(const Bounds: TPointsSet); override;` — fill `PointYCoord[i]` for the
     interval from the parameters.
   - `function GetCurveExpression: string; override;` — the same formula as an analytic
     **numpy-syntax** string in `x` and the parameter names (e.g.
     `A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))`). This is what lets the **Python
     backend** fit the curve: it is model-agnostic and evaluates whatever expression it is
     sent (see `docs/contributing/client-server.md`). Use `where(cond, a, b)` for
     branch/asymmetric shapes. If a shape genuinely has no closed form, leave the inherited
     empty result — the Python backend then reports a clear error instead of mis-fitting.
   - the parameter set the curve exposes (amplitude, position, width, …).
   - *(optional)* `class function CreatePreferredAxis(AWaveLength: double): TArgumentAxis;
     override;` — the abscissa the curve is meant for. The inherited default is
     `TIdentityAxis` ("Position", the argument exactly as loaded), which is right for anything
     that is not diffraction; the peak lineshapes override it with
     `TDiffractionAngleAxis.Create(dmTwoTheta, AWaveLength)`. See
     [adding-an-argument-axis.md](adding-an-argument-axis.md).
2. **Self-register** in the unit's `initialization` section:

   ```pascal
   initialization
     CTS := TCurveTypesSingleton.CreateCurveFactory;
     CTS.RegisterCurveType(TMyPointsSet);
   ```

3. **Link the unit** so its `initialization` runs (registration only happens for units the
   program actually links). Add it to the `uses` clause of **all three** binaries that must
   know the type:
   - `Desktop/Forms/form_main.pas` — the desktop client (so it appears in the curve menu);
   - `Worker/fit_server.lpr` — the compute server (so it registers in the engine process);
   - `Server/fit_task.pas` — and add a branch to **`TFitTask.CreatePatternInstance`** that
     constructs it for its GUID (this is how the server instantiates the selected type).

   You do **not** touch the curve-selection UI: `TFormMain.ActionSelCurveExecute` iterates the
   curve-type factory (`ICurveTypeIterator`), so a registered type is selectable automatically.
   `tests/testcase_user_curve.pas` guards that registration works.

   **Selecting a type has two sides.** `TCurveTypesSingleton` is the *client's* registry: it
   drives the menu and names curves in the legend. The compute server is a separate process
   with its own selection, which is what actually decides the fitted model. The UI must
   therefore never call `ICurveTypeSelector.SelectCurveType` directly — it calls
   **`TFitClient.SelectCurveType`**, the single entry point that updates both. Updating only
   the registry left the menu checking one type while the server went on fitting with its
   default (the alphabetically first registered one). `tests/testcase_curve_type_selection.pas`
   pins the invariant that the two never disagree.

### Parameter roles

The fit recognises special parameters so it can initialise them from the data. A parameter
gets a role either **by name** or **by explicit type** (`TParameterType` in
`special_curve_parameter.pas`), resolved in `TCurvePointsSet.SetSpecParamPtr`:

| Role      | By name  | By type                                  | Initialised from     |
|-----------|----------|------------------------------------------|----------------------|
| argument  | —        | `Argument`                               | axis variable        |
| position  | —        | `InvariablePosition` / `VariablePosition`| placement            |
| amplitude | `A`      | `Amplitude`                              | data peak            |
| width     | `SIGMA`  | `Width`                                  | fitting interval     |

`Has*`/`Set*` accessors (`HasA`/`A`, `HasSigma`/`Sigma`, `Hasx0`/`x0`) let the fit set these
during `RecreateCurves` (`Server/fit_task.pas`). Amplitude and width parameters stay
**variable** (optimised).

### Parameter limits

If a new parameter class clamps its value in `SetValue` (as amplitude, sigma, eta and
position do), also override **`GetMinValue`/`GetMaxValue`**, right next to it, to report
that range.

The two exist because the engines enforce limits differently, on purpose:

| engine | how the limit is applied | why |
|--------|--------------------------|-----|
| native Downhill Simplex | `SetValue` **clamps** on every write; the search itself is unconstrained | DHS is derivative-free, so a clamped region is just a plateau |
| Python / trf | the range is shipped as **box bounds** to the optimizer | trf is gradient-based; clamping would flatten the Jacobian and destroy the uncertainties |

You do not have to make them identical. The rule is:

> every value inside `[GetMinValue, GetMaxValue]` must survive `SetValue` unchanged

A *tighter* bound than the clamp is fine (sigma does this deliberately). A *looser* one is
a bug: the Python fit would then optimise into a region the native side clamps away on
writeback, and the recomputed model stops matching the fitted one — which is exactly how a
40x fit-quality regression got in. `tests/testcase_parameter_bounds.pas` enforces the rule;
see `docs/contributing/client-server.md` for the full reasoning.

Declare the range on the parameter class only — never hand-write bounds in the marshalling
or per curve type.

## Tests

Add a headless end-to-end fit for a new shape, modelled on `tests/testcase_fit.pas`
(built-in) or `tests/testcase_user_curve_fit.pas` (user curve): build a synthetic peak,
`RecreateCurves`, `MinimizeDifference`, and assert the R-factor is small. Run with
`./scripts/build-app.ps1 -Task test`.

Also guard the new `GetCurveExpression`: `tests/testcase_curve_expression.pas` checks it
names every parameter, and `tests/testcase_python_backend_process.pas` fits the curve
through the real Python sidecar and checks it agrees with the native engine.

**For a formula-driven curve (§2), pin the formula two ways:**

- **Native↔numpy parity** — add a row (or two) to `tests/expr_fidelity_cases.txt`:
  `pascal ;; numpy ;; name=val,… ;; expected`. The three-legged harness then proves
  `native(pascal) == expected == numpy(translate(pascal))` automatically — `testcase_expr_fidelity.pas`
  checks the Pascal legs and `Worker/py/test_fit_backend.py` checks the numpy leg, both off the
  same file. Generate `expected` from numpy (they must agree; the Pascal test re-verifies).
- **Golden + limit** — a `tests/testcase_<shape>.pas` (see `testcase_pearson7.pas` /
  `testcase_moffat.pas`) that drives the curve through its own compute path
  (`AddNewPoint` → set `ValuesByName` incl. `x0` → `ReCalc(nil)` → read `PointYCoord`) and
  asserts numpy-generated golden values plus the shape's **analytic limit** (per the parity
  rule above). Register it in `tests/fit_tests.lpr`.

For a compiled `DoCalc` curve (§3), keep the expression numerically faithful to `DoCalc` — the
sidecar's own tests compare the built-in expressions against `SimpMath.pas` to ~1e-9.
