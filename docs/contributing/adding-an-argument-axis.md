<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Adding an argument axis

An **argument axis** is a display-only transform between the stored (raw) x-argument and the
value shown to the user. It supplies the axis name and unit and a forward/inverse transform.
It **never** alters stored data or the fit — it only affects display, export,
and reported positions.

The abstraction lives in `Desktop/argument_axis.pas` (LCL-free, so it is unit-testable):

```pascal
IArgumentAxis = interface
    function DisplayName: string;                              // e.g. '2*Theta'
    function UnitName: string;                                 // e.g. 'deg'
    function ToDisplay(const RawValue: double): double;        // raw  -> shown
    function FromDisplay(const DisplayValue: double): double;  // shown -> raw
end;
```

`TArgumentAxis` is the common base (so callers can hold/free any axis by one type). Existing
implementations to copy from:

- `TIdentityAxis` — the general default; both transforms are the identity.
- `TDiffractionAngleAxis` — the legacy 2θ / θ / sin(θ)/λ family in one class (raw = 2θ°).
- `TExpressionAxis` — a user-defined axis whose forward/inverse are formulas in `x`, evaluated
  by `Common/native_math_expr.pas`.

## Attaching an axis to a curve type

Which axis is shown is **the model's** decision by default, not a global setting. Every curve
class answers:

```pascal
class function CreatePreferredAxis(AWaveLength: double): TArgumentAxis; virtual;
```

`TNamedPointsSet` returns `TIdentityAxis`, so a type that declares nothing shows the raw
argument as `Position`. The diffraction peak roots (`TGaussPointsSet`, `TPseudoVoigtPointsSet`,
`T2BranchesPseudoVoigtPointsSet`, `TFormulaPointsSet`) override it with
`TDiffractionAngleAxis.Create(dmTwoTheta, AWaveLength)`, and every other lineshape inherits
through them. The caller owns the returned axis.

The display mode `XCM_CURVE` is what asks the selected class for it, and it is the default:
**Data → Argument Transformation → From Curve Type**. Choosing any other item in that menu is
an explicit override — it wins over the model's preference and is persisted
(`Settings_v1.ViewModeChosenByUser`), so the app never silently moves an axis the user picked.

Cover a new type's axis in `tests/testcase_axis_defaulting.pas`, which selects a type through
`TFitClient.SelectCurveType` and asserts the caption the user would read.

## Steps

1. **Subclass `TArgumentAxis`** in `Desktop/argument_axis.pas` and override the four methods.
   Keep `ToDisplay` and `FromDisplay` exact inverses so round-tripping a value is stable. If
   the transform needs a parameter (like a wavelength), take it in the constructor and store
   it; guard undefined states with `Common/checks.pas` — **not** `Assert`, which is
   compiled out of the build users run. See
   [no-silent-degradation.md](no-silent-degradation.md); a build test refuses a new one.
2. **Make the viewer route through it.** `TFitViewer.DisplayX` (`Desktop/fit_viewer.pas`)
   single-sources every raw→shown mapping used for plotting and min/max; add your axis there
   (or, better, have the viewer build the axis once and call `ToDisplay`). Do **not**
   reintroduce per-mode `Point2T`/`PointT`/`PointSinTL` switches — that duplication was the
   reason this abstraction exists.
3. **Expose it in the UI** under **Data → Argument Transformation** in
   `Desktop/Forms/form_main.pas` (see how `General Position` and `Custom Position...` items are
   created at run time, and `ApplyViewMode` / `RestoreViewMode`). Assign a new `XCM_*` display-
   mode constant in `Server/mscr_specimen_list.pas` (the single source of these constants — do
   not redeclare them elsewhere).
4. **Persist it.** Add any axis-defining fields to `Settings_v1`
   (`Desktop/app_settings.pas`) as published properties and read/write them in the main form's
   `ReadSettings`/`WriteSettings`, mirroring the custom-axis fields.

## Parameterised / wavelength-style axes

If the axis depends on external state (e.g. `TDiffractionAngleAxis` needs the wavelength),
gate the relevant UI control to that axis and pass the value into the constructor. Keep the
validation inside the axis, as a `CheckThat` with a sentence saying what the axis
needed, so an undefined parameter fails loudly rather than producing silent garbage.
`TDiffractionAngleAxis` is the worked example: a `sin(theta)/lambda` axis with a zero
wavelength refuses instead of dividing by it.

## Tests

Add deterministic in→out tests to `tests/testcase_argument_axis.pas` (FPCUnit, `nogui`): assert
`DisplayName`/`UnitName`, sample `ToDisplay` values against a hand calculation, and assert
`FromDisplay(ToDisplay(v)) = v` (round-trip). If the axis is persisted, extend
`tests/testcase_settings_persistence.pas` with a round-trip of its settings fields. Run with
`./scripts/build-app.ps1 -Task test`.
