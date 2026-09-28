<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Adding an axis mode

An **axis mode** is one way of showing a coordinate: the argument along the bottom
of the chart, or the value up its side. It supplies the axis's name and unit, a
forward and inverse transform, and, when it has a reason to, how the axis is marked
and read out. It **never** alters stored data or the fit. It changes only what is
drawn, what the chart's titles say, what the pointer's readout on the right says,
and how positions read in the Curve Attributes table.

Modes are **registered**, not enumerated. The framework registers its field-neutral
ones, and a module registers its own from its front door. The menus, the chart's
popup, the defaulting rule and the project file are all derived
from what is registered. Nothing anywhere lists which mode goes with which field.

## The pieces

| Unit | What it is |
|---|---|
| `Desktop/coordinate_axis.pas` | `TCoordinateAxis`: the transform, the name and unit, `Title`, `ReadoutCaption`, `ReadoutText`, and optional marks (`ChooseMarks`, `FormatsMarks`, `MarkText`). The axes that exist: `TNamedAxis`, `TLogarithmicAxis`, `TDateAxis`, `TBarDateAxis` (bars named by their dates), `TExpressionAxis`, `TDiffractionAngleAxis`, `TDurationAxis`. |
| `Desktop/axis_mode_registry.pas` | `TAxisMode`: a class declaring `Info` (id, caption, topic, which coordinates it shows, what parameter it reads) and building its axis. `RegisterAxisMode`, `FindAxisMode`, `AxisModesFor`, and the completeness walk `AxisModeFindings`. |
| `Desktop/axis_mode_registration.pas` | The framework's own modes (General Position, General Value, Logarithmic, Custom) and `RegisterAllAxisModes`. |
| `Desktop/axis_choice.pas` | The **automatic rule**, what a remembered choice resolves to (`UsableModeId`), and the migration of the former `XCM_*` integers (`LegacyArgumentModeId`, `StoredModeId`). |
| `Desktop/chart_axes.pas` | `TChartAxes`: the window's one object for both axes. The viewer draws through it, and the titles, readout captions and menu ticks are read off it. |

## Writing a mode

```pascal
TPriceAxisMode = class(TAxisMode)
public
    class function Info: TAxisModeInfo; override;
    class function CreateAxis(ADimension: TAxisDimension;
        const AContext: TAxisContext): TCoordinateAxis; override;
end;

class function TPriceAxisMode.Info: TAxisModeInfo;
begin
    Result.Id := 'price.price';        //  what projects remember - never change it
    Result.Caption := 'Price';          //  the menu entry
    Result.Topic := PriceDataTopic;     //  must resolve to an explanation
    Result.Dimensions := [adValue];     //  which coordinates it can show
    Result.Parameter := apNone;         //  what it reads besides the coordinate
    Result.ParameterRequired := False;  //  and whether it cannot draw without it
end;
```

Then call `RegisterAxisMode(TPriceAxisMode)` from your module's front door.

- **The id is permanent.** Prefix it with your module (`vendor.thing`). Renaming it
  silently loses every remembered choice of it.
- **`Dimensions` may hold both coordinates.** Logarithmic and Custom do. Override
  `CaptionFor` when the entry should read differently on each side (Custom Position /
  Custom Value).
- **Keep `ToDisplay` and `FromDisplay` exact inverses.** The grid shows one and stores
  the other, so a mismatch moves a curve when the user merely looks at it.
  `EveryRegisteredModeBuildsAnAxisThatRoundTrips` checks every registered mode.
- **A value with no place on the axis is `NaN`.** The logarithm of zero is the example.
  The chart leaves a gap there (TAGraph's `PointIsDrawn`). Never clamp or raise: a clamp
  draws a point that is not in the data, and raising stops the whole plot over one sample.
- **A mode that transforms a quantity keeps its name.** `AContext.QuantityName` is what
  the automatic rule says the coordinate is, so a log axis over a price is titled
  "Price, log scale".
- **A dated series read by bar hands its dates over.** `AContext.ArgumentDates` holds
  the day of each bar when the argument counts bars of a series that had them
  (`TDataLoader.ArgumentDates`), and is empty for the value and for everything else.
  The price Date mode builds a `TBarDateAxis` from them - still one bar a step, each
  named by its day - and a `TDateAxis` over a series read by date.
- **Parameters are declared, not asked for.** `apWaveLength` and `apDefinition` are the
  two that exist; the window asks for the one a mode declares
  (`TChartAxes.NeedsParameter`). A new kind of parameter is a new seam: add it here,
  not as a branch in the window.

## Saying which mode a coordinate is in

Nothing picks a mode by name at run time; things **prefer** one, by id:

- **A curve type** overrides `TNamedPointsSet.PreferredAxisMode(ADimension)`. Answer
  only for a coordinate the type really knows. A wave pattern knows its value is a price,
  but whether its argument is a bar or a date is the data's to say.
- **A curve type's fallback**, `TNamedPointsSet.FallbackAxisMode`, is a weaker
  preference: used only when the data says nothing, so the data outranks it. A wave
  pattern assumes bar numbers; a series read by date still says Date.
- **A data format** declares its coordinates when its loader is registered - the
  price-series module's front door registers its price reader as
  `RegisterDataLoader(TOHLCFileLoader, '.CSV', 'Price data, OHLC', '', PriceAxisModeId)`,
  after `RegisterPriceAxisModes`, since a format may name only a registered mode.
  It belongs to the registration and not to the reader, because one reader can serve
  formats that mean different things. Declare only what EVERY file of the format is:
  `.dat` declares nothing, because any two-column series is saved as one - price
  samples included - and declaring 2 Theta captioned them in scattering angle.
- **A reader** that decides a coordinate as it reads overrides
  `TDataLoader.CoordinateMode`. The price-series module's OHLC reader answers Date when it read by date, and
  also when it read by bar from a file whose dates it could read without guessing -
  it then keeps them (`ArgumentDates`) - and Bar otherwise.

The **automatic rule** (`axis_choice`) resolves each coordinate on its own. It asks the
model's curves (when they agree), then the data, then the curves' fallback, then the
selected curve type (only while the model is empty), and otherwise uses the general
name. The user's own choice
from the menu outranks all of it, and is saved **in the project, and only there**. It is
not a setting: a new project, and a different data file imported into the open one,
start on Automatic (`TChartAxes.Reset`), and a changed axis is unsaved work
(`ProjectFingerprintParts`). It was a setting once, and one project's log axis then
followed the user into every later one.

What the data said, and the bars' dates, are saved in the project too, because a
project stores the profile's points and not the file, so nothing reads the file again.

## Explaining it

Every mode's topic must resolve (`EveryRegisteredModeIsComplete`), and every mode must
be named in the guide by its menu path, for each coordinate it shows:
`Data > Argument Transformation > Use Rule > <caption>` and
`Data > Value Transformation > Use Rule > <caption>`
(`EveryAxisEntryIsNamedByItsPath`). A module names its modes in its own chapter.

## Tests

- **The axis itself:** deterministic in→out values in `tests/testcase_coordinate_axis.pas`.
- **The mode:** the registry walks above cover it by construction.
- **A preference:** assert the title the user reads, through `TFitClient.AxisPreferences`
  and `TChartAxes`, as `tests/testcase_axis_defaulting.pas` does.
- **A module:** the red test belongs in the module's suite, through the real client and
  server.
