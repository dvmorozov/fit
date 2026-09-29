<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Writing a module

A module is **a directory plus one registration unit**. Putting that directory
first on a project's unit search path is the entire difference between a build
that has your module and one that does not. No framework file changes, no
`{$IFDEF}`, no dynamic loading.

`Modules/example-linear/` is a complete working module — six files, its own tests,
and three projects. Read it alongside this document; it is the template.

This page is the walk-through. **The contract** - what a module is, every seam,
the assumptions still standing in the way and the criteria a new seam must meet -
is [module-architecture.md](module-architecture.md), and it wins where the two
disagree.

**Everything a module registers explains itself.** Override
`TNamedPointsSet.Explanation` on each curve type, and register an
`IExplanationProvider` for anything else a user meets - rules, verdicts, row
menu entries. `ExplanationFindings` over the registry must come back empty, and a
module's suite should assert it.

## The shape

```
your-module/
  your_curve.pas          your actual content
  your_module.pas         the front door: ONE procedure, registers everything
  app_modules.pas         overrides Common/app_modules.pas
  module_tests.pas        overrides tests/no-modules/module_tests.pas
  testcase_your.pas       your tests
  YourApp.lpi, ...        the public projects plus your directory
```

Two of those files are copies of framework units that do nothing. Your copies do
something, and they win because your directory comes **first** on the search path:

| Your copy of | Overrides | Answers |
|---|---|---|
| `app_modules.pas` | `Common/app_modules.pas` | which modules the applications contain |
| `module_tests.pas` | `tests/no-modules/module_tests.pas` | which module tests the suite contains |

Both framework stubs deliberately live somewhere no project calls home. A
project's own directory is searched before its search path, so a copy sitting
beside the program could never be overridden.

## The front door

```pascal
unit your_module;
interface
procedure RegisterYourModule;
implementation
uses
    curve_type_registration,
    //  Naming your units is what LINKS them.
    your_curve;

procedure RegisterYourModule;
begin
    ExpectCurveTypes('Your pack', [TYourCurve]);
end;
end.
```

**Why it ends with `ExpectCurveTypes`.** A unit that nothing references is not
compiled into the binary, so its `initialization` section never runs and its curve
type is simply *absent* — with nothing to say so. That is not hypothetical: a
whole vertical was once missing from the compute server for exactly this reason,
while every test passed, because the test binary linked the units either way. The
symptom was a fit that looked like a hang. `ExpectCurveTypes` asserts at start-up,
in whichever binary is running, that your types really did register.

## What a module can contribute

| Contribution | Through |
|---|---|
| curve types, and **how their instances are placed and built** | virtual class methods on `TNamedPointsSet` |
| a picked point set of its own | `IModulePointSink` + a per-problem `IModuleSession` |
| its own server resources | `GET/POST /problems/{id}/modules/{vendor}/{resource}` |
| menus, buttons in the Tools pane, a right-panel tab, a pick mode | `IUiModule` — declared as data, so you name no LCL type |
| data loaders, backends, minimizers, losses, REST actions | the corresponding registry |
| **where data is fetched from** | `data_source_registry.RegisterDataSource` |
| Python sidecar routes | `@routes.get` / `@routes.post` in `<name>_routes.py` |

Every one is a registration call from your front door. None of them requires an
edit to a framework file.

### Contributing a file format and a data source

Both go through the framework's registries, from your front door, and the
framework learns nothing about your field:

```pascal
procedure RegisterMyModule;
begin
    RegisterMyExplanations;                                     //  first: the walk asks for topics
    RegisterDataLoader(TMyLoader, '.ABC;.DEF', 'My format');    //  a format this field uses
    RegisterDataSource(TMySource);                              //  where its data comes from
end;
```

**A source finds and fetches; it never parses data.** It declares which
extension the file it hands over has, and the loader registry decides what reads
it — so a file fetched from your source and the same file opened from disk take
one path. What a source *does* parse is its own catalogue: the JSON or HTML
saying which records exist.

Your source class descends from `TDataSource` (`Desktop/DataSources/`) and
answers four things:

| Member | Answers |
|---|---|
| `Info` (class function) | id, title, category, summary, explanation topic, the query fields it asks, the extensions it produces, whether it needs the network, and whether results can hold other results |
| its query fields | declared as data: `ChoiceField` for a closed list, `SuggestedField` for free text with the common answers offered, `TextField` for something nobody can enumerate (an address, a DOI), `DateField` for a date the user picks from a calendar; `EnabledWhen(Field, OtherId, Value)` for a field asked only while another holds one value - dates only while a range is "Custom dates" |
| `Search` | what a query finds — files, or containers when it said so |
| `Children` | what is inside a container |
| `Download` | writes the bytes and answers the file name they are saved under |

**Fetch only through the `IWebClient` your source is constructed with** - no
connection, `fphttpclient` or TLS library of your own. It runs the system `curl`
and already applies the size cap, the cancel and the failure wording. For
anything else fetched from the internet, reuse `TCurlClient`
(`Common/curl_client.pas`). Non-negotiable 11.

The wizard derives its steps from `Info` alone: declare no query field and it
shows a list, declare containers and it adds a step for choosing inside one.
There is no table anywhere of which source needs which step.

**A choice your module's menu makes can outlive the session**: keep it with
`SetModulePreference('yourmodule.key', Value)` and read it with
`ModulePreference('yourmodule.key', Default)` (`Common/module_preferences.pas`).
Read it where it is used - a price reader reads which column
to take when it is created - and tick your menu from it when you declare it; it
is read on first use, so the order in which the window starts does not matter.
Never add a property to the framework's settings for it.

**A field that matters only for one answer to another declares it**:
`EnabledWhen(DateField('from', ...), 'range', 'custom')`. The wizard greys it
until the answer is chosen, writes the condition under it in the words on
screen (`FieldCondition`: *Used only when Range is "Custom dates".*), leaves it
out of the search (`EffectiveQuery` - what is left in a greyed box is not sent)
and does not count it as missing when it is required. The walk refuses a
condition naming a field you did not declare, or a choice that field does not
offer. Your `Search` should still read a greyed field as unasked, because a query
can reach it by another route than the wizard.

**NOBODY SHOULD HAVE TO KNOW AN IDENTIFIER.** A service names a series `SP500`
and a substance `C71432`; a user knows "S&P 500" and "benzene". So a choice
carries both - `Choices(['S&P 500', 'SP500', ...])` - and the wizard shows the
caption while your source is handed the value. The walk refuses a choice with no
caption, because that would put the identifier in front of the user, which is the
one thing this exists to prevent.

**Two rules the walk enforces**, so get them right before you run anything: a
source needs an explanation topic that resolves, and every extension it produces
needs a loader registered in the same build — otherwise your user searches,
downloads and is then told the file cannot be read.

`Modules/open-data-spectra` is the worked example: one loader, one source, one
provider, over recorded pages so that no test needs the network.

### Where your `<name>_routes.py` goes

Keep it in **your own repository's `Worker/py/`**, beside the rest of your
sidecar code — nothing of yours belongs in the framework's tree. Name the pack
from your front door with `RegisterSidecarModule('<name>')`, and the sidecar
imports `<name>_routes.py` on start-up, looking in

1. its own directory (the installed layout, where everything sits together),
2. any directory named by `--module-path` / `FIT_MODULE_PATH`,
3. `Worker/py` of every repository checked out beside `fit/` (the development
   layout — this is the one that finds yours while you work).

A pack that is named and found nowhere is a **start-up failure** naming the
directories searched, not a 404 on its routes later.

### Capabilities, not type tests

Where a decision can live on your curve class, it lives there. The engine asks
the class; it never asks "is this one of ours?".

| Class method | Answers |
|---|---|
| `GetCurveExpression` | the closed-form formula the Python and remote backends evaluate — empty if there is none |
| `IsAnalytic` | whether those backends may be offered at all (must agree with the above; a test checks) |
| `GetCurveTypeGroup` | which submenu of the Curve Type menu it appears in |
| `AmplitudeIsUnbounded` | whether the amplitude can move over orders of magnitude, which decides objective compatibility |
| `PlacedByPointSet` | the point set instances are placed from, or empty for the ordinary one-x-per-curve path |
| `PreferredAxisMode` | the axis mode, by id, each coordinate is meant to be shown in - or none |

Adding a seventh curve type therefore needs no edit to any compatibility table.

## The UI, without naming a widget

A module implements `IUiModule` and **declares** its menu — ids, captions, hints,
kind, radio group — as data. The host builds the widgets and calls back with the
id. Questions, messages, hints and the pick gesture go through `IUiHost`. A module
that does this uses no LCL unit at all, so it survives a replacement of the thin
client instead of being rewritten with it.

### Where each entry is shown

One declaration, up to three surfaces. `Surface` says which:

| `Surface` | Drawn in |
|---|---|
| `csMenu` (the default) | the Model menu only |
| `csPane` | the Tools pane only |
| `csBoth` | both |

A declaration written before the pane existed leaves `Surface` alone and is
unaffected by its arrival — but an entry that asks for a button owes it two
things:

- **`ShortCaption`.** The pane is narrow and every button in it is one width, so
  a full menu caption widens all of them, which moves the splitter and the chart.
  The menu keeps the long wording, the button gets the short one — short enough
  to pass `tool_pane_layout.PaneCaptionFits`, which is the framework's own budget
  and what its rows are held to. Assert it in your suite; the window also reports
  a caption that does not fit, under `/CHECK_UI`.
- **`Hint`.** Your row drives no action to take a hint from, so your declaration
  is the only text there is. The window's `/CHECK_UI` reports a pane button
  without one.

`PaneGroup` names the heading the button sits under: give it the id of one of
your own submenus and the pane groups your entries the way the menu nests them,
under that submenu's **caption**. Anything else — an unknown group, or none —
sits under your module's own name, the same word the menu bar shows.

**A toggle or a radio becomes a latch**: the button stays pressed while the thing
is on, which is the one claim a menu entry cannot make. A radio's siblings are
released by the framework when one of them is clicked, in *either* surface — so
you need not tell the host which of your settings is on, only what to do when one
is chosen. What you do state, through `IUiHost.SetMenuChecked` and
`SetMenuEnabled`, reaches the button and the menu entry alike.

### The pick gesture

`BeginPointPicking(APointSet, AMenuId, APicksPerGesture, AHint)` starts collecting
picks into your point set. Three of those four are about the gesture **ending**,
because that is where a picking mode goes wrong:

- `AMenuId` is your own toggle entry. The host ticks it while the mode is on and
  unticks it whenever the mode ends — including the ends you never hear about,
  such as another selection mode starting or a profile being loaded. A tick left
  behind claims a mode that is off, and the next click on the entry then reads as
  *leave* instead of *enter*.
- `APicksPerGesture` is how many picks make one whole thing — two for a pair of
  bounds. The host ends the mode when they are made: what they made is drawn, the
  pick markers come off, the entry unticks. Pass `0` only for a mode with no
  natural end.

Each pick is marked on the chart as it is made (a diagonal cross on the point),
and the markers go with the gesture. Between the picks of a pair there is nothing
on the server to draw, so those markers are the only thing telling the user that
the first pick landed, and where.

### While an operation runs, your panel is left alone

The window does not rebuild the Model panel while a fit or a computation is
running, so `PanelSelectionChanged` is not called during one. That is a rule
about the framework, not a request about your code, and it exists because of
what happened without it: rebuilding the panel re-establishes the selected row,
which hands that row back to the module — and a module that answers by asking
the engine about the model waits for the running fit's session lock. That wait
happens on the **window's own thread**, so the whole window stops: no repaint,
no live progress, no response to the user, for the length of the fit. It was
reported as "the application hangs and the animation does not work", and the
live progress view had been working correctly the whole time.

The panel is rebuilt again when the operation finishes, from the result.

If you nevertheless need to read from the engine on the window's thread, read a
polled route (`/state`, `/async`, `/rfactor`, `/progress`) — those are answered
without waiting for the fit. Anything else waits.

## Traps worth knowing before you hit them

- **Declare the position by role, not by class.** The engine builds any registered
  type through the one-argument constructor and assigns `x0` afterwards.
  `TPositionCurveParameter` derives its variation boundaries at *construction*
  time, so on a curve with no points yet it pins the position to its seed. Use a
  `TUserCurveParameter` named `x0` with `Type_ := VariablePosition`.
- **`GetCurveExpression` must agree with `DoCalc`**, including any support
  boundary. The expression is what an out-of-process backend fits with; `DoCalc`
  is what fits in process. If they disagree, the same model gives different
  answers depending on which engine ran it, and both look plausible.
- **Name every parameter in the expression.** One it omits is one a formula-based
  backend cannot vary, so the fit quietly holds it at its seed.
- **`AddPoint` deletes on a repeated x.** It is add-or-toggle, which is right for
  a set of independent picks and wrong for pairs. If your module collects pairs,
  bring your own `IModulePointSink`; do not route picks through the shared helper.
- **A project copied one directory deeper must use forward slashes** in its unit
  filenames. A backslash is a literal character on Linux, and the main program
  file then cannot be found.

## Making a module explain itself

Everything a user meets in your module - a curve type, a rule, a refusal, a greyed
menu entry, a row in the Model panel - has to say what it is, why it is allowed
or refused, how settled that is, and where it comes from. The framework draws all
of it (the Explain pane, Help ▸ Explain Everything, the generated guide) from one
registry; your module only fills it.

**Curve types** override `TNamedPointsSet.Explanation`. The framework's
curve-type provider adapts every registered type, so there is nothing to register.

**Everything else** goes through one `IExplanationProvider` of your own, registered
from your front door with `RegisterExplanationProvider`:

- `Namespace` is your module's name, the first segment of every topic you issue.
- `StaticTopics` lists every topic that exists whatever the user's document holds
  - one per rule, per choice your menus can offer. The completeness tests and the
  generated guide walk this list.
- `Explain` answers those, and may also answer **computed** topics that are not
  listed, such as a verdict about one pattern in the user's markup. It answers
  only the exact topic asked, never a neighbour.

**Topics are stable, namespaced strings** (`<namespace>/rule/<id>`), built in one
unit your menus, rows and provider all call, so the place that issues a topic and
the place that answers it cannot spell it differently. A row points at one with
`TOutlineRow.Topic`, a menu entry with `TUiMenuDecl.Topic`, and a refusal puts one
in front of the user with `IUiHost.ShowExplanation`.

**Every explanation states its standing**, and the standing decides what it must
carry:

| Standing | Means | Must carry |
|---|---|---|
| `esCanonical` | stated by the field's authoritative sources | the rule's words as `Quote`, and at least one reference |
| `esConvention` | usual practice, a guideline | a body that says so |
| `esModelChoice` | a decision this software made - a bound, a simplification | a limitation saying what it costs |
| `esUnsettled` | the sources disagree or say nothing | a body that says which |

`ExplanationIsComplete` names whatever is missing: a summary that is not a
sentence or repeats the title, an empty body, a canonical claim without its source.
A related link must resolve too. None of this is checked by a reader, so it is
checked by the tests below.

## Building and testing it

```
lazbuild your-module/YourApp.lpi
lazbuild your-module/your_tests.lpi
tests/your_tests --all --format=plain
```

Your suite runs the public tests **and** yours: the framework's tests prove you
did not break it, and yours prove your module works. Neither suite depends on the
other's fixtures.

**The explanation completeness tests run in your suite too.** They walk the
registries, so a curve type your module registers without overriding
`Explanation`, a topic your provider lists that does not resolve, or a related
link to nothing fails the build by name - see
[module architecture](module-architecture.md#the-seams).

**Your module's explanations as guide pages.** Build
`scripts/dump-registries/dump_registries.lpr` from a project of your own whose
search path puts your module first, exactly as your application project does, run
it to write a registry dump, and give the dump to the generator:

```
python3 scripts/gen-diagrams/gen_diagrams.py guide --dump registries.json --out docs/generated
```

It writes one Markdown page per explanation namespace - yours beside the
framework's - from the same records the Explain pane shows, so the pages cannot
say something the application does not.

## Publishing a module, or keeping it private

A module published from its own repository and one kept private use the same
contract; the difference is only where the repository is published. The one
thing a private module adds is its identifiers on the leak gate's list, so a
published tree never names it.

### Keeping a module private

Nothing above requires your module to live in this repository. Point the search
path at a directory in a repository of your own and the arrangement is unchanged —
which is how the modules that are not published here are carried. **No file exists
in two repositories**, so nothing is ever merged back and forth.

One consequence to know: this repository is published as a **snapshot** — one
orphan commit, force-pushed — so its history here is not the history upstream, and
an external pull request cannot be merged directly. See `CONTRIBUTING.md`.
