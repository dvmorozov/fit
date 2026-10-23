<!-- SPDX-License-Identifier: CC-BY-4.0 -->
# Module architecture

This is **the one contract every module is written against**, public or private,
from this repository or from one it has never heard of. `writing-a-module.md` is
the tutorial that walks through it; `architecture.md` is where the system as a
whole is drawn. When those and this page disagree, this page is right and the
others are stale.

## What a module is

> **A module is an entity that covers one class of fitting tasks and is fully
> responsible for it:** it creates the user interface suited to that class of
> tasks, and provides the model elements (curve types, parameters, point sets,
> axes, loaders), the rules (constraints, refusals, verdicts) and the
> computations (builders, sidecar routes, actions, losses) that class needs, and
> explains every one of them.
>
> **The framework provides only the hosting:** the fitting engine, transport,
> persistence, the window, and the registries modules contribute through.

Three consequences follow, and each is a rule:

1. **The framework ships no domain.** Every domain is a module - including
   diffraction, the one this application grew out of: its shapes, angles and
   formats are the scientific module's (`Modules/scientific`), and the pieces of
   it still in the core are listed in [roadmap.md](../internal/roadmap.md),
   section 3a, as debt to be moved out.
2. **A module never makes the framework name it, and never depends on another
   module.** If a module seems to need either, the seam is missing: add the
   seam, not the branch.
3. **Public and private modules use the same contract.** Whether a module is
   published is a publishing decision, never an architectural one.

## Why the application is built this way

This is a **framework for fitting arbitrary data to arbitrary classes of
models**, meant for research and for teaching as much as for use:

- *Research* - results reproduce, and every override the user can notice
  explains itself (`Server/fit_advice.pas`); nothing degrades silently.
- *Teaching* - everything a user meets says what it is, why it is allowed or
  refused, whether that rests on the field's canonical sources, on convention or
  on this software's own choice, what it does not cover, and where to read more.

So every seam below is judged by one question: *what does it cost the person who
adds the next module, for a field nobody here has thought about - and does what
it adds explain itself?*

## The seams

Every contribution is a registration call from the module's one front door
(`Register<Name>Module`). A module uses only these.

| Seam | Registry / contract | What a module contributes | Completeness test a module must pass |
|---|---|---|---|
| Curve types | `curve_types_singleton.RegisterCurveType`, proved by `curve_type_registration.ExpectCurveTypes` | model elements and their capabilities (class functions on `TNamedPointsSet`) | `ExpectCurveTypes` at start-up; `testcase_curve_type_explanations` |
| Explanations | `explanation_registry.RegisterExplanationProvider`; `TNamedPointsSet.Explanation` | what each thing is, its standing, limitations and sources | `ExplanationFindings` over the registry is empty |
| Notices | `notice_registry.RegisterNotice(ATopic, ARequiresAcknowledgement)` | terms the module states: listed in Help > About FIRST, above the framework's own licence notice (`legal_notices.AboutNotices`), because the terms of the build in hand are the ones read first; and - when acknowledgement is required - shown at start-up until accepted. Declining closes the application, since a notice dismissed without accepting it has not been acknowledged. Acknowledgement is per major version (a new major version may change the terms, a new build does not), is kept as the module preference key `fit.acknowledged-notices` in `settings.json`, and is not asked in an unattended run (`--check-ui`, a recording), which says so in the log | `NoticeFindings` is empty: every notice's topic resolves |
| Per-problem state | `module_registry.RegisterAppModule` - `IAppModule`, `IModuleSession`, `IModulePointSink`; the session's factory registered in the module's copy of `server_modules` (`RegisterServerModules`, compute server only) | resources under `/problems/{id}/modules/{vendor}/{resource}`; its own picks. The `IAppModule` declaration (name, resources) is registered in both hosts - the client reads which resources are long-running - but its session, and anything else that reaches the engine, only in the server: the client contains no server-side unit | the module's own REST tests; `client_without_engine.tests.ps1` walks the module's client project |
| Project state | `TProjectModuleDocument` through the session's project resource | what a saved project keeps for the module | a save-and-reopen test |
| User interface | `int_ui_host.RegisterUiModule` - `IUiModule`, `IUiHost` | menus and Tools-pane entries declared as data - an entry declared `Disabled` starts greyed on the menu bar and the pane, and its `Topic` is what hovering it explains; Model-panel rows; a web page opened for the user (`IUiHost.OpenWebPage`, https only); the context menu over a row (`RowMenuItems`) - over the module's own rows, and over the framework's with the curve's handle as the row id, where a module answers only for a curve it placed; explanations shown on request | declaration tests against a scripted `TMockUiHost` |
| Reports (opt-in) | `IUiModule.ReportCaption` declares a tab; `IUiHost.ShowModuleReport` and `IFitViewer.ShowModuleReport` fill it with a `module_report_types.TModuleReport` | the module's verdicts on the whole model: sections of findings, each with a status in words, what was measured, the limit, and the topic of the rule applied; a module that reports nothing returns '' and no tab exists | `ModuleReportFindings` is empty over the module's reports: every failure and warning links to an explanation that resolves |
| Chart overlay | `int_module_overlay.RegisterModuleOverlay`; `IFitViewer.PlotModuleSeries` with a `module_view_types.TModuleSeriesStyle` | what it draws after each redraw - markers, lines, captions, or candles (`CandleOpen/High/Low`); a series it lets the user drag names its own command (`DropCommand`), and the drop comes back through `IUiModule.Command` with `DropCommandData` | presenter tests against a mock viewer (`TMockFitViewer.ModuleStyle`); a drag through the module's REST resource |
| Drawn baseline | `TNamedPointsSet.DrawnBaselineIn(AModel)`, carried as the optional `baseline` of a points reply (`TPointsData.Baseline`) | what each point of a curve is drawn ON, given the whole model - for a component whose contribution is a deviation from other components, drawn where it belongs. Display only: the fit, the residual and every statistic read the curve's own values; the chart draws value + baseline (`TTitlePointsSet.DrawnY`) | `TDrawnBaselineRegistryTest` walks every registered type: nothing, or one value per point |
| Model building | `curve_builder_registry.RegisterCurveBuilder` | how its markup becomes placed curves | engine tests over the builder |
| Model settling | `model_settlers.RegisterModelSettler` | the relations between its curves that the fit must keep: called on a task's curves before they are computed - in a fit, before every evaluation - so a component placed on another follows it as the fit moves it, and a fitted model is one the builder would rebuild | a unit test calling `SettleModel` as the task does; a save-and-reopen of a fitted model |
| Model ownership | `curve_type_registration.ExpectCurveTypes` - the pack name it is called with is the OWNER of the types (`CurveTypeOwner`); `TNamedPointsSet.AcceptsBackground` | which module a model belongs to: a model holds one module's curves only, so once it holds anything another module's types are greyed and refused (`fit_advice.AdviseCurveTypeChoice`); whether a model of a type takes the framework's background | the module's REST tests of the refusals, both ways |
| Fit intervals | `IModuleSession.ProposeFitIntervals`; the exclusive right limit `CheckIntervalAllowed` and `SliceForInterval` receive (`fit_interval_limits`) | how Fit intervals > Auto splits the module's model - its proposal replaces the intervals; two intervals may meet at one sample, owned by the later, and an item ending there still belongs to the earlier. A module that proposes nothing has Auto refused for its model rather than searched for peaks | a unit test of the split; the module's REST test of Auto and of a fit over the result |
| Automatic placement | `IModuleSession.SearchFitIntervals` and `PlaceInstancesFromData`, asked of the session whose point set places the selected type (`fit_advice.AdviseIntervalSource`, `AdvisePositionSource`) | for an empty model, the stretches its instances would span, found in the data (Fit Intervals > Auto); and the instances themselves, added to its markup inside the intervals only where none are (Curve Positions > Auto) - so a model built automatically is corrected by hand and the command run again | the module's REST tests of both commands, including that an instance already there is left as it was |
| Automatic reduction | `TNamedPointsSet.RemovableInReduction` and `DescribesOnlyNoise`; `TFitTask.ReduceInstancesByBicAlg`, run by Fit > Automatically on a model it placed itself | which of the instances a module placed may go after the fit, and which describe only noise: smallest first, each removed when BIC is lower without it, or whatever BIC says when it describes only noise; re-fitted after a sweep that removed something. The handles removed reach the module through `TryRemoveInstance`, as Delete curve's do - so the next rebuild does not place them again. Peaks keep their own reduction | the framework's test of the reduction (`TFitTest`), and the module's tests of its answers |
| Run progress | `TFitService.SetRunStage`, `AddRunEvent` (a `fit_progress_json.TRunEvent`), `PublishModelFrame` - called by the session while an operation it runs is under way | what a long placement is doing: the stage named in the window's header, each decision it takes for the Run Log (a title, the reason, and the topic that explains it), and the model so far as a frame on the chart, rationed like the engine's. The framework logs its own decisions through the same log, so the Run Log is one tab in every build | the module's REST test polling `/progress` while its placement runs |
| Computations | `python_sidecar.RegisterSidecarModule`; `action_registry.RegisterAction`; `fit_loss.RegisterLoss`; `minimizer_registry.RegisterMinimizer` | sidecar routes, REST actions, objectives, optimisers | the sidecar's 100 % coverage gate; the registry dumps |
| Data | `data_loader_registry.RegisterDataLoader`; `TDataLoader.SampleColumns`, `TDataLoader.ValueName` | file formats, and values read beside the one plotted, by name and per point (`sample_columns`) - kept with the profile and saved with the project, for the module's own drawing, and offered by the data source wizard's preview as a choice of value column | a collision is refused in words; an identical registration is a no-op, because a front door may be called twice; the framework's general readers (.csv/.tsv tables, .xlsx/.ods sheets) are DEFAULTS (`RegisterDefaultDataLoader`) that a module's claim on the same extension takes over, whichever registers first |
| Coordinates | `axis_mode_registry.RegisterAxisMode`; preferences through `TNamedPointsSet.PreferredAxisMode` and the weaker `FallbackAxisMode`, the loader registration's declared modes and `TDataLoader.CoordinateMode` | ways of showing the argument, the value or a running fit's R-factor - a name and unit, a transform, marks and a readout - and which of them its curve types and formats are in | `AxisModeFindings` over the registry is empty; every mode is named in the guide by its menu path; every preferred, fallback or declared mode is registered |
| Module preferences | `module_preferences.ModulePreference` / `SetModulePreference` | a module's own choices between sessions, under keys it prefixes with its name - which price column a `.csv` file is read by. Read on first use, so a module can tick its menu from them while it declares it. Kept as the `preferences` section of this machine's `settings.json` (`machine_settings`), and in memory until the application names that file, so no test writes a user's configuration (a test resets with `UseMachineSettings('')`) | `testcase_module_preferences`; the framework's settings publish no module's property (`testcase_settings_model`) |
| Web access | `IWebClient` (`GetText`, `Download`, `PostForm`, `PostJson`), through the system curl only (non-negotiable 11) | fetching a page or a file, and posting a form or a JSON query to a service - a licence service, figshare's search - whose refusal comes back as its own body rather than as the client's words for the status. Every transfer is logged (`data_source_failure.TransferLogLine`), and every failure the wizard meets is logged with what was being done and ends by naming the log file | tests serve recorded answers through `TMockWebClient` |
| Data sources | `data_source_registry.RegisterDataSource` | where data is fetched from: the source's query fields, category and explanation declared as data, the extensions it produces, and whether it needs the network | `data_source_findings` walks the registry - every source's topic resolves and every extension it produces has a loader - and `-Task check-ui` asks the RUNNING build what it registered |
| Updates | `update_feed.RegisterUpdateGate` | a gate that holds a release back with a reason the update check shows; the installers and feed themselves are the product identity's (`product_identity`), never a module's | unit tests of the gate; `update_check` tests through a web-client double |
| Product identity | `<module>/packaging/identity.json` - read by packaging (`build-app.ps1 -Identity`), and written by the build into the Pascal unit its `pascalUnit` names (`Product<Key>` constants; `tools/build-lib/module_identity.ps1`); keys under `build` never become constants | the product's name, licence and licence text, publisher and support pages, feed, store - edited in that one file, never copied into Pascal by hand | the module's own test that its constants are the file's; `module_identity.tests.ps1` |
| Product feed | `-Task pro-feed -NotesFile <notes>` | the folder its installed copies read - this version's installers under the names the program looks for, and `latest.json` - uploaded with the identity's `build.updateFeedUpload` command, or left ready when it has none | `module_feed.tests.ps1` |

A module names no LCL type, no widget and no other module. Its UI is data; its
explanations are records; its rules are functions a test can call. A window a
module cannot do without - the dialogs that define a user's formula - is the
CLIENT APPLICATION'S, kept in the client's tree (`Desktop/ScientificForms`) and
linked only beside the module: its `module.json` names it as `forms`, and the
product's `client_modules`, which only the desktop client calls, registers it
where the module looks (decided with the user, 2026-10-17).

**A Model-panel row names at most one curve** (`TOutlineRow.CurveId`). Selecting
the row highlights that curve's series on the chart, and the commands on one
curve act on it. A parent row whose children are curves of their own does not
bring the children with it, so selecting a parent highlights only its own curve.
This is a known limit of the user-interface seam, not something a module should
work around. The options for lifting it are recorded in
[roadmap.md](../internal/roadmap.md), section 3a, "A Model-panel row that stands
for several curves".

## The assumptions still standing in the way

Each row is something the framework still assumes about ONE field. None may be
worked around from a module; each is retired by a field-neutral seam, staged in
`docs/internal/roadmap.md`.

| # | Assumption | Blocks | Field-neutral seam |
|---|---|---|---|
| A1 | ~~The x axis is a diffraction angle or the identity (closed `XCM_*` modes)~~ | - | **retired**: axis modes are registered, for either coordinate, and persisted by id (the Coordinates seam above). The framework registers only its own field-neutral modes (`CoreAxisModes`); the diffraction angles and intensity are registered by the scientific module's front door, and the price modes by the module that analyses price series. What is left of diffraction's axis in the core is the wavelength parameter the sin(θ)/λ mode reads (roadmap section 3a) |
| A2 | ~~Per-problem physical constants are framework settings (the wavelength)~~ | - | **retired 2026-10-17**: the wavelength is the scientific module's session state and project part, and the client reads it through `problem_value_store`, by name - the background fraction too, since 2026-10-18, with the peak search that reads it (`peak_search_registry`) |
| A3 | ~~The point-set base class carries domain physics (`TNeutronPointsSet`)~~ | - | **retired 2026-10-17**: every set is a `TPointsSet`, and a loader builds a plain one (`TDataLoader.CreatePointsSet`) |
| A4 | ~~File formats are claimed by the framework (`.DAT` = "Diffraction profile")~~ | - | **retired**: modules register their own loaders from their front doors. The scientific module claims `.DAT` as a diffraction profile; the framework registers the same reader as the field-neutral two-column format, and `.DAT` with it only when no module has claimed it (`RegisterAllDataLoaders`) |
| A5 | ~~The framework links and names concrete model types~~ | - | **retired for the peak and background shapes (2026-10-16)**: they are the scientific module's (`Modules/scientific`), which free Fit links and a product for another field leaves out. The engine builds every registered type through the registry, and the automatic run's background is a capability (`IsDefaultBackground`). **And for the user's formula (2026-10-17)**: the engine asks whether a type is a formula (`IsDefinedByFormula`) and hands it one (`ApplyFormula`), so the formula type is the scientific module's too, and the framework ships no curve type at all. **And for the user-curve menus, their dialogs and the saved formulas (2026-10-17)**: the module adds its entries to its own group of the Curve Type menu through a framework seam (`curve_type_menu_extension`), and the dialogs are the client's windows linked beside the module (`Desktop/ScientificForms`). The core keeps only the generic configuration bases (`Desktop/ModelCurves/UserPointsSet`) |
| A6 | The framework's words are one field's words ("Characteristic Points of Peak") | clarity in every field | vocabulary supplied by the active module. The axis titles and the pointer's readout already come from the axis modes in force, so "Intensity" is diffraction's word only where diffraction says it |
| A7 | Workflow commands are fixed (background, positions, intervals) | fields whose workflow differs | generic operations opted into by capability - **intervals and positions done** (the Fit intervals and Automatic placement seams above). The Background menu and Residual Spectrum entries are still fixed in the window, and are to be offered by capability too. **A build that can never use a command no longer shows it** (2026-10-16, `Desktop/build_capabilities.pas`): with no background shape registered, Model > Background and its Tools-pane rows are hidden; with no type placed at curve positions, Model > Curve Positions; with no axis that reads a wavelength, Set Rule Parameters. Greyed still means "not for this model" (`action_state`) |
| A8 | Data is one 1-D profile | global fits, 2-D data, several response channels | recorded as the largest known limit; not staged, because it changes the engine contract |

### Field probes

A seam is not general because it avoids a field's name; it is general when
fields it was not designed for can use it. Every seam is checked against these:

| Field | Data | Models | Rules | Computations |
|---|---|---|---|---|
| Diffraction | intensity vs 2θ | peak profiles | - | background search |
| Technical analysis (wave counts) | price vs time | wave patterns | the wave grammar | detection |
| Chromatography | signal vs retention time | EMG peaks | resolution limits | peak integration |
| Enzyme kinetics, dose-response | rate vs concentration | Michaelis-Menten, Hill | positivity | IC50 |
| Decay, fluorescence lifetime | counts vs time | exponential sums | component count | deconvolution |
| Astronomy light curves | flux vs time | transit models | physical bounds | period search |

**How two probes use the report seam.** Chromatography: one section per
adjacent pair of peaks, a finding "resolution Rs" measured `1.2`, limit
`at least 1.5`, status Warning, linked to the resolution rule's explanation and
to the later peak's row. Enzyme kinetics: one section per fitted curve, findings
"Km > 0" and "Vmax > 0" as Pass or Fail, and the Hill coefficient as Info. Only
one module uses the seam today, which is why it is opt-in: a module returns ''
from `ReportCaption` and no tab exists.

**How two probes use the automatic-placement seam.** Astronomy light curves: a
period search finds the transits, `SearchFitIntervals` answers one window around
each and `PlaceInstancesFromData` places a transit model in each window that has
none. Decay: a component-count search answers the whole record as one interval
and places as many exponentials as it finds. Neither names the other's field, and
both leave what the user already placed alone.

## What the framework keeps

Moving a field out is decided piece by piece, and each piece is classed as
**field-specific** (it leaves for the field's module), **generic** (it stays,
renamed only where its words are one field's) or **mixed** (generic in shape but
carrying one field's assumption: the assumption leaves for the module, the shape
stays). What diffraction's move left in the framework, as generic:

- background points - the point set, its verbs and its place in the project;
- curve positions, and fit intervals;
- smoothing;
- the R-factor normalised by the observed integral (`fit_loss.RegisterLoss`;
  `docs/contributing/loss-functions.md` says why it divides by that);
- the sidecar's lineshapes (`Worker/py/lineshapes.py`).

The automatic background proposal and the peak search are mixed pieces that
went the other way: their arithmetic is the scientific module's, and the engine
asks for them by name through a registry (`background_algorithms`,
`peak_search_registry`), refusing in words in a build without the module.

## The criteria a new seam must meet before it lands

A field - diffraction was the first - is the first proof of a seam, never its
design driver: a seam shaped around what one field needs is that field's
assumption moved, not retired.

1. **It is stated without naming any field.** A doc sentence or test that needs
   the name of one field to explain the seam means the seam is wrong.
2. **At least two probes above can use it as written**, and this page shows how.
3. **What it contributes explains itself** through the explanation registry.
4. **A completeness test walks its registry**, so a module that half-implements
   it fails by name.
5. **It reuses the existing client/server verbs** where they exist - read
   `Desktop/http_fit_service.pas` before adding one.

### The data source seam, against those criteria

The newest seam, as a worked example of the five above.

1. **Stated without naming a field.** "Where data is fetched from" - a source
   declares an id, a category, the questions it asks, the extensions it
   produces and whether it needs a network. Nothing in it mentions a field, a
   service or a module.
2. **Two probes use it as written.** *Technical analysis*: a private pack
   registers a source for the economic series a wave count is drawn on.
   *Spectroscopy*: a public module registers one for a spectrum library, and
   the JCAMP-DX loader that reads what it fetches. Neither needed a framework
   change, and the framework names neither.
3. **What it contributes explains itself.** Every source carries a topic with
   its terms, its limitations and its sources; the wizard shows it beside the
   source.
4. **A completeness test walks the registry.** `data_source_findings` reports a
   source with no topic, a topic that resolves nowhere, a query field with no
   caption, and - the one that matters most - a source producing a kind of file
   this build has no reader for, which would otherwise be a dead end the user
   met after searching and downloading. `-Task check-ui` asks the running
   application the same question, because registration is a property of
   start-up that no test inside another binary can answer.
5. **It reuses the existing verbs.** A fetched file goes through
   `TFitClient.LoadDataSet` and `PUT /problems/{id}/profile`, exactly as File >
   Import Profile does. **No REST verb was added**: downloading happens in the
   client, so the server learns nothing about data sources at all.
   A source fetches only through the `IWebClient` it is constructed with
   (non-negotiable 11).

### The axis mode seam, against those criteria

1. **Stated without naming a field.** "A way of showing a coordinate": an id,
   a caption, a topic, which coordinates it can show and what parameter it
   reads, plus a factory for the axis. What decides which one is shown - the
   model's curves, then the data, then what the curves assume when the data
   says nothing, then the selection over an empty model - is stated in terms of
   those sources alone (`axis_choice`).
2. **Two probes use it as written.** *Technical analysis*: the price loader
   declares its argument as a bar number or a date and its value as a price,
   and the pack's patterns prefer a price for their value and assume bars when
   the data says nothing - so a wave count is captioned Bar (or Date) and Price,
   with no word of the field in the framework's rule.
   *Enzyme kinetics, dose-response*: the framework's own Logarithmic mode,
   offered for the argument, is the log-concentration axis such a curve is read
   against, and a module's curve type would prefer it by id. *Diffraction*, the
   first proof, registers its three angles and intensity through the same call.
3. **What it contributes explains itself.** Every mode names a topic; the
   entry's explanation is shown when the pointer rests on it, and the guide
   names every entry by its menu path.
4. **A completeness test walks the registry.** `AxisModeFindings` reports a mode
   with no caption, no topic, a topic resolving nowhere, or a required parameter
   it does not name; `EveryAxisEntryIsNamedByItsPath` walks the menus' entries
   against every registered chapter; and the curve-type and loader walks refuse
   a preference for a mode nobody registered, which the rule would otherwise pass
   over in silence.
5. **It reuses the existing verbs.** **No REST verb was added.** Axes are the
   client's; the curve types' preferences are class functions the client
   already has, a curve's type is read from its title as the Model panel reads
   it, and what the data said travels in the project's working context beside
   the choices themselves.

### The drawn-baseline seam, against those criteria

1. **Stated without naming a field.** "What a curve's points are drawn on, given
   the model": a curve that is one component of a sum, and whose contribution
   is a deviation from other components, says what it rests on, and the chart
   draws its value plus that. Nothing in the framework knows why.
2. **Two probes use it as written.** *Technical analysis*: a nested wave pattern
   is its own wiggle about its parent's leg; it rests on its parent chain, so
   its line passes through the stars the overlay already places there.
   *Diffraction and chromatography*: a peak fitted on a background is drawn on
   that background rather than on zero, the way the field reads a pattern.
   *Kinetics with a decomposed decay* is the same shape: one component drawn
   on the rest.
3. **What it contributes explains itself.** The Graphs tab's explanation says
   a curve may be drawn on what it rests on, and that its own values are what
   the tables and the fit report.
4. **A completeness test walks its registry.** `TDrawnBaselineRegistryTest`
   asks every registered curve type inside a model and fails by name for an
   answer that is neither nothing nor one value per point. The server also
   refuses to send one out of step (`TFitService.CurveDrawnBaseline`), because
   the reader refuses the whole curve over it.
5. **It reuses the existing verbs.** **No verb, resource or DTO was added.**
   The baseline is an optional field of the points every curve already sends
   (`GET /curves/{cid}/points` and each animated frame), beside the `ids` field
   it copies the rules of. Absent is zero, so every existing reply is
   byte-identical. Both server paths convert through one function
   (`wire_point_sets.PointsDataOf`), so the field reaches both or neither.

**Rejected:** drawing the line from the module's overlay, as a series of its
own. The framework's own near-zero line would still be drawn beside it, and an
overlay is not redrawn while a fit runs, so the line would freeze for the
whole fit.

## Composing modules into one build

A module directory declares itself with a **manifest**, `module.json`: its
name, the directories its units and tests are in, and its front doors for the
client, the compute server, the client's own windows (`forms`) and its tests. A
product's identity lists the modules it links (`build.modules`, or `"all"`), and
the build writes the four composition units - `app_modules`, `server_modules`,
`client_modules` and `module_tests` - from those manifests into the product's
folder, first on its search path (`tools/build-lib/product_modules.ps1`, which
says why the list is not kept in Pascal by hand). So one build links as many
modules as its product names.

Not done yet: the leak gate's identifiers and the Python coverage lists are not
read from the manifests, so `tools/` still names the modules they cover.

## Public or private

A private module lives in a repository that is never published; the leak gate
keeps its identifiers out of every published tree. A public module is published
from its own repository by the same means the framework is. Neither is a
different kind of module.

## The smallest complete module

`Modules/example-linear/` is the template for any field, and is where each new
seam is first exercised in its smallest form. It shows:

- a curve type with its explanation;
- an explanation provider of its own, with one rule stated as this software's
  choice (`esModelChoice`) - a rule need not be canonical to be explained;
- a UI module declaring one menu entry, whose Topic explains it before it is
  chosen and whose command puts an explanation in front of the user.

Its suite is its own project - `lazbuild --widgetset=nogui
Modules/example-linear/fit_tests_example.lpi`, then `tests/fit_tests_example
--all --format=plain` - and this repository's tooling builds and runs it, so a
template that stopped compiling or passing would be seen.

What it does **not** show, and why: a row menu, because only the module whose
rows fill the Model panel is asked for one and the example places ordinary
curves, whose rows are the framework's; and a module resource, because a linear
ramp needs no per-problem state. The technical-analysis pack is the worked example of both.

A module that fills the Model panel names it by its types' `PlacedByPointSet`
(`IUiModule.PanelId`). The window keeps the module's latest push and shows it
whenever one of its rows stands for a curve the model holds (`CurveId`),
whatever curve type is selected, followed by the framework's rows for any curve
no row of it stands for (`model_outline.ComposePanel`). Rows that name no curve
of the open model are not shown, so a module does not have to clear them when a
project is closed. A row that stands for a curve must carry its handle in
`CurveId`, or the panel cannot tell that it describes the model. The panel describes the model, and the selected
type only says what will be placed next. An empty push describes nothing and
never displaces the framework's rows: a module redraws after every fit, and an
empty redraw once replaced another model's rows and the selection on them.
