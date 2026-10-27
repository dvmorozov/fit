// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Whether the surfaces built from one command table agree with each
other.)

WHAT THIS CATCHES THAT NOTHING ELSE CAN. A command is declared once and drawn
three times - the menu bar, the Tools pane, the Model panel's context menu - and
the failure mode is not a line of code being wrong. It is two widgets
disagreeing: a command live in the pane and refused in the menu, a button with
no explanation where the menu entry has one, one button drawn wider than the
rest. Every one of those is a correct program in a state nobody chose, and no
headless suite can see it, because the widgets are the evidence.

MEASURED IN THE WINDOW, JUDGED HERE. Reading a button's width and an action's
Enabled needs the widget set; deciding whether two readings disagree does not.
The window gathers the pairs and this says which are wrong, in the words the log
will carry - so the rules can be read, and tested, without a display.

THE FINDINGS ARE SENTENCES, not codes. They go to the log and a person reads
them after a build fails, so each one says what disagreed and what the two
values were - "enabled in the pane, disabled in the menu" is actionable and
"consistency violation 3" is not.
}
unit ui_selfcheck;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, series_style,
    //  The rule a measured button is judged against, asserted from the unit
    //  that decides the spacing rather than restated here - a check with its
    //  own copy of the number passes a window the decision never reached.
    toolbar_metrics,
    //  And the Model tab's title against the panel's own rule.
    model_outline,
    //  The level a verdict's line is written at, and the history the R-factor
    //  kept check picks its model from.
    log, model_history;

type
    { One command, as each surface currently shows it. Gathered by the window
      from the widgets themselves - not from the table that generated them,
      which would make this check assert that the table equals itself. }
    TSurfaceRow = record
        { The command's id, for naming it in a finding. }
        Id: string;
        { What the pane's button reads, and how it is drawn. }
        PaneCaption: string;
        PaneHint: string;
        PaneEnabled: boolean;
        PaneWidth: longint;
        { Whether this row has a menu entry to be compared with at all.

          THE FRAMEWORK'S ROWS DRIVE AN ACTION, and the action is the menu side.
          A module's row drives none, but it does have an entry - the one it
          declared under Model - and the window reads that instead, because a
          pane button and a menu entry generated from one declaration are
          exactly the pair worth watching. False is left for a row the menus
          never drew, which has nothing to disagree with. }
        HasMenuSide: boolean;
        MenuHint: string;
        MenuEnabled: boolean;
        { Whether this row enters a picking mode, and whether its button is
          currently held down.

          A PICKING BUTTON IS A LATCH: it stays pressed while its own mode runs,
          which is the one thing a menu entry cannot show - the menu says start
          or stop instead. So the button's pressed state is the only place that
          claim is made, and a latch left down over a mode that has ended says
          the program is waiting for clicks it will ignore. }
        HasPicking: boolean;
        PaneDown: boolean;
        ModeSaysDown: boolean;
        { Whether this row's pressed state is also said by a tick on its menu
          entry, and what that tick currently says.

          A MODULE'S TOGGLE IS DRAWN TWICE and written by two paths: the module
          ticks its entry through IUiHost, and the pane's button follows the
          command table. One saying a mode is on while the other says it is off
          is the program contradicting itself, and whichever the user looks at
          first decides what they believe. }
        HasMenuTick: boolean;
        MenuChecked: boolean;
    end;

    TSurfaceRows = array of TSurfaceRow;

    { One row of the legend, and the series it is supposed to name. }
    TLegendRow = record
        { What the row reads. }
        Text_: string;
        { Whether the row carries a series at all. The row's object IS the
          series, which is what makes the pairing exact rather than positional. }
        HasSeries: boolean;
        { Whether that series is still on the chart. }
        SeriesOnChart: boolean;
        { What the series calls itself, which must be what the row reads. }
        SeriesTitle: string;
    end;

    TLegendRows = array of TLegendRow;

    { One line of the pointer's readout beside the chart, as the window drew
      it: the caption the axis in force gives it, and the value beside it. In
      pixels of the panel both sit on. }
    TReadoutRow = record
        Caption: string;
        CaptionRight: longint;
        { Where the value's label starts, and how wide it is. }
        ValueLeft: longint;
        ValueWidth: longint;
        { The value written in it, and how wide that text is in its font. }
        Value: string;
        ValueTextWidth: longint;
    end;

    TReadoutRows = array of TReadoutRow;

    { What the Model panel's context menu did over the two kinds of row that can
      name a curve, as the window found it after driving a model into existence.

      TWO ROWS, NOT ONE, and that is the whole point of the record. The framework
      builds rows for its own curves and a pack builds rows for its patterns; the
      second kind identifies itself by the contributor's own markup and carries
      the curve handle BESIDE that id rather than as it. The window used to
      answer "which curve is selected?" from the row id alone, so every pack row
      answered "none" and Delete curve was dead over every pattern in a wave
      count - while the same check went on passing over the framework's rows.
      Judging the two together is what makes that visible. }
    TRowCommandReach = record
        { How many entries the context menu has at all. Zero is its own finding:
          a menu with nothing on it cannot be told from one whose entries are
          merely disabled. }
        EntryCount: longint;
        { Whether any entry was live over the framework's own row. }
        OfferedOnOwnRow: boolean;
        { The curve handle the check put on the contributor row, or '' when the
          model it built reported none - in which case the contributor half was
          not checked and must say so rather than pass. }
        ContributorCurveId: string;
        { What the window read as the selected curve once that row was picked.
          Anything but ContributorCurveId is the defect itself. }
        SelectedCurveId: string;
        { Whether any entry was live over the contributor row. }
        OfferedOnContributorRow: boolean;
        { How many entries a module added to the menu over the contributor
          row, as it was actually opened. They count toward the menu having
          entries at all: a menu whose only entries are a module's is not
          empty. }
        ModuleEntryCount: longint;
        { Every topic an entry named that resolves to no explanation. An entry
          promising an explanation the pane cannot show is a lesson that
          leads nowhere. }
        UnresolvedTopics: array of string;
        { How many entry topics were looked up, resolved or not - so the
          verdict can say the lookup ran at all. }
        TopicsChecked: longint;
    end;

    { WHAT A PANE'S OWN ZOOM BUTTONS DID, read off the real window after each
      was clicked.

      WHY A WINDOW IS NEEDED FOR THIS AT ALL. The ladder is arithmetic and is
      tested as arithmetic; what no headless suite can see is whether the
      button on the pane is wired to it - whether the strip was built with both
      of its buttons, whether a click reaches the size the pane is drawn at,
      and whether the button greys when there is nowhere left to go. Every one
      of those is a correct program that does nothing when clicked. }
    TZoomReading = record
        { The pane, as a finding would name it. }
        Pane: string;
        { How many buttons its strip carries. Two, or the pane has a strip that
          cannot do what it is for. }
        Buttons: longint;
        { The size it was drawn at before anything was clicked, after Zoom In,
          and after Zoom Out again. }
        Before: longint;
        AfterIn: longint;
        AfterOut: longint;
        { THE OTHER PANE'S SIZE ACROSS THE SAME CLICKS. The whole point of two
          strips is that each moves its own pane; a shared number would be
          invisible here unless it is read. }
        OtherBefore: longint;
        OtherAfter: longint;
        { Whether Zoom In was still offered once the top of the ladder was
          reached, and Zoom Out at the bottom. }
        OfferedInAtTop: boolean;
        OfferedOutAtBottom: boolean;
    end;

    TZoomReadings = array of TZoomReading;

    { ONE BORDER THE USER CAN DRAG, read off the real window after its pane was
      given a larger remembered size - the road every session after the first
      opens by - and its splitter then moved the way a mouse drag moves it.

      WHY THE RESTORE COMES FIRST. A splitter resizes whichever control lies
      next to it on the side it faces, found afresh on every drag from where
      the controls are - not one it was told. The table's border stopped
      dragging only once a remembered height had been applied: that put the
      tables below the status bar, and from then on the splitter found the
      status bar. A window as designed showed nothing wrong. }
    TSplitterReading = record
        { The part it gives room to, as the layout names it; '' for a
          splitter that gives room to no part the layout remembers. }
        Pane: string;
        { The splitter that resizes the pane as designed; '' when none does. }
        Splitter: string;
        { The pane's control, and what the splitter resizes once the pane has
          been given a remembered size; '' for nothing. }
        PaneControl: string;
        ResizedAfterRestore: string;
        { How far the pane was asked to shrink by a drag, and its size before
          and after it. }
        Dragged: longint;
        SizeBefore: longint;
        SizeAfter: longint;
        { The window made small ('720 x 420'), and what the splitter resizes
          in it; '' when that was not measured. }
        SmallWindow: string;
        ResizedInSmallWindow: string;
    end;

    TSplitterReadings = array of TSplitterReading;

    TUiFindings = array of string;

    { WHETHER THE CHART SHOWS THE SELECTION, read off the series the chart holds
      at one moment - after a row was selected, after a replot, after the
      selection was cleared. Counted from the series themselves rather than
      from what the viewer was asked, so a replot that loses the highlight is
      what gets read. Filled series by series by CountSeriesHighlight. }
    THighlightReading = record
        { True where a curve is meant to be selected; False where none is, and
          then any highlight at all is the finding. }
        ExpectSelection: boolean;
        { The curve the window holds as selected, and the one the Model panel's
          selected row stands for. They must agree: the panel's commands act on
          the first, and the user reads the second. }
        CurveId: string;
        PanelCurveId: string;
        { The curves the window highlights: the selected row's own and those of
          the rows nested under it (model_outline.CurvesHighlightedByRow), or
          the one curve chosen where no row stands for it. A series counts as
          the selection's when it was drawn for any of them. }
        CurveIds: TStringArray;
        { How many curves the gesture's row stands for, where the check knows
          it; 0 where it does not. A row whose nested curves were not brought
          along highlights its own curve perfectly, so only this can see it. }
        ExpectedCurveCount: longint;
        { The curve the gesture chose, where the check knows it; '' where it
          does not. A selection that did not follow the gesture leaves every
          count consistent - with the old curve - so only this can see it. }
        ExpectedCurveId: string;
        { Series drawn for any of CurveIds; how many of them read as
          highlighted; how many series drawn for anything else show any
          emphasis at all. }
        OwnedCount: longint;
        OwnedHighlighted: longint;
        OtherHighlighted: longint;
    end;

    { One toolbar, as the running window measured it.

      WHY A TOOLBAR IS WORTH MEASURING AT ALL. Left at the widget set's own
      metrics a toolbar draws a button barely larger than the icon in it and
      starts the first one on the bar's edge: the icons run together in one
      block with no boundary, and a pointer aimed between two of them still
      hits one. The spacing is decided once (toolbar_metrics) and applied to
      every bar there is, so what can go wrong is not the arithmetic - it is a
      bar the application builds somewhere this never reaches, which looks like
      an ordinary toolbar while being the one thing on the window nobody sized.
      Only the window can tell them apart, because only there do the bars exist.

      THE SMALLEST BUTTON, not each of them: a bar is spaced or it is not, and
      one finding per bar is what a reader can act on. }
    TToolBarReading = record
        { The bar's component name, for naming it in a finding. }
        Name: string;
        { Whether it is parented at all. A bar built and never attached is
          invisible, and nothing else reports it. }
        Attached: boolean;
        { How many buttons it holds. }
        ButtonCount: longint;
        { The smallest button on it, in each direction. }
        SmallestButtonWidth, SmallestButtonHeight: longint;
        { The white space between its outermost button and its own edge. }
        Margin: longint;
        { How far its first button is pushed in beyond that margin.

          ON ONE EDGE ONLY, which is the whole problem: a TToolBar's indent
          applies to the leading edge and to nothing else, so any of it on top
          of the margin makes that side twice the other three. }
        LeadingIndent: longint;
        { Whether it still draws its own border. A TToolBar draws one on all
          four sides by default, and while its buttons sat against that border
          it read as the frame around them; with the margin between the two it
          is a rule above the first button with nothing under it. The margin is
          the boundary, so the border is one boundary too many. }
        DrawsEdgeBorder: boolean;
        { Whether it draws every button raised at rest - that is, is not Flat.
          The LCL mimics a raised button by drawing the HOT state all the time;
          on Cocoa that is a rounded fill over the whole button, the buttons
          touch, and a row of icons reads as one grey pill. Flat is also what
          every current desktop guideline draws a toolbar with: no frame at
          rest, the platform's own highlight under the pointer. }
        RaisedAtRest: boolean;
    end;

    TToolBarReadings = array of TToolBarReading;

    { One control that can show an on/off state - a toolbar button or a menu
      entry - and whether the command behind it has one.

      A SURFACE MAY NOT CLAIM A STATE ITS COMMAND DOES NOT HAVE. A check-style
      button or a self-ticking menu entry over a command that just does
      something presses or ticks itself on every second use, saying something
      is on when nothing is. Zoom Out was such a button for years; the raised
      drawing hid it, and a flat bar would have shown it stuck down. }
    TStateReading = record
        { The control's component name, for naming it in a finding. }
        Name: string;
        { Whether the control toggles its own pressed or ticked look. }
        ShowsState: boolean;
        { Whether the command it runs is itself a toggle or a radio. }
        CommandHasState: boolean;
    end;

    TStateReadings = array of TStateReading;

    { What the window saw of itself while a real fit ran.

      TAKEN IN THE RUNNING APPLICATION, because this is the one question the
      headless suites cannot answer. They drive TFitClient directly and assert
      that it asks the engine and hands frames to a viewer; whether the WINDOW's
      timer fires while a fit occupies another thread, and whether what it draws
      reaches the chart, is a fact about a real event loop over a real socket.
      The feature was reported broken by its user twice while every one of those
      suites was green, which is what a check over real widgets is for. }
    TLiveProgressReading = record
        { Whether the fit was entered and left at all. }
        Ran: boolean;
        { Whether Animation Mode was on for it. }
        Animating: boolean;
        { How long it took, in seconds. }
        Seconds: double;
        { How many times the window was handed a progress view to draw. }
        Frames: longint;
        { The most points the loss chart ever held. }
        LossPoints: longint;
        { How many times the MODEL was redrawn from a frame - the curves
          moving, which is what Animation Mode means. }
        Curves: longint;
        { How often the window's own timer fired during it. Separates a window
          that never asked from one that asked and was not answered. }
        Ticks: longint;
        { Whether a module's rows were pushed while the fit ran - only in a
          build with a module that declares a panel - whether the panel was
          drawn from them then, and whether it was once the fit had ended. }
        PanelPushChecked: boolean;
        PanelDrawnDuringFit: boolean;
        PanelDrawnAfterFit: boolean;
    end;

const
    { HOW SHORT IS TOO SHORT to place picks in. Two picks and an interval have to
      land on real samples, and the thirds this check picks them at have to be
      distinct - so a handful of points is not a profile for this purpose. }
    MinProfilePointsForPicks = 8;

    { HOW SHORT IS TOO SHORT TO WATCH. A fit that converges before the window's
      first tick showed nothing because there was nothing to show, and failing a
      build over a fast machine would teach everyone to ignore this check. }
    MinFitSecondsToWatch: double = 0.4;

{ Every way the pane and the menus disagree about the rows they were both built
  from. Empty when they agree, which is the answer a passing build wants. }
function SurfaceFindings(const ARows: TSurfaceRows): TUiFindings;

{ Every way the legend disagrees with the chart it describes. Empty when they
  agree.

  WHY THIS IS CHECKED AT ALL. The legend's rows and the chart's series were
  index-parallel by luck of construction: rows are appended only while the
  legend is being updated, and the redraws during a running fit switch that off,
  so after the first fit row i named one series and controlled another. Ticking a
  row then hid a curve the user was not pointing at. The rows carry their series
  as an object now, and this is what keeps that true. }
function LegendFindings(const ARows: TLegendRows): TUiFindings;

{ The sentence for a build with no module that reports, which therefore has no
  report tab and no strip on one.

  SAID, NEVER SKIPPED SILENTLY - the rule RowCommandBlocked states: a check that
  quietly does nothing reads exactly like a check that passed, and which modules
  a build links is precisely the kind of thing that changes underneath one. }
function NoReportTabToZoom: string;

{ What each pane's zoom buttons did wrong, if anything. }
function ZoomFindings(const AReadings: TZoomReadings): TUiFindings;

{ Every toolbar that was left unspaced, empty or unattached, judged against the
  smallest button a pointer can be expected to hit and the smallest margin that
  reads as a boundary - both handed in already scaled for the display, because
  what a scaled pixel is belongs to ui_scaling. }
function ToolBarFindings(const AReadings: TToolBarReadings;
    AMinimumButton, AMinimumMargin: longint): TUiFindings;

{ The toolbar check's verdict line, whatever the outcome. }
function ToolBarSpacingSummary(ABarCount, AFindingCount: longint): string;

{ Every control that toggles its own look over a command with no state. }
function StateFindings(const AReadings: TStateReadings): TUiFindings;

{ The state check's verdict line, whatever the outcome. }
function StateSummary(AControlCount, AFindingCount: longint): string;

{ The zoom check's verdict line, whatever the outcome. }
function ZoomSummary(APaneCount, AFindingCount: longint): string;

{ The legend's verdict line, whatever the outcome. }
function LegendSummary(ARowCount, AFindingCount: longint): string;

{ What is wrong with the pointer's readout, if anything: values that do not
  start in one column, a caption running into its value, a value its label
  cuts off.

  WHY IT IS CHECKED. The captions are the axes' names now - "Position:",
  "Price:", "Sin(Theta)/Lambda:" - so their widths change with the data, and a
  readout laid out for "Intensity:" was reported with its price cut off. }
function ReadoutFindings(const ARows: TReadoutRows): TUiFindings;

{ The readout check's verdict line, whatever the outcome. }
function ReadoutSummary(ARowCount, AFindingCount: longint): string;

const
    { How many times the chart may still be resized in the idle period: the
      last echo of the layout that has just settled. A window caught in a
      layout loop resizes it several times a second. }
    IDLE_RESIZE_ALLOWANCE = 1;

{ A WINDOW LEFT ALONE STOPS LAYING ITSELF OUT, or '' when it did.

  The window once never stopped: the LCL and Qt6 kept correcting each other's
  sizes for the bottom tabs about seven times a second, in every session, cheap
  enough to go unseen until a model of five hundred curves made every round
  redraw all of them and the window looked hung. Only the running window can
  show it, which is why it is measured under --check-ui. }
function IdleLayoutFinding(AResizes, AMilliseconds: longint): string;

{ Whether the Model tab, read off the running window as AShown over a model
  that holds curves of AModelOwner's types, says whose model it is - the
  panel's own rule (model_outline.ModelPanelCaptionFor). '' when it does. }
function ModelTabFinding(const AShown, AModelOwner: string): string;

{ The idle-layout check's verdict line, whatever the outcome. }
function IdleLayoutSummary(AResizes, AMilliseconds: longint): string;

{ Every border that does not resize its pane, and every pane with no border. }
function SplitterFindings(const AReadings: TSplitterReadings): TUiFindings;

{ The splitter check's verdict line, whatever the outcome. }
function SplitterSummary(ACount, AFindingCount: longint;
    const ASmallWindow: string = ''): string;

const
    { How long the window may take to come back after Stop: the cycle the
      engine is in, and taking in what the run reached. 2.1 s over 534 curves
      in the VM once each curve was read on its own (TFitService.
      CurvePointsCopy); 5 s before that, 55 s before the stalls went - so a
      return of either is a finding. }
    STOP_RETURN_ALLOWANCE_MS = 4000;

{ THE WINDOW COMES BACK WHEN STOP IS PRESSED, or '' when it did.

  Measured from pressing Stop to the window having taken in the result - which
  is what the user waits for, and not the same as the engine stopping: the fit
  ended within a second and the window was still busy fifty seconds later. }
function StopFinding(AMilliseconds: int64): string;

{ The one-line verdict the task reads, whatever the outcome.

  ALWAYS WRITTEN, and that is deliberate: a check that logs only when it finds
  something is indistinguishable from a check that did not run, and the task
  cannot tell the difference either. The layout check states its count the same
  way. }
function SurfaceSummary(ARowCount, AFindingCount: longint): string;

{ Why the row-command check cannot run, or '' when it can.

  SAID, NEVER SKIPPED SILENTLY. This check has several reasons to bow out before
  it has looked at anything - no file, too little of one - and a check that
  quietly does nothing reads exactly like a check that passed. So the reason is a
  sentence the caller logs, and the caller bows out only when there is one. }
function RowCommandBlocked(ADataFileOpen: boolean;
    AProfilePoints: longint): string;

{ The finding for a model that produced no row naming a curve, which is a bow-out
  of its own: the panel filled, and nothing in it is what the menu is about. }
function NoRowNamesACurve: string;

{ Every way the Model panel's context menu is unusable over a row that names a
  curve. Empty when it can be used over both kinds of row. }
function RowCommandFindings(const AReach: TRowCommandReach): TUiFindings;
{ Notes one context entry's topic: counted, and listed among the unresolved
  ones when AResolves is False and it is not listed already. An entry with no
  topic is not noted. }
procedure NoteEntryTopic(var AReach: TRowCommandReach; const ATopic: string;
    AResolves: boolean);
{ The verdict for the framework's own rows, whatever the outcome: how many of
  the menu's own entries were offered, how many a module added over a
  contributor row, and how many entry topics were looked up and how many of
  them resolve to nothing - the last two said even when zero, so a log shows
  that those halves ran. The contributor rows have a line of their own
  (ContributorRowSummary), because they are a separate defect and a single
  count would hide one behind the other. }
function RowCommandSummary(const AReach: TRowCommandReach): string;
function ContributorRowSummary(AOffered: boolean; AEntryCount: longint): string;
{ Adds one series of the chart to a reading: its owner, and how it is drawn.
  A series drawn for any of the selected curves (CurveIds) counts as
  highlighted only when it is
  wide AND on top (SeriesDrawnHighlighted); any other series counts against
  the chart when it is either, since half a highlight on the wrong curve still
  points the user at it. }
procedure CountSeriesHighlight(var AReading: THighlightReading;
    const AOwnerCurveId: string; ALineWidth: longint; AOnTop: boolean);

{ What is wrong with a reading, each finding beginning with AWhen - the moment
  it was taken - so a log line says which gesture lost the highlight. }
function HighlightFindings(const AReading: THighlightReading;
    const AWhen: string): TUiFindings;

{ Why choosing a curve in the Curve Attributes table was not checked: the table
  held no row naming a curve other than the one already selected. }
function NoTableRowToSelect: string;

{ A reading's verdict, written whether or not anything is wrong, so a log with
  no finding can be told from a check that never ran. }
function HighlightSummary(const AReading: THighlightReading;
    const AWhen: string): string;

{ Every way a real fit went by without the window showing it. Empty when the
  user would have seen it happen.

  The two modes promise different things and are judged differently: the default
  promises a loss chart that fills as the fit runs, and Animation Mode promises
  the model's own curves moving over the data. }
type
    { WHAT THE DATA SOURCES LOOK LIKE IN THE RUNNING BINARY.

      WHY THIS CANNOT BE ASKED ANYWHERE ELSE. Registration is a property of the
      BUILD and of the start-up sequence: the test suite links every source unit
      and registers them itself, so an assertion there is true by construction
      and would stay true in an application whose start-up never made the call.
      That exact defect - a whole vertical registered in the test binary and
      absent from the running one - is in findings.md more than once.

      So the running window is asked: how many sources it has, whether they are
      complete by the same walk the suite uses, and whether the command that
      reaches them is really on the File menu. }
    TDataSourceReading = record
        { How many sources the running application has. }
        SourceCount: longint;
        { How many of them need no network - the ones usable offline. }
        OfflineCount: longint;
        { The completeness walk's report, empty when the build is sound. }
        Findings: string;
        { Whether the menu command that opens the wizard exists and is enabled
          in the window as built. }
        CommandFound: boolean;
        CommandEnabled: boolean;
    end;

{ What is wrong with the sources this running build has, if anything. }
function DataSourceFindings(const AReading: TDataSourceReading): TUiFindings;

{ The verdict line, written whatever the outcome. }
function DataSourceSummary(const AReading: TDataSourceReading): string;

function LiveProgressFindings(const AReading: TLiveProgressReading): TUiFindings;

{ The verdict line, written whatever the outcome - including the one saying the
  fit was too short to be judged, since silence would read as a pass. }
function LiveProgressSummary(const AReading: TLiveProgressReading): string;

type
    { WHAT THE DIFFERENCE STRUCTURE STRIP DREW after a fit: it is
      drawn by the window alone, from what the window holds, on the model's
      chart's x and marks - joins no unit test reaches. }
    TStructureStripReading = record
        { The samples of the fit intervals the client judges; 0 for none. }
        Samples: longint;
        { The points of the strip's curve and of its level line, and of the
          difference drawn on its right axis. }
        Points, LevelPoints, DifferencePoints: longint;
        { The x range the strip shows, and the model's chart's - after the
          model's chart was zoomed in. THE STRIP SHOWS THE MODEL'S X, zoom and
          scroll included; only its values are never zoomed (the user,
          2026-10-18, on the rule of 2026-10-15). }
        StripLo, StripHi, ModelLo, ModelHi: double;
        { Where each chart draws its vertical grid lines, in x. }
        StripMarks, ModelMarks: array of double;
    end;

function StructureStripFindings(const AReading: TStructureStripReading): TUiFindings;

type
    { WHAT THE RESIDUAL SPECTRUM TAB DREW: the map is asked cell by
      cell by the colour-map series, from a grid the window took, on an x tied
      to the model's chart - joins only the running window has. }
    TSpectrumMapReading = record
        { True when no module of this build offers the Residual Spectrum
          (chart_tabs.OfferChartTab): there is no map, and the verdict says so
          rather than reporting the build. }
        NotInBuild: boolean;
        { The samples of the fit intervals; 0 for none. }
        Samples: longint;
        { The map's columns and rows, how many kinds of cell the legend under
          it names, and how many there are (the scientific module's
          difference_scalogram.TScalogramCell, which the core does not name). }
        Columns, Rows, KeyNamed, KeyKinds: longint;
        { The x range the tab shows, and the model's chart. }
        MapLo, MapHi, ChartLo, ChartHi: double;
    end;

function SpectrumMapFindings(const AReading: TSpectrumMapReading): TUiFindings;

type
    { WHERE A CHART'S PLOT RECTANGLE STANDS IN ITS TAB, left and right edges
      in pixels from the tab's left: the charts sharing the model's x must
      stand over each other (the user, 2026-10-14: "it looks ugly when charts
      are offset"). }
    TChartFrameReading = record
        Name: string;
        Left, Right: longint;
        { The chart control itself, from the tab's left: where a plot stands
          is the control's place plus the margins the chart keeps. }
        ControlLeft, ControlWidth: longint;
    end;
    TChartFrameReadings = array of TChartFrameReading;

    { ZOOMING THE MAP AS A USER DOES, and what the model's chart did: the
      dates the map was zoomed to and the ones the model's chart then showed,
      and whether the model's chart and the strip were still zoomed after the
      map was told to show everything (the user, 2026-10-14: "when I zoom out
      in the diagram the model chart is not zoomed out"). }
    TLinkedZoomReading = record
        Checked: boolean;
        { True when this build offers no Residual Spectrum to zoom with the
          model's chart. }
        NotInBuild: boolean;
        MapLo, MapHi, ModelLo, ModelHi: double;
        { After the map was shown whole: the model's chart still zoomed, and
          the map or the strip on an x other than the model's chart's. Not
          their IsZoomed: they are SET to the model's whole x, which TAChart
          counts as a zoom. }
        ModelZoomedAfterShowAll, OthersApartAfterShowAll: boolean;
        { The other way: the model's chart zoomed in with its tab in front and
          the map then on another x; the model's chart shown whole and the map
          then not. }
        MapApartAfterModelZoom, MapZoomedAfterModelShownWhole: boolean;
        { Scrolled with either tab's bar, the charts on another x each; the
          Residual Spectrum tab with no bar to scroll; the two bars' thumbs in
          different places or of different lengths. }
        ApartAfterScroll, SpectrumHasNoBar, BarsDisagree: boolean;
        { The map with no vertical scroll bar; zoomed in by the command with
          its tab in front, its wave lengths not zoomed, or not moved to the
          longest by that bar's thumb at the top (the user, 2026-10-15). }
        SpectrumHasNoYBar, MapYNotScrolled: boolean;
    end;

function LinkedZoomFindings(const AReading: TLinkedZoomReading): TUiFindings;

type
    { THE DATA AS OPENED, ON THE MODEL'S CHART, before any model: its x range
      and the one the chart shows once everything has painted. The strip under
      it, empty until there is a model, once lent the chart its own -1..1
      through the link, and every recording showed one bar of 1254 (the
      site's recordings, 2026-10-14). }
    TOpenedDataReading = record
        Checked: boolean;
        DataLo, DataHi, ChartLo, ChartHi: double;
    end;

type
    { A MODEL MADE CURRENT FROM THE HISTORY REDRAWS ITS R-FACTOR, as opening a
      project does - the same restore (the user, 2026-10-14): whether it
      could be made current, whether its entry is a fit's, the samples the
      entry keeps and the points the R-factor tab then draws. }
    TRFactorKeptReading = record
        Checked, MadeCurrent, EntryIsFit: boolean;
        Kept, Drawn: longint;
    end;

function RFactorKeptFindings(const AReading: TRFactorKeptReading): TUiFindings;
{ 'ui: R-factor kept: ...', whatever the outcome. }
function RFactorKeptSummary(const AReading: TRFactorKeptReading;
    AFindingCount: longint): string;
{ The model the R-factor kept check makes current: the last fit's, and one
  that is not current if there is one, so making it current restores it as
  opening a project does. -1 when no fit was recorded. }
function RFactorKeptEntry(AHistory: TModelHistory): longint;

type
    { A CLICK ON THE CHART, sent to it as the widget set sends a pointer's: the
      pointer moved over a sample of the data, the left button pressed and
      released there, in Curve Positions picking - then the same again. Every
      other test reaches a pick through the client, below the chart; this is
      the one that goes through the pointer, the crosshair that snaps it to a
      sample and the release that places the pick.

      Picks are counted from the set the mode collects into. Where the pick
      landed is said in pixels on the chart's x - the clicked sample's and the
      added pick's - because a dense profile puts several samples under one
      pixel, and the user aimed at the pixel. }
    TChartClickReading = record
        Checked: boolean;
        { Why there was nothing to click, when Checked is False. }
        NotCheckedWhy: string;
        { The menu entry started Curve Positions picking. }
        PickingStarted: boolean;
        { The picks before, after the first click and after the second. }
        Before, AfterFirst, AfterSecond: longint;
        { The added pick was found among the picks after the first click. }
        PickFound: boolean;
        ClickedPixelX, PickedPixelX: longint;
    end;

{ What is wrong with a click's reading: a click that added no pick or several,
  a pick away from the pixel clicked, a second click on it that did not take it
  back, and a menu entry that started no picking. }
function ChartClickFindings(const AReading: TChartClickReading): TUiFindings;
{ 'ui: chart click: ...', whatever the outcome. }
function ChartClickSummary(const AReading: TChartClickReading;
    AFindingCount: longint): string;

type
    { Positions in a list of menu entries. }
    TEntryOrder = array of longint;

{ The order the R-factor scale check switches View > R-factor Scale's entries
  in: only those on the menu bar - the chart's right-click menu holds the same
  modes again - and Linear first, because its points are the R-factor itself,
  which every other scale is judged against. AOnMenuBar and AIsLinear are one
  element per entry. }
function RFactorScaleOrder(const AOnMenuBar, AIsLinear: array of boolean): TEntryOrder;

type
    { Where a verdict's lines go: the log, in the window, where the build task
      reads them; a collector, in a test. A PARAMETER rather than a call to
      WriteLog here, so this unit stays records in and sentences out - and a
      test has no log file to read. }
    TUiLineWriter = procedure(const ALine: string; ALevel: TMsgType);

{ A check's verdict, written: each finding a Warning with 'ui: ' before it,
  then ASummary as a Notification - '' when the check writes its own summary
  later. ONE ROUTINE for every check, each of which had repeated the same
  three lines in the window. }
procedure LogUiVerdict(const AFindings: TUiFindings; const ASummary: string;
    AWrite: TUiLineWriter);

{ A finding when the chart does not show the data's whole x range. }
function OpenedDataFindings(const AReading: TOpenedDataReading): TUiFindings;
{ 'ui: opened data: ...', whatever the outcome. }
function OpenedDataSummary(const AReading: TOpenedDataReading;
    AFindingCount: longint): string;

{ 'ui: linked zoom: ...', whatever the outcome. }
function LinkedZoomSummary(const AReading: TLinkedZoomReading;
    AFindingCount: longint): string;

{ A finding for every chart whose plot does not start and end where the first
  one's does, to within a pixel. }
function ChartFrameFindings(const AReadings: TChartFrameReadings): TUiFindings;
{ 'ui: chart frames: ...' with every plot's edges, whatever the outcome. }
function ChartFrameSummary(const AReadings: TChartFrameReadings;
    AFindingCount: longint): string;

{ The verdict line, 'ui: residual spectrum: ...', whatever the outcome. }
function SpectrumMapSummary(const AReading: TSpectrumMapReading;
    AFindingCount: longint): string;

{ The verdict line, 'ui: difference structure: ...', whatever the outcome. }
function StructureStripSummary(const AReading: TStructureStripReading;
    AFindingCount: longint): string;

type
    { WHAT ONE ENTRY OF VIEW > R-FACTOR SCALE LEFT ON SCREEN, read after the
      window chose it through the menu's own handler and redrew the progress
      view the way a poll does.

      WHY THE WINDOW HAS TO BE ASKED. The user found, twice, that switching
      the scale did not do what the menu said to the line above the chart -
      first it changed nothing, then it gave both forms - with every unit test
      green: the menu, the axis, the client and the label are four places,
      joined only in the running window. }
    TRFactorScaleRow = record
        { The entry, as the menu reads. }
        Caption: string;
        { Whether that entry is ticked in the menu bar and in the progress
          chart's right-click menu, and how many other R-factor entries are. }
        TickedInMenu, TickedInPopup: boolean;
        OthersTicked: longint;
        { The chart's title as drawn, and the title of the axis now in force. }
        ChartTitle, AxisTitle: string;
        { What that axis draws (TCoordinateAxis.DrawnQuantityName). }
        DrawnName: string;
        { The last point the chart holds, and where the axis in force puts the
          last sample. }
        LastDrawn, Expected: double;
        { The line above the chart, as drawn. }
        Line: string;
        { The History tab's row for a fitted model, as the list holds it; ''
          when the history held none, which leaves the rows unjudged. The
          rows give the R-factor on the same scale (history_rows). }
        HistoryRow: string;
    end;
    TRFactorScaleRows = array of TRFactorScaleRow;

{ Everything the scales switched to disagree with the menu about. }
function RFactorScaleFindings(const ARows: TRFactorScaleRows): TUiFindings;

{ The verdict line once the scales were switched: how many, how many problems,
  and the entries that were not - ASkippedCaptions, say a custom one, which
  asks for formulas in a dialog the check cannot answer. AHistoryChecked False
  says the History tab's rows were not judged: it held no fitted model. }
function RFactorScaleSummary(AScaleCount, AFindingCount: longint;
    const ASkippedCaptions: string; AHistoryChecked: boolean): string;

{ The verdict line when nothing could be switched, with the reason. }
function RFactorScaleNotChecked(const AReason: string): string;

type
    { WHAT THE MODEL PANEL DREW for a module's nested rows while a curve type
      placed by picks was selected - read off the tree itself.

      WHY A WINDOW IS NEEDED FOR THIS. Which rows the panel shows is decided in
      model_outline, and tested there; what no headless suite can see is the
      window's use of that decision - whether a module's push and the
      framework's refresh both reach it, in the order the application sends
      them, and whether the tree control ends up nested. The panel once showed
      a nested pattern count flat whenever an ordinary type was selected, and
      every unit test was green. }
    TModelTreeReading = record
        { False when no module in this build declares a panel: there is no
          module to describe the model, and the verdict says the check did not
          run rather than passing. }
        Ran: boolean;
        { Whether the selected type was one placed by picks while reading.
          Under a module's own type the old rule showed the tree too, so a
          reading taken there proves nothing. }
        PicksTypeSelected: boolean;
        { True when the build registers no curve type placed by picks that asks
          for nothing - a product for another field, whose types its own
          module places and whose only other is the user's formula. The case
          the check guards cannot arise, so it says it was not checked rather
          than reporting the build. }
        NoPicksTypeInBuild: boolean;
        { How many curves the model holds, and how many nodes the tree drew.
          Every curve is drawn once - under the module's row that stands for
          it, or as the framework's own row - so more drawn than held is a
          curve listed twice, the module's row and the framework's for it. }
        CurveCount: longint;
        NodesDrawn: longint;
        { Whether the pushed child row was drawn under its parent row. }
        ChildUnderParent: boolean;
        { Whether the parent row, collapsed as a user collapses it, was still
          collapsed after a command refresh that changed nothing. A refresh
          that draws the panel again opens every subtree. }
        CollapseSurvivedRefresh: boolean;
        { Whether rows a module pushed naming no curve of the open model - as
          rows kept from a model since closed do - were drawn. }
        StaleRowsShown: boolean;
        { Whether, with a row selected and the selected curve then cleared as
          a refused Delete curve clears it, a refresh selected the row's curve
          again. }
        CurveFollowsRowAfterRefresh: boolean;
        { THE HIGHLIGHT OVER THE PARENT ROW, read with the parent selected as
          a click selects it: the row stands for its own curve and the child's,
          so both must stand out on the chart. ParentHighlightRead is False
          where the parent's node was not found to select. }
        ParentHighlightRead: boolean;
        ParentHighlight: THighlightReading;
    end;

{ What is wrong with AReading, in the words the log carries. None for a reading
  that did not run - its verdict says so. }
function ModelTreeFindings(const AReading: TModelTreeReading): TUiFindings;

{ The verdict line, written whatever the outcome. Both of its sentences name "a
  module's nested rows", which is what check-ui requires of the log. }
function ModelTreeSummary(const AReading: TModelTreeReading): string;

type
    { The samples the row-command check builds its model from, as indices into
      a profile: an interval spanning the data, and two picks inside it. }
    TRowCommandProbe = record
        BoundFirst, BoundLast, PickFirst, PickSecond: longint;
    end;

    TSampleIndexes = array of longint;

{ The row-command check's samples of a profile of APoints points. A pick must
  name a real sample, which is why these are indices rather than positions. }
function RowCommandProbe(APoints: longint): TRowCommandProbe;

{ The picks the live-fit check adds to a profile of APoints points: eleven,
  spread evenly - but only to the small model the check before it built (fewer
  than three positions, AExistingPositions). Over a project the user opened, the
  curves already there are the ones worth watching a fit of. }
function LiveProgressProbe(APoints, AExistingPositions: longint): TSampleIndexes;

{ Counts one button of a toolbar into AReading's smallest width and height. A
  SEPARATOR IS NOT A TARGET: it is deliberately a few pixels wide and nobody aims
  at it, so measuring it would report every bar that has one. }
procedure CountToolButton(var AReading: TToolBarReading; AWidth, AHeight: longint;
    AIsSeparator: boolean);

{ Keeps the longer of AWidest and AText in AWidest. }
procedure KeepLonger(var AWidest: string; const AText: string);

{ How the Stop check ended: still running ATookMs after Stop, or ended in that
  time leaving ASeries series on the chart. }
function StopOutcome(AStillRunning: boolean; ATookMs: QWord;
    ASeries: longint): string;

implementation

uses
    Math;

function Add(var AFindings: TUiFindings; const AText: string): longint;
begin
    Result := Length(AFindings);
    SetLength(AFindings, Result + 1);
    AFindings[Result] := AText;
end;

{ The text as a reader compares it: trimmed, and without a closing full stop. }
function AsRead(const AText: string): string;
begin
    Result := Trim(AText);
    while (Result <> '') and (Result[Length(Result)] = '.') do
        SetLength(Result, Length(Result) - 1);
    Result := Trim(Result);
end;

function SurfaceFindings(const ARows: TSurfaceRows): TUiFindings;
var
    i, Widest, WidestAt: longint;
begin
    Result := nil;
    Widest := -1;
    WidestAt := -1;

    for i := 0 to High(ARows) do
    begin
        //  ---- A BUTTON WITH NO EXPLANATION. The hints live on the actions,
        //  beside the captions the menus show, and the pane's rows took none of
        //  them until they were asked for - so twelve buttons were silent while
        //  every menu entry behind them explained itself.
        if Trim(ARows[i].PaneHint) = '' then
            Add(Result, Format('%s: the pane button has no hint', [ARows[i].Id]))
        //  ---- A HINT THAT ONLY REPEATS THE CAPTION. It passes the check
        //  above and explains nothing - 'About' over 'About' - which in an
        //  application meant to teach is the same defect as no hint at all.
        else if SameText(AsRead(ARows[i].PaneHint),
            AsRead(ARows[i].PaneCaption)) then
            Add(Result, Format('%s: the hint "%s" repeats its caption and ' +
                'explains nothing', [ARows[i].Id, Trim(ARows[i].PaneHint)]));

        if ARows[i].HasMenuSide then
        begin
            //  ---- AND THE SAME EXPLANATION. Two texts for one command agree
            //  on the day they are written and not after the first edit, which
            //  is why neither surface declares its own.
            if ARows[i].PaneHint <> ARows[i].MenuHint then
                Add(Result, Format(
                    '%s: the pane says "%s" and the menu says "%s"',
                    [ARows[i].Id, ARows[i].PaneHint, ARows[i].MenuHint]));

            //  ---- THE ONE THAT WOULD COST THE MOST. A command offered in one
            //  surface and refused in the other is a program that contradicts
            //  itself in front of the user, and whichever one they reach for
            //  first decides whether the feature exists.
            if ARows[i].PaneEnabled <> ARows[i].MenuEnabled then
                Add(Result, Format(
                    '%s: %s in the pane, %s in the menu',
                    [ARows[i].Id,
                     BoolToStr(ARows[i].PaneEnabled, 'enabled', 'disabled'),
                     BoolToStr(ARows[i].MenuEnabled, 'enabled', 'disabled')]));
        end;

        //  ---- A LATCH THAT DISAGREES WITH ITS OWN MODE. The button is the
        //  only thing that says a picking mode is running, so one left down
        //  over a finished mode tells the user to keep clicking, and one left up
        //  during a running mode hides that their clicks are being taken.
        if ARows[i].HasPicking and (ARows[i].PaneDown <> ARows[i].ModeSaysDown) then
            Add(Result, Format(
                '%s: the button is %s and its picking mode is %s',
                [ARows[i].Id,
                 BoolToStr(ARows[i].PaneDown, 'pressed', 'not pressed'),
                 BoolToStr(ARows[i].ModeSaysDown, 'running', 'not running')]));

        //  ---- AND A LATCH THAT DISAGREES WITH ITS OWN MENU ENTRY. The
        //  framework's latches are checked against the mode they claim; a
        //  module's are checked against the tick, because the tick is where a
        //  module states the same thing and the two are written apart.
        if ARows[i].HasMenuTick and
            (ARows[i].PaneDown <> ARows[i].MenuChecked) then
            Add(Result, Format(
                '%s: the button is %s and its menu entry is %s',
                [ARows[i].Id,
                 BoolToStr(ARows[i].PaneDown, 'pressed', 'not pressed'),
                 BoolToStr(ARows[i].MenuChecked, 'ticked', 'not ticked')]));

        if ARows[i].PaneWidth > Widest then
        begin
            Widest := ARows[i].PaneWidth;
            WidestAt := i;
        end;
    end;

    //  ---- ONE WIDTH FOR ALL OF THEM, in a second pass because the odd one out
    //  can only be named once the widest is known. A row of buttons in two
    //  sizes reads as two kinds of control.
    for i := 0 to High(ARows) do
        if (ARows[i].PaneWidth <> Widest) and (WidestAt >= 0) then
            Add(Result, Format(
                '%s: the button is %d px wide and %s is %d',
                [ARows[i].Id, ARows[i].PaneWidth, ARows[WidestAt].Id, Widest]));
end;

function NoReportTabToZoom: string;
begin
    Result := 'ui: no module in this build reports, so there is no analysis ' +
        'tab and its zoom buttons were not checked';
end;

function ZoomFindings(const AReadings: TZoomReadings): TUiFindings;
var
    i: longint;
    R: TZoomReading;
begin
    Result := nil;
    for i := 0 to High(AReadings) do
    begin
        R := AReadings[i];
        if R.Buttons <> 2 then
            Add(Result, Format('the %s carries %d zoom button(s), not 2',
                [R.Pane, R.Buttons]));
        //  A CLICK THAT CHANGES NOTHING is the defect worth catching here: the
        //  button exists, the action runs, and the pane is drawn exactly as it
        //  was.
        if R.AfterIn <= R.Before then
            Add(Result, Format(
                'Zoom In left the %s at %d, having been %d',
                [R.Pane, R.AfterIn, R.Before]));
        if R.AfterOut <> R.Before then
            Add(Result, Format(
                'Zoom Out left the %s at %d rather than back at %d',
                [R.Pane, R.AfterOut, R.Before]));
        //  AND A CLICK THAT CHANGES SOMEBODY ELSE. Each strip is on the pane it
        //  moves, so a button that resizes the other one moves something the
        //  user is not looking at.
        if R.OtherAfter <> R.OtherBefore then
            Add(Result, Format(
                'zooming the %s moved the other pane from %d to %d',
                [R.Pane, R.OtherBefore, R.OtherAfter]));
        if R.OfferedInAtTop then
            Add(Result, Format(
                'the %s still offers Zoom In at the top of its range',
                [R.Pane]));
        if R.OfferedOutAtBottom then
            Add(Result, Format(
                'the %s still offers Zoom Out at the bottom of its range',
                [R.Pane]));
    end;
end;

function ToolBarFindings(const AReadings: TToolBarReadings;
    AMinimumButton, AMinimumMargin: longint): TUiFindings;
var
    i: longint;
    R: TToolBarReading;
begin
    Result := nil;
    for i := 0 to High(AReadings) do
    begin
        R := AReadings[i];
        if not R.Attached then
            Add(Result, Format('toolbar %s is not attached to anything, so ' +
                'nothing it holds is visible', [R.Name]));
        if R.ButtonCount <= 0 then
        begin
            Add(Result, Format('toolbar %s holds no buttons', [R.Name]));
            //  Nothing below is answerable about a bar with nothing on it, and
            //  three findings for one empty bar buries the one that matters.
            Continue;
        end;
        //  ONE FINDING FOR A BUTTON TOO SMALL IN EITHER DIRECTION. Both
        //  dimensions come from one square metric, so a bar wrong in one is
        //  wrong in both, and saying it twice would read as two defects.
        if not (PointerTargetMet(R.SmallestButtonWidth, AMinimumButton) and
            PointerTargetMet(R.SmallestButtonHeight, AMinimumButton)) then
            Add(Result, Format('toolbar %s draws a button %dx%d, under the ' +
                '%d px a pointer needs to hit it',
                [R.Name, R.SmallestButtonWidth, R.SmallestButtonHeight,
                 AMinimumButton]));
        //  AFTER the margin rule and not instead of it: a bar that never went
        //  through the spacing has both, and the margin is the one to act on.
        if R.LeadingIndent > 0 then
            Add(Result, Format('toolbar %s indents its first button %d px ' +
                'beyond its margin, so its leading edge is wider than its ' +
                'other three', [R.Name, R.LeadingIndent]));
        if R.DrawsEdgeBorder then
            Add(Result, Format('toolbar %s still draws its own border, which ' +
                'with the margin is a rule above its first button and nothing ' +
                'under it', [R.Name]));
        if R.RaisedAtRest then
            Add(Result, Format('toolbar %s draws every button raised at rest, ' +
                'so its buttons run together into one block', [R.Name]));
        if R.Margin < AMinimumMargin then
            Add(Result, Format('toolbar %s leaves %d px between its buttons ' +
                'and its edge, under the %d that reads as a boundary',
                [R.Name, R.Margin, AMinimumMargin]));
    end;

    //  AND THEN THE BARS AGAINST EACH OTHER. Every rule above judges one bar
    //  on its own, and a window whose bars each pass them can still be plainly
    //  wrong: what a reader compares is one bar with the next, and the odd one
    //  out is the only one anybody notices. The main bar's buttons sat two
    //  pixels further in than every other bar's - its panel has a bevel the
    //  other panels were given none of - and not one single-bar rule could see
    //  it.
    //
    //  THE FIRST BAR IS THE STANDARD, rather than a majority or an average: the
    //  spacing is applied from one set of numbers to every bar in one walk, so
    //  any disagreement at all is a bar something else reached. Which one is
    //  "right" is not the question; that they differ is.
    for i := 1 to High(AReadings) do
    begin
        R := AReadings[i];
        if R.ButtonCount <= 0 then
            Continue;
        if (R.SmallestButtonWidth <> AReadings[0].SmallestButtonWidth) or
            (R.SmallestButtonHeight <> AReadings[0].SmallestButtonHeight) then
            Add(Result, Format('toolbar %s draws %dx%d buttons where %s draws ' +
                '%dx%d, so the bars do not look alike',
                [R.Name, R.SmallestButtonWidth, R.SmallestButtonHeight,
                 AReadings[0].Name, AReadings[0].SmallestButtonWidth,
                 AReadings[0].SmallestButtonHeight]));
        if R.Margin <> AReadings[0].Margin then
            Add(Result, Format('toolbar %s keeps %d px around its buttons ' +
                'where %s keeps %d, so the bars do not look alike',
                [R.Name, R.Margin, AReadings[0].Name, AReadings[0].Margin]));
    end;
end;

function ToolBarSpacingSummary(ABarCount, AFindingCount: longint): string;
begin
    Result := Format('ui: measured %d toolbar(s), %d problem(s)',
        [ABarCount, AFindingCount]);
end;

function StateFindings(const AReadings: TStateReadings): TUiFindings;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(AReadings) do
        if AReadings[i].ShowsState and not AReadings[i].CommandHasState then
            Add(Result, Format('%s presses or ticks itself on every second ' +
                'use, but the command behind it has no state to show',
                [AReadings[i].Name]));
end;

function StateSummary(AControlCount, AFindingCount: longint): string;
begin
    Result := Format('ui: measured %d control(s) that can show a state, ' +
        '%d problem(s)', [AControlCount, AFindingCount]);
end;

function ZoomSummary(APaneCount, AFindingCount: longint): string;
begin
    Result := Format(
        'ui: checked the zoom buttons of %d pane(s), %d problem(s)',
        [APaneCount, AFindingCount]);
end;

function LegendFindings(const ARows: TLegendRows): TUiFindings;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(ARows) do
    begin
        //  A ROW THAT NAMES NOTHING is the state the old code could not even
        //  detect: it left rows behind when a series went, and drew them
        //  against whatever series happened to sit at that position.
        if not ARows[i].HasSeries then
        begin
            Add(Result, Format('legend row %d ("%s") carries no series',
                [i, ARows[i].Text_]));
            Continue;
        end;
        if not ARows[i].SeriesOnChart then
        begin
            Add(Result, Format(
                'legend row %d ("%s") names a series that is not on the chart',
                [i, ARows[i].Text_]));
            Continue;
        end;
        //  AND IT MUST NAME THE ONE IT CARRIES. A row reading one curve's name
        //  while controlling another is the failure this pairing exists to
        //  prevent, and it is invisible until the user ticks it.
        if ARows[i].Text_ <> ARows[i].SeriesTitle then
            Add(Result, Format(
                'legend row %d reads "%s" and its series is called "%s"',
                [i, ARows[i].Text_, ARows[i].SeriesTitle]));
    end;
end;

function LegendSummary(ARowCount, AFindingCount: longint): string;
begin
    Result := Format('ui: checked %d legend row(s), %d that do not match the ' +
        'series they name', [ARowCount, AFindingCount]);
end;

function ReadoutFindings(const ARows: TReadoutRows): TUiFindings;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to High(ARows) do
    begin
        //  THE CAPTION MUST END BEFORE ITS VALUE BEGINS, or the two are written
        //  over each other.
        if ARows[i].CaptionRight > ARows[i].ValueLeft then
            Add(Result, Format('the readout caption "%s" runs into its value ' +
                '(it ends at %d, the value starts at %d)',
                [ARows[i].Caption, ARows[i].CaptionRight, ARows[i].ValueLeft]));
        //  AND THE VALUE MUST FIT ITS LABEL - the report was "Price: (".
        if ARows[i].ValueTextWidth > ARows[i].ValueWidth then
            Add(Result, Format('the value beside "%s" needs %d px and its ' +
                'label has %d, so "%s" is cut off',
                [ARows[i].Caption, ARows[i].ValueTextWidth, ARows[i].ValueWidth,
                 ARows[i].Value]));
    end;
    //  ONE COLUMN: every value starts where the first one does.
    for i := 1 to High(ARows) do
        if ARows[i].ValueLeft <> ARows[0].ValueLeft then
            Add(Result, Format('the readout values do not form a column: ' +
                '"%s" starts at %d and "%s" at %d',
                [ARows[0].Caption, ARows[0].ValueLeft, ARows[i].Caption,
                 ARows[i].ValueLeft]));
end;

function ReadoutSummary(ARowCount, AFindingCount: longint): string;
begin
    Result := Format('ui: checked %d readout line(s), %d problem(s)',
        [ARowCount, AFindingCount]);
end;

function ModelTabFinding(const AShown, AModelOwner: string): string;
var
    Wanted: string;
begin
    //  The panel's own rule, asked rather than restated: a check with its own
    //  copy of the wording passes a window the rule never reached.
    Wanted := ModelPanelCaptionFor(AModelOwner, True);
    if AShown = Wanted then
        Result := ''
    else
        Result := Format('model tab: reads "%s" over a model of %s curves; ' +
            'it should read "%s"', [AShown, AModelOwner, Wanted]);
end;

function IdleLayoutFinding(AResizes, AMilliseconds: longint): string;
begin
    if AResizes <= IDLE_RESIZE_ALLOWANCE then
        Exit('');
    Result := Format('the window never settles: the chart was resized %d ' +
        'time(s) in %d ms with nothing happening - a layout loop, which ' +
        'redraws every curve each time round', [AResizes, AMilliseconds]);
end;

{ AName, or 'nothing' for no control. }
function ControlOrNothing(const AName: string): string;
begin
    if AName = '' then
        Exit('nothing');
    Result := AName;
end;

function SplitterFindings(const AReadings: TSplitterReadings): TUiFindings;
var
    i: longint;
    R: TSplitterReading;
    Moved: longint;
begin
    Result := nil;
    for i := 0 to High(AReadings) do
    begin
        R := AReadings[i];
        //  ONE SENTENCE A BORDER: a splitter that resizes the wrong control
        //  also fails to move its pane, and saying both says one thing twice.
        if R.Pane = '' then
            Add(Result, Format('splitters: %s gives room to no part the ' +
                'window remembers, so where the user drags it is forgotten ' +
                'when the window closes', [R.Splitter]))
        else if R.Splitter = '' then
            Add(Result, Format('splitters: no splitter resizes the %s pane ' +
                '(%s) - its border cannot be dragged', [R.Pane, R.PaneControl]))
        else if R.ResizedAfterRestore <> R.PaneControl then
            Add(Result, Format('splitters: once the %s pane had been given ' +
                'a remembered size, dragging %s resized %s instead of %s - ' +
                'the border no longer drags', [R.Pane, R.Splitter,
                ControlOrNothing(R.ResizedAfterRestore), R.PaneControl]))
        else if (R.ResizedInSmallWindow <> '') and
            (R.ResizedInSmallWindow <> R.PaneControl) then
            Add(Result, Format('splitters: in a window made %s small, dragging ' +
                '%s resized %s instead of %s - the pane no longer fitted beside ' +
                'its splitter', [R.SmallWindow, R.Splitter,
                ControlOrNothing(R.ResizedInSmallWindow), R.PaneControl]))
        else
        begin
            Moved := R.SizeBefore - R.SizeAfter;
            if Moved <> R.Dragged then
                Add(Result, Format('splitters: dragging %s by %d changed the ' +
                    '%s pane by %d', [R.Splitter, R.Dragged, R.Pane, Moved]));
        end;
    end;
end;

function SplitterSummary(ACount, AFindingCount: longint;
    const ASmallWindow: string): string;
begin
    Result := Format('ui: splitters: %d border(s) restored and dragged', [ACount]);
    //  SAID WHEN IT WAS, so a pass reads as covering the small window too.
    if ASmallWindow <> '' then
        Result := Result + Format(', and again in a window %s', [ASmallWindow]);
    Result := Result + Format(', %d problem(s)', [AFindingCount]);
end;

function StopFinding(AMilliseconds: int64): string;
begin
    if AMilliseconds <= STOP_RETURN_ALLOWANCE_MS then
        Exit('');
    Result := Format('stop: the window came back %d ms after Stop was ' +
        'pressed - it looks hung for all of that time', [AMilliseconds]);
end;

function IdleLayoutSummary(AResizes, AMilliseconds: longint): string;
begin
    Result := Format('ui: idle layout: %d resize(s) of the chart in %d ms',
        [AResizes, AMilliseconds]);
end;

function SurfaceSummary(ARowCount, AFindingCount: longint): string;
begin
    Result := Format('ui: checked %d command row(s), %d disagreement(s) ' +
        'between the pane and the menus', [ARowCount, AFindingCount]);
end;

function RowCommandBlocked(ADataFileOpen: boolean;
    AProfilePoints: longint): string;
begin
    Result := '';
    //  IN THE ORDER THEY STOP THE CHECK. Without a file there is no profile to
    //  ask about, so its length is not a second reason - it is a question that
    //  cannot be asked yet, and reporting both would name a cause that is only a
    //  consequence.
    if not ADataFileOpen then
        Result := 'no data file open, so the Model panel could not be ' +
            'checked - the task must pass --infile'
    else if AProfilePoints < MinProfilePointsForPicks then
        Result := 'the open profile is too short to place picks in, so the ' +
            'Model panel could not be checked';
end;

function NoRowNamesACurve: string;
begin
    Result := 'two picks and an interval produced no Model panel row that ' +
        'names a curve, so nothing on its menu can ever apply';
end;

function RowCommandFindings(const AReach: TRowCommandReach): TUiFindings;
var
    t: longint;
begin
    Result := nil;
    //  ---- AN ENTRY PROMISING AN EXPLANATION THAT IS NOT THERE. Every one,
    //  not the first: a module that links its entries to a renamed rule loses
    //  them all at once.
    for t := 0 to High(AReach.UnresolvedTopics) do
        Add(Result, Format('a context entry names the explanation %s, which ' +
            'resolves to nothing, so the Explain pane cannot show it',
            [AReach.UnresolvedTopics[t]]));
    //  ---- THE FRAMEWORK'S OWN ROW.
    //  A MODULE'S ENTRIES COUNT: a menu whose only entries a module built as
    //  it opened is not an empty menu.
    if AReach.EntryCount + AReach.ModuleEntryCount = 0 then
        //  A menu with no entries at all, which the enabled-count below cannot
        //  distinguish from one whose entries are merely all disabled.
        Add(Result, 'the Model panel''s context menu has no entries at all, ' +
            'so there is nothing for a row to enable')
    else if not AReach.OfferedOnOwnRow then
        Add(Result, 'a Model panel row naming a curve is selected and every ' +
            'context entry is still disabled, so the menu can never be used');

    //  ---- AND THE ROW A CONTRIBUTOR PUT THERE, which is the one the check
    //  could not see and the one the user reported.
    if AReach.ContributorCurveId = '' then
    begin
        //  NOT A PASS. The contributor half was not reached, and saying so is
        //  the difference between "checked and sound" and "never looked".
        Add(Result, 'the model built for this check reports no curve handle, ' +
            'so a contributor row could not be checked');
        Exit;
    end;
    if AReach.SelectedCurveId <> AReach.ContributorCurveId then
        //  THE DEFECT ITSELF, stated as the question it is: a row that says
        //  which curve it stands for must select that curve, whoever put the
        //  row there.
        Add(Result, 'a contributor row naming a curve was selected and the ' +
            'window read it as naming none, so every command on one curve is ' +
            'dead over it');
    if (AReach.EntryCount > 0) and (not AReach.OfferedOnContributorRow) then
        Add(Result, 'over a contributor row naming a curve every context ' +
            'entry is disabled, so the menu can never be used there');
end;

procedure NoteEntryTopic(var AReach: TRowCommandReach; const ATopic: string;
    AResolves: boolean);
var
    t: longint;
begin
    if ATopic = '' then
        Exit;
    Inc(AReach.TopicsChecked);
    if AResolves then
        Exit;
    //  ONCE: the menu is read over two rows and meets the same entry over
    //  each, and one broken link is one finding.
    for t := 0 to High(AReach.UnresolvedTopics) do
        if AReach.UnresolvedTopics[t] = ATopic then
            Exit;
    SetLength(AReach.UnresolvedTopics, Length(AReach.UnresolvedTopics) + 1);
    AReach.UnresolvedTopics[High(AReach.UnresolvedTopics)] := ATopic;
end;

function RowCommandSummary(const AReach: TRowCommandReach): string;
begin
    //  THE REQUIRED LINE, extended rather than joined by another: check-ui
    //  prints only the verdicts it knows (Read-CheckUiLog), and this one it
    //  requires, so what is added here is read on every run.
    Result := Format('ui: the Model panel offers %d of %d context entr(ies) ' +
        'over a row naming a curve; %d entr(ies) a module added over a ' +
        'contributor row; %d explanation topic(s) looked up, %d resolving to ' +
        'nothing', [Ord(AReach.OfferedOnOwnRow) * AReach.EntryCount,
        AReach.EntryCount, AReach.ModuleEntryCount, AReach.TopicsChecked,
        Length(AReach.UnresolvedTopics)]);
end;

procedure CountSeriesHighlight(var AReading: THighlightReading;
    const AOwnerCurveId: string; ALineWidth: longint; AOnTop: boolean);
begin
    //  An empty selection owns nothing, so with none every series is counted
    //  as somebody else's - which is what makes a leftover highlight visible.
    if OwnedByAnyOf(AOwnerCurveId, AReading.CurveIds) then
    begin
        Inc(AReading.OwnedCount);
        if SeriesDrawnHighlighted(ALineWidth, AOnTop) then
            Inc(AReading.OwnedHighlighted);
    end
    else if AOnTop or (ALineWidth > NormalLineWidth) then
        Inc(AReading.OtherHighlighted);
end;

function HighlightFindings(const AReading: THighlightReading;
    const AWhen: string): TUiFindings;
begin
    Result := nil;
    if not AReading.ExpectSelection then
    begin
        if AReading.OtherHighlighted > 0 then
            Add(Result, Format('%s, %d series are still highlighted on the chart',
                [AWhen, AReading.OtherHighlighted]));
        Exit;
    end;
    //  NOT A PASS where nothing was held or nothing was drawn for it: zero of
    //  zero highlighted is nothing wrong only because nothing was looked at.
    if Length(AReading.CurveIds) = 0 then
        Add(Result, Format('%s, the window held no selected curve, so the ' +
            'highlight could not be checked', [AWhen]))
    else if AReading.OwnedCount = 0 then
        Add(Result, Format('%s, no series on the chart was drawn for the ' +
            'selected curve, so its highlight could not be checked', [AWhen]))
    else if AReading.OwnedHighlighted < AReading.OwnedCount then
        Add(Result, Format('%s, %d of the %d series drawn for the selected ' +
            'curve are not highlighted on the chart',
            [AWhen, AReading.OwnedCount - AReading.OwnedHighlighted,
            AReading.OwnedCount]));
    if AReading.OtherHighlighted > 0 then
        Add(Result, Format('%s, %d series are highlighted although they were ' +
            'not drawn for the selected curve',
            [AWhen, AReading.OtherHighlighted]));
    //  THE ROW'S WHOLE SET. A parent row that lit only its own curve passes
    //  every count above, over that one curve.
    if (AReading.ExpectedCurveCount > 0) and
        (Length(AReading.CurveIds) <> AReading.ExpectedCurveCount) then
        Add(Result, Format('%s, the selected row stands for %d curve(s) and ' +
            'the window highlights %d', [AWhen, AReading.ExpectedCurveCount,
            Length(AReading.CurveIds)]));
    //  THE GESTURE'S CURVE. Every count above agrees with whatever is held, so
    //  a selection that never moved passes them all - with the old curve.
    if (AReading.ExpectedCurveId <> '') and
        (AReading.CurveId <> AReading.ExpectedCurveId) then
        Add(Result, Format('%s, the window holds curve %s rather than %s, ' +
            'the one chosen', [AWhen, AReading.CurveId,
            AReading.ExpectedCurveId]));
    //  A PANEL SHOWING NO ROW is not a disagreement: a curve no row stands for
    //  leaves the panel pointing at nothing, which misleads nobody.
    if (AReading.PanelCurveId <> '') and
        (AReading.PanelCurveId <> AReading.CurveId) then
        Add(Result, Format('%s, the Model panel shows another curve selected ' +
            '(%s) than the one the window acts on (%s)',
            [AWhen, AReading.PanelCurveId, AReading.CurveId]));
end;

function NoTableRowToSelect: string;
begin
    Result := 'the Curve Attributes table has no row naming a curve other ' +
        'than the selected one, so choosing a curve there could not be checked';
end;

function HighlightSummary(const AReading: THighlightReading;
    const AWhen: string): string;
begin
    if AReading.ExpectSelection then
        Result := Format('ui: %s, the %d selected curve(s) have %d of %d ' +
            'series highlighted on the chart, and %d other series highlighted',
            [AWhen, Length(AReading.CurveIds), AReading.OwnedHighlighted,
            AReading.OwnedCount, AReading.OtherHighlighted])
    else
        Result := Format('ui: %s, %d series highlighted on the chart',
            [AWhen, AReading.OtherHighlighted]);
end;

function ContributorRowSummary(AOffered: boolean; AEntryCount: longint): string;
begin
    Result := Format('ui: over a contributor row the Model panel offers %d ' +
        'of %d context entr(ies)', [Ord(AOffered) * AEntryCount, AEntryCount]);
end;


function DataSourceFindings(const AReading: TDataSourceReading): TUiFindings;
begin
    Result := nil;
    if AReading.SourceCount = 0 then
        //  The wizard would open on an empty list, which reads as a build
        //  without the feature rather than as a start-up that skipped a call.
        Add(Result, 'the running application has no data sources at all, so ' +
            'the wizard has nothing to offer - start-up did not register them');
    if AReading.OfflineCount = 0 then
        //  Every source needing the network means the wizard is useless on a
        //  machine with none, and that the end-to-end path has nothing to run
        //  against offline.
        Add(Result, 'every data source in this build needs the network, so ' +
            'there is nothing to fetch on a machine without one');
    if AReading.Findings <> '' then
        Add(Result, 'the data sources in this build are incomplete: ' +
            AReading.Findings);
    if not AReading.CommandFound then
        Add(Result, 'no menu command opens the data source wizard, so ' +
            'nothing the sources offer can be reached')
    else if not AReading.CommandEnabled then
        Add(Result, 'the data source command is on the menu and disabled, so ' +
            'the wizard cannot be opened');
end;

function DataSourceSummary(const AReading: TDataSourceReading): string;
begin
    //  'ui: ' as every other verdict carries it - the task that drives the
    //  check reads only the lines with that prefix, so a summary without one
    //  is written and never seen.
    Result := Format('ui: data sources: %d registered, %d of them usable ' +
        'offline', [AReading.SourceCount, AReading.OfflineCount]);
    if not AReading.CommandFound then
        Result := Result + ', and no menu command reaches them';
end;

{ Whether A and B hold the same values in the same order, each within ATolerance. }
function SameValuesWithin(const A, B: array of double; ATolerance: double): boolean;
var
    i: longint;
begin
    Result := Length(A) = Length(B);
    if not Result then
        Exit;
    for i := 0 to High(A) do
        if Abs(A[i] - B[i]) > ATolerance then
            Exit(False);
end;

{ The values as a finding writes them: "50, 100, 150", or "none". }
function ValuesText(const AValues: array of double): string;
var
    i: longint;
begin
    if Length(AValues) = 0 then
        Exit('none');
    Result := Format('%g', [AValues[0]]);
    for i := 1 to High(AValues) do
        Result := Result + Format(', %g', [AValues[i]]);
end;

function StructureStripFindings(const AReading: TStructureStripReading): TUiFindings;
var
    Span: double;
begin
    Result := nil;
    if AReading.Samples = 0 then
        Exit;
    if AReading.Points <> AReading.Samples then
        Add(Result, Format('the Difference structure strip drew %d point(s) ' +
            'for %d sample(s) of the fit intervals', [AReading.Points,
            AReading.Samples]));
    if AReading.DifferencePoints <> AReading.Samples then
        Add(Result, Format('the Difference structure strip drew %d point(s) ' +
            'of the difference for %d sample(s)', [AReading.DifferencePoints,
            AReading.Samples]));
    if AReading.LevelPoints <> 2 then
        Add(Result, 'the Difference structure strip has no level line to ' +
            'read its curve by');
    //  THE MODEL'S X, zoom included, and its grid lines where the model's
    //  are: the strip's lines are read as the model's dates.
    Span := Max(1e-12, Abs(AReading.ModelHi - AReading.ModelLo));
    if (Abs(AReading.StripLo - AReading.ModelLo) > 1e-6 * Span) or
       (Abs(AReading.StripHi - AReading.ModelHi) > 1e-6 * Span) then
        Add(Result, Format('the Difference structure strip shows x %g..%g ' +
            'while the model''s chart shows %g..%g', [AReading.StripLo,
            AReading.StripHi, AReading.ModelLo, AReading.ModelHi]));
    if not SameValuesWithin(AReading.StripMarks, AReading.ModelMarks,
        1e-6 * Span) then
        Add(Result, Format('the Difference structure strip draws its ' +
            'vertical lines at x %s, the model''s chart at %s',
            [ValuesText(AReading.StripMarks), ValuesText(AReading.ModelMarks)]));
end;

function SpectrumMapFindings(const AReading: TSpectrumMapReading): TUiFindings;
var
    Span: double;
begin
    Result := nil;
    if AReading.NotInBuild or (AReading.Samples = 0) then
        Exit;
    if AReading.Columns <> AReading.Samples then
        Add(Result, Format('the Residual Spectrum map holds %d column(s) for ' +
            '%d sample(s) of the fit intervals', [AReading.Columns,
            AReading.Samples]));
    if AReading.Rows = 0 then
        Add(Result, 'the Residual Spectrum map holds no rhythm to draw');
    if AReading.KeyNamed < AReading.KeyKinds then
        Add(Result, Format('the key under the Residual Spectrum map names %d ' +
            'of the %d kinds of cell', [AReading.KeyNamed,
            AReading.KeyKinds]));

    Span := Max(1e-12, Abs(AReading.ChartHi - AReading.ChartLo));
    if (Abs(AReading.MapLo - AReading.ChartLo) > 1e-6 * Span) or
       (Abs(AReading.MapHi - AReading.ChartHi) > 1e-6 * Span) then
        Add(Result, Format('the Residual Spectrum map shows x %g..%g where ' +
            'the model''s chart shows %g..%g', [AReading.MapLo, AReading.MapHi,
            AReading.ChartLo, AReading.ChartHi]));
end;

function LinkedZoomFindings(const AReading: TLinkedZoomReading): TUiFindings;
var
    Span: double;
begin
    Result := nil;
    if AReading.NotInBuild or not AReading.Checked then
        Exit;
    Span := Max(1e-12, Abs(AReading.MapHi - AReading.MapLo));
    if (Abs(AReading.ModelLo - AReading.MapLo) > 1e-6 * Span) or
       (Abs(AReading.ModelHi - AReading.MapHi) > 1e-6 * Span) then
        Add(Result, Format('linked zoom: the Residual Spectrum was zoomed to ' +
            'x %g..%g and the model''s chart shows %g..%g', [AReading.MapLo,
            AReading.MapHi, AReading.ModelLo, AReading.ModelHi]));
    if AReading.MapApartAfterModelZoom then
        Add(Result, 'linked zoom: the model''s chart was zoomed in and the ' +
            'Residual Spectrum shows another stretch of x');
    if AReading.MapZoomedAfterModelShownWhole then
        Add(Result, 'linked zoom: the model''s chart was made to show ' +
            'everything and the Residual Spectrum still shows part of it');
    if AReading.ApartAfterScroll then
        Add(Result, 'linked zoom: scrolled with a scroll bar, the model''s ' +
            'chart and the Residual Spectrum show different stretches of x');
    if AReading.SpectrumHasNoBar then
        Add(Result, 'linked zoom: the Residual Spectrum has no scroll bar to ' +
            'move along x');
    if AReading.SpectrumHasNoYBar then
        Add(Result, 'linked zoom: the Residual Spectrum has no scroll bar to ' +
            'move along its wave lengths');
    if AReading.MapYNotScrolled then
        Add(Result, 'linked zoom: the Residual Spectrum''s wave lengths were ' +
            'not zoomed by the command, or not moved by its vertical scroll bar');
    if AReading.BarsDisagree then
        Add(Result, 'linked zoom: the two tabs'' scroll bars show the view in ' +
            'different places');
    if AReading.ModelZoomedAfterShowAll or AReading.OthersApartAfterShowAll then
        Add(Result, 'linked zoom: the Residual Spectrum was made to show ' +
            'everything and the model''s chart stayed zoomed, or the charts ' +
            'sharing its x show different stretches of it');
end;

function RFactorKeptFindings(const AReading: TRFactorKeptReading): TUiFindings;
begin
    Result := nil;
    if not AReading.Checked then
        Exit;
    if not AReading.MadeCurrent then
    begin
        Add(Result, 'R-factor kept: the model of the last fit could not be ' +
            'made current from the History');
        Exit;
    end;
    if AReading.EntryIsFit and (AReading.Kept = 0) then
        Add(Result, 'R-factor kept: the History entry of a fit keeps no ' +
            'R-factor');
    if AReading.Drawn <> AReading.Kept then
        Add(Result, Format('R-factor kept: the model made current keeps %d ' +
            'R-factor sample(s) and the R-factor tab draws %d',
            [AReading.Kept, AReading.Drawn]));
end;

function ChartClickFindings(const AReading: TChartClickReading): TUiFindings;
begin
    Result := nil;
    if not AReading.Checked then
        Exit;
    if not AReading.PickingStarted then
    begin
        Add(Result, 'chart click: Model > Curve Positions > Start Manual ' +
            'Selection did not start picking, so nothing was clicked');
        Exit;
    end;
    if AReading.AfterFirst <> AReading.Before + 1 then
        Add(Result, Format('chart click: a left click on a sample of the data ' +
            'in Curve Positions picking left %d pick(s) where there were %d - ' +
            'it should add one', [AReading.AfterFirst, AReading.Before]))
    //  A PIXEL OF SLACK EACH WAY: the pick is drawn where the sample is, and
    //  rounding its x to the screen can land one pixel off the pointer's.
    else if not AReading.PickFound or
        (Abs(AReading.PickedPixelX - AReading.ClickedPixelX) > 1) then
        Add(Result, Format('chart click: the click at x pixel %d added a pick ' +
            'at x pixel %d, not at the sample under the pointer',
            [AReading.ClickedPixelX, AReading.PickedPixelX]));
    if AReading.AfterSecond <> AReading.Before then
        Add(Result, Format('chart click: clicking the new pick again left %d ' +
            'pick(s) where there were %d before the first click - it should ' +
            'take the pick back', [AReading.AfterSecond, AReading.Before]));
end;

function ChartClickSummary(const AReading: TChartClickReading;
    AFindingCount: longint): string;
begin
    if not AReading.Checked then
        Result := 'ui: chart click: not checked - ' + AReading.NotCheckedWhy
    else
        Result := Format('ui: chart click: a sample clicked twice with the ' +
            'pointer in Curve Positions picking, %d pick(s) before, %d after ' +
            'the first click, %d after the second, %d problem(s)',
            [AReading.Before, AReading.AfterFirst, AReading.AfterSecond,
            AFindingCount]);
end;

function RFactorKeptEntry(AHistory: TModelHistory): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to AHistory.Count - 1 do
        if AHistory[i].RunKind in [hrkMinimizeDifference,
            hrkMinimizeNumberOfCurves, hrkAutomatically] then
            if (Result < 0) or (AHistory[i].Id <> AHistory.CurrentId) then
                Result := i;
end;

function RFactorScaleOrder(const AOnMenuBar, AIsLinear: array of boolean): TEntryOrder;
var
    Pass, i: longint;
begin
    Result := nil;
    //  NOT "for .. in [True, False]": a set is walked in ordinal order, False
    //  first, and Linear came last.
    for Pass := 0 to 1 do
        for i := 0 to High(AOnMenuBar) do
            if AOnMenuBar[i] and (AIsLinear[i] = (Pass = 0)) then
            begin
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := i;
            end;
end;

procedure LogUiVerdict(const AFindings: TUiFindings; const ASummary: string;
    AWrite: TUiLineWriter);
var
    i: longint;
begin
    for i := 0 to High(AFindings) do
        AWrite('ui: ' + AFindings[i], Warning);
    if ASummary <> '' then
        AWrite(ASummary, Notification);
end;

function RFactorKeptSummary(const AReading: TRFactorKeptReading;
    AFindingCount: longint): string;
begin
    if not AReading.Checked then
        Result := 'ui: R-factor kept: not checked - no fit in the History'
    else
        Result := Format('ui: R-factor kept: the last fit made current from ' +
            'the History, %d sample(s) kept, %d drawn, %d problem(s)',
            [AReading.Kept, AReading.Drawn, AFindingCount]);
end;

function OpenedDataFindings(const AReading: TOpenedDataReading): TUiFindings;
var
    Tol: double;
begin
    Result := nil;
    if not AReading.Checked then
        Exit;
    Tol := 1e-6 * Max(1e-12, Abs(AReading.DataHi - AReading.DataLo));
    if (AReading.ChartLo > AReading.DataLo + Tol) or
       (AReading.ChartHi < AReading.DataHi - Tol) then
        Add(Result, Format('opened data: the data runs over x %g..%g and the ' +
            'model''s chart shows %g..%g of it', [AReading.DataLo,
            AReading.DataHi, AReading.ChartLo, AReading.ChartHi]));
end;

function OpenedDataSummary(const AReading: TOpenedDataReading;
    AFindingCount: longint): string;
begin
    if not AReading.Checked then
        Result := 'ui: opened data: not checked - no data was opened'
    else
        Result := Format('ui: opened data: x %g..%g, the model''s chart ' +
            'showing %g..%g before any model, %d problem(s)', [AReading.DataLo,
            AReading.DataHi, AReading.ChartLo, AReading.ChartHi, AFindingCount]);
end;

function LinkedZoomSummary(const AReading: TLinkedZoomReading;
    AFindingCount: longint): string;
begin
    if AReading.NotInBuild then
        Result := 'ui: linked zoom: not checked - this build offers no ' +
            'Residual Spectrum to zoom with the model''s chart'
    else if not AReading.Checked then
        Result := 'ui: linked zoom: not checked - nothing on the charts to zoom'
    else
        Result := Format('ui: linked zoom: the Residual Spectrum and the ' +
            'model''s chart each zoomed, shown whole and scrolled, the other ' +
            'charts and both scroll bars following, %d problem(s)',
            [AFindingCount]);
end;

function ChartFrameFindings(const AReadings: TChartFrameReadings): TUiFindings;
var
    i: longint;
begin
    Result := nil;
    for i := 1 to High(AReadings) do
        if (Abs(AReadings[i].Left - AReadings[0].Left) > 1) or
           (Abs(AReadings[i].Right - AReadings[0].Right) > 1) then
            Add(Result, Format('the %s plot runs from x %d to %d in its tab where ' +
                'the %s plot runs from %d to %d', [AReadings[i].Name,
                AReadings[i].Left, AReadings[i].Right, AReadings[0].Name,
                AReadings[0].Left, AReadings[0].Right]));
end;

function ChartFrameSummary(const AReadings: TChartFrameReadings;
    AFindingCount: longint): string;
var
    i: longint;
    Edges: string;
begin
    if Length(AReadings) < 2 then
        Exit('ui: chart frames: not checked - fewer than two charts to stand ' +
            'over each other');
    Edges := '';
    for i := 0 to High(AReadings) do
    begin
        if i > 0 then
            Edges := Edges + ', ';
        Edges := Edges + Format('%s %d..%d in a chart at %d, %d wide',
            [AReadings[i].Name, AReadings[i].Left, AReadings[i].Right,
            AReadings[i].ControlLeft, AReadings[i].ControlWidth]);
    end;
    Result := Format('ui: chart frames: %d plot(s) measured against the ' +
        'model''s (%s), %d problem(s)', [Length(AReadings), Edges, AFindingCount]);
end;

function SpectrumMapSummary(const AReading: TSpectrumMapReading;
    AFindingCount: longint): string;
begin
    if AReading.NotInBuild then
        Result := 'ui: residual spectrum: not checked - this build offers no ' +
            'Residual Spectrum'
    else if AReading.Samples = 0 then
        Result := 'ui: residual spectrum: not checked - no fitted model over ' +
            'a fit interval to map'
    else
        Result := Format('ui: residual spectrum: %d column(s) by %d rhythm(s) ' +
            'mapped, %d kind(s) of cell in its key, x tied to the model''s ' +
            'chart, %d problem(s)', [AReading.Columns, AReading.Rows,
            AReading.KeyNamed, AFindingCount]);
end;

function StructureStripSummary(const AReading: TStructureStripReading;
    AFindingCount: longint): string;
begin
    if AReading.Samples = 0 then
        Result := 'ui: difference structure: not checked - no fitted model ' +
            'over a fit interval to judge'
    else
        Result := Format('ui: difference structure: %d point(s) drawn for %d ' +
            'sample(s) of the fit intervals, on the model''s x and its lines when the model''s chart zooms, %d ' +
            'problem(s)', [AReading.Points, AReading.Samples, AFindingCount]);
end;

function LiveProgressFindings(const AReading: TLiveProgressReading): TUiFindings;
begin
    Result := nil;
    if not AReading.Ran then
    begin
        Add(Result, 'the fit did not run, so nothing about what the window ' +
            'draws during one was measured');
        Exit;
    end;
    //  A MODULE'S ROWS WHILE IT RAN - judged whatever the fit's length: the
    //  push happened while it was running, which is the case in question.
    if AReading.PanelPushChecked then
    begin
        if AReading.PanelDrawnDuringFit then
            Add(Result, 'a module''s rows pushed during a fit were drawn in ' +
                'the Model panel while the fit ran, so the window can wait on ' +
                'the fit''s lock');
        if not AReading.PanelDrawnAfterFit then
            Add(Result, 'a module''s rows pushed during a fit were never drawn ' +
                'after it ended, so the Model panel describes the model as it ' +
                'was before the fit');
    end;
    //  Nothing below is answerable when the fit was over before the window
    //  could tick. Said in the summary rather than found here.
    if AReading.Seconds < MinFitSecondsToWatch then
        Exit;

    if AReading.Frames = 0 then
        Add(Result, Format('the window drew no progress frame while a fit ' +
            'ran for %.1f s: the chart area stayed exactly as it was',
            [AReading.Seconds]));

    if AReading.Animating then
    begin
        if AReading.Curves = 0 then
            Add(Result, Format('Animation Mode drew no frame of the model ' +
                'during a fit of %.1f s: the curves did not move',
                [AReading.Seconds]));
    end
    else if AReading.LossPoints = 0 then
        Add(Result, Format('the loss chart held no point during a fit of ' +
            '%.1f s: the user watched an empty chart', [AReading.Seconds]));
end;

function LiveProgressSummary(const AReading: TLiveProgressReading): string;
begin
    if not AReading.Ran then
    begin
        Result := 'ui: live progress: no fit ran, so it was not watched';
        Exit;
    end;
    if AReading.Seconds < MinFitSecondsToWatch then
    begin
        Result := Format('ui: live progress: the fit took %.2f s, too short ' +
            'for the window to draw anything, so it was not judged',
            [AReading.Seconds]);
        Exit;
    end;
    Result := Format('ui: live progress: during a fit of %.1f s the window''s ' +
        'timer fired %d ' +
        'time(s) and it drew %d progress frame(s), %d loss point(s) and %d ' +
        'frame(s) of the model itself (animation %s)',
        [AReading.Seconds, AReading.Ticks, AReading.Frames,
        AReading.LossPoints, AReading.Curves,
        BoolToStr(AReading.Animating, 'on', 'off')]);
end;

function RFactorScaleFindings(const ARows: TRFactorScaleRows): TUiFindings;
var
    i, j: longint;
begin
    Result := nil;
    for i := 0 to High(ARows) do
        with ARows[i] do
        begin
            if not TickedInMenu then
                Add(Result, Format('View > R-factor Scale > %s was chosen and ' +
                    'is not ticked in the menu bar', [Caption]));
            if not TickedInPopup then
                Add(Result, Format('View > R-factor Scale > %s was chosen and ' +
                    'is not ticked in the progress chart''s right-click menu',
                    [Caption]));
            if OthersTicked > 0 then
                Add(Result, Format('after choosing %s, %d another R-factor ' +
                    'entr(ies) stayed ticked', [Caption, OthersTicked]));
            if ChartTitle <> AxisTitle then
                Add(Result, Format('after choosing %s, the progress chart is ' +
                    'titled "%s" while its axis is "%s"',
                    [Caption, AsRead(ChartTitle), AsRead(AxisTitle)]));
            if not SameValue(LastDrawn, Expected,
                1e-9 * Max(1, Abs(Expected))) then
                Add(Result, Format('after choosing %s, the progress chart drew ' +
                    'its last point at %g where the scale puts it at %g',
                    [Caption, LastDrawn, Expected]));
            if Pos(DrawnName + ' ', Line) = 0 then
                Add(Result, Format('after choosing %s, the line above the ' +
                    'chart does not give %s: "%s"',
                    [Caption, DrawnName, AsRead(Line)]));
            if (HistoryRow <> '') and (Pos(DrawnName + ' ', HistoryRow) = 0) then
                Add(Result, Format('after choosing %s, the History tab''s row ' +
                    'does not give %s: "%s"',
                    [Caption, DrawnName, AsRead(HistoryRow)]));
        end;
    //  THE ROWS LEFT AS THE LAST SCALE DREW THEM - the list not redrawn when
    //  the scale changed. Only the pair sees it: "R-factor " is inside
    //  "log10 R-factor " too.
    for i := 0 to High(ARows) do
        for j := i + 1 to High(ARows) do
            if (ARows[i].HistoryRow <> '') and
                (ARows[i].DrawnName <> ARows[j].DrawnName) and
                (ARows[i].HistoryRow = ARows[j].HistoryRow) then
                Add(Result, Format('%s and %s draw different things and leave ' +
                    'the same History row: "%s"',
                    [ARows[i].Caption, ARows[j].Caption,
                    AsRead(ARows[i].HistoryRow)]));
    //  THE FIRST REPORT: two scales that draw different things leaving the
    //  same words above the chart - the user switches and sees nothing change.
    for i := 0 to High(ARows) do
        for j := i + 1 to High(ARows) do
            if (ARows[i].DrawnName <> ARows[j].DrawnName) and
                (ARows[i].Line = ARows[j].Line) then
                Add(Result, Format('%s and %s draw different things and leave ' +
                    'the same line above the chart: "%s"',
                    [ARows[i].Caption, ARows[j].Caption, AsRead(ARows[i].Line)]))
            else if (ARows[i].DrawnName = ARows[j].DrawnName) and
                (ARows[i].Line = ARows[j].Line) and
                (ARows[i].AxisTitle <> ARows[j].AxisTitle) then
                Add(Result, Format('%s and %s leave the same line above the ' +
                    'chart, so switching between them changes nothing in it: ' +
                    '"%s"', [ARows[i].Caption, ARows[j].Caption,
                    AsRead(ARows[i].Line)]));
end;

function RFactorScaleSummary(AScaleCount, AFindingCount: longint;
    const ASkippedCaptions: string; AHistoryChecked: boolean): string;
begin
    Result := Format('ui: R-factor scale: switched between %d scale(s) through ' +
        'View > R-factor Scale, %d problem(s)', [AScaleCount, AFindingCount]);
    if ASkippedCaptions <> '' then
        Result := Result + Format('; %s not checked: it asks for formulas in a ' +
            'dialog', [ASkippedCaptions]);
    if not AHistoryChecked then
        Result := Result + '; the History tab''s rows not checked: it held no ' +
            'fitted model';
end;

function RFactorScaleNotChecked(const AReason: string): string;
begin
    Result := 'ui: R-factor scale: not checked: ' + AReason;
end;

function ModelTreeFindings(const AReading: TModelTreeReading): TUiFindings;
begin
    Result := nil;
    if not AReading.Ran or AReading.NoPicksTypeInBuild then
        Exit;
    if not AReading.PicksTypeSelected then
    begin
        //  NOT A PASS: the reading was taken where the old rule passed too.
        Add(Result, 'no curve type placed by picks could be selected, so ' +
            'whether the Model panel keeps a module''s nested rows under one ' +
            'was not checked');
        Exit;
    end;
    if not AReading.ChildUnderParent then
        Add(Result, 'with a curve type placed by picks selected, a module''s ' +
            'nested rows were not drawn as a tree in the Model panel, so the ' +
            'model''s hierarchy is not shown');
    if AReading.NodesDrawn <> AReading.CurveCount then
        Add(Result, Format('the Model panel drew %d row(s) for a model of %d ' +
            'curve(s), so a curve is listed twice or not at all',
            [AReading.NodesDrawn, AReading.CurveCount]));
    if not AReading.CurveFollowsRowAfterRefresh then
        Add(Result, 'with a row selected and its curve cleared, a refresh left ' +
            'the curve cleared under the highlighted row, so the panel and the ' +
            'commands on one curve disagree');
    if not AReading.CollapseSurvivedRefresh then
        Add(Result, 'a command refresh that changed nothing drew the Model ' +
            'panel again, so a pattern the user collapsed opened again');
    if AReading.StaleRowsShown then
        Add(Result, 'a module''s rows naming no curve of the open model were ' +
            'shown in the Model panel, so it described a model that is not open');
end;

function ModelTreeSummary(const AReading: TModelTreeReading): string;
const
    Shape: array[boolean] of string = ('flat', 'nested');
begin
    if not AReading.Ran then
        Result := 'ui: no module in this build describes the model, so how ' +
            'the Model panel draws a module''s nested rows was not checked'
    else if AReading.NoPicksTypeInBuild then
        Result := 'ui: this build has no curve type placed by picks that asks ' +
            'for nothing, so how the Model panel draws a module''s nested rows ' +
            'under one was not checked'
    else
        Result := Format('ui: with a curve type placed by picks selected, ' +
            'the Model panel drew a module''s nested rows as %d row(s), %s',
            [AReading.NodesDrawn, Shape[AReading.ChildUnderParent]]);
end;

function RowCommandProbe(APoints: longint): TRowCommandProbe;
begin
    Result.BoundFirst := 0;
    Result.BoundLast := APoints - 1;
    Result.PickFirst := APoints div 3;
    Result.PickSecond := (2 * APoints) div 3;
end;

function LiveProgressProbe(APoints, AExistingPositions: longint): TSampleIndexes;
var
    i: longint;
begin
    Result := nil;
    if AExistingPositions >= 3 then
        Exit;
    SetLength(Result, 11);
    for i := 1 to 11 do
        Result[i - 1] := (i * APoints) div 12;
end;

procedure CountToolButton(var AReading: TToolBarReading; AWidth, AHeight: longint;
    AIsSeparator: boolean);
begin
    if AIsSeparator then
        Exit;
    if (AReading.SmallestButtonWidth = 0) or
        (AWidth < AReading.SmallestButtonWidth) then
        AReading.SmallestButtonWidth := AWidth;
    if (AReading.SmallestButtonHeight = 0) or
        (AHeight < AReading.SmallestButtonHeight) then
        AReading.SmallestButtonHeight := AHeight;
end;

procedure KeepLonger(var AWidest: string; const AText: string);
begin
    if Length(AText) > Length(AWidest) then
        AWidest := AText;
end;

function StopOutcome(AStillRunning: boolean; ATookMs: QWord;
    ASeries: longint): string;
begin
    if AStillRunning then
        Result := Format('stop: the automatic run was still going %d ms ' +
            'after Stop', [ATookMs])
    else
        Result := Format('stop: the automatic run ended %d ms after Stop, ' +
            'leaving %d series on the chart', [ATookMs, ASeries]);
end;

end.
