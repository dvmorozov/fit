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
    toolbar_metrics;

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
        { The curve the gesture chose, where the check knows it; '' where it
          does not. A selection that did not follow the gesture leaves every
          count consistent - with the old curve - so only this can see it. }
        ExpectedCurveId: string;
        { Series drawn for CurveId; how many of them read as highlighted; how
          many series drawn for anything else show any emphasis at all. }
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
    end;

    TToolBarReadings = array of TToolBarReading;

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

{ The verdict for the framework's own rows, whatever the outcome - and for the
  contributor rows, which is a separate line because they are a separate defect
  and a single count would hide one behind the other. }
function RowCommandSummary(AOffered: boolean; AEntryCount: longint): string;
function ContributorRowSummary(AOffered: boolean; AEntryCount: longint): string;
{ Adds one series of the chart to a reading: its owner, and how it is drawn.
  A series drawn for the selected curve counts as highlighted only when it is
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
    end;

{ What is wrong with AReading, in the words the log carries. None for a reading
  that did not run - its verdict says so. }
function ModelTreeFindings(const AReading: TModelTreeReading): TUiFindings;

{ The verdict line, written whatever the outcome. Both of its sentences name "a
  module's nested rows", which is what check-ui requires of the log. }
function ModelTreeSummary(const AReading: TModelTreeReading): string;

implementation

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
            'checked - the task must pass /INFILE'
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

procedure CountSeriesHighlight(var AReading: THighlightReading;
    const AOwnerCurveId: string; ALineWidth: longint; AOnTop: boolean);
begin
    //  An empty selection owns nothing, so with none every series is counted
    //  as somebody else's - which is what makes a leftover highlight visible.
    if (AReading.CurveId <> '') and (AOwnerCurveId = AReading.CurveId) then
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
    if AReading.CurveId = '' then
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
        Result := Format('ui: %s, the selected curve has %d of %d series ' +
            'highlighted on the chart, and %d other series highlighted',
            [AWhen, AReading.OwnedHighlighted, AReading.OwnedCount,
            AReading.OtherHighlighted])
    else
        Result := Format('ui: %s, %d series highlighted on the chart',
            [AWhen, AReading.OtherHighlighted]);
end;

function RowCommandSummary(AOffered: boolean; AEntryCount: longint): string;
begin
    Result := Format('ui: the Model panel offers %d of %d context entr(ies) ' +
        'over a row naming a curve', [Ord(AOffered) * AEntryCount, AEntryCount]);
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

function ModelTreeFindings(const AReading: TModelTreeReading): TUiFindings;
begin
    Result := nil;
    if not AReading.Ran then
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
    else
        Result := Format('ui: with a curve type placed by picks selected, ' +
            'the Model panel drew a module''s nested rows as %d row(s), %s',
            [AReading.NodesDrawn, Shape[AReading.ChildUnderParent]]);
end;

end.
