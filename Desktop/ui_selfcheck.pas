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
    SysUtils, series_style;

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

const
    { HOW SHORT IS TOO SHORT to place picks in. Two picks and an interval have to
      land on real samples, and the thirds this check picks them at have to be
      distinct - so a handful of points is not a profile for this purpose. }
    MinProfilePointsForPicks = 8;

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

{ The legend's verdict line, whatever the outcome. }
function LegendSummary(ARowCount, AFindingCount: longint): string;

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

end.
