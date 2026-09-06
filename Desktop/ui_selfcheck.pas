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
    SysUtils;

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
    TUiFindings = array of string;

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

implementation

function Add(var AFindings: TUiFindings; const AText: string): longint;
begin
    Result := Length(AFindings);
    SetLength(AFindings, Result + 1);
    AFindings[Result] := AText;
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
            Add(Result, Format('%s: the pane button has no hint', [ARows[i].Id]));

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

end.
