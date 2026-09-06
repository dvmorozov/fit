// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which commands the window offers, and which of them are ticked.)

WHAT THIS REPLACES. The main form decided all of this by packing bit flags into
the `Tag` of each `TAction` and `TMenuItem` - clearing bit 0 on every command,
setting it for the ones a state allows, calling into a second method that cleared
and set some more, and finally unpacking every Tag into `Enabled` and `Checked`
with a pair of `if`s apiece. Some two hundred lines, spread over four methods
that had to be called in the right order, over widgets that only exist once a
window does.

The consequences of getting it wrong are not cosmetic. A command left enabled
during a fit lets the user start a second one; a command left disabled after an
operation finishes leaves the application looking hung, with nothing wrong except
that nothing can be done. A tick left behind after a picking mode ends says the
mode is on when it is off, and the next click on the entry reads as "leave"
rather than "enter" - so the user clicks, nothing happens, and only a second
click works. Every one of those has been a real defect here.

WHY A RECORD OF INPUTS. The old code read its inputs from wherever it happened to
be standing - a global application object, a service call, a widget's own
selection - in the middle of deciding. Gathering them first makes the decision a
function, and a function is a thing that can be asked what it would do.

THE TAG BITS ARE GONE. They were an accumulator, needed because the decision was
spread across methods that could not return anything. A record can be returned.
}
unit action_state;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_fit_service, fit_client, service_state_rules;

type
    { Every command whose availability the window derives rather than fixes.

      NAMED, not addressed by widget. This unit knows nothing about actions or
      menu items; the form maps these onto whichever it holds. }
    TUiCommand = (
        //  The document. New and Open need nothing open - they are how
        //  something comes to be open - so they are not under the guard below.
        ucNewProject, ucOpenProject, ucSaveProject, ucSaveProjectAs,
        //  The exports. ONE PER TABLE, replacing a single command that decided
        //  which table to write from the tab in front: a command whose label
        //  cannot say what it will do is a decision made from invisible state,
        //  and its `if`/`else if` had no else, so on any other tab it silently
        //  did nothing. Its flag was also cleared on every state change and set
        //  again nowhere, so it was never actually offered - see findings.md.
        ucExportCurveParameters, ucExportSummaryTable,
        //  File
        ucReloadData,
        //  Operation
        ucDoAllAutomatically, ucSmoothProfile, ucSubtractBackground,
        ucSubtractBackgroundBySelectedPoints,
        ucMinimizeNumberOfCurves, ucMinimizeDifference, ucStopFit,
        //  Dataset
        ucSelectIntervalBounds, ucSelectDataInterval, ucSelectEntireProfile,
        ucCurvePositions, ucBackground, ucRFactorIntervals,
        ucSelectCharacteristicPoints, ucSelectCurveBounds,
        //  The model's structure
        ucDeleteCurve,
        //  The results grid
        ucCopy, ucDelete, ucSelectAll,
        //  The chart
        ucZoomIn, ucZoomOut, ucViewMarkers, ucUseRule
        );

    { State of the results grid, which is what the three grid commands follow. }
    TResState = (
        { The grid is not the active control. }
        GridInvisible,
        { It is, but nothing is selected. }
        GridSelEmpty,
        { Some of it is selected. }
        GridSelNonEmpty,
        { All of it is. }
        GridSelAll
        );

    TCommandFlags = record
        Enabled: boolean;
        Checked: boolean;
    end;

    TCommandStates = array[TUiCommand] of TCommandFlags;

    { Everything the answer depends on, gathered before any of it is decided. }
    TUiInputs = record
        { Whether the last attempt to open a file succeeded. }
        Open: TOpenState;
        { What the engine says it is ready for. }
        Server: TFitServerState;
        { Whether a long operation is running. }
        Async: TAsyncState;
        { Which picking mode the user is in. }
        Selection: TSelMode;
        { Whether a sub-interval of the profile is in force. }
        SelectedAreaInForce: boolean;
        { How many points the current picking mode has collected. }
        SelectedPointCount: longint;
        { Whether the chart is drawing anything. }
        GraphHasSeries: boolean;
        { What the results grid's selection looks like. }
        Grid: TResState;
        { Whether the grid the commands follow is the CURVE TABLE.

          Delete removes CURVES, and only the curve table's rows name any: it
          used to be enabled from whichever grid had focus and then act on the
          curve table regardless, so it lit up over the data and silently
          rewrote the model's parameters. }
        GridIsCurveTable: boolean;
        { Whether the parameter table has any row to write out. Each export
          follows ITS OWN table, so neither is offered for an empty one and
          neither depends on which tab is in front. }
        ParameterTableHasRows: boolean;
        { The same for the summary datasheet. }
        SummaryTableHasRows: boolean;
        { Whether the Model panel has a row selected that stands for a curve.
          A ROW IS NOT ALWAYS A CURVE: the panel shows the model's structure,
          and a heading, an empty-text placeholder or a module's grouping row
          names nothing that can be deleted. }
        ModelRowNamesACurve: boolean;
    end;

{ Inputs describing a window with nothing open. Every field explicit, so a field
  added later cannot be silently left as whatever the stack held. }
function EmptyUiInputs: TUiInputs;

{ What every command's state should be. This is the whole decision. }
function CommandStates(const AInputs: TUiInputs): TCommandStates;

{ Which state the results grid is in, from what the widget reports.

  The comparison is against the FIXED row and column counts, not against zero: a
  grid's first data cell is after its headers, so "everything is selected" means
  from the first non-fixed cell to the last. }
function GridSelectionState(AGridIsActive: boolean;
    ASelLeft, ASelTop, ASelRight, ASelBottom: longint;
    AFixedCols, AFixedRows, AColCount, ARowCount: longint): TResState;


{ The mode in force after the user chooses AEntry while ACurrent is in force.

  EVERY PICKING ENTRY TOGGLES: choosing it while its own mode is already in force
  leaves that mode rather than re-entering it. That is what lets one entry say
  both "start" and "stop", and what lets a ticked entry be un-ticked by the same
  click. Choosing it from any OTHER mode enters it, so the user never has to
  leave one picking mode before starting another.

  THIS USED TO SAY "a manual picking entry toggles", and to answer AEntry
  unchanged for the rest - which contradicted the window, where all seven entries
  toggled by hand. Nothing failed, because the four hand-written ones never
  called this; the rule simply described a program that did not exist. The
  window's behaviour was the one the user sees, so it is the one kept, and the
  seven copies are now this one rule.

  Getting it wrong costs the user a click that appears to do nothing: the tick
  says the mode is on, the click reads as "leave", and only the second click
  starts anything. }
function ModeAfterPicking(ACurrent, AEntry: TSelMode): TSelMode;

type
    { The three manual-picking entries the window offers, as things rather than
      as three copies of one rule. A module's entry is not among them: the module
      named it, and the window is not allowed to know it by any other means. }
    TPickingEntry = (peBackground, peCurvePositions, peIntervalBounds);

const
    { The captions, which are the only place the user learns whether a mode is
      running. Two pairs and not three, because the bounds entry says the same
      thing the background one does - stated once here rather than by two
      identical branches in an event handler. }
    PICKING_START_CAPTION = 'Start Visual Selection';
    PICKING_STOP_CAPTION = 'Stop Visual Selection';
    POSITIONS_START_CAPTION = 'Start Visual Position Selection';
    POSITIONS_STOP_CAPTION = 'Stop Visual Position Selection';

{ Which mode the given entry starts. }
function PickingEntryMode(AEntry: TPickingEntry): TSelMode;

{ What the given entry reads while ACurrentMode is in force.

  "STOP" ON ITS OWN MODE AND "START" OTHERWISE, which is the visible half of
  ModeAfterPicking above: the same click that the caption describes is the one
  that rule decides. They were written out separately - the rule three times in
  three action handlers, the captions three times in a fourth method - so a
  caption could disagree with what its own entry would do, and the user would see
  "stop" on an entry that starts something. }
function PickingEntryCaption(AEntry: TPickingEntry;
    ACurrentMode: TSelMode): string;

implementation

function EmptyUiInputs: TUiInputs;
begin
    Result := Default(TUiInputs);
    Result.Open := OpenFailure;
    Result.Server := ProfileWaiting;
    Result.Async := AsyncDone;
    Result.Selection := ModeSelectNothing;
    Result.SelectedAreaInForce := False;
    Result.SelectedPointCount := 0;
    Result.GraphHasSeries := False;
    Result.Grid := GridInvisible;
    Result.GridIsCurveTable := False;
    Result.ModelRowNamesACurve := False;
end;

function ModeAfterPicking(ACurrent, AEntry: TSelMode): TSelMode;
begin
    //  No exception for any entry: the same click that starts a mode leaves it.
    if ACurrent = AEntry then
        Result := ModeSelectNothing
    else
        Result := AEntry;
end;

function PickingEntryMode(AEntry: TPickingEntry): TSelMode;
begin
    case AEntry of
        peBackground:     Result := ModeSelectBackground;
        peCurvePositions: Result := ModeSelectCurvePositions;
        peIntervalBounds: Result := ModeSelectRFactorBounds;
    else
        //  An entry added to the enum and not mapped here would otherwise start
        //  whatever the first branch happens to be.
        Result := ModeSelectNothing;
    end;
end;

function PickingEntryCaption(AEntry: TPickingEntry;
    ACurrentMode: TSelMode): string;
var
    Running: boolean;
begin
    Running := (PickingEntryMode(AEntry) <> ModeSelectNothing) and
        (ACurrentMode = PickingEntryMode(AEntry));
    if AEntry = peCurvePositions then
    begin
        if Running then
            Result := POSITIONS_STOP_CAPTION
        else
            Result := POSITIONS_START_CAPTION;
    end
    else
    begin
        if Running then
            Result := PICKING_STOP_CAPTION
        else
            Result := PICKING_START_CAPTION;
    end;
end;

function GridSelectionState(AGridIsActive: boolean;
    ASelLeft, ASelTop, ASelRight, ASelBottom: longint;
    AFixedCols, AFixedRows, AColCount, ARowCount: longint): TResState;
begin
    if not AGridIsActive then
        Exit(GridInvisible);

    //  A single cell is not a selection: it is where the cursor is, and copying
    //  or deleting it is not what the user meant by clicking in a table.
    if (ASelTop = ASelBottom) and (ASelLeft = ASelRight) then
        Exit(GridSelEmpty);

    if (ASelLeft = AFixedCols) and (ASelRight = AColCount - 1) and
        (ASelTop = AFixedRows) and (ASelBottom = ARowCount - 1) then
        Exit(GridSelAll);

    Result := GridSelNonEmpty;
end;

function CommandStates(const AInputs: TUiInputs): TCommandStates;
var
    Running: boolean;
    Opened: boolean;
begin
    Result := Default(TCommandStates);

    Running := AInputs.Async = AsyncWorks;
    Opened := AInputs.Open = OpenSuccess;

    //  ---- the document. ABOVE the "needs something open" guard, because these
    //  two are how something comes to be open at all: gating them on it would
    //  make the window unusable from the state it starts in.
    Result[ucNewProject].Enabled := True;
    Result[ucOpenProject].Enabled := True;

    //  ---- the chart. Independent of everything else: something is drawn or it
    //  is not, and a chart with nothing on it cannot be zoomed.
    Result[ucZoomIn].Enabled := AInputs.GraphHasSeries;
    Result[ucZoomOut].Enabled := AInputs.GraphHasSeries;
    Result[ucViewMarkers].Enabled := AInputs.GraphHasSeries;
    Result[ucUseRule].Enabled := AInputs.GraphHasSeries;

    //  ---- the results grid. Also independent: it follows the selection in the
    //  table and nothing else.
    case AInputs.Grid of
        GridInvisible:
        begin
            Result[ucCopy].Enabled := False;
            Result[ucDelete].Enabled := False;
            Result[ucSelectAll].Enabled := False;
        end;
        GridSelEmpty:
        begin
            Result[ucCopy].Enabled := False;
            Result[ucDelete].Enabled := False;
            Result[ucSelectAll].Enabled := True;
        end;
        GridSelNonEmpty:
        begin
            Result[ucCopy].Enabled := True;
            //  A selection of curves, or a selection of numbers? Only the
            //  curve table's rows name curves, and Delete removes curves.
            Result[ucDelete].Enabled := AInputs.GridIsCurveTable;
            Result[ucSelectAll].Enabled := True;
        end;
        GridSelAll:
        begin
            Result[ucCopy].Enabled := True;
            Result[ucDelete].Enabled := AInputs.GridIsCurveTable;
            //  Nothing left to select.
            Result[ucSelectAll].Enabled := False;
        end;
    end;

    //  ---- everything that needs a document. With nothing open the whole of the
    //  File, Operation and Dataset machinery is off, which is the state the
    //  window starts in.
    if not Opened then
        Exit;

    Result[ucReloadData].Enabled := True;
    //  Saving needs something to save; saving an empty window would write a
    //  project with nothing in it, which nobody means to do.
    Result[ucSaveProject].Enabled := True;
    Result[ucSaveProjectAs].Enabled := True;
    //  EACH EXPORT FOLLOWS ITS OWN TABLE. Offered exactly when that table has
    //  something in it - not when whichever table happens to be in front does.
    Result[ucExportCurveParameters].Enabled := AInputs.ParameterTableHasRows;
    Result[ucExportSummaryTable].Enabled := AInputs.SummaryTableHasRows;
    Result[ucDoAllAutomatically].Enabled := True;
    Result[ucSmoothProfile].Enabled := True;
    //  The background submenu and the two commands under it that are not
    //  otherwise conditioned move together. Subtracting BY the picked points is
    //  one of them: the window has never made it depend on any having been
    //  picked, whatever the old code appeared to say - see findings.md.
    Result[ucSubtractBackground].Enabled := True;
    Result[ucSubtractBackgroundBySelectedPoints].Enabled := True;

    //  ASKED, NOT RESTATED. The window's rule and the service's live next to
    //  each other in service_state_rules, because they differ at one state on
    //  purpose and a difference stated in two processes is a difference nobody
    //  can see.
    if FitIsOffered(AInputs.Server) then
    begin
        Result[ucMinimizeNumberOfCurves].Enabled := True;
        Result[ucMinimizeDifference].Enabled := True;
    end;

    Result[ucSelectIntervalBounds].Enabled := True;
    Result[ucCurvePositions].Enabled := True;
    Result[ucBackground].Enabled := True;
    Result[ucRFactorIntervals].Enabled := True;
    Result[ucSelectCharacteristicPoints].Enabled := True;
    Result[ucSelectCurveBounds].Enabled := True;

    //  Deleting one curve needs a curve to delete, and the only thing that
    //  names one is the selected row of the Model panel. Offered disabled
    //  rather than hidden when nothing is selected: an entry that vanishes
    //  tells the user nothing about why.
    Result[ucDeleteCurve].Enabled := AInputs.ModelRowNamesACurve;

    //  ---- what the picking mode adds. Ticks, the two "remove" entries, and the
    //  two commands that only make sense once enough has been picked.
    case AInputs.Selection of
        ModeSelectNothing:
            //  Back to the whole profile - offered only when a sub-interval is
            //  actually in force, or it is a command that does nothing.
            Result[ucSelectEntireProfile].Enabled := AInputs.SelectedAreaInForce;
        ModeSelectIntervalBounds:
        begin
            Result[ucSelectIntervalBounds].Checked := True;
            //  An interval needs BOTH ends. Offering it after one pick would
            //  select an area with no second edge.
            Result[ucSelectDataInterval].Enabled :=
                AInputs.SelectedPointCount = 2;
        end;
        ModeSelectCharacteristicPoints:
            Result[ucSelectCharacteristicPoints].Checked := True;
        ModeSelectCurveBounds:
            Result[ucSelectCurveBounds].Checked := True;
        //  The three picking modes that collect a set of their own - the
        //  background, the positions and the intervals - add nothing here. The
        //  old code set flags for a "Remove" entry apiece and for subtracting by
        //  the picked points, and nothing ever read them; what the entries
        //  actually follow is the submenu they sit in. Offering each removal
        //  only while its own set is being picked is the evident intent and a
        //  deliberate change to what the user sees, so it is recorded in
        //  findings.md rather than smuggled in here.
    end;

    //  ---- a long operation is running. LAST, because it overrides: whatever
    //  the state above allows, during a fit the only thing that may be done is
    //  stopping it. Applied as a final pass rather than woven into the cases
    //  above, so that a command added later is off during a fit by default.
    if Running then
    begin
        Result[ucDoAllAutomatically].Enabled := False;
        Result[ucSmoothProfile].Enabled := False;
        Result[ucSubtractBackground].Enabled := False;
        Result[ucSelectIntervalBounds].Enabled := False;
        Result[ucSelectDataInterval].Enabled := False;
        Result[ucSelectEntireProfile].Enabled := False;
        Result[ucCurvePositions].Enabled := False;
        Result[ucBackground].Enabled := False;
        Result[ucRFactorIntervals].Enabled := False;
        Result[ucSelectCharacteristicPoints].Enabled := False;
        Result[ucSelectCurveBounds].Enabled := False;
        Result[ucSubtractBackgroundBySelectedPoints].Enabled := False;
        Result[ucDeleteCurve].Enabled := False;
        Result[ucMinimizeNumberOfCurves].Enabled := False;
        Result[ucMinimizeDifference].Enabled := False;
        Result[ucStopFit].Enabled := True;
    end
    else
        //  Nothing to stop.
        Result[ucStopFit].Enabled := False;
end;

end.
