// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which commands the window offers, and which of them are ticked.)

WHAT THESE DEFEND. Every failure here is one the user experiences as the program
being broken rather than as a wrong answer:

- a command left enabled during a fit lets a second fit be started on top of the
  first;
- a command left disabled after an operation finishes leaves the window looking
  hung, with nothing wrong except that nothing can be done;
- a tick left behind after a picking mode ends says the mode is on when it is
  off, so the next click on that entry reads as "leave" and the user has to
  click twice.

All three have happened here. None of them could be tested: the whole decision
was bit flags packed into the `Tag` of widgets that exist only once a window
does, spread over four methods that had to run in the right order.

The extraction is action_state; the form gathers the inputs and applies the
answer. Nothing in this file opens a window.
}
unit testcase_action_state;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    action_state, int_fit_service, fit_client;

type
    TActionStateTest = class(TTestCase)
    private
        FInputs: TUiInputs;
        FStates: TCommandStates;
        { Decides from the inputs as they now stand. }
        procedure Decide;
        function IsEnabled(ACommand: TUiCommand): boolean;
        function IsChecked(ACommand: TUiCommand): boolean;
        { A window with a file open, nothing running, nothing being picked. }
        procedure OpenAndIdle;
    protected
        procedure SetUp; override;
    published
        //  Nothing open.
        procedure WithNothingOpenTheOperationsAreOff;
        procedure WithNothingOpenTheDatasetCommandsAreOff;
        procedure WithNothingOpenTheChartStillFollowsTheChart;

        //  A file open.
        procedure OpeningAFileOffersTheOperations;
        procedure OpeningAFileOffersThePickingModes;

        //  Whether a fit may start.
        procedure AFitIsOfferedWhenTheEngineIsReady;
        procedure AFitIsOfferedWhenTheEngineWillCompleteTheDataItself;
        procedure AFitIsOfferedAgainAfterOneFinished;
        procedure AFitIsNotOfferedBeforeThereIsData;
        procedure AFitIsNotOfferedWhileTheBackgroundIsStillThere;

        //  While something is running.
        procedure DuringAFitOnlyStoppingIsOffered;
        procedure DuringAFitTheDatasetCommandsAreOff;
        procedure StoppingIsOfferedOnlyWhileSomethingRuns;
        procedure WhenAFitEndsTheCommandsComeBack;

        //  The picking modes.
        procedure EachPickingModeTicksItsOwnEntryAndNoOther;
        procedure NoModeTicksNothing;
        procedure AnIntervalNeedsBothEndsBeforeItCanBeSelected;
        procedure OneEndIsNotAnInterval;
        procedure SubtractingByPickedPointsFollowsItsSubmenu;

        //  Going back to the whole profile.
        procedure TheWholeProfileIsOfferedOnlyWhenAnIntervalIsInForce;

        //  The results grid.
        procedure AGridThatIsNotActiveOffersNothing;
        procedure ACursorIsNotASelection;
        procedure APartialSelectionCanBeCopiedAndExtended;
        procedure ACompleteSelectionCannotBeExtended;
        procedure OnlyTheCurveTableCanHaveCurvesDeletedFromIt;
        procedure AndTheWholeOfItToo;
        procedure DeletingACurveNeedsTheModelPanelToNameOne;
        procedure AndNotWhileAFitIsRunning;
        procedure TheFullSelectionIsMeasuredFromTheFirstDataCell;

        //  The chart.
        procedure AnEmptyChartCannotBeZoomed;
        procedure AChartWithSeriesCan;

        //  What is never offered.
        //  THE DOCUMENT COMMANDS. New and Open need nothing open - they are
        //  how something gets opened - and Save needs something to save.
        procedure StartingANewProjectIsAlwaysOffered;
        procedure OpeningAProjectIsAlwaysOffered;
        procedure SavingIsOfferedOnlyOnceSomethingIsOpen;

        //  THE EXPORTS. Each names its own table, so each follows that table
        //  rather than whichever one happens to be in front.
        procedure AnExportIsNotOfferedForAnEmptyTable;
        procedure EachExportFollowsItsOwnTable;
        procedure NeitherExportIsOfferedWithNothingOpen;

        //  SELF-ENFORCING, and the one that would have caught the defect
        //  this file used to pin instead of fixing.
        procedure EveryCommandCanBeOfferedUnderSomeConditions;

        //  Which entry toggles, and what it toggles to.
        procedure ChoosingAnEntryWhileItsOwnModeIsOnLeavesIt;
        procedure ChoosingItFromAnotherModeEntersIt;
        procedure ChoosingItFromNothingEntersIt;
        procedure SwitchingBetweenTwoPickingModesNeedsNoStopFirst;
        procedure AnEntryThatDoesNotToggleAlwaysMeansItself;
        procedure LeavingAlwaysMeansNothingRatherThanThePrevious;

        //  What the picking entries read.
        procedure AnEntrySaysStopOnlyWhileItsOwnModeRuns;
        procedure EveryEntryHasItsOwnMode;
        procedure ThePositionsEntryNamesWhatItPicks;
        procedure ACaptionAgreesWithWhatTheEntryWouldDo;
        procedure NoEntryIsEverUncaptioned;
        procedure AnUnmappedEntryStartsNothingAndReadsAsStart;
    end;

implementation

procedure TActionStateTest.SetUp;
begin
    FInputs := EmptyUiInputs;
end;

procedure TActionStateTest.Decide;
begin
    FStates := CommandStates(FInputs);
end;

function TActionStateTest.IsEnabled(ACommand: TUiCommand): boolean;
begin
    Result := FStates[ACommand].Enabled;
end;

function TActionStateTest.IsChecked(ACommand: TUiCommand): boolean;
begin
    Result := FStates[ACommand].Checked;
end;

procedure TActionStateTest.OpenAndIdle;
begin
    FInputs := EmptyUiInputs;
    FInputs.Open := OpenSuccess;
    FInputs.Server := ReadyForFit;
    FInputs.Async := AsyncDone;
    FInputs.Selection := ModeSelectNothing;
end;

{ ---- nothing open ---------------------------------------------------------- }

procedure TActionStateTest.WithNothingOpenTheOperationsAreOff;
begin
    //  THE STATE THE WINDOW STARTS IN. Every one of these needs data, and
    //  offering them before there is any is how a first-run crash happens.
    Decide;
    AssertFalse('reload', IsEnabled(ucReloadData));
    AssertFalse('do it all', IsEnabled(ucDoAllAutomatically));
    AssertFalse('smooth', IsEnabled(ucSmoothProfile));
    AssertFalse('background', IsEnabled(ucSubtractBackground));
    AssertFalse('fit', IsEnabled(ucMinimizeDifference));
    AssertFalse('reduce', IsEnabled(ucMinimizeNumberOfCurves));
end;

procedure TActionStateTest.WithNothingOpenTheDatasetCommandsAreOff;
begin
    Decide;
    AssertFalse('interval bounds', IsEnabled(ucSelectIntervalBounds));
    AssertFalse('the data interval', IsEnabled(ucSelectDataInterval));
    AssertFalse('the whole profile', IsEnabled(ucSelectEntireProfile));
    AssertFalse('positions', IsEnabled(ucCurvePositions));
    AssertFalse('background', IsEnabled(ucBackground));
    AssertFalse('intervals', IsEnabled(ucRFactorIntervals));
    AssertFalse('characteristic points',
        IsEnabled(ucSelectCharacteristicPoints));
    AssertFalse('curve bounds', IsEnabled(ucSelectCurveBounds));
end;

procedure TActionStateTest.WithNothingOpenTheChartStillFollowsTheChart;
begin
    //  The chart commands are INDEPENDENT of the document: a chart can hold
    //  something with no file open - a module's own series, say - and refusing
    //  to zoom it because no profile is loaded would be arbitrary.
    FInputs.GraphHasSeries := True;
    Decide;
    AssertTrue('zoom in', IsEnabled(ucZoomIn));
    AssertTrue('zoom out', IsEnabled(ucZoomOut));
end;

{ ---- a file open ----------------------------------------------------------- }

procedure TActionStateTest.OpeningAFileOffersTheOperations;
begin
    OpenAndIdle;
    Decide;
    AssertTrue('reload', IsEnabled(ucReloadData));
    AssertTrue('do it all', IsEnabled(ucDoAllAutomatically));
    AssertTrue('smooth', IsEnabled(ucSmoothProfile));
    AssertTrue('background', IsEnabled(ucSubtractBackground));
end;

procedure TActionStateTest.OpeningAFileOffersThePickingModes;
begin
    OpenAndIdle;
    Decide;
    AssertTrue('interval bounds', IsEnabled(ucSelectIntervalBounds));
    AssertTrue('positions', IsEnabled(ucCurvePositions));
    AssertTrue('background', IsEnabled(ucBackground));
    AssertTrue('intervals', IsEnabled(ucRFactorIntervals));
    AssertTrue('characteristic points',
        IsEnabled(ucSelectCharacteristicPoints));
    AssertTrue('curve bounds', IsEnabled(ucSelectCurveBounds));
end;

{ ---- whether a fit may start ----------------------------------------------- }

procedure TActionStateTest.AFitIsOfferedWhenTheEngineIsReady;
begin
    OpenAndIdle;
    FInputs.Server := ReadyForFit;
    Decide;
    AssertTrue('fit', IsEnabled(ucMinimizeDifference));
    AssertTrue('reduce', IsEnabled(ucMinimizeNumberOfCurves));
end;

procedure TActionStateTest.AFitIsOfferedWhenTheEngineWillCompleteTheDataItself;
begin
    //  ReadyForAutoFit COUNTS. The data such a fit still needs is completed
    //  automatically, so refusing here would disable the button whose whole job
    //  is to do that.
    OpenAndIdle;
    FInputs.Server := ReadyForAutoFit;
    Decide;
    AssertTrue('fit', IsEnabled(ucMinimizeDifference));
end;

procedure TActionStateTest.AFitIsOfferedAgainAfterOneFinished;
begin
    //  Refitting from the fitted parameters is an ordinary thing to do - it is
    //  how a fit is refined - so a finished fit must not disable the command
    //  that produced it.
    OpenAndIdle;
    FInputs.Server := Finished;
    Decide;
    AssertTrue('fit again', IsEnabled(ucMinimizeDifference));
end;

procedure TActionStateTest.AFitIsNotOfferedBeforeThereIsData;
begin
    OpenAndIdle;
    FInputs.Server := ProfileWaiting;
    Decide;
    AssertFalse('nothing to fit', IsEnabled(ucMinimizeDifference));
end;

procedure TActionStateTest.AFitIsNotOfferedWhileTheBackgroundIsStillThere;
begin
    //  Fitting a profile that still has its background in it fits the
    //  background, which produces a plausible model of the wrong thing.
    OpenAndIdle;
    FInputs.Server := BackNotRemoved;
    Decide;
    AssertFalse('background first', IsEnabled(ucMinimizeDifference));
end;

{ ---- while something is running -------------------------------------------- }

procedure TActionStateTest.DuringAFitOnlyStoppingIsOffered;
begin
    //  THE OVERRIDE. Whatever the state below allows, a running operation
    //  leaves only the one command that ends it - starting a second fit on top
    //  of a running one is not something the engine survives.
    OpenAndIdle;
    FInputs.Async := AsyncWorks;
    Decide;
    AssertTrue('stop', IsEnabled(ucStopFit));
    AssertFalse('fit', IsEnabled(ucMinimizeDifference));
    AssertFalse('reduce', IsEnabled(ucMinimizeNumberOfCurves));
    AssertFalse('do it all', IsEnabled(ucDoAllAutomatically));
    AssertFalse('smooth', IsEnabled(ucSmoothProfile));
    AssertFalse('background', IsEnabled(ucSubtractBackground));
end;

procedure TActionStateTest.DuringAFitTheDatasetCommandsAreOff;
begin
    //  Editing the data under a running fit changes what is being fitted while
    //  it is being fitted.
    OpenAndIdle;
    FInputs.Async := AsyncWorks;
    Decide;
    AssertFalse('interval bounds', IsEnabled(ucSelectIntervalBounds));
    AssertFalse('the data interval', IsEnabled(ucSelectDataInterval));
    AssertFalse('the whole profile', IsEnabled(ucSelectEntireProfile));
    AssertFalse('positions', IsEnabled(ucCurvePositions));
    AssertFalse('background', IsEnabled(ucBackground));
    AssertFalse('intervals', IsEnabled(ucRFactorIntervals));
    AssertFalse('characteristic points',
        IsEnabled(ucSelectCharacteristicPoints));
    AssertFalse('curve bounds', IsEnabled(ucSelectCurveBounds));
end;

procedure TActionStateTest.StoppingIsOfferedOnlyWhileSomethingRuns;
begin
    //  A Stop that is always available is a Stop the user presses when nothing
    //  is running, and then wonders what it did.
    OpenAndIdle;
    Decide;
    AssertFalse('idle', IsEnabled(ucStopFit));
    FInputs.Async := AsyncStart;
    Decide;
    AssertFalse('starting is not yet running', IsEnabled(ucStopFit));
    FInputs.Async := AsyncWorks;
    Decide;
    AssertTrue('running', IsEnabled(ucStopFit));
end;

procedure TActionStateTest.WhenAFitEndsTheCommandsComeBack;
begin
    //  THE "APPLICATION LOOKS HUNG" DEFECT, in one assertion: after an
    //  operation finishes, everything it disabled has to come back, or there is
    //  nothing wrong except that nothing can be done.
    OpenAndIdle;
    FInputs.Async := AsyncWorks;
    Decide;
    AssertFalse('off while running', IsEnabled(ucDoAllAutomatically));
    FInputs.Async := AsyncDone;
    Decide;
    AssertTrue('back afterwards', IsEnabled(ucDoAllAutomatically));
    AssertTrue('and so is fitting', IsEnabled(ucMinimizeDifference));
    AssertFalse('and stopping is not', IsEnabled(ucStopFit));
end;

{ ---- the picking modes ----------------------------------------------------- }

procedure TActionStateTest.EachPickingModeTicksItsOwnEntryAndNoOther;
begin
    //  ONE TICK AT A TIME. Two entries ticked says two modes are running, and
    //  the user cannot tell which of them their next click goes to.
    OpenAndIdle;
    FInputs.Selection := ModeSelectIntervalBounds;
    Decide;
    AssertTrue('bounds', IsChecked(ucSelectIntervalBounds));
    AssertFalse('not characteristic points',
        IsChecked(ucSelectCharacteristicPoints));
    AssertFalse('not curve bounds', IsChecked(ucSelectCurveBounds));

    FInputs.Selection := ModeSelectCharacteristicPoints;
    Decide;
    AssertTrue('characteristic points',
        IsChecked(ucSelectCharacteristicPoints));
    AssertFalse('not bounds', IsChecked(ucSelectIntervalBounds));

    FInputs.Selection := ModeSelectCurveBounds;
    Decide;
    AssertTrue('curve bounds', IsChecked(ucSelectCurveBounds));
    AssertFalse('not characteristic points',
        IsChecked(ucSelectCharacteristicPoints));
end;

procedure TActionStateTest.NoModeTicksNothing;
begin
    //  THE TICK LEFT BEHIND. A mode ends in ways its entry never hears about -
    //  another mode starting, a profile being loaded - and a tick that survives
    //  makes the next click read as "leave" instead of "enter", so nothing
    //  happens until the user clicks a second time.
    OpenAndIdle;
    FInputs.Selection := ModeSelectNothing;
    Decide;
    AssertFalse('bounds', IsChecked(ucSelectIntervalBounds));
    AssertFalse('characteristic points',
        IsChecked(ucSelectCharacteristicPoints));
    AssertFalse('curve bounds', IsChecked(ucSelectCurveBounds));
end;

procedure TActionStateTest.AnIntervalNeedsBothEndsBeforeItCanBeSelected;
begin
    OpenAndIdle;
    FInputs.Selection := ModeSelectIntervalBounds;
    FInputs.SelectedPointCount := 2;
    Decide;
    AssertTrue('two ends make an interval', IsEnabled(ucSelectDataInterval));
end;

procedure TActionStateTest.OneEndIsNotAnInterval;
begin
    //  Offering it after one pick would select an area with no second edge.
    OpenAndIdle;
    FInputs.Selection := ModeSelectIntervalBounds;
    FInputs.SelectedPointCount := 1;
    Decide;
    AssertFalse('one end', IsEnabled(ucSelectDataInterval));
    FInputs.SelectedPointCount := 0;
    Decide;
    AssertFalse('no ends', IsEnabled(ucSelectDataInterval));
end;

procedure TActionStateTest.SubtractingByPickedPointsFollowsItsSubmenu;
begin
    //  AS THE WINDOW HAS ALWAYS BEHAVED. The old code carried flags making this
    //  depend on some background points having been picked, and nothing ever
    //  read them - see findings.md. It follows the background submenu, and the
    //  only thing that turns it off is a running operation.
    OpenAndIdle;
    Decide;
    AssertTrue('offered with the submenu',
        IsEnabled(ucSubtractBackgroundBySelectedPoints));
    FInputs.Selection := ModeSelectBackground;
    Decide;
    AssertTrue('and still offered while picking',
        IsEnabled(ucSubtractBackgroundBySelectedPoints));
    FInputs.Async := AsyncWorks;
    Decide;
    AssertFalse('but not during a fit',
        IsEnabled(ucSubtractBackgroundBySelectedPoints));
end;

{ ---- back to the whole profile --------------------------------------------- }

procedure TActionStateTest.TheWholeProfileIsOfferedOnlyWhenAnIntervalIsInForce;
begin
    //  A command that does nothing is worse than a command that is absent: the
    //  user presses it and learns nothing about why the display did not change.
    OpenAndIdle;
    FInputs.SelectedAreaInForce := False;
    Decide;
    AssertFalse('nothing to go back from', IsEnabled(ucSelectEntireProfile));
    FInputs.SelectedAreaInForce := True;
    Decide;
    AssertTrue('an interval is in force', IsEnabled(ucSelectEntireProfile));
end;

{ ---- the results grid ------------------------------------------------------ }

procedure TActionStateTest.AGridThatIsNotActiveOffersNothing;
begin
    //  Copy and Delete act on the grid's selection, so with no grid in front of
    //  the user they would act on a selection nobody can see.
    AssertTrue('invisible', GridSelectionState(False, 0, 0, 0, 0, 1, 1, 5, 5) =
        GridInvisible);
    FInputs.Grid := GridInvisible;
    Decide;
    AssertFalse('copy', IsEnabled(ucCopy));
    AssertFalse('delete', IsEnabled(ucDelete));
    AssertFalse('select all', IsEnabled(ucSelectAll));
end;

procedure TActionStateTest.ACursorIsNotASelection;
begin
    //  A single cell is where the cursor is, not something the user chose.
    //  Copying it is not what clicking in a table means.
    AssertTrue('one cell', GridSelectionState(True, 2, 3, 2, 3, 1, 1, 5, 5) =
        GridSelEmpty);
    FInputs.Grid := GridSelEmpty;
    Decide;
    AssertFalse('copy', IsEnabled(ucCopy));
    AssertFalse('delete', IsEnabled(ucDelete));
    AssertTrue('but everything can still be selected', IsEnabled(ucSelectAll));
end;

procedure TActionStateTest.APartialSelectionCanBeCopiedAndExtended;
begin
    AssertTrue('some of it',
        GridSelectionState(True, 1, 1, 2, 3, 1, 1, 5, 5) = GridSelNonEmpty);
    FInputs.Grid := GridSelNonEmpty;
    //  ON THE CURVE TABLE, because Delete removes curves and that is the one
    //  grid whose rows name any.
    FInputs.GridIsCurveTable := True;
    Decide;
    AssertTrue('copy', IsEnabled(ucCopy));
    AssertTrue('delete', IsEnabled(ucDelete));
    AssertTrue('and extend', IsEnabled(ucSelectAll));
end;

procedure TActionStateTest.OnlyTheCurveTableCanHaveCurvesDeletedFromIt;
begin
    //  THE DEFECT THIS PINS. Delete used to be enabled from whichever grid had
    //  focus and then acted on the curve table regardless - so it lit up over
    //  the data and silently rewrote the model's parameters, and the rows came
    //  back on the next refresh with nothing said.
    FInputs.Grid := GridSelNonEmpty;
    FInputs.GridIsCurveTable := False;
    Decide;
    AssertTrue('the numbers can still be copied', IsEnabled(ucCopy));
    AssertFalse('but no curve is named here', IsEnabled(ucDelete));
end;

procedure TActionStateTest.AndTheWholeOfItToo;
begin
    FInputs.Grid := GridSelAll;
    FInputs.GridIsCurveTable := False;
    Decide;
    AssertFalse('still no curve named', IsEnabled(ucDelete));
end;

procedure TActionStateTest.DeletingACurveNeedsTheModelPanelToNameOne;
begin
    //  The context entry over the Model panel follows the panel's selection,
    //  not a grid's: a row that stands for no curve offers nothing.
    FInputs.Open := OpenSuccess;
    FInputs.ModelRowNamesACurve := False;
    Decide;
    AssertFalse('nothing selected', IsEnabled(ucDeleteCurve));
    FInputs.ModelRowNamesACurve := True;
    Decide;
    AssertTrue('a curve selected', IsEnabled(ucDeleteCurve));
end;

procedure TActionStateTest.AndNotWhileAFitIsRunning;
begin
    FInputs.Open := OpenSuccess;
    FInputs.ModelRowNamesACurve := True;
    FInputs.Async := AsyncWorks;
    Decide;
    //  Editing the model under a running optimiser is what the whole
    //  during-a-fit override exists to prevent.
    AssertFalse('not while a fit runs', IsEnabled(ucDeleteCurve));
end;

procedure TActionStateTest.ACompleteSelectionCannotBeExtended;
begin
    AssertTrue('all of it',
        GridSelectionState(True, 1, 1, 4, 4, 1, 1, 5, 5) = GridSelAll);
    FInputs.Grid := GridSelAll;
    FInputs.GridIsCurveTable := True;
    Decide;
    AssertTrue('copy', IsEnabled(ucCopy));
    AssertTrue('delete', IsEnabled(ucDelete));
    AssertFalse('nothing left to select', IsEnabled(ucSelectAll));
end;

procedure TActionStateTest.TheFullSelectionIsMeasuredFromTheFirstDataCell;
begin
    //  FROM THE FIXED COUNTS, not from zero. A grid's first data cell is after
    //  its headers, so comparing against zero would mean "everything is
    //  selected" never becomes true and Select All stays offered forever.
    AssertTrue('with two fixed rows and columns',
        GridSelectionState(True, 2, 2, 9, 9, 2, 2, 10, 10) = GridSelAll);
    AssertTrue('one row short is not all of it',
        GridSelectionState(True, 2, 2, 9, 8, 2, 2, 10, 10) = GridSelNonEmpty);
end;

{ ---- the chart ------------------------------------------------------------- }

procedure TActionStateTest.AnEmptyChartCannotBeZoomed;
begin
    FInputs.GraphHasSeries := False;
    Decide;
    AssertFalse('zoom in', IsEnabled(ucZoomIn));
    AssertFalse('zoom out', IsEnabled(ucZoomOut));
    AssertFalse('markers', IsEnabled(ucViewMarkers));
    AssertFalse('the rule', IsEnabled(ucUseRule));
end;

procedure TActionStateTest.AChartWithSeriesCan;
begin
    FInputs.GraphHasSeries := True;
    Decide;
    AssertTrue('zoom in', IsEnabled(ucZoomIn));
    AssertTrue('zoom out', IsEnabled(ucZoomOut));
    AssertTrue('markers', IsEnabled(ucViewMarkers));
    AssertTrue('the rule', IsEnabled(ucUseRule));
end;

procedure TActionStateTest.EveryCommandCanBeOfferedUnderSomeConditions;
var
    Cmd: TUiCommand;
    Ever: array[TUiCommand] of boolean;

    { Records what the current inputs make available. }
    procedure Note;
    var
        C: TUiCommand;
    begin
        Decide;
        for C := Low(TUiCommand) to High(TUiCommand) do
            if IsEnabled(C) then
                Ever[C] := True;
    end;

begin
    //  THE DEFECT THIS GUARDS IS A REAL ONE AND WAS HERE FOR YEARS.
    //  ucSaveModelAsText had its flag cleared on every state change and set
    //  again nowhere, so the one export this program had was unreachable - and
    //  the test that noticed PINNED it as behaviour rather than failing on it,
    //  because nothing said a command must be reachable at all.
    //
    //  A command that can never be offered is either a defect or a row nobody
    //  deleted. Either way the answer is not "leave it", and this fails until
    //  somebody decides which.
    for Cmd := Low(TUiCommand) to High(TUiCommand) do
        Ever[Cmd] := False;

    //  A handful of states, chosen to be between them everything the window
    //  can be in: nothing open; open and idle; open with a fit possible; a
    //  chart drawn; each grid selection; a curve row picked; both results
    //  tables with rows in them.
    FInputs := EmptyUiInputs;
    Note;

    OpenAndIdle;
    Note;

    OpenAndIdle;
    FInputs.GraphHasSeries := True;
    FInputs.ParameterTableHasRows := True;
    FInputs.SummaryTableHasRows := True;
    FInputs.ModelRowNamesACurve := True;
    FInputs.SelectedAreaInForce := True;
    Note;

    OpenAndIdle;
    FInputs.Grid := GridSelNonEmpty;
    FInputs.GridIsCurveTable := True;
    Note;

    OpenAndIdle;
    FInputs.Grid := GridSelEmpty;
    Note;

    OpenAndIdle;
    FInputs.Server := Finished;
    Note;

    OpenAndIdle;
    FInputs.Async := AsyncWorks;
    Note;

    //  MID-PICK, with both ends of an interval down. Reached only from here,
    //  which is the point of walking states rather than commands: the rule for
    //  "select this interval" needs a picking mode AND a count, and no state
    //  above produces either.
    OpenAndIdle;
    FInputs.Selection := ModeSelectIntervalBounds;
    FInputs.SelectedPointCount := 2;
    Note;

    //  And each of the other picking modes, so a command that only its own mode
    //  offers is reachable too.
    OpenAndIdle;
    FInputs.Selection := ModeSelectCharacteristicPoints;
    Note;
    OpenAndIdle;
    FInputs.Selection := ModeSelectCurveBounds;
    Note;

    for Cmd := Low(TUiCommand) to High(TUiCommand) do
        AssertTrue('no state offers command ' + IntToStr(Ord(Cmd)) +
            ' - it is either a defect or a row nobody deleted', Ever[Cmd]);
end;

{ ---- what is never offered ------------------------------------------------- }

procedure TActionStateTest.StartingANewProjectIsAlwaysOffered;
begin
    //  WITH NOTHING OPEN TOO. New and Open are how something comes to be open,
    //  so gating them on something being open would make the window unusable
    //  from the state it starts in.
    FInputs := EmptyUiInputs;
    Decide;
    AssertTrue('with nothing open', IsEnabled(ucNewProject));
    OpenAndIdle;
    Decide;
    AssertTrue('and with something open', IsEnabled(ucNewProject));
end;

procedure TActionStateTest.OpeningAProjectIsAlwaysOffered;
begin
    FInputs := EmptyUiInputs;
    Decide;
    AssertTrue('with nothing open', IsEnabled(ucOpenProject));
    OpenAndIdle;
    Decide;
    AssertTrue('and with something open', IsEnabled(ucOpenProject));
end;

procedure TActionStateTest.SavingIsOfferedOnlyOnceSomethingIsOpen;
begin
    //  Saving an empty window would write a project with nothing in it, which
    //  is not something anyone means to do.
    FInputs := EmptyUiInputs;
    Decide;
    AssertFalse('nothing to save yet', IsEnabled(ucSaveProject));
    AssertFalse('nor under another name', IsEnabled(ucSaveProjectAs));
    OpenAndIdle;
    Decide;
    AssertTrue('now there is', IsEnabled(ucSaveProject));
    AssertTrue('', IsEnabled(ucSaveProjectAs));
end;

procedure TActionStateTest.AnExportIsNotOfferedForAnEmptyTable;
begin
    //  THE FIX FOR A COMMAND THAT WAS NEVER OFFERED AT ALL. Its predecessor,
    //  ucSaveModelAsText, had its flag cleared on every state change and set
    //  again nowhere, so the one export this program has was unreachable for
    //  years - pinned by the test this one replaces. It also decided WHICH
    //  table to write from the tab in front, with no else branch, so on any
    //  other tab it silently did nothing.
    OpenAndIdle;
    Decide;
    AssertFalse('an empty parameter table', IsEnabled(ucExportCurveParameters));
    AssertFalse('an empty datasheet', IsEnabled(ucExportSummaryTable));
end;

procedure TActionStateTest.EachExportFollowsItsOwnTable;
begin
    //  Each names its target, so each is offered exactly when THAT table has
    //  something in it - not when whichever table is in front does.
    OpenAndIdle;
    FInputs.ParameterTableHasRows := True;
    Decide;
    AssertTrue('the parameters can be written', IsEnabled(ucExportCurveParameters));
    AssertFalse('the datasheet still cannot', IsEnabled(ucExportSummaryTable));

    FInputs.ParameterTableHasRows := False;
    FInputs.SummaryTableHasRows := True;
    Decide;
    AssertFalse('and the other way round', IsEnabled(ucExportCurveParameters));
    AssertTrue('', IsEnabled(ucExportSummaryTable));
end;

procedure TActionStateTest.NeitherExportIsOfferedWithNothingOpen;
begin
    FInputs := EmptyUiInputs;
    FInputs.ParameterTableHasRows := True;
    FInputs.SummaryTableHasRows := True;
    Decide;
    AssertFalse('', IsEnabled(ucExportCurveParameters));
    AssertFalse('', IsEnabled(ucExportSummaryTable));
end;


{ ---- which entry toggles, and to what ------------------------------------- }

{ WHICH MODES "TOGGLE" WAS A DISTINCTION THIS PROGRAM DID NOT MAKE, and the two
  tests that pinned it are gone with the predicate they tested. It answered that
  the interval-bounds and characteristic-point entries do not toggle; the window
  has always toggled them, by hand, in each of their handlers. What survives is
  the rule the window now asks - ModeAfterPicking, below - and the captions, which
  are the visible half of it. See docs/contributing/findings.md. }

procedure TActionStateTest.ChoosingAnEntryWhileItsOwnModeIsOnLeavesIt;
begin
    //  THE HALF THAT IS EASY TO LOSE. One entry says both "start" and "stop",
    //  and getting this wrong costs the user a click that appears to do
    //  nothing: the tick says the mode is on, the click re-enters it, and only
    //  a second click seems to work.
    AssertTrue('background leaves',
        ModeAfterPicking(ModeSelectBackground, ModeSelectBackground) =
        ModeSelectNothing);
    AssertTrue('curve positions leaves',
        ModeAfterPicking(ModeSelectCurvePositions, ModeSelectCurvePositions) =
        ModeSelectNothing);
    AssertTrue('fit intervals leaves',
        ModeAfterPicking(ModeSelectRFactorBounds, ModeSelectRFactorBounds) =
        ModeSelectNothing);
end;

procedure TActionStateTest.ChoosingItFromAnotherModeEntersIt;
begin
    AssertTrue('from the background mode',
        ModeAfterPicking(ModeSelectBackground, ModeSelectCurvePositions) =
        ModeSelectCurvePositions);
end;

procedure TActionStateTest.ChoosingItFromNothingEntersIt;
begin
    AssertTrue('the ordinary case',
        ModeAfterPicking(ModeSelectNothing, ModeSelectBackground) =
        ModeSelectBackground);
end;

procedure TActionStateTest.SwitchingBetweenTwoPickingModesNeedsNoStopFirst;
var
    M: TSelMode;
begin
    //  The user goes straight from picking background points to picking curve
    //  positions. A rule that only entered from ModeSelectNothing would make
    //  every switch take two clicks, the first of which looks like nothing.
    M := ModeAfterPicking(ModeSelectBackground, ModeSelectRFactorBounds);
    AssertTrue('entered the second', M = ModeSelectRFactorBounds);
    M := ModeAfterPicking(M, ModeSelectCurvePositions);
    AssertTrue('and then the third', M = ModeSelectCurvePositions);
end;

procedure TActionStateTest.AnEntryThatDoesNotToggleAlwaysMeansItself;
begin
    //  THIS TEST USED TO ASSERT THE OPPOSITE, and it was pinning a rule the
    //  program did not follow: it said choosing the interval-bounds entry twice
    //  enters it twice, while the window's own handler left the mode - as its
    //  tick, which un-ticks, says it should. Nothing failed, because that
    //  handler never called this rule; four handlers wrote their own copy. The
    //  window's behaviour is what the user sees, so the rule follows it now, and
    //  the four copies are gone.
    AssertTrue('from nothing',
        ModeAfterPicking(ModeSelectNothing, ModeSelectIntervalBounds) =
        ModeSelectIntervalBounds);
    AssertTrue('and choosing it again leaves it, like every other entry',
        ModeAfterPicking(ModeSelectIntervalBounds, ModeSelectIntervalBounds) =
        ModeSelectNothing);
end;

procedure TActionStateTest.LeavingAlwaysMeansNothingRatherThanThePrevious;
begin
    //  Leaving a picking mode goes to no mode at all, not back to whatever was
    //  in force before it. There is no stack here, and a user who left a mode
    //  expects the chart to stop collecting rather than to start collecting
    //  something else.
    AssertTrue('nothing, not the previous mode',
        ModeAfterPicking(ModeSelectCurvePositions, ModeSelectCurvePositions) =
        ModeSelectNothing);
end;

procedure TActionStateTest.AnEntrySaysStopOnlyWhileItsOwnModeRuns;
begin
    //  THE WHOLE OF WHAT THE USER SEES about whether a mode is running.
    AssertTrue('stop, while it runs',
        Pos('Stop', PickingEntryCaption(peBackground, ModeSelectBackground)) > 0);
    AssertTrue('start, while another runs',
        Pos('Start', PickingEntryCaption(peBackground,
            ModeSelectCurvePositions)) > 0);
    AssertTrue('start, while none runs',
        Pos('Start', PickingEntryCaption(peBackground, ModeSelectNothing)) > 0);
end;

procedure TActionStateTest.EveryEntryHasItsOwnMode;
var
    Entry, Other: TPickingEntry;
begin
    //  Two entries sharing a mode would both read "stop" for one running mode,
    //  which says the user is in two picking modes at once.
    for Entry := Low(TPickingEntry) to High(TPickingEntry) do
    begin
        AssertTrue('an entry that starts nothing',
            PickingEntryMode(Entry) <> ModeSelectNothing);
        for Other := Low(TPickingEntry) to High(TPickingEntry) do
            if Other <> Entry then
                AssertTrue('two entries, one mode',
                    PickingEntryMode(Other) <> PickingEntryMode(Entry));
    end;
end;

procedure TActionStateTest.ThePositionsEntryNamesWhatItPicks;
begin
    //  Positions are picked one at a time and the others are picked in pairs, so
    //  the entry says which it is: "Visual Position Selection" against plain
    //  "Visual Selection". Two entries deliberately share the plain wording.
    AssertTrue('positions are named',
        Pos('Position', PickingEntryCaption(peCurvePositions,
            ModeSelectNothing)) > 0);
    AssertTrue('the background is not',
        Pos('Position', PickingEntryCaption(peBackground,
            ModeSelectNothing)) = 0);
    AssertEquals('the bounds entry reads like the background one',
        PickingEntryCaption(peBackground, ModeSelectNothing),
        PickingEntryCaption(peIntervalBounds, ModeSelectNothing));
end;

procedure TActionStateTest.ACaptionAgreesWithWhatTheEntryWouldDo;
var
    Entry: TPickingEntry;
    Current: TSelMode;
    SaysStop, WouldLeave: boolean;
begin
    //  THE CLAIM WORTH SWEEPING. The caption describes the click, and
    //  ModeAfterPicking decides it: an entry reads "stop" exactly when clicking
    //  it would leave a mode. The two used to be written in different methods,
    //  where one could be changed without the other.
    for Entry := Low(TPickingEntry) to High(TPickingEntry) do
        for Current := Low(TSelMode) to High(TSelMode) do
        begin
            SaysStop := Pos('Stop',
                PickingEntryCaption(Entry, Current)) > 0;
            WouldLeave := ModeAfterPicking(Current, PickingEntryMode(Entry)) =
                ModeSelectNothing;
            AssertTrue('the caption and the click disagree',
                SaysStop = WouldLeave);
        end;
end;

procedure TActionStateTest.NoEntryIsEverUncaptioned;
var
    Entry: TPickingEntry;
    Current: TSelMode;
begin
    //  A menu entry with an empty caption is an entry the user cannot see, and
    //  an unmapped enum member is how one would get there.
    for Entry := Low(TPickingEntry) to High(TPickingEntry) do
        for Current := Low(TSelMode) to High(TSelMode) do
            AssertTrue('an empty caption',
                PickingEntryCaption(Entry, Current) <> '');
end;

procedure TActionStateTest.AnUnmappedEntryStartsNothingAndReadsAsStart;
var
    Bogus: TPickingEntry;
begin
    //  Reached by casting past the end of the enum, which is what an entry added
    //  and not mapped would amount to. It must start NO mode - an unmapped entry
    //  falling into the first branch would start visual background selection
    //  from a menu item meant for something else - and it must still read as
    //  "start", because it is not running anything.
    Bogus := TPickingEntry(Ord(High(TPickingEntry)) + 1);
    AssertTrue('it starts nothing',
        PickingEntryMode(Bogus) = ModeSelectNothing);
    AssertTrue('and reads as start',
        Pos('Start', PickingEntryCaption(Bogus, ModeSelectNothing)) > 0);
    //  AND IT MUST NOT READ AS "STOP" IN ANY MODE. With no mode of its own, the
    //  comparison "is my mode the current one" would otherwise be true whenever
    //  no picking mode runs at all - which is precisely when the entry must
    //  offer to start.
    AssertTrue('never stop',
        Pos('Stop', PickingEntryCaption(Bogus, ModeSelectNothing)) = 0);
end;

initialization
    //  A unit test: a record in, a record out. No window, no widget, no client.
    RegisterTest('unit', TActionStateTest);
end.
