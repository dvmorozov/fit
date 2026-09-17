// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Entering and leaving a picking mode: which set the clicks go into,
what appears on the chart, and what is taken off it again.)

A PICKING MODE IS A GESTURE THE WHOLE WINDOW ENTERS. The user chooses "Select
area limits" from a menu, the chart starts collecting clicks into a set of its
own, and the crosses that mark them are drawn over the data. Choosing the item
again - or finishing the gesture - has to put everything back.

FOUR MODES COLLECT INTO A SET MADE FOR THE GESTURE, and three edit a set that
already exists (the background points, the curve positions, the R-factor
bounds). The two kinds behave differently on the way out: the first four throw
their set away, the second three must not, because that set is part of the
model.

WHAT GOES WRONG, AND WHY IT IS NOT OBVIOUS.

A set NOT thrown away on the way out leaves crosses on the chart over a result
the user has finished making - the marks that built a thing sitting on top of
the thing. Worse, the next gesture collects into it, so picks from two different
gestures end up in one set and the second gesture starts part-done.

A set thrown away that SHOULD NOT BE deletes model data: the background points
the user spent a minute picking vanish when they leave the mode.

And the branch that decides which of those to do reads the mode being LEFT, not
the one being entered - so it has to run against the previous value while the
new one is already current. That ordering is the single most delicate line in
the state machine, and it exists because the series the entering mode creates is
styled by the mode that is current when it is made.

THE PRECONDITIONS ARE DELIBERATE. Switching into a mode that edits an existing
set asserts that the set is there; the application offers those menu items only
once it is. Driving one without is a programming error rather than a case to
cover, so the fixture arms them the way the application does.
}
unit testcase_selection_modes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, mock_fit_viewer, mock_http_transport,
    title_points_set, points_set;

type
    TSelectionModesTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TFitClient;
        { Arms the three sets the model owns, the way the application does
          before it offers the modes that edit them. }
        procedure GivenTheModelsSets;
        { The title of the set clicks currently go into, or '' when there is
          none. }
        function CollectingInto: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Nothing is being picked to begin with.
        procedure NoModeIsActiveAtTheStart;
        procedure NothingIsCollectedUntilAModeIsEntered;

        //  The four gestures that make a set of their own.
        procedure AreaLimitsCollectIntoTheirOwnSet;
        procedure CharacteristicPointsCollectIntoTheirOwn;
        procedure CurveBoundsCollectIntoTheirOwn;
        procedure ModulePicksCollectIntoTheirOwn;
        procedure EachGestureIsTitledForItself;
        procedure EnteringAGestureDrawsItsMarks;

        //  Leaving them.
        procedure LeavingAGestureTakesItsMarksOff;
        procedure LeavingAGestureThrowsItsSetAway;
        procedure SwitchingBetweenGesturesStartsTheSecondEmpty;
        procedure LeavingWhenNothingWasBeingPickedIsHarmless;

        //  The three modes that edit a set the model owns.
        procedure BackgroundPicksEditTheModelsBackground;
        procedure CurvePositionPicksEditTheModelsPositions;
        procedure RFactorBoundPicksEditTheModelsBounds;
        procedure EnteringOneOfThoseDrawsTheSetItEdits;
        procedure LeavingOneOfThoseKeepsItsSet;

        //  What the mode itself reports.
        procedure TheModeInForceIsReportedBack;
        procedure LeavingReportsNoMode;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TSelectionModesTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TSelectionModesTest.TearDown;
begin
    //  The viewer is referenced by the client; the client goes first.
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TSelectionModesTest.GivenTheModelsSets;
begin
    //  The order the application does it in: the sets exist before the menu
    //  items that edit them are offered.
    FSvc.Reply('background', '{"title":"Background","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
    FSvc.Reply('positions', '{"title":"Positions","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('rfactor-bounds', '{"title":"Bounds","x":[1,2],"y":[1,2]}');
    FClient.ComputeCurvePositionsDone;
end;

function TSelectionModesTest.CollectingInto: string;
var
    S: TTitlePointsSet;
begin
    Result := '';
    S := FClient.GetCurrentPointsSet;
    if Assigned(S) then
        Result := S.FTitle;
end;

{ ---- nothing is being picked to begin with --------------------------------- }

procedure TSelectionModesTest.NoModeIsActiveAtTheStart;
begin
    //  A window that opened mid-gesture would turn the user's first click on
    //  their data into a pick they never asked to make.
    AssertTrue('nothing', FClient.SelectionMode = ModeSelectNothing);
end;

procedure TSelectionModesTest.NothingIsCollectedUntilAModeIsEntered;
begin
    AssertEquals('no set to collect into', '', CollectingInto);
end;

{ ---- the four gestures that make a set of their own ------------------------ }

procedure TSelectionModesTest.AreaLimitsCollectIntoTheirOwnSet;
begin
    //  A SET MADE FOR THE GESTURE, not one of the model's. Collecting area
    //  limits into the background points would put two clicks into the data the
    //  fit subtracts.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    AssertEquals('Area Limits', CollectingInto);
end;

procedure TSelectionModesTest.CharacteristicPointsCollectIntoTheirOwn;
begin
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    AssertEquals('Characteristic Points', CollectingInto);
end;

procedure TSelectionModesTest.CurveBoundsCollectIntoTheirOwn;
begin
    FClient.SelectionMode := ModeSelectCurveBounds;
    AssertEquals('Curve Bounds', CollectingInto);
end;

procedure TSelectionModesTest.ModulePicksCollectIntoTheirOwn;
begin
    //  A module's own gesture, which reuses the same collect-then-act shape so
    //  it is one users already know from placing a curve.
    FClient.SelectionMode := ModeSelectModulePoints;
    AssertEquals('Module Points', CollectingInto);
end;

procedure TSelectionModesTest.EachGestureIsTitledForItself;
begin
    //  THE TITLE IS WHAT THE USER READS IN THE LEGEND while they are picking,
    //  and it is the only thing on screen that says which gesture they are in.
    //  Four gestures sharing one title would leave them unable to tell.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    AssertEquals('Area Limits', CollectingInto);
    FClient.SelectionMode := ModeSelectCurveBounds;
    AssertEquals('Curve Bounds', CollectingInto);
end;

procedure TSelectionModesTest.EnteringAGestureDrawsItsMarks;
begin
    //  The set is empty, and it is plotted anyway: the series has to exist
    //  before the first pick, or the first cross appears only after the second
    //  click.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    AssertTrue('the picks are on the chart',
        FView.Plotted('PlotSelectedPoints'));
end;

{ ---- leaving them ---------------------------------------------------------- }

procedure TSelectionModesTest.LeavingAGestureTakesItsMarksOff;
begin
    //  WHAT THE PICKS MADE IS ON THE CHART BY NOW, and leaving is what takes
    //  the crosses that built it off again - otherwise the marks sit on top of
    //  the finished thing.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    FView.Log.Clear;
    FClient.SelectionMode := ModeSelectNothing;
    AssertTrue('hidden', FView.Plotted('Hide'));
end;

procedure TSelectionModesTest.LeavingAGestureThrowsItsSetAway;
begin
    //  Kept, the next gesture would collect into it and start part-done, with
    //  picks from two different gestures in one set.
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.SelectionMode := ModeSelectNothing;
    AssertEquals('nothing to collect into', '', CollectingInto);
end;

procedure TSelectionModesTest.SwitchingBetweenGesturesStartsTheSecondEmpty;
var
    S: TTitlePointsSet;
begin
    //  DIRECTLY FROM ONE GESTURE TO ANOTHER, without passing through "nothing"
    //  - which is what choosing a second picking item from the menu does. The
    //  first gesture's picks must not carry into the second.
    FClient.SelectionMode := ModeSelectIntervalBounds;
    FClient.AddPointToSelected(1, 1);
    FClient.SelectionMode := ModeSelectCurveBounds;
    S := FClient.GetCurrentPointsSet;
    AssertTrue('there is a set', Assigned(S));
    AssertEquals('and it is the new gesture''s', 'Curve Bounds', S.FTitle);
    AssertEquals('with none of the previous picks', 0, S.PointsCount);
end;

procedure TSelectionModesTest.LeavingWhenNothingWasBeingPickedIsHarmless;
begin
    //  The menu item can be chosen twice, and the window also leaves the mode
    //  by itself when a gesture completes - so "leave" arrives when there is
    //  nothing to leave, as an ordinary event.
    FClient.SelectionMode := ModeSelectNothing;
    AssertTrue('still nothing', FClient.SelectionMode = ModeSelectNothing);
    AssertEquals('and nothing to collect into', '', CollectingInto);
end;

{ ---- the three modes that edit a set the model owns ------------------------ }

procedure TSelectionModesTest.BackgroundPicksEditTheModelsBackground;
begin
    //  NOT A NEW SET. These three edit data the model already holds, so a click
    //  adds to it rather than to a gesture of its own.
    GivenTheModelsSets;
    FClient.SelectionMode := ModeSelectBackground;
    AssertTrue('the model''s background', FClient.GetCurrentPointsSet =
        TTitlePointsSet(FClient.GetBackgroundPoints));
end;

procedure TSelectionModesTest.CurvePositionPicksEditTheModelsPositions;
begin
    GivenTheModelsSets;
    FClient.SelectionMode := ModeSelectCurvePositions;
    AssertTrue('the model''s positions', FClient.GetCurrentPointsSet =
        TTitlePointsSet(FClient.GetCurvePositions));
end;

procedure TSelectionModesTest.RFactorBoundPicksEditTheModelsBounds;
begin
    GivenTheModelsSets;
    FClient.SelectionMode := ModeSelectRFactorBounds;
    AssertTrue('the model''s bounds', FClient.GetCurrentPointsSet =
        TTitlePointsSet(FClient.GetRFactorBounds));
end;

procedure TSelectionModesTest.EnteringOneOfThoseDrawsTheSetItEdits;
begin
    //  Its own series, not the gesture series: the user has to see the points
    //  they are about to edit.
    GivenTheModelsSets;
    FView.Log.Clear;
    FClient.SelectionMode := ModeSelectBackground;
    AssertTrue('the background is drawn', FView.Plotted('PlotBackground'));
end;

procedure TSelectionModesTest.LeavingOneOfThoseKeepsItsSet;
begin
    //  THE DIFFERENCE BETWEEN THE TWO KINDS OF MODE, and the one that destroys
    //  work if it is got wrong: throwing this set away on the way out would
    //  delete the background points the user spent a minute picking.
    GivenTheModelsSets;
    FClient.SelectionMode := ModeSelectBackground;
    FClient.SelectionMode := ModeSelectNothing;
    AssertTrue('the background survived',
        Assigned(FClient.GetBackgroundPoints));
    AssertEquals('with its points', 2,
        FClient.GetBackgroundPoints.PointsCount);
end;

{ ---- what the mode itself reports ------------------------------------------ }

procedure TSelectionModesTest.TheModeInForceIsReportedBack;
begin
    //  The window reads this to tick the menu item and to decide what a click
    //  on the chart means. A mode that did not report itself would leave the
    //  tick and the behaviour disagreeing.
    FClient.SelectionMode := ModeSelectCurveBounds;
    AssertTrue('curve bounds',
        FClient.SelectionMode = ModeSelectCurveBounds);
end;

procedure TSelectionModesTest.LeavingReportsNoMode;
begin
    FClient.SelectionMode := ModeSelectCurveBounds;
    FClient.SelectionMode := ModeSelectNothing;
    AssertTrue('nothing', FClient.SelectionMode = ModeSelectNothing);
end;

initialization
    //  A unit test: the client over a mock viewer and a mock transport. No
    //  chart, no menu and no server.
    RegisterTest('unit', TSelectionModesTest);
end.
