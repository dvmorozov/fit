// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a click on the chart does to the set it goes into, and which set
that is.)

CLICKING A PICK AGAIN TAKES IT AWAY. There is no separate unpick gesture, no
modifier key and nothing in the menus - the same click that made a mark removes
it, and that is the only way to undo one. If it stopped working the user would
have no way to correct a misplaced pick short of leaving the mode and starting
the gesture again, losing the picks they got right.

A POINT IS IDENTIFIED BY ITS ABSCISSA ALONE, which looks careless and is not: a
set holds at most one point per x, so the x IS the identity. Comparing the
ordinate as well would make a pick un-removable whenever the y the click
reported differed in the last bit from the y that was stored - which is exactly
what happens when the value has been through the chart's pixel arithmetic and
back.

AND WHICH SET RECEIVES THE CLICK IS THE SELECTION MODE'S ANSWER. Seven modes,
five destinations. Sending a background pick into the profile edits the measured
data - and the profile is what everything else is measured against, so the
mistake propagates into every result and is invisible in the table the user was
looking at.

THE REDRAW IS UNCONDITIONAL, including on the paths that changed nothing. It is
what clears the entry fields after a value the model refused, so a redraw skipped
as an optimisation leaves the rejected text sitting in the cell looking accepted.
}
unit testcase_client_picking;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, mock_fit_viewer, mock_http_transport,
    title_points_set, points_set, neutron_points_set;

type
    TTestablePickClient = class(TFitClient)
    public
        { The picks of the gesture in progress. }
        function PickCount: longint;
        function PickX(AIndex: longint): double;
        function PickY(AIndex: longint): double;
        { Builds the selected interval from index bounds, as a drag does. }
        procedure TakeInterval(APoints: TNeutronPointsSet;
            AFirst, ALast: longint);
        function SelectedAreaCount: longint;
        function SelectedAreaFirstX: double;
        { Gives every curve the wavelength the client holds. }
        procedure PushWaveLengthToCurves;
    end;

    TClientPickingTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TTestablePickClient;
        { A five-point profile, one unit apart. }
        function AProfile: TNeutronPointsSet;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Picking and unpicking.
        procedure AClickAddsAPick;
        procedure ASecondClickOnTheSamePointRemovesIt;
        procedure AClickElsewhereAddsAnother;
        procedure RemovingOnePickLeavesTheOthers;
        procedure APickIsIdentifiedByItsAbscissaAlone;
        procedure ThePicksAreRedrawnEitherWay;

        //  Which set the click goes into.
        procedure WithNoModeAClickGoesNowhere;
        procedure AnAreaLimitGoesIntoTheGesturesSet;
        procedure ABackgroundPickGoesIntoTheBackground;

        //  Taking an interval out of the profile.
        procedure AnIntervalCarriesThePointsBetweenItsBounds;
        procedure ItStartsAtTheFirstBound;
        procedure AOnePointIntervalIsStillAnInterval;
        procedure TakingASecondIntervalReplacesTheFirst;

        //  The wavelength every curve needs.
        procedure PushingTheWaveLengthNeedsACurveListToPushItInto;
    end;

implementation

const
    BASE = 'http://localhost:8080';

function TTestablePickClient.PickCount: longint;
begin
    Result := 0;
    if Assigned(FSelectedPoints) then
        Result := FSelectedPoints.PointsCount;
end;

function TTestablePickClient.PickX(AIndex: longint): double;
begin
    Result := FSelectedPoints.PointXCoord[AIndex];
end;

function TTestablePickClient.PickY(AIndex: longint): double;
begin
    Result := FSelectedPoints.PointYCoord[AIndex];
end;

procedure TTestablePickClient.TakeInterval(APoints: TNeutronPointsSet;
    AFirst, ALast: longint);
begin
    SelectProfileIntervalActual(APoints, AFirst, ALast);
end;

function TTestablePickClient.SelectedAreaCount: longint;
begin
    Result := 0;
    if Assigned(FSelectedArea) then
        Result := FSelectedArea.PointsCount;
end;

function TTestablePickClient.SelectedAreaFirstX: double;
begin
    Result := -1;
    if Assigned(FSelectedArea) and (FSelectedArea.PointsCount > 0) then
        Result := FSelectedArea.PointXCoord[0];
end;

procedure TTestablePickClient.PushWaveLengthToCurves;
begin
    SetCurvesListLambda;
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TClientPickingTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TTestablePickClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TClientPickingTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

function TClientPickingTest.AProfile: TNeutronPointsSet;
var
    i: longint;
begin
    Result := TNeutronPointsSet.Create(nil);
    for i := 0 to 4 do
        Result.AddNewPoint(10 + i, 100 + i);
end;

{ ---- picking and unpicking ------------------------------------------------- }

procedure TClientPickingTest.AClickAddsAPick;
begin
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.AddPointToActive(12, 102);
    AssertEquals('one pick', 1, FClient.PickCount);
    AssertEquals('where it was clicked', 12.0, FClient.PickX(0), 1E-9);
end;

procedure TClientPickingTest.ASecondClickOnTheSamePointRemovesIt;
begin
    //  THE ONLY WAY TO UNDO A PICK. There is no separate gesture and nothing in
    //  the menus; if this stopped working, a misplaced pick could only be
    //  corrected by leaving the mode and starting again, losing the picks that
    //  were right.
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.AddPointToActive(12, 102);
    FClient.AddPointToActive(12, 102);
    AssertEquals('taken away again', 0, FClient.PickCount);
end;

procedure TClientPickingTest.AClickElsewhereAddsAnother;
begin
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.AddPointToActive(12, 102);
    FClient.AddPointToActive(13, 103);
    AssertEquals('two picks', 2, FClient.PickCount);
end;

procedure TClientPickingTest.RemovingOnePickLeavesTheOthers;
begin
    //  A toggle that cleared the set would lose everything the user had marked
    //  so far, and they would have no reason to expect it.
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.AddPointToActive(12, 102);
    FClient.AddPointToActive(13, 103);
    FClient.AddPointToActive(12, 102);
    AssertEquals('one left', 1, FClient.PickCount);
    AssertEquals('and it is the other one', 13.0, FClient.PickX(0), 1E-9);
end;

procedure TClientPickingTest.APickIsIdentifiedByItsAbscissaAlone;
begin
    //  LOOKS CARELESS AND IS NOT: a set holds at most one point per x, so the x
    //  IS the identity. Comparing the ordinate too would make a pick
    //  un-removable whenever the y the click reported differed in the last bit
    //  from the y that was stored - which is what happens to a value that has
    //  been through the chart's pixel arithmetic and back.
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.AddPointToActive(12, 102);
    FClient.AddPointToActive(12, 999);
    AssertEquals('still removed', 0, FClient.PickCount);
end;

procedure TClientPickingTest.ThePicksAreRedrawnEitherWay;
begin
    //  UNCONDITIONALLY, including where nothing was added. The redraw is what
    //  clears the entry fields after a value the model refused, so one skipped
    //  as an optimisation leaves rejected text sitting in the cell looking
    //  accepted.
    FClient.SelectionMode := ModeSelectCharacteristicPoints;
    FClient.AddPointToActive(12, 102);
    FView.Log.Clear;
    FClient.AddPointToActive(12, 102);
    AssertTrue('drawn again after a removal',
        FView.Plotted('PlotSelectedPoints'));
end;

{ ---- which set the click goes into ----------------------------------------- }

procedure TClientPickingTest.WithNoModeAClickGoesNowhere;
begin
    //  A CLICK ON THE DATA IS JUST A CLICK unless a gesture is running. Falling
    //  through to some default set would turn every stray click on the chart
    //  into a mark the user never meant to make.
    FClient.AddPointToActive(12, 102);
    AssertEquals('nothing was picked', 0, FClient.PickCount);
end;

procedure TClientPickingTest.AnAreaLimitGoesIntoTheGesturesSet;
begin
    FClient.SelectionMode := ModeSelectIntervalBounds;
    FClient.AddPointToActive(12, 102);
    AssertEquals('into the gesture''s own set', 1, FClient.PickCount);
end;

procedure TClientPickingTest.ABackgroundPickGoesIntoTheBackground;
begin
    //  NOT INTO THE GESTURE SET. A background pick sent to the profile would
    //  edit the measured data - and the profile is what everything else is
    //  measured against, so the mistake propagates into every result while
    //  being invisible in the table the user was looking at.
    FSvc.Reply('background', '{"title":"b","x":[1,2],"y":[1,2]}');
    FClient.ComputeBackgroundPointsDone;
    FClient.SelectionMode := ModeSelectBackground;
    FClient.AddPointToActive(12, 102);
    AssertEquals('the background grew', 3,
        FClient.GetBackgroundPoints.PointsCount);
    AssertEquals('and no gesture set was made', 0, FClient.PickCount);
end;

{ ---- taking an interval out of the profile --------------------------------- }

procedure TClientPickingTest.AnIntervalCarriesThePointsBetweenItsBounds;
var
    P: TNeutronPointsSet;
begin
    //  INCLUSIVE OF BOTH ENDS. The user dragged from one point to another and
    //  both are inside what they chose; dropping either would fit a window one
    //  channel narrower than the one on screen.
    P := AProfile;
    try
        FClient.TakeInterval(P, 1, 3);
        AssertEquals('three points', 3, FClient.SelectedAreaCount);
    finally
        P.Free;
    end;
end;

procedure TClientPickingTest.ItStartsAtTheFirstBound;
var
    P: TNeutronPointsSet;
begin
    //  Off by one at the start and the fit runs over a stretch shifted from the
    //  one the user marked - which looks like a fit that will not sit on the
    //  peak.
    P := AProfile;
    try
        FClient.TakeInterval(P, 1, 3);
        AssertEquals('the second point', 11.0,
            FClient.SelectedAreaFirstX, 1E-9);
    finally
        P.Free;
    end;
end;

procedure TClientPickingTest.AOnePointIntervalIsStillAnInterval;
var
    P: TNeutronPointsSet;
begin
    //  What a double-click produces. It is the engine's business whether to
    //  refuse it; what matters here is that it is built as asked rather than
    //  coming out empty.
    P := AProfile;
    try
        FClient.TakeInterval(P, 2, 2);
        AssertEquals('one point', 1, FClient.SelectedAreaCount);
    finally
        P.Free;
    end;
end;

procedure TClientPickingTest.TakingASecondIntervalReplacesTheFirst;
var
    P: TNeutronPointsSet;
begin
    //  REPLACED, not added to. Two intervals at once is not a state the rest of
    //  the program has a meaning for, and the chart would draw both.
    P := AProfile;
    try
        FClient.TakeInterval(P, 0, 3);
        FClient.TakeInterval(P, 2, 3);
        AssertEquals('only the second', 2, FClient.SelectedAreaCount);
        AssertEquals('starting where it does', 12.0,
            FClient.SelectedAreaFirstX, 1E-9);
    finally
        P.Free;
    end;
end;

{ ---- the wavelength every curve needs -------------------------------------- }

procedure TClientPickingTest.PushingTheWaveLengthNeedsACurveListToPushItInto;
var
    Raised: boolean;
begin
    //  A CURVE WITHOUT A WAVELENGTH CANNOT ANSWER FOR THE DIFFRACTION AXIS -
    //  the conversion divides by it - so this exists to make sure every curve
    //  has one before the user can switch to that axis.
    //
    //  ASSERTED, NOT GUARDED. It is called after the curves have been read
    //  back, never before, so arriving here without a list is a caller in the
    //  wrong order rather than a state to handle - and an assertion names the
    //  caller, where a silent return would leave the curves without their
    //  wavelength and raise later from inside a paint.
    Raised := False;
    try
        FClient.PushWaveLengthToCurves;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('refused with no curves', Raised);
end;

initialization
    //  A unit test: point sets in memory, a mock transport and a mock viewer.
    //  No chart and no server.
    RegisterTest('unit', TClientPickingTest);
end.
