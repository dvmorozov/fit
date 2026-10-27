// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Adding and moving one point of a set, against a real compute server.)

THE PICKING PATHS, END TO END. POST /problems/<id>/points/<set> adds one point
and PUT moves one: they are what every click and every drag on the chart sends,
and until these tests they had been run only in process (testcase_rest_api) and
classified (testcase_rest_routes) - never answered by a started fit_server over
a socket. In process, a handler can be right while the binary that ships does
not link it, the body it parses is not the body the client writes, or a reply
is lost on the way back; none of that is visible from inside one process.

ENTERED WHERE THE WINDOW ENTERS. A pick and a pick moved are TFitClient's
gestures (AddPointToCurvePositions, MovePickTo), driven over the client's own
transport (THttpFitService) to the started server, and the answer is read back
from the SERVER - its picks, their handles, its curves - not from the client's
copy, which the client edits itself before it asks.

NO CURVE TYPE IS NAMED. A pick builds an instance of the type the server
starts on, whichever product's server this is; what is asserted is where the
pick is and which instance it carries, never what shape that instance has.
}
unit testcase_point_routes_e2e;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, worker_process_harness,
    fit_client, mock_fit_viewer, points_set, title_points_set, int_fit_service,
    SimpMath, MyExceptions;

type
    TPointRoutesE2ETest = class(TWorkerProcessTest)
    private
        FClient: TFitClient;
        FView: TMockFitViewer;
        { The worker's profile and one fit interval over all of it, read back
          into a client - what opening a profile leaves the window with. }
        procedure GivenAProfileTheClientShows;
        { The server's picks, as the abscissae they stand at. }
        function ServerPositionsX: string;
        function ServerBoundsX: string;
        function ServerBackground: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure APickTheClientAddsIsTheServersPickAndBuildsItsCurve;
        procedure TheSamePickPostedAgainIsTakenBack;
        procedure APickMovedByTheClientTakesItsCurveWithIt;
        procedure ABoundMovedByTheClientIsTheServersBound;
        procedure ABackgroundPointMovesOverTheWireAndNothingIsAdded;
        { A background point picked on the chart is the server's at once, as
          every other pick is: the server is what a saved project captures and
          what a move names its point in. }
        procedure ABackgroundPointTheClientPicksIsTheServers;
        { A move naming a point the server's set does not hold is refused in
          words, and adds nothing - it was once quietly added. }
        procedure AMoveOfAPointTheServerDoesNotHoldIsRefused;
        { A row of the Background table typed over moves the point the row
          shows, named at the precision the client holds it - the table's
          four decimals named a point the server does not hold, and the move
          was refused. }
        procedure ABackgroundRowEditMovesThePointAtFullPrecision;
        { The same for the Data table: the sample moves, and no second one is
          added beside it. }
        procedure AProfileRowEditMovesTheSampleAtFullPrecision;
        { A row edit the server refuses leaves the client showing the server's
          points, not the edit it was refused. }
        procedure ARefusedBackgroundRowEditLeavesTheServersPoints;
    end;

implementation

{ Abscissae as text, in the set's order - so a failure shows the whole set
  rather than one count. Nine digits: a pick dropped on the chart lands on the
  profile's own sample, and the samples of a profile built by adding 0.2 over
  and over sit a few ulps from the round numbers they are named by. }
function XsOf(APoints: TTitlePointsSet): string;
var
    i: longint;
begin
    Result := '';
    try
        for i := 0 to APoints.PointsCount - 1 do
        begin
            if i > 0 then
                Result := Result + ' ';
            Result := Result + Format('%.9g', [APoints.PointXCoord[i]]);
        end;
    finally
        APoints.Free;
    end;
end;

procedure TPointRoutesE2ETest.SetUp;
begin
    inherited;
    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FClient.FProgressView := FView;
end;

procedure TPointRoutesE2ETest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    inherited;
end;

procedure TPointRoutesE2ETest.GivenAProfileTheClientShows;
var
    Bounds: TTitlePointsSet;
begin
    AssertTrue('the compute server is reachable', FSvc.IsAvailable);
    FSvc.SetProfilePointsSet(GaussianProfile);
    //  An interval as well as the picks, or the engine builds nothing: what to
    //  fit and where to fit it.
    Bounds := TTitlePointsSet.Create(nil);
    Bounds.AddNewPoint(0, 0);
    Bounds.AddNewPoint(20, 0);
    FSvc.SetRFactorBounds(Bounds);
    FClient.ResyncFromService;
end;

function TPointRoutesE2ETest.ServerPositionsX: string;
begin
    Result := XsOf(FSvc.GetCurvePositions);
end;

function TPointRoutesE2ETest.ServerBoundsX: string;
begin
    Result := XsOf(FSvc.GetRFactorBounds);
end;

{ Points as x:y text, in the set's order. }
function PointsText(APoints: TPointsSet): string;
var
    i: longint;
begin
    Result := '';
    if not Assigned(APoints) then
        Exit;
    for i := 0 to APoints.PointsCount - 1 do
    begin
        if i > 0 then
            Result := Result + ' ';
        Result := Result + Format('%.9g:%.9g',
            [APoints.PointXCoord[i], APoints.PointYCoord[i]]);
    end;
end;

function TPointRoutesE2ETest.ServerBackground: string;
var
    Points: TTitlePointsSet;
begin
    Points := FSvc.GetBackgroundPoints;
    try
        Result := PointsText(Points);
    finally
        Points.Free;
    end;
end;

procedure TPointRoutesE2ETest.APickTheClientAddsIsTheServersPickAndBuildsItsCurve;
var
    Ids: TCurveInstanceIdList;
begin
    GivenAProfileTheClientShows;

    FClient.AddPointToCurvePositions(8, GaussPoint(100, 1.5, 10, 8));

    AssertEquals('the server holds the pick', '8', ServerPositionsX);
    //  A pick is the seed of a curve, and the curve carries the handle issued
    //  to it: the instance the rest of the model is addressed by.
    Ids := FSvc.GetCurvePositionIds;
    AssertEquals('one handle for the one pick', 1, Length(Ids));
    AssertTrue('a handle was issued', Ids[0] <> '');
    AssertEquals('the pick built its curve', 1, FSvc.GetCurveCount);
    AssertEquals('and the curve is the one the pick carries', Ids[0],
        FSvc.GetCurveInstanceId(0));
end;

procedure TPointRoutesE2ETest.TheSamePickPostedAgainIsTakenBack;
begin
    //  ADD-OR-TOGGLE, the interactive gesture: the user clicking the same
    //  sample twice takes the pick back. The client mirrors the rule, so the
    //  server answering otherwise leaves the chart and the model disagreeing.
    GivenAProfileTheClientShows;
    FClient.AddPointToCurvePositions(8, GaussPoint(100, 1.5, 10, 8));
    FClient.AddPointToCurvePositions(8, GaussPoint(100, 1.5, 10, 8));

    AssertEquals('no pick left', '', ServerPositionsX);
    AssertEquals('and no curve', 0, FSvc.GetCurveCount);
end;

procedure TPointRoutesE2ETest.APickMovedByTheClientTakesItsCurveWithIt;
var
    Before: TCurveInstanceIdList;
    After: TCurveInstanceIdList;
    Why: string;
begin
    GivenAProfileTheClientShows;
    FClient.AddPointToCurvePositions(8, GaussPoint(100, 1.5, 10, 8));
    Before := FSvc.GetCurvePositionIds;
    AssertEquals('one pick to move', 1, Length(Before));

    //  Dropped on the chart at 12: the client plans the move to the sample
    //  there and sends the old and the new coordinates.
    AssertTrue('the move was made: ' + Why,
        FClient.MovePickTo(pkCurvePositions, 0, 12, Why));

    AssertEquals('the server''s pick is where it was dropped', '12',
        ServerPositionsX);
    //  THE IDENTITY MOVES WITH THE PICK. Read as one pick taken away and
    //  another added, the curve would come back under a new handle, and
    //  everything stored against the old one - its fitted values above all -
    //  would be attached to nothing.
    After := FSvc.GetCurvePositionIds;
    AssertEquals('still one pick', 1, Length(After));
    AssertEquals('carrying the handle it had', Before[0], After[0]);
    AssertEquals('one curve', 1, FSvc.GetCurveCount);
    AssertEquals('the same curve', Before[0], FSvc.GetCurveInstanceId(0));
end;

procedure TPointRoutesE2ETest.ABoundMovedByTheClientIsTheServersBound;
var
    Why: string;
begin
    GivenAProfileTheClientShows;
    AssertEquals('the interval the test begins with', '0 20', ServerBoundsX);

    AssertTrue('the move was made: ' + Why,
        FClient.MovePickTo(pkFitBounds, 1, 15, Why));

    //  The bound moved, and only it: a move that added a point instead would
    //  leave three bounds, pairing into an interval nobody chose.
    AssertEquals('the server''s interval ends where the bound was dropped',
        '0 15', ServerBoundsX);
end;

procedure TPointRoutesE2ETest.ABackgroundPointMovesOverTheWireAndNothingIsAdded;
begin
    GivenAProfileTheClientShows;
    //  Put on the server directly, over the route a pick uses, and read back
    //  into the client as opening a project would.
    FSvc.AddPointToBackground(2, 1);
    FSvc.AddPointToBackground(5, 1);
    FClient.ResyncFromService;
    AssertEquals('the two points the test begins with', '2:1 5:1',
        ServerBackground);

    FClient.ReplacePointInBackground(5, 1, 6, 2);

    AssertEquals('the point moved, value and all, and no other appeared',
        '2:1 6:2', ServerBackground);
end;

procedure TPointRoutesE2ETest.ABackgroundPointTheClientPicksIsTheServers;
begin
    GivenAProfileTheClientShows;

    FClient.AddPointToBackground(2, 1);
    FClient.AddPointToBackground(5, 1);

    AssertEquals('the server holds both picks', '2:1 5:1', ServerBackground);

    //  ADD-OR-TOGGLE on both sides, as the other picking modes are: the same
    //  sample clicked again is taken back, here and there.
    FClient.AddPointToBackground(5, 1);
    AssertEquals('the second taken back on the server too', '2:1',
        ServerBackground);
end;

procedure TPointRoutesE2ETest.AMoveOfAPointTheServerDoesNotHoldIsRefused;
var
    Refusal: string;
begin
    GivenAProfileTheClientShows;
    FSvc.AddPointToBackground(2, 1);

    Refusal := '';
    try
        FSvc.ReplacePointInBackground(5, 1, 6, 2);
    except
        on E: Exception do
            Refusal := E.Message;
    end;
    AssertTrue('refused, in words: "' + Refusal + '"',
        Pos('No point of background', Refusal) > 0);
    AssertEquals('and nothing was added', '2:1', ServerBackground);
end;

procedure TPointRoutesE2ETest.ABackgroundRowEditMovesThePointAtFullPrecision;
begin
    GivenAProfileTheClientShows;
    //  More decimals than the table shows: 2.1235 and 1.6543 there.
    FSvc.AddPointToBackground(2.123456, 1.654321);
    FSvc.AddPointToBackground(5, 1);
    FClient.ResyncFromService;

    //  The first row typed over, as the window's edit handler sends it.
    FClient.EditBackgroundPoint(0, 3, 2);

    AssertEquals('the point the row showed moved, and nothing was added',
        '3:2 5:1', ServerBackground);
    AssertEquals('and the client shows what the server holds', '3:2 5:1',
        PointsText(FClient.GetBackgroundPoints));
end;

procedure TPointRoutesE2ETest.AProfileRowEditMovesTheSampleAtFullPrecision;
var
    Profile, Server: TTitlePointsSet;
begin
    AssertTrue('the compute server is reachable', FSvc.IsAvailable);
    Profile := TTitlePointsSet.Create(nil);
    Profile.AddNewPoint(1.234567, 10.123456);
    Profile.AddNewPoint(2.345678, 20.654321);
    Profile.AddNewPoint(3.456789, 10.5);
    FSvc.SetProfilePointsSet(Profile);
    FClient.ResyncFromService;

    //  The position of the second row typed over: the row showed 2.3457 and
    //  20.6543, and the sample it stands for is the one at 2.345678.
    FClient.EditProfilePoint(1, 2.5, 20.654321);

    Server := FSvc.GetProfilePointsSet;
    try
        AssertEquals('the sample moved, and no second one was added',
            '1.234567:10.123456 2.5:20.654321 3.456789:10.5', PointsText(Server));
    finally
        Server.Free;
    end;
end;

procedure TPointRoutesE2ETest.ARefusedBackgroundRowEditLeavesTheServersPoints;
var
    Refusal: string;
begin
    GivenAProfileTheClientShows;
    FSvc.AddPointToBackground(2, 1);
    FClient.ResyncFromService;
    //  Taken back on the server behind the client's back: the client's row
    //  now names a point the server does not hold.
    FSvc.AddPointToBackground(2, 1);

    Refusal := '';
    try
        FClient.EditBackgroundPoint(0, 3, 2);
    except
        on E: EUserException do
            Refusal := E.Message;
    end;
    AssertTrue('refused, in words: "' + Refusal + '"', Refusal <> '');
    AssertEquals('nothing reached the server', '', ServerBackground);
    AssertEquals('and the client shows what the server holds, not the edit',
        '', PointsText(FClient.GetBackgroundPoints));
end;

initialization
    RegisterTest('integration', TPointRoutesE2ETest);
end.
