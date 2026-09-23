// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client does when a project is restored under it, and when the
user starts a new one.)

TWO DEFECTS THAT NO TEST SAW, and both for the same reason: every test of the
project file drove IFitService directly, and nobody looked at what the CLIENT
held afterwards. The client keeps its own copies of the profile and the picks -
they are the chart's series - and a restore does not go through it.

  * OPENING A PROJECT DREW NOTHING. UpdateComputedData re-reads the derived sets
    and the picks and leaves the experimental profile alone, because every other
    path that changes it has already put it there. Restoring is the one that has
    not, so the model came back and the chart stayed empty.

  * NEW PROJECT RAISED. It went through Reload, which re-reads the DATA FILE and
    begins by asserting there is a loader. A session that opened a project has
    never had one, so the command faulted instead of clearing.

Both are in the client, which is counted and testable; the client is driven here
against a real engine, in process.
}
unit testcase_client_project_state;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, fit_service, fit_client, mock_fit_viewer,
    title_points_set, points_set, MyExceptions;

type
    TClientProjectStateTest = class(TTestCase)
    private
        FService: TFitService;
        FClient: TFitClient;
        FViewObj: TMockFitViewer;
        { Puts a profile and a pick into the ENGINE only, as restoring a project
          does - without going through the client at all. }
        procedure GivenARestoredProblem;
        function ProfilePointCount: longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ResyncingBringsTheProfileTheEngineHolds;
        procedure AndTheBackgroundAndThePicksWithIt;
        procedure ResyncingTwiceIsHarmless;
        procedure ResyncingFollowsTheEngineIntoASelectedInterval;

        procedure StartingEmptyNeedsNoDataLoader;
        procedure StartingEmptyLeavesTheEngineAloneOnPurpose;
        procedure AndReportsThatNothingIsOpen;
        procedure StartingEmptyDropsWhatWasOnTheChart;
    end;

implementation

procedure TClientProjectStateTest.SetUp;
begin
    FService := TFitService.Create;
    FViewObj := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FService;
    FClient.FFitViewer := FViewObj;
end;

procedure TClientProjectStateTest.TearDown;
begin
    //  The client holds the viewer by interface and the service by interface;
    //  everything compiles -SIcorba, so the references go before the objects.
    FClient.FFitViewer := nil;
    FClient.FitService := nil;
    FreeAndNil(FClient);
    FreeAndNil(FViewObj);
    FreeAndNil(FService);
end;

procedure TClientProjectStateTest.GivenARestoredProblem;
var
    P, B, Picks: TTitlePointsSet;
    Svc: IFitService;
    i: longint;
begin
    //  STRAIGHT INTO THE ENGINE, which is exactly what applying a project does:
    //  it goes through IFitService and never touches this client.
    Svc := FService;
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    Svc.SetProfilePointsSet(P);

    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    Svc.SetRFactorBounds(B);

    Svc.AddPointToBackground(0, 10);

    Picks := TTitlePointsSet.Create(nil);
    Picks.AddNewPoint(10, 110);
    Svc.SetCurvePositions(Picks);
end;

function TClientProjectStateTest.ProfilePointCount: longint;
begin
    Result := 0;
    if Assigned(FClient.GetProfilePoints) then
        Result := FClient.GetProfilePoints.PointsCount;
end;

procedure TClientProjectStateTest.ResyncingBringsTheProfileTheEngineHolds;
begin
    //  THE DEFECT. Before this, the client held nothing after a restore and the
    //  chart drew nothing - while the model was perfectly well restored, so
    //  every test that asked the engine passed.
    GivenARestoredProblem;
    AssertEquals('the client has nothing to start with', 0, ProfilePointCount);

    FClient.ResyncFromService;
    AssertEquals('and now it has what the engine has', 21, ProfilePointCount);
end;

procedure TClientProjectStateTest.AndTheBackgroundAndThePicksWithIt;
begin
    GivenARestoredProblem;
    FClient.ResyncFromService;
    AssertTrue('the background', Assigned(FClient.GetBackgroundPoints));
    AssertEquals('', 1, FClient.GetBackgroundPoints.PointsCount);
    AssertTrue('the picks', Assigned(FClient.GetCurvePositions));
    AssertEquals('', 1, FClient.GetCurvePositions.PointsCount);
    AssertTrue('and the fit intervals', Assigned(FClient.GetRFactorBounds));
    AssertEquals('', 2, FClient.GetRFactorBounds.PointsCount);
end;

procedure TClientProjectStateTest.ResyncingTwiceIsHarmless;
begin
    //  It drops and re-reads rather than adding to what is there, so opening a
    //  second project does not leave the first one's points on the chart.
    GivenARestoredProblem;
    FClient.ResyncFromService;
    FClient.ResyncFromService;
    AssertEquals('still one profile''s worth', 21, ProfilePointCount);
    AssertEquals('and one background point', 1,
        FClient.GetBackgroundPoints.PointsCount);
end;

procedure TClientProjectStateTest.ResyncingFollowsTheEngineIntoASelectedInterval;
var
    Svc: IFitService;
begin
    //  WHETHER A SUB-INTERVAL IS IN FORCE IS THE ENGINE'S ANSWER. The restore
    //  windowed the problem without going through this client, so a client that
    //  trusted its own flag would draw the whole profile over a windowed model
    //  and hand the wrong set to every edit made afterwards.
    GivenARestoredProblem;
    Svc := FService;
    Svc.SelectProfileInterval(4, 16);

    FClient.ResyncFromService;
    AssertEquals('the client is showing the window, not the whole profile',
        13, ProfilePointCount);
end;

procedure TClientProjectStateTest.StartingEmptyNeedsNoDataLoader;
var
    Raised: boolean;
begin
    //  THE SECOND DEFECT. New Project went through Reload, which asserts there
    //  is a data loader before it does anything - and a session that opened a
    //  PROJECT has never had one. The command faulted instead of clearing.
    GivenARestoredProblem;
    FClient.ResyncFromService;

    Raised := False;
    try
        FClient.StartEmpty;
    except
        on E: Exception do
            Raised := True;
    end;
    AssertFalse('no loader is needed to start again', Raised);
end;

procedure TClientProjectStateTest.StartingEmptyLeavesTheEngineAloneOnPurpose;
var
    Svc: IFitService;
begin
    //  CHARACTERISED, because the obvious alternative was tried and is wrong.
    //  Setting a profile is what resets a problem, but an EMPTY profile is
    //  refused - correctly: an empty profile is not data - so there is no
    //  "empty the problem" call to make.
    //
    //  Nothing needs one. Whatever comes next begins by setting a profile - a
    //  data file, or a project's first restore step - and both reset the
    //  problem, so these leftovers cannot survive into it. Until then nothing
    //  can reach them, which the next test is about.
    GivenARestoredProblem;
    FClient.ResyncFromService;
    FClient.StartEmpty;

    Svc := FService;
    AssertEquals('the engine still holds the old picks', 1,
        Svc.GetCurvePositions.PointsCount);
end;

procedure TClientProjectStateTest.AndReportsThatNothingIsOpen;
begin
    //  WHICH IS WHAT TURNS THE COMMANDS OFF. The window derives every one of
    //  them from this, so an empty window offers nothing that would reach the
    //  leftovers the test above describes.
    GivenARestoredProblem;
    FClient.ResyncFromService;
    FClient.StartEmpty;
    AssertFalse('nothing is open', FClient.OpenState = OpenSuccess);
end;

procedure TClientProjectStateTest.StartingEmptyDropsWhatWasOnTheChart;
begin
    GivenARestoredProblem;
    FClient.ResyncFromService;
    AssertEquals('something to drop', 21, ProfilePointCount);

    FClient.StartEmpty;
    AssertEquals('nothing left', 0, ProfilePointCount);
end;

initialization
    //  A unit test: an engine and a client, both ordinary objects, in one
    //  process. No socket, no file, no fit run to convergence.
    RegisterTest('unit', TClientProjectStateTest);
end.
