// SPDX-License-Identifier: GPL-3.0-or-later
{ The thin client end to end: THttpFitService implements IFitService entirely
  over REST against a real, independently running fit_server. This is the proof
  that the transport which replaced XML-RPC/WST actually carries the whole
  workflow - profile, positions, fit, curves, parameters, R-factor. }
unit testcase_http_fit_service;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, DateUtils, fpcunit, testregistry,
  worker_process_harness,
  http_fit_service, int_fit_service, title_points_set, points_set,
  self_copied_component, named_points_set, mscr_specimen_list,
  persistent_curve_parameters, SimpMath, MyExceptions, fit_statistics,
  //  Curve types the selection tests pick between.
  gauss_points_set, asym_pseudo_voigt_points_set,
  two_branches_pseudo_voigt_points_set, Variants;
type
  THttpFitServiceTest = class(TWorkerProcessTest)
  published
    procedure RunsTheWholeWorkflowOverRest;
    procedure TheProfileAndBackgroundStayTheCallers;
    procedure AnUnreachableServerIsReportedToTheUser;
    procedure ProgressIsReadableWhileAnOperationRuns;
    procedure TheSelectedCurveTypeReachesTheServer;
    procedure TheServerFitsWithTheSelectedCurveType;
    //  Removing one curve, over the wire. The route is new and the two claims
    //  it makes - the curve goes, and it stays gone through a re-fit - are
    //  claims about the ENGINE's state, which only a real request can settle.
    procedure DeletingACurveOverRestRemovesItAndItsPick;
    procedure ADeletedCurveDoesNotComeBackOnTheNextFit;
    procedure DeletingAHandleTheModelHasNotGotIsRefused;
    procedure DeletingTheLastCurveEmptiesTheModelOverRest;
  end;

implementation

type
  { Runs a fit on the server while the test polls it, as the client's worker
    thread does. }
  TFitThread = class(TThread)
  private
    FSvc: THttpFitService;
  public
    Finished_: boolean;
    constructor Create(ASvc: THttpFitService);
    procedure Execute; override;
  end;

constructor TFitThread.Create(ASvc: THttpFitService);
begin
  FSvc := ASvc;
  inherited Create(False);
end;

procedure TFitThread.Execute;
begin
  try
    FSvc.MinimizeDifference;
  except
  end;
  Finished_ := True;
end;


procedure THttpFitServiceTest.RunsTheWholeWorkflowOverRest;
var
  Positions: TTitlePointsSet;
  Curves: TSelfCopiedCompList;
  Attrs: TMSCRCurveList;
  Curve: TNamedPointsSet;
  Profile: TTitlePointsSet;
  RFactor: string;
  Stats: TFitStatistics;
begin
  AssertTrue('the compute server is reachable', FSvc.IsAvailable);

  //  1. Send the profile (the service takes ownership).
  FSvc.SetProfilePointsSet(GaussianProfile);

  //  Read it back over the wire.
  Profile := FSvc.GetProfilePointsSet;
  try
    AssertEquals('profile round-trips', 101, Profile.PointsCount);
    AssertEquals('peak value', GaussPoint(100, 1.5, 10, 10),
      Profile.PointYCoord[50], 1e-9);
  finally
    Profile.Free;
  end;

  //  2. Place one curve at the peak (the y seeds its amplitude).
  Positions := TTitlePointsSet.Create(nil);
  Positions.AddNewPoint(10, GaussPoint(100, 1.5, 10, 10));
  FSvc.SetCurvePositions(Positions);

  //  3. Fit - on the server.
  FSvc.MinimizeDifference;

  //  4. The fitted curves come back as plottable point sets.
  Curves := FSvc.GetCurves;
  try
    AssertTrue('curves returned', Assigned(Curves));
    AssertEquals('one fitted curve', 1, Curves.Count);
    Curve := TNamedPointsSet(Curves.Items[0]);
    AssertTrue('the curve carries its points', Curve.PointsCount > 0);
  finally
    Curves.Free;
  end;

  //  5. Its parameters populate the grid.
  Attrs := FSvc.GetCurveAttributes;
  try
    AssertEquals('attributes for one curve', 1, Attrs.Count);
    AssertTrue('the curve exposes parameters',
      Curve_parameters(Attrs.Items[0]).Count > 0);
  finally
    Attrs.Free;
  end;

  //  6. And the R-factor is reported.
  RFactor := FSvc.GetRFactorStr;
  AssertTrue('an R-factor is reported (' + RFactor + ')', RFactor <> '');

  //  7. The goodness-of-fit statistics come back parsed off the wire.
  Stats := FSvc.GetStatistics;
  AssertTrue('statistics are valid after a fit', Stats.Valid);
  AssertTrue('R-squared is high (' + FloatToStr(Stats.RSquared) + ')',
    Stats.RSquared > 0.95);
  AssertTrue('reduced chi-squared is positive', Stats.ReducedChiSquare > 0);
end;

{ TFitClient goes on using the profile it hands over - it is one of the viewer's
  chart series - and frees the background set itself. A transport that freed
  either left the chart drawing freed memory, which the next allocation (the
  curve positions) reused: computing positions appeared to replace the profile.
  A component owner answers "was it freed?" exactly. }
procedure THttpFitServiceTest.TheProfileAndBackgroundStayTheCallers;
var
  Owner: TComponent;
  Profile, Background: TTitlePointsSet;
begin
  Owner := TComponent.Create(nil);
  try
    Profile := GaussianProfile;
    Owner.InsertComponent(Profile);
    FSvc.SetProfilePointsSet(Profile);
    AssertEquals('the profile must survive the call', 1, Owner.ComponentCount);
    AssertTrue('and still hold its points', Profile.PointsCount > 0);

    Background := TTitlePointsSet.Create(nil);
    Owner.InsertComponent(Background);
    Background.AddNewPoint(0, 1);
    Background.AddNewPoint(20, 1);
    FSvc.SetBackgroundPointsSet(Background);
    AssertEquals('the background must survive the call', 2, Owner.ComponentCount);
    AssertTrue('and still hold its points', Background.PointsCount = 2);
  finally
    Owner.Free;   //  frees both - the caller owns them, as it must
  end;
end;

{ The server may be absent, dead or unreachable at any moment - it is a separate
  process, possibly on another machine. That must reach the user as a message
  naming the server, not as a raw socket error or a silent freeze. }
procedure THttpFitServiceTest.AnUnreachableServerIsReportedToTheUser;
var
  Svc: THttpFitService;
  Raised: boolean;
  Msg: string;
begin
  //  A port nothing listens on.
  Svc := THttpFitService.Create('http://127.0.0.1:9');
  try
    AssertFalse('nothing answers there', Svc.IsAvailable);

    Raised := False;
    Msg := '';
    try
      Svc.GetState;
    except
      on E: EUserException do
      begin
        Raised := True;
        Msg := E.Message;
      end;
    end;
    AssertTrue('an unreachable server raises a user error', Raised);
    AssertTrue('the message names the server (' + Msg + ')',
      Pos('127.0.0.1:9', Msg) > 0);
  finally
    Svc.Free;
  end;
end;

{ The client polls the server's state on its UI thread while an operation runs on
  a worker thread. A single-threaded server answers the poll only after the
  operation finishes, which freezes the application for the whole fit. }
procedure THttpFitServiceTest.ProgressIsReadableWhileAnOperationRuns;
var
  Worker: TFitThread;
  Positions: TPointsSet;
  Started: TDateTime;
  Slowest, Elapsed: int64;
  Polls: integer;
begin
  FSvc.SetProfilePointsSet(GaussianProfile);
  Positions := TPointsSet.Create(nil);
  Positions.AddNewPoint(10, GaussPoint(100, 1.5, 10, 10));
  FSvc.SetCurvePositions(Positions);   //  the service takes this one

  //  The client uses one service: the worker thread runs the action on it...
  Worker := TFitThread.Create(FSvc);
  try
    Slowest := 0;
    Polls := 0;
    //  ...while this connection polls progress, as the UI thread does.
    while not Worker.Finished_ and (Polls < 200) do
    begin
      Started := Now;
      FSvc.GetState;
      Elapsed := MilliSecondsBetween(Now, Started);
      if Elapsed > Slowest then
        Slowest := Elapsed;
      Inc(Polls);
    end;
    Worker.WaitFor;
    AssertTrue(Format('progress stays readable while the fit runs (slowest poll ' +
      '%d ms)', [Slowest]), Slowest < 5000);
  finally
    Worker.Free;
  end;

  //  And the fit really did run.
  AssertTrue('the fit produced a fitted curve', FSvc.GetCurveCount > 0);
  AssertTrue('and reported an R-factor', FSvc.GetRFactorStr <> '');
end;

{ The curve type is a server-side setting: the client picks it in the menu, but
  the fitting happens on the server, which keeps its own selection. It must
  round-trip over REST - the client had no other way to say what to fit with. }
procedure THttpFitServiceTest.TheSelectedCurveTypeReachesTheServer;
begin
  FSvc.SetCurveType(TAsymPseudoVoigtPointsSet.GetCurveTypeId);
  AssertTrue('the server reports back the type it was given', IsEqualGUID(
    TAsymPseudoVoigtPointsSet.GetCurveTypeId, FSvc.GetCurveType));

  //  And a second choice replaces the first, rather than being ignored.
  FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
  AssertTrue('a later choice replaces it', IsEqualGUID(
    TGaussPointsSet.GetCurveTypeId, FSvc.GetCurveType));
end;

{ The symptom the user sees: the fitted curves come back carrying their type
  name, which is what the legend and the results table show. Selecting
  "Asym. Pseudo-Voigt" and getting curves named "2 br. Pseudo-Voigt" - the
  server's default, alphabetically first among the registered types - means the
  selection never arrived. }
{ Places two curves, deletes one over HTTP, and checks the engine agrees. }
procedure THttpFitServiceTest.DeletingACurveOverRestRemovesItAndItsPick;
var
  Positions: TTitlePointsSet;
  Bounds: TTitlePointsSet;
  Before: longint;
  Picks: TTitlePointsSet;
begin
  AssertTrue('the compute server is reachable', FSvc.IsAvailable);
  FSvc.SetProfilePointsSet(GaussianProfile);

  Positions := TTitlePointsSet.Create(nil);
  Positions.AddNewPoint(8, GaussPoint(100, 1.5, 10, 8));
  Positions.AddNewPoint(12, GaussPoint(100, 1.5, 10, 12));
  FSvc.SetCurvePositions(Positions);

  //  BOTH are needed before the engine builds anything: what to fit (the
  //  picks) and where to fit it (an interval). With only picks it is not ready
  //  and the curve list is empty.
  Bounds := TTitlePointsSet.Create(nil);
  Bounds.AddNewPoint(0, 0);
  Bounds.AddNewPoint(20, 0);
  FSvc.SetRFactorBounds(Bounds);

  Before := FSvc.GetCurveCount;
  AssertTrue('two picks made at least two curves', Before >= 2);

  //  OVER THE WIRE, by the handle the model reports for that index - which is
  //  what the route takes, because an index held across an edit names a
  //  different curve.
  FSvc.DeleteCurve(0);

  AssertTrue('one fewer curve', FSvc.GetCurveCount < Before);

  //  AND THE PICK WENT WITH IT. That is what makes the deletion stick: the
  //  model is rebuilt from its inputs, so a pick left standing would put a
  //  fresh instance back on the next rebuild.
  Picks := FSvc.GetCurvePositions;
  try
    AssertEquals('one pick left', 1, Picks.PointsCount);
  finally
    Picks.Free;
  end;
end;

procedure THttpFitServiceTest.ADeletedCurveDoesNotComeBackOnTheNextFit;
var
  Positions: TTitlePointsSet;
  Bounds: TTitlePointsSet;
  AfterDelete: longint;
begin
  AssertTrue('the compute server is reachable', FSvc.IsAvailable);
  FSvc.SetProfilePointsSet(GaussianProfile);

  Positions := TTitlePointsSet.Create(nil);
  Positions.AddNewPoint(8, GaussPoint(100, 1.5, 10, 8));
  Positions.AddNewPoint(12, GaussPoint(100, 1.5, 10, 12));
  FSvc.SetCurvePositions(Positions);

  Bounds := TTitlePointsSet.Create(nil);
  Bounds.AddNewPoint(0, 0);
  Bounds.AddNewPoint(20, 0);
  FSvc.SetRFactorBounds(Bounds);

  FSvc.MinimizeDifference;
  FSvc.DeleteCurve(0);
  AfterDelete := FSvc.GetCurveCount;

  //  THE CLAIM THAT MATTERS. Every model edit frees the task list and rebuilds
  //  every instance from the picks, so a deletion that only dropped the
  //  identity would be undone here - the pick would still be there and the
  //  rebuild would issue a fresh handle for it.
  FSvc.MinimizeDifference;
  AssertEquals('still gone after a re-fit', AfterDelete, FSvc.GetCurveCount);
end;

procedure THttpFitServiceTest.DeletingTheLastCurveEmptiesTheModelOverRest;
var
  Positions: TTitlePointsSet;
  Bounds: TTitlePointsSet;
begin
  //  THE ONE THE USER MET. Deleting the last curve removed its pick and its
  //  identity and left the curve in what the model REPORTS - a separate list
  //  that only a finished fit rebuilds - so the panel kept showing a curve that
  //  was no longer there, and a second attempt on it was told it carried no
  //  handle. Over the wire, because that is where it was seen and because the
  //  count is the server's answer rather than the client's copy of it.
  AssertTrue('the compute server is reachable', FSvc.IsAvailable);
  FSvc.SetProfilePointsSet(GaussianProfile);

  Bounds := TTitlePointsSet.Create(nil);
  Bounds.AddNewPoint(0, 0);
  Bounds.AddNewPoint(20, 0);
  FSvc.SetRFactorBounds(Bounds);

  Positions := TTitlePointsSet.Create(nil);
  Positions.AddNewPoint(10, GaussPoint(100, 1.5, 10, 10));
  FSvc.SetCurvePositions(Positions);
  AssertEquals('one curve to start with', 1, FSvc.GetCurveCount);

  FSvc.DeleteCurve(0);
  AssertEquals('and none afterwards', 0, FSvc.GetCurveCount);
  //  AND NO MARKER LEFT BEHIND. The fitted positions are derived from the curve
  //  list, and one marker stayed on the chart after every curve was deleted -
  //  under a legend row reading "Fitted positions" over a model that held none.
  AssertEquals('nor any fitted position', 0,
    FSvc.GetResultedCurvePositions.PointsCount);
end;

procedure THttpFitServiceTest.DeletingAHandleTheModelHasNotGotIsRefused;
var
  Positions: TTitlePointsSet;
  Bounds: TTitlePointsSet;
  Refused: boolean;
begin
  AssertTrue('the compute server is reachable', FSvc.IsAvailable);
  FSvc.SetProfilePointsSet(GaussianProfile);
  Positions := TTitlePointsSet.Create(nil);
  Positions.AddNewPoint(10, GaussPoint(100, 1.5, 10, 10));
  FSvc.SetCurvePositions(Positions);
  Bounds := TTitlePointsSet.Create(nil);
  Bounds.AddNewPoint(0, 0);
  Bounds.AddNewPoint(20, 0);
  FSvc.SetRFactorBounds(Bounds);

  //  An index the model does not hold resolves to no handle, and the client
  //  refuses before sending rather than letting the server delete whatever
  //  sits at that position now. Deleting the wrong curve is the worst outcome
  //  available here.
  Refused := False;
  try
    FSvc.DeleteCurve(99);
  except
    on E: Exception do
      Refused := True;
  end;
  AssertTrue('refused rather than guessed', Refused);
end;

procedure THttpFitServiceTest.TheServerFitsWithTheSelectedCurveType;
var
  Positions: TTitlePointsSet;
  Curves: TSelfCopiedCompList;
  Curve: TNamedPointsSet;
begin
  //  Guard the premise: if the selected type were the server's default, this
  //  test would pass even with the selection thrown away.
  AssertFalse('the selected type is not the server default', IsEqualGUID(
    TAsymPseudoVoigtPointsSet.GetCurveTypeId,
    T2BranchesPseudoVoigtPointsSet.GetCurveTypeId));

  FSvc.SetCurveType(TAsymPseudoVoigtPointsSet.GetCurveTypeId);
  FSvc.SetProfilePointsSet(GaussianProfile);
  Positions := TTitlePointsSet.Create(nil);
  Positions.AddNewPoint(10, GaussPoint(100, 1.5, 10, 10));
  FSvc.SetCurvePositions(Positions);

  FSvc.MinimizeDifference;

  Curves := FSvc.GetCurves;
  try
    AssertEquals('one fitted curve', 1, Curves.Count);
    Curve := TNamedPointsSet(Curves.Items[0]);
    //  This is the string the legend and the results table display.
    AssertEquals('the fitted curve is of the type the user selected',
      TAsymPseudoVoigtPointsSet.GetCurveTypeName, Curve.FTitle);
  finally
    Curves.Free;
  end;
end;

initialization
  RegisterTest('integration', THttpFitServiceTest);
end.
