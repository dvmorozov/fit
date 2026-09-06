// SPDX-License-Identifier: GPL-3.0-or-later
{ Every pick mode must refresh the computed data.

  A pick changes the model on the SERVER. The client shows nothing until it reads
  the model back, so a dispatch branch that only sends the point leaves the user
  clicking on a chart that never changes - with no error, nothing in the log, and
  nothing failing. That is exactly what happened when the module branch was
  rewritten from a call to a method (which ended in the refresh) to two inline
  lines (which did not): marking stopped working and every test still passed.

  So the invariant is asserted for ALL modes rather than for the one that broke.
  The next branch added here fails this test unless it refreshes too. }
unit testcase_pick_refresh;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry, MyExceptions,
  fit_client, http_fit_service, points_set, title_points_set;

type
  { The transport stubbed out: the picks must not need a server, and the test is
    about what the CLIENT does after sending, not about what is sent. }
  TSilentPickService = class(THttpFitService)
  public
    Sent: longint;
    SentSet: string;
    procedure AddPointToSet(const AKind: string; XValue, YValue: double); override;
    procedure AddPointToRFactorBounds(XValue, YValue: double); override;
    procedure AddPointToCurvePositions(XValue, YValue: double); override;
    function GetProfilePointsSet: TTitlePointsSet; override;
  end;

  { A service that REFUSES a module pick, the way a pattern module declines a
    second pattern over a stretch one already covers. }
  TRefusingPickService = class(TSilentPickService)
  public
    procedure AddPointToSet(const AKind: string; XValue, YValue: double); override;
  end;

  { The real client, counting its own refreshes. }
  TCountingFitClient = class(TFitClient)
  public
    Refreshes: longint;
    procedure UpdateComputedData(ShowExtraData: boolean); override;
    { Entering a mode asserts that the set it draws exists - which in the running
      application a loaded profile guarantees. Supplying them here is what lets
      the test reach every branch of the dispatch rather than only the one that
      needs nothing. }
    procedure GiveEmptySets;
  end;

  TPickRefreshTest = class(TTestCase)
  private
    FSvc: TSilentPickService;
    FClient: TCountingFitClient;
    { Enters AMode, picks one point, and answers how many refreshes it caused. }
    function RefreshesAfterPickIn(AMode: TSelMode): longint;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure APickOnAModulePointSetRefreshes;
    procedure AModulePickIsAlsoShownWhereItWasMade;
    procedure ModulePicksAccumulateRatherThanCancel;
    procedure APickOnACurvePositionRefreshes;
    procedure APickOnAFitIntervalRefreshes;
    procedure APickOnTheBackgroundRefreshes;
    procedure AModulePickNamesItsOwnPointSet;
    procedure ARefusedPickRaisesEUserExceptionAndNothingWorse;
    procedure ARefusedPickLeavesTheClientUsable;
  end;

implementation

procedure TSilentPickService.AddPointToSet(const AKind: string;
  XValue, YValue: double);
begin
  Inc(Sent);
  SentSet := AKind;
end;

procedure TRefusingPickService.AddPointToSet(const AKind: string;
  XValue, YValue: double);
begin
  Inc(Sent);
  SentSet := AKind;
  raise EUserException.Create('The Ending diagonal already covers exactly ' +
    'this stretch, so a second pattern over it was not added.');
end;

procedure TSilentPickService.AddPointToRFactorBounds(XValue, YValue: double);
begin
  Inc(Sent);
end;

procedure TSilentPickService.AddPointToCurvePositions(XValue, YValue: double);
begin
  Inc(Sent);
end;

function TSilentPickService.GetProfilePointsSet: TTitlePointsSet;
begin
  //  UpdateComputedData reads the model back; with no server there is nothing to
  //  read, and an empty profile is a legitimate answer.
  Result := TTitlePointsSet.Create(nil);
end;

procedure TCountingFitClient.GiveEmptySets;
begin
  if not Assigned(FBackgroundPoints) then
    FBackgroundPoints := TTitlePointsSet.Create(nil);
  if not Assigned(FCurvePositions) then
    FCurvePositions := TTitlePointsSet.Create(nil);
  if not Assigned(FRFactorBounds) then
    FRFactorBounds := TTitlePointsSet.Create(nil);
end;

procedure TCountingFitClient.UpdateComputedData(ShowExtraData: boolean);
begin
  Inc(Refreshes);
  //  Deliberately NOT calling inherited: the real refresh needs a viewer and a
  //  server. What is under test is that the dispatch asks for one.
end;

procedure TPickRefreshTest.SetUp;
begin
  //  Nothing listens on this port: these tests must not touch the network.
  FSvc := TSilentPickService.Create('http://127.0.0.1:9');
  FClient := TCountingFitClient.Create;
  FClient.FitService := FSvc;
  FClient.GiveEmptySets;
end;

procedure TPickRefreshTest.TearDown;
begin
  FreeAndNil(FClient);
  FreeAndNil(FSvc);
end;

function TPickRefreshTest.RefreshesAfterPickIn(AMode: TSelMode): longint;
begin
  if AMode = ModeSelectModulePoints then
    FClient.BeginModuleSelection('sample-picks')
  else
    FClient.SelectionMode := AMode;
  FClient.Refreshes := 0;
  FSvc.Sent := 0;
  //  Straight through the entry point the chart's click handler uses.
  FClient.AddPointToActive(10, 20);
  Result := FClient.Refreshes;
end;

procedure TPickRefreshTest.APickOnAModulePointSetRefreshes;
begin
  //  The regression: this branch sent the point and stopped, so a marked pattern
  //  existed on the server and was never drawn.
  AssertEquals('a module pick must refresh the computed data',
    1, RefreshesAfterPickIn(ModeSelectModulePoints));
  AssertEquals('and it must have reached the service', 1, FSvc.Sent);
end;

procedure TPickRefreshTest.APickOnACurvePositionRefreshes;
begin
  AssertEquals('a curve-position pick must refresh',
    1, RefreshesAfterPickIn(ModeSelectCurvePositions));
end;

procedure TPickRefreshTest.APickOnAFitIntervalRefreshes;
begin
  AssertEquals('a fit-interval pick must refresh',
    1, RefreshesAfterPickIn(ModeSelectRFactorBounds));
end;

procedure TPickRefreshTest.APickOnTheBackgroundRefreshes;
begin
  //  The background set is the client's own, so this one redraws rather than
  //  re-reads - but the user must still see the point appear, which is the same
  //  guarantee. Asserted through the plot, not the refresh count.
  RefreshesAfterPickIn(ModeSelectBackground);
  AssertEquals('the point reached the background set',
    1, FClient.GetBackgroundPoints.PointsCount);
end;

procedure TPickRefreshTest.AModulePickIsAlsoShownWhereItWasMade;
begin
  //  A pair of picks bounds one thing, and between the two the server has
  //  nothing to draw. Without a marker of its own the first pick is invisible -
  //  the user cannot see that it landed, or where - so the gesture is only
  //  visible once it is over and a mis-aimed pick cannot be seen at all.
  RefreshesAfterPickIn(ModeSelectModulePoints);
  AssertEquals('the pick must be drawable, not only sent',
    1, FClient.GetSelectedPoints.PointsCount);
  AssertEquals('and at the point that was picked',
    10, FClient.GetSelectedPoints.PointXCoord[0]);
end;

procedure TPickRefreshTest.ModulePicksAccumulateRatherThanCancel;
begin
  //  The framework's own range gestures treat a repeated x as an edit and a
  //  repeated point as a DELETE. A module's picks must not go through that:
  //  a nested pattern shares a bound with its parent, so the second pick of
  //  such a pair would annihilate the first and the gesture would never end.
  RefreshesAfterPickIn(ModeSelectModulePoints);
  FClient.AddPointToActive(10, 20);
  AssertEquals('both picks must stand', 2, FClient.GetSelectedPoints.PointsCount);
  AssertEquals('and both must have reached the service', 2, FSvc.Sent);
end;

procedure TPickRefreshTest.AModulePickNamesItsOwnPointSet;
begin
  //  The set travels beside the mode: a module brings its own, and the framework
  //  must not substitute one of its own sets for it.
  RefreshesAfterPickIn(ModeSelectModulePoints);
  AssertEquals('the pick went to the module''s set', 'sample-picks', FSvc.SentSet);
end;

{ A REFUSED PICK MUST ARRIVE AS EUserException AND NOTHING ELSE.

  That class is the whole contract between a server that declines and a UI that
  explains: TFormMain's chart handler catches EUserException specifically, and
  anything else reaches OnException - the last-resort FAULT handler, which logs at
  Fatal and stops the state poll. So a refusal delivered as a plain Exception
  would reach the user as their own explanation with "Server polling has been
  stopped" stapled to it, which is exactly what happened when the non-Windows
  branch of that handler was a bare `raise`.

  Asserted here because it cannot be asserted where it matters: the handler lives
  on an LCL form, which the headless suite cannot drive. This pins the half that
  can be tested - what the client lets through - so the half that cannot at least
  has a guaranteed input. }
procedure TPickRefreshTest.ARefusedPickRaisesEUserExceptionAndNothingWorse;
var
  Svc: TRefusingPickService;
  Client: TCountingFitClient;
  Raised: boolean;
begin
  Svc := TRefusingPickService.Create('http://127.0.0.1:1');
  Client := TCountingFitClient.Create;
  try
    Client.FitService := Svc;
    Client.GiveEmptySets;
    Client.BeginModuleSelection('wave-bounds');

    Raised := False;
    try
      Client.AddPointToActive(10, 20);
    except
      on E: EUserException do
        Raised := True;
      on E: Exception do
        Fail('a refusal arrived as ' + E.ClassName + ', which the chart ' +
          'handler does not catch: ' + E.Message);
    end;
    AssertTrue('the refusal reaches the caller', Raised);
    AssertEquals('and it was actually sent to the service', 1, Svc.Sent);
  finally
    Client.Free;
    Svc.Free;
  end;
end;

{ ...and the client is still working afterwards. A refusal is an ordinary answer,
  so nothing about it may leave the client in a state where the next pick fails
  for a different reason. }
procedure TPickRefreshTest.ARefusedPickLeavesTheClientUsable;
var
  Svc: TRefusingPickService;
  Client: TCountingFitClient;
  i: longint;
begin
  Svc := TRefusingPickService.Create('http://127.0.0.1:1');
  Client := TCountingFitClient.Create;
  try
    Client.FitService := Svc;
    Client.GiveEmptySets;
    Client.BeginModuleSelection('wave-bounds');

    for i := 1 to 3 do
      try
        Client.AddPointToActive(10 + i, 20);
      except
        on E: EUserException do ;   //  expected, every time
      end;

    AssertEquals('every pick still reached the service', 3, Svc.Sent);
    AssertEquals('and the mode was not silently abandoned', 'wave-bounds',
      Svc.SentSet);
  finally
    Client.Free;
    Svc.Free;
  end;
end;

initialization
  //  UNIT: TSilentPickService and TRefusingPickService are THttpFitService
  //  descendants with the transport overridden, so a pick is dispatched and
  //  counted without a server existing.
  RegisterTest('unit', TPickRefreshTest);
end.
