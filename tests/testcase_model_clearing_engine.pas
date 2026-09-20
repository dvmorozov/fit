// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Clearing the whole model against a real engine, in process.)

A SEPARATE UNIT because the one curve shape no pick places is a user formula,
and the user-defined curve reaches the LCL - so this half cannot be in the
plain-FPC test binary, and the mocked half beside it (testcase_model_clearing)
should not be dragged out of it.
}
unit testcase_model_clearing_engine;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_ui_host, fit_client, model_clearing, fit_service,
    title_points_set,
    persistent_curve_parameters, persistent_curve_parameter_container,
    special_curve_parameter, user_curve_parameter, user_points_set,
    http_fit_service, fit_rest_api,
    mock_ui_host, mock_fit_viewer;

type
    { THE SAME GESTURE AGAINST A REAL ENGINE, in process.

      The tests above prove the client asks for every deletion; they cannot
      prove the model is EMPTY afterwards, because a mocked transport answers
      the same curves whatever was deleted. Whether each deletion sticks - or
      the next rebuild puts a curve back - is the engine's to answer. }
    TModelClearingEngineTest = class(TTestCase)
    private
        FService: TFitService;
        FHostObj: TMockUiHost;
        FHost: IUiHost;
        FView: TMockFitViewer;
        FClient: TFitClient;
        procedure GivenAPeakedProfileWithBounds;
        procedure GivenAFormulaCurveWithNoPosition;
        function ParametersWithoutAPosition: Curve_parameters;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ClearingLeavesTheEngineWithNoCurves;
        procedure AndNoPicksToRebuildThemFrom;
        procedure ACompleteClearingSaysNothingMore;
        procedure ACurveTheEngineKeepsIsExplainedNotForced;
        procedure AndIsNotCountedAsRemoved;
        procedure AndTheCurveTypeIsLeftAlone;
    end;

    { THE CLIENT'S OWN TRANSPORT, answered by the server's own router.

      Loopback, in process: the requests are the ones THttpFitService writes,
      and they are routed by the TFitRestApi fit_server runs - so what is tested
      is the wire contract the application actually uses, with no socket. }
    TLoopbackFitService = class(THttpFitService)
    private
        FApi: TFitRestApi;
        function PathOf(const AUrl: string): string;
    protected
        //  LONGINT, not integer: http_fit_service is a Delphi-mode unit, where
        //  integer is 32 bits, and in this objfpc unit integer is 16.
        function Fetch(const AUrl: string; ATimeoutMs: longint): string; override;
        function Send(const AMethod, AUrl, ABody: string;
            ATimeoutMs: longint): string; override;
    public
        constructor Create(AApi: TFitRestApi);
    end;

    { Model > Clear Model through REST, the surface the application reaches. }
    TModelClearingRestTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        FSvc: TLoopbackFitService;
        FHostObj: TMockUiHost;
        FHost: IUiHost;
        FView: TMockFitViewer;
        FClient: TFitClient;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ClearingOverTheWireLeavesTheServerWithNoCurves;
        procedure AndNoPicks;
        { Model > Curve Positions > Remove All and Model > Fit Intervals >
          Remove All, over the same wire. They used to drop the client's copy
          only, and the server went on holding both. }
        procedure RemoveAllPositionsLeavesTheServerWithNoPicksAndNoCurves;
        procedure RemoveAllFitIntervalsLeavesTheServerWithNoBounds;
        { Data > Range > Select Data Interval a second time: the server used to
          refuse it with "Range of data already selected", the client dropped
          the refusal and drew the new interval, and the engine went on fitting
          the old one. }
        procedure ASecondDataIntervalReplacesTheFirstOnTheServer;
        { Typing a new position into the Curve Positions table: the pick moves
          on the server to the nearest sample, and the curve moves with it. }
        procedure TypingAPositionMovesThePickOnTheServer;
        procedure TypingABoundMovesItOnTheServer;
        { Typing a value into the Curve Attributes table: the engine holds it
          for that parameter of that curve. }
        procedure TypingAParameterValueReachesTheEngine;
        procedure TypingAComputedParameterIsRefused;
        { A user-curve parameter held at its value travels to the server and
          back held - the engine is what keeps it from being varied. }
        procedure AHeldUserCurveParameterReachesTheServerHeld;
    end;

implementation

{ ---- against a real engine ---- }

procedure TModelClearingEngineTest.SetUp;
begin
    FService := TFitService.Create;
    FHostObj := TMockUiHost.Create;
    FHost := FHostObj;
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FService;
    FClient.FFitViewer := FView;
end;

procedure TModelClearingEngineTest.TearDown;
begin
    FClient.FFitViewer := nil;
    FClient.FitService := nil;
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FService);
end;

procedure TModelClearingEngineTest.GivenAPeakedProfileWithBounds;
var
    P, B: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    for i := 0 to 20 do
        P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
    FService.SetProfilePointsSet(P);
    B := TTitlePointsSet.Create(nil);
    B.AddNewPoint(0, 0);
    B.AddNewPoint(20, 0);
    FService.SetRFactorBounds(B);
end;

procedure TModelClearingEngineTest.ClearingLeavesTheEngineWithNoCurves;
begin
    GivenAPeakedProfileWithBounds;
    FService.AddPointToCurvePositions(6, 50);
    FService.AddPointToCurvePositions(10, 110);
    FService.AddPointToCurvePositions(14, 50);
    AssertEquals('three curves to start with', 3, FService.GetCurveCount);

    AssertEquals('all three reported removed', 3,
        ClearModelWithConsent(FHost, FClient));
    AssertEquals('and none left in the engine', 0, FService.GetCurveCount);
end;

procedure TModelClearingEngineTest.AndNoPicksToRebuildThemFrom;
begin
    //  A pick left behind would put its curve straight back on the next edit.
    GivenAPeakedProfileWithBounds;
    FService.AddPointToCurvePositions(6, 50);
    FService.AddPointToCurvePositions(14, 50);
    ClearModelWithConsent(FHost, FClient);
    AssertEquals('no picks', 0, FService.GetCurvePositions.PointsCount);
end;

procedure TModelClearingEngineTest.GivenAFormulaCurveWithNoPosition;
begin
    GivenAPeakedProfileWithBounds;
    FService.SetCurveType(TUserPointsSet.GetCurveTypeId);
    FService.SetSpecialCurveParameters('A*exp(-x*x)',
        ParametersWithoutAPosition);
    FService.AddPointToCurvePositions(10, 110);
    AssertTrue('the formula was built into a curve',
        FService.GetCurveCount > 0);
end;

function TModelClearingEngineTest.ParametersWithoutAPosition: Curve_parameters;

    procedure Add(const AName: string; AType: TParameterType; AValue: double);
    var
        P: TSpecialCurveParameter;
    begin
        P := TUserCurveParameter.Create;
        P.Name := AName;
        P.Type_ := AType;
        P.Value := AValue;
        TPersistentCurveParameterContainer(Result.Params.Add).Parameter := P;
    end;

begin
    Result := Curve_parameters.Create(nil);
    Result.Params.Clear;
    Add('x', Argument, 0);
    Add('A', Variable, 1);
end;

procedure TModelClearingEngineTest.ACompleteClearingSaysNothingMore;
begin
    GivenAPeakedProfileWithBounds;
    FService.AddPointToCurvePositions(6, 50);
    ClearModelWithConsent(FHost, FClient);
    AssertFalse('no message after a clean clearing', FHostObj.ShowedAMessage);
end;

{ SMOOTH, NOT FORCED. A formula declaring no position is placed by neither a
  pick nor a module: the engine keeps one per fit interval, and removing it would
  mean changing the curve type or the intervals behind the user's back. So the
  curve stays, nothing raises, and the user is told once, in plain words, why. }
procedure TModelClearingEngineTest.ACurveTheEngineKeepsIsExplainedNotForced;
begin
    GivenAFormulaCurveWithNoPosition;
    ClearModelWithConsent(FHost, FClient);
    AssertTrue('told once', FHostObj.ShowedAMessage);
    AssertTrue('how many stayed: ' + FHostObj.LastMessage,
        Pos('1 curve ', FHostObj.LastMessage) > 0);
end;

procedure TModelClearingEngineTest.AndIsNotCountedAsRemoved;
begin
    GivenAFormulaCurveWithNoPosition;
    AssertEquals('the curve is still there, so none went', 0,
        ClearModelWithConsent(FHost, FClient));
end;

procedure TModelClearingEngineTest.AndTheCurveTypeIsLeftAlone;
begin
    GivenAFormulaCurveWithNoPosition;
    ClearModelWithConsent(FHost, FClient);
    //  THE TYPE, not the count: forcing the curve out by switching to a type
    //  placed from picks would rebuild a curve on the pick that is still there,
    //  and the count would read 1 either way.
    AssertEquals('not switched to make the curve go',
        GUIDToString(TUserPointsSet.GetCurveTypeId),
        GUIDToString(FService.GetCurveType));
end;

{ ---- over the wire ---- }

constructor TLoopbackFitService.Create(AApi: TFitRestApi);
begin
    inherited Create('http://loopback');
    FApi := AApi;
end;

function TLoopbackFitService.PathOf(const AUrl: string): string;
begin
    Result := Copy(AUrl, Length('http://loopback') + 1, MaxInt);
end;

function TLoopbackFitService.Fetch(const AUrl: string;
    ATimeoutMs: longint): string;
var
    Code: longint;
begin
    FApi.Handle('GET', PathOf(AUrl), '', Code, Result);
end;

function TLoopbackFitService.Send(const AMethod, AUrl, ABody: string;
    ATimeoutMs: longint): string;
var
    Code: longint;
begin
    FApi.Handle(AMethod, PathOf(AUrl), ABody, Code, Result);
end;

procedure TModelClearingRestTest.SetUp;
begin
    FApi := TFitRestApi.Create;
    FSvc := TLoopbackFitService.Create(FApi);
    FHostObj := TMockUiHost.Create;
    FHost := FHostObj;
    FView := TMockFitViewer.Create;
    FClient := TFitClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
end;

procedure TModelClearingRestTest.TearDown;
begin
    FClient.FFitViewer := nil;
    FClient.FitService := nil;
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FSvc);
    FreeAndNil(FApi);
end;

procedure TModelClearingRestTest.ClearingOverTheWireLeavesTheServerWithNoCurves;
var
    P, B: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
        FSvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(20, 0);
        //  Handed over: this setter frees its argument, as the engine's does.
        FSvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;
    FSvc.AddPointToCurvePositions(6, 50);
    FSvc.AddPointToCurvePositions(14, 50);
    AssertEquals('two curves on the server', 2, FSvc.GetCurveCount);

    AssertEquals('both gone', 2, ClearModelWithConsent(FHost, FClient));
    AssertEquals('as the server reports it', 0, FSvc.GetCurveCount);
end;

procedure TModelClearingRestTest.AndNoPicks;
var
    P, B, Picks: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
        FSvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(20, 0);
        //  Handed over: this setter frees its argument, as the engine's does.
        FSvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;
    FSvc.AddPointToCurvePositions(6, 50);
    ClearModelWithConsent(FHost, FClient);
    Picks := FSvc.GetCurvePositions;
    try
        AssertEquals('no picks left to rebuild from', 0, Picks.PointsCount);
    finally
        Picks.Free;
    end;
end;

procedure TModelClearingRestTest.RemoveAllPositionsLeavesTheServerWithNoPicksAndNoCurves;
var
    P, B, Picks: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
        FSvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(20, 0);
        FSvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;
    FSvc.AddPointToCurvePositions(6, 50);
    FSvc.AddPointToCurvePositions(14, 50);
    AssertEquals('two curves first', 2, FSvc.GetCurveCount);

    FClient.RemoveAllCurvePositions;

    Picks := FSvc.GetCurvePositions;
    try
        AssertEquals('no picks on the server', 0, Picks.PointsCount);
    finally
        Picks.Free;
    end;
    AssertEquals('and no curves built from them', 0, FSvc.GetCurveCount);
end;

procedure TModelClearingRestTest.RemoveAllFitIntervalsLeavesTheServerWithNoBounds;
var
    P, B, Bounds: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
        FSvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(20, 0);
        FSvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;

    FClient.RemoveAllRFactorBounds;

    Bounds := FSvc.GetRFactorBounds;
    try
        AssertEquals('no bounds on the server', 0, Bounds.PointsCount);
    finally
        Bounds.Free;
    end;
end;

procedure TModelClearingRestTest.ASecondDataIntervalReplacesTheFirstOnTheServer;
var
    P, Interval: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + i);
        FSvc.SetProfilePointsSet(P);
    finally
        P.Free;
    end;
    //  The client's own copy of the profile, which the gesture indexes into -
    //  read back the way a restored project's is.
    FClient.ResyncFromService;

    FClient.SelectProfileInterval(2, 6);
    FClient.SelectProfileInterval(8, 14);

    Interval := FSvc.GetSelectedProfileInterval;
    try
        AssertEquals('the second interval, as the server holds it', 7,
            Interval.PointsCount);
        AssertEquals('starting where it was picked', 8.0,
            Interval.PointXCoord[0], 1e-9);
    finally
        Interval.Free;
    end;
end;

procedure TModelClearingRestTest.TypingAPositionMovesThePickOnTheServer;
var
    P, B, Picks: TTitlePointsSet;
    i: longint;
    Why: string;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
        FSvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(20, 0);
        FSvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;
    FSvc.AddPointToCurvePositions(6, 50);
    FSvc.AddPointToCurvePositions(14, 50);
    FClient.ResyncFromService;

    AssertTrue('moved: ' + Why, FClient.MovePick(pkCurvePositions, 0, '8.3', Why));

    Picks := FSvc.GetCurvePositions;
    try
        AssertEquals('still two picks', 2, Picks.PointsCount);
        AssertTrue('one at the nearest sample, 8',
            Picks.IndexOfValueX(8) >= 0);
        AssertTrue('none left at 6', Picks.IndexOfValueX(6) < 0);
    finally
        Picks.Free;
    end;
    AssertEquals('and still two curves', 2, FSvc.GetCurveCount);
end;

procedure TModelClearingRestTest.TypingABoundMovesItOnTheServer;
var
    P, B, Bounds: TTitlePointsSet;
    i: longint;
    Why: string;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + i);
        FSvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 10);
        B.AddNewPoint(20, 30);
        FSvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;
    FClient.ResyncFromService;

    //  The end of the first interval.
    AssertTrue('moved: ' + Why, FClient.MovePick(pkFitBounds, 1, '15', Why));

    Bounds := FSvc.GetRFactorBounds;
    try
        AssertTrue('the end is at 15', Bounds.IndexOfValueX(15) >= 0);
        AssertTrue('and no longer at 20', Bounds.IndexOfValueX(20) < 0);
    finally
        Bounds.Free;
    end;
end;

procedure GivenTwoCurves(ASvc: TLoopbackFitService);
var
    P, B: TTitlePointsSet;
    i: longint;
begin
    P := TTitlePointsSet.Create(nil);
    B := TTitlePointsSet.Create(nil);
    try
        for i := 0 to 20 do
            P.AddNewPoint(i, 10 + 100 * Exp(-Sqr((i - 10) / 2.5)));
        ASvc.SetProfilePointsSet(P);
        B.AddNewPoint(0, 0);
        B.AddNewPoint(20, 0);
        ASvc.SetRFactorBounds(B);
    finally
        P.Free;
    end;
    ASvc.AddPointToCurvePositions(6, 50);
    ASvc.AddPointToCurvePositions(14, 50);
end;

procedure TModelClearingRestTest.TypingAParameterValueReachesTheEngine;
var
    Names: TStringList;
    Col, ParamIndex: longint;
    Value: double;
    Why, Name_: string;
    V: double;
    T: longint;
begin
    GivenTwoCurves(FSvc);
    FClient.ResyncFromService;
    Names := TStringList.Create;
    try
        FClient.CurveAttributesForDisplay.CollectColumnNames(Names);
        //  The first column the fit varies.
        Col := 0;
        while (Col < Names.Count) and
            not FClient.CurveAttributesForDisplay.ColumnIsEditable(Col) do
            Inc(Col);
        AssertTrue('an editable column', Col < Names.Count);
        AssertTrue('planned: ' + Why, FClient.CurveAttributesForDisplay.
            PlanParameterEdit(1, Col, '3.25', ParamIndex, Value, Why));

        AssertTrue('applied: ' + Why, FClient.EditCurveParameter(1, Col,
            '3.25', Why));
        FSvc.GetCurveParameter(1, ParamIndex, Name_, V, T);
        AssertEquals('the engine holds it', Names[Col], Name_);
        AssertEquals('at the typed value', Value, V, 1e-9);
    finally
        Names.Free;
    end;
end;

procedure TModelClearingRestTest.TypingAComputedParameterIsRefused;
var
    Names: TStringList;
    Col: longint;
    Why: string;
begin
    GivenTwoCurves(FSvc);
    FClient.ResyncFromService;
    Names := TStringList.Create;
    try
        FClient.CurveAttributesForDisplay.CollectColumnNames(Names);
        AssertFalse('refused', FClient.EditCurveParameter(0, 0, 'abc', Why));
        AssertTrue('in words: ' + Why, Why <> '');
    finally
        Names.Free;
    end;
end;

procedure TModelClearingRestTest.AHeldUserCurveParameterReachesTheServerHeld;
var
    CP, Back: Curve_parameters;
    Container: TPersistentCurveParameterContainer;
    P: TUserCurveParameter;
    i: longint;
    Found: boolean;
begin
    //  A problem to hold it: the engine takes a curve definition only then.
    GivenTwoCurves(FSvc);
    CP := Curve_parameters.Create(nil);
    CP.Params.Clear;
    P := TUserCurveParameter.Create;
    P.Name := 'k';
    P.Type_ := Variable;
    P.Value := 2.5;
    P.VariationDisabled := True;
    Container := TPersistentCurveParameterContainer(CP.Params.Add);
    Container.Parameter := P;
    //  Handed over: the service takes the set.
    FSvc.SetSpecialCurveParameters('k*x', CP);

    Back := FSvc.GetSpecialCurveParameters;
    try
        Found := False;
        for i := 0 to Back.Count - 1 do
            if Back[i].Name = 'k' then
            begin
                Found := True;
                AssertTrue('held on the server', Back[i].VariationDisabled);
            end;
        AssertTrue('the parameter came back', Found);
    finally
        Back.Free;
    end;
end;

initialization
    RegisterTest('unit', TModelClearingEngineTest);
    //  UNIT: in process, no socket - the transport is overridden onto the router.
    RegisterTest('unit', TModelClearingRestTest);
end.
