// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Stop ends a fit at once, however many parameters it has.)

FOUND IN USE: Stop pressed during a fit of four hundred curves in one interval
took nine and a half seconds to end it. The simplex begins by evaluating a
starting simplex - one evaluation per parameter, twelve hundred of them - and
asks whether it was stopped only between its cycles. A stopped fit now
evaluates nothing more: the simplex finishes what it is building at once and
ends, and the task leaves the best point it really evaluated as the model,
measured.
}
unit testcase_prompt_stop;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_rest_api, fit_service, gauss_points_set, title_points_set, points_set,
    SimpMath, int_fit_service;

type
    TPromptStopTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure StopEndsAFitStillBuildingItsSimplexAtOnce;
    end;

implementation

type
    TFitThread = class(TThread)
    public
        Api: TFitRestApi;
        Id: longint;
    protected
        procedure Execute; override;
    end;

procedure TFitThread.Execute;
var
    Code: longint;
    Resp: string;
begin
    Api.Handle('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
        '', Code, Resp);
end;

procedure TPromptStopTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TPromptStopTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TPromptStopTest.Call(const M, P, B: string;
    out Code: longint): TJSONObject;
var
    Resp: string;
    D: TJSONData;
begin
    FApi.Handle(M, P, B, Code, Resp);
    D := GetJSON(Resp);
    if D is TJSONObject then
        Result := TJSONObject(D)
    else
    begin
        D.Free;
        Result := TJSONObject.Create;
    end;
end;

procedure TPromptStopTest.StopEndsAFitStillBuildingItsSimplexAtOnce;
var
    Code, Id, i: longint;
    R: TJSONObject;
    Svc: TFitService;
    Profile: TTitlePointsSet;
    Picks: TPointsSet;
    x, Sum: double;
    Stopped: QWord;
    Fit: TFitThread;
    Busy: boolean;
    LeftAt: string;
begin
    R := Call('POST', '/problems', '', Code);
    Id := R.Get('id', 0);
    R.Free;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s"}', [GUIDToString(TGaussPointsSet.GetCurveTypeId)]),
        Code).Free;
    //  FOUR HUNDRED CURVES IN ONE INTERVAL: twelve hundred parameters, seconds
    //  of evaluations before the simplex's first cycle.
    Svc := FApi.Sessions.Find(Id).Service;
    Profile := TTitlePointsSet.Create(nil);
    try
        x := 0;
        while x <= 200 + 1e-9 do
        begin
            Sum := 0;
            for i := 0 to 15 do
                Sum := Sum + GaussPoint(100, 1.5, 6 + i * 12.0, x);
            Profile.AddNewPoint(x, Sum);
            x := x + 0.25;
        end;
        Svc.SetProfilePointsSet(Profile);
    finally
        Profile.Free;
    end;
    Picks := TPointsSet.Create(nil);
    for i := 1 to 398 do
        Picks.AddNewPoint(i * 0.5, 1);
    Svc.SetCurvePositions(Picks);

    Fit := TFitThread.Create(True);
    try
        Fit.Api := FApi;
        Fit.Id := Id;
        Fit.Start;
        Busy := False;
        for i := 1 to 500 do
        begin
            R := Call('GET', Format('/problems/%d/state', [Id]), '', Code);
            try
                Busy := R.Get('state', 0) = Ord(AsyncOperation);
            finally
                R.Free;
            end;
            if Busy then
                Break;
            Sleep(10);
        end;
        AssertTrue('the fit is running', Busy);
        //  Inside the starting simplex: past building the tasks, long before
        //  the simplex's first cycle.
        Sleep(500);
        Stopped := GetTickCount64;
        Call('POST', Format('/problems/%d/actions/stop', [Id]), '', Code).Free;
        Fit.WaitFor;
        AssertTrue(Format('ended %d ms after Stop', [GetTickCount64 - Stopped]),
            GetTickCount64 - Stopped < 2000);
    finally
        Fit.Free;
    end;
    R := Call('GET', Format('/problems/%d/stats', [Id]), '', Code);
    try
        LeftAt := R.Get('rFactor', '');
    finally
        R.Free;
    end;
    AssertTrue('and the model it left reports its R-factor: ' + LeftAt,
        LeftAt <> 'Not calculated');
end;

initialization
    //  An INTEGRATION test: it runs the optimiser on a thread.
    RegisterTest('integration', TPromptStopTest);
end.
