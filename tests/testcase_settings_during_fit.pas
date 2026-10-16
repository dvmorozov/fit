// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The settings are read while a fit runs, without waiting for it.)

FOUND IN USE: Fit > Automatically on a 22-interval profile left the window
frozen for the whole run. The window refreshes its menus and tool pane while a
fit runs - which curve type is selected, which minimizer, whether scaling is on
- and every one of those reads GET /settings. That route took the problem's
lock, which the running fit holds, so the window waited on its own thread: a
minute to its timeout, then again, for five minutes. The settings cannot change
while a run holds the problem, so the route answers from the copy taken as the
run began.
}
unit testcase_settings_during_fit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_rest_api, fit_service, gauss_points_set, title_points_set, points_set,
    SimpMath, int_fit_service;

type
    TSettingsDuringFitTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheSettingsAreReadWhileAFitRuns;
    end;

implementation

type
    { The fit, on a thread of its own, as a connection of its own. }
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

procedure TSettingsDuringFitTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TSettingsDuringFitTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TSettingsDuringFitTest.Call(const M, P, B: string;
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

procedure TSettingsDuringFitTest.TheSettingsAreReadWhileAFitRuns;
var
    Code, Id, i: longint;
    R: TJSONObject;
    Svc: TFitService;
    Profile: TTitlePointsSet;
    Picks: TPointsSet;
    x, Before, Took: double;
    Started: QWord;
    Fit: TFitThread;
    Busy: boolean;
begin
    R := Call('POST', '/problems', '', Code);
    Id := R.Get('id', 0);
    R.Free;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s","maxRFactor":0.0123}',
        [GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code).Free;
    //  A FIT OF SECONDS: sixteen peaks, four hundred curves in one interval.
    Svc := FApi.Sessions.Find(Id).Service;
    Profile := TTitlePointsSet.Create(nil);
    try
        x := 0;
        while x <= 200 + 1e-9 do
        begin
            Before := 0;
            for i := 0 to 15 do
                Before := Before + GaussPoint(100, 1.5, 6 + i * 12.0, x);
            Profile.AddNewPoint(x, Before);
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
        //  Until the fit holds the problem.
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
        Sleep(200);

        Started := GetTickCount64;
        R := Call('GET', Format('/problems/%d/settings', [Id]), '', Code);
        try
            Took := GetTickCount64 - Started;
            AssertEquals('answered', 200, Code);
            AssertEquals('as they were when the fit began', 0.0123,
                R.Get('maxRFactor', 0.0), 1e-12);
        finally
            R.Free;
        end;
        AssertTrue(Format('without waiting for the fit: %.0f ms', [Took]),
            Took < 500);
    finally
        Call('POST', Format('/problems/%d/actions/stop', [Id]), '', Code).Free;
        Fit.WaitFor;
        Fit.Free;
    end;
end;

initialization
    //  An INTEGRATION test: it runs the optimiser on a thread.
    RegisterTest('integration', TSettingsDuringFitTest);
end.
