// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A fit that finds nothing better still reports the model it holds.)

FOUND ON A REAL PROJECT: fitted, then Fit pressed again, the window read "Not
calculated". Runs are incremental - each starts from the model the last one
left (AGENTS.md) - so an interval already at its best gave the optimiser
nothing to improve, and an interval is measured when the optimiser reports an
improvement. That one never was, and one unmeasured interval makes the whole
model "Not calculated". A fit ends measuring what it holds.
}
unit testcase_converged_fit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_rest_api, gauss_points_set, SimpMath, fit_service, title_points_set,
    points_set;

type
    TConvergedFitTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
        function RFactor(AId: longint): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFitThatCannotImproveStillReportsItsRFactor;
    end;

implementation

procedure TConvergedFitTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TConvergedFitTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TConvergedFitTest.Call(const M, P, B: string;
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

function TConvergedFitTest.RFactor(AId: longint): string;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/stats', [AId]), '', Code);
    try
        Result := R.Get('rFactor', '');
    finally
        R.Free;
    end;
end;

procedure TConvergedFitTest.AFitThatCannotImproveStillReportsItsRFactor;
var
    Code, Id, i: longint;
    R: TJSONObject;
    XS, YS: string;
    FS: TFormatSettings;
    Svc: TFitService;
    Bounds: TTitlePointsSet;
    Picks: TPointsSet;

    function Data(x: double): double;
    begin
        Result := GaussPoint(100, 3, 20, x) + GaussPoint(60, 2, 60, x);
    end;

begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    R := Call('POST', '/problems', '', Code);
    Id := R.Get('id', 0);
    R.Free;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s","curveScaling":false}',
        [GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code).Free;
    XS := '';
    YS := '';
    for i := 0 to 80 do
    begin
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + IntToStr(i);
        YS := YS + FloatToStr(Data(i), FS);
    end;
    Call('PUT', Format('/problems/%d/profile', [Id]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code).Free;
    //  TWO INTERVALS, fitted side by side, one peak in each - the real case
    //  had twenty-two.
    Svc := FApi.Sessions.Find(Id).Service;
    Svc.SetFitThreads(2);
    //  Both setters take what they are given.
    Bounds := TTitlePointsSet.Create(nil);
    Bounds.AddNewPoint(0, Data(0));
    Bounds.AddNewPoint(40, Data(40));
    Bounds.AddNewPoint(41, Data(41));
    Bounds.AddNewPoint(80, Data(80));
    Svc.SetRFactorBounds(Bounds);
    Picks := TPointsSet.Create(nil);
    Picks.AddNewPoint(20, Data(20));
    Picks.AddNewPoint(60, Data(60));
    Svc.SetCurvePositions(Picks);
    //  EACH CURVE IS ITS PEAK EXACTLY, R-factor 0 in each interval, so no step
    //  of any fit improves on it. What the user met came the incremental way: a
    //  fit, and Fit again over intervals already at their best.
    Svc.SetCurveParametersByName(0, ['A', 'sigma', 'x0'], [100, 3, 20]);
    Svc.SetCurveParametersByName(1, ['A', 'sigma', 'x0'], [60, 2, 60]);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '',
        Code).Free;
    AssertEquals('fitted', 200, Code);
    AssertEquals('side by side', 2, Svc.LastFitWorkers);
    AssertTrue('the model reports its R-factor: ' + RFactor(Id),
        RFactor(Id) <> 'Not calculated');
end;

initialization
    //  An INTEGRATION test: it runs the optimiser.
    RegisterTest('integration', TConvergedFitTest);
end.
