// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The randomness verdict, through the route the window reads.)

One peak fitted to a peak with a wave beside it: the model explains the peak
and not the wave, so /stats says the difference is not noise, and names where.
An integration test: it runs the optimiser.
}
unit testcase_residual_randomness_rest;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    fit_rest_api, test_curve_types, SimpMath, fit_service, title_points_set,
    points_set;

type
    TResidualRandomnessRestTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AWaveTheModelLeftOutIsNamedByTheStatistics;
    end;

implementation

procedure TResidualRandomnessRestTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TResidualRandomnessRestTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TResidualRandomnessRestTest.Call(const M, P, B: string;
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

procedure TResidualRandomnessRestTest.AWaveTheModelLeftOutIsNamedByTheStatistics;
var
    Code, Id, i: longint;
    R, St: TJSONObject;
    Ranges, Events: TJSONArray;
    XS, YS: string;
    FS: TFormatSettings;
    Svc: TFitService;
    Bounds: TTitlePointsSet;
    Picks: TPointsSet;
    Seed: longword;
    Y: double;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    R := Call('POST', '/problems', '', Code);
    Id := R.Get('id', 0);
    R.Free;
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s","curveScaling":false}',
        [GUIDToString(TTestGaussPointsSet.GetCurveTypeId)]), Code).Free;
    XS := '';
    YS := '';
    Seed := 3;
    for i := 0 to 300 do
    begin
        Seed := Seed * 1103515245 + 12345;
        Y := GaussPoint(100, 5, 80, i) +
            (((Seed shr 16) and $7FFF) / $7FFF * 2 - 1);
        //  The wave the model has nothing for.
        if i >= 200 then
            Y := Y + 4 * Sin(2 * Pi * i / 25);
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + IntToStr(i);
        YS := YS + FloatToStr(Y, FS);
    end;
    Call('PUT', Format('/problems/%d/profile', [Id]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code).Free;
    Svc := FApi.Sessions.Find(Id).Service;
    Bounds := TTitlePointsSet.Create(nil);
    Bounds.AddNewPoint(0, 0);
    Bounds.AddNewPoint(300, 0);
    Svc.SetRFactorBounds(Bounds);
    Picks := TPointsSet.Create(nil);
    Picks.AddNewPoint(80, 110);
    Svc.SetCurvePositions(Picks);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '',
        Code).Free;
    AssertEquals('fitted', 200, Code);

    R := Call('GET', Format('/problems/%d/stats', [Id]), '', Code);
    try
        St := TJSONObject(R.Find('statistics'));
        AssertTrue('statistics: ' + R.AsJSON, Assigned(St));
        AssertTrue('judged: ' + St.AsJSON, Assigned(St.Find('residualP')));
        AssertTrue('not noise: ' + St.AsJSON, St.Get('residualP', 1.0) < 0.05);
        Ranges := TJSONArray(St.Find('nonRandomRanges'));
        AssertTrue('a stretch is named', Ranges.Count > 0);
        //  WHERE THE WAVE IS, not the peak the model fits (80, done by 95).
        //  The wave begins at 200; a stretch reaches before it as far as the
        //  window of the longest rhythm it registers in - the 32-54 sample
        //  octave, about 50 samples (named from 148 here).
        AssertTrue('where the wave is: ' + Ranges.AsJSON,
            Ranges.Objects[0].Get('from', 0.0) >= 120);
        AssertTrue('and over it: ' + Ranges.AsJSON,
            Ranges.Objects[Ranges.Count - 1].Get('to', 0.0) >= 250);
    finally
        R.Free;
    end;
    //  AND SAID IN THE RUN LOG, as the run's last word (/progress events).
    R := Call('GET', Format('/problems/%d/progress?since=0', [Id]), '', Code);
    try
        Events := TJSONArray(R.Find('events'));
        AssertTrue('the run said something: ' + R.AsJSON, Assigned(Events) and
            (Events.Count > 0));
        AssertEquals('its result', 'Result',
            Events.Objects[Events.Count - 1].Get('section', ''));
        AssertTrue('not random', Pos('not random',
            Events.Objects[Events.Count - 1].Get('title', '')) > 0);
    finally
        R.Free;
    end;
end;

initialization
    RegisterTest('integration', TResidualRandomnessRestTest);
end.
