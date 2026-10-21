// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Fit intervals fitted side by side, through the verb the window calls.)

FIT INTERVALS ARE INDEPENDENT BY CONSTRUCTION, so they are fitted at once on a
bounded pool (docs/internal/fit-performance.md, stage 8; job_pool). What that
must not change is the answer: a fit on several workers reaches exactly the
model the same fit reaches on one, bit for bit, because no interval reads
another's state while it fits. And what it must change is the time - which is
why the server says how many workers the last fit used, and a test can tell a
parallel run from one that only claims to be.

`fitThreads` is the setting: 0, the default, means one worker per processor
(never more than the intervals); 1 is the old loop exactly.
}
unit testcase_parallel_fit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
    fit_rest_api, gauss_points_set;

type
    TParallelFitTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
        function Num(const AValue: double): string;
        { Three peaks, a pick on each, an interval round each, fitted on at
          most AThreads workers. Answers the problem. }
        function FitThreePeaks(AThreads: longint): longint;
        { The three peaks picked, then run automatically - the intervals and
          the reduction the engine's own - on at most AThreads. Picked, because
          with nothing placed the run seeds a curve at every sample, which
          takes minutes and tests nothing more here. }
        function AutomaticThreePeaks(AThreads: longint): longint;
        function NewThreePeaks(AThreads: longint): longint;
        function FittedValues(AId: longint): TStringList;
        function WorkersUsed(AId: longint): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure SideBySideReachesExactlyTheModelOneByOneDoes;
        procedure OneThreadIsTheOldLoop;
        procedure NoMoreWorkersThanIntervals;
        procedure AnAutomaticRunSideBySideReachesTheModelOneByOneDoes;
    end;

implementation

procedure TParallelFitTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TParallelFitTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TParallelFitTest.Call(const M, P, B: string;
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

function TParallelFitTest.Num(const AValue: double): string;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := FloatToStrF(AValue, ffGeneral, 17, 0, FS);
end;

const
    CENTRES: array[0..2] of double = (10, 30, 50);
    { A pick one sample off a peak, at about the profile's height there: a pick
      seeds its curve's amplitude, and one at zero starts a flat curve that no
      engine moves - so two runs would agree whether or not they fitted. }
    PICK_Y = '90';

function TParallelFitTest.NewThreePeaks(AThreads: longint): longint;
var
    Code, i, k: longint;
    R: TJSONObject;
    XS, YS: string;
    Y: double;
begin
    R := Call('POST', '/problems', '', Code);
    try
        Result := R.Get('id', 0);
    finally
        R.Free;
    end;
    XS := '';
    YS := '';
    for i := 0 to 60 do
    begin
        Y := 0;
        for k := 0 to 2 do
            //  A little structure the seeds do not have, so each fit has work
            //  - under the peaks only, so the stretches between them are empty
            //  and the automatic interval search finds three.
            Y := Y + (100 + 3 * Sin(i * 0.7)) * Exp(-Sqr((i - CENTRES[k]) / 3));
        //  A floor that rises every other sample: the peak search walks down
        //  each peak while the values fall, so on a smooth profile the three
        //  walks would meet between the peaks and make one interval.
        Y := Y + 0.1 * (i mod 2);
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + IntToStr(i);
        YS := YS + Num(Y);
    end;
    R := Call('PUT', Format('/problems/%d/profile', [Result]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code);
    R.Free;
    //  Asked for by name: the selection is process-wide, and a suite that
    //  fits Gaussians must not depend on which suite ran before it.
    R := Call('PUT', Format('/problems/%d/settings', [Result]),
        Format('{"fitThreads":%d,"curveType":"%s"}',
        [AThreads, GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code);
    try
        AssertEquals('the thread count is a setting (' + R.Get('error', '') +
            ')', 200, Code);
    finally
        R.Free;
    end;
end;

function TParallelFitTest.AutomaticThreePeaks(AThreads: longint): longint;
var
    Code, k: longint;
    R: TJSONObject;
begin
    Result := NewThreePeaks(AThreads);
    for k := 0 to 2 do
    begin
        R := Call('POST', Format('/problems/%d/points/positions', [Result]),
            Format('{"x":%s,"y":%s}', [Num(CENTRES[k] + 1), PICK_Y]), Code);
        R.Free;
    end;
    R := Call('POST', Format('/problems/%d/actions/do-all-automatically',
        [Result]), '', Code);
    try
        AssertEquals('run automatically (' + R.Get('error', '') + ')', 200,
            Code);
    finally
        R.Free;
    end;
end;

function TParallelFitTest.FitThreePeaks(AThreads: longint): longint;
var
    Code, k: longint;
    R: TJSONObject;
    BX, BY: string;
begin
    Result := NewThreePeaks(AThreads);
    BX := '';
    BY := '';
    for k := 0 to 2 do
    begin
        if k > 0 then
        begin
            BX := BX + ',';
            BY := BY + ',';
        end;
        BX := BX + Num(CENTRES[k] - 9) + ',' + Num(CENTRES[k] + 9);
        BY := BY + '0,0';
        R := Call('POST', Format('/problems/%d/points/positions', [Result]),
            Format('{"x":%s,"y":%s}', [Num(CENTRES[k] + 1), PICK_Y]), Code);
        R.Free;
    end;
    R := Call('PUT', Format('/problems/%d/rfactor-bounds', [Result]),
        Format('{"x":[%s],"y":[%s]}', [BX, BY]), Code);
    R.Free;
    AssertEquals('the intervals are set', 200, Code);
    R := Call('POST', Format('/problems/%d/actions/minimize-difference',
        [Result]), '', Code);
    try
        AssertEquals('fitted (' + R.Get('error', '') + ')', 200, Code);
    finally
        R.Free;
    end;
end;

function TParallelFitTest.FittedValues(AId: longint): TStringList;
var
    Code, i, j: longint;
    R: TJSONObject;
    Curves, Params: TJSONArray;
    P: TJSONObject;
begin
    Result := TStringList.Create;
    R := Call('GET', Format('/problems/%d/curves', [AId]), '', Code);
    try
        Curves := TJSONArray(R.Find('curves'));
        for i := 0 to Curves.Count - 1 do
        begin
            Params := TJSONArray(TJSONObject(Curves.Items[i]).Find('params'));
            for j := 0 to Params.Count - 1 do
            begin
                P := TJSONObject(Params.Items[j]);
                if P.Get('kind', '') = 'text' then
                    Continue;
                Result.Add(Num(P.Get('value', NaN)));
            end;
        end;
    finally
        R.Free;
    end;
end;

function TParallelFitTest.WorkersUsed(AId: longint): longint;
var
    Code: longint;
    R: TJSONObject;
begin
    R := Call('GET', Format('/problems/%d/stats', [AId]), '', Code);
    try
        Result := R.Get('fitWorkers', -1);
    finally
        R.Free;
    end;
end;

procedure TParallelFitTest.SideBySideReachesExactlyTheModelOneByOneDoes;
var
    One, Three: longint;
    A, B: TStringList;
    i: longint;
begin
    One := FitThreePeaks(1);
    Three := FitThreePeaks(3);
    AssertEquals('one by one', 1, WorkersUsed(One));
    AssertEquals('side by side', 3, WorkersUsed(Three));
    A := FittedValues(One);
    B := FittedValues(Three);
    try
        AssertTrue('a model was fitted', A.Count > 0);
        AssertEquals('as many values', A.Count, B.Count);
        for i := 0 to A.Count - 1 do
            AssertEquals(Format('value %d, bit for bit', [i]), A[i], B[i]);
    finally
        A.Free;
        B.Free;
    end;
end;

procedure TParallelFitTest.OneThreadIsTheOldLoop;
begin
    AssertEquals(1, WorkersUsed(FitThreePeaks(1)));
end;

procedure TParallelFitTest.NoMoreWorkersThanIntervals;
begin
    //  Asked for eight; three intervals are three jobs.
    AssertEquals(3, WorkersUsed(FitThreePeaks(8)));
end;

{ A RUN OF STAGES: the automatic run reduces the curves, then fits what is left,
  and the second stage starts from what the first one collected. So each stage
  must end - collected, remembered - before the next begins, side by side as
  one by one; a stage that only the operation's own end collected would hand the
  last stage a model the reduction never reached. }
procedure TParallelFitTest.AnAutomaticRunSideBySideReachesTheModelOneByOneDoes;
var
    One, Three: longint;
    A, B: TStringList;
    i: longint;
begin
    One := AutomaticThreePeaks(1);
    Three := AutomaticThreePeaks(3);
    AssertEquals('one by one', 1, WorkersUsed(One));
    AssertTrue('side by side', WorkersUsed(Three) > 1);
    A := FittedValues(One);
    B := FittedValues(Three);
    try
        AssertTrue('a model was fitted', A.Count > 0);
        AssertEquals('as many values', A.Count, B.Count);
        for i := 0 to A.Count - 1 do
            AssertEquals(Format('value %d, bit for bit', [i]), A[i], B[i]);
    finally
        A.Free;
        B.Free;
    end;
end;

initialization
    RegisterTest('integration', TParallelFitTest);
end.
