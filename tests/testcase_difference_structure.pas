// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The "Difference structure" curve the model chart draws under the
difference.)
}
unit testcase_difference_structure;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, fpcunit, testregistry, fpjson, jsonparser,
    residual_randomness, difference_structure, fit_window;

type
    TDifferenceStructureTest = class(TTestCase)
    private
        X, Data, Fit, Noise: TDoubleArray;
        { 400 samples of noise of variance 4 on a level the model holds, with
          a wave of 20 samples over 250..330 the model has nothing for. Noise
          is that variance at every sample, as a noise rule would give it. }
        procedure WaveTheModelMisses;
        { The structure over the fit intervals ABoundsX, as the window takes
          it (FitWindowOf, the noise beside the data). }
        function StructureOver(const ABoundsX: array of double): TDifferenceStructure;
    published
        //  ONE MECHANISM: the curve reaches its level at exactly the samples
        //  the statistics shade.
        procedure TheCurveReachesItsLevelExactlyWhereTheStretchesAre;
        procedure OnlyTheFitIntervalsSamplesAreDrawn;
        //  The difference itself rides along, drawn on the strip's right axis
        //  so its spikes sit over the curve's.
        procedure TheDifferenceIsTheDataLessTheModelAtTheSameSamples;
        procedure NoModelDrawsNothing;
        //  The framework knows no field's noise: with none, nothing is judged.
        procedure WithNoNoiseNothingIsDrawn;
        //  Not a flat line of zeros: that would read as noise everywhere.
        procedure IntervalsTooShortToJudgeDrawNothing;
        //  NOT JOINED ACROSS A GAP between two fit intervals: a straight line
        //  over samples nobody judged read as a value there (the FullProfile
        //  recording, 2026-10-14).
        procedure TheLineBreaksBetweenTwoFitIntervals;
        //  THE SERVER COMPUTES IT, THE WINDOW DRAWS IT: what crosses the wire
        //  is the strip as it was computed, to the last digit.
        procedure TheWireCarriesTheStripAsItWasComputed;
        //  JSON has no infinity: nothing judged travels without a level.
        procedure NothingJudgedTravelsWithoutALevel;
        //  A reply that is not a strip is drawn as nothing judged, never
        //  from a half-read array.
        procedure AReplyThatIsNotAStripReadsAsNothingJudged;
    end;

implementation

procedure TDifferenceStructureTest.WaveTheModelMisses;
var
    i: longint;
begin
    RandSeed := 31;
    SetLength(X, 400);
    SetLength(Data, 400);
    SetLength(Fit, 400);
    SetLength(Noise, 400);
    for i := 0 to 399 do
    begin
        X[i] := i;
        Noise[i] := 4;
        Fit[i] := 100;
        Data[i] := 100 + 2 * Sqrt(-2 * Ln(1 - Random)) * Cos(2 * Pi * Random);
        if (i >= 250) and (i <= 330) then
            Data[i] := Data[i] + 5 * Sin(2 * Pi * i / 20);
    end;
end;

function TDifferenceStructureTest.StructureOver(
    const ABoundsX: array of double): TDifferenceStructure;
begin
    Result := DifferenceStructureOf(FitWindowOf(X, Data, Fit, Noise, ABoundsX));
end;

procedure TDifferenceStructureTest.TheCurveReachesItsLevelExactlyWhereTheStretchesAre;
var
    S: TDifferenceStructure;
    R: TResidualRandomness;
    i, k: longint;
    Shaded: boolean;
begin
    WaveTheModelMisses;
    S := StructureOver([0, 399]);
    R := JudgeResiduals(X, Data, Fit, [SampleSpan(0, 399)], Noise);
    AssertTrue('something is shaded', Length(R.NonRandomRanges) > 0);
    AssertEquals('a value per sample', 400, Length(S.Power));
    for i := 0 to High(S.X) do
    begin
        Shaded := False;
        for k := 0 to High(R.NonRandomRanges) do
            Shaded := Shaded or ((S.X[i] >= R.NonRandomRanges[k].FromX) and
                (S.X[i] <= R.NonRandomRanges[k].ToX));
        AssertEquals(Format('sample %g', [S.X[i]]), Shaded,
            S.Power[i] >= S.Level);
    end;
end;

procedure TDifferenceStructureTest.OnlyTheFitIntervalsSamplesAreDrawn;
var
    S: TDifferenceStructure;
begin
    WaveTheModelMisses;
    S := StructureOver([100, 299]);
    AssertEquals('the interval''s samples', 200, Length(S.X));
    AssertEquals('from its start', 100, S.X[0], 0);
    AssertEquals('to its end', 299, S.X[High(S.X)], 0);
end;

procedure TDifferenceStructureTest.TheLineBreaksBetweenTwoFitIntervals;
var
    S: TDifferenceStructure;
    GX, GY: TDoubleArray;
    i, Gaps: longint;
begin
    WaveTheModelMisses;
    S := StructureOver([0, 179, 220, 399]);
    DrawnByInterval(S, S.Power, GX, GY);
    AssertEquals('every sample and one break', Length(S.X) + 1, Length(GX));
    Gaps := 0;
    for i := 0 to High(GY) do
        if IsNan(GY[i]) then
        begin
            Inc(Gaps);
            AssertTrue('between the two', (GX[i] > 179) and (GX[i] < 220));
        end;
    AssertEquals('one break', 1, Gaps);
    //  One interval, no break.
    S := StructureOver([0, 399]);
    DrawnByInterval(S, S.Difference, GX, GY);
    AssertEquals('no break in one interval', Length(S.X), Length(GX));
end;

procedure TDifferenceStructureTest.TheDifferenceIsTheDataLessTheModelAtTheSameSamples;
var
    S: TDifferenceStructure;
    i: longint;
begin
    WaveTheModelMisses;
    S := StructureOver([100, 299]);
    AssertEquals('one per sample', Length(S.X), Length(S.Difference));
    for i := 0 to High(S.X) do
        AssertEquals(Format('at %g', [S.X[i]]),
            Data[Round(S.X[i])] - Fit[Round(S.X[i])], S.Difference[i], 1e-12);
end;

procedure TDifferenceStructureTest.NoModelDrawsNothing;
var
    S: TDifferenceStructure;
begin
    S := DifferenceStructureOf(FitWindowOf([], [], [], [], []));
    AssertEquals('nothing', 0, Length(S.X));
    AssertTrue('and no level', IsInfinite(S.Level));
end;

procedure TDifferenceStructureTest.WithNoNoiseNothingIsDrawn;
var
    S: TDifferenceStructure;
begin
    WaveTheModelMisses;
    S := DifferenceStructureOf(FitWindowOf(X, Data, Fit, [0, 399]));
    AssertEquals('nothing drawn', 0, Length(S.X));
    AssertTrue('and no level', IsInfinite(S.Level));
end;

procedure TDifferenceStructureTest.IntervalsTooShortToJudgeDrawNothing;
var
    S: TDifferenceStructure;
begin
    WaveTheModelMisses;
    S := StructureOver([100, 110, 200, 212]);
    AssertEquals('nothing drawn', 0, Length(S.X));
    AssertTrue('and no level', IsInfinite(S.Level));
end;

procedure TDifferenceStructureTest.TheWireCarriesTheStripAsItWasComputed;
var
    Sent, Read: TDifferenceStructure;
    O: TJSONObject;
    D: TJSONData;
    i: longint;
begin
    WaveTheModelMisses;
    Sent := StructureOver([0, 199, 220, 399]);
    AssertTrue('something to carry', Length(Sent.X) > 0);
    O := DifferenceStructureToJson(Sent);
    try
        //  Through the text, as the wire carries it.
        D := GetJSON(O.AsJSON);
    finally
        O.Free;
    end;
    try
        AssertTrue('read back', DifferenceStructureFromJson(D, Read));
    finally
        D.Free;
    end;
    AssertEquals('samples', Length(Sent.X), Length(Read.X));
    AssertEquals('the level', Sent.Level, Read.Level, 0);
    for i := 0 to High(Sent.X) do
    begin
        AssertEquals('x', Sent.X[i], Read.X[i], 0);
        AssertEquals('power', Sent.Power[i], Read.Power[i], 0);
        AssertEquals('difference', Sent.Difference[i], Read.Difference[i], 0);
        AssertEquals('interval', Sent.SpanOf[i], Read.SpanOf[i]);
    end;
    AssertEquals('both intervals', 1, Read.SpanOf[High(Read.SpanOf)]);
end;

procedure TDifferenceStructureTest.NothingJudgedTravelsWithoutALevel;
var
    S: TDifferenceStructure;
    O: TJSONObject;
begin
    S := DifferenceStructureOf(FitWindowOf([], [], [], [], []));
    O := DifferenceStructureToJson(S);
    try
        AssertNull('no level: ' + O.AsJSON, O.Find('level'));
        AssertTrue('read back', DifferenceStructureFromJson(O, S));
    finally
        O.Free;
    end;
    AssertEquals('nothing', 0, Length(S.X));
    AssertTrue('and no level', IsInfinite(S.Level));
end;

procedure TDifferenceStructureTest.AReplyThatIsNotAStripReadsAsNothingJudged;

    procedure Refused(const AJson: string);
    var
        D: TJSONData;
        S: TDifferenceStructure;
    begin
        D := GetJSON(AJson);
        try
            AssertFalse('refused: ' + AJson, DifferenceStructureFromJson(D, S));
        finally
            D.Free;
        end;
        AssertEquals('nothing kept: ' + AJson, 0, Length(S.X));
        AssertEquals('no power: ' + AJson, 0, Length(S.Power));
        AssertTrue('no level: ' + AJson, IsInfinite(S.Level));
    end;

begin
    Refused('[]');
    Refused('{"ok":true}');
    Refused('{"x":[1,2],"power":[1],"difference":[0,0],"spanOf":[0,0],' +
        '"level":3}');
    Refused('{"x":[1],"power":["a"],"difference":[0],"spanOf":[0]}');
    Refused('{"x":[1],"power":[1],"difference":[0],"spanOf":[0,1]}');
end;

initialization
    RegisterTest('unit', TDifferenceStructureTest);
end.
