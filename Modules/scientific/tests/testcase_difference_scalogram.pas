// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The heat map the Residual Spectrum tab draws: where, and at which
rhythm, the difference holds more than noise.)
}
unit testcase_difference_scalogram;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, fpcunit, testregistry, fpjson, residual_randomness,
    residual_wavelet, fit_window, app_theme, difference_scalogram,
    fit_rest_api, scientific_module, scientific_app_module, gauss_points_set;

type
    TDifferenceScalogramTest = class(TTestCase)
    private
        X, Data, Fit, Noise: TDoubleArray;
        { 600 samples of noise of variance 4 the model holds, with a rhythm of
          16 samples over 200..280 it has nothing for. Noise is that variance
          at every sample, as a noise rule would give it. }
        procedure RhythmTheModelMisses;
    published
        procedure EverySampleOfTheIntervalsHasAColumn;
        procedure AShorterIntervalHasNoRowItCannotHold;
        procedure TheRhythmIsStrongestWhereAndAtThePeriodItIs;
        procedure NothingIsReadOutsideTheIntervals;
        //  Measured against the noise of the model's rule, and nothing
        //  without one.
        procedure ThePowerIsOverTheNoiseTheWindowCarries;
        procedure WithNoNoiseThereIsNoMap;
        //  WHAT A CELL SAYS (the user, 2026-10-14: "totally unclear what it
        //  means"): the stretches' colour exactly where the stretches are, and
        //  the ends of an interval and the gaps between them not judged.
        procedure TheMissedCellsAreExactlyTheShadedStretches;
        procedure TheRhythmIsMissedWhereAndAtThePeriodItIs;
        procedure NothingIsJudgedOutsideTheIntervalsOrAtTheirEnds;
        //  Its colours and the key that names them.
        procedure TheColoursKeepTheirMeaning;
        procedure TheKeyNamesEveryKindOnceInOrder;
        //  The period axis is log2 of the period, its marks whole octaves
        //  read as periods.
        procedure ThePeriodAxisMarksOctavesAndReadsThemAsPeriods;
        //  THE GRID IS THE SERVER'S: what crosses the wire is the grid as it
        //  was computed, cell for cell, and the view draws that.
        procedure TheWireCarriesTheGridAsItWasComputed;
        procedure TheResourceIsTheGridOverTheWindowItIsHanded;
        procedure AReplyThatIsNotAGridDrawsNothing;
    end;

    { THE RESIDUAL SPECTRUM AS THE WINDOW ASKS FOR IT: the resource the view
      names, through the module route, over a fitted model. Integration: it
      runs the optimiser. }
    TScalogramResourceRestTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        function Call(const M, P, B: string; out Code: longint): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheMapCoversOnlyTheFitIntervalsTheServerHolds;
        procedure BeforeAnyFitThereIsNoMap;
    end;

implementation

procedure TDifferenceScalogramTest.RhythmTheModelMisses;
var
    i: longint;
begin
    RandSeed := 41;
    SetLength(X, 600);
    SetLength(Data, 600);
    SetLength(Fit, 600);
    SetLength(Noise, 600);
    for i := 0 to 599 do
    begin
        X[i] := i;
        Noise[i] := 4;
        Fit[i] := 100;
        Data[i] := 100 + 2 * Sqrt(-2 * Ln(1 - Random)) * Cos(2 * Pi * Random);
        if (i >= 200) and (i <= 280) then
            Data[i] := Data[i] + 6 * Sin(2 * Pi * i / 16);
    end;
end;

procedure TDifferenceScalogramTest.EverySampleOfTheIntervalsHasAColumn;
var
    G: TScalogramGrid;
    j: longint;
begin
    RhythmTheModelMisses;
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 399, 450, 599]));
    AssertEquals('a column per sample of both intervals', 550, Length(G.X));
    AssertTrue('some periods', Length(G.Periods) > 4);
    for j := 0 to High(G.Periods) do
        AssertEquals('a value per column', 550, Length(G.Power[j]));
    AssertEquals('a cone value per column', 550, Length(G.Cone));
    AssertEquals('the cells'' level', Ln(20), G.Level, 1e-9);
end;

procedure TDifferenceScalogramTest.AShorterIntervalHasNoRowItCannotHold;
var
    G: TScalogramGrid;
    Top: longint;
begin
    RhythmTheModelMisses;
    //  400 samples and 150: the longest periods are the longer one's alone.
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 399, 450, 599]));
    Top := High(G.Periods);
    AssertFalse('the longer interval has it', IsNan(G.Power[Top][100]));
    AssertTrue('the shorter one does not', IsNan(G.Power[Top][500]));
end;

procedure TDifferenceScalogramTest.TheRhythmIsStrongestWhereAndAtThePeriodItIs;
var
    G: TScalogramGrid;
begin
    RhythmTheModelMisses;
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 599]));
    AssertTrue('above the level at its place and period',
        ScalogramValueAt(G, 240, 16) >= G.Level);
    AssertTrue('far stronger there than at the same period elsewhere',
        ScalogramValueAt(G, 240, 16) > 5 * ScalogramValueAt(G, 450, 16));
    AssertTrue('and than at another period there',
        ScalogramValueAt(G, 240, 16) > 5 * ScalogramValueAt(G, 240, 128));
end;

procedure TDifferenceScalogramTest.NothingIsReadOutsideTheIntervals;
var
    G: TScalogramGrid;
begin
    RhythmTheModelMisses;
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 399, 450, 599]));
    AssertTrue('in the gap between them', IsNan(ScalogramValueAt(G, 420, 16)));
    AssertTrue('before the first', IsNan(ScalogramValueAt(G, -5, 16)));
    AssertTrue('after the last', IsNan(ScalogramValueAt(G, 700, 16)));
    AssertTrue('below the shortest period',
        IsNan(ScalogramValueAt(G, 100, 1)));
end;

{ Four times the noise, a quarter of the power over it, in every cell: the map
  is measured against the noise the window carries - the model's rule's - and
  not against a noise of its own. }
procedure TDifferenceScalogramTest.ThePowerIsOverTheNoiseTheWindowCarries;
var
    AtFour, AtSixteen: TScalogramGrid;
    Loud: TDoubleArray;
    i, j: longint;
begin
    RhythmTheModelMisses;
    AtFour := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 599]));
    SetLength(Loud, Length(Noise));
    for i := 0 to High(Loud) do
        Loud[i] := 4 * Noise[i];
    AtSixteen := ScalogramGridOf(FitWindowOf(X, Data, Fit, Loud, [0, 599]));
    AssertEquals('the same rows', Length(AtFour.Power), Length(AtSixteen.Power));
    AssertTrue('a map to compare', Length(AtFour.Power) > 0);
    for j := 0 to High(AtFour.Power) do
        for i := 0 to High(AtFour.Power[j]) do
            if IsNan(AtFour.Power[j][i]) then
                AssertTrue('not held in either', IsNan(AtSixteen.Power[j][i]))
            else
                AssertEquals(Format('row %d, column %d', [j, i]),
                    AtFour.Power[j][i] / 4, AtSixteen.Power[j][i],
                    1e-9 * Max(1, AtFour.Power[j][i]));
end;

procedure TDifferenceScalogramTest.WithNoNoiseThereIsNoMap;
var
    G: TScalogramGrid;
begin
    RhythmTheModelMisses;
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, [0, 599]));
    AssertEquals('no column', 0, Length(G.X));
    AssertEquals('no row', 0, Length(G.Periods));
end;

procedure TDifferenceScalogramTest.TheMissedCellsAreExactlyTheShadedStretches;
var
    W: TFitWindow;
    G: TScalogramGrid;
    S: TWaveletStructure;
    i, j, Named: longint;
    Missed: boolean;
begin
    RhythmTheModelMisses;
    W := FitWindowOf(X, Data, Fit, Noise, [0, 399, 450, 599]);
    G := ScalogramGridOf(W);
    //  The judgement the shaded stretches are drawn from.
    S := WaveletStructureOver(StandardisedResiduals(W.Data, W.Fit, W.Noise), W.Spans);
    Named := 0;
    for i := 0 to High(G.X) do
    begin
        Missed := False;
        for j := 0 to High(G.Periods) do
            Missed := Missed or
                (ScalogramCellAt(G, G.X[i], G.Periods[j]) = scMissed);
        AssertEquals(Format('x %g', [G.X[i]]), S.Named[i], Missed);
        if Missed then
            Inc(Named);
    end;
    AssertTrue('something to see', Named > 0);
end;

procedure TDifferenceScalogramTest.TheRhythmIsMissedWhereAndAtThePeriodItIs;
var
    G: TScalogramGrid;
begin
    RhythmTheModelMisses;
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 599]));
    AssertTrue('at its place and period',
        ScalogramCellAt(G, 240, 16) = scMissed);
    AssertTrue('not elsewhere', ScalogramCellAt(G, 450, 16) <> scMissed);
    AssertTrue('nor at a far other period',
        ScalogramCellAt(G, 240, 128) <> scMissed);
end;

procedure TDifferenceScalogramTest.NothingIsJudgedOutsideTheIntervalsOrAtTheirEnds;
var
    G: TScalogramGrid;
begin
    RhythmTheModelMisses;
    G := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 399, 450, 599]));
    AssertTrue('in the gap', ScalogramCellAt(G, 420, 16) = scNotJudged);
    AssertTrue('before the first', ScalogramCellAt(G, -5, 16) = scNotJudged);
    AssertTrue('after the last', ScalogramCellAt(G, 700, 16) = scNotJudged);
    AssertTrue('a period not looked at',
        ScalogramCellAt(G, 100, 1) = scNotJudged);
    //  Two samples from an interval's start a long wave is all edge.
    AssertTrue('inside the cone', ScalogramCellAt(G, 2, 64) = scNotJudged);
    AssertTrue('but judged in the middle',
        ScalogramCellAt(G, 100, 16) <> scNotJudged);
end;

procedure TDifferenceScalogramTest.TheColoursKeepTheirMeaning;
var
    Dark: boolean;
    P: TThemePalette;
    a, b: TScalogramCell;
begin
    for Dark := False to True do
    begin
        P := PaletteFor(Dark);
        AssertEquals(P.Name + ': the stretches'' colour', P.NonRandom,
            ScalogramColorOf(P, scMissed));
        AssertEquals(P.Name + ': noise is the background', P.Background,
            ScalogramColorOf(P, scNoise));
        AssertEquals(P.Name + ': not judged as a disabled cell', P.DisabledCell,
            ScalogramColorOf(P, scNotJudged));
        for a := Low(TScalogramCell) to High(TScalogramCell) do
            for b := Succ(a) to High(TScalogramCell) do
                if b > a then
                    AssertTrue(Format('%s: %d and %d told apart',
                        [P.Name, Ord(a), Ord(b)]),
                        ScalogramColorOf(P, a) <> ScalogramColorOf(P, b));
    end;
end;

procedure TDifferenceScalogramTest.TheKeyNamesEveryKindOnceInOrder;
var
    P: TThemePalette;
    K: TScalogramKey;
    i: longint;
begin
    P := PaletteFor(True);
    K := ScalogramKey(P);
    AssertEquals('every kind', Ord(High(TScalogramCell)) + 1, Length(K));
    for i := 0 to High(K) do
    begin
        AssertTrue('in order', Ord(K[i].Cell) = i);
        AssertTrue('worded', K[i].Text <> '');
        AssertEquals('in its colour', ScalogramColorOf(P, K[i].Cell), K[i].Color);
    end;
end;

procedure TDifferenceScalogramTest.ThePeriodAxisMarksOctavesAndReadsThemAsPeriods;
var
    Start, Step: double;
begin
    ScalogramPeriodMarks(1.7, 7.2, Start, Step);
    AssertEquals('from the first whole octave', 2, Start, 0);
    AssertEquals('an octave apart', 1, Step, 0);
    AssertEquals('read as the period', '16', ScalogramPeriodMarkText(4));
    AssertEquals('the shortest', '4', ScalogramPeriodMarkText(2));
    AssertEquals('a position is a period', 16, ScalogramPeriodAt(4), 1e-12);
end;

{ Two grids, cell for cell; NaN equal to NaN. }
procedure AssertSameGrid(const AExpected, AActual: TScalogramGrid);
var
    i, j: longint;
begin
    TAssert.AssertEquals('columns', Length(AExpected.X), Length(AActual.X));
    TAssert.AssertEquals('rows', Length(AExpected.Periods),
        Length(AActual.Periods));
    TAssert.AssertEquals('level', AExpected.Level, AActual.Level, 0);
    for i := 0 to High(AExpected.X) do
    begin
        TAssert.AssertEquals('x', AExpected.X[i], AActual.X[i], 0);
        TAssert.AssertEquals('interval', AExpected.SpanOf[i], AActual.SpanOf[i]);
        TAssert.AssertEquals('cone', AExpected.Cone[i], AActual.Cone[i], 0);
    end;
    for j := 0 to High(AExpected.Periods) do
    begin
        TAssert.AssertEquals('period', AExpected.Periods[j], AActual.Periods[j], 0);
        for i := 0 to High(AExpected.X) do
        begin
            if IsNan(AExpected.Power[j][i]) then
                TAssert.AssertTrue(Format('not held at row %d column %d',
                    [j, i]), IsNan(AActual.Power[j][i]))
            else
                TAssert.AssertEquals('power', AExpected.Power[j][i],
                    AActual.Power[j][i], 0);
            TAssert.AssertEquals('named', AExpected.Named[j][i],
                AActual.Named[j][i]);
        end;
    end;
end;

procedure TDifferenceScalogramTest.TheWireCarriesTheGridAsItWasComputed;
var
    Sent, Read: TScalogramGrid;
    O: TJSONObject;
    Json: string;
begin
    RhythmTheModelMisses;
    //  Two intervals of different lengths, so a row the shorter one cannot
    //  hold travels as null and comes back as NaN.
    Sent := ScalogramGridOf(FitWindowOf(X, Data, Fit, Noise, [0, 399, 450, 599]));
    AssertTrue('a grid to send', Length(Sent.X) > 0);
    O := ScalogramGridToJson(Sent);
    try
        Json := O.AsJSON;
    finally
        O.Free;
    end;
    AssertTrue('read back', ScalogramGridFromJson(Json, Read));
    AssertSameGrid(Sent, Read);
end;

procedure TDifferenceScalogramTest.TheResourceIsTheGridOverTheWindowItIsHanded;
var
    W: TFitWindow;
    Read: TScalogramGrid;
begin
    RhythmTheModelMisses;
    W := FitWindowOf(X, Data, Fit, Noise, [100, 399]);
    AssertTrue('read back', ScalogramGridFromJson(ScalogramOverWindow(W), Read));
    AssertSameGrid(ScalogramGridOf(W), Read);
    //  An empty window - nothing computed yet - is an empty map, still a grid.
    AssertTrue('an empty one reads',
        ScalogramGridFromJson(ScalogramOverWindow(Default(TFitWindow)), Read));
    AssertEquals('of nothing', 0, Length(Read.X));
end;

procedure TDifferenceScalogramTest.AReplyThatIsNotAGridDrawsNothing;

    procedure Refused(const AJson: string);
    var
        G: TScalogramGrid;
    begin
        AssertFalse('refused: ' + AJson, ScalogramGridFromJson(AJson, G));
        AssertEquals('nothing kept: ' + AJson, 0, Length(G.X));
        AssertEquals('no rows: ' + AJson, 0, Length(G.Power));
    end;

begin
    Refused('');
    Refused('not json');
    Refused('[]');
    Refused('{"ok":true}');
    //  A row of another length than x.
    Refused('{"x":[1,2],"spanOf":[0,0],"periods":[2],"cone":[1,1],' +
        '"power":[[1]],"named":[[false,false]],"level":3}');
    //  A cell that is not a flag.
    Refused('{"x":[1],"spanOf":[0],"periods":[2],"cone":[1],' +
        '"power":[[1]],"named":[[1]],"level":3}');
    //  No level.
    Refused('{"x":[1],"spanOf":[0],"periods":[2],"cone":[1],' +
        '"power":[[1]],"named":[[true]]}');
end;

{ ---- TScalogramResourceRestTest ---- }

const
    RestSamples = 200;

procedure TScalogramResourceRestTest.SetUp;
begin
    RegisterScientificModule;
    FApi := TFitRestApi.Create;
end;

procedure TScalogramResourceRestTest.TearDown;
begin
    FreeAndNil(FApi);
end;

function TScalogramResourceRestTest.Call(const M, P, B: string;
    out Code: longint): string;
begin
    FApi.Handle(M, P, B, Code, Result);
end;

function ProblemIdOf(const ABody: string): longint;
var
    D: TJSONData;
begin
    D := GetJSON(ABody);
    try
        Result := TJSONObject(D).Get('id', -1);
    finally
        D.Free;
    end;
end;

{ A peak with a ripple of period 10 on it, as a points body. }
function RippleBody: string;
var
    i: longint;
    Xs, Ys: string;
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Xs := '';
    Ys := '';
    for i := 0 to RestSamples - 1 do
    begin
        if i > 0 then
        begin
            Xs := Xs + ',';
            Ys := Ys + ',';
        end;
        Xs := Xs + IntToStr(i);
        Ys := Ys + FloatToStr(100 * Exp(-Sqr(i - 100) / (2 * Sqr(15))) +
            3 * Sin(2 * Pi * i / 10) + 20, FS);
    end;
    Result := Format('{"x":[%s],"y":[%s]}', [Xs, Ys]);
end;

procedure TScalogramResourceRestTest.TheMapCoversOnlyTheFitIntervalsTheServerHolds;
var
    Id, Code, i: longint;
    Body: string;
    G: TScalogramGrid;
begin
    Body := Call('POST', '/problems', '', Code);
    Id := ProblemIdOf(Body);
    Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"curveType":"%s","curveScaling":false}',
        [GUIDToString(TGaussPointsSet.GetCurveTypeId)]), Code);
    AssertEquals('the type', 200, Code);
    Call('PUT', Format('/problems/%d/profile', [Id]), RippleBody, Code);
    AssertEquals('the profile', 200, Code);
    Call('PUT', Format('/problems/%d/rfactor-bounds', [Id]),
        '{"x":[10,80,90,190],"y":[0,0,0,0]}', Code);
    AssertEquals('the intervals', 200, Code);
    Call('PUT', Format('/problems/%d/positions', [Id]),
        '{"x":[100],"y":[120]}', Code);
    AssertEquals('the pick', 200, Code);
    Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]), '',
        Code);
    AssertEquals('fitted', 200, Code);

    Body := Call('GET', Format('/problems/%d/modules/%s', [Id,
        ScalogramResource]), '', Code);
    AssertEquals('the map answers: ' + Body, 200, Code);
    AssertTrue('a map: ' + Copy(Body, 1, 200), ScalogramGridFromJson(Body, G));
    AssertEquals('a column per sample of both intervals',
        (80 - 10 + 1) + (190 - 90 + 1), Length(G.X));
    for i := 0 to High(G.X) do
    begin
        AssertTrue(Format('column %g inside an interval', [G.X[i]]),
            ((G.X[i] >= 10) and (G.X[i] <= 80)) or
            ((G.X[i] >= 90) and (G.X[i] <= 190)));
        AssertEquals('its interval', Ord(G.X[i] >= 90), G.SpanOf[i]);
    end;
    AssertTrue('some periods', Length(G.Periods) > 0);
end;

procedure TScalogramResourceRestTest.BeforeAnyFitThereIsNoMap;
var
    Id, Code: longint;
    Body: string;
    G: TScalogramGrid;
begin
    Body := Call('POST', '/problems', '', Code);
    Id := ProblemIdOf(Body);
    Body := Call('GET', Format('/problems/%d/modules/%s', [Id,
        ScalogramResource]), '', Code);
    AssertEquals('answered: ' + Body, 200, Code);
    AssertTrue('a map: ' + Body, ScalogramGridFromJson(Body, G));
    AssertEquals('of nothing', 0, Length(G.X));
end;

initialization
    RegisterTest('unit', TDifferenceScalogramTest);
    RegisterTest('integration', TScalogramResourceRestTest);
end.
