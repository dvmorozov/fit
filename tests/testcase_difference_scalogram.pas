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
    SysUtils, Math, fpcunit, testregistry, residual_randomness, residual_wavelet,
    fit_window, app_theme, difference_scalogram;

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

initialization
    RegisterTest('unit', TDifferenceScalogramTest);
end.
