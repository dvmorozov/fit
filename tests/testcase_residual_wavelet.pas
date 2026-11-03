// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where the difference is not noise: the wavelet transform.)

The series are noise from a fixed sequence, so every verdict is the same on
every run, with a burst of a known period put where the test says. The law
of a sum of independent exponentials of distinct means l_i is the
hypoexponential: P(Q > x) = sum_i prod_(j <> i) l_i / (l_i - l_j) exp(-x / l_i)
(Feller, An Introduction to Probability Theory II, I.13); for one or two terms
it is written out in the tests.
}
unit testcase_residual_wavelet;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, fpcunit, testregistry, residual_randomness,
    residual_wavelet;

type
    TResidualWaveletTest = class(TTestCase)
    private
        function Gauss: double;
        function Noise(AN: longint): TDoubleArray;
        { Noise with a sine of APeriod and amplitude AAmp over ALo..AHi. }
        function NoiseWithBurst(AN, ALo, AHi: longint;
            APeriod, AAmp: double): TDoubleArray;
    protected
        procedure SetUp; override;
    published
        //  The law an octave's average follows: a mean of one, and the
        //  closed form's tail where a sum of exponentials is known.
        procedure TheOctaveLawHasAMeanOfOne;
        procedure ASingleWeightIsTheExponential;
        procedure TwoWeightsMatchTheirClosedForm;
        //  Scaled so noise sits at one: what the heat map's colours say.
        procedure NoiseAveragesOne;
        //  Hundreds of samples tested at once rarely raise a false alarm on
        //  noise (Benjamini-Hochberg).
        procedure NoiseIsNamedNowhere;
        //  Named where the structure is, and only there.
        procedure ABurstIsNamedWhereItIs;
        procedure TheLevelSeparatesWhatIsNamed;
        //  And at which periods: what the map colours, so the map and the
        //  shaded stretches can never disagree.
        procedure ASampleIsNamedExactlyWhereAnOctaveOfItIs;
        procedure TheNamedOctaveIsTheBurstsOwn;
        procedure TheScalogramFindsTheBurstsPeriod;
        //  Outside the cone of influence only.
        procedure NothingIsNamedWhereTheEdgeShapesEveryOctave;
        procedure TheConeIsNarrowAtTheEdgesAndWideInTheMiddle;
        procedure TooShortASeriesIsNotJudged;
        //  A long series is correlated by the Fourier transform, and the
        //  answer is the direct sum's; a short one keeps the direct sum.
        procedure ALongSeriesIsCorrelatedAsTheDirectSumWouldBe;
        procedure AShortSeriesKeepsTheDirectSum;
        procedure ALongSeriesLargeScalesGoThroughTheFourierTransform;
    end;

implementation

procedure TResidualWaveletTest.SetUp;
begin
    RandSeed := 7;
end;

function TResidualWaveletTest.Gauss: double;
var
    U1, U2: double;
begin
    //  Box-Muller over the RTL's Mersenne Twister, seeded so every run draws
    //  the same. NOT the linear congruential pairs the Ljung-Box tests use:
    //  consecutive pairs of that generator are not independent, and this test
    //  is sharp enough to see it - it named two samples of that "noise", and a
    //  model of the same arithmetic over numpy's noise named none.
    U1 := 1 - Random;
    U2 := Random;
    Result := Sqrt(-2 * Ln(U1)) * Cos(2 * Pi * U2);
end;

function TResidualWaveletTest.Noise(AN: longint): TDoubleArray;
var
    i: longint;
begin
    SetLength(Result, AN);
    for i := 0 to AN - 1 do
        Result[i] := Gauss;
end;

function TResidualWaveletTest.NoiseWithBurst(AN, ALo, AHi: longint;
    APeriod, AAmp: double): TDoubleArray;
var
    i: longint;
begin
    Result := Noise(AN);
    for i := ALo to AHi do
        Result[i] := Result[i] + AAmp * Sin(2 * Pi * (i - ALo) / APeriod);
end;

procedure TResidualWaveletTest.TheOctaveLawHasAMeanOfOne;
var
    Lam: TDoubleArray;
    i: longint;
    Sum: double;
begin
    Lam := OctaveLaw;
    AssertEquals('one weight per scale of an octave', Round(1 / WAVELET_DJ),
        Length(Lam));
    Sum := 0;
    for i := 0 to High(Lam) do
    begin
        AssertTrue('a positive weight', Lam[i] > 0);
        Sum := Sum + Lam[i];
    end;
    AssertEquals('the mean noise gives', 1, Sum, 1e-9);
end;

procedure TResidualWaveletTest.ASingleWeightIsTheExponential;
begin
    AssertEquals('exp(-x)', Exp(-2.5), LawSurvival(2.5, [1.0]), 1e-12);
    AssertEquals('nothing below zero', 1, LawSurvival(-1, [1.0]), 0);
end;

procedure TResidualWaveletTest.TwoWeightsMatchTheirClosedForm;
begin
    //  a e^(-x/a) / (a - b) + b e^(-x/b) / (b - a): the hypoexponential
    //  survival of two exponentials of means a and b.
    AssertEquals('a = 0.7, b = 0.3', (0.7 * Exp(-3 / 0.7) - 0.3 * Exp(-3 / 0.3)) / 0.4,
        LawSurvival(3, [0.7, 0.3]), 1e-12);
    AssertEquals('its quantile inverts it', 3, LawQuantile(LawSurvival(3,
        [0.7, 0.3]), [0.7, 0.3]), 1e-9);
end;

procedure TResidualWaveletTest.NoiseAveragesOne;
const
    Series = 8;
var
    G: TWaveletScalogram;
    Means: TDoubleArray;
    i, j, t, Counted: longint;
    All: double;
begin
    //  TOLERANCES FROM THE SPREAD MEASURED on a model of this arithmetic
    //  (numpy, 25 groups of eight series of 2000, 2026-10-13), which matches
    //  this unit to three places on one series: an eight-series mean deviates
    //  0.025 to 0.075 up to a period of 32, and 0.024 averaged over every
    //  period up to 130 - so 0.2 and 0.1 are three to four deviations out.
    Means := nil;
    for t := 1 to Series do
    begin
        G := WaveletScalogram(Noise(2000));
        if Means = nil then
            SetLength(Means, Length(G.Periods));
        for j := 0 to High(G.Periods) do
            for i := 0 to High(G.Power[j]) do
                Means[j] := Means[j] + G.Power[j][i] / (Series * 2000);
    end;
    All := 0;
    Counted := 0;
    for j := 0 to High(Means) do
    begin
        if G.Periods[j] <= 32 then
            AssertEquals(Format('noise averages one at period %.1f',
                [G.Periods[j]]), 1, Means[j], 0.2);
        if G.Periods[j] <= 130 then
        begin
            All := All + Means[j];
            Inc(Counted);
        end;
    end;
    AssertEquals('and over every period up to 130', 1, All / Counted, 0.1);
    AssertEquals('the shortest period is four samples', 4, G.Periods[0], 1e-9);
end;

procedure TResidualWaveletTest.NoiseIsNamedNowhere;
const
    Series = 100;
var
    S: TWaveletStructure;
    Seed, i, Alarms: longint;
    Named: boolean;
begin
    //  WHAT THE RATE PROMISES: a series of pure noise names anything at most
    //  one time in twenty - not never. Asked of five series, one named an
    //  excursion; over a hundred the model of this arithmetic named something
    //  in four. Fixed seeds, so the count is the same on every run.
    Alarms := 0;
    for Seed := 1 to Series do
    begin
        RandSeed := Seed;
        S := WaveletStructure(Noise(1000));
        Named := False;
        for i := 0 to High(S.Named) do
            Named := Named or S.Named[i];
        if Named then
            Inc(Alarms);
    end;
    AssertTrue(Format('noise named something in %d of %d series', [Alarms,
        Series]), Alarms <= Series div 10);
end;

procedure TResidualWaveletTest.ABurstIsNamedWhereItIs;
var
    S: TWaveletStructure;
    i, Inside, Outside: longint;
begin
    //  A wave of 16 samples, as tall as the noise is wide, over 400..480 of
    //  1000: what a pattern the model left out leaves behind.
    S := WaveletStructure(NoiseWithBurst(1000, 400, 480, 16, 1.5));
    Inside := 0;
    Outside := 0;
    for i := 0 to High(S.Named) do
        if S.Named[i] then
            //  The transform reaches about one period beyond a burst.
            if (i >= 400 - 16) and (i <= 480 + 16) then
                Inc(Inside)
            else
                Inc(Outside);
    AssertTrue(Format('most of the burst is named: %d', [Inside]), Inside >= 40);
    AssertEquals('nothing far from it', 0, Outside);
end;

procedure TResidualWaveletTest.TheLevelSeparatesWhatIsNamed;
var
    S: TWaveletStructure;
    i: longint;
begin
    S := WaveletStructure(NoiseWithBurst(1000, 400, 480, 16, 1.5));
    AssertTrue('a level above noise', S.Level > 1);
    for i := 0 to High(S.Power) do
        AssertEquals(Format('sample %d: named exactly when above the level', [i]),
            S.Power[i] >= S.Level, S.Named[i]);
end;

procedure TResidualWaveletTest.ASampleIsNamedExactlyWhereAnOctaveOfItIs;
var
    S: TWaveletStructure;
    i, j: longint;
    Any: boolean;
begin
    S := WaveletStructure(NoiseWithBurst(1000, 400, 480, 16, 1.5));
    AssertEquals('a row per scale', Length(WaveletScalogram(
        NoiseWithBurst(1000, 400, 480, 16, 1.5)).Periods), Length(S.NamedAt));
    for i := 0 to High(S.Named) do
    begin
        Any := False;
        for j := 0 to High(S.NamedAt) do
            Any := Any or S.NamedAt[j][i];
        AssertEquals(Format('sample %d', [i]), S.Named[i], Any);
    end;
end;

procedure TResidualWaveletTest.TheNamedOctaveIsTheBurstsOwn;
var
    S: TWaveletStructure;
    G: TWaveletScalogram;
    j, Count: longint;
begin
    S := WaveletStructure(NoiseWithBurst(1000, 400, 480, 16, 1.5));
    G := WaveletScalogram(NoiseWithBurst(1000, 400, 480, 16, 1.5));
    Count := 0;
    for j := 0 to High(S.NamedAt) do
        if S.NamedAt[j][440] then
        begin
            Inc(Count);
            AssertTrue(Format('a period of %.1f is within an octave of 16',
                [G.Periods[j]]), Abs(Log2(G.Periods[j] / 16)) <= 1);
        end;
    AssertTrue('named at some period', Count > 0);
end;

procedure TResidualWaveletTest.TheScalogramFindsTheBurstsPeriod;
var
    G: TWaveletScalogram;
    j, Best: longint;
begin
    G := WaveletScalogram(NoiseWithBurst(1000, 400, 480, 16, 1.5));
    AssertTrue('some periods', Length(G.Periods) > 5);
    AssertEquals('a row per period', Length(G.Periods), Length(G.Power));
    Best := 0;
    for j := 1 to High(G.Periods) do
        if G.Power[j][440] > G.Power[Best][440] then
            Best := j;
    AssertEquals('strongest at the burst''s period: ' +
        FloatToStr(G.Periods[Best]), 16, G.Periods[Best], 16 * 0.15);
end;

procedure TResidualWaveletTest.NothingIsNamedWhereTheEdgeShapesEveryOctave;
var
    S: TWaveletStructure;
    X: TDoubleArray;
    i: longint;
begin
    //  A strong rhythm of 6 samples over the last 8 of 500. The shortest
    //  octave's cone reaches 9.2 samples in (sqrt 2 times its largest scale,
    //  a period of 6.7), so samples 491 on are inside every cone and judged by
    //  none; those before may be named, their wavelets reaching the rhythm.
    X := Noise(500);
    for i := 492 to 499 do
        X[i] := X[i] + 6 * Sin(2 * Pi * i / 6);
    S := WaveletStructure(X);
    for i := 491 to 499 do
        AssertFalse(Format('sample %d not named', [i]), S.Named[i]);
end;

procedure TResidualWaveletTest.TheConeIsNarrowAtTheEdgesAndWideInTheMiddle;
var
    G: TWaveletScalogram;
    i: longint;
begin
    G := WaveletScalogram(Noise(301));
    AssertEquals('a value per sample', 301, Length(G.Cone));
    AssertEquals('nothing is trusted at the very edge', 0, G.Cone[0], 1e-12);
    AssertEquals('symmetric', G.Cone[20], G.Cone[280], 1e-9);
    for i := 1 to 150 do
        AssertTrue(Format('wider towards the middle at %d', [i]),
            G.Cone[i] >= G.Cone[i - 1]);
end;

procedure TResidualWaveletTest.TooShortASeriesIsNotJudged;
var
    S: TWaveletStructure;
    i: longint;
begin
    S := WaveletStructure(Noise(WAVELET_MIN_SAMPLES - 1));
    for i := 0 to High(S.Named) do
        AssertFalse('nothing named', S.Named[i]);
    AssertEquals('no p below one', 1, MinValue(S.P), 0);
end;

{ The wavelet coefficient at AI, summed term by term as the transform always
  did: the wavelet's conjugate over the samples it reaches, and the weight of
  the part of it that falls on the series. }
procedure DirectCoefficient(const AD: array of double; AScale: double;
    AI: longint; out ARe, AIm, AWeight: double);
var
    Reach, k, n: longint;
    Eta, Env, WRe, WIm: double;
begin
    n := Length(AD);
    Reach := Ceil(WAVELET_CUT * AScale);
    ARe := 0;
    AIm := 0;
    AWeight := 0;
    for k := Max(-Reach, -AI) to Min(Reach, n - 1 - AI) do
    begin
        Eta := k / AScale;
        Env := Power(Pi, -0.25) * Exp(-Sqr(Eta) / 2) / Sqrt(AScale);
        WRe := Env * Cos(MORLET_W0 * Eta);
        WIm := Env * Sin(MORLET_W0 * Eta);
        ARe := ARe + AD[AI + k] * WRe;
        AIm := AIm - AD[AI + k] * WIm;
        AWeight := AWeight + Sqr(WRe) + Sqr(WIm);
    end;
end;

{ THE STRIP UNDER THE MODEL FROZE THE WINDOW. Every frame of a run redraws the
  Difference structure, and the transform summed each coefficient directly -
  N times the sum of the wavelets' widths, which grow with N: about 10^10 steps
  for the S&P 500's 24 816 bars, 16 s on the window's thread for every frame
  (findings.md, "The Difference structure strip froze the window on a long
  series"). Past a size, each scale is a correlation through the Fourier
  transform, and it must be the same correlation: at the edges, where the
  wavelet runs off the series, as in the middle. }
procedure TResidualWaveletTest.ALongSeriesIsCorrelatedAsTheDirectSumWouldBe;
const
    N = 6000;
var
    D: TDoubleArray;
    Re, Im, Weight: TDoubleArray;
    Scale, DRe, DIm, DWeight: double;
    Scales: array of double;
    Cells: array of longint;
    c, j: longint;
begin
    D := Noise(N);
    Scales := [100.0, 600.0, N / (2 * Sqrt(2))];
    for j := 0 to High(Scales) do
    begin
        Scale := Scales[j];
        AssertTrue(Format('scale %g goes through the Fourier transform',
            [Scale]), TransformsByFourier(N, Ceil(WAVELET_CUT * Scale)));
        MorletCorrelation(D, Scale, Re, Im, Weight);
        AssertEquals('a coefficient per sample', N, Length(Re));
        Cells := [0, 1, 17, N div 2, N - 2, N - 1];
        for c in Cells do
        begin
            DirectCoefficient(D, Scale, c, DRe, DIm, DWeight);
            AssertEquals(Format('real part at scale %g, sample %d', [Scale, c]),
                DRe, Re[c], 1e-9 * Max(1, Abs(DRe)));
            AssertEquals(Format('imaginary part at scale %g, sample %d',
                [Scale, c]), DIm, Im[c], 1e-9 * Max(1, Abs(DIm)));
            AssertEquals(Format('weight at scale %g, sample %d', [Scale, c]),
                DWeight, Weight[c], 1e-9 * Max(1, DWeight));
        end;
    end;
end;

{ What a model of ordinary size measures stays exactly what it was: below a
  few million steps a scale is summed directly, as it always was. }
procedure TResidualWaveletTest.AShortSeriesKeepsTheDirectSum;
const
    N = 1000;
var
    D, Re, Im, Weight: TDoubleArray;
    Scale, DRe, DIm, DWeight: double;
begin
    AssertFalse('the largest scale of a thousand samples is summed',
        TransformsByFourier(N, N));
    D := Noise(N);
    Scale := 100;
    MorletCorrelation(D, Scale, Re, Im, Weight);
    DirectCoefficient(D, Scale, 500, DRe, DIm, DWeight);
    AssertEquals('the same sum, exactly', DRe, Re[500], 0);
    AssertEquals('the same sum, exactly', DIm, Im[500], 0);
end;

procedure TResidualWaveletTest.ALongSeriesLargeScalesGoThroughTheFourierTransform;
begin
    AssertTrue('the largest scale of 24 816 bars',
        TransformsByFourier(24816, Ceil(WAVELET_CUT * 24816 / (2 * Sqrt(2)))));
    AssertFalse('its smallest scale, a few samples wide',
        TransformsByFourier(24816, 3));
end;

initialization
    RegisterTest('unit', TResidualWaveletTest);
end.
