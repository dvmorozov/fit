// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Is the difference between the data and the model only noise?)

The golden values are closed forms, computed with Python's math module, so
they need no statistics package to reproduce:
  * chi-square survival for even k: exp(-x/2) * sum over i < k/2 of (x/2)^i / i!
    (Abramowitz & Stegun 26.4.21);
  * for k = 1: erfc(sqrt(x/2)); for k = 3: erfc(sqrt(x/2)) +
    sqrt(2x/pi) exp(-x/2) (A&S 26.4.4, 26.4.5);
  * Ljung-Box Q = n(n+2) * sum over k <= h of rho_k^2 / (n-k) (Ljung & Box 1978), the
    definition statsmodels' acorr_ljungbox implements.
}
unit testcase_residual_randomness;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, Types, fpcunit, testregistry, fpjson, special_functions,
    residual_randomness, sample_coverage;

type
    TChiSquareSurvivalTest = class(TTestCase)
    published
        procedure MatchesTheClosedFormForEvenDegrees;
        procedure MatchesTheErrorFunctionForOneDegree;
        procedure MatchesTheClosedFormForThreeDegrees;
        procedure IsOneAtZeroAndNeverNegativeFarOut;
    end;

    TResidualRandomnessTest = class(TTestCase)
    private
        function Gauss: double;
        { AN samples of white noise of standard deviation ASigma on a level
          of 100, and the model that level. }
        procedure NoiseOnALevel(AN: longint; ASigma: double;
            out AX, AData, AModel: TDoubleArray);
    published
        //  Benjamini & Hochberg: the largest p under its rank's share of q
        //  and every one below it, however many and in whatever order.
        procedure BenjaminiHochbergNamesUpToTheLargestPUnderItsShare;
        procedure ManyPValuesAreJudgedAsByTheDefinition;
        procedure LjungBoxMatchesItsDefinition;
        procedure TooFewSamplesAreNotTested;
        procedure FisherCombinesIndependentTests;
        procedure WhiteNoiseIsRandom;
        procedure AWaveLeftInTheDifferenceIsNotRandom;
        //  Shaded where the structure is, and only there.
        procedure OnlyTheStretchWithStructureIsNamed;
        //  Named where the structure is to within its own period - not the
        //  whole of a fixed window it falls in (the wavelet).
        procedure AShortBurstIsNamedWhereItIs;
        //  Many samples tested at once raise no false alarm on pure noise
        //  (Benjamini-Hochberg).
        procedure ManySamplesOfNoiseNameNothing;
        procedure EachFitIntervalIsTestedOnItsOwn;
        procedure TheVerdictTravelsAndAnOlderReplyReadsAsNotTested;
        //  The reduction keeps a curve whose removal would leave a pattern.
        procedure ARemovalThatLeavesStructureIsRefused;
        procedure ARemovalThatLeavesNoiseIsNot;
        procedure ADifferenceAlreadyNotRandomRefusesNothing;
    end;

implementation

{ ------------------------------------------------------------ chi-square }

procedure TChiSquareSurvivalTest.MatchesTheClosedFormForEvenDegrees;
begin
    AssertEquals('Q 10, k 10', 0.4404932850652124, ChiSquareSurvival(10, 10),
        1e-12);
    AssertEquals('the 5 % point of k 10', 0.05,
        ChiSquareSurvival(18.307038053275146, 10), 1e-12);
    AssertEquals('k 2 is exp(-x/2)', 0.7788007830714049,
        ChiSquareSurvival(0.5, 2), 1e-14);
    AssertEquals('k 4', 0.5578254003710745, ChiSquareSurvival(3, 4), 1e-12);
    AssertEquals('k 20, far in the tail', 0.004995412308307588,
        ChiSquareSurvival(40, 20), 1e-14);
    AssertEquals('k 50, further', 3.454931382984867e-05,
        ChiSquareSurvival(100, 50), 1e-15);
end;

procedure TChiSquareSurvivalTest.MatchesTheErrorFunctionForOneDegree;
begin
    AssertEquals('the 5 % point', 0.05,
        ChiSquareSurvival(3.841458820694124, 1), 1e-12);
    AssertEquals('near zero', 0.7518296340458492, ChiSquareSurvival(0.1, 1),
        1e-12);
    AssertEquals('far out', 5.733031437583873e-07, ChiSquareSurvival(25, 1),
        1e-17);
end;

procedure TChiSquareSurvivalTest.MatchesTheClosedFormForThreeDegrees;
begin
    AssertEquals('the 5 % point', 0.05,
        ChiSquareSurvival(7.814727903251178, 3), 1e-12);
    AssertEquals('x 2', 0.5724067044708798, ChiSquareSurvival(2, 3), 1e-12);
end;

procedure TChiSquareSurvivalTest.IsOneAtZeroAndNeverNegativeFarOut;
begin
    AssertEquals('nothing exceeds zero by chance alone - everything does', 1,
        ChiSquareSurvival(0, 5), 0);
    AssertTrue('far out, a probability still',
        (ChiSquareSurvival(1e4, 10) >= 0) and (ChiSquareSurvival(1e4, 10) < 1e-300));
end;

{ ------------------------------------------------------------ randomness }

function TResidualRandomnessTest.Gauss: double;
var
    U1, U2: double;
begin
    //  Box-Muller over the RTL's Mersenne Twister, seeded so every run draws
    //  the same. It was a linear congruential sequence, whose consecutive
    //  pairs are not independent - the wavelet test that names stretches now
    //  is sharp enough to see that (residual_wavelet's tests).
    U1 := 1 - Random;
    U2 := Random;
    Result := Sqrt(-2 * Ln(U1)) * Cos(2 * Pi * U2);
end;

procedure TResidualRandomnessTest.NoiseOnALevel(AN: longint; ASigma: double;
    out AX, AData, AModel: TDoubleArray);
var
    i: longint;
begin
    SetLength(AX, AN);
    SetLength(AData, AN);
    SetLength(AModel, AN);
    for i := 0 to AN - 1 do
    begin
        AX[i] := i;
        AModel[i] := 100;
        AData[i] := 100 + ASigma * Gauss;
    end;
end;

procedure TResidualRandomnessTest.LjungBoxMatchesItsDefinition;
var
    LB: TLjungBox;
begin
    //  Fifteen samples: three lags (a fifth of them), Q by the definition.
    LB := LjungBox([0.3, -1.2, 0.8, 1.5, -0.4, -0.9, 0.2, 1.1, -1.3, 0.6, 0.05,
        -0.7, 1.4, -0.2, -1.0]);
    AssertTrue('tested', LB.Valid);
    AssertEquals('a fifth of the samples as lags', 3, LB.Lags);
    AssertEquals('Q', 7.896502347380791, LB.Q, 1e-12);
    AssertEquals('p, chi-square with three degrees', 0.04819982521605977,
        LB.P, 1e-12);
end;

procedure TResidualRandomnessTest.TooFewSamplesAreNotTested;
begin
    AssertFalse('nine samples give one lag; too few to say anything',
        LjungBox([1, -1, 2, -2, 1, 0, -1, 2, -2]).Valid);
end;

procedure TResidualRandomnessTest.FisherCombinesIndependentTests;
begin
    //  -2 (ln 0.1 + ln 0.2) on four degrees: 0.02 (1 - ln 0.02).
    AssertEquals(0.02 * (1 - Ln(0.02)), FisherCombined([0.1, 0.2]), 1e-12);
end;

procedure TResidualRandomnessTest.WhiteNoiseIsRandom;
var
    X, D, M: TDoubleArray;
    R: TResidualRandomness;
begin
    RandSeed := 11;
    NoiseOnALevel(600, 2, X, D, M);
    R := JudgeResiduals(X, D, M, [SampleSpan(0, 599)]);
    AssertTrue('tested', R.Tested);
    AssertTrue(Format('random: p = %g', [R.P]), R.P >= RANDOMNESS_SIGNIFICANCE);
    AssertEquals('no stretch named', 0, Length(R.NonRandomRanges));
    AssertTrue('so it reads as random', ResidualIsRandom(R));
end;

procedure TResidualRandomnessTest.AWaveLeftInTheDifferenceIsNotRandom;
var
    X, D, M: TDoubleArray;
    R: TResidualRandomness;
    i: longint;
begin
    RandSeed := 12;
    NoiseOnALevel(600, 2, X, D, M);
    for i := 0 to 599 do
        D[i] := D[i] + 3 * Sin(2 * Pi * i / 80);
    R := JudgeResiduals(X, D, M, [SampleSpan(0, 599)]);
    AssertTrue(Format('not random: p = %g', [R.P]), R.P < 1e-6);
    AssertFalse(ResidualIsRandom(R));
end;

procedure TResidualRandomnessTest.OnlyTheStretchWithStructureIsNamed;
var
    X, D, M: TDoubleArray;
    R: TResidualRandomness;
    i, Inside, Outside, In_: longint;
begin
    //  WHAT BENJAMINI-HOCHBERG PROMISES: at most 5 % of what it names is
    //  noise - not that nothing of noise is ever named. With a wave of 250
    //  samples named, a few samples of noise elsewhere pass too (five, at
    //  270..274, on this series).
    RandSeed := 13;
    NoiseOnALevel(1000, 2, X, D, M);
    for i := 500 to 749 do
        D[i] := D[i] + 4 * Sin(2 * Pi * i / 60);
    R := JudgeResiduals(X, D, M, [SampleSpan(0, 999)]);
    Inside := 0;
    Outside := 0;
    //  Sample by sample (x is the sample here). Within two and a half of
    //  its periods counts as the wave's: the longer octaves register it too,
    //  and reach as far as their windows do (measured: 2 to 2.5 periods).
    for i := 0 to High(R.NonRandomRanges) do
    begin
        In_ := Max(0, Min(Round(R.NonRandomRanges[i].ToX), 749 + 150) -
            Max(Round(R.NonRandomRanges[i].FromX), 500 - 150) + 1);
        Inc(Inside, In_);
        Inc(Outside, R.NonRandomRanges[i].Count - In_);
    end;
    AssertTrue(Format('the wave is named: %d samples', [Inside]), Inside >= 150);
    //  The rate is an average over many series; one may exceed it.
    AssertTrue(Format('nine in ten of what is named is the wave''s: %d of %d away',
        [Outside, Inside + Outside]), Outside <= 0.1 * (Inside + Outside));
end;

procedure TResidualRandomnessTest.AShortBurstIsNamedWhereItIs;
var
    X, D, M: TDoubleArray;
    R: TResidualRandomness;
    i: longint;
begin
    //  A rhythm of 10 samples over 515..545 of 1000: inside one window of 50
    //  (500..549), which the window test named whole.
    RandSeed := 16;
    NoiseOnALevel(1000, 2, X, D, M);
    for i := 515 to 545 do
        D[i] := D[i] + 6 * Sin(2 * Pi * (i - 515) / 10);
    R := JudgeResiduals(X, D, M, [SampleSpan(0, 999)]);
    AssertTrue('a stretch is named', Length(R.NonRandomRanges) > 0);
    for i := 0 to High(R.NonRandomRanges) do
        AssertTrue(Format('%g..%g lies within a period of the burst',
            [R.NonRandomRanges[i].FromX, R.NonRandomRanges[i].ToX]),
            (R.NonRandomRanges[i].FromX >= 515 - 10) and
            (R.NonRandomRanges[i].ToX <= 545 + 10));
end;

procedure TResidualRandomnessTest.ManySamplesOfNoiseNameNothing;
var
    X, D, M: TDoubleArray;
    R: TResidualRandomness;
begin
    RandSeed := 14;
    NoiseOnALevel(5000, 2, X, D, M);
    R := JudgeResiduals(X, D, M, [SampleSpan(0, 4999)]);
    AssertEquals('every sample judged', 5000, R.Samples);
    AssertEquals('none named', 0, Length(R.NonRandomRanges));
end;

procedure TResidualRandomnessTest.EachFitIntervalIsTestedOnItsOwn;
var
    X, D, M: TDoubleArray;
    R: TResidualRandomness;
    i, First, Second: longint;
begin
    //  The second interval holds a wave; the first is noise. Each is
    //  transformed on its own, so no stretch crosses the gap between them.
    RandSeed := 15;
    NoiseOnALevel(400, 2, X, D, M);
    for i := 200 to 399 do
        D[i] := D[i] + 4 * Sin(2 * Pi * i / 40);
    R := JudgeResiduals(X, D, M, [SampleSpan(0, 199), SampleSpan(200, 399)]);
    AssertEquals('every sample of both', 400, R.Samples);
    First := 0;
    Second := 0;
    for i := 0 to High(R.NonRandomRanges) do
    begin
        AssertTrue(Format('%g..%g does not cross the gap',
            [R.NonRandomRanges[i].FromX, R.NonRandomRanges[i].ToX]),
            (R.NonRandomRanges[i].ToX < 200) or
            (R.NonRandomRanges[i].FromX >= 200));
        if R.NonRandomRanges[i].FromX >= 200 then
            Inc(Second, R.NonRandomRanges[i].Count)
        else
            Inc(First, R.NonRandomRanges[i].Count);
    end;
    //  Not all of it: near an interval's ends the longer octaves are inside
    //  their cone and judge nothing (57 of its 200 samples named here).
    AssertTrue(Format('the wave is named: %d', [Second]), Second >= 40);
    //  NOT A RATE HERE: with few samples judged and many of them the wave's,
    //  Benjamini-Hochberg's cut is loose, and on this series 11 of 68 named
    //  samples were the noise interval's. The rate is asserted where it can
    //  be, over a hundred series of noise (residual_wavelet's tests); this
    //  test is about the gap.
    AssertTrue(Format('more of the wave than of the noise: %d against %d',
        [Second, First]), Second > First);
end;

procedure TResidualRandomnessTest.TheVerdictTravelsAndAnOlderReplyReadsAsNotTested;
var
    R, Back: TResidualRandomness;
    O: TJSONObject;
begin
    R := Default(TResidualRandomness);
    R.Tested := True;
    R.P := 0.0123;
    R.Samples := 7;
    SetLength(R.NonRandomRanges, 1);
    R.NonRandomRanges[0].FromX := 10;
    R.NonRandomRanges[0].ToX := 60;
    R.NonRandomRanges[0].Count := 51;
    O := TJSONObject.Create;
    try
        AddRandomnessJson(O, R);
        ReadRandomnessJson(O, Back);
        AssertTrue('tested', Back.Tested);
        AssertEquals('p', 0.0123, Back.P, 0);
        AssertEquals('samples', 7, Back.Samples);
        AssertEquals('the stretch', 60, Back.NonRandomRanges[0].ToX, 0);
    finally
        O.Free;
    end;
    O := TJSONObject.Create;
    try
        ReadRandomnessJson(O, Back);
        AssertFalse('a reply from before: not tested', Back.Tested);
    finally
        O.Free;
    end;
end;

procedure TResidualRandomnessTest.ARemovalThatLeavesStructureIsRefused;
var
    X, D, M: TDoubleArray;
    With_, Without: TDoubleArray;
    i, Seed: longint;
begin
    //  With the curve the difference is noise; without it a bump is left.
    //  THE PREMISE FIRST: noise reads as noise in nineteen series of twenty,
    //  so the first seed from 21 whose difference does is taken - the rule is
    //  about that case, not about the twentieth.
    Seed := 21;
    repeat
        RandSeed := Seed;
        NoiseOnALevel(300, 1, X, D, M);
        With_ := StandardisedResiduals(D, M);
        Inc(Seed);
    until LjungBox(With_).P >= RANDOMNESS_SIGNIFICANCE;
    SetLength(Without, 300);
    for i := 0 to 299 do
        Without[i] := D[i] - M[i] + 3 * Exp(-Sqr((i - 150) / 20));
    AssertTrue(RemovalBreaksRandomness(With_, Without));
end;

procedure TResidualRandomnessTest.ARemovalThatLeavesNoiseIsNot;
var
    X, D, M: TDoubleArray;
    Without: TDoubleArray;
    i: longint;
begin
    RandSeed := 22;
    NoiseOnALevel(300, 1, X, D, M);
    SetLength(Without, 300);
    for i := 0 to 299 do
        Without[i] := (D[i] - M[i]) * 1.01;
    AssertFalse(RemovalBreaksRandomness(StandardisedResiduals(D, M),
        Without));
end;

procedure TResidualRandomnessTest.ADifferenceAlreadyNotRandomRefusesNothing;
var
    With_, Without: TDoubleArray;
    i: longint;
begin
    //  BIC decides there, as before: the curve is not what made it random.
    SetLength(With_, 300);
    SetLength(Without, 300);
    for i := 0 to 299 do
    begin
        With_[i] := 3 * Sin(2 * Pi * i / 40);
        Without[i] := With_[i] + Exp(-Sqr((i - 150) / 20));
    end;
    AssertFalse(RemovalBreaksRandomness(With_, Without));
end;

procedure TResidualRandomnessTest.BenjaminiHochbergNamesUpToTheLargestPUnderItsShare;
var
    Named: TBooleanDynArray;
begin
    //  Sorted: 0.001 0.008 0.039 0.041 0.042 0.06 0.074 0.205; q 0.05, m 8:
    //  shares 0.00625 0.0125 0.01875 0.025 0.03125 0.0375 0.04375 0.05. The
    //  largest under its share is 0.008, the second - so the two smallest.
    Named := BenjaminiHochberg([0.041, 0.001, 0.205, 0.008, 0.042, 0.039,
        0.06, 0.074], 0.05);
    AssertFalse('0.041', Named[0]);
    AssertTrue('0.001', Named[1]);
    AssertFalse('0.205', Named[2]);
    AssertTrue('0.008', Named[3]);
    AssertFalse('0.042', Named[4]);
    AssertFalse('0.039', Named[5]);
end;

{ THE WAVELET JUDGES EVERY (SAMPLE, OCTAVE) AT ONCE - 300 000 p-values for a
  series of 24 816 bars - and the procedure sorted them by insertion, a
  quarter of a minute (findings.md, "The Difference structure strip froze the
  window on a long series"). However they are sorted, the cut is the
  definition's: the largest p(k) <= k q / m. }
procedure TResidualRandomnessTest.ManyPValuesAreJudgedAsByTheDefinition;
const
    M = 5000;
    Q = 0.05;
var
    P: TDoubleArray;
    Named: TBooleanDynArray;
    i, j, Rank: longint;
    Cut: double;
begin
    SetLength(P, M);
    for i := 0 to M - 1 do
        //  Small values among large ones, in no order.
        P[i] := Abs(Frac(Sin(i * 12.9898) * 43758.5453)) *
            IfThen(i mod 7 = 0, 0.001, 1);
    Cut := -1;
    for i := 0 to M - 1 do
    begin
        Rank := 0;
        for j := 0 to M - 1 do
            if P[j] <= P[i] then
                Inc(Rank);
        if (P[i] <= Rank / M * Q) and (P[i] > Cut) then
            Cut := P[i];
    end;
    AssertTrue('the series names something', Cut > 0);
    Named := BenjaminiHochberg(P, Q);
    for i := 0 to M - 1 do
        AssertEquals(Format('p %g', [P[i]]), P[i] <= Cut, Named[i]);
end;

initialization
    RegisterTest('unit', TChiSquareSurvivalTest);
    RegisterTest('unit', TResidualRandomnessTest);
end.
