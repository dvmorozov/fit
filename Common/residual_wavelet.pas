// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where the difference is not noise: the wavelet transform.)

WHY. A test of the whole difference says whether the model left something,
not where; the user asked to SEE where the model falls short. A Fourier
transform over small windows is that question, and its standard form is the
continuous wavelet transform (Torrence & Compo 1998, A Practical Guide to
Wavelet Analysis, Bulletin of the American Meteorological Society 79:61-78),
whose window grows with the period looked at and whose power has a known
distribution when the series is noise. Canonical, and every number below is
theirs unless it says otherwise.

THE TRANSFORM. The Morlet wavelet, w0 = 6 (their Table 1), by direct
convolution in samples - their eq. 2 with dt = 1 - cut where its envelope
falls below exp(-8). Scales s0 2^(j dj), s0 the scale of a 4-sample period
(see WAVELET_SHORTEST_PERIOD for why not their eq. 9's 2 samples), dj = 1/4, up to the largest scale whose cone of influence still
leaves the middle sample clear (N / (2 sqrt 2)) - a larger one is all edge.
The period of a scale is 1.033 s (their Table 1).

OVER THE NOISE, AT EACH SAMPLE. The input is the standardised difference
(residual_randomness.StandardisedResiduals), so noise has unit variance. Each
coefficient's squared modulus is divided by what noise would give at that
sample and scale: the sum of the wavelet's squared weights that fall on the
series. Away from the edges that sum is one (their eq. 3 normalisation); at an
edge fewer weights fall on the series, so dividing by it, rather than padding
with zeros and losing power, keeps an edge as honest as the middle. This
software's choice; the cone of influence is still reported, because an edge
coefficient rests on fewer samples - and NOTHING INSIDE THE CONE IS NAMED OR
MARKED (theirs: interpret it with caution): there an octave's wavelets are cut
by the edge, stop averaging to zero, and answer to what is not a rhythm. A wave
running to the series' last sample was named by every long octave from 130
samples before it began, until they were held to the cone (measured on the
model, 2026-10-13).

THE SCALOGRAM'S TEST (their eq. 18 with a white background): power over noise
is chi-square of 2 degrees over 2 - an exponential of mean one - so a cell
passes at 95 % above ln 20 = 3.00 (measured on noise: 4 to 5 % of cells pass
from a period of four samples up). Cell by cell, as they test it: a picture to read, not a list to act on.

WHERE, ONE NUMBER A SAMPLE (their section 5a). The power averaged over each
octave of scales, weighted 1/s (eq. 24), over what noise would give there -
noise sits at one; a sample's number is its strongest octave.

ITS LAW, EXACT, NOT THEIR EQ. 28. The four coefficients of an octave are
correlated complex normals under noise, so their weighted power is a sum of
independent exponentials whose means are the eigenvalues of the weighted
covariance (a quadratic form in normal variables; Imhof 1961, Biometrika
48:419) - the hypoexponential law, closed form. Eq. 28's chi-square of 3.8
degrees has the right mean and nearly the right variance (a variance-matched
chi-square gives 3.9) but too light a far tail, which is where the cut below
is made: on a model of this arithmetic over 200 series of noise, eq. 28 named
something in 8.5 % of them and the variance match in 10 %, where at most 5 %
is due; the exact law in 3.5 % (measured 2026-10-13). Every octave is the same
wavelets at another size, so one law serves all; computed from the middle of
the series, where every weight falls on it - at an edge it is approximate.
The covariance is the circular one: the Morlet's pseudo-covariance is small
from a period of four samples up.

Every (sample, octave) therefore has a p-value on one scale. They are judged
together by Benjamini & Hochberg's false
discovery rate at 5 % (1995, JRSS B 57:289), as the window test judged its
windows: a thousand samples tested one by one at 5 % would name fifty of noise.
Neighbouring samples are positively correlated, which the procedure allows
(Benjamini & Yekutieli 2001, Ann. Statist. 29:1165). Averaged over every scale
at once instead, a burst at one period was diluted to nothing (the first
draft's test).

THE LEVEL. Named exactly when the number reaches it: the weakest number
named, or, with none named, what a single hypothesis would need (p = q / m).
It is the line the chart draws.

LIMITS. A white-noise background: a difference whose noise is itself red
(slow) reads as structure at long periods. The noise estimate the input was
standardised by moves every verdict. Direct convolution is O(N times the sum of
the scales): a few million steps for a thousand samples; an FFT was not used,
for the reason residual_randomness gives.
}
unit residual_wavelet;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Types, Math, residual_randomness;

const
    { Fewer samples than this hold less than one octave of scales whose cone
      leaves the middle clear - the least one judgement averages over. }
    WAVELET_MIN_SAMPLES = 20;
    { The Morlet wavelet's frequency (Torrence & Compo, Table 1). }
    MORLET_W0 = 6.0;
    { Scales per octave, as 1 / dj. }
    WAVELET_DJ = 0.25;
    { The shortest period looked at, in samples. Not their eq. 9's twice the
      step (a period of 2.07): a Morlet that short aliases, and passed its 95 %
      test on 8 % of noise where 5 % is due (measured, 2026-10-13) - and a
      wave needs two samples each way, so four is the shortest a pattern
      leaves. This software's choice. }
    WAVELET_SHORTEST_PERIOD = 4.0;
    { Where the wavelet is cut, in scales either side of its centre. }
    WAVELET_CUT = 4.0;

type
    TDoubleArrays = array of TDoubleArray;

    { Where the difference is not noise, one number a sample. }
    TWaveletStructure = record
        { The strongest octave's averaged power over what noise would give:
          one for noise. }
        Power: TDoubleArray;
        { The chance noise reaches that power in that octave. }
        P: TDoubleArray;
        { The samples Benjamini-Hochberg names at the significance. }
        Named: TBooleanDynArray;
        { Named exactly when Power reaches it. }
        Level: double;
        { [scale][sample]: the scale lies in an octave named at that sample. }
        NamedAt: array of TBooleanDynArray;
    end;

    { The power over noise at every period and sample. }
    TWaveletScalogram = record
        { The period of each scale, in samples, ascending. }
        Periods: TDoubleArray;
        { [scale][sample]: the power over what noise would give. }
        Power: array of TDoubleArray;
        { The power a cell must reach: ln 20. }
        Level: double;
        { Per sample, the period beyond which the edges shape what is seen
          (the cone of influence, e-folding time sqrt(2) s). }
        Cone: TDoubleArray;
    end;

{ The period, in samples, of a Morlet scale. }
function MorletPeriod(AScale: double): double;
{ The law of an octave's averaged power under noise: the means of the
  independent exponentials it sums, summing to one. }
function OctaveLaw: TDoubleArray;
{ The chance a sum of independent exponentials of distinct means ALam exceeds
  AX (the hypoexponential survival). }
function LawSurvival(AX: double; const ALam: array of double): double;
{ The AX at which LawSurvival is AP. }
function LawQuantile(AP: double; const ALam: array of double): double;
{ Where AR - a standardised difference - is not noise, each of ASpans (fit
  intervals, as indices into AR) transformed on its own and every sample of
  them judged together. A span below WAVELET_MIN_SAMPLES, and every sample
  outside the spans, keeps p one and is not named. }
function WaveletStructureOver(const AR: array of double;
    const ASpans: array of TSampleSpan): TWaveletStructure;
{ The same over the whole of AR. }
function WaveletStructure(const AR: array of double): TWaveletStructure;
{ The power of AR over noise at every period and sample. Below
  WAVELET_MIN_SAMPLES no period is reported. }
function WaveletScalogram(const AR: array of double): TWaveletScalogram;

implementation


type
    { The squared modulus of each coefficient and what noise would give. }
    TTransform = record
        Scales: TDoubleArray;
        Power, Expected: array of TDoubleArray;
    end;

function MorletPeriod(AScale: double): double;
begin
    Result := 4 * Pi * AScale / (MORLET_W0 + Sqrt(2 + Sqr(MORLET_W0)));
end;

function ScalesFor(AN: longint): TDoubleArray;
var
    Smallest, Largest: double;
    j, Count: longint;
begin
    Result := nil;
    Smallest := WAVELET_SHORTEST_PERIOD / MorletPeriod(1);
    Largest := AN / (2 * Sqrt(2));
    if Largest < Smallest then
        Exit;
    Count := Floor(Log2(Largest / Smallest) / WAVELET_DJ + 1e-9) + 1;
    SetLength(Result, Count);
    for j := 0 to Count - 1 do
        Result[j] := Smallest * Power(2, j * WAVELET_DJ);
end;

function Transform(const AR: array of double): TTransform;
var
    n, j, i, k, Reach: longint;
    Mean, S, Eta, Env, Re, Im, Weight: double;
    WRe, WIm: TDoubleArray;
begin
    Result := Default(TTransform);
    n := Length(AR);
    Result.Scales := ScalesFor(n);
    SetLength(Result.Power, Length(Result.Scales));
    SetLength(Result.Expected, Length(Result.Scales));
    Mean := 0;
    for i := 0 to n - 1 do
        Mean := Mean + AR[i];
    if n > 0 then
        Mean := Mean / n;
    for j := 0 to High(Result.Scales) do
    begin
        S := Result.Scales[j];
        Reach := Ceil(WAVELET_CUT * S);
        //  The wavelet's weights once per scale: pi^(-1/4) exp(i w0 eta)
        //  exp(-eta^2 / 2), times sqrt(1/s) (their eq. 3).
        SetLength(WRe, 2 * Reach + 1);
        SetLength(WIm, 2 * Reach + 1);
        for k := -Reach to Reach do
        begin
            Eta := k / S;
            Env := Power(Pi, -0.25) * Exp(-Sqr(Eta) / 2) / Sqrt(S);
            WRe[k + Reach] := Env * Cos(MORLET_W0 * Eta);
            WIm[k + Reach] := Env * Sin(MORLET_W0 * Eta);
        end;
        SetLength(Result.Power[j], n);
        SetLength(Result.Expected[j], n);
        for i := 0 to n - 1 do
        begin
            Re := 0;
            Im := 0;
            Weight := 0;
            for k := Max(-Reach, -i) to Min(Reach, n - 1 - i) do
            begin
                //  The conjugate of the wavelet, so the sign of Im is the
                //  opposite; its modulus is what is used.
                Re := Re + (AR[i + k] - Mean) * WRe[k + Reach];
                Im := Im - (AR[i + k] - Mean) * WIm[k + Reach];
                Weight := Weight + Sqr(WRe[k + Reach]) + Sqr(WIm[k + Reach]);
            end;
            Result.Power[j][i] := Sqr(Re) + Sqr(Im);
            Result.Expected[j][i] := Weight;
        end;
    end;
end;

{ The eigenvalues of the symmetric AM, by Jacobi's rotations. }
function SymmetricEigenvalues(AM: array of TDoubleArray): TDoubleArray;
var
    n, p, q, i, Sweep: longint;
    Off, Theta, T, C, S, App, Aqq, Apq, Aip, Aiq: double;
begin
    n := Length(AM);
    for Sweep := 1 to 100 do
    begin
        Off := 0;
        for p := 0 to n - 1 do
            for q := p + 1 to n - 1 do
                Off := Off + Sqr(AM[p][q]);
        if Off < 1e-30 then
            Break;
        for p := 0 to n - 2 do
            for q := p + 1 to n - 1 do
            begin
                if Abs(AM[p][q]) < 1e-300 then
                    Continue;
                App := AM[p][p];
                Aqq := AM[q][q];
                Apq := AM[p][q];
                Theta := (Aqq - App) / (2 * Apq);
                if Theta = 0 then
                    T := 1
                else
                    T := Sign(Theta) / (Abs(Theta) + Sqrt(Sqr(Theta) + 1));
                C := 1 / Sqrt(Sqr(T) + 1);
                S := T * C;
                for i := 0 to n - 1 do
                begin
                    Aip := AM[i][p];
                    Aiq := AM[i][q];
                    AM[i][p] := C * Aip - S * Aiq;
                    AM[i][q] := S * Aip + C * Aiq;
                end;
                for i := 0 to n - 1 do
                begin
                    Aip := AM[p][i];
                    Aiq := AM[q][i];
                    AM[p][i] := C * Aip - S * Aiq;
                    AM[q][i] := S * Aip + C * Aiq;
                end;
            end;
    end;
    SetLength(Result, n);
    for i := 0 to n - 1 do
        Result[i] := AM[i][i];
end;

function OctaveLaw: TDoubleArray;
var
    Per, R, a, b, k: longint;
    Scales, ReW, ImW: array of TDoubleArray;
    Smallest, Eta, Env, Total, SRe, SIm, Swap: double;
    CovRe, CovIm: array of TDoubleArray;
    M: array of TDoubleArray;
    Eig: TDoubleArray;
begin
    Per := Round(1 / WAVELET_DJ);
    Smallest := WAVELET_SHORTEST_PERIOD / MorletPeriod(1);
    SetLength(Scales, 1);
    SetLength(Scales[0], Per);
    for a := 0 to Per - 1 do
        Scales[0][a] := Smallest * Power(2, a * WAVELET_DJ);
    //  The octave's wavelets on one support, wide enough for its largest,
    //  each cut where Transform cuts it.
    R := Ceil(WAVELET_CUT * Scales[0][Per - 1]);
    SetLength(ReW, Per, 2 * R + 1);
    SetLength(ImW, Per, 2 * R + 1);
    for a := 0 to Per - 1 do
        for k := -R to R do
        begin
            Eta := k / Scales[0][a];
            if Abs(k) > Ceil(WAVELET_CUT * Scales[0][a]) then
                Continue;
            Env := Power(Pi, -0.25) * Exp(-Sqr(Eta) / 2) / Sqrt(Scales[0][a]);
            ReW[a][k + R] := Env * Cos(MORLET_W0 * Eta);
            ImW[a][k + R] := Env * Sin(MORLET_W0 * Eta);
        end;
    //  Cov(W_a, W_b) under unit noise is sum conj(psi_a) psi_b; weighted by
    //  1/s on both sides as C^(1/2) Cov C^(1/2), over what noise gives in
    //  all, its trace - so the law's mean is one.
    SetLength(CovRe, Per, Per);
    SetLength(CovIm, Per, Per);
    for a := 0 to Per - 1 do
        for b := 0 to Per - 1 do
        begin
            SRe := 0;
            SIm := 0;
            for k := 0 to 2 * R do
            begin
                SRe := SRe + ReW[a][k] * ReW[b][k] + ImW[a][k] * ImW[b][k];
                SIm := SIm + ReW[a][k] * ImW[b][k] - ImW[a][k] * ReW[b][k];
            end;
            CovRe[a][b] := SRe / Sqrt(Scales[0][a] * Scales[0][b]);
            CovIm[a][b] := SIm / Sqrt(Scales[0][a] * Scales[0][b]);
        end;
    Total := 0;
    for a := 0 to Per - 1 do
        Total := Total + CovRe[a][a];
    //  The Hermitian matrix as the real symmetric [[Re, -Im], [Im, Re]],
    //  which has each of its eigenvalues twice.
    SetLength(M, 2 * Per, 2 * Per);
    for a := 0 to Per - 1 do
        for b := 0 to Per - 1 do
        begin
            M[a][b] := CovRe[a][b] / Total;
            M[a + Per][b + Per] := CovRe[a][b] / Total;
            M[a][b + Per] := -CovIm[a][b] / Total;
            M[a + Per][b] := CovIm[a][b] / Total;
        end;
    Eig := SymmetricEigenvalues(M);
    for a := 1 to High(Eig) do
        for b := a downto 1 do
            if Eig[b] < Eig[b - 1] then
            begin
                Swap := Eig[b];
                Eig[b] := Eig[b - 1];
                Eig[b - 1] := Swap;
            end;
    SetLength(Result, Per);
    for a := 0 to Per - 1 do
        //  One of each pair.
        Result[a] := Eig[2 * a];
end;

function LawSurvival(AX: double; const ALam: array of double): double;
var
    i, j: longint;
    Prod: double;
begin
    if AX <= 0 then
        Exit(1);
    Result := 0;
    for i := 0 to High(ALam) do
    begin
        Prod := 1;
        for j := 0 to High(ALam) do
            if j <> i then
                Prod := Prod * ALam[i] / (ALam[i] - ALam[j]);
        Result := Result + Prod * Exp(-AX / ALam[i]);
    end;
    //  The partial fractions cancel near zero; the law cannot leave 0..1.
    Result := EnsureRange(Result, 0, 1);
end;

function LawQuantile(AP: double; const ALam: array of double): double;
var
    Lo, Hi, Mid: double;
    i: longint;
begin
    Lo := 0;
    Hi := 1;
    while LawSurvival(Hi, ALam) > AP do
        Hi := Hi * 2;
    for i := 1 to 200 do
    begin
        Mid := (Lo + Hi) / 2;
        if LawSurvival(Mid, ALam) > AP then
            Lo := Mid
        else
            Hi := Mid;
    end;
    Result := (Lo + Hi) / 2;
end;

{ The averaged power of each octave at each sample of AR, over noise's. Empty
  when AR holds less than one octave. }
function OctavePowers(const AR: array of double;
    out AReach: TDoubleArray): TDoubleArrays;
var
    T: TTransform;
    n, j, i, b, Count, Bands, PerBand, Lo: longint;
    Avg, Expect: double;
begin
    Result := nil;
    AReach := nil;
    n := Length(AR);
    if n < WAVELET_MIN_SAMPLES then
        Exit;
    T := Transform(AR);
    Count := Length(T.Scales);
    PerBand := Round(1 / WAVELET_DJ);
    if Count < PerBand then
        Exit;
    //  ONE OCTAVE AT A TIME (their section 5a averages over a band). Over
    //  every scale at once a burst at one period was diluted to nothing.
    //  Full octaves from the smallest scale, and the last one aligned to the
    //  largest so no scale is left out; every band is four scales in the same
    //  ratio, so every band follows the same law (OctaveLaw).
    Bands := Count div PerBand;
    if Count mod PerBand <> 0 then
        Inc(Bands);
    SetLength(Result, Bands);
    SetLength(AReach, Bands);
    for b := 0 to Bands - 1 do
    begin
        Lo := Min(b * PerBand, Count - PerBand);
        //  How near an edge a sample may be and still be judged in this
        //  octave: the cone of influence of its largest scale.
        AReach[b] := Sqrt(2) * T.Scales[Lo + PerBand - 1];
        SetLength(Result[b], n);
        for i := 0 to n - 1 do
        begin
            //  Their eq. 24, over the noise's own (Cdelta and dj cancel).
            Avg := 0;
            Expect := 0;
            for j := Lo to Lo + PerBand - 1 do
            begin
                Avg := Avg + T.Power[j][i] / T.Scales[j];
                Expect := Expect + T.Expected[j][i] / T.Scales[j];
            end;
            if Expect > 0 then
                Result[b][i] := Avg / Expect;
        end;
    end;
end;

function WaveletStructureOver(const AR: array of double;
    const ASpans: array of TSampleSpan): TWaveletStructure;
var
    n, s, b, i, m, Lo, Len: longint;
    Lam, Part: TDoubleArray;
    Powers: array of TDoubleArrays;
    Reach: array of TDoubleArray;
    PairP: TDoubleArray;
    PairSample, PairFirstScale: array of longint;
    PairPower: TDoubleArray;
    PairNamed: TBooleanDynArray;
    Scales: array of longint;
    First: boolean;
    j, Rows: longint;
begin
    Result := Default(TWaveletStructure);
    n := Length(AR);
    SetLength(Result.Power, n);
    SetLength(Result.P, n);
    SetLength(Result.Named, n);
    for i := 0 to n - 1 do
        Result.P[i] := 1;
    Result.Level := Infinity;
    Lam := OctaveLaw;
    //  EACH SPAN ON ITS OWN, so nothing reaches across the gap between two
    //  fit intervals; every (sample, octave) of every span one hypothesis.
    SetLength(Powers, Length(ASpans));
    SetLength(Reach, Length(ASpans));
    SetLength(Scales, Length(ASpans));
    Rows := 0;
    m := 0;
    for s := 0 to High(ASpans) do
    begin
        Lo := ASpans[s].Lo;
        Len := ASpans[s].Hi - Lo + 1;
        SetLength(Part, Len);
        for i := 0 to Len - 1 do
            Part[i] := AR[Lo + i];
        Powers[s] := OctavePowers(Part, Reach[s]);
        Inc(m, Length(Powers[s]) * Len);
        //  Every span's scales are the same ladder from the same smallest
        //  scale (ScalesFor), so scale j is one period in all of them.
        Scales[s] := Length(ScalesFor(Len));
        Rows := Max(Rows, Scales[s]);
    end;
    SetLength(Result.NamedAt, Rows);
    for j := 0 to Rows - 1 do
        SetLength(Result.NamedAt[j], n);
    SetLength(PairP, m);
    SetLength(PairSample, m);
    SetLength(PairFirstScale, m);
    SetLength(PairPower, m);
    //  m counted every pair; it ends as the number judged.
    m := 0;
    for s := 0 to High(ASpans) do
        for b := 0 to High(Powers[s]) do
            for i := 0 to High(Powers[s][b]) do
            begin
                //  INSIDE THE CONE an octave's wavelets are cut by the edge,
                //  no longer average to zero, and answer to what is not a
                //  rhythm: a wave running to a series' last sample was named
                //  by every long octave up to 130 samples before it began. Not
                //  judged there, as Torrence & Compo say of the cone.
                if Min(i, High(Powers[s][b]) - i) < Reach[s][b] then
                    Continue;
                PairSample[m] := ASpans[s].Lo + i;
                //  The band's scales, as OctavePowers lays them.
                PairFirstScale[m] := Min(b * Round(1 / WAVELET_DJ),
                    Scales[s] - Round(1 / WAVELET_DJ));
                PairPower[m] := Powers[s][b][i];
                PairP[m] := LawSurvival(PairPower[m], Lam);
                Inc(m);
            end;
    if m = 0 then
        Exit;
    SetLength(PairP, m);
    SetLength(PairSample, m);
    SetLength(PairPower, m);
    PairNamed := BenjaminiHochberg(PairP, RANDOMNESS_SIGNIFICANCE);
    //  A sample's number is its strongest octave.
    for i := 0 to m - 1 do
    begin
        if PairPower[i] > Result.Power[PairSample[i]] then
        begin
            Result.Power[PairSample[i]] := PairPower[i];
            Result.P[PairSample[i]] := PairP[i];
        end;
        Result.Named[PairSample[i]] := Result.Named[PairSample[i]] or
            PairNamed[i];
        if PairNamed[i] then
            for j := PairFirstScale[i] to PairFirstScale[i] +
                Round(1 / WAVELET_DJ) - 1 do
                Result.NamedAt[j][PairSample[i]] := True;
    end;
    First := True;
    for i := 0 to n - 1 do
        if Result.Named[i] and (First or (Result.Power[i] < Result.Level)) then
        begin
            Result.Level := Result.Power[i];
            First := False;
        end;
    if First then
        Result.Level := LawQuantile(RANDOMNESS_SIGNIFICANCE / m, Lam);
end;

function WaveletStructure(const AR: array of double): TWaveletStructure;
begin
    Result := WaveletStructureOver(AR, [SampleSpan(0, High(AR))]);
end;

function WaveletScalogram(const AR: array of double): TWaveletScalogram;
var
    T: TTransform;
    n, j, i: longint;
begin
    Result := Default(TWaveletScalogram);
    n := Length(AR);
    //  Their eq. 18 with a white background: chi-square 2 over 2 passes 95 %
    //  above -ln 0.05.
    Result.Level := -Ln(RANDOMNESS_SIGNIFICANCE);
    SetLength(Result.Cone, n);
    for i := 0 to n - 1 do
        Result.Cone[i] := MorletPeriod(Min(i, n - 1 - i) / Sqrt(2));
    if n < WAVELET_MIN_SAMPLES then
        Exit;
    T := Transform(AR);
    SetLength(Result.Periods, Length(T.Scales));
    SetLength(Result.Power, Length(T.Scales));
    for j := 0 to High(T.Scales) do
    begin
        Result.Periods[j] := MorletPeriod(T.Scales[j]);
        SetLength(Result.Power[j], n);
        for i := 0 to n - 1 do
        begin
            if T.Expected[j][i] > 0 then
                Result.Power[j][i] := T.Power[j][i] / T.Expected[j][i];
        end;
    end;
end;

end.
