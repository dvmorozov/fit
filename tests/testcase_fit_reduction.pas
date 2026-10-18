// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The score a reduction by BIC compares, and where its noise comes from.)

The automatic run starts from a curve on nearly every sample. A BIC whose
variance is estimated from the residual - the form lmfit reports - lets such a
model fit the noise, drive the residual to zero and score towards minus
infinity, so no removal ever looks better. The score here divides the residual
by a noise variance measured from the DATA, which no number of parameters can
shrink - and measured around each point, since counts are noisier where they
are high.
}
unit testcase_fit_reduction;

{$MODE Delphi}

interface

uses Classes, SysUtils, Math, fpcunit, testregistry, fit_reduction;

type
    TFitReductionTest = class(TTestCase)
    published
        procedure AnOverfitPaysForEveryParameter;
        procedure TheNoiseIsMeasuredFromTheDataNotTheFit;
        procedure TheNoiseFollowsTheCounts;
        procedure TwoSamplesCannotMeasureNoiseAndCountAtUnitVariance;
    end;

implementation

const
    N = 60;

procedure TFitReductionTest.AnOverfitPaysForEveryParameter;
var
    Data: array of double;
    i: longint;
    Rss, Noise: double;
begin
    SetLength(Data, N);
    for i := 0 to N - 1 do
        //  Scattered about 100 by +-10, alternately.
        Data[i] := 100 + 10 * (2 * (i mod 2) - 1);
    Noise := NoiseVariancesOf(Data)[N div 2];
    //  A flat model leaves the scatter as its residual, with one parameter; a
    //  model with nearly a parameter per point fits it exactly.
    Rss := 0;
    for i := 0 to N - 1 do
        Rss := Rss + Sqr(Data[i] - 100);
    AssertTrue('the flat model scores lower than the one that fits the noise',
        ReductionBic(Rss / Noise, N, 1) < ReductionBic(0, N, N - 2));
end;

procedure TFitReductionTest.TheNoiseIsMeasuredFromTheDataNotTheFit;
var
    Data: array of double;
    i: longint;
    Seed: longword;
    Noise: double;
begin
    //  A slow parabola, well above the noise, with scatter of standard
    //  deviation 5 from a fixed sequence.
    SetLength(Data, 400);
    Seed := 4321;
    for i := 0 to High(Data) do
    begin
        Seed := Seed * 1103515245 + 12345;
        Data[i] := 1000 + 0.01 * Sqr(i - 200) +
            5 * Sqrt(3) * (((Seed shr 16) and $7FFF) / $7FFF * 2 - 1);
    end;
    Noise := NoiseVariancesOf(Data)[200];
    AssertTrue(Format('the noise variance, 25, within a factor of two: %.2f',
        [Noise]), (Noise > 12.5) and (Noise < 50));
end;

{ COUNTS ARE NOISIER WHERE THEY ARE HIGH. One figure for the whole interval is
  set by its many quiet points and made every residual on a peak look
  significant, so BIC kept curves the peak did not need. Counts with a constant
  taken off - a background - keep the noise of the counts that were there. }
procedure TFitReductionTest.TheNoiseFollowsTheCounts;
var
    Data, Noise: array of double;
    i: longint;
    Seed: longword;
    Counts, Draw: double;
    k: longint;
begin
    SetLength(Data, 600);
    Seed := 777;
    for i := 0 to High(Data) do
    begin
        //  Counts rising from 300 to 3300, Poisson-like: variance = counts;
        //  then a background of 300 is taken off.
        Counts := 300 + 5 * i;
        //  Four uniform draws summed: close to normal, variance one.
        Draw := 0;
        for k := 1 to 4 do
        begin
            Seed := Seed * 1103515245 + 12345;
            Draw := Draw + ((Seed shr 16) and $7FFF) / $7FFF;
        end;
        Data[i] := Counts + Sqrt(Counts) * Sqrt(3) * (Draw - 2) - 300;
    end;
    Noise := NoiseVariancesOf(Data);
    AssertTrue(Format('quiet end: 300 within a factor of two, got %.1f',
        [Noise[0]]), (Noise[0] > 150) and (Noise[0] < 600));
    AssertTrue(Format('loud end: 3300 within a factor of two, got %.1f',
        [Noise[High(Noise)]]),
        (Noise[High(Noise)] > 1650) and (Noise[High(Noise)] < 6600));
end;

{ Noise is measured from second differences, and two samples have none. Each
  residual then counts at unit variance - the scale the data is in - rather than
  at a figure invented from nothing. }
procedure TFitReductionTest.TwoSamplesCannotMeasureNoiseAndCountAtUnitVariance;
var
    Noise: TNoiseVariances;
begin
    Noise := NoiseVariancesOf([3, 5]);
    AssertEquals(2, Length(Noise));
    AssertEquals(1, Noise[0], 0);
    AssertEquals(1, Noise[1], 0);
end;

initialization
    RegisterTest('unit', TFitReductionTest);
end.
