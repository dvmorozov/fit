// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Goodness-of-fit statistics for a fitted profile.)

The native downhill-simplex engine reports only an R-factor. These are the
publishable numbers a fit needs beyond that (Stage 2): the weighted reduced
chi-squared, the coefficient of determination R^2, and the Akaike / Bayesian
information criteria. The formulas match lmfit so the native and Python backends
report the same statistic for the same fit.

The record is shared: fit_server computes it, the desktop parses it off the wire
and displays it.
}
unit fit_statistics;

{$mode objfpc}{$H+}

interface

uses
    Math, sample_coverage, residual_randomness, fit_reduction;

type
    { How residuals are weighted. Poisson (1/sqrt(counts)) is the counting-data
      default for diffraction/spectroscopy; None leaves the fit unweighted;
      MeasuredNoise divides each squared difference by the variance of the
      noise measured from the data, for data that are not counts - what a
      model whose noise rule does not weigh as counts asks for
      (TNamedPointsSet.NoiseRule). Added last, so the ordinals
      of the first two are what they always were. }
    TWeighting = (WeightNone, WeightPoisson, WeightMeasuredNoise);

const
    { How the statistics' "weighting" field names WeightMeasuredNoise on the
      wire. Sent only for it: a reply without the field is weighted as counts,
      which is every reply there was before a model could say otherwise. }
    STATISTICS_WEIGHTING_MEASURED_NOISE = 'measured-noise';

type

    { The goodness-of-fit numbers for one fit. Valid is False when there are too
      few points to compute them. }
    TFitStatistics = record
        Valid:            boolean;
        DataPoints:       longint;   //  points in the fitting window (ndata)
        Params:           longint;   //  varying parameters (nvarys)
        DegreesOfFreedom: longint;   //  ndata - nvarys
        ChiSquare:        double;    //  weighted sum of squared residuals
        ReducedChiSquare: double;    //  ChiSquare / DegreesOfFreedom
        RSquared:         double;    //  1 - SS_res / SS_tot
        AIC:              double;    //  Akaike information criterion
        BIC:              double;    //  Bayesian information criterion
        { THE SAMPLES NO CURVE COVERS, which the model gives 0 - part of no
          figure above, every one of which still counts them. They are named
          so the window can say why a figure is what it is: one such sample
          can outweigh every other residual (sample_coverage). }
        UncoveredRanges:  TSampleRanges;
        { Their part of the squared difference over the fitting window, 0..1. }
        UncoveredResidualShare: double;
        { WHETHER THE DIFFERENCE IS ONLY NOISE, each fit interval judged on its
          own, and where it is not (residual_randomness). }
        Residual: TResidualRandomness;
        { HOW EACH DIFFERENCE WAS WEIGHTED in ChiSquare and every figure made
          from it, so the window can say so: a reduced chi-squared means
          nothing to someone who does not know what it was divided by. }
        Weighting: TWeighting;
    end;

{ Computes the statistics from the data and the fitted model sampled at the same
  points. ANVarys is the number of varying parameters. A record with Valid=False
  is returned when there is not enough data. }
function ComputeFitStatistics(const ADataY, AFitY: array of double;
    ANVarys: longint; AWeighting: TWeighting): TFitStatistics;

{ THE SAME FIGURES WITH EACH SQUARED DIFFERENCE DIVIDED BY THE VARIANCE OF THE
  NOISE AT ITS POINT, ANoiseVariances - one per point, measured from the data
  (fit_reduction.NoiseVariancesOf, or a module's own measure of it); a constant
  noise is the same value at every point. Weighting is WeightMeasuredNoise.

  AIC AND BIC ARE THE KNOWN-VARIANCE FORMS, chi2 + 2k and chi2 + k ln n
  (fit_reduction.ReductionBic), not lmfit's n ln(chi2/n) + ...: the variance
  here is measured from the data, not estimated from the residual, so chi2 is
  already minus twice the log-likelihood up to a constant. It is also the score
  a reduction by BIC compares, so the BIC shown is the one the count was chosen
  by. A variance that is not above zero counts as 1, so a bad measure cannot
  divide by zero; invalid when the three lengths differ. }
function ComputeFitStatistics(const ADataY, AFitY, ANoiseVariances: array of double;
    ANVarys: longint): TFitStatistics;

{ Empty statistics (Valid=False), for "no fit yet". }
function EmptyFitStatistics: TFitStatistics;

implementation

function EmptyFitStatistics: TFitStatistics;
begin
    Result := Default(TFitStatistics);
    Result.Valid := False;
end;

function PointWeight(AValue: double; AWeighting: TWeighting): double;
begin
    case AWeighting of
        WeightPoisson:
            //  1/sqrt(counts), floored at 1 count so empty channels neither
            //  divide by zero nor dominate the sum.
            Result := 1.0 / Sqrt(Max(AValue, 1.0));
        else
            Result := 1.0;
    end;
end;

function ComputeFitStatistics(const ADataY, AFitY: array of double;
    ANVarys: longint; AWeighting: TWeighting): TFitStatistics;
var
    i, n: longint;
    w, r, chisqr, ssRes, ssTot, meanY, neg2LogLikel: double;
begin
    Result := EmptyFitStatistics;
    n := Length(ADataY);
    if (n = 0) or (n <> Length(AFitY)) then
        Exit;

    //  Weighted chi-squared and the residual sum of squares.
    chisqr := 0;
    ssRes := 0;
    meanY := 0;
    for i := 0 to n - 1 do
        meanY := meanY + ADataY[i];
    meanY := meanY / n;

    ssTot := 0;
    for i := 0 to n - 1 do
    begin
        w := PointWeight(ADataY[i], AWeighting);
        r := (AFitY[i] - ADataY[i]) * w;
        chisqr := chisqr + Sqr(r);
        ssRes := ssRes + Sqr(AFitY[i] - ADataY[i]);
        ssTot := ssTot + Sqr(ADataY[i] - meanY);
    end;

    Result.Valid := True;
    Result.Weighting := AWeighting;
    Result.DataPoints := n;
    Result.Params := ANVarys;
    Result.DegreesOfFreedom := n - ANVarys;
    Result.ChiSquare := chisqr;

    if Result.DegreesOfFreedom > 0 then
        Result.ReducedChiSquare := chisqr / Result.DegreesOfFreedom
    else
        Result.ReducedChiSquare := chisqr;

    if ssTot > 0 then
        Result.RSquared := 1.0 - ssRes / ssTot
    else
        Result.RSquared := 0.0;

    //  AIC / BIC as lmfit computes them, from the (weighted) chi-squared.
    //  chisqr is floored so a near-perfect fit does not take ln(0).
    if chisqr < 1e-250 then
        chisqr := 1e-250;
    neg2LogLikel := n * Ln(chisqr / n);
    Result.AIC := neg2LogLikel + 2 * ANVarys;
    Result.BIC := neg2LogLikel + Ln(n) * ANVarys;
end;

function ComputeFitStatistics(const ADataY, AFitY, ANoiseVariances: array of double;
    ANVarys: longint): TFitStatistics;
var
    i, n: longint;
    v, chisqr, ssRes, ssTot, meanY: double;
begin
    Result := EmptyFitStatistics;
    n := Length(ADataY);
    if (n = 0) or (n <> Length(AFitY)) or (n <> Length(ANoiseVariances)) then
        Exit;

    meanY := 0;
    for i := 0 to n - 1 do
        meanY := meanY + ADataY[i];
    meanY := meanY / n;
    chisqr := 0;
    ssRes := 0;
    ssTot := 0;
    for i := 0 to n - 1 do
    begin
        v := ANoiseVariances[i];
        //  NaN first: comparing NaN raises under the FPU's default mask, so
        //  "not (v > 0)" alone never reached the point it was written for.
        if IsNan(v) or not (v > 0) then
            v := 1;
        chisqr := chisqr + Sqr(AFitY[i] - ADataY[i]) / v;
        ssRes := ssRes + Sqr(AFitY[i] - ADataY[i]);
        ssTot := ssTot + Sqr(ADataY[i] - meanY);
    end;

    Result.Valid := True;
    Result.Weighting := WeightMeasuredNoise;
    Result.DataPoints := n;
    Result.Params := ANVarys;
    Result.DegreesOfFreedom := n - ANVarys;
    Result.ChiSquare := chisqr;
    if Result.DegreesOfFreedom > 0 then
        Result.ReducedChiSquare := chisqr / Result.DegreesOfFreedom
    else
        Result.ReducedChiSquare := chisqr;
    //  UNWEIGHTED, as under counts: R-squared is a share of the data's own
    //  spread, which no weighting enters.
    if ssTot > 0 then
        Result.RSquared := 1.0 - ssRes / ssTot
    else
        Result.RSquared := 0.0;
    //  THE KNOWN-VARIANCE FORMS: no floor and no logarithm, because nothing
    //  here is estimated from the residual - a perfect fit scores its
    //  parameters' cost alone, which is what it is.
    Result.AIC := chisqr + 2 * ANVarys;
    Result.BIC := ReductionBic(chisqr, n, ANVarys);
end;

end.
