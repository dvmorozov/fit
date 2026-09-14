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
    Math;

type
    { How residuals are weighted. Poisson (1/sqrt(counts)) is the counting-data
      default for diffraction/spectroscopy; None leaves the fit unweighted. }
    TWeighting = (WeightNone, WeightPoisson);

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
    end;

{ Computes the statistics from the data and the fitted model sampled at the same
  points. ANVarys is the number of varying parameters. A record with Valid=False
  is returned when there is not enough data. }
function ComputeFitStatistics(const ADataY, AFitY: array of double;
    ANVarys: longint; AWeighting: TWeighting): TFitStatistics;

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

end.
