// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Goodness-of-fit statistics computed from an IFitService.)

Bridges the pure statistics formulas (fit_statistics) to a live service: it reads
the experimental and calculated profiles, the R-factor bounds and the curve
parameters through the public IFitService contract, and computes the fit's
reduced chi-squared, R^2 and AIC/BIC over the fitting window. Used both by the
native engine (TFitService.GetStatistics) and by the REST layer.
}
unit fit_service_statistics;

{$mode objfpc}{$H+}

interface

uses
    int_fit_service, fit_statistics, sample_coverage, title_points_set,
    residual_randomness, fit_window, SysUtils, log, named_points_set,
    fit_reduction, fit_advice, noise_rule;

{ The statistics for the service's current fit, over the R-factor bound
  intervals. Valid=False when nothing has been fitted yet. }
function ServiceStatistics(ASvc: IFitService): TFitStatistics; overload;
{ The same, naming AUncovered - the stretches of the fitting window no curve
  covers - and their part of the squared difference. Every figure is computed
  exactly as without them: the samples are named, never dropped. }
function ServiceStatistics(ASvc: IFitService;
    const AUncovered: TSampleRanges): TFitStatistics; overload;
{ The same for a model made of curves of AModelTypes, whose residuals are
  weighted as those types say (named_points_set.ModelResidualNoise): as
  counts, the default, unless one of them measures the noise of its data. }
function ServiceStatistics(ASvc: IFitService;
    const AUncovered: TSampleRanges;
    const AModelTypes: array of TNamedPointsSetClass): TFitStatistics; overload;

implementation

{ The number of curve parameters across all fitted curves. }
function VaryingParamCount(ASvc: IFitService): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to ASvc.GetCurveCount - 1 do
        Result := Result + ASvc.GetCurveParameterCount(i);
end;

function ServiceStatistics(ASvc: IFitService): TFitStatistics;
begin
    Result := ServiceStatistics(ASvc, nil);
end;

function ServiceStatistics(ASvc: IFitService;
    const AUncovered: TSampleRanges): TFitStatistics;
begin
    Result := ServiceStatistics(ASvc, AUncovered, []);
end;

function ServiceStatistics(ASvc: IFitService;
    const AUncovered: TSampleRanges;
    const AModelTypes: array of TNamedPointsSetClass): TFitStatistics;
var
    Exp, Calc, Bounds: TTitlePointsSet;
    Reason: string;
    AllX, AllData, AllFit, BoundsX: array of double;
    W: TFitWindow;
    i: longint;
    Residual, AllResidual, UncoveredResidual: double;
    Noise: TNoiseVariances;
    Rule: TNoiseRuleClass;
    Measured: boolean;
begin
    Result := EmptyFitStatistics;
    Exp := ASvc.GetProfilePointsSet;
    Calc := ASvc.GetCalcProfilePointsSet;
    Bounds := ASvc.GetRFactorBounds;
    try
        //  A fit must have run (calc profile) over defined intervals (bounds).
        //
        //  Saying WHICH precondition failed, rather than returning an invalid
        //  record silently. An empty statistic is indistinguishable from a bad
        //  one at the call site, and the candidate ranking depends on this: when
        //  it comes back invalid every count shows as "not scored" and the
        //  complexity penalty quietly does nothing.
        if not Assigned(Exp) then
            Reason := 'no experimental profile'
        else if not Assigned(Calc) then
            Reason := 'no calculated profile - nothing has been computed yet'
        else if not Assigned(Bounds) then
            Reason := 'no R-factor bounds'
        else if Bounds.PointsCount < 2 then
            Reason := Format('R-factor bounds need a pair; there are %d point(s)',
                [Bounds.PointsCount])
        else if Exp.PointsCount <> Calc.PointsCount then
            Reason := Format(
                'the calculated profile has %d point(s) but the data has %d',
                [Calc.PointsCount, Exp.PointsCount])
        else
            Reason := '';

        if Reason <> '' then
        begin
            WriteLog('statistics unavailable: ' + Reason, log.Notification);
            Exit;
        end;

        //  Only the points inside the fitting window, so the empty (zero) tails
        //  of the calc profile do not swamp the residuals - the same region the
        //  engine's R-factor is taken over, and the same samples the window's
        //  spectrum reads (fit_window).
        SetLength(AllX, Exp.PointsCount);
        SetLength(AllData, Exp.PointsCount);
        SetLength(AllFit, Exp.PointsCount);
        for i := 0 to Exp.PointsCount - 1 do
        begin
            AllX[i] := Exp.PointXCoord[i];
            AllData[i] := Exp.PointYCoord[i];
            AllFit[i] := Calc.PointYCoord[i];
        end;
        SetLength(BoundsX, Bounds.PointsCount);
        for i := 0 to Bounds.PointsCount - 1 do
            BoundsX[i] := Bounds.PointXCoord[i];
        //  THE NOISE OF THE WHOLE SERIES, by the rule of the model's curve
        //  types (TNamedPointsSet.NoiseRule), read over the window: the window
        //  picks its samples from the noise exactly as from the data, so each
        //  difference meets the variance measured at its own sample.
        Noise := ModelResidualNoise(AModelTypes, AllData, Rule);
        Measured := Assigned(Rule) and not Rule.WeighsFiguresAsCounts;
        W := FitWindowOf(AllX, AllData, AllFit, Noise, BoundsX);

        AllResidual := 0;
        UncoveredResidual := 0;
        for i := 0 to High(W.X) do
        begin
            //  UNWEIGHTED, as the R-factor's numerator is: the share says how
            //  much of THAT the uncovered samples make.
            Residual := Sqr(W.Data[i] - W.Fit[i]);
            AllResidual := AllResidual + Residual;
            if InRanges(AUncovered, W.X[i]) then
                UncoveredResidual := UncoveredResidual + Residual;
        end;

        if StatisticsWeightingFor(Measured) = WeightMeasuredNoise then
            Result := ComputeFitStatistics(W.Data, W.Fit, W.Noise,
                VaryingParamCount(ASvc))
        else
            Result := ComputeFitStatistics(W.Data, W.Fit,
                VaryingParamCount(ASvc), WeightPoisson);
        Result.UncoveredRanges := AUncovered;
        //  EACH INTERVAL ON ITS OWN: a window must not reach across the gap
        //  between two. AGAINST THE RULE'S NOISE: a price judged by the noise
        //  of counts had its quiet years named as structure and its loud ones
        //  read as noise. With no rule - no curve - nothing is judged: the
        //  framework knows no field's noise.
        if Length(W.Noise) > 0 then
            Result.Residual := JudgeResiduals(W.X, W.Data, W.Fit, W.Spans,
                W.Noise);
        if (Length(AUncovered) > 0) and (AllResidual > 0) then
            Result.UncoveredResidualShare := UncoveredResidual / AllResidual;
    finally
        Exp.Free;
        Calc.Free;
        Bounds.Free;
    end;
end;

end.
