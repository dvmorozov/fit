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
    int_fit_service, fit_statistics, title_points_set, SysUtils, log;

{ The statistics for the service's current fit, over the R-factor bound
  intervals. Valid=False when nothing has been fitted yet. }
function ServiceStatistics(ASvc: IFitService): TFitStatistics;

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

{ Whether an abscissa falls inside any R-factor bound interval. Bounds come as
  consecutive (start, end) pairs. }
function InBounds(ABounds: TTitlePointsSet; AX: double): boolean;
var
    i: longint;
begin
    Result := False;
    if not Assigned(ABounds) then
        Exit;
    i := 0;
    while i + 1 < ABounds.PointsCount do
    begin
        if (AX >= ABounds.PointXCoord[i]) and (AX <= ABounds.PointXCoord[i + 1]) then
            Exit(True);
        Inc(i, 2);
    end;
end;

function ServiceStatistics(ASvc: IFitService): TFitStatistics;
var
    Exp, Calc, Bounds: TTitlePointsSet;
    Reason: string;
    DataY, FitY: array of double;
    i, n: longint;
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
        //  complexity penalty quietly does nothing (D26).
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

        SetLength(DataY, Exp.PointsCount);
        SetLength(FitY, Exp.PointsCount);
        n := 0;
        //  Only the points inside the fitting window, so the empty (zero) tails
        //  of the calc profile do not swamp the residuals - the same region the
        //  engine's R-factor is taken over.
        for i := 0 to Exp.PointsCount - 1 do
            if InBounds(Bounds, Exp.PointXCoord[i]) then
            begin
                DataY[n] := Exp.PointYCoord[i];
                FitY[n] := Calc.PointYCoord[i];
                Inc(n);
            end;
        SetLength(DataY, n);
        SetLength(FitY, n);

        Result := ComputeFitStatistics(DataY, FitY, VaryingParamCount(ASvc),
            WeightPoisson);
    finally
        Exp.Free;
        Calc.Free;
        Bounds.Free;
    end;
end;

end.
