// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How a reduction decides whether a curve may go: the two rules, their
names on the wire and in a project file, and the one comparison each makes.)

A REDUCTION REMOVES CURVES ONE AT A TIME and keeps each removal only if the
model is still good enough without it. "Good enough" is the whole question, and
there are two answers:

  * MAX-DIFFERENCE, the original rule. A removal is kept while the R-factor stays
    within Fit > Set Max Acceptable Difference. The limit is this software's
    choice and the user's to tune; nothing in the data says where it belongs, so
    it decides how many curves there are as much as the data does.
  * BIC, the Bayesian information criterion (Schwarz 1978). A removal is kept
    when the model without the curve scores LOWER: a better fit is rewarded, every
    parameter is charged ln(n). A curve stays exactly when it improves the fit by
    more than it costs, which needs no limit from anyone.

WHY BIC IS THE DEFAULT FOR NEW WORK, AND NOT FOR OLD. It is the user's decision
(2026-10-09) and the reason is above: the count comes from the data. A project
file that does not state the rule was made under the limit, so it is read as
max-difference and reproduces exactly; only a session that starts fresh removes
by BIC.

LIVES IN Common/ because the name is persisted by the client, carried on the
wire, held by the service and applied by the task - places in two processes
that must not disagree about what a value means (the same reason as
fit_weighting).
}
unit fit_reduction;

{$MODE Delphi}

interface

type
    { Ordinal 0 is the ORIGINAL rule on purpose: a task made through the
      inherited TComponent constructor is zero-initialised, and must then behave
      as it always did. }
    TReductionCriterion = (rcMaxDifference, rcInformationCriterion);

    TNoiseVariances = array of double;

const
    REDUCTION_MAX_DIFFERENCE = 'max-difference';
    REDUCTION_BIC = 'bic';

{ The canonical name of ACriterion, as stored and sent. }
function ReductionCriterionName(ACriterion: TReductionCriterion): string;

{ Reads a name written by ReductionCriterionName. False for anything else, so
  the boundary can refuse it in words rather than guess. }
function TryReductionCriterion(const AName: string;
    out ACriterion: TReductionCriterion): boolean;

{ The names there are, for a refusal to list. }
function KnownReductionCriteria: string;

{ THE SCORE A REDUCTION BY BIC COMPARES: chi2 + k ln n (Schwarz 1978), where
  AChiSquare is the residual divided by the variance of the noise - so minus
  twice the log-likelihood for Gaussian noise of known variance.

  KNOWN, NOT ESTIMATED FROM THE RESIDUAL. The form lmfit reports,
  n ln(chi2/n) + k ln n, estimates the variance from the residual, and the
  automatic run starts from a curve on nearly every sample: such a model fits
  the noise, drives the residual to zero and scores towards minus infinity, so
  no removal would ever look better. The variance here is measured from the
  data (NoiseVariancesOf), which no number of parameters can change. }
function ReductionBic(AChiSquare: double; APoints, AParams: longint): double;

{ THE VARIANCE OF THE NOISE AT EACH POINT OF AY, measured from the data alone
  as a straight line in the signal's level: variance = a + b * level, the
  Poisson-Gaussian model of counting detectors (Foi et al. 2008). b carries the
  counts' own noise, which grows with them; a carries a constant part - read
  noise, or the counts of a background that has been taken off, which is what
  Fit > Automatically does to the copy it reduces. Data that are not counts and
  whose noise is flat simply get b near zero.

  How a and b are found: the second difference y[i-1] - 2 y[i] + y[i+1] of
  independent noise has six times its variance and a smooth signal hardly
  reaches it. The points are sorted by level into up to NOISE_LEVEL_BINS bins;
  in each, the median squared second difference over 6 * 0.4549 (the median of
  a chi-square of one degree) estimates the variance robustly - a peak's few
  steep points do not carry it - and a least-squares line through the bins gives
  a and b, b no less than zero.

  WHY NOT ONE FIGURE FOR ALL, OR A WINDOW AROUND EACH POINT. One figure is set by
  the many quiet points and made every residual on a peak look significant: BIC
  kept curves the peak did not need. A window around each point did the same on
  a peak a few samples wide, whose window is mostly baseline.

  Floored at a quarter of the smallest bin's variance and at a millionth of the
  largest value, squared: noiseless data would otherwise make every residual
  infinitely significant. }
function NoiseVariancesOf(const AY: array of double): TNoiseVariances;

const
    { How many level bins the noise line is fitted through: enough to see a
      slope, few enough that each bin's median rests on many points. }
    NOISE_LEVEL_BINS = 10;
    { Points per bin below which there are fewer bins. }
    NOISE_POINTS_PER_BIN = 30;

{ Whether a removal is kept under BIC: the model without the curve must score
  strictly lower. Equal is not better - a removal that changes nothing measurable
  is not evidence the curve was superfluous, and keeping it would make the
  outcome depend on the order the curves were tried in. }
function BicFavoursRemoval(ABicWith, ABicWithout: double): boolean;

implementation

uses
    Math;

function ReductionCriterionName(ACriterion: TReductionCriterion): string;
begin
    case ACriterion of
        rcInformationCriterion: Result := REDUCTION_BIC;
    else
        Result := REDUCTION_MAX_DIFFERENCE;
    end;
end;

function TryReductionCriterion(const AName: string;
    out ACriterion: TReductionCriterion): boolean;
begin
    Result := True;
    if AName = REDUCTION_BIC then
        ACriterion := rcInformationCriterion
    else if AName = REDUCTION_MAX_DIFFERENCE then
        ACriterion := rcMaxDifference
    else
    begin
        ACriterion := rcMaxDifference;
        Result := False;
    end;
end;

function KnownReductionCriteria: string;
begin
    Result := REDUCTION_BIC + ', ' + REDUCTION_MAX_DIFFERENCE;
end;

{ The median of AValues, which it sorts. }
function MedianOf(var AValues: array of double): double;
var
    i, j: longint;
    T: double;
begin
    //  Insertion sort: one interval's worth of samples, once per judgement.
    for i := 1 to High(AValues) do
    begin
        T := AValues[i];
        j := i - 1;
        while (j >= 0) and (AValues[j] > T) do
        begin
            AValues[j + 1] := AValues[j];
            Dec(j);
        end;
        AValues[j + 1] := T;
    end;
    i := Length(AValues) div 2;
    if Odd(Length(AValues)) then
        Result := AValues[i]
    else
        Result := (AValues[i - 1] + AValues[i]) / 2;
end;

function ReductionBic(AChiSquare: double; APoints, AParams: longint): double;
begin
    Result := AChiSquare + Ln(Max(APoints, 1)) * AParams;
end;

function NoiseVariancesOf(const AY: array of double): TNoiseVariances;
const
    //  The median of a chi-square of one degree of freedom.
    CHI2_1_MEDIAN = 0.4549;
var
    Level, Sq, BinLevel, BinVar, Part: array of double;
    Order: array of longint;
    i, j, k, n, Bins, Lo, Hi, T: longint;
    Peak, Floor, a, b, MeanL, MeanV, Sxx, Sxy, Smallest: double;

    function LevelAt(AIndex: longint): double;
    begin
        if (AIndex > 0) and (AIndex < High(AY)) then
            Result := (AY[AIndex - 1] + AY[AIndex] + AY[AIndex + 1]) / 3
        else
            Result := AY[AIndex];
    end;

begin
    SetLength(Result, Length(AY));
    Peak := 0;
    for i := 0 to High(AY) do
        Peak := Max(Peak, Abs(AY[i]));
    Floor := Max(Sqr(1e-6 * Peak), 1e-300);
    n := Length(AY) - 2;
    if n <= 0 then
    begin
        for i := 0 to High(Result) do
            Result[i] := Max(Floor, 1);
        Exit;
    end;

    //  Each second difference with the level it was taken at, ordered by level.
    SetLength(Level, n);
    SetLength(Sq, n);
    SetLength(Order, n);
    for k := 0 to n - 1 do
    begin
        Level[k] := LevelAt(k + 1);
        Sq[k] := Sqr(AY[k] - 2 * AY[k + 1] + AY[k + 2]) / 6;
        Order[k] := k;
    end;
    for i := 1 to n - 1 do
    begin
        T := Order[i];
        j := i - 1;
        while (j >= 0) and (Level[Order[j]] > Level[T]) do
        begin
            Order[j + 1] := Order[j];
            Dec(j);
        end;
        Order[j + 1] := T;
    end;

    Bins := Max(1, Min(NOISE_LEVEL_BINS, n div NOISE_POINTS_PER_BIN));
    SetLength(BinLevel, Bins);
    SetLength(BinVar, Bins);
    for k := 0 to Bins - 1 do
    begin
        Lo := (k * n) div Bins;
        Hi := ((k + 1) * n) div Bins - 1;
        SetLength(Part, Hi - Lo + 1);
        for i := Lo to Hi do
            Part[i - Lo] := Level[Order[i]];
        BinLevel[k] := MedianOf(Part);
        for i := Lo to Hi do
            Part[i - Lo] := Sq[Order[i]];
        BinVar[k] := MedianOf(Part) / CHI2_1_MEDIAN;
    end;

    //  The line through the bins, its slope no less than zero.
    MeanL := 0;
    MeanV := 0;
    Smallest := BinVar[0];
    for k := 0 to Bins - 1 do
    begin
        MeanL := MeanL + BinLevel[k] / Bins;
        MeanV := MeanV + BinVar[k] / Bins;
        Smallest := Min(Smallest, BinVar[k]);
    end;
    Sxx := 0;
    Sxy := 0;
    for k := 0 to Bins - 1 do
    begin
        Sxx := Sxx + Sqr(BinLevel[k] - MeanL);
        Sxy := Sxy + (BinLevel[k] - MeanL) * (BinVar[k] - MeanV);
    end;
    if Sxx > 0 then
        b := Max(0, Sxy / Sxx)
    else
        b := 0;
    a := MeanV - b * MeanL;
    Floor := Max(Floor, Smallest / 4);

    for i := 0 to High(AY) do
        Result[i] := Max(a + b * LevelAt(i), Floor);
end;

function BicFavoursRemoval(ABicWith, ABicWithout: double): boolean;
begin
    Result := ABicWithout < ABicWith;
end;

end.
