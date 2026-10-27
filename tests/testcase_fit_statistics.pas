// SPDX-License-Identifier: GPL-3.0-or-later
{ The goodness-of-fit statistics unit. The reference values are lmfit's own
  formulas evaluated on the same arrays, so the native and Python backends agree
  on what a fit's reduced chi-squared, R^2 and AIC/BIC are. }
unit testcase_fit_statistics;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Math, fpcunit, testregistry, fit_statistics,
  fit_reduction;

type
  TFitStatisticsTest = class(TTestCase)
  published
    procedure MatchesLmfitOnAKnownCase;
    procedure PerfectFitIsFiniteAndRSquaredOne;
    procedure EmptyOrMismatchedIsInvalid;
    procedure PoissonStatisticsSayTheyAreWeightedAsCounts;
    procedure MeasuredNoiseDividesEachDifferenceByTheVarianceAtItsPoint;
    procedure MeasuredNoiseScoresByTheKnownVarianceFormsTheReductionUses;
    procedure AConstantNoiseIsTheSameVarianceAtEveryPoint;
    procedure MeasuredNoiseOfAnotherLengthIsInvalid;
  procedure AVarianceThatIsNotPositiveWeighsItsDifferenceAsOne;
  procedure AVarianceThatIsNotANumberWeighsItsDifferenceAsOne;
  procedure MeasuredNoiseWithNoDegreesOfFreedomLeavesTheChiSquareUndivided;
  procedure MeasuredNoiseOnFlatDataExplainsNoSpread;
  end;

implementation

procedure TFitStatisticsTest.MatchesLmfitOnAKnownCase;
const
  //  Data and model sampled at the same five points; 3 varying parameters.
  Data: array[0..4] of double = (10.0, 50.0, 200.0, 60.0, 12.0);
  Fit:  array[0..4] of double = ( 9.0, 55.0, 190.0, 58.0, 15.0);
var
  S: TFitStatistics;
begin
  S := ComputeFitStatistics(Data, Fit, 3, WeightPoisson);

  AssertTrue('valid', S.Valid);
  AssertEquals('data points', 5, S.DataPoints);
  AssertEquals('degrees of freedom', 2, S.DegreesOfFreedom);
  //  Reference values from lmfit's formulas on the same arrays.
  AssertEquals('chi-square', 1.9166666667, S.ChiSquare, 1e-6);
  AssertEquals('reduced chi-square', 0.9583333333, S.ReducedChiSquare, 1e-6);
  AssertEquals('R-squared', 0.9942796471, S.RSquared, 1e-6);
  AssertEquals('AIC', 1.2057482685, S.AIC, 1e-6);
  AssertEquals('BIC', 0.0340620058, S.BIC, 1e-6);
end;

procedure TFitStatisticsTest.PerfectFitIsFiniteAndRSquaredOne;
const
  Data: array[0..3] of double = (5.0, 20.0, 80.0, 15.0);
var
  S: TFitStatistics;
begin
  //  Model equals data: chi-square is zero, but AIC/BIC must stay finite
  //  (the chi-square is floored before the logarithm, as in lmfit).
  S := ComputeFitStatistics(Data, Data, 2, WeightPoisson);
  AssertTrue('valid', S.Valid);
  AssertEquals('R-squared is 1', 1.0, S.RSquared, 1e-12);
  AssertEquals('chi-square is 0', 0.0, S.ChiSquare, 1e-12);
  AssertTrue('AIC is finite', not IsInfinite(S.AIC) and not IsNan(S.AIC));
  AssertTrue('BIC is finite', not IsInfinite(S.BIC) and not IsNan(S.BIC));
end;

procedure TFitStatisticsTest.EmptyOrMismatchedIsInvalid;
var
  Empty: array of double;
  A: array[0..2] of double = (1, 2, 3);
  B: array[0..1] of double = (1, 2);
begin
  SetLength(Empty, 0);
  AssertFalse('empty is invalid', ComputeFitStatistics(Empty, Empty, 0, WeightNone).Valid);
  AssertFalse('length mismatch is invalid',
    ComputeFitStatistics(A, B, 1, WeightNone).Valid);
end;

procedure TFitStatisticsTest.PoissonStatisticsSayTheyAreWeightedAsCounts;
const
  Data: array[0..4] of double = (10.0, 50.0, 200.0, 60.0, 12.0);
  Fit:  array[0..4] of double = ( 9.0, 55.0, 190.0, 58.0, 15.0);
begin
  AssertTrue('weighted as counts',
    ComputeFitStatistics(Data, Fit, 3, WeightPoisson).Weighting = WeightPoisson);
end;

procedure TFitStatisticsTest.MeasuredNoiseDividesEachDifferenceByTheVarianceAtItsPoint;
const
  Data:  array[0..4] of double = (10.0, 50.0, 200.0, 60.0, 12.0);
  Fit:   array[0..4] of double = ( 9.0, 55.0, 190.0, 58.0, 15.0);
  Noise: array[0..4] of double = ( 0.5,  2.0,  4.0,  1.0,  9.0);
var
  S: TFitStatistics;
begin
  S := ComputeFitStatistics(Data, Fit, Noise, 3);
  AssertTrue('valid', S.Valid);
  AssertTrue('says it was weighted by the measured noise',
    S.Weighting = WeightMeasuredNoise);
  //  1/0.5 + 25/2 + 100/4 + 4/1 + 9/9 = 2 + 12.5 + 25 + 4 + 1.
  AssertEquals('chi-square', 44.5, S.ChiSquare, 1e-9);
  AssertEquals('reduced chi-square', 44.5 / 2, S.ReducedChiSquare, 1e-9);
  //  R-squared is unweighted whatever the weighting, as it always was.
  AssertEquals('R-squared', 0.9942796471, S.RSquared, 1e-6);
end;

procedure TFitStatisticsTest.MeasuredNoiseScoresByTheKnownVarianceFormsTheReductionUses;
const
  Data:  array[0..4] of double = (10.0, 50.0, 200.0, 60.0, 12.0);
  Fit:   array[0..4] of double = ( 9.0, 55.0, 190.0, 58.0, 15.0);
  Noise: array[0..4] of double = ( 0.5,  2.0,  4.0,  1.0,  9.0);
var
  S: TFitStatistics;
begin
  S := ComputeFitStatistics(Data, Fit, Noise, 3);
  //  The variance is measured, not estimated from the residual: chi2 + 2k and
  //  chi2 + k ln n - the score a reduction by BIC compares, the same function.
  AssertEquals('AIC', 44.5 + 2 * 3, S.AIC, 1e-9);
  AssertEquals('BIC is the reduction''s', ReductionBic(44.5, 5, 3), S.BIC, 1e-9);
  AssertEquals('which is chi2 + k ln n', 44.5 + 3 * Ln(5), S.BIC, 1e-9);
end;

procedure TFitStatisticsTest.AConstantNoiseIsTheSameVarianceAtEveryPoint;
const
  Data:  array[0..3] of double = (5.0, 20.0, 80.0, 15.0);
  Fit:   array[0..3] of double = (6.0, 18.0, 83.0, 15.0);
  Noise: array[0..3] of double = (4.0, 4.0, 4.0, 4.0);
begin
  //  (1 + 4 + 9 + 0) / 4.
  AssertEquals(3.5, ComputeFitStatistics(Data, Fit, Noise, 1).ChiSquare, 1e-9);
end;

procedure TFitStatisticsTest.MeasuredNoiseOfAnotherLengthIsInvalid;
const
  Data:  array[0..2] of double = (1, 2, 3);
  Noise: array[0..1] of double = (1, 1);
begin
  AssertFalse(ComputeFitStatistics(Data, Data, Noise, 1).Valid);
end;

procedure TFitStatisticsTest.AVarianceThatIsNotPositiveWeighsItsDifferenceAsOne;
const
  Data:  array[0..2] of double = (5.0, 20.0, 80.0);
  Fit:   array[0..2] of double = (6.0, 18.0, 83.0);
  Noise: array[0..2] of double = (0.0, -4.0, 9.0);
begin
  //  A variance of zero would divide by zero, and a negative one means
  //  nothing: each such point counts its plain squared difference.
  //  1 + 4 + 9 / 9.
  AssertEquals(6.0, ComputeFitStatistics(Data, Fit, Noise, 1).ChiSquare, 1e-9);
end;

procedure TFitStatisticsTest.AVarianceThatIsNotANumberWeighsItsDifferenceAsOne;
var
  Data, Fit, Noise: array[0..1] of double;
begin
  //  A curve type that could not measure a point's noise may hand back NaN.
  //  It is weighed as a variance that means nothing - as one - and the
  //  comparison deciding so must not itself raise.
  Data[0] := 5.0;  Fit[0] := 6.0;  Noise[0] := NaN;
  Data[1] := 20.0; Fit[1] := 18.0; Noise[1] := 4.0;
  AssertEquals(2.0, ComputeFitStatistics(Data, Fit, Noise, 1).ChiSquare, 1e-9);
end;

procedure TFitStatisticsTest.MeasuredNoiseWithNoDegreesOfFreedomLeavesTheChiSquareUndivided;
const
  Data:  array[0..1] of double = (5.0, 20.0);
  Fit:   array[0..1] of double = (6.0, 18.0);
  Noise: array[0..1] of double = (1.0, 4.0);
var
  S: TFitStatistics;
begin
  S := ComputeFitStatistics(Data, Fit, Noise, 2);
  AssertEquals('no degrees of freedom', 0, S.DegreesOfFreedom);
  //  (1 + 1): divided by nothing rather than by zero.
  AssertEquals(2.0, S.ReducedChiSquare, 1e-9);
end;

procedure TFitStatisticsTest.MeasuredNoiseOnFlatDataExplainsNoSpread;
const
  Data:  array[0..2] of double = (7.0, 7.0, 7.0);
  Fit:   array[0..2] of double = (6.0, 7.0, 8.0);
  Noise: array[0..2] of double = (1.0, 1.0, 1.0);
begin
  //  Data with no spread of its own has none for a model to explain: R-squared
  //  is reported as 0, not as a division by zero.
  AssertEquals(0.0, ComputeFitStatistics(Data, Fit, Noise, 1).RSquared, 1e-12);
end;

initialization
  RegisterTest('unit', TFitStatisticsTest);
end.
