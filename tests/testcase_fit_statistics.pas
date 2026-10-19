// SPDX-License-Identifier: GPL-3.0-or-later
{ The goodness-of-fit statistics unit. The reference values are lmfit's own
  formulas evaluated on the same arrays, so the native and Python backends agree
  on what a fit's reduced chi-squared, R^2 and AIC/BIC are. }
unit testcase_fit_statistics;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Math, fpcunit, testregistry, fit_statistics;

type
  TFitStatisticsTest = class(TTestCase)
  published
    procedure MatchesLmfitOnAKnownCase;
    procedure PerfectFitIsFiniteAndRSquaredOne;
    procedure EmptyOrMismatchedIsInvalid;
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

initialization
  RegisterTest('unit', TFitStatisticsTest);
end.
