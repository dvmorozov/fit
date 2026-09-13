// SPDX-License-Identifier: GPL-3.0-or-later
{ Golden-oracle + analytic-limit tests for the exponentially modified Gaussian,
  computed via the native expression engine (erfcx from special_functions,
  mirrored by scipy in the sidecar). Pins the golden numpy values and the tau -> 0
  limit (the area-normalised Gaussian). }
unit testcase_emg;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry,
  points_set, emg_points_set;
type
  TEmgTest = class(TTestCase)
  private
    function Emg(A, sigma, tau, x: double): double;
    function Gauss(A, sigma, x: double): double;
  published
    procedure GoldenValuesMatchNumpy;
    procedure LimitTauToZeroIsGaussian;
    procedure ExpressionIsNumpyAndNamesParameters;
  end;

implementation

const
  X0 = 10.0;

function TEmgTest.Emg(A, sigma, tau, x: double): double;
var
  C: TEmgPointsSet;
begin
  C := TEmgPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    C.ValuesByName['x0'] := X0;
    C.ValuesByName['A'] := A;
    C.ValuesByName['sigma'] := sigma;
    C.ValuesByName['tau'] := tau;
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

function TEmgTest.Gauss(A, sigma, x: double): double;
begin
  Result := A / (sigma * Sqrt(2 * Pi)) * Exp(-Sqr(x - X0) / (2 * Sqr(sigma)));
end;

procedure TEmgTest.GoldenValuesMatchNumpy;
begin
  AssertEquals('golden 1', 28.845040163, Emg(100, 1.0, 0.8, 10), 1e-6);
  AssertEquals('golden 2', 25.067687392, Emg(100, 1.0, 0.8, 11.5), 1e-6);
  AssertEquals('golden 3', 14.039226049, Emg(80, 1.5, 2.0, 12), 1e-6);
end;

procedure TEmgTest.LimitTauToZeroIsGaussian;
var
  x: double;
begin
  //  tau = 1e-6 -> deviation from the Gaussian is O(tau) ~ 1e-5 (see the
  //  companion numpy check); a 1e-3 tolerance is a comfortable margin.
  x := 8.0;
  while x <= 12.0 do
  begin
    AssertEquals('EMG(tau->0) = Gaussian at x=' + FloatToStr(x),
      Gauss(100, 1.2, x), Emg(100, 1.2, 1e-6, x), 1e-3);
    x := x + 0.25;
  end;
end;

procedure TEmgTest.ExpressionIsNumpyAndNamesParameters;
var
  C: TEmgPointsSet;
  Expr: string;
begin
  C := TEmgPointsSet.Create(nil, X0);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('non-empty', Length(Expr) > 0);
    AssertTrue('uses emg', Pos('emg', Expr) > 0);
    AssertTrue('no native ^ left', Pos('^', Expr) = 0);
    AssertTrue('names A', Pos('A', Expr) > 0);
    AssertTrue('names sigma', Pos('sigma', Expr) > 0);
    AssertTrue('names tau', Pos('tau', Expr) > 0);
    AssertTrue('names x0', Pos('x0', Expr) > 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TEmgTest);
end.
