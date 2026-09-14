// SPDX-License-Identifier: GPL-3.0-or-later
{ Golden-oracle + analytic-limit tests for the skewed Gaussian, computed via the
  native engine (erf, mirrored by scipy in the sidecar). Pins golden numpy values
  and the beta -> 0 limit (the area-normalised Gaussian). }
unit testcase_skewed_gaussian;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry,
  points_set, skewed_gaussian_points_set;
type
  TSkewedGaussianTest = class(TTestCase)
  private
    function Skg(A, sigma, beta, x: double): double;
    function Gauss(A, sigma, x: double): double;
  published
    procedure GoldenValuesMatchNumpy;
    procedure LimitBetaToZeroIsGaussian;
    procedure ExpressionIsNumpyAndNamesParameters;
  end;

implementation

const
  X0 = 10.0;

function TSkewedGaussianTest.Skg(A, sigma, beta, x: double): double;
var
  C: TSkewedGaussianPointsSet;
begin
  C := TSkewedGaussianPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    C.ValuesByName['x0'] := X0;
    C.ValuesByName['A'] := A;
    C.ValuesByName['sigma'] := sigma;
    C.ValuesByName['beta'] := beta;
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

function TSkewedGaussianTest.Gauss(A, sigma, x: double): double;
begin
  Result := A / (sigma * Sqrt(2 * Pi)) * Exp(-Sqr(x - X0) / (2 * Sqr(sigma)));
end;

procedure TSkewedGaussianTest.GoldenValuesMatchNumpy;
begin
  AssertEquals('golden 1', 57.407469775, Skg(100, 1.0, 2.0, 10.7), 1e-6);
  AssertEquals('golden 2', 25.382641311, Skg(80, 1.5, -1.0, 9.5), 1e-6);
end;

procedure TSkewedGaussianTest.LimitBetaToZeroIsGaussian;
var
  x: double;
begin
  x := 8.0;
  while x <= 12.0 do
  begin
    AssertEquals('SkewedGaussian(beta->0) = Gaussian at x=' + FloatToStr(x),
      Gauss(100, 1.2, x), Skg(100, 1.2, 1e-9, x), 1e-6);
    x := x + 0.25;
  end;
end;

procedure TSkewedGaussianTest.ExpressionIsNumpyAndNamesParameters;
var
  C: TSkewedGaussianPointsSet;
  Expr: string;
begin
  C := TSkewedGaussianPointsSet.Create(nil, X0);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('non-empty', Length(Expr) > 0);
    AssertTrue('uses erf', Pos('erf', Expr) > 0);
    AssertTrue('power translated to **', Pos('**', Expr) > 0);
    AssertTrue('no native ^ left', Pos('^', Expr) = 0);
    AssertTrue('names A', Pos('A', Expr) > 0);
    AssertTrue('names sigma', Pos('sigma', Expr) > 0);
    AssertTrue('names beta', Pos('beta', Expr) > 0);
    AssertTrue('names x0', Pos('x0', Expr) > 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TSkewedGaussianTest);
end.
