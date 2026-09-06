// SPDX-License-Identifier: GPL-3.0-or-later
{ Golden-oracle + analytic-limit tests for the true Voigt lineshape, computed via
  the native Faddeeva (special_functions.VoigtProfile, mirrored by scipy's
  voigt_profile in the sidecar). Pins golden numpy values and both limits:
  gamma -> 0 gives a Gaussian, sigma -> 0 gives a Lorentzian (checkpoint G3). }
unit testcase_voigt;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry,
  points_set, voigt_points_set;
type
  TVoigtTest = class(TTestCase)
  private
    function Voigt(A, sigma, gamma, x: double): double;
    function Gauss(A, sigma, x: double): double;
    function LorentzArea(A, gamma, x: double): double;
  published
    procedure GoldenValuesMatchNumpy;
    procedure LimitGammaToZeroIsGaussian;
    procedure LimitSigmaToZeroIsLorentzian;
    procedure ExpressionIsNumpyAndNamesParameters;
  end;

implementation

const
  X0 = 10.0;

function TVoigtTest.Voigt(A, sigma, gamma, x: double): double;
var
  C: TVoigtPointsSet;
begin
  C := TVoigtPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    C.ValuesByName['x0'] := X0;
    C.ValuesByName['A'] := A;
    C.ValuesByName['sigma'] := sigma;
    C.ValuesByName['gamma'] := gamma;
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

function TVoigtTest.Gauss(A, sigma, x: double): double;
begin
  Result := A / (sigma * Sqrt(2 * Pi)) * Exp(-Sqr(x - X0) / (2 * Sqr(sigma)));
end;

function TVoigtTest.LorentzArea(A, gamma, x: double): double;
begin
  //  Area-normalised Lorentzian, HWHM = gamma.
  Result := A * gamma / (Pi * (Sqr(x - X0) + Sqr(gamma)));
end;

procedure TVoigtTest.GoldenValuesMatchNumpy;
begin
  AssertEquals('golden peak', 27.895547039, Voigt(100, 1.0, 0.5, 10), 1e-6);
  AssertEquals('golden 2', 13.543427630, Voigt(100, 1.0, 0.5, 11.5), 1e-6);
  AssertEquals('golden 3', 6.692777644, Voigt(80, 2.0, 1.0, 12.5), 1e-6);
end;

procedure TVoigtTest.LimitGammaToZeroIsGaussian;
var
  x: double;
begin
  x := 8.0;
  while x <= 12.0 do
  begin
    AssertEquals('Voigt(gamma->0) = Gaussian at x=' + FloatToStr(x),
      Gauss(100, 1.2, x), Voigt(100, 1.2, 1e-7, x), 1e-4);
    x := x + 0.5;
  end;
end;

procedure TVoigtTest.LimitSigmaToZeroIsLorentzian;
var
  x: double;
begin
  x := 8.0;
  while x <= 12.0 do
  begin
    AssertEquals('Voigt(sigma->0) = Lorentzian at x=' + FloatToStr(x),
      LorentzArea(100, 0.8, x), Voigt(100, 1e-5, 0.8, x), 1e-4);
    x := x + 0.5;
  end;
end;

procedure TVoigtTest.ExpressionIsNumpyAndNamesParameters;
var
  C: TVoigtPointsSet;
  Expr: string;
begin
  C := TVoigtPointsSet.Create(nil, X0);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('non-empty', Length(Expr) > 0);
    AssertTrue('uses voigt', Pos('voigt', Expr) > 0);
    AssertTrue('names A', Pos('A', Expr) > 0);
    AssertTrue('names sigma', Pos('sigma', Expr) > 0);
    AssertTrue('names gamma', Pos('gamma', Expr) > 0);
    AssertTrue('names x0', Pos('x0', Expr) > 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TVoigtTest);
end.
