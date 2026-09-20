// SPDX-License-Identifier: GPL-3.0-or-later
{ Golden-oracle + analytic-limit tests for the Doniach-Sunjic lineshape, computed
  via the native expression engine the Python sidecar mirrors. Pins the golden
  numpy values and the alpha -> 0 limit (a Lorentzian with sigma as half-width). }
unit testcase_doniach_sunjic;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  points_set, doniach_sunjic_points_set;
type
  TDoniachSunjicTest = class(TTestCase)
  private
    function DS(A, sigma, alpha, x: double): double;
    function LorentzHalfWidth(A, sigma, x: double): double;
  published
    procedure GoldenValuesMatchNumpy;
    procedure LimitAlpha0IsLorentzian;
    procedure ExpressionIsNumpyAndNamesParameters;
  end;

implementation

const
  X0  = 10.0;

function TDoniachSunjicTest.DS(A, sigma, alpha, x: double): double;
var
  C: TDoniachSunjicPointsSet;
begin
  C := TDoniachSunjicPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    C.ValuesByName['x0'] := X0;
    C.ValuesByName['A'] := A;
    C.ValuesByName['sigma'] := sigma;
    C.ValuesByName['alpha'] := alpha;
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

function TDoniachSunjicTest.LorentzHalfWidth(A, sigma, x: double): double;
begin
  //  alpha = 0 limit: A*sigma/(sigma^2+(x-x0)^2).
  Result := A * sigma / (Sqr(sigma) + Sqr(x - X0));
end;

procedure TDoniachSunjicTest.GoldenValuesMatchNumpy;
begin
  AssertEquals('golden 1', 54.623852789, DS(100, 2, 0.2, 10), 1e-6);
  AssertEquals('golden 2', 32.461239725, DS(100, 2, 0.2, 11.5), 1e-6);
  AssertEquals('golden 3', 38.755707538, DS(80, 1.5, 0.35, 10.7), 1e-6);
end;

procedure TDoniachSunjicTest.LimitAlpha0IsLorentzian;
var
  x: double;
begin
  x := 8.0;
  while x <= 12.0 do
  begin
    AssertEquals('DS(alpha=0) = Lorentzian at x=' + FloatToStr(x),
      LorentzHalfWidth(100, 2, x), DS(100, 2, 0, x), 1e-6);
    x := x + 0.5;
  end;
end;

procedure TDoniachSunjicTest.ExpressionIsNumpyAndNamesParameters;
var
  C: TDoniachSunjicPointsSet;
  Expr: string;
begin
  C := TDoniachSunjicPointsSet.Create(nil, X0);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('non-empty', Length(Expr) > 0);
    AssertTrue('power translated to **', Pos('**', Expr) > 0);
    AssertTrue('no native ^ left', Pos('^', Expr) = 0);
    AssertTrue('names A', Pos('A', Expr) > 0);
    AssertTrue('names sigma', Pos('sigma', Expr) > 0);
    AssertTrue('names alpha', Pos('alpha', Expr) > 0);
    AssertTrue('names x0', Pos('x0', Expr) > 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TDoniachSunjicTest);
end.
