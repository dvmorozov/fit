// SPDX-License-Identifier: GPL-3.0-or-later
{ Golden-oracle + analytic-limit tests for the Moffat lineshape, computed via the
  native expression engine the Python sidecar mirrors. Pins: golden values match
  numpy; peak = A; and the m -> 1 limit is a Lorentzian with sigma as half-width. }
unit testcase_moffat;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  points_set, moffat_points_set;
type
  TMoffatTest = class(TTestCase)
  private
    function Moffat(A, sigma, m, x: double): double;
    function LorentzHalfWidth(A, sigma, x: double): double;
  published
    procedure PeakEqualsAmplitude;
    procedure GoldenValuesMatchNumpy;
    procedure LimitMEquals1IsLorentzian;
    procedure ExpressionIsNumpyAndNamesParameters;
  end;

implementation

const
  X0  = 10.0;
  EPS = 1e-6;

function TMoffatTest.Moffat(A, sigma, m, x: double): double;
var
  C: TMoffatPointsSet;
begin
  C := TMoffatPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    C.ValuesByName['x0'] := X0;
    C.ValuesByName['A'] := A;
    C.ValuesByName['sigma'] := sigma;
    C.ValuesByName['m'] := m;
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

function TMoffatTest.LorentzHalfWidth(A, sigma, x: double): double;
begin
  Result := A / (1 + Sqr((x - X0) / sigma));
end;

procedure TMoffatTest.PeakEqualsAmplitude;
begin
  AssertEquals('peak = A', 100.0, Moffat(100, 2, 2.5, X0), EPS);
end;

procedure TMoffatTest.GoldenValuesMatchNumpy;
begin
  AssertEquals('golden 1', 17.677669530, Moffat(100, 2, 2.5, 12), 1e-6);
  AssertEquals('golden 2', 51.252613883, Moffat(80, 1.5, 3, 10.6), 1e-6);
  AssertEquals('golden 3', 85.912040051, Moffat(120, 3, 1.2, 8.3), 1e-6);
end;

procedure TMoffatTest.LimitMEquals1IsLorentzian;
var
  x: double;
begin
  x := 9.0;
  while x <= 11.0 do
  begin
    AssertEquals('Moffat(m=1) = Lorentzian at x=' + FloatToStr(x),
      LorentzHalfWidth(100, 2, x), Moffat(100, 2, 1, x), 1e-6);
    x := x + 0.25;
  end;
end;

procedure TMoffatTest.ExpressionIsNumpyAndNamesParameters;
var
  C: TMoffatPointsSet;
  Expr: string;
begin
  C := TMoffatPointsSet.Create(nil, X0);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('non-empty', Length(Expr) > 0);
    AssertTrue('power translated to **', Pos('**', Expr) > 0);
    AssertTrue('no native ^ left', Pos('^', Expr) = 0);
    AssertTrue('names A', Pos('A', Expr) > 0);
    AssertTrue('names sigma', Pos('sigma', Expr) > 0);
    AssertTrue('names m', Pos('m', Expr) > 0);
    AssertTrue('names x0', Pos('x0', Expr) > 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TMoffatTest);
end.
