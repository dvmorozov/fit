// SPDX-License-Identifier: GPL-3.0-or-later
{ Golden-oracle + asymptote tests for the error-function step, computed via the
  native engine (erf, mirrored by scipy in the sidecar). Pins golden numpy values,
  the edge midpoint (A/2 at x0), and the plateaus (0 far left, A far right). }
unit testcase_step;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  points_set, step_points_set;
type
  TStepTest = class(TTestCase)
  private
    function Step(A, sigma, x: double): double;
  published
    procedure GoldenValuesMatchNumpy;
    procedure MidpointAndPlateaus;
    procedure ExpressionIsNumpyAndNamesParameters;
  end;

implementation

const
  X0 = 10.0;

function TStepTest.Step(A, sigma, x: double): double;
var
  C: TStepPointsSet;
begin
  C := TStepPointsSet.Create(nil, X0);
  try
    C.AddNewPoint(x, 0);
    C.ValuesByName['x0'] := X0;
    C.ValuesByName['A'] := A;
    C.ValuesByName['sigma'] := sigma;
    C.ReCalc;
    Result := C.PointYCoord[0];
  finally
    C.Free;
  end;
end;

procedure TStepTest.GoldenValuesMatchNumpy;
begin
  AssertEquals('golden 1', 97.724986805, Step(100, 0.5, 11.0), 1e-6);
  AssertEquals('golden 2', 2.275013195, Step(100, 0.5, 9.0), 1e-6);
  AssertEquals('golden 3', 49.432913775, Step(80, 1.0, 10.3), 1e-6);
end;

procedure TStepTest.MidpointAndPlateaus;
begin
  AssertEquals('midpoint A/2 at x0', 50.0, Step(100, 0.5, X0), 1e-9);
  AssertEquals('right plateau -> A', 100.0, Step(100, 0.5, X0 + 10), 1e-9);
  AssertEquals('left plateau -> 0', 0.0, Step(100, 0.5, X0 - 10), 1e-9);
end;

procedure TStepTest.ExpressionIsNumpyAndNamesParameters;
var
  C: TStepPointsSet;
  Expr: string;
begin
  C := TStepPointsSet.Create(nil, X0);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('non-empty', Length(Expr) > 0);
    AssertTrue('uses erf', Pos('erf', Expr) > 0);
    AssertTrue('names A', Pos('A', Expr) > 0);
    AssertTrue('names sigma', Pos('sigma', Expr) > 0);
    AssertTrue('names x0', Pos('x0', Expr) > 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest('unit', TStepTest);
end.
