// SPDX-License-Identifier: GPL-3.0-or-later
{ The example module's tests.

  They live with the module, not with the framework's suite, and they run only
  when this directory is on the suite's search path - the same one-entry
  difference that puts the module into the application. That is the point they
  exist to demonstrate as much as the curve is.

  What they check is what a module OWES the framework: the shape it computes, the
  agreement between that shape and the expression the remote backends evaluate,
  and the fact that registering the module really does make the type creatable. }
unit testcase_linear_ramp;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry,
  linear_points_set, example_module,
  named_points_set, curve_types_singleton, int_curve_factory,
  special_curve_parameter;

type
  TLinearRampTest = class(TTestCase)
  private
    { The curve's value at AX, for the given parameters. }
    function Ramp(A, sigma, alpha, AX: double): double;
  published
    procedure TheRightHandEndIsTheAmplitude;
    procedure ItFallsAwayWithTheSlope;
    procedure OutsideItsSupportItIsZero;
    procedure TheExpressionAgreesWithWhatItComputes;
    procedure ItNamesEveryParameterInItsExpression;
    procedure RegisteringTheModuleMakesTheTypeCreatable;
  end;

implementation

const
  X0  = 10.0;
  EPS = 1e-9;

function TLinearRampTest.Ramp(A, sigma, alpha, AX: double): double;
var
  C: TLinearPointsSet;
begin
  C := TLinearPointsSet.Create(nil);
  try
    C.AddNewPoint(AX, 0);
    //  The position is set by the application after placement - the constructor
    //  only fixes its boundaries - so the test does what the application does.
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

procedure TLinearRampTest.TheRightHandEndIsTheAmplitude;
begin
  //  At x0 the slope term vanishes, so A is the value there - which is what
  //  makes A meaningful to a user reading the parameters grid.
  AssertEquals('value at x0', 5.0, Ramp(5, 4, 2, X0), EPS);
end;

procedure TLinearRampTest.ItFallsAwayWithTheSlope;
begin
  //  Two units to the left of x0 with slope 2: A - 4.
  AssertEquals('value inside the support', 1.0, Ramp(5, 4, 2, X0 - 2), EPS);
end;

procedure TLinearRampTest.OutsideItsSupportItIsZero;
begin
  //  Zero on BOTH sides, and exactly zero rather than a small number: several
  //  ramps sum into one profile, so a curve that leaked outside its support
  //  would silently bias every fit that contains it.
  AssertEquals('beyond the right end', 0.0, Ramp(5, 4, 2, X0 + 0.5), EPS);
  AssertEquals('beyond the left end', 0.0, Ramp(5, 4, 2, X0 - 4.5), EPS);
end;

procedure TLinearRampTest.TheExpressionAgreesWithWhatItComputes;
var
  C: TLinearPointsSet;
  Expr: string;
begin
  //  The expression is what the Python sidecar and a remote compute server fit
  //  with; DoCalc is what fits here. If they disagree, the SAME model gives
  //  different answers depending on which engine ran it - and both look
  //  plausible, which is why this is asserted rather than reviewed.
  //
  //  The Pascal side can only check the expression's SHAPE: it must be a
  //  piecewise form bounded by the same support DoCalc uses. The numeric
  //  agreement is proved end-to-end, native against Python, by the fidelity
  //  suite the framework already runs.
  C := TLinearPointsSet.Create(nil);
  try
    Expr := C.GetCurveExpression;
    AssertTrue('the expression is piecewise, as the curve is',
      Pos('where', Expr) > 0);
    AssertTrue('and bounded by the same support',
      (Pos('x <= x0', Expr) > 0) and (Pos('x0 - sigma', Expr) > 0));
  finally
    C.Free;
  end;
end;

procedure TLinearRampTest.ItNamesEveryParameterInItsExpression;
var
  C: TLinearPointsSet;
  Expr, PName: string;
  j: longint;
begin
  //  A parameter the expression does not mention is one a formula-based backend
  //  cannot vary, so the fit would quietly hold it at its seed.
  C := TLinearPointsSet.Create(nil);
  try
    Expr := C.GetCurveExpression;
    for j := 0 to C.Parameters.Count - 1 do
    begin
      if C.Parameters[j].Type_ = Argument then
        Continue;
      PName := C.Parameters[j].Name;
      AssertTrue('the expression names ' + PName, Pos(PName, Expr) > 0);
    end;
  finally
    C.Free;
  end;
end;

procedure TLinearRampTest.RegisteringTheModuleMakesTheTypeCreatable;
var
  Cls: TCurveClass;
begin
  //  The whole contract in one assertion: after the front door has run, the
  //  framework resolves this type from its id alone - knowing nothing about it
  //  beyond what it registered. The front door also RAISES here if the unit did
  //  not link, so this covers that too.
  RegisterExampleModule;
  Cls := FindCurveClassById(TLinearPointsSet.GetCurveTypeId);
  AssertTrue('the registry resolved the id', Assigned(Cls));
  AssertEquals('and it is this module''s type', 'Linear ramp',
    Cls.GetCurveTypeName);
end;

initialization
  //  CLASSIFIED, like every test in the framework's own suite. It calls the
  //  module's front door and asks the registry a question - no process, no file,
  //  no fit - so it is a unit test.
  //
  //  This mattered more than it looks. Unclassified, it would fail
  //  testcase_suite_split the day anyone built fit_tests_example.lpi, and until
  //  then it would fail nothing at all - while being the example a module author
  //  copies. An unclassified test does not error; it quietly vanishes from
  //  --suite=unit, which is the half line coverage is measured over.
  RegisterTest('unit', TLinearRampTest);
end.
