// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_curve_class;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, curve_points_set,
  gauss_points_set;
type
  TCurveClassTest = class(TTestCase)
  published
    procedure GaussianClassEvaluatesViaParams;
  end;

  { THE STEP SIZE, ADDRESSED BY INDEX, which is how the native simplex reaches
    it: the optimiser knows a curve as a count of varied values and asks about
    each by number, never by name. Three of the four routines that answer had no
    test - a curve could report a step for the wrong parameter, or report that
    the minimum step was reached when it was not, and the fit would stop early or
    wander without anything failing.

    A GAUSSIAN VARIES THREE THINGS, so an off-by-one is not caught by an index
    that happens to be in range. These walk every index and state the relation
    to the parameter's own name, which is the only thing that ties the
    optimiser's numbering to the model. }
  TCurveVariationStepTest = class(TTestCase)
  private
    FCurve: TCurvePointsSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ACurveKnowsHowManyValuesItVaries;
    procedure EveryIndexNamesADistinctParameter;
    procedure EveryStepStartsPositiveOnceInitialised;
    procedure AStepSetByIndexComesBackFromThatIndex;
    procedure AndDoesNotDisturbItsNeighbours;
    procedure TheMinimumStepIsNotAchievedAtTheInitialStep;
    procedure ButIsOnceTheStepHasBeenDrivenDown;
  end;

implementation
procedure TCurveClassTest.GaussianClassEvaluatesViaParams;
const AREA = 5.0; X0 = 10.0; SIG = 2.0;
var G: TGaussPointsSet; i, PeakIdx: longint; x: double;
begin
  G := TGaussPointsSet.Create(nil, X0);
  try
    G.A := AREA; G.ValuesByName['sigma'] := SIG; G.x0 := X0;
    x := 0.0;
    while x <= 20.0 + 1e-9 do begin G.AddNewPoint(x, 0.0); x := x + 0.5; end;
    G.ReCalc;
    PeakIdx := -1;
    for i := 0 to G.PointsCount - 1 do
      if Abs(G.PointXCoord[i] - X0) < 1e-9 then PeakIdx := i;
    AssertTrue('peak present', PeakIdx >= 0);
    AssertEquals('gauss class peak', 0.9973557, G.PointYCoord[PeakIdx], 1e-6);
    AssertEquals('symmetry', G.PointYCoord[PeakIdx-4], G.PointYCoord[PeakIdx+4], 1e-9);
  finally G.Free; end;
end;
{ ---------------------- the step size, addressed by index ------------------- }

procedure TCurveVariationStepTest.SetUp;
begin
  FCurve := TGaussPointsSet.Create(nil, 10.0);
end;

procedure TCurveVariationStepTest.TearDown;
begin
  FreeAndNil(FCurve);
end;

procedure TCurveVariationStepTest.ACurveKnowsHowManyValuesItVaries;
begin
  //  Guards the walks below rather than the code: a curve reporting nothing
  //  would make every one of them vacuous, and they would all still pass.
  AssertTrue('a Gaussian varies something', FCurve.VariableCount > 0);
  AssertEquals('amplitude, position and width', 3, FCurve.VariableCount);
end;

procedure TCurveVariationStepTest.EveryIndexNamesADistinctParameter;
var
  i, j: longint;
begin
  //  THE NUMBERING IS THE ONLY THING JOINING the optimiser to the model. Two
  //  indices naming one parameter means the simplex varies it twice and never
  //  varies the other - a fit that converges, on a model with one parameter
  //  frozen at its seed, reporting nothing.
  for i := 0 to FCurve.VariableCount - 1 do
  begin
    AssertTrue(Format('index %d is named', [i]),
      FCurve.VariableNames[i] <> '');
    for j := i + 1 to FCurve.VariableCount - 1 do
      AssertTrue(Format('indices %d and %d name different parameters (%s)',
        [i, j, FCurve.VariableNames[i]]),
        FCurve.VariableNames[i] <> FCurve.VariableNames[j]);
  end;
end;

procedure TCurveVariationStepTest.EveryStepStartsPositiveOnceInitialised;
var
  i: longint;
begin
  //  A ZERO STEP IS A FROZEN PARAMETER. The simplex moves a value by its step,
  //  so one left at zero is varied by nothing - the fit runs, converges on the
  //  remaining axes, and the frozen one keeps its seed. That is the failure
  //  mode the calculated-parameter policy uses ON PURPOSE, which is exactly why
  //  a fitted parameter must not share it by accident.
  for i := 0 to FCurve.VariableCount - 1 do
  begin
    FCurve.InitVariationStep(i);
    AssertTrue(Format('%s has a step to move by',
      [FCurve.VariableNames[i]]), FCurve.VariationSteps[i] > 0);
  end;
end;

procedure TCurveVariationStepTest.AStepSetByIndexComesBackFromThatIndex;
var
  i: longint;
begin
  //  Round-tripped per index, with a different value at each so a getter
  //  reading a fixed index would answer the wrong number rather than a
  //  plausible one.
  for i := 0 to FCurve.VariableCount - 1 do
    FCurve.VariationSteps[i] := 0.125 * (i + 1);
  for i := 0 to FCurve.VariableCount - 1 do
    AssertEquals(Format('step of %s', [FCurve.VariableNames[i]]),
      0.125 * (i + 1), FCurve.VariationSteps[i], 1e-12);
end;

procedure TCurveVariationStepTest.AndDoesNotDisturbItsNeighbours;
var
  i: longint;
  Before: array of double;
begin
  //  ONE INDEX AT A TIME. A setter writing through a shared parameter object -
  //  or through the wrong container - would move every step together, and the
  //  simplex would take equal steps on quantities measured in different units:
  //  a width and a position that move by the same amount fit nothing.
  for i := 0 to FCurve.VariableCount - 1 do
    FCurve.InitVariationStep(i);
  SetLength(Before, FCurve.VariableCount);
  for i := 0 to FCurve.VariableCount - 1 do
    Before[i] := FCurve.VariationSteps[i];

  FCurve.VariationSteps[1] := Before[1] * 0.5;

  for i := 0 to FCurve.VariableCount - 1 do
    if i <> 1 then
      AssertEquals(Format('%s was left alone', [FCurve.VariableNames[i]]),
        Before[i], FCurve.VariationSteps[i], 1e-12);
  AssertEquals('and the one that was set did change',
    Before[1] * 0.5, FCurve.VariationSteps[1], 1e-12);
end;

procedure TCurveVariationStepTest.TheMinimumStepIsNotAchievedAtTheInitialStep;
var
  i: longint;
begin
  //  THIS IS THE SIMPLEX'S STOPPING CONDITION. Answered True too early and the
  //  fit stops at its seed and reports success; the R-factor is whatever the
  //  seed gives, which on a well-placed peak is not obviously wrong.
  for i := 0 to FCurve.VariableCount - 1 do
  begin
    FCurve.InitVariationStep(i);
    AssertFalse(Format('%s has further to go', [FCurve.VariableNames[i]]),
      FCurve.MinimumStepAchieved(i));
  end;
end;

procedure TCurveVariationStepTest.ButIsOnceTheStepHasBeenDrivenDown;
var
  i: longint;
begin
  //  AND ANSWERED FALSE FOREVER, the fit never terminates on that parameter -
  //  so both directions are asserted, per index. Driven down by a factor no
  //  plausible threshold survives rather than to the threshold itself, which
  //  would restate a constant this test has no business knowing.
  for i := 0 to FCurve.VariableCount - 1 do
  begin
    FCurve.InitVariationStep(i);
    FCurve.VariationSteps[i] := FCurve.VariationSteps[i] * 1e-12;
    AssertTrue(Format('%s has arrived', [FCurve.VariableNames[i]]),
      FCurve.MinimumStepAchieved(i));
  end;
end;

initialization
  RegisterTest('unit', TCurveClassTest);
  RegisterTest('unit', TCurveVariationStepTest);
end.
