// SPDX-License-Identifier: GPL-3.0-or-later
unit testcase_fit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  Math, fit_task, points_set, gauss_points_set, curve_types_singleton,
  int_curve_type_selector, int_fit_service, int_fit_backend, native_fit_backend,
  SimpMath;
type
  TCB = class
    procedure NoOp;
  end;
  TFitTest = class(TTestCase)
  private
    { Fits one synthetic Gaussian with the given minimizer kind, returns R-factor. }
    function RunGaussianFit(AMinimizerKind: longint): double;
    { Builds a task with a synthetic Gaussian peak ready to optimize. Caller frees
      the task; Profile/Positions are owned by it. CB must outlive the task.
      AtEveryPoint seeds a position on every sample instead of one on the peak. }
    function BuildGaussianTask(CB: TCB; AtEveryPoint: boolean = False): TFitTask;
  published
    procedure FitsSyntheticGaussian;
    procedure NativeBackendFitsAndReports;
    procedure PositionsAtEveryDataPointStillMakeOneCurveEach;
  end;
implementation

procedure TCB.NoOp; begin end;

function TFitTest.BuildGaussianTask(CB: TCB; AtEveryPoint: boolean): TFitTask;
var
  Profile, Positions: TPointsSet;
  Sel: ICurveTypeSelector;
  x: double;
  i: longint;
begin
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
  Sel.SelectCurveType(TGaussPointsSet.GetCurveTypeId);

  Profile := TPointsSet.Create(nil);
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    Profile.AddNewPoint(x, GaussPoint(100, 1.5, 10, x));
    x := x + 0.2;
  end;
  //  The position's y is the data height at that x - RecreateCurves seeds the
  //  curve's amplitude from it. Passing 0 (as this test used to) starts the fit
  //  from a zero-amplitude curve that never converges.
  Positions := TPointsSet.Create(nil);
  if AtEveryPoint then
    for i := 0 to Profile.PointsCount - 1 do
      Positions.AddNewPoint(Profile.PointXCoord[i], Profile.PointYCoord[i])
  else
    Positions.AddNewPoint(10, GaussPoint(100, 1.5, 10, 10));

  Result := TFitTask.Create(nil, False, False);
  Result.ServerShowCurMin := @CB.NoOp;
  Result.ServerDoneProc := @CB.NoOp;
  Result.SetProfilePointsSet(Profile);
  Result.SetCurvePositions(Positions);
  Result.RecreateCurves(nil);
  Result.BegIndex := 0;
  Result.EndIndex := Result.GetCalcProfile.PointsCount - 1;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end;

function TFitTest.RunGaussianFit(AMinimizerKind: longint): double;
var
  Task: TFitTask;
  CB: TCB;
begin
  CB := TCB.Create;
  Task := BuildGaussianTask(CB);
  try
    Task.MinimizerKind := AMinimizerKind;
    Task.MinimizeDifference;
    Result := Task.GetCurMin;
  finally
    Task.Free;
    CB.Free;
  end;
end;

procedure TFitTest.FitsSyntheticGaussian;
var
  r: double;
begin
  //  The default Downhill Simplex kind (the only one exposed today) reproduces the
  //  original baseline - also exercises the minimizer-selection seam end to end.
  r := RunGaussianFit(MIN_KIND_DHS);
  AssertTrue('R-factor genuinely small after fitting a matching peak (' + FloatToStr(r) + ')',
    (r > 0) and (r < 0.05));
end;

procedure TFitTest.NativeBackendFitsAndReports;
var
  Task: TFitTask;
  CB: TCB;
  Backend: IFitBackend;
  R: TFitResult;
begin
  //  The native backend performs one whole fit through the IFitBackend seam and
  //  reports the outcome - this is the contract Stage 2's worker/sidecar backends
  //  will implement.
  CB := TCB.Create;
  Task := BuildGaussianTask(CB);
  try
    Backend := TNativeFitBackend.Create;
    AssertTrue('backend has a name', Backend.Name <> '');
    R := Backend.Fit(Task);
    AssertEquals('backend success code', 0, R.ErrorCode);
    //  Must be strictly > 0: a 0 R-factor means the optimizer never ran.
    AssertTrue('backend reports a genuine small R-factor (' + FloatToStr(R.RFactor) + ')',
      (R.RFactor > 0) and (R.RFactor < 0.05));
  finally
    Task.Free;
    CB.Free;
  end;
end;

{ ONE CURVE PER DATA POINT IS THE ORDINARY STRATEGY FOR A PEAK TYPE, not an
  accident: the automatic path seeds a curve on every sample, the amplitudes come
  from the data and the unwanted ones are then pruned away. Only a type placed
  from its own POINT SET rejects that reading, and the refusal and the
  position-list rewrite that came with it are both keyed on
  TNamedPointsSet.PlacedByPointSet, which a peak leaves empty.

  So this states what a Gaussian must go on doing: every position becomes an
  instance, and the position list is the user's, unchanged by the build. }
procedure TFitTest.PositionsAtEveryDataPointStillMakeOneCurveEach;
var
  Task: TFitTask;
  CB: TCB;
  Count: longint;
begin
  CB := TCB.Create;
  Task := BuildGaussianTask(CB, True);
  try
    Count := Task.ProfilePoints.PointsCount;
    AssertTrue('the synthetic profile has points to seed from', Count > 1);
    AssertEquals('one curve per position', Count, Task.GetCurves.Count);
    AssertEquals('and the positions are still the ones that were set',
      Count, Task.GetCurvePositions.PointsCount);
  finally
    Task.Free;
    CB.Free;
  end;
end;

initialization
  RegisterTest('integration', TFitTest);
end.
