// SPDX-License-Identifier: GPL-3.0-or-later
{ Cross-process curve×minimizer matrix: fit every curve type through BOTH the
  native engine (in-process) and the Python sidecar (spawned), driving the real
  Pascal→sidecar→Pascal path (TPythonFitBackend), and assert the two agree. This
  is the e2e guard the earlier runtime bugs (unsupported curve type, M<N) slipped
  past. When the sidecar venv is absent the Python cases are Ignore()d - counted
  and reported, never a silent pass. }
unit testcase_python_backend_process;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  fit_task, fit_problem_json, fit_task_marshalling, test_fit_quality,
  python_sidecar, python_fit_backend, int_fit_backend,
  points_set, curve_types_singleton, int_curve_type_selector,
  special_curve_parameter, persistent_curve_parameters,
  persistent_curve_parameter_container, user_curve_parameter,
  gauss_points_set, lorentz_points_set, pseudo_voigt_points_set,
  asym_pseudo_voigt_points_set, two_branches_pseudo_voigt_points_set,
  pearson7_points_set, moffat_points_set, doniach_sunjic_points_set,
  emg_points_set, voigt_points_set, skewed_gaussian_points_set,
  user_points_set, SimpMath;

type
  TPyCB = class
    procedure NoOp;
  end;

  TPythonBackendProcessTest = class(TTestCase)
  private
    FCB: TPyCB;
    FSidecar: TPythonSidecar;
    function BuiltinTask(const TypeId: TGuid): TFitTask;
    function UserTask: TFitTask;
    function FittedX0(Task: TFitTask): double;
    function EnsureSidecarOrIgnore: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NativeFitsEveryCurveType;
    procedure PythonFitsEveryCurveType;
    procedure PythonAndNativeAgreeOnGaussian;
    procedure PythonQualityIsNoWorseThanNative;
    procedure PythonFitsMoreParametersThanPoints;
  end;

const
  PEAK = 10.0;
  { How much worse than the native engine the gradient minimizer may be before the
    comparison fails. Slack for ordinary solver differences, not for a regression. }
  QUALITY_TOLERANCE = 1.25;

implementation

procedure TPyCB.NoOp; begin end;

procedure TPythonBackendProcessTest.SetUp;
begin
  FCB := TPyCB.Create;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
end;

procedure TPythonBackendProcessTest.TearDown;
begin
  FSidecar.Free;   //  stops the spawned worker, if any test started it
  FSidecar := nil;
  FCB.Free;
end;

{ A synthetic peak at x=PEAK that any peak curve can fit. A small deterministic
  ripple is added so that no curve type reproduces the data exactly: without it a
  Gaussian curve fits Gaussian data to machine zero and a quality comparison between
  the two engines would be 0 <= 0, i.e. vacuous. Deterministic (no RNG) so the tests
  are reproducible. }
procedure FillGaussianArrays(out X, Y: TDoubleArray);
var v: double; n: integer;
begin
  n := 0; v := 0;
  while v <= 20 + 1e-9 do
  begin
    SetLength(X, n + 1); SetLength(Y, n + 1);
    X[n] := v;
    Y[n] := GaussPoint(100, 1.5, PEAK, v) + 0.5 * Sin(3.0 * n);
    Inc(n); v := v + 0.2;
  end;
end;

function TPythonBackendProcessTest.BuiltinTask(const TypeId: TGuid): TFitTask;
var P: TFitProblem;
begin
  P := Default(TFitProblem);
  FillGaussianArrays(P.ProfileX, P.ProfileY);
  P.PositionsX := TDoubleArray.Create(PEAK);
  P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, PEAK, PEAK));
  P.CurveTypeId := GUIDToString(TypeId);
  P.MinimizerKind := 0;
  Result := BuildTaskFromProblem(P);
end;

{ A user curve equivalent to a Gaussian, built like the desktop does. }
function TPythonBackendProcessTest.UserTask: TFitTask;
var
  Params: Curve_parameters;
  Cont: TPersistentCurveParameterContainer;
  Prm: TUserCurveParameter;
  Profile, Positions: TPointsSet;

  procedure Add(const AName: string; AType: TParameterType; AValue: double);
  begin
    Prm := TUserCurveParameter.Create;
    Prm.Name := AName; Prm.Type_ := AType; Prm.Value := AValue;
    Cont := TPersistentCurveParameterContainer(Params.Params.Add);
    Cont.Parameter := Prm;
  end;

var v: double;
begin
  Params := Curve_parameters.Create(nil);
  Params.Params.Clear;
  Add('A', Variable, 0);
  Add('x', Argument, 0);
  Add('x0', InvariablePosition, 0);
  Add('SIGMA', Variable, 0.25);

  Profile := TPointsSet.Create(nil);
  v := 0;
  while v <= 20 + 1e-9 do
  begin
    Profile.AddNewPoint(v, GaussPoint(100, 1.5, PEAK, v));
    v := v + 0.2;
  end;
  Positions := TPointsSet.Create(nil);
  Positions.AddNewPoint(PEAK, 0);

  Result := TFitTask.Create(nil, False, False);
  Result.ServerShowCurMin := @FCB.NoOp;
  Result.ServerDoneProc := @FCB.NoOp;
  Result.SetSpecialCurve('A*exp(-((x-x0)/SIGMA)^2)', Params);
  Result.SetProfilePointsSet(Profile);
  Result.SetCurvePositions(Positions);
  Result.RecreateCurves(nil);
  Result.BegIndex := 0;
  Result.EndIndex := Result.GetCalcProfile.PointsCount - 1;
end;

function TPythonBackendProcessTest.FittedX0(Task: TFitTask): double;
var O: TFitOutcome; j: integer;
begin
  Result := -1;
  O := ReadOutcomeFromTask(Task);
  if Length(O.Curves) = 0 then Exit;
  for j := 0 to High(O.Curves[0].Params) do
    if O.Curves[0].Params[j].Name = 'x0' then
      Exit(O.Curves[0].Params[j].Value);
end;

{ Starts the sidecar, or Ignore()s the test (counted + reported) if the venv is
  not installed. Returns the sidecar URL. }
function TPythonBackendProcessTest.EnsureSidecarOrIgnore: string;
begin
  if FSidecar = nil then
    FSidecar := TPythonSidecar.Create;
  if not FSidecar.IsConfigured then
    Ignore('Python sidecar venv not installed - see docs/user-guide/building-from-source.md');
  //  Kept alive until TearDown: freeing it stops the spawned worker.
  Result := FSidecar.EnsureRunning;
  if Result = '' then
    Ignore('Python sidecar could not be started (missing numpy/scipy/lmfit?)');
end;

procedure TPythonBackendProcessTest.NativeFitsEveryCurveType;

  procedure FitAndCheck(Task: TFitTask; const What: string);
  begin
    try
      Task.MinimizeDifference;
      AssertTrue(What + ': native fitted centre near peak (' +
        FloatToStr(FittedX0(Task)) + ')', Abs(FittedX0(Task) - PEAK) < 0.3);
    finally
      Task.Free;
    end;
  end;

begin
  FitAndCheck(BuiltinTask(TGaussPointsSet.GetCurveTypeId), 'Gaussian');
  FitAndCheck(BuiltinTask(TLorentzPointsSet.GetCurveTypeId), 'Lorentzian');
  FitAndCheck(BuiltinTask(TPseudoVoigtPointsSet.GetCurveTypeId), 'Pseudo-Voigt');
  FitAndCheck(BuiltinTask(TAsymPseudoVoigtPointsSet.GetCurveTypeId), 'Asym PV');
  FitAndCheck(BuiltinTask(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId), '2-br PV');
  FitAndCheck(BuiltinTask(TPearson7PointsSet.GetCurveTypeId), 'Pearson VII');
  FitAndCheck(BuiltinTask(TMoffatPointsSet.GetCurveTypeId), 'Moffat');
  FitAndCheck(BuiltinTask(TDoniachSunjicPointsSet.GetCurveTypeId), 'Doniach-Sunjic');
  FitAndCheck(BuiltinTask(TEmgPointsSet.GetCurveTypeId), 'EMG');
  FitAndCheck(BuiltinTask(TVoigtPointsSet.GetCurveTypeId), 'Voigt');
  FitAndCheck(BuiltinTask(TSkewedGaussianPointsSet.GetCurveTypeId), 'Skewed Gaussian');
  FitAndCheck(UserTask, 'User');
end;

procedure TPythonBackendProcessTest.PythonFitsEveryCurveType;
var Url: string;

  procedure FitAndCheck(Task: TFitTask; const What: string);
  var Backend: IFitBackend;
  begin
    try
      Backend := TPythonFitBackend.Create(Url);
      Backend.Fit(Task);
      AssertTrue(What + ': Python fitted centre near peak (' +
        FloatToStr(FittedX0(Task)) + ')', Abs(FittedX0(Task) - PEAK) < 0.3);
    finally
      Task.Free;
    end;
  end;

begin
  Url := EnsureSidecarOrIgnore;   //  Ignore()s and stops here if no venv.
  FitAndCheck(BuiltinTask(TGaussPointsSet.GetCurveTypeId), 'Gaussian');
  FitAndCheck(BuiltinTask(TLorentzPointsSet.GetCurveTypeId), 'Lorentzian');
  FitAndCheck(BuiltinTask(TPseudoVoigtPointsSet.GetCurveTypeId), 'Pseudo-Voigt');
  FitAndCheck(BuiltinTask(TAsymPseudoVoigtPointsSet.GetCurveTypeId), 'Asym PV');
  FitAndCheck(BuiltinTask(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId), '2-br PV');
  FitAndCheck(BuiltinTask(TPearson7PointsSet.GetCurveTypeId), 'Pearson VII');
  FitAndCheck(BuiltinTask(TMoffatPointsSet.GetCurveTypeId), 'Moffat');
  FitAndCheck(BuiltinTask(TDoniachSunjicPointsSet.GetCurveTypeId), 'Doniach-Sunjic');
  FitAndCheck(BuiltinTask(TEmgPointsSet.GetCurveTypeId), 'EMG');
  FitAndCheck(BuiltinTask(TVoigtPointsSet.GetCurveTypeId), 'Voigt');
  FitAndCheck(BuiltinTask(TSkewedGaussianPointsSet.GetCurveTypeId), 'Skewed Gaussian');
  FitAndCheck(UserTask, 'User');
end;

procedure TPythonBackendProcessTest.PythonAndNativeAgreeOnGaussian;
var
  Url: string;
  NativeTask, PyTask: TFitTask;
  Backend: IFitBackend;
begin
  Url := EnsureSidecarOrIgnore;

  NativeTask := BuiltinTask(TGaussPointsSet.GetCurveTypeId);
  PyTask := BuiltinTask(TGaussPointsSet.GetCurveTypeId);
  try
    NativeTask.MinimizeDifference;
    Backend := TPythonFitBackend.Create(Url);
    Backend.Fit(PyTask);
    //  Same data, same model: the two minimizers must land on the same centre.
    AssertEquals('native and Python agree on the fitted centre',
      FittedX0(NativeTask), FittedX0(PyTask), 0.05);
  finally
    NativeTask.Free;
    PyTask.Free;
  end;
end;

{ The property that actually matters, per curve type: fitting the same data with the
  gradient minimizer must be no worse than the native Downhill Simplex, measured by the
  same weighted chi-square the app displays. "Both can fit it" is not enough - that was
  true while Python was silently 40x worse on real data. }
procedure TPythonBackendProcessTest.PythonQualityIsNoWorseThanNative;
var
  Url: string;

  procedure CompareOn(NativeTask, PyTask: TFitTask; const What: string);
  var
    Backend: IFitBackend;
    ChiNative, ChiPython: double;
  begin
    try
      NativeTask.MinimizeDifference;
      ChiNative := WeightedChiSquare(NativeTask);

      Backend := TPythonFitBackend.Create(Url);
      Backend.Fit(PyTask);
      ChiPython := WeightedChiSquare(PyTask);

      AssertTrue(Format(
        '%s: Python weighted chi2 (%.4g) must be <= native (%.4g) x %.2f',
        [What, ChiPython, ChiNative, QUALITY_TOLERANCE]),
        ChiPython <= ChiNative * QUALITY_TOLERANCE);
    finally
      NativeTask.Free;
      PyTask.Free;
    end;
  end;

begin
  Url := EnsureSidecarOrIgnore;
  CompareOn(BuiltinTask(TGaussPointsSet.GetCurveTypeId),
            BuiltinTask(TGaussPointsSet.GetCurveTypeId), 'Gaussian');
  CompareOn(BuiltinTask(TLorentzPointsSet.GetCurveTypeId),
            BuiltinTask(TLorentzPointsSet.GetCurveTypeId), 'Lorentzian');
  CompareOn(BuiltinTask(TPseudoVoigtPointsSet.GetCurveTypeId),
            BuiltinTask(TPseudoVoigtPointsSet.GetCurveTypeId), 'Pseudo-Voigt');
  CompareOn(BuiltinTask(TAsymPseudoVoigtPointsSet.GetCurveTypeId),
            BuiltinTask(TAsymPseudoVoigtPointsSet.GetCurveTypeId), 'Asym PV');
  CompareOn(BuiltinTask(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId),
            BuiltinTask(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId), '2-br PV');
  CompareOn(UserTask, UserTask, 'User');
end;

procedure TPythonBackendProcessTest.PythonFitsMoreParametersThanPoints;
var
  Url: string;
  P: TFitProblem;
  Task: TFitTask;
  Backend: IFitBackend;
  O: TFitOutcome;
  v: double; n, c: integer;
begin
  Url := EnsureSidecarOrIgnore;

  //  Three 2-br PV curves (6 params each = 18) over a 13-point window: M<N, which
  //  MINPACK leastsq rejects and the trf solver handles - as the native engine does.
  P := Default(TFitProblem);
  n := 0; v := 4;
  while v <= 10 + 1e-9 do
  begin
    SetLength(P.ProfileX, n + 1); SetLength(P.ProfileY, n + 1);
    P.ProfileX[n] := v; P.ProfileY[n] := GaussPoint(100, 1.5, 7, v);
    Inc(n); v := v + 0.5;
  end;
  SetLength(P.PositionsX, 3); SetLength(P.PositionsY, 3);
  for c := 0 to 2 do
  begin
    P.PositionsX[c] := 5 + c;
    P.PositionsY[c] := GaussPoint(100, 1.5, 7, 5 + c);
  end;
  P.CurveTypeId := GUIDToString(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
  P.MinimizerKind := 0;

  Task := BuildTaskFromProblem(P);
  try
    Backend := TPythonFitBackend.Create(Url);
    //  Must not raise "Improper input: N must not exceed M".
    Backend.Fit(Task);
    O := ReadOutcomeFromTask(Task);
    AssertEquals('all three curves came back', 3, Length(O.Curves));
  finally
    Task.Free;
  end;
end;

initialization
  RegisterTest('integration', TPythonBackendProcessTest);
end.
