// SPDX-License-Identifier: GPL-3.0-or-later
{ Real-data quality gate: on Data/2.dat (the file where the Python minimizer was
  seen to fit far worse than DHS) a multi-peak 2-br Pseudo-Voigt fit through the
  Python backend must be **no worse** than the native Downhill Simplex, measured
  by the same Poisson-weighted chi-square the app displays. This guards the
  parameter-scaling convergence fix. Skips (Ignore) when the sidecar venv is
  absent. }
unit testcase_python_real_data;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  fit_task, fit_problem_json, fit_task_marshalling, test_fit_quality,
  python_sidecar, python_fit_backend, int_fit_backend,
  dat_file_loader, title_points_set, points_set,
  two_branches_pseudo_voigt_points_set;

type
  TPythonRealDataTest = class(TTestCase)
  private
    function BuildTask: TFitTask;
  published
    procedure PythonIsNoWorseThanDhsOn2Dat;
  end;

implementation

//  The prominent local maxima of Data/2.dat (deterministic; see the peaks in the
//  116-121 window). Eight 2-br PV curves => a hard, badly-scaled 48-parameter fit.
const
  PEAKS: array[0..7] of double =
    (116.2, 116.7, 117.5, 118.4, 119.7, 120.0, 120.4, 120.7);

function DataDir: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
    DirectorySeparator + 'Data' + DirectorySeparator);
end;

{ A 2-br PV fit of Data/2.dat with curves placed at PEAKS. }
function TPythonRealDataTest.BuildTask: TFitTask;
var
  Loader: TDATFileLoader;
  PS: TTitlePointsSet;
  P: TFitProblem;
  i, n: integer;

  function HeightAt(AX: double): double;
  var j, best: integer; bd, d: double;
  begin
    best := 0; bd := Abs(PS.PointXCoord[0] - AX);
    for j := 1 to PS.PointsCount - 1 do
    begin
      d := Abs(PS.PointXCoord[j] - AX);
      if d < bd then begin bd := d; best := j; end;
    end;
    Result := PS.PointYCoord[best];
  end;

begin
  Loader := TDATFileLoader.Create(nil);
  try
    Loader.LoadDataSet(DataDir + '2.dat');
    PS := Loader.GetPointsSetCopy;
  finally
    Loader.Free;
  end;
  try
    P := Default(TFitProblem);
    SetLength(P.ProfileX, PS.PointsCount);
    SetLength(P.ProfileY, PS.PointsCount);
    for i := 0 to PS.PointsCount - 1 do
    begin
      P.ProfileX[i] := PS.PointXCoord[i];
      P.ProfileY[i] := PS.PointYCoord[i];
    end;
    n := Length(PEAKS);
    SetLength(P.PositionsX, n);
    SetLength(P.PositionsY, n);
    for i := 0 to n - 1 do
    begin
      P.PositionsX[i] := PEAKS[i];
      P.PositionsY[i] := HeightAt(PEAKS[i]);
    end;
    P.CurveTypeId := GUIDToString(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
    P.MinimizerKind := 0;
  finally
    PS.Free;
  end;
  Result := BuildTaskFromProblem(P);
end;

procedure TPythonRealDataTest.PythonIsNoWorseThanDhsOn2Dat;
var
  Sidecar: TPythonSidecar;
  Url: string;
  NativeTask, PyTask: TFitTask;
  Backend: IFitBackend;
  ChiNative, ChiPython: double;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  Sidecar := TPythonSidecar.Create;
  try
    if not Sidecar.IsConfigured then
      Ignore('Python sidecar venv not installed - see docs/user-guide/building-from-source.md');
    Url := Sidecar.EnsureRunning;
    if Url = '' then
      Ignore('Python sidecar could not be started (missing numpy/scipy/lmfit?)');

    NativeTask := BuildTask;
    PyTask := BuildTask;
    try
      NativeTask.MinimizeDifference;
      ChiNative := WeightedChiSquare(NativeTask);

      Backend := TPythonFitBackend.Create(Url);
      Backend.Fit(PyTask);
      ChiPython := WeightedChiSquare(PyTask);

      //  Python must be no worse than DHS on the displayed metric (small slack
      //  for solver differences). Before the x_scale fix Python was ~40x worse.
      AssertTrue(Format(
        'Python weighted chi2 (%.1f) must be <= DHS (%.1f) x 1.25',
        [ChiPython, ChiNative]), ChiPython <= ChiNative * 1.25);
    finally
      NativeTask.Free;
      PyTask.Free;
    end;
  finally
    Sidecar.Free;
  end;
end;

initialization
  RegisterTest('integration', TPythonRealDataTest);
end.
