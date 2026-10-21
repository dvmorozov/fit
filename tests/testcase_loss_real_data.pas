// SPDX-License-Identifier: GPL-3.0-or-later
{ Real-data gate for the objective functions, on Data/2.dat.

  WHY THIS FILE. Changing what a fit minimises is exactly the kind of change that
  looks fine in unit tests and quietly degrades real work, because the arithmetic
  can be provably correct while the optimiser behaves worse on an actual profile.
  Data/2.dat is the project's hard case - eight overlapping 2-branch Pseudo-Voigt
  peaks, 48 badly-scaled parameters, the file where the Python minimizer was once
  seen to fit ~40x worse than DHS - so it is the right place to prove that
  diffraction fitting is PRESERVED.

  MEASURED INDEPENDENTLY OF THE OBJECTIVE. Comparing each loss by its own value
  would be meaningless: they are different scales, and each is trivially best at
  minimising itself. So every fit is scored by the same scaling-aware sum of
  squared residuals, model versus data, computed here. That is the number that
  says whether the fit is actually good. }
unit testcase_loss_real_data;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  fit_task, fit_problem_json, fit_task_marshalling, fit_loss,
  dat_file_loader, title_points_set, points_set,
  two_branches_pseudo_voigt_points_set;

type
    { The value the optimiser actually minimises is PROTECTED - the algorithm
      reaches it through inheritance, not from outside.

      It used to be reachable through a public GetLossValue on TFitTask that
      nothing in the application called: a method kept in a production class for
      one test. That is gone, and this takes its place.

      REACHED BY CAST, not by construction, because the task is built by
      BuildTaskFromProblem - which makes a TFitTask by name, so there is no
      point at which a descendant could be substituted. The cast is safe as
      written: this class adds no fields and the method only forwards. }
    TTestableFitTask = class(TFitTask)
    public
        function LossValue: double;
    end;

type
  TLossRealDataTest = class(TTestCase)
  private
    function BuildTask(ALoss: longint; AScaling: boolean): TFitTask;
    { Model versus data with the engine's own scaling factor applied, so a fit
      is judged on the curve the engine actually compares. }
    function Residual(ATask: TFitTask): double;
    function FitAndScore(ALoss: longint; AScaling: boolean): double;
  published
    procedure EveryLossFitsTheDiffractionProfileWell;
    procedure TheDefaultIsNoWorseThanTheModelNormalisedForm;
    procedure TheModelNormalisedFormIsTheOnlyOneScalingCanGame;
    procedure PoolingIntervalsIsNotTheSameAsAddingTheirRFactors;
  end;

implementation

function TTestableFitTask.LossValue: double;
begin
    Result := GetOptimizingRFactor;
end;

//  The prominent local maxima of Data/2.dat, as testcase_python_real_data uses.
const
  PEAKS: array[0..7] of double =
    (116.2, 116.7, 117.5, 118.4, 119.7, 120.0, 120.4, 120.7);

function DataDir: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
    DirectorySeparator + 'Data' + DirectorySeparator);
end;

function TLossRealDataTest.BuildTask(ALoss: longint; AScaling: boolean): TFitTask;
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
    //  Explicit, because it is the whole subject here. Default(TFitProblem)
    //  leaves scaling OFF, which is NOT how the application runs - fit_service
    //  enables it - so a test that took the default would not be testing the
    //  configuration that exhibits the problem.
    P.CurveScaling := AScaling;
  finally
    PS.Free;
  end;
  Result := BuildTaskFromProblem(P);
  Result.LossKind := ALoss;
end;

function TLossRealDataTest.Residual(ATask: TFitTask): double;
var
  Calc, Exp_: TPointsSet;
  i, N: integer;
  D, S: double;
begin
  Result := 0;
  Calc := ATask.GetCalcProfile;
  Exp_ := ATask.ExpProfile;
  if (not Assigned(Calc)) or (not Assigned(Exp_)) then Exit;
  S := ATask.GetScalingFactor;
  N := Min(Calc.PointsCount, Exp_.PointsCount);
  for i := 0 to N - 1 do
  begin
    D := Exp_.PointYCoord[i] - Calc.PointYCoord[i] * S;
    Result := Result + D * D;
  end;
end;

function TLossRealDataTest.FitAndScore(ALoss: longint; AScaling: boolean): double;
var
  T: TFitTask;
begin
  T := BuildTask(ALoss, AScaling);
  try
    T.MinimizeDifference;
    Result := Residual(T);
    //  Both figures, because they are not the same question. The residual is a
    //  SUM OF SQUARES for every loss, so a loss that minimises absolute
    //  deviation instead is being scored on someone else's objective: it trades
    //  a few large residuals for many small ones, which is the point of it and
    //  which reads as worse in L2. Printing each loss's own figure beside it is
    //  what tells a real regression apart from that trade.
    //  stderr, not stdout: the runner writes its XML report there and a
    //  diagnostic interleaved into it truncates both.
    writeln(StdErr, Format('  2.dat  %-22s scaling=%-5s  residual=%.6g  |dev|=%.6g',
      [LossName(ALoss), BoolToStr(AScaling, 'on', 'off'), Result,
       T.GetAbsRFactor]));
  finally
    T.Free;
  end;
end;

{ PRESERVATION. Whatever the objective, fitting eight peaks to this profile must
  produce a genuinely good model - this is the "previously working functionality
  is preserved" check, and it runs for every loss so a newly added one cannot
  claim to be usable without demonstrating it here. }
procedure TLossRealDataTest.EveryLossFitsTheDiffractionProfileWell;
var
  K: longint;
  Flat, R: double;
  T: TFitTask;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  //  The yardstick: the residual of the trivial "no peaks at all" model. A fit
  //  that does not beat it by a wide margin has not modelled anything. Using a
  //  derived baseline rather than a hard-coded number keeps the test meaningful
  //  if the fixture is ever changed.
  T := BuildTask(LOSS_KIND_RFACTOR, False);
  try
    Flat := Residual(T);
  finally
    T.Free;
  end;

  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
  begin
    R := FitAndScore(K, True);
    AssertTrue(Format('%s: fit residual %.6g must be far below the unfitted ' +
      '%.6g on real diffraction data', [LossName(K), R, Flat]),
      R < Flat * 0.05);
  end;
end;

{ The default must not be a regression on the app's core domain. }
procedure TLossRealDataTest.TheDefaultIsNoWorseThanTheModelNormalisedForm;
var
  Default_, Legacy: double;
  T: TFitTask;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  //  A task that is never told which objective to use must already be on the
  //  corrected one - that is what "default" means, and it is the thing most
  //  easily lost in a later refactor.
  T := TFitTask.Create(nil);
  try
    AssertEquals('the corrected R-factor must be what a task minimises unless ' +
      'asked otherwise', LOSS_KIND_RFACTOR, T.LossKind);
  finally
    T.Free;
  end;

  Default_ := FitAndScore(LOSS_KIND_RFACTOR, True);
  Legacy := FitAndScore(LOSS_KIND_RFACTOR_LEGACY, True);

  //  Slack for solver path differences: the claim is "no meaningful regression",
  //  not "identical", since the two objectives genuinely differ.
  AssertTrue(Format('corrected R-factor (%.6g) must be no worse than the ' +
    'model-normalised form (%.6g) on real diffraction data',
    [Default_, Legacy]), Default_ <= Legacy * 1.10);
end;

{ THE DEFECT ITSELF, on real data rather than on constructed arrays.

  With curve scaling on, s = (sum obs)/(sum calc) makes calc*s invariant under a
  change of model amplitude - so the model-normalised objective can be lowered by
  inflating the model, which changes nothing about the agreement with the data.
  Dividing by the OBSERVED integral serves the same purpose (a dimensionless,
  count-time-independent measure) with a denominator that is constant during the
  fit and therefore cannot be gamed. }
procedure TLossRealDataTest.TheModelNormalisedFormIsTheOnlyOneScalingCanGame;
var
  T: TFitTask;
  K: longint;
  Calc: TPointsSet;
  i: integer;
  Before, After: double;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);

  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
  begin
    T := BuildTask(K, True);
    try
      T.ComputeProfile;
      Before := TTestableFitTask(T).LossValue;

      //  Double the model outright. With scaling on this is a NO-OP as far as
      //  agreement with the data goes: calc*s is unchanged.
      Calc := T.GetCalcProfile;
      for i := 0 to Calc.PointsCount - 1 do
        Calc.PointYCoord[i] := Calc.PointYCoord[i] * 2;
      After := TTestableFitTask(T).LossValue;

      if LossIsSelfNormalising(K) then
        AssertTrue(Format('%s: inflating the model must lower it (%.6g -> ' +
          '%.6g) - this is the defect', [LossName(K), Before, After]),
          After < Before * 0.9999)
      else
        AssertEquals(Format('%s: a pure change of model scale must not change ' +
          'the objective', [LossName(K)]), Before, After,
          Max(Abs(Before) * 1e-9, 1e-12));
    finally
      T.Free;
    end;
  end;
end;

{ POOLING, not adding. Fitting intervals are separate sub-problems, so each is
  measured on its own - but the figure the user reads is about the model as a
  whole, and a ratio is not additive.

  Adding them meant that marking a SECOND, equally well fitted interval doubled
  the reported number, so improving a model's coverage made it read as twice as
  bad. Two intervals of identical quality must read the same as one; that is the
  property, and it is what the pooled form gives. }
procedure TLossRealDataTest.PoolingIntervalsIsNotTheSameAsAddingTheirRFactors;
var
  Calc, Obs: TLossDoubleArray;
  One, Both: TLossParts;
  Single_, Pooled, Added: double;
begin
  //  One interval: model off by 1 at every point, data at 10.
  SetLength(Calc, 4);
  SetLength(Obs, 4);
  Calc[0] := 9; Calc[1] := 9; Calc[2] := 9; Calc[3] := 9;
  Obs[0] := 10; Obs[1] := 10; Obs[2] := 10; Obs[3] := 10;

  One := LossPartsOf(Calc, Obs, 1);
  Single_ := LossFromParts(LOSS_KIND_RELATIVE, One);

  //  A second interval exactly like it. The model is no better and no worse.
  Both := Default(TLossParts);
  AddLossParts(Both, One);
  AddLossParts(Both, One);
  Pooled := LossFromParts(LOSS_KIND_RELATIVE, Both);
  Added := Single_ + Single_;

  AssertEquals('two intervals of identical quality read the same as one',
    Single_, Pooled, 1e-15);
  AssertTrue(Format('and that is NOT what adding them gives (%.6g vs %.6g)',
    [Added, Pooled]), Abs(Added - Pooled) > 1e-9);

  //  The squared form pools the same way.
  AssertTrue('the squared form is also pooled, not summed',
    LossFromParts(LOSS_KIND_RFACTOR, Both) <
    2 * LossFromParts(LOSS_KIND_RFACTOR, One));
end;


initialization
  RegisterTest('integration', TLossRealDataTest);
end.
