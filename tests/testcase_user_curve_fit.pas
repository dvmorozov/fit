// SPDX-License-Identifier: GPL-3.0-or-later
{ End-to-end fit of a user-defined curve, headless (Stage 1A). Builds a synthetic
  peak and fits it with a user formula A*exp(-((x-x0)/SIGMA)^2), exercising the
  whole user-curve path: role recognition by name (A/x/x0/SIGMA), initialisation
  from the data, and convergence. Guards the "user curve won't fit" regressions. }
unit testcase_user_curve_fit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  fit_task, points_set, curve_types_singleton, int_curve_type_selector,
  special_curve_parameter, persistent_curve_parameters,
  persistent_curve_parameter_container, user_curve_parameter, user_points_set,
  //  WHICH INSTANCE IS WHICH, and by which key it was issued - the two things
  //  "one instance per pick" actually means.
  curve_identity_registry, curve_instance_id, curve_points_set,
  self_copied_component,
  SimpMath;

type
  TUcCB = class
    procedure NoOp;
  end;

  TUserCurveFitTest = class(TTestCase)
  private
    function BuildParams: Curve_parameters;
    { The same set with NO position parameter at all - a formula that declares
      none, which is the only thing that takes the slot path. }
    function BuildParamsWithoutAPosition: Curve_parameters;
    { Three separated peaks, so three picks are three distinguishable curves. }
    function ThreePeakProfile: TPointsSet;
  published
    procedure FitsSyntheticPeakWithUserFormula;
    procedure FitsWithoutAPlacedPosition;
    procedure FitsWithRolesByTypeNotName;

    //  One instance per pick, at N > 1. Covered at N = 1 and N = 0 above, and
    //  nowhere at all until now - for exactly the curve type most likely to
    //  regress, and it is the property deleting one curve depends on.
    procedure ThreePicksGiveThreeInstances;
    procedure EachWithItsOwnHandle;
    procedure EachSeededFromItsOwnPick;
    procedure DroppingTheMiddlePickLeavesTheOthersHandles;

    //  Which identity path a curve takes, which nothing asserted.
    procedure APositionedUserCurveTakesTheSeedPath;
    procedure OnlyAFormulaDeclaringNoPositionTakesTheSlotPath;
  end;

implementation

procedure TUcCB.NoOp; begin end;

{ Adds one parameter (owned by the container) to the parameter set. }
procedure AddParam(P: Curve_parameters; const AName: string;
  AType: TParameterType; AValue: double);
var
  Param: TUserCurveParameter;
  Cont: TPersistentCurveParameterContainer;
begin
  Param := TUserCurveParameter.Create;
  Param.Name := AName;
  Param.Type_ := AType;
  Param.Value := AValue;
  Cont := TPersistentCurveParameterContainer(P.Params.Add);
  Cont.Parameter := Param;
end;

function TUserCurveFitTest.BuildParams: Curve_parameters;
begin
  Result := Curve_parameters.Create(nil);
  Result.Params.Clear;   //  drop the default placeholder parameter
  AddParam(Result, 'A', Variable, 0);              //  amplitude (from data peak)
  AddParam(Result, 'x', Argument, 0);              //  the argument
  AddParam(Result, 'x0', InvariablePosition, 0);   //  position (from placement)
  AddParam(Result, 'SIGMA', Variable, 0.25);       //  width (non-zero default)
end;

function TUserCurveFitTest.BuildParamsWithoutAPosition: Curve_parameters;
begin
  Result := Curve_parameters.Create(nil);
  Result.Params.Clear;
  AddParam(Result, 'A', Variable, 0);
  AddParam(Result, 'x', Argument, 0);
  //  NO position parameter of any type. Hasx0 is Assigned(FPositionP), which is
  //  set for VariablePosition or InvariablePosition and nothing else - so this
  //  is the one shape of curve no pick can place.
  AddParam(Result, 'SIGMA', Variable, 0.25);
end;

function TUserCurveFitTest.ThreePeakProfile: TPointsSet;
var
  x: double;
begin
  Result := TPointsSet.Create(nil);
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    Result.AddNewPoint(x,
      GaussPoint(100, 0.8, 5, x) + GaussPoint(100, 0.8, 10, x) +
      GaussPoint(100, 0.8, 15, x));
    x := x + 0.1;
  end;
end;

procedure TUserCurveFitTest.FitsSyntheticPeakWithUserFormula;
var
  Task: TFitTask;
  Profile, Positions: TPointsSet;
  Sel: ICurveTypeSelector;
  CB: TUcCB;
  x, r: double;
begin
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
  Sel.SelectCurveType(TUserPointsSet.GetCurveTypeId);

  Profile := TPointsSet.Create(nil);
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    Profile.AddNewPoint(x, GaussPoint(100, 1.5, 10, x));
    x := x + 0.2;
  end;
  Positions := TPointsSet.Create(nil);
  Positions.AddNewPoint(10, 0);

  Task := TFitTask.Create(nil, False, False);
  try
    Task.ServerShowCurMin := @CB.NoOp;
    Task.ServerDoneProc := @CB.NoOp;
    //  Task takes ownership of the parameter set.
    Task.SetSpecialCurve('A*exp(-((x-x0)/SIGMA)^2)', BuildParams);
    Task.SetProfilePointsSet(Profile);
    Task.SetCurvePositions(Positions);
    Task.RecreateCurves(nil);
    Task.BegIndex := 0;
    Task.EndIndex := Task.GetCalcProfile.PointsCount - 1;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
      exUnderflow, exPrecision]);
    Task.MinimizeDifference;
    r := Task.GetCurMin;
    AssertTrue('R-factor small after fitting a user curve to a matching peak (' +
      FloatToStr(r) + ')', (r >= 0) and (r < 0.05));
  finally
    Task.Free;
    CB.Free;
  end;
end;

procedure TUserCurveFitTest.FitsWithoutAPlacedPosition;
var
  Task: TFitTask;
  Profile, Positions: TPointsSet;
  Sel: ICurveTypeSelector;
  CB: TUcCB;
  x, r: double;
begin
  //  A positioned user curve with no explicitly placed position must still get
  //  one instance (defaulted to the interval centre), like a built-in curve,
  //  instead of being silently discarded so nothing is fitted.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
  Sel.SelectCurveType(TUserPointsSet.GetCurveTypeId);

  Profile := TPointsSet.Create(nil);
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    Profile.AddNewPoint(x, GaussPoint(100, 1.5, 10, x));
    x := x + 0.2;
  end;

  Task := TFitTask.Create(nil, False, False);
  try
    Task.ServerShowCurMin := @CB.NoOp;
    Task.ServerDoneProc := @CB.NoOp;
    Task.SetSpecialCurve('A*exp(-((x-x0)/SIGMA)^2)', BuildParams);
    Task.SetProfilePointsSet(Profile);
    //  An empty positions set (as the UI provides when the user places none):
    //  the fit must create the instance itself, not discard it.
    Positions := TPointsSet.Create(nil);
    Task.SetCurvePositions(Positions);
    Task.RecreateCurves(nil);
    Task.BegIndex := 0;
    Task.EndIndex := Task.GetCalcProfile.PointsCount - 1;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
      exUnderflow, exPrecision]);
    Task.MinimizeDifference;
    r := Task.GetCurMin;
    AssertTrue('a curve is created and fitted without a placed position (' +
      FloatToStr(r) + ')', (r >= 0) and (r < 0.05));
  finally
    Task.Free;
    CB.Free;
  end;
end;

procedure TUserCurveFitTest.FitsWithRolesByTypeNotName;
var
  Task: TFitTask;
  Profile, Positions: TPointsSet;
  Sel: ICurveTypeSelector;
  CB: TUcCB;
  Params: Curve_parameters;
  x, r: double;
begin
  //  Amplitude and width are designated by ROLE (type), with non-conventional
  //  names k and q, so the fit must still recognise and initialise them - proving
  //  roles no longer depend on the A/SIGMA naming convention.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
  Sel.SelectCurveType(TUserPointsSet.GetCurveTypeId);

  Profile := TPointsSet.Create(nil);
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    Profile.AddNewPoint(x, GaussPoint(100, 1.5, 10, x));
    x := x + 0.2;
  end;
  Positions := TPointsSet.Create(nil);
  Positions.AddNewPoint(10, 0);

  Params := Curve_parameters.Create(nil);
  Params.Params.Clear;
  AddParam(Params, 'k', Amplitude, 0);            //  amplitude by role
  AddParam(Params, 'x', Argument, 0);
  AddParam(Params, 'x0', InvariablePosition, 0);
  AddParam(Params, 'q', Width, 0);                //  width by role (starts 0!)

  Task := TFitTask.Create(nil, False, False);
  try
    Task.ServerShowCurMin := @CB.NoOp;
    Task.ServerDoneProc := @CB.NoOp;
    Task.SetSpecialCurve('k*exp(-((x-x0)/q)^2)', Params);
    Task.SetProfilePointsSet(Profile);
    Task.SetCurvePositions(Positions);
    Task.RecreateCurves(nil);
    Task.BegIndex := 0;
    Task.EndIndex := Task.GetCalcProfile.PointsCount - 1;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
      exUnderflow, exPrecision]);
    Task.MinimizeDifference;
    r := Task.GetCurMin;
    AssertTrue('role-typed amplitude/width initialise and fit (' +
      FloatToStr(r) + ')', (r >= 0) and (r < 0.05));
  finally
    Task.Free;
    CB.Free;
  end;
end;


{ ---- one instance per pick, at more than one pick ------------------------- }

{ Builds the model from a list of picks and hands back the task, ready for
  assertions. The caller frees it. }
function PickedTask(const APicks: array of double;
  AParams: Curve_parameters; const AFormula: string;
  AProfile: TPointsSet; ACallback: TUcCB;
  ARegistry: TCurveIdentityRegistry): TFitTask;
var
  Positions: TPointsSet;
  i: longint;
begin
  Positions := TPointsSet.Create(nil);
  for i := 0 to High(APicks) do
    Positions.AddNewPoint(APicks[i], 0);

  Result := TFitTask.Create(nil, False, False);
  //  SUPPLIED, because a task's own fallback registry is protected and only the
  //  borrowed one is reachable - and because this is how the service does it:
  //  one registry for the whole model, so a handle outlives one task.
  Result.Identity := ARegistry;
  Result.ServerShowCurMin := @ACallback.NoOp;
  Result.ServerDoneProc := @ACallback.NoOp;
  Result.SetSpecialCurve(AFormula, AParams);
  Result.SetProfilePointsSet(AProfile);
  Result.SetCurvePositions(Positions);
  Result.RecreateCurves(nil);
  Result.BegIndex := 0;
  Result.EndIndex := Result.GetCalcProfile.PointsCount - 1;
end;

{ The n-th built curve. GetCurves hands back a copy of the list, and FInstanceId
  is copied with it - which is the whole reason a handle can cross a boundary. }
function CurveAt(ATask: TFitTask; AIndex: longint): TCurvePointsSet;
var
  L: TSelfCopiedCompList;
begin
  L := ATask.GetCurves;
  try
    Result := TCurvePointsSet(L.Items[AIndex]);
  finally
    //  NOT freed: the list owns its components and the caller reads one of
    //  them. Held by the task's own list too, so nothing here owns it.
  end;
end;

function CurvesCount(ATask: TFitTask): longint;
begin
  Result := ATask.GetCurves.Count;
end;

procedure TUserCurveFitTest.ThreePicksGiveThreeInstances;
var
  Task: TFitTask;
  CB: TUcCB;
  Reg: TCurveIdentityRegistry;
begin
  //  N > 1 WAS UNTESTED. One pick and no picks were covered; three were not,
  //  for the type whose placement rules were most often claimed to be special -
  //  and "one instance per pick" is what deleting one curve rests on.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Reg := TCurveIdentityRegistry.Create;
  TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
    TUserPointsSet.GetCurveTypeId);
  Task := PickedTask([5, 10, 15], BuildParams,
    'A*exp(-((x-x0)/SIGMA)^2)', ThreePeakProfile, CB, Reg);
  try
    AssertEquals('one curve per pick', 3, CurvesCount(Task));
  finally
    Task.Free;
    //  After the task, which borrows it.
    Reg.Free;
    CB.Free;
  end;
end;

procedure TUserCurveFitTest.EachWithItsOwnHandle;
var
  Task: TFitTask;
  CB: TUcCB;
  Reg: TCurveIdentityRegistry;
  A, B, C: string;
begin
  //  DISTINCT, because the handle is what every operation on ONE curve takes -
  //  two curves sharing one would be deleted together, and would be handed the
  //  same fitted values on the next rebuild.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Reg := TCurveIdentityRegistry.Create;
  TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
    TUserPointsSet.GetCurveTypeId);
  Task := PickedTask([5, 10, 15], BuildParams,
    'A*exp(-((x-x0)/SIGMA)^2)', ThreePeakProfile, CB, Reg);
  try
    A := CurveInstanceIdToWire(CurveAt(Task, 0).FInstanceId);
    B := CurveInstanceIdToWire(CurveAt(Task, 1).FInstanceId);
    C := CurveInstanceIdToWire(CurveAt(Task, 2).FInstanceId);
    AssertTrue('the first has one', A <> '');
    AssertTrue('and they differ', (A <> B) and (B <> C) and (A <> C));
  finally
    Task.Free;
    //  After the task, which borrows it.
    Reg.Free;
    CB.Free;
  end;
end;

procedure TUserCurveFitTest.EachSeededFromItsOwnPick;
var
  Task: TFitTask;
  CB: TUcCB;
  Reg: TCurveIdentityRegistry;
  i, Found: longint;
begin
  //  THE REGISTRY KEYS THEM BY PICK, which is what makes a handle survive a
  //  rebuild that renumbers the curve list - the ordering caveat the engine
  //  records about its own curve order.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Reg := TCurveIdentityRegistry.Create;
  TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
    TUserPointsSet.GetCurveTypeId);
  Task := PickedTask([5, 10, 15], BuildParams,
    'A*exp(-((x-x0)/SIGMA)^2)', ThreePeakProfile, CB, Reg);
  try
    AssertEquals('three instances', 3, Reg.Count);
    Found := 0;
    for i := 0 to Reg.Count - 1 do
    begin
      AssertFalse('every one is placed by a pick',
        Reg.Item(i).Positionless);
      //  A tolerance rather than equality, for the reason the registry gives:
      //  pick abscissae are copied between point sets, so "very nearly equal"
      //  is not a property anything enforces.
      if Abs(Reg.Item(i).Seed - 10) < 1e-9 then
        Inc(Found);
    end;
    AssertEquals('and the middle pick is one of them', 1, Found);
  finally
    Task.Free;
    //  After the task, which borrows it.
    Reg.Free;
    CB.Free;
  end;
end;

procedure TUserCurveFitTest.DroppingTheMiddlePickLeavesTheOthersHandles;
var
  Task: TFitTask;
  CB: TUcCB;
  Reg: TCurveIdentityRegistry;
  Positions: TPointsSet;
  Before: array[0..2] of string;
  Left: array[0..1] of string;
  i: longint;
begin
  //  WHAT DELETING A CURVE RELIES ON. The model is rebuilt from its inputs, so
  //  a pick removed must take its curve - and only its curve. If the survivors
  //  came back with new handles, the values stored under the old ones would be
  //  orphaned, which is the failure the identity mechanism exists to remove.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Reg := TCurveIdentityRegistry.Create;
  TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
    TUserPointsSet.GetCurveTypeId);
  Task := PickedTask([5, 10, 15], BuildParams,
    'A*exp(-((x-x0)/SIGMA)^2)', ThreePeakProfile, CB, Reg);
  try
    for i := 0 to 2 do
      Before[i] := CurveInstanceIdToWire(CurveAt(Task, i).FInstanceId);

    //  The middle pick goes, which is what deleting that curve does to the
    //  model input.
    Positions := TPointsSet.Create(nil);
    Positions.AddNewPoint(5, 0);
    Positions.AddNewPoint(15, 0);
    Task.SetCurvePositions(Positions);
    Task.RecreateCurves(nil);

    AssertEquals('two curves left', 2, CurvesCount(Task));
    for i := 0 to 1 do
      Left[i] := CurveInstanceIdToWire(CurveAt(Task, i).FInstanceId);
    //  THE SAME TWO HANDLES, so whatever a fit found for them is still theirs.
    AssertTrue('the first kept its handle',
      (Left[0] = Before[0]) or (Left[0] = Before[2]));
    AssertTrue('so did the second',
      (Left[1] = Before[0]) or (Left[1] = Before[2]));
    AssertTrue('and neither took the deleted one' + #39 + 's',
      (Left[0] <> Before[1]) and (Left[1] <> Before[1]));
  finally
    Task.Free;
    //  After the task, which borrows it.
    Reg.Free;
    CB.Free;
  end;
end;

{ ---- which identity path a curve takes ------------------------------------ }

procedure TUserCurveFitTest.APositionedUserCurveTakesTheSeedPath;
var
  Task: TFitTask;
  CB: TUcCB;
  Reg: TCurveIdentityRegistry;
begin
  //  A USER CURVE IS AN ORDINARY CURVE. It has an x0, so it is placed one
  //  instance per pick and keyed by that pick - exactly like a built-in. The
  //  branch is on Hasx0 and nothing else: no class test and no name test.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Reg := TCurveIdentityRegistry.Create;
  TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
    TUserPointsSet.GetCurveTypeId);
  Task := PickedTask([7], BuildParams,
    'A*exp(-((x-x0)/SIGMA)^2)', ThreePeakProfile, CB, Reg);
  try
    AssertTrue('it has a position', CurveAt(Task, 0).Hasx0);
    AssertEquals('one instance', 1, Reg.Count);
    AssertFalse('keyed by its pick, not by a slot',
      Reg.Item(0).Positionless);
    AssertTrue('and by THAT pick', Abs(Reg.Item(0).Seed - 7) < 1e-9);
  finally
    Task.Free;
    //  After the task, which borrows it.
    Reg.Free;
    CB.Free;
  end;
end;

procedure TUserCurveFitTest.OnlyAFormulaDeclaringNoPositionTakesTheSlotPath;
var
  Task: TFitTask;
  CB: TUcCB;
  Reg: TCurveIdentityRegistry;
begin
  //  THE ONLY SHAPE THAT DOES. Not "a user curve" - a formula that declares no
  //  position parameter at all, which no pick can place. Its handle is keyed by
  //  the fit interval instead, and the picks are irrelevant to it.
  SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  CB := TUcCB.Create;
  Reg := TCurveIdentityRegistry.Create;
  TCurveTypesSingleton.CreateCurveTypeSelector.SelectCurveType(
    TUserPointsSet.GetCurveTypeId);
  Task := PickedTask([5, 10, 15], BuildParamsWithoutAPosition,
    'A*exp(-((x-10)/SIGMA)^2)', ThreePeakProfile, CB, Reg);
  try
    AssertFalse('no position parameter', CurveAt(Task, 0).Hasx0);
    AssertEquals('one instance whatever the picks say', 1,
      Reg.Count);
    AssertTrue('keyed by its slot',
      Reg.Item(0).Positionless);
  finally
    Task.Free;
    //  After the task, which borrows it.
    Reg.Free;
    CB.Free;
  end;
end;

initialization
  RegisterTest('integration', TUserCurveFitTest);
end.
