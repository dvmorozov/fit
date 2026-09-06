// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Maps between the engine-free fit wire contract and a live TFitTask.)

The compute server rebuilds a TFitTask from a received TFitProblem, runs it, and
reads the fitted curves back into a TFitOutcome. Keeping this in one unit lets it
be tested in-process (without HTTP) and reused by the /fit endpoint.

Covers a built-in curve type (selected by GUID); user-defined (expression)
curves are added with the corresponding TFitProblem fields in a later slice.
}
unit fit_task_marshalling;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_task, fit_problem_json;

{ Server side: builds and configures a task for the given problem. The caller owns
  the task (the profile/positions it is given are owned by the task). }
function BuildTaskFromProblem(const P: TFitProblem): TFitTask;
{ Server side: reads the fitted curves + R-factor from a task after optimization. }
function ReadOutcomeFromTask(ATask: TFitTask): TFitOutcome;

{ Client side: describes a live task as a problem to send to a compute server. }
function BuildProblemFromTask(ATask: TFitTask): TFitProblem;
{ Client side: writes a server's fitted parameter values back into a live task's
  curves (matched by curve index and parameter name). }
procedure ApplyOutcomeToTask(ATask: TFitTask; const O: TFitOutcome);

implementation

uses
    Classes, Math, points_set, curve_points_set, named_points_set,
    curve_types_singleton, int_curve_type_selector, persistent_curve_parameters,
    special_curve_parameter, self_copied_component;

type
    { A do-nothing progress/done callback target for the headless server. }
    TNoOp = class
        procedure NoOp;
    end;

procedure TNoOp.NoOp;
begin
end;

var
    GNoOp: TNoOp;

{ The profile's y at the sample nearest to AX (0 when the profile is empty). }
function ProfileValueAt(const P: TFitProblem; AX: double): double;
var
    i, Best: integer;
    D, BestD: double;
begin
    Result := 0;
    if Length(P.ProfileX) = 0 then
        Exit;
    Best := 0;
    BestD := Abs(P.ProfileX[0] - AX);
    for i := 1 to High(P.ProfileX) do
    begin
        D := Abs(P.ProfileX[i] - AX);
        if D < BestD then
        begin
            BestD := D;
            Best := i;
        end;
    end;
    if Best <= High(P.ProfileY) then
        Result := P.ProfileY[Best];
end;

function BuildTaskFromProblem(const P: TFitProblem): TFitTask;
var
    Profile, Positions: TPointsSet;
    Selector: ICurveTypeSelector;
    i: integer;
begin
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(StringToGUID(P.CurveTypeId));

    Profile := TPointsSet.Create(nil);
    for i := 0 to High(P.ProfileX) do
        Profile.AddNewPoint(P.ProfileX[i], P.ProfileY[i]);

    //  The position's y matters: RecreateCurves seeds each curve's amplitude from
    //  it, so passing 0 would start the fit from a zero-amplitude (un-fittable)
    //  curve. Fall back to the profile height at that x when no y was supplied.
    Positions := TPointsSet.Create(nil);
    for i := 0 to High(P.PositionsX) do
        if i <= High(P.PositionsY) then
            Positions.AddNewPoint(P.PositionsX[i], P.PositionsY[i])
        else
            Positions.AddNewPoint(P.PositionsX[i],
                ProfileValueAt(P, P.PositionsX[i]));

    Result := TFitTask.Create(nil, P.BackgroundVariation, P.CurveScaling);
    Result.LossKind := P.LossKind;
    Result.ServerShowCurMin := @GNoOp.NoOp;
    Result.ServerDoneProc   := @GNoOp.NoOp;
    Result.MinimizerKind := P.MinimizerKind;
    Result.MaxAcceptableRFactor := P.MaxRFactor;
    Result.SetProfilePointsSet(Profile);
    Result.SetCurvePositions(Positions);
    Result.RecreateCurves(nil);
    Result.BegIndex := P.BegIndex;
    //  A non-positive EndIndex means "to the end of the profile".
    if P.EndIndex > 0 then
        Result.EndIndex := P.EndIndex
    else
        Result.EndIndex := Result.GetCalcProfile.PointsCount - 1;

    //  Task setup can unmask FP exceptions; re-mask them so a probe into a bad
    //  region during optimization yields Inf/NaN instead of aborting the fit.
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
end;

function ReadOutcomeFromTask(ATask: TFitTask): TFitOutcome;
var
    Curves: TSelfCopiedCompList;
    Curve:  TCurvePointsSet;
    Params: Curve_parameters;
    i, j:   integer;
begin
    Result := Default(TFitOutcome);
    Result.ErrorCode := 0;
    Result.RFactor := ATask.GetCurMin;

    Curves := ATask.GetCurves;
    SetLength(Result.Curves, Curves.Count);
    for i := 0 to Curves.Count - 1 do
    begin
        Curve := TCurvePointsSet(Curves.Items[i]);
        Params := Curve.Parameters;
        SetLength(Result.Curves[i].Params, Params.Count);
        for j := 0 to Params.Count - 1 do
        begin
            Result.Curves[i].Params[j].Name  := Params[j].Name;
            Result.Curves[i].Params[j].Value := Params[j].Value;
            Result.Curves[i].Params[j].Error := Params[j].Error;
        end;
    end;
end;

function BuildProblemFromTask(ATask: TFitTask): TFitProblem;
var
    Profile, Positions: TPointsSet;
    Selector: ICurveTypeSelector;
    Curves: TSelfCopiedCompList;
    Curve:  TCurvePointsSet;
    Params: Curve_parameters;
    i, j, n: integer;
begin
    Result := Default(TFitProblem);

    Profile := ATask.ExpProfile;
    if Assigned(Profile) then
    begin
        SetLength(Result.ProfileX, Profile.PointsCount);
        SetLength(Result.ProfileY, Profile.PointsCount);
        for i := 0 to Profile.PointsCount - 1 do
        begin
            Result.ProfileX[i] := Profile.PointXCoord[i];
            Result.ProfileY[i] := Profile.PointYCoord[i];
        end;
    end;

    Positions := ATask.GetCurvePositions;
    if Assigned(Positions) then
    begin
        SetLength(Result.PositionsX, Positions.PointsCount);
        SetLength(Result.PositionsY, Positions.PointsCount);
        for i := 0 to Positions.PointsCount - 1 do
        begin
            Result.PositionsX[i] := Positions.PointXCoord[i];
            //  The y seeds each curve's amplitude - carry it, don't drop it.
            Result.PositionsY[i] := Positions.PointYCoord[i];
        end;
    end;

    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Result.CurveTypeId := GUIDToString(Selector.GetSelectedCurveType);

    //  Carry the curve's analytic formula and each placed curve's seed parameters
    //  so the Python backend can fit without knowing the curve type. The task's
    //  curves already hold the engine-seeded starting values (RecreateCurves).
    Curves := ATask.GetCurves;
    if Assigned(Curves) and (Curves.Count > 0) then
    begin
        Result.Expression :=
            TNamedPointsSet(Curves.Items[0]).GetCurveExpression;
        SetLength(Result.Curves, Curves.Count);
        for i := 0 to Curves.Count - 1 do
        begin
            Curve  := TCurvePointsSet(Curves.Items[i]);
            Params := Curve.Parameters;
            SetLength(Result.Curves[i].Params, Params.Count);
            n := 0;
            for j := 0 to Params.Count - 1 do
            begin
                //  The Argument placeholder (the abscissa x, named '?') is not a
                //  model parameter and is not referenced by the expression - skip
                //  it so the backend fits only the real curve parameters.
                if Params[j].Type_ = Argument then
                    Continue;
                Result.Curves[i].Params[n].Name  := Params[j].Name;
                Result.Curves[i].Params[n].Value := Params[j].Value;
                Result.Curves[i].Params[n].Error := Params[j].Error;
                //  Match the native engine's classification so the Python fit
                //  varies, holds and ties exactly the same parameters:
                //  varied iff a variable-role type and not disabled; Shared params
                //  are one value tied across all instances (FCommonVariableParameters).
                Result.Curves[i].Params[n].Vary :=
                    (not Params[j].VariationDisabled) and
                    (Params[j].Type_ in [Variable, VariablePosition, Amplitude,
                        Width, Shared]);
                Result.Curves[i].Params[n].Shared := Params[j].Type_ = Shared;
                //  Carry the parameter's physical bounds so the Python fit stays
                //  in the same feasible region the native engine clamps to.
                Result.Curves[i].Params[n].Min := Params[j].GetMinValue;
                Result.Curves[i].Params[n].Max := Params[j].GetMaxValue;
                Inc(n);
            end;
            SetLength(Result.Curves[i].Params, n);
        end;
    end;

    Result.Weighting           := ATask.Weighting;
    Result.MaxRFactor          := ATask.MaxAcceptableRFactor;
    Result.BackgroundVariation := ATask.BackgroundVariationEnabled;
    Result.CurveScaling        := ATask.CurveScalingEnabled;
    Result.LossKind            := ATask.LossKind;
    Result.MinimizerKind       := ATask.MinimizerKind;
    Result.BegIndex            := ATask.BegIndex;
    Result.EndIndex            := ATask.EndIndex;
end;

procedure ApplyOutcomeToTask(ATask: TFitTask; const O: TFitOutcome);
var
    Curves: TSelfCopiedCompList;
    Curve:  TCurvePointsSet;
    Params: Curve_parameters;
    i, j, k: integer;
begin
    Curves := ATask.GetCurves;
    for i := 0 to Curves.Count - 1 do
    begin
        if i > High(O.Curves) then
            Break;
        Curve := TCurvePointsSet(Curves.Items[i]);
        Params := Curve.Parameters;
        //  Set through SetValueByName, not the parameter object directly: that is
        //  what the native engine and the UI do, and it flags the curve for
        //  recomputation (FRecalculate). Writing Params[j].Value alone leaves the
        //  curve's cached profile at its pre-fit (seed) values, so ComputeProfile's
        //  ReCalc would skip it and the fitted result would never take effect.
        for j := 0 to Params.Count - 1 do
            for k := 0 to High(O.Curves[i].Params) do
                if Params[j].Name = O.Curves[i].Params[k].Name then
                begin
                    Curve.ValuesByName[Params[j].Name] :=
                        O.Curves[i].Params[k].Value;
                    //  Carry the backend's uncertainty onto the parameter so it
                    //  reaches the grid; the value setter does not touch it.
                    Params[j].Error := O.Curves[i].Params[k].Error;
                    Break;
                end;
    end;
    //  Recompute the model from the applied parameters so the view is consistent.
    ATask.ComputeProfile;
end;

initialization
    GNoOp := TNoOp.Create;

finalization
    GNoOp.Free;

end.
