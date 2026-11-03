// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A task's evaluation budget: what bounds a fit whatever minimizer the
user chose.)

TFitTask.EvaluationBudget is spent in TFitTask.ComputeFunc, the one callback
every native minimizer evaluates through - which is why these tests run each
native kind and name none of their internals. The background's step is the user
of it (TFitService.FitProfileBackground); its own test, through the service, is
Modules/scientific's TLiveStopTest.EveryMinimizerKindStopsWhenAsked.
}
unit testcase_evaluation_budget;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fit_task, points_set,
    test_curve_types, curve_types_singleton, int_curve_type_selector,
    int_fit_service, SimpMath;

type
    TEvaluationBudgetTest = class(TTestCase)
    private
        { A task of one Gaussian, picked off its peak so the fit has somewhere
          to go; the data is that Gaussian exactly, so the goal is driven
          towards zero, where a relative goal never settles. }
        function BuildTask(AKind: longint; ABudget: int64): TFitTask;
    published
        procedure EveryNativeKindEndsWithinTheBudget;
        procedure WithoutABudgetTheSameFitRunsOnPastIt;
        procedure ABudgetEndedFitStillHoldsAMeasuredModel;
        procedure TheBudgetIsPerOptimisation;
    end;

implementation

const
    BUDGET = 60;
    { Evaluations a minimizer makes of its own accord outside the callback's
      budget check: the goal read before and after its sweeps. }
    SLACK = 5;

type
    TQuiet = class
        procedure NoOp;
    end;

procedure TQuiet.NoOp;
begin
end;

var
    Quiet: TQuiet;

function TEvaluationBudgetTest.BuildTask(AKind: longint;
    ABudget: int64): TFitTask;
var
    Profile, Positions: TPointsSet;
    Sel: ICurveTypeSelector;
    x: double;
begin
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
    Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
    Sel.SelectCurveType(TTestGaussPointsSet.GetCurveTypeId);
    Profile := TPointsSet.Create(nil);
    x := 0;
    while x <= 20 + 1e-9 do
    begin
        Profile.AddNewPoint(x, GaussPoint(100, 1.5, 10, x));
        x := x + 0.2;
    end;
    Positions := TPointsSet.Create(nil);
    Positions.AddNewPoint(12, GaussPoint(100, 1.5, 10, 12));
    Result := TFitTask.Create(nil, False);
    Result.ServerShowCurMin := @Quiet.NoOp;
    Result.ServerDoneProc := @Quiet.NoOp;
    Result.SetProfilePointsSet(Profile);
    Result.SetCurvePositions(Positions);
    Result.RecreateCurves(nil);
    Result.BegIndex := 0;
    Result.EndIndex := Result.GetCalcProfile.PointsCount - 1;
    Result.MinimizerKind := AKind;
    Result.EvaluationBudget := ABudget;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
end;

procedure TEvaluationBudgetTest.EveryNativeKindEndsWithinTheBudget;
const
    KINDS: array[0..2] of longint = (MIN_KIND_DHS, MIN_KIND_DHS_BLOCKS,
        MIN_KIND_DHS_RESTARTED);
var
    k: longint;
    Task: TFitTask;
begin
    for k := 0 to High(KINDS) do
    begin
        Task := BuildTask(KINDS[k], BUDGET);
        try
            Task.MinimizeDifference;
            AssertTrue(Format('kind %d: the budget is what ended the fit',
                [KINDS[k]]), Task.BudgetSpent);
            AssertTrue(Format('kind %d spent %d evaluations of a budget of %d',
                [KINDS[k], Task.Evaluations, BUDGET]),
                Task.Evaluations <= BUDGET + SLACK);
        finally
            Task.Free;
        end;
    end;
end;

{ THE CONTROL: without it the test above would pass for a fit that never
  needed more than the budget, and prove nothing about the budget. }
procedure TEvaluationBudgetTest.WithoutABudgetTheSameFitRunsOnPastIt;
const
    KINDS: array[0..2] of longint = (MIN_KIND_DHS, MIN_KIND_DHS_BLOCKS,
        MIN_KIND_DHS_RESTARTED);
var
    k: longint;
    Task: TFitTask;
begin
    for k := 0 to High(KINDS) do
    begin
        Task := BuildTask(KINDS[k], 0);
        try
            Task.MinimizeDifference;
            AssertFalse(Format('kind %d: no budget, none spent', [KINDS[k]]),
                Task.BudgetSpent);
            AssertTrue(Format('kind %d needed more than %d evaluations: %d',
                [KINDS[k], BUDGET + SLACK, Task.Evaluations]),
                Task.Evaluations > BUDGET + SLACK);
        finally
            Task.Free;
        end;
    end;
end;

{ A budget is a stop that is not Stop: what the fit reached is the model, and
  the task reports its figure like any other that ended. }
procedure TEvaluationBudgetTest.ABudgetEndedFitStillHoldsAMeasuredModel;
var
    Task: TFitTask;
begin
    Task := BuildTask(MIN_KIND_DHS_RESTARTED, BUDGET);
    try
        Task.MinimizeDifference;
        AssertTrue('the budget ended it', Task.BudgetSpent);
        AssertFalse('it reports a figure', IsNan(Task.GetCurMin));
        AssertTrue('and it is the figure of a model, not of a probe: ' +
            FloatToStr(Task.GetCurMin), Task.GetCurMin < 1);
    finally
        Task.Free;
    end;
end;

{ THE RUNS ARE INCREMENTAL AND THE ROUNDS CALL IT AGAIN: each optimisation has
  the whole budget, or the second turn of a background would have none. }
procedure TEvaluationBudgetTest.TheBudgetIsPerOptimisation;
var
    Task: TFitTask;
begin
    Task := BuildTask(MIN_KIND_DHS, BUDGET);
    try
        Task.MinimizeDifference;
        Task.MinimizeDifference;
        AssertTrue('the second optimisation counted its own evaluations: ' +
            IntToStr(Task.Evaluations), Task.Evaluations <= BUDGET + SLACK);
        AssertTrue('and was ended by the budget as well', Task.BudgetSpent);
    finally
        Task.Free;
    end;
end;

initialization
    Quiet := TQuiet.Create;
    //  INTEGRATION: it runs the optimiser.
    RegisterTest('integration', TEvaluationBudgetTest);
finalization
    Quiet.Free;
end.
