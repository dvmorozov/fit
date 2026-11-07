// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A task's fixed part of the model: added after its curves, outside
the curve scaling, and carried to a formula backend.)

One background is fitted over the whole profile, so inside each fit interval
it is no curve of the task but a fixed part of the model the task's curves are
fitted on top of - and the background's own fit holds the fitted peaks the same
way. Unit tests: a task, a flat profile, no optimiser.
}
unit testcase_fixed_model_part;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, points_set,
    test_curve_types, fit_task, fit_loss, fit_problem_json,
    fit_task_marshalling, span_gauss_points_set;

type
    TFixedModelPartTest = class(TTestCase)
    private
        FTask: TFitTask;
        { A task over 1..75 at 10 everywhere, curve scaling on, with one span
          Gaussian picked at 40 and the fixed part 4 at every sample. }
        procedure BuildTaskWithFixedPart;
        function FixedFour: TLossDoubleArray;
        { A fixed part that follows the curves: twice their values. }
        procedure TwiceTheCurves(const ACurves: array of double;
            var AFixed: TLossDoubleArray);
    protected
        procedure TearDown; override;
    published
        procedure TheScaleIsTheDataAboveTheFixedPartOverTheCurves;
        procedure TheModelIsTheScaledCurvesPlusTheFixedPart;
        procedure EveryFigureIsMeasuredOnThatModel;
        procedure WithNoCurveTheModelIsTheFixedPartAlone;
        procedure AFixedPartCoversEverySample;
        procedure AFixedPartOfTheWrongLengthIsRefused;
        procedure TheFixedPartTravelsToAFormulaBackend;
        procedure AndOverTheWire;
        procedure AndARebuiltTaskHoldsIt;
        procedure TheScaleAboveAnyFixedIntegralIsTheSameRule;
        procedure AFixedPartThatFollowsTheCurvesIsMeasuredWithThem;
    end;

implementation

const
    SAMPLES = 75;

function TFixedModelPartTest.FixedFour: TLossDoubleArray;
var
    i: longint;
begin
    SetLength(Result, SAMPLES);
    for i := 0 to SAMPLES - 1 do
        Result[i] := 4;
end;

procedure TFixedModelPartTest.BuildTaskWithFixedPart;
var
    P: TPointsSet;
    i: longint;
begin
    FTask := TFitTask.Create(nil, True);
    P := TPointsSet.Create(nil);
    for i := 1 to SAMPLES do
        P.AddNewPoint(i, 10);
    FTask.SetProfilePointsSet(P);
    TSpanGaussPointsSet.SpanFrom := 30;
    TSpanGaussPointsSet.SpanTo := 50;
    FTask.CurveTypeId := TSpanGaussPointsSet.GetCurveTypeId;
    P := TPointsSet.Create(nil);
    P.AddNewPoint(40, 10);
    FTask.SetCurvePositions(P);
    FTask.RecreateCurves(nil);
    FTask.HoldFixedPart(FixedFour);
    FTask.ComputeProfile;
end;

procedure TFixedModelPartTest.TwiceTheCurves(const ACurves: array of double;
    var AFixed: TLossDoubleArray);
var
    i: longint;
begin
    for i := 0 to High(AFixed) do
        AFixed[i] := 2 * ACurves[i];
end;

procedure TFixedModelPartTest.TearDown;
begin
    FreeAndNil(FTask);
end;

procedure TFixedModelPartTest.TheScaleIsTheDataAboveTheFixedPartOverTheCurves;
var
    Curves: double;
    i: longint;
begin
    BuildTaskWithFixedPart;
    Curves := 0;
    for i := 0 to FTask.GetCalcProfile.PointsCount - 1 do
        Curves := Curves + FTask.GetCalcProfile.PointYCoord[i];
    AssertTrue('the curves contribute something', Curves > 0);
    //  The data is 10 at 75 samples and the fixed part 4: 450 is left for the
    //  curves. Scaling the fixed part too would leave 750.
    AssertEquals((SAMPLES * 10 - SAMPLES * 4) / Curves,
        FTask.GetScalingFactor, 1e-9);
end;

procedure TFixedModelPartTest.TheModelIsTheScaledCurvesPlusTheFixedPart;
var
    i: longint;
begin
    BuildTaskWithFixedPart;
    for i := 0 to SAMPLES - 1 do
        AssertEquals('sample ' + IntToStr(i),
            FTask.GetCalcProfile.PointYCoord[i] * FTask.GetScalingFactor + 4,
            FTask.FittedModelAt(i), 1e-9);
end;

procedure TFixedModelPartTest.EveryFigureIsMeasuredOnThatModel;
var
    Calc, Obs: TLossDoubleArray;
    Expected, Actual: TLossParts;
    i: longint;
begin
    BuildTaskWithFixedPart;
    SetLength(Calc, SAMPLES);
    SetLength(Obs, SAMPLES);
    for i := 0 to SAMPLES - 1 do
    begin
        Calc[i] := FTask.FittedModelAt(i);
        Obs[i] := 10;
    end;
    Expected := LossPartsOf(Calc, Obs, 1);
    Actual := FTask.GetLossParts;
    AssertEquals('squares', Expected.SumSq, Actual.SumSq, 1e-9);
    AssertEquals('observed', Expected.SumObs, Actual.SumObs, 1e-9);
    AssertEquals('the R-factor', LossFromParts(LOSS_KIND_RFACTOR, Expected),
        FTask.GetSqrRFactor, 1e-12);
end;

procedure TFixedModelPartTest.WithNoCurveTheModelIsTheFixedPartAlone;
var
    P: TPointsSet;
    i: longint;
begin
    FTask := TFitTask.Create(nil, True);
    P := TPointsSet.Create(nil);
    for i := 1 to SAMPLES do
        P.AddNewPoint(i, 10);
    FTask.SetProfilePointsSet(P);
    FTask.SetCurvePositions(TPointsSet.Create(nil));
    FTask.RecreateCurves(nil);
    FTask.HoldFixedPart(FixedFour);
    FTask.ComputeProfile;
    AssertEquals('every residual is 10 - 4', SAMPLES * 36.0,
        FTask.GetLossParts.SumSq, 1e-9);
end;

procedure TFixedModelPartTest.AFixedPartCoversEverySample;
begin
    //  The background is part of the model at every sample, so no sample is
    //  modelled as nothing - though the one curve reaches only 30..50.
    BuildTaskWithFixedPart;
    AssertEquals(0, Length(FTask.UncoveredSamples));
end;

procedure TFixedModelPartTest.AFixedPartOfTheWrongLengthIsRefused;
var
    Short: TLossDoubleArray;
    Raised: boolean;
begin
    BuildTaskWithFixedPart;
    SetLength(Short, 3);
    Raised := False;
    try
        FTask.HoldFixedPart(Short);
    except
        Raised := True;
    end;
    AssertTrue('one value per sample of the task, or none', Raised);
end;

procedure TFixedModelPartTest.TheFixedPartTravelsToAFormulaBackend;
var
    P: TFitProblem;
begin
    BuildTaskWithFixedPart;
    P := BuildProblemFromTask(FTask);
    AssertEquals(SAMPLES, Length(P.ProfileOffset));
    AssertEquals(4, P.ProfileOffset[SAMPLES - 1], 0);
end;

procedure TFixedModelPartTest.AndOverTheWire;
var
    P, Back: TFitProblem;
begin
    BuildTaskWithFixedPart;
    P := BuildProblemFromTask(FTask);
    AssertTrue(FitProblemFromJson(FitProblemToJson(P), Back));
    AssertEquals(SAMPLES, Length(Back.ProfileOffset));
    AssertEquals(4, Back.ProfileOffset[0], 0);
    //  And a problem without one is the message it always was.
    P.ProfileOffset := nil;
    AssertEquals(0, Pos('profileOffset', FitProblemToJson(P)));
end;

procedure TFixedModelPartTest.AndARebuiltTaskHoldsIt;
var
    Rebuilt: TFitTask;
begin
    BuildTaskWithFixedPart;
    Rebuilt := BuildTaskFromProblem(BuildProblemFromTask(FTask));
    try
        Rebuilt.ComputeProfile;
        AssertEquals(FTask.FittedModelAt(10), Rebuilt.FittedModelAt(10), 1e-9);
    finally
        Rebuilt.Free;
    end;
end;

{ THE BACKGROUND'S STEP ASKS EACH INTERVAL what its peaks would be scaled to
  over a background it is only trying (TFitService.PeaksOverBackground): the
  same rule GetScalingFactor applies to the fixed part the interval holds. }
procedure TFixedModelPartTest.TheScaleAboveAnyFixedIntegralIsTheSameRule;
var
    Curves: double;
    i: longint;
begin
    BuildTaskWithFixedPart;
    AssertEquals('over the part it holds', FTask.GetScalingFactor,
        FTask.ScalingFactorAbove(SAMPLES * 4), 1e-12);
    Curves := 0;
    for i := 0 to FTask.GetCalcProfile.PointsCount - 1 do
        Curves := Curves + FTask.GetCalcProfile.PointYCoord[i];
    AssertEquals('over another', (SAMPLES * 10 - 100) / Curves,
        FTask.ScalingFactorAbove(100), 1e-9);
    AssertEquals('none above the data: no upside-down peaks', 1,
        FTask.ScalingFactorAbove(SAMPLES * 11), 0);
end;

{ THE BACKGROUND'S OWN TASK holds the peaks, and they are rescaled with every
  background it tries: held as fixed values, the peaks it was fitted against
  were not the ones the intervals then drew over it, and the turns settled
  short of the best fit. }
procedure TFixedModelPartTest.AFixedPartThatFollowsTheCurvesIsMeasuredWithThem;
var
    Calc, Obs: TLossDoubleArray;
    Actual: TLossParts;
    i: longint;
begin
    BuildTaskWithFixedPart;
    FTask.FixedPartOf := @TwiceTheCurves;
    Actual := FTask.GetLossParts;
    SetLength(Calc, SAMPLES);
    SetLength(Obs, SAMPLES);
    for i := 0 to SAMPLES - 1 do
    begin
        AssertEquals('follows the curves at ' + IntToStr(i),
            2 * FTask.GetCalcProfile.PointYCoord[i], FTask.FixedPart[i], 1e-9);
        Calc[i] := FTask.FittedModelAt(i);
        Obs[i] := 10;
    end;
    AssertEquals('and is what was measured', LossPartsOf(Calc, Obs, 1).SumSq,
        Actual.SumSq, 1e-9);
end;

initialization
    RegisterTest('unit', TFixedModelPartTest);
end.
