// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where a removed curve's amplitude goes in a reduction by BIC.)

A reduction judges each removal as the model stands, handing the removed
curve's amplitude to the nearest curve first (TFitTask.HandAmplitudeOver) so a
crowd of curves on one peak does not look worse for losing one. Handed to a
curve FAR from it, the amplitude is not what the removed curve held but
intensity added somewhere else, and a model that has not been re-fitted always
has somewhere that wants some: on Tb2Fe17 the last curve of a bump was removed
because its amplitude, given to a curve 0.34 degrees away, scored better, and
the bump was left without a curve (decisions.md, "Where a removed curve's
amplitude goes").

The model here is one Gaussian of 100 fitted at 60 - short by 40, as a crowd's
heir is - and a small curve beside it. The data is the Gaussian alone.
}
unit testcase_hand_over;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fit_task, points_set,
    curve_points_set, test_curve_types, curve_types_singleton,
    int_curve_type_selector, SimpMath;

type
    THandOverTest = class(TTestCase)
    private
        FTask: TFitTask;
        { A task over a Gaussian of 100 at 10, sigma 1.5, with a curve picked at
          10 of height 60 and one at ASmallAt of height 10. }
        procedure Given(ASmallAt: double);
        function CurveNear(AX: double): TCurvePointsSet;
    protected
        procedure TearDown; override;
    published
        procedure ACurveNextToTheRemovedOneTakesItsAmplitude;
        procedure ACurveFarFromTheRemovedOneDoesNot;
        procedure TheRunLogLineSaysWhereItWentOrWhyNot;
    end;

implementation

type
    TQuiet = class
        procedure NoOp;
    end;

procedure TQuiet.NoOp;
begin
end;

var
    Quiet: TQuiet;

procedure THandOverTest.Given(ASmallAt: double);
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
    Positions.AddNewPoint(10, 60);
    Positions.AddNewPoint(ASmallAt, 10);
    FTask := TFitTask.Create(nil, False);
    FTask.ServerShowCurMin := @Quiet.NoOp;
    FTask.ServerDoneProc := @Quiet.NoOp;
    FTask.SetProfilePointsSet(Profile);
    FTask.SetCurvePositions(Positions);
    FTask.RecreateCurves(nil);
    FTask.BegIndex := 0;
    FTask.EndIndex := FTask.GetCalcProfile.PointsCount - 1;
    //  The curves' values, as a reduction has them when it judges a removal.
    FTask.ComputeProfile;
    FTask.MeasureReductionNoise;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
end;

procedure THandOverTest.TearDown;
begin
    FreeAndNil(FTask);
    inherited TearDown;
end;

function THandOverTest.CurveNear(AX: double): TCurvePointsSet;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to FTask.GetCurves.Count - 1 do
        if Abs(TCurvePointsSet(FTask.GetCurves.Items[i]).x0 - AX) < 0.01 then
            Result := TCurvePointsSet(FTask.GetCurves.Items[i]);
    AssertTrue(Format('a curve at %g', [AX]), Assigned(Result));
end;

procedure THandOverTest.ACurveNextToTheRemovedOneTakesItsAmplitude;
var
    Heir, Small: TCurvePointsSet;
    Before: double;
begin
    //  Beside it - 0.2 away, a sample - the heir is short by 40 and the 10 it
    //  is handed brings it nearer the data: the hand-over scores better.
    Given(10.2);
    Heir := CurveNear(10);
    Small := CurveNear(10.2);
    Before := Heir.A;
    FTask.GetCurves.Extract(Small);
    FTask.ComputeProfile;
    FTask.HandAmplitudeOver(Small, Infinity);
    AssertEquals('the heir took the 10', Before + Small.A, Heir.A, 1e-9);
    Small.Free;
end;

procedure THandOverTest.ACurveFarFromTheRemovedOneDoesNot;
var
    Heir, Small: TCurvePointsSet;
    Before: double;
begin
    //  EIGHT AWAY - past two widths and three samples - the same 10 would
    //  still bring the short heir nearer the data, which is the whole mistake:
    //  it is intensity added at 10, not what the curve at 18 held.
    Given(18);
    Heir := CurveNear(10);
    Small := CurveNear(18);
    Before := Heir.A;
    FTask.GetCurves.Extract(Small);
    FTask.ComputeProfile;
    FTask.HandAmplitudeOver(Small, Infinity);
    AssertEquals('the heir is as it was', Before, Heir.A, 1e-12);
    Small.Free;
end;

procedure THandOverTest.TheRunLogLineSaysWhereItWentOrWhyNot;
var
    Small: TCurvePointsSet;
begin
    Given(18);
    Small := CurveNear(18);
    FTask.GetCurves.Extract(Small);
    FTask.ComputeProfile;
    FTask.HandAmplitudeOver(Small, Infinity);
    AssertTrue('not handed over, and the distance: ' + FTask.HandOverNote,
        (Pos('not handed over', FTask.HandOverNote) > 0) and
        (Pos('8.0000 away', FTask.HandOverNote) > 0));
    Small.Free;
    FreeAndNil(FTask);
    Given(10.2);
    Small := CurveNear(10.2);
    FTask.GetCurves.Extract(Small);
    FTask.ComputeProfile;
    FTask.HandAmplitudeOver(Small, Infinity);
    AssertTrue('handed over: ' + FTask.HandOverNote,
        Pos('was handed to the nearest curve', FTask.HandOverNote) > 0);
    Small.Free;
end;

initialization
    Quiet := TQuiet.Create;
    RegisterTest('unit', THandOverTest);
end.
