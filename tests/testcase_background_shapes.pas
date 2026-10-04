// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The background shapes: what each computes, how each seeds itself, and
what each declares about itself.)

A BACKGROUND SHAPE IS A CURVE WITH NO POSITION. It is placed under the peaks of
a fit interval rather than at a pick, so nothing in it may take a role the
engine reads as a peak's - no position, no amplitude by role or by NAME (a
parameter called "A" is an amplitude to the engine, whatever the class meant),
no width. Its starting values come from the data's own baseline instead
(SeedFromBaseline), which is what these tests pin for every shape.

Plain objects throughout: a curve, a window over a synthetic profile, and
numbers written out by hand - the golden values are arithmetic, not a second
call to the code under test.
}
unit testcase_background_shapes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    points_set, curve_points_set, named_points_set, gauss_points_set,
    background_points_set, polynomial_background_points_set,
    exponential_background_points_set, power_law_background_points_set;

type
    TBackgroundShapeTest = class(TTestCase)
    private
        FProfile: TPointsSet;
        { A curve of ACls over the whole of FProfile, with values set by name. }
        function Built(ACls: TNamedPointsSetClass): TNamedPointsSet;
        function ValueAt(ACurve: TNamedPointsSet; AX: double): double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheConstantIsItsLevelEverywhere;
        procedure TheLinearRisesByItsSlopeFromTheReference;
        procedure TheQuadraticCurvesAboutTheReference;
        procedure TheExponentialFallsByEOverOneTau;
        procedure ThePowerLawScalesWithXOverTheReference;

        procedure EveryShapeIsABackgroundAndAPeakIsNot;
        procedure OnlyThePowerLawNeedsPositiveX;
        procedure NoShapeTakesAPeaksRole;
        procedure EveryShapeHasAFormula;

        procedure TheConstantSeedsToTheMeanLevel;
        procedure TheLinearSeedsToTheLineThroughItsBaseline;
        procedure TheQuadraticSeedsToTheParabolaThroughItsBaseline;
        procedure TheExponentialSeedsToTheDecayThroughItsBaseline;
        procedure ThePowerLawSeedsToTheLawThroughItsBaseline;
        procedure ASeedWithTooFewPointsFallsBackToTheLevel;
        procedure AShapeThatCannotTakeTheLogFallsBackToTheLevel;
        procedure TheReferenceIsTheFirstBaselinePoint;

        procedure ACoefficientStepsInProportionToItsValue;
        procedure AndAZeroCoefficientStillSteps;
    end;

implementation

const
    Eps = 1e-6;

procedure TBackgroundShapeTest.SetUp;
var
    i: longint;
begin
    FProfile := TPointsSet.Create(nil);
    for i := 0 to 20 do
        FProfile.AddNewPoint(1 + i * 0.5, 0);
end;

procedure TBackgroundShapeTest.TearDown;
begin
    FreeAndNil(FProfile);
end;

function TBackgroundShapeTest.Built(ACls: TNamedPointsSetClass): TNamedPointsSet;
begin
    Result := ACls.Create(nil);
    Result.SetWindow(FProfile, 0, FProfile.PointsCount - 1);
end;

function TBackgroundShapeTest.ValueAt(ACurve: TNamedPointsSet; AX: double): double;
var
    i: longint;
begin
    ACurve.ReCalc;
    for i := 0 to ACurve.PointsCount - 1 do
        if Abs(ACurve.PointXCoord[i] - AX) < 1e-9 then
            Exit(ACurve.PointYCoord[i]);
    Fail('no sample at ' + FloatToStr(AX));
    Result := NaN;
end;

procedure TBackgroundShapeTest.TheConstantIsItsLevelEverywhere;
var
    C: TNamedPointsSet;
begin
    C := Built(TConstantBackgroundPointsSet);
    try
        C.ValuesByName['b0'] := 5;
        AssertEquals('at the start', 5, ValueAt(C, 1), Eps);
        AssertEquals('in the middle', 5, ValueAt(C, 6), Eps);
        AssertEquals('at the end', 5, ValueAt(C, 11), Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheLinearRisesByItsSlopeFromTheReference;
var
    C: TNamedPointsSet;
begin
    C := Built(TLinearBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 2;
        C.ValuesByName['b0'] := 1;
        C.ValuesByName['b1'] := 3;
        AssertEquals('b0 at the reference', 1, ValueAt(C, 2), Eps);
        AssertEquals('1 + 3 * (5 - 2)', 10, ValueAt(C, 5), Eps);
        AssertEquals('below the reference too', 1 - 3 * 1, ValueAt(C, 1), Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheQuadraticCurvesAboutTheReference;
var
    C: TNamedPointsSet;
begin
    C := Built(TQuadraticBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 2;
        C.ValuesByName['b0'] := 1;
        C.ValuesByName['b1'] := -1;
        C.ValuesByName['b2'] := 0.5;
        //  1 - (6 - 2) + 0.5 * (6 - 2)^2 = 1 - 4 + 8
        AssertEquals(5, ValueAt(C, 6), Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheExponentialFallsByEOverOneTau;
var
    C: TNamedPointsSet;
begin
    C := Built(TExponentialBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 1;
        C.ValuesByName['b0'] := 10;
        C.ValuesByName['tau'] := 2;
        AssertEquals('b0 at the reference', 10, ValueAt(C, 1), Eps);
        AssertEquals('b0 / e one tau later', 10 / Exp(1), ValueAt(C, 3), Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.ThePowerLawScalesWithXOverTheReference;
var
    C: TNamedPointsSet;
begin
    C := Built(TPowerLawBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 2;
        C.ValuesByName['b0'] := 4;
        C.ValuesByName['p'] := -1.5;
        AssertEquals('b0 at the reference', 4, ValueAt(C, 2), Eps);
        //  4 * (8 / 2)^-1.5 = 4 / 8
        AssertEquals(0.5, ValueAt(C, 8), Eps);
    finally
        C.Free;
    end;
end;

function Shapes: specialize TArray<TNamedPointsSetClass>;
begin
    Result := [TConstantBackgroundPointsSet, TLinearBackgroundPointsSet,
        TQuadraticBackgroundPointsSet, TExponentialBackgroundPointsSet,
        TPowerLawBackgroundPointsSet];
end;

procedure TBackgroundShapeTest.EveryShapeIsABackgroundAndAPeakIsNot;
var
    Cls: TNamedPointsSetClass;
begin
    for Cls in Shapes do
        AssertTrue(Cls.GetCurveTypeName + ' is a background', Cls.IsBackground);
    AssertFalse('a Gaussian is not', TGaussPointsSet.IsBackground);
end;

procedure TBackgroundShapeTest.OnlyThePowerLawNeedsPositiveX;
var
    Cls: TNamedPointsSetClass;
begin
    for Cls in Shapes do
        AssertEquals(Cls.GetCurveTypeName,
            Cls = TPowerLawBackgroundPointsSet, Cls.ArgumentMustBePositive);
    AssertFalse('nor does a peak', TGaussPointsSet.ArgumentMustBePositive);
end;

procedure TBackgroundShapeTest.NoShapeTakesAPeaksRole;
var
    Cls: TNamedPointsSetClass;
    C: TNamedPointsSet;
begin
    //  EACH OF THESE IS READ BY THE ENGINE AS "A PEAK": seeded from a pick,
    //  deleted by curve reduction when small, drawn with a position marker. A
    //  background answering any of them would be treated as one.
    for Cls in Shapes do
    begin
        C := Cls.Create(nil);
        try
            AssertFalse(Cls.GetCurveTypeName + ' has no position', C.Hasx0);
            AssertFalse(Cls.GetCurveTypeName + ' has no amplitude', C.HasA);
            AssertFalse(Cls.GetCurveTypeName + ' has no width', C.HasSigma);
        finally
            C.Free;
        end;
    end;
end;

procedure TBackgroundShapeTest.EveryShapeHasAFormula;
var
    Cls: TNamedPointsSetClass;
    C: TNamedPointsSet;
begin
    //  What lets the formula backends fit it (see fit_task_marshalling).
    for Cls in Shapes do
    begin
        C := Cls.Create(nil);
        try
            AssertTrue(Cls.GetCurveTypeName, C.GetCurveExpression <> '');
            AssertTrue(Cls.GetCurveTypeName + ' says so', Cls.IsAnalytic);
        finally
            C.Free;
        end;
    end;
end;

procedure TBackgroundShapeTest.TheConstantSeedsToTheMeanLevel;
var
    C: TNamedPointsSet;
begin
    C := TConstantBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline([1, 5, 9], [10, 12, 14]);
        AssertEquals(12, C.ValuesByName['b0'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheLinearSeedsToTheLineThroughItsBaseline;
var
    C: TNamedPointsSet;
begin
    C := TLinearBackgroundPointsSet.Create(nil);
    try
        //  y = 7 + 2 (x - 1)
        C.SeedFromBaseline([1, 4, 6], [7, 13, 17]);
        AssertEquals('reference', 1, C.ValuesByName['xr'], Eps);
        AssertEquals('level', 7, C.ValuesByName['b0'], Eps);
        AssertEquals('slope', 2, C.ValuesByName['b1'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheQuadraticSeedsToTheParabolaThroughItsBaseline;
var
    C: TNamedPointsSet;
    X, Y: array of double;
    i: longint;
begin
    SetLength(X, 6);
    SetLength(Y, 6);
    for i := 0 to 5 do
    begin
        X[i] := 2 + i;
        Y[i] := 3 - 0.5 * (X[i] - 2) + 0.25 * Sqr(X[i] - 2);
    end;
    C := TQuadraticBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline(X, Y);
        AssertEquals('level', 3, C.ValuesByName['b0'], Eps);
        AssertEquals('slope', -0.5, C.ValuesByName['b1'], Eps);
        AssertEquals('curvature', 0.25, C.ValuesByName['b2'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheExponentialSeedsToTheDecayThroughItsBaseline;
var
    C: TNamedPointsSet;
    X, Y: array of double;
    i: longint;
begin
    SetLength(X, 5);
    SetLength(Y, 5);
    for i := 0 to 4 do
    begin
        X[i] := 3 + 2 * i;
        Y[i] := 500 * Exp(-(X[i] - 3) / 4);
    end;
    C := TExponentialBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline(X, Y);
        AssertEquals('reference', 3, C.ValuesByName['xr'], Eps);
        AssertEquals('level', 500, C.ValuesByName['b0'], 1e-4);
        AssertEquals('decay length', 4, C.ValuesByName['tau'], 1e-6);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.ThePowerLawSeedsToTheLawThroughItsBaseline;
var
    C: TNamedPointsSet;
    X, Y: array of double;
    i: longint;
begin
    SetLength(X, 5);
    SetLength(Y, 5);
    for i := 0 to 4 do
    begin
        X[i] := 2 + 3 * i;
        Y[i] := 80 * Power(X[i] / 2, -0.75);
    end;
    C := TPowerLawBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline(X, Y);
        AssertEquals('reference', 2, C.ValuesByName['xr'], Eps);
        AssertEquals('level', 80, C.ValuesByName['b0'], 1e-4);
        AssertEquals('exponent', -0.75, C.ValuesByName['p'], 1e-6);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.ASeedWithTooFewPointsFallsBackToTheLevel;
var
    C: TNamedPointsSet;
begin
    //  ONE POINT CANNOT FIX A SLOPE. The level it gives is still the best
    //  start there is, and a zero slope is a start the fit can leave.
    C := TQuadraticBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline([4], [9]);
        AssertEquals('level', 9, C.ValuesByName['b0'], Eps);
        AssertEquals('no slope', 0, C.ValuesByName['b1'], Eps);
        AssertEquals('no curvature', 0, C.ValuesByName['b2'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.AShapeThatCannotTakeTheLogFallsBackToTheLevel;
var
    C: TNamedPointsSet;
begin
    //  A baseline at or below zero has no logarithm. The exponential then
    //  starts flat at the mean level rather than at a NaN.
    C := TExponentialBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline([1, 2, 3], [-2, 0, 2]);
        AssertEquals('level', 0, C.ValuesByName['b0'], Eps);
        AssertFalse('a decay length that is a number',
            IsNan(C.ValuesByName['tau']) or IsInfinite(C.ValuesByName['tau']));
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheReferenceIsTheFirstBaselinePoint;
var
    C: TNamedPointsSet;
begin
    //  Whatever order the points come in - ProposeBackgroundPoints gives them
    //  outward from the minimum, not left to right.
    C := TLinearBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline([6, 1, 4], [17, 7, 13]);
        AssertEquals(1, C.ValuesByName['xr'], Eps);
        AssertEquals(7, C.ValuesByName['b0'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.ACoefficientStepsInProportionToItsValue;
var
    C: TNamedPointsSet;
    i: longint;
begin
    //  THE SIMPLEX STEPS BY AN ABSOLUTE AMOUNT. A level of 780 started with a
    //  step of 0.1 would take the whole fit to walk to the right scale.
    C := TConstantBackgroundPointsSet.Create(nil);
    try
        C.ValuesByName['b0'] := 780;
        for i := 0 to C.VariableCount - 1 do
            C.InitVariationStep(i);
        AssertEquals(78, C.VariationSteps[0], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.AndAZeroCoefficientStillSteps;
var
    C: TNamedPointsSet;
    i: longint;
begin
    C := TLinearBackgroundPointsSet.Create(nil);
    try
        C.ValuesByName['b0'] := 0;
        C.ValuesByName['b1'] := 0;
        for i := 0 to C.VariableCount - 1 do
        begin
            C.InitVariationStep(i);
            AssertTrue('a step that moves', C.VariationSteps[i] > 0);
        end;
    finally
        C.Free;
    end;
end;

initialization
    RegisterTest('unit', TBackgroundShapeTest);
end.
