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
    exponential_background_points_set, power_law_background_points_set,
    special_curve_parameter, curve_types_singleton, int_curve_type_iterator;

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
        procedure ABaselineNoOrderCanFitFallsBackToItsMeanLevel;
        procedure AShapeThatCannotTakeTheLogFallsBackToTheLevel;
        procedure TheReferenceIsTheFirstBaselinePoint;

        procedure ACoefficientStepsInProportionToItsValue;
        procedure AndAZeroCoefficientStillSteps;

        procedure EveryShapeDeclaresItsValuesNonNegative;
        procedure ABackgroundIsNeverBelowZero;
        procedure TheLiftIsCarriedByTheLevelSoTheCoefficientsDescribeTheCurve;
        procedure ABackgroundAboveZeroIsLeftAsItIs;
        procedure TheLevelOfADecayOrAPowerLawCannotBeNegative;
        procedure EveryRegisteredBackgroundStaysAtOrAboveZero;
        procedure ALiftedBackgroundIsLiftedNoFurther;
        procedure ALiftedCurveIsExactlyWhatItsParametersGive;
        procedure AShapeWithNoLevelDrawsTheLiftAndKeepsItsCoefficients;
        procedure ACopiedLevelIsStillHeldAtZero;
        procedure NothingIsBelowZeroEvenWhenTheLevelCarriesTooLittle;
        procedure OverDataBelowZeroABackgroundMayGoBelowItToo;
        procedure AndTheLevelOfADecayMayThenBeNegative;
        procedure ACopyRemembersWhetherItsDataWentBelowZero;
    end;

implementation

type
    { A background shape a module might bring: a slope through its reference,
      with no level among its coefficients to carry a lift. Not registered. }
    TSlopeOnlyBackground = class(TBackgroundPointsSet)
    protected
        function GetNativeExpression: string; override;
    public
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
    end;

type
    { A quadratic whose level takes only half the lift it is handed, as a
      module's shape with a level that saturates might: after the recompute its
      lowest point is still well below zero, not a rounding error below it. }
    THalfLevelBackground = class(TQuadraticBackgroundPointsSet)
    protected
        function AbsorbLift(const ADelta: double): boolean; override;
    end;

function THalfLevelBackground.AbsorbLift(const ADelta: double): boolean;
begin
    ValuesByName['b0'] := ValuesByName['b0'] + ADelta / 2;
    Result := True;
end;

constructor TSlopeOnlyBackground.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    AddCoefficient('b1');
    AddReferenceParameter(0);
    InitListOfVariableParameters;
end;

function TSlopeOnlyBackground.GetNativeExpression: string;
begin
    Result := 'b1*(x-xr)';
end;

class function TSlopeOnlyBackground.GetCurveTypeName: string;
begin
    Result := 'Slope-only background (test)';
end;

class function TSlopeOnlyBackground.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{3C1E0F7A-6B2D-4E59-9A41-0D7F5B8C2E61}');
end;

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
        //  A level of 4, so the line stays above zero across the window: a
        //  background that would dip below it is lifted (see
        //  ABackgroundIsNeverBelowZero), and this test is about the slope.
        C.ValuesByName['xr'] := 2;
        C.ValuesByName['b0'] := 4;
        C.ValuesByName['b1'] := 3;
        AssertEquals('b0 at the reference', 4, ValueAt(C, 2), Eps);
        AssertEquals('4 + 3 * (5 - 2)', 13, ValueAt(C, 5), Eps);
        AssertEquals('below the reference too', 4 - 3 * 1, ValueAt(C, 1), Eps);
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

procedure TBackgroundShapeTest.ABaselineNoOrderCanFitFallsBackToItsMeanLevel;
var
    C: TNamedPointsSet;
begin
    //  THE LAST RESORT: least squares refuses every order, down to the plain
    //  level, only when it meets a value that is not a number - and the seed is
    //  then the mean, whatever that is, rather than nothing.
    C := TQuadraticBackgroundPointsSet.Create(nil);
    try
        C.SeedFromBaseline([1, 2, 3], [5, NaN, 7]);
        AssertTrue('the mean level, NaN and all', IsNan(C.ValuesByName['b0']));
        AssertEquals('and no slope invented', 0, C.ValuesByName['b1'], 0);
        AssertEquals('nor a curvature', 0, C.ValuesByName['b2'], 0);
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

{ ---- a background is a count, and is never below zero ---- }

procedure TBackgroundShapeTest.EveryShapeDeclaresItsValuesNonNegative;
var
    Cls: TNamedPointsSetClass;
begin
    for Cls in Shapes do
        AssertTrue(Cls.GetCurveTypeName, Cls.ValuesAreNonNegative);
end;

procedure TBackgroundShapeTest.ABackgroundIsNeverBelowZero;
var
    C: TNamedPointsSet;
    i: longint;
    Lowest: double;
begin
    //  THE SHAPE THAT PROMPTED THIS, scaled to this window (x = 1 .. 11): a
    //  parabola opening upwards whose lowest point, at x = 5, is 13 below zero.
    //  Fitted beside a peak that had grown wider than its interval, a quadratic
    //  background went to -919 to cancel the peak's hump.
    C := Built(TQuadraticBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 1;
        C.ValuesByName['b0'] := -5;
        C.ValuesByName['b1'] := -4;
        C.ValuesByName['b2'] := 0.5;
        C.ReCalc;
        Lowest := Infinity;
        for i := 0 to C.PointsCount - 1 do
            Lowest := Min(Lowest, C.PointYCoord[i]);
        AssertEquals('it is lifted until its lowest point touches zero', 0,
            Lowest, Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheLiftIsCarriedByTheLevelSoTheCoefficientsDescribeTheCurve;
var
    C: TNamedPointsSet;
begin
    //  The parameters the user reads are the curve that is drawn: the lift is
    //  added to the level, not kept somewhere the table does not show.
    C := Built(TQuadraticBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 1;
        C.ValuesByName['b0'] := -5;
        C.ValuesByName['b1'] := -4;
        C.ValuesByName['b2'] := 0.5;
        //  -5 - 4 * 4 + 0.5 * 16 = -13 at x = 5, so the level rises by 13.
        AssertEquals('drawn at the reference', 8, ValueAt(C, 1), Eps);
        AssertEquals('the level says so', 8, C.ValuesByName['b0'], Eps);
        AssertEquals('the slope is kept', -4, C.ValuesByName['b1'], Eps);
        AssertEquals('the curvature is kept', 0.5, C.ValuesByName['b2'], Eps);
        AssertEquals('8 - 4 * 8 + 0.5 * 64', 8, ValueAt(C, 9), Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.ABackgroundAboveZeroIsLeftAsItIs;
var
    C: TNamedPointsSet;
begin
    C := Built(TQuadraticBackgroundPointsSet);
    try
        C.ValuesByName['xr'] := 2;
        C.ValuesByName['b0'] := 1;
        C.ValuesByName['b1'] := -1;
        C.ValuesByName['b2'] := 0.5;
        //  Lowest at x = 3: 1 - 1 + 0.5 = 0.5.
        AssertEquals(0.5, ValueAt(C, 3), Eps);
        AssertEquals('the level is untouched', 1, C.ValuesByName['b0'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.TheLevelOfADecayOrAPowerLawCannotBeNegative;
var
    Cls: TNamedPointsSetClass;
    C: TNamedPointsSet;
begin
    //  Their sign IS the sign of b0, so the bound is on the parameter, where a
    //  formula backend is handed it too - a lifted upside-down decay would be
    //  non-negative and still not a decay.
    for Cls in [TNamedPointsSetClass(TExponentialBackgroundPointsSet),
        TNamedPointsSetClass(TPowerLawBackgroundPointsSet)] do
    begin
        C := Built(Cls);
        try
            C.ValuesByName['b0'] := -3;
            AssertEquals(Cls.GetCurveTypeName + ': held at zero', 0,
                C.ValuesByName['b0'], Eps);
            AssertEquals(Cls.GetCurveTypeName + ': and says so', 0,
                C.Parameters.FindByName('b0').GetMinValue, Eps);
        finally
            C.Free;
        end;
    end;
end;

procedure TBackgroundShapeTest.EveryRegisteredBackgroundStaysAtOrAboveZero;
var
    It: ICurveTypeIterator;
    Cls: TNamedPointsSetClass;
    C: TNamedPointsSet;
    i, Walked: longint;
begin
    //  SELF-ENFORCING: every background shape registered - including the next
    //  one, a module's among them - is driven with every coefficient negative,
    //  and must still draw nothing below zero. Every curve unit registers its
    //  type when it is linked, so the walk covers whatever this binary holds;
    //  curve_type_registration is not used, because it brings the user-curve
    //  dialogs and with them the LCL, which the plain-FPC suite cannot link.
    Walked := 0;
    It := TCurveTypesSingleton.CreateCurveTypeIterator;
    It.FirstCurveType;
    repeat
        Cls := It.GetCurrentCurveClass;
        if Cls.IsBackground then
        begin
            Inc(Walked);
            AssertTrue(Cls.GetCurveTypeName + ' declares its values non-negative',
                Cls.ValuesAreNonNegative);
            C := Built(Cls);
            try
                for i := 0 to C.Parameters.Count - 1 do
                    if C.Parameters[i].Type_ = Variable then
                        C.ValuesByName[C.Parameters[i].Name] := -3 - i;
                C.ReCalc;
                for i := 0 to C.PointsCount - 1 do
                    AssertTrue(Format('%s at x = %g: %g', [Cls.GetCurveTypeName,
                        C.PointXCoord[i], C.PointYCoord[i]]),
                        C.PointYCoord[i] >= -Eps);
            finally
                C.Free;
            end;
        end;
        if It.EndCurveType then
            Break;
        It.NextCurveType;
    until False;
    AssertTrue('the walk reached the backgrounds', Walked >= Length(Shapes));
end;

procedure TBackgroundShapeTest.ALiftedBackgroundIsLiftedNoFurther;
var
    C: TNamedPointsSet;
    Profile: TPointsSet;
    i, k: longint;
    Level, Lowest: double;
begin
    //  A FIXED POINT. A background that touches zero is rebuilt on every edit
    //  and every open; recomputed, its lowest point comes back as a rounding
    //  error either side of zero, and lifting by that crept the level by a few
    //  units in the last place every time - so a project saved and reopened was
    //  not the project saved. The shape and the grid are the ones that prompted
    //  the lift: 96.2 .. 98.2 sampled every 0.02.
    Profile := TPointsSet.Create(nil);
    C := TQuadraticBackgroundPointsSet.Create(nil);
    try
        for i := 0 to 100 do
            Profile.AddNewPoint(96.2 + i * 0.02, 0);
        C.SetWindow(Profile, 0, Profile.PointsCount - 1);
        C.ValuesByName['xr'] := 96.2;
        C.ValuesByName['b1'] := -1751.694201380001;
        C.ValuesByName['b2'] := 919.1055347533206;
        //  Many levels, because whether the recomputed lowest point lands a
        //  rounding error below zero or above it depends on the digits.
        for k := 0 to 199 do
        begin
            C.ValuesByName['b0'] := -451.537265121659 - k * 0.3713;
            C.ReCalc;
            Level := C.ValuesByName['b0'];
            for i := 1 to 3 do
            begin
                //  As a rebuild does: the level the table shows, written back.
                C.ValuesByName['b0'] := Level;
                C.ReCalc;
                AssertEquals(Format('level %d, rebuild %d', [k, i]), Level,
                    C.ValuesByName['b0'], 0);
            end;
        end;
        Lowest := Infinity;
        for i := 0 to C.PointsCount - 1 do
            Lowest := Min(Lowest, C.PointYCoord[i]);
        AssertTrue('and nothing is below zero: ' + FloatToStr(Lowest), Lowest >= 0);
    finally
        C.Free;
        Profile.Free;
    end;
end;

procedure TBackgroundShapeTest.ALiftedCurveIsExactlyWhatItsParametersGive;
var
    Lifted, Fresh: TNamedPointsSet;
    Profile: TPointsSet;
    i, j, k: longint;
begin
    //  BIT FOR BIT. A project is saved as parameters and reopened by evaluating
    //  them, so points that came from "raw plus the lift" rather than from the
    //  lifted parameters reopen a rounding error away - and the model-wide
    //  scaling factor carries that into every curve's integral.
    Profile := TPointsSet.Create(nil);
    try
        for i := 0 to 100 do
            Profile.AddNewPoint(96.2 + i * 0.02, 0);
        for k := 0 to 199 do
        begin
            Lifted := TQuadraticBackgroundPointsSet.Create(nil);
            Fresh := TQuadraticBackgroundPointsSet.Create(nil);
            try
                Lifted.SetWindow(Profile, 0, Profile.PointsCount - 1);
                Fresh.SetWindow(Profile, 0, Profile.PointsCount - 1);
                Lifted.ValuesByName['xr'] := 96.2;
                Lifted.ValuesByName['b0'] := -451.537265121659 - k * 0.3713;
                Lifted.ValuesByName['b1'] := -1751.694201380001;
                Lifted.ValuesByName['b2'] := 919.1055347533206;
                Lifted.ReCalc;
                for j := 0 to Lifted.Parameters.Count - 1 do
                    if Lifted.Parameters[j].IsNumeric then
                        Fresh.ValuesByName[Lifted.Parameters[j].Name] :=
                            Lifted.Parameters[j].Value;
                Fresh.ReCalc;
                for i := 0 to Lifted.PointsCount - 1 do
                    AssertEquals(Format('level %d, sample %d', [k, i]),
                        Fresh.PointYCoord[i], Lifted.PointYCoord[i], 0);
            finally
                Fresh.Free;
                Lifted.Free;
            end;
        end;
    finally
        Profile.Free;
    end;
end;

procedure TBackgroundShapeTest.AShapeWithNoLevelDrawsTheLiftAndKeepsItsCoefficients;
var
    C: TNamedPointsSet;
begin
    //  Nothing among its parameters can carry the lift, so the drawn curve
    //  carries it and the slope stays what was asked for.
    C := Built(TSlopeOnlyBackground);
    try
        C.ValuesByName['xr'] := 1;
        C.ValuesByName['b1'] := -2;
        //  -2 * (x - 1) over x = 1 .. 11 is lowest at x = 11: -20.
        AssertEquals('lifted at the far end', 0, ValueAt(C, 11), Eps);
        AssertEquals('and by the same at the reference', 20, ValueAt(C, 1), Eps);
        AssertEquals('the slope is kept', -2, C.ValuesByName['b1'], Eps);
    finally
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.ACopiedLevelIsStillHeldAtZero;
var
    C: TNamedPointsSet;
    P: TSpecialCurveParameter;
begin
    //  The service collects curves by copying them, parameters and all.
    C := Built(TExponentialBackgroundPointsSet);
    P := nil;
    try
        P := C.Parameters.FindByName('b0').CreateCopy;
        P.Value := -7;
        AssertEquals(0, P.Value, Eps);
        AssertEquals(0, P.GetMinValue, Eps);
        AssertEquals('b0', P.Name);
    finally
        P.Free;
        C.Free;
    end;
end;

procedure TBackgroundShapeTest.NothingIsBelowZeroEvenWhenTheLevelCarriesTooLittle;
var
    C: TNamedPointsSet;
    i: longint;
begin
    //  THE GUARANTEE DOES NOT REST ON THE TYPE. Whatever a type's AbsorbLift
    //  does, what is drawn and summed is never below zero.
    C := Built(THalfLevelBackground);
    try
        C.ValuesByName['xr'] := 1;
        C.ValuesByName['b0'] := -5;
        C.ValuesByName['b1'] := -4;
        C.ValuesByName['b2'] := 0.5;
        C.ReCalc;
        for i := 0 to C.PointsCount - 1 do
            AssertTrue(Format('x = %g: %g', [C.PointXCoord[i], C.PointYCoord[i]]),
                C.PointYCoord[i] >= 0);
    finally
        C.Free;
    end;
end;

{ A copy of FProfile whose sample at x = 6 is below zero: data with a
  background already taken off, or a difference, look like this. }
function ProfileDippingBelowZero(AFrom: TPointsSet): TPointsSet;
var
    i: longint;
begin
    Result := TPointsSet.Create(nil);
    for i := 0 to AFrom.PointsCount - 1 do
        if Abs(AFrom.PointXCoord[i] - 6) < 1e-9 then
            Result.AddNewPoint(AFrom.PointXCoord[i], -0.5)
        else
            Result.AddNewPoint(AFrom.PointXCoord[i], 1);
end;

procedure TBackgroundShapeTest.OverDataBelowZeroABackgroundMayGoBelowItToo;
var
    C: TNamedPointsSet;
    Data: TPointsSet;
begin
    //  The floor is the data's: where the measurement itself goes below zero,
    //  holding its background above zero would refuse the fit it needs.
    Data := ProfileDippingBelowZero(FProfile);
    C := TQuadraticBackgroundPointsSet.Create(nil);
    try
        C.SetWindow(Data, 0, Data.PointsCount - 1);
        C.ValuesByName['xr'] := 1;
        C.ValuesByName['b0'] := -5;
        C.ValuesByName['b1'] := -4;
        C.ValuesByName['b2'] := 0.5;
        AssertEquals('-5 - 4 * 4 + 0.5 * 16 at x = 5', -13, ValueAt(C, 5), Eps);
        AssertEquals('the level as typed', -5, C.ValuesByName['b0'], Eps);
    finally
        C.Free;
        Data.Free;
    end;
end;

procedure TBackgroundShapeTest.AndTheLevelOfADecayMayThenBeNegative;
var
    C: TNamedPointsSet;
    Data: TPointsSet;
begin
    Data := ProfileDippingBelowZero(FProfile);
    C := TExponentialBackgroundPointsSet.Create(nil);
    try
        C.SetWindow(Data, 0, Data.PointsCount - 1);
        C.ValuesByName['b0'] := -3;
        AssertEquals(-3, C.ValuesByName['b0'], Eps);
        AssertTrue('and says it has no floor',
            IsInfinite(C.Parameters.FindByName('b0').GetMinValue));
    finally
        C.Free;
        Data.Free;
    end;
end;

procedure TBackgroundShapeTest.ACopyRemembersWhetherItsDataWentBelowZero;
var
    C, Copy_: TNamedPointsSet;
    Data: TPointsSet;
begin
    //  The service collects copies, and the copy is what a formula backend is
    //  described from.
    Data := ProfileDippingBelowZero(FProfile);
    C := TQuadraticBackgroundPointsSet.Create(nil);
    Copy_ := nil;
    try
        C.SetWindow(Data, 0, Data.PointsCount - 1);
        Copy_ := TNamedPointsSet(C.GetCopy);
        Copy_.ValuesByName['xr'] := 1;
        Copy_.ValuesByName['b0'] := -5;
        Copy_.ValuesByName['b1'] := -4;
        Copy_.ValuesByName['b2'] := 0.5;
        AssertEquals(-13, ValueAt(Copy_, 5), Eps);
        AssertFalse('nor is it kept above zero', Copy_.KeepsAboveZero);
        AssertTrue('and over data that stay above zero, it is',
            TQuadraticBackgroundPointsSet.ValuesAreNonNegative);
    finally
        Copy_.Free;
        C.Free;
        Data.Free;
    end;
end;

initialization
    RegisterTest('unit', TBackgroundShapeTest);
end.
