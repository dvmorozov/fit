// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How wide a peak may be: no wider at half maximum than the fit
interval it is fitted in.)

A width parameter means different things in different curve types - a standard
deviation, a full width at half maximum, a half width - so its cap is set from
how wide the PEAK is per unit of it (TCurvePointsSet.FullWidthPerUnit). These
tests build instances the way the engine does (TFitTask.NewInstanceOfType) and
measure the peak itself, so a type that declares the wrong conversion fails by
name.

In the LCL-linked suite only: the engine's instance factory reaches the
user-curve units, which need the LCL.
}
unit testcase_peak_widths;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    points_set, curve_points_set, named_points_set, fit_task,
    special_curve_parameter, curve_types_singleton, int_curve_type_iterator,
    doniach_sunjic_points_set, width_curve_parameter, user_curve_parameter,
    persistent_curve_parameters, persistent_curve_parameter_container,
    user_points_set, peak_full_width, voigt_points_set;

type
    TPeakWidthTest = class(TTestCase)
    private
        FProfile: TPointsSet;
        { Curves to measure: a Gaussian of standard deviation 2 at 5, nothing,
          and a step at 5. }
        function GaussAt(const AX: double): double;
        function NothingAt(const AX: double): double;
        function StepAt(const AX: double): double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EveryPeakAtItsWidestFitsItsFitInterval;
        procedure ACapThatDependsOnTheShapeFollowsIt;
        procedure TheDoniachSunjicFullWidthIsMeasuredFromTheLine;
        procedure AWidthWhoseFullWidthCannotBeMeasuredIsNotCapped;
        procedure AUserCurvesRolesAreLimitedInItsInterval;
        procedure AUserWidthReadAsAStandardDeviationIsHeldByThePeak;
        procedure AUserWidthOfACurveWithNoPeakIsCountedAsAFullWidth;
        procedure AVoigtWithBothWidthsAtTheirCapsStillFits;
        procedure AUserWidthIsMeasuredEvenWhileItsAmplitudeIsZero;
        procedure AUserCurveWithNoPositionIsMeasuredAboutItsInterval;
        procedure AVoigtWithNoWindowHasThePlainConversions;
        procedure AGaussianThatFillsTheIntervalLeavesTheLorentzianNoRoom;
        procedure TheVoigtsRoomForEachPartFollowsOliveroAndLongbothum;
        procedure APeakIsMeasuredAtHalfItsMaximum;
        procedure WhatIsNotAPeakHasNoWidthToMeasure;
    end;

implementation

procedure TPeakWidthTest.SetUp;
var
    i: longint;
begin
    FProfile := TPointsSet.Create(nil);
    //  The same samples TCurveWindowTest uses: x = 100, 103, ... 127.
    for i := 0 to 9 do
        FProfile.AddNewPoint(100 + i * 3, 0);
end;

procedure TPeakWidthTest.TearDown;
begin
    FreeAndNil(FProfile);
end;

procedure TPeakWidthTest.EveryPeakAtItsWidestFitsItsFitInterval;
const
    //  The fit interval: 99 .. 101, sampled every 0.01.
    NarrowFrom = 99;
    NarrowTo = 101;
    //  Where the width at half maximum is measured: forty intervals wide.
    WideFrom = 60;
    WideTo = 140;
    Step = 0.005;
    //  What every other width is held at while one is measured.
    Narrowest = 1e-4;
var
    Narrow, Wide: TPointsSet;
    Task: TFitTask;
    It: ICurveTypeIterator;
    Cls: TNamedPointsSetClass;
    Capped, Measured: TCurvePointsSet;
    i, j, k, Walked: longint;
    Cap, Highest, Left, Right: double;
    Name_: string;
    Failures: string;

    function NewOne: TCurvePointsSet;
    begin
        Result := Task.NewInstanceOfType(Cls.GetCurveTypeId, 100);
    end;

    function IsWidth(ANarrow, AWide: TCurvePointsSet; AIndex: longint): boolean;
    begin
        //  A WIDTH IS WHAT THE WINDOW BOUNDS, found by asking rather than by a
        //  list: its upper bound differs between the narrow window and the
        //  wide one. A position is bounded by its window too, by the samples
        //  either side of it, and is not a width.
        Result := (ANarrow.Parameters[AIndex].Type_ <> VariablePosition) and
            (ANarrow.Parameters[AIndex].Type_ <> InvariablePosition) and
            (ANarrow.Parameters[AIndex].GetMaxValue <>
             AWide.Parameters[AIndex].GetMaxValue);
    end;

begin
    //  SELF-ENFORCING, and the rule the user chose: every peak type, with any
    //  one of its widths at the largest value its fit interval allows, is no
    //  wider at half maximum than the interval. Measured, not derived from
    //  the declared conversions, so a type that declares the wrong one fails
    //  here by name.
    Narrow := TPointsSet.Create(nil);
    Wide := TPointsSet.Create(nil);
    //  Through the constructor the engine uses: the inherited TComponent one
    //  leaves the shared-parameter list unmade.
    Task := TFitTask.Create(nil, False, True);
    Failures := '';
    Walked := 0;
    try
        i := 0;
        while NarrowFrom + i * 0.01 <= NarrowTo + 1e-9 do
        begin
            Narrow.AddNewPoint(NarrowFrom + i * 0.01, 0);
            Inc(i);
        end;
        i := 0;
        while WideFrom + i * Step <= WideTo + 1e-9 do
        begin
            Wide.AddNewPoint(WideFrom + i * Step, 0);
            Inc(i);
        end;
        It := TCurveTypesSingleton.CreateCurveTypeIterator;
        It.FirstCurveType;
        repeat
            Cls := It.GetCurrentCurveClass;
            //  NOT A USER-DEFINED TYPE: the engine builds one from the formula
            //  the user wrote, which this task has none of; its widths are
            //  limited by role and tested with a formula of their own. Named as
            //  text so the walk does not link the user-curve units.
            if (not Cls.IsBackground) and (Cls.PlacedByPointSet = '') and
               (Cls.ClassName <> 'TUserPointsSet') then
            begin
                Capped := NewOne;
                try
                    Capped.SetWindow(Narrow, 0, Narrow.PointsCount - 1);
                    for j := 0 to Capped.Parameters.Count - 1 do
                    begin
                        Measured := NewOne;
                        try
                            Measured.SetWindow(Wide, 0, Wide.PointsCount - 1);
                            if not IsWidth(Capped, Measured, j) then
                                Continue;
                            Name_ := Capped.Parameters[j].Name;
                            Cap := Capped.Parameters[j].GetMaxValue;
                            for k := 0 to Capped.Parameters.Count - 1 do
                                if (k <> j) and IsWidth(Capped, Measured, k) then
                                    Measured.ValuesByName[Capped.Parameters[k].Name] :=
                                        Narrowest;
                            Measured.ValuesByName[Name_] := Cap;
                            //  Where the engine would seed it: a new instance's
                            //  position is 0 until it is.
                            if Measured.Hasx0 then
                                Measured.x0 := 100;
                            //  A new peak's amplitude is 0, which is flat.
                            if Measured.HasA then
                                Measured.A := 1;
                            Measured.ReCalc;
                            Highest := 0;
                            for i := 0 to Measured.PointsCount - 1 do
                                Highest := Max(Highest, Measured.PointYCoord[i]);
                            //  A step rises once and stays up: it has no width
                            //  at half maximum to measure.
                            if (Measured.PointYCoord[0] >= Highest / 2) or
                               (Measured.PointYCoord[Measured.PointsCount - 1] >=
                                Highest / 2) then
                                Continue;
                            Left := NaN;
                            Right := NaN;
                            for i := 0 to Measured.PointsCount - 1 do
                                if Measured.PointYCoord[i] >= Highest / 2 then
                                begin
                                    if IsNan(Left) then
                                        Left := Measured.PointXCoord[i];
                                    Right := Measured.PointXCoord[i];
                                end;
                            Inc(Walked);
                            if Right - Left > (NarrowTo - NarrowFrom) * 1.01 + 2 * Step then
                                Failures := Failures + Format('%s, %s at %g: %g wide' +
                                    LineEnding, [Cls.GetCurveTypeName, Name_, Cap,
                                    Right - Left]);
                        finally
                            Measured.Free;
                        end;
                    end;
                finally
                    Capped.Free;
                end;
            end;
            if It.EndCurveType then
                Break;
            It.NextCurveType;
        until False;
    finally
        Task.Free;
        Wide.Free;
        Narrow.Free;
    end;
    AssertEquals('wider at half maximum than the interval 2 wide:' + LineEnding +
        Failures, '', Failures);
    AssertTrue('the walk measured the widths of the peak types: ' +
        IntToStr(Walked), Walked >= 10);
end;

procedure TPeakWidthTest.ACapThatDependsOnTheShapeFollowsIt;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
    AtStart: double;
begin
    //  How wide a Doniach-Sunjic line is per unit of sigma grows with its
    //  asymmetry, so the cap on sigma shrinks as the fit raises alpha - it is
    //  read again before every calculation, not fixed when the window is set.
    Task := TFitTask.Create(nil, False, True);
    Curve := nil;
    try
        Curve := Task.NewInstanceOfType(
            StringToGUID('{ec663a56-0e89-4bc3-91fd-f243aadb253e}'), 106);
        Curve.SetWindow(FProfile, 1, 4);
        Curve.ValuesByName['alpha'] := 0;
        Curve.ReCalc;
        AtStart := Curve.Parameters.FindByName('sigma').GetMaxValue;
        AssertEquals('a Lorentzian: half the interval 9 wide', 4.5, AtStart, 1e-6);
        Curve.ValuesByName['sigma'] := 100;
        Curve.ReCalc;
        AssertEquals('and a sigma is held there', 4.5,
            Curve.ValuesByName['sigma'], 1e-6);
        //  The fit raises alpha. The cap is read again at the next
        //  calculation, and the sigma already held comes down with it.
        Curve.ValuesByName['alpha'] := 0.3;
        Curve.ReCalc;
        AssertTrue('narrower once asymmetric: ' + FloatToStr(
            Curve.Parameters.FindByName('sigma').GetMaxValue),
            Curve.Parameters.FindByName('sigma').GetMaxValue < AtStart * 0.7);
        AssertEquals('and the held sigma follows it',
            Curve.Parameters.FindByName('sigma').GetMaxValue,
            Curve.ValuesByName['sigma'], 1e-12);
    finally
        Curve.Free;
        Task.Free;
    end;
end;

procedure TPeakWidthTest.TheDoniachSunjicFullWidthIsMeasuredFromTheLine;
begin
    //  Against the same line sampled every 5e-5 in numpy (findings.md).
    AssertEquals('a Lorentzian at alpha = 0', 2, DoniachSunjicFullWidth(0), 1e-9);
    AssertEquals('at alpha = 0.1', 2.27445, DoniachSunjicFullWidth(0.1), 1e-4);
    AssertEquals('at alpha = 0.3', 3.3535, DoniachSunjicFullWidth(0.3), 1e-4);
    AssertEquals('none at alpha = 1, where the line is flat', 0,
        DoniachSunjicFullWidth(1), 0);
end;

procedure TPeakWidthTest.AWidthWhoseFullWidthCannotBeMeasuredIsNotCapped;
begin
    //  A shape with no width at half maximum caps nothing rather than
    //  everything - and neither does a window of no extent.
    AssertTrue(IsInfinite(TWidthCurveParameter.CapFor(2, 0)));
    AssertTrue(IsInfinite(TWidthCurveParameter.CapFor(0, 1)));
    AssertEquals(4, TWidthCurveParameter.CapFor(2, 0.5), 1e-12);
end;

procedure AddUserParam(P: Curve_parameters; const AName: string;
    AType: TParameterType; AValue: double);
var
    Param: TUserCurveParameter;
begin
    Param := TUserCurveParameter.Create;
    Param.Name := AName;
    Param.Type_ := AType;
    Param.Value := AValue;
    TPersistentCurveParameterContainer(P.Params.Add).Parameter := Param;
end;

{ A user curve of AFormula with an amplitude A, a position x0 and a width W,
  built by the engine at a pick at 10 in the interval 0 .. 20. Owned by ATask. }
function UserCurveIn(ATask: TFitTask; const AFormula: string;
    AAmplitude: double = 1): TCurvePointsSet;
var
    Params: Curve_parameters;
    Profile, Positions: TPointsSet;
    i: longint;
begin
    Params := Curve_parameters.Create(nil);
    Params.Params.Clear;
    AddUserParam(Params, 'A', Amplitude, AAmplitude);
    AddUserParam(Params, 'x', Argument, 0);
    AddUserParam(Params, 'x0', InvariablePosition, 0);
    AddUserParam(Params, 'W', Width, 0.25);
    Profile := TPointsSet.Create(nil);
    for i := 0 to 40 do
        Profile.AddNewPoint(i * 0.5, 1);
    Positions := TPointsSet.Create(nil);
    Positions.AddNewPoint(10, 1);
    ATask.CurveTypeId := TUserPointsSet.GetCurveTypeId;
    ATask.SetSpecialCurve(AFormula, Params);
    ATask.SetProfilePointsSet(Profile);
    ATask.SetCurvePositions(Positions);
    ATask.RecreateCurves(nil);
    Result := TCurvePointsSet(ATask.GetCurves.Items[0]);
end;

procedure TPeakWidthTest.AUserCurvesRolesAreLimitedInItsInterval;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    //  BUILT AS THE ENGINE BUILDS A USER CURVE: the user's formula and the
    //  roles chosen in its dialog. exp(-((x-x0)/W)^2) is half its height at
    //  x - x0 = W sqrt(ln 2), so the peak is 2 sqrt(ln 2) W wide at half
    //  maximum and W is held where that is the interval's 20.
    Task := TFitTask.Create(nil, False, False);
    try
        Curve := UserCurveIn(Task, 'A*exp(-((x-x0)/W)^2)');
        Curve.ValuesByName['W'] := 100;
        Curve.ReCalc;
        AssertEquals('the width role, held where the peak is the interval wide',
            20 / (2 * Sqrt(Ln(2))), Curve.ValuesByName['W'], 1e-3);
        Curve.ValuesByName['A'] := -5;
        AssertEquals('the amplitude role, not below zero', 5,
            Curve.ValuesByName['A'], 1e-9);
    finally
        Task.Free;
    end;
end;

procedure TPeakWidthTest.AUserWidthReadAsAStandardDeviationIsHeldByThePeak;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    //  WHAT THE PROGRAM CANNOT KNOW, it measures: here W is a standard
    //  deviation, and the peak 2.3548 W wide.
    Task := TFitTask.Create(nil, False, False);
    try
        Curve := UserCurveIn(Task, 'A*exp(-((x-x0)/W)^2/2)');
        Curve.ValuesByName['W'] := 100;
        Curve.ReCalc;
        AssertEquals(20 / FULL_WIDTH_PER_STANDARD_DEVIATION,
            Curve.ValuesByName['W'], 1e-3);
    finally
        Task.Free;
    end;
end;

procedure TPeakWidthTest.AUserWidthIsMeasuredEvenWhileItsAmplitudeIsZero;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    //  The measurement is taken when the window is set, before the engine
    //  seeds the amplitude from the data - so at 0, which is flat. Taken at 1
    //  instead, it still finds a standard deviation; at 0 it would have found
    //  no peak and counted W as a full width, held at 20.
    Task := TFitTask.Create(nil, False, False);
    try
        Curve := UserCurveIn(Task, 'A*exp(-((x-x0)/W)^2/2)', 0);
        Curve.ValuesByName['W'] := 100;
        Curve.ReCalc;
        AssertEquals('measured as a standard deviation',
            20 / FULL_WIDTH_PER_STANDARD_DEVIATION, Curve.ValuesByName['W'], 1e-3);
    finally
        Task.Free;
    end;
end;

procedure TPeakWidthTest.AUserCurveWithNoPositionIsMeasuredAboutItsInterval;
var
    Task: TFitTask;
    Params: Curve_parameters;
    Profile, Positions: TPointsSet;
    Curve: TCurvePointsSet;
    i: longint;
begin
    //  No parameter places it, so the peak is looked for about the middle of
    //  its interval - where this formula puts it, at 10.
    Params := Curve_parameters.Create(nil);
    Params.Params.Clear;
    AddUserParam(Params, 'A', Amplitude, 1);
    AddUserParam(Params, 'x', Argument, 0);
    AddUserParam(Params, 'W', Width, 0.25);
    Profile := TPointsSet.Create(nil);
    for i := 0 to 40 do
        Profile.AddNewPoint(i * 0.5, 1);
    Positions := TPointsSet.Create(nil);
    Task := TFitTask.Create(nil, False, False);
    try
        Task.CurveTypeId := TUserPointsSet.GetCurveTypeId;
        Task.SetSpecialCurve('A*exp(-((x-10)/W)^2/2)', Params);
        Task.SetProfilePointsSet(Profile);
        Task.SetCurvePositions(Positions);
        Task.RecreateCurves(nil);
        AssertTrue('the formula alone is the model', Task.GetCurves.Count > 0);
        Curve := TCurvePointsSet(Task.GetCurves.Items[0]);
        Curve.ValuesByName['W'] := 100;
        Curve.ReCalc;
        AssertEquals(20 / FULL_WIDTH_PER_STANDARD_DEVIATION,
            Curve.ValuesByName['W'], 1e-3);
    finally
        Task.Free;
    end;
end;

procedure TPeakWidthTest.AVoigtWithNoWindowHasThePlainConversions;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    //  The client's copy of a curve has values and no window: nothing to
    //  leave room in, so each width converts as it would alone.
    Task := TFitTask.Create(nil, False, True);
    Curve := nil;
    try
        Curve := Task.NewInstanceOfType(
            StringToGUID('{eeed2ec3-d036-473e-81e1-8e40943d8158}'), 100);
        AssertEquals(FULL_WIDTH_PER_STANDARD_DEVIATION,
            Curve.FullWidthPerUnit('sigma'), 1e-12);
        AssertEquals(2, Curve.FullWidthPerUnit('gamma'), 1e-12);
    finally
        Curve.Free;
        Task.Free;
    end;
end;

procedure TPeakWidthTest.AGaussianThatFillsTheIntervalLeavesTheLorentzianNoRoom;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    //  sigma alone at its cap is a peak as wide as the interval; the
    //  Lorentzian then has none left and is held at the narrowest a width is.
    Task := TFitTask.Create(nil, False, True);
    Curve := nil;
    try
        Curve := Task.NewInstanceOfType(
            StringToGUID('{eeed2ec3-d036-473e-81e1-8e40943d8158}'), 106);
        Curve.SetWindow(FProfile, 1, 4);
        Curve.ValuesByName['gamma'] := 0;
        Curve.ReCalc;
        Curve.ValuesByName['sigma'] := 100;
        Curve.ReCalc;
        AssertEquals('sigma fills the interval 9 wide',
            9 / FULL_WIDTH_PER_STANDARD_DEVIATION, Curve.ValuesByName['sigma'], 1e-5);
        AssertTrue('and gamma may be no wider than the narrowest a width is: ' +
            FloatToStr(Curve.Parameters.FindByName('gamma').GetMaxValue),
            Curve.Parameters.FindByName('gamma').GetMaxValue < 1e-5);
    finally
        Curve.Free;
        Task.Free;
    end;
end;

procedure TPeakWidthTest.TheVoigtsRoomForEachPartFollowsOliveroAndLongbothum;
begin
    AssertEquals('a pure Gaussian may fill the extent', 2,
        VoigtLargestGaussFull(2, 0), 1e-12);
    //  To the published constants' rounding: 0.5346 + sqrt(0.2166) is
    //  0.999997, inside the formula's own 0.02 %.
    AssertEquals('a pure Lorentzian may fill it too', 2,
        VoigtLargestLorentzFull(2, 0), 2 * 0.0002);
    AssertEquals('a Lorentzian that fills it leaves the Gaussian none', 0,
        VoigtLargestGaussFull(2, 2.0000001), 0);
    AssertEquals('and so does one far past it, where the quadratic turns ' +
        'positive again', 0, VoigtLargestGaussFull(2, 40), 0);
    AssertEquals('a Gaussian that fills it leaves the Lorentzian none', 0,
        VoigtLargestLorentzFull(2, 2), 2 * 0.0002);
    //  Between: the pair's width is the extent.
    AssertEquals(2, 0.5346 * 1 + Sqrt(0.2166 * 1 + Sqr(VoigtLargestGaussFull(2, 1))),
        1e-12);
end;

procedure TPeakWidthTest.AUserWidthOfACurveWithNoPeakIsCountedAsAFullWidth;
var
    Task: TFitTask;
    Curve: TCurvePointsSet;
begin
    //  A formula with no peak has no width at half maximum to measure; its
    //  Width is then counted as a full width, as it was.
    Task := TFitTask.Create(nil, False, False);
    try
        Curve := UserCurveIn(Task, 'A*(x-x0)/W');
        Curve.ValuesByName['W'] := 100;
        Curve.ReCalc;
        AssertEquals(20, Curve.ValuesByName['W'], 1e-9);
    finally
        Task.Free;
    end;
end;

{ The full width at half maximum of ACurve's drawn points, by the first and
  last sample at or above half its highest. }
function DrawnFullWidth(ACurve: TCurvePointsSet): double;
var
    i: longint;
    Highest, Left, Right: double;
begin
    Highest := 0;
    for i := 0 to ACurve.PointsCount - 1 do
        Highest := Max(Highest, ACurve.PointYCoord[i]);
    Left := NaN;
    Right := NaN;
    for i := 0 to ACurve.PointsCount - 1 do
        if ACurve.PointYCoord[i] >= Highest / 2 then
        begin
            if IsNan(Left) then
                Left := ACurve.PointXCoord[i];
            Right := ACurve.PointXCoord[i];
        end;
    Result := Right - Left;
end;

procedure TPeakWidthTest.AVoigtWithBothWidthsAtTheirCapsStillFits;
var
    Task: TFitTask;
    Narrow, Wide: TPointsSet;
    Capped, Measured: TCurvePointsSet;
    i: longint;
const
    Voigt = '{eeed2ec3-d036-473e-81e1-8e40943d8158}';
begin
    //  A VOIGT'S TWO WIDTHS COMBINE: each at its own cap made a peak about
    //  1.6 times its interval. Pushed past both at once, the pair is held so
    //  the PEAK fits - in the interval 99 .. 101.
    Narrow := TPointsSet.Create(nil);
    Wide := TPointsSet.Create(nil);
    Task := TFitTask.Create(nil, False, True);
    Capped := nil;
    Measured := nil;
    try
        for i := 0 to 200 do
            Narrow.AddNewPoint(99 + i * 0.01, 0);
        for i := 0 to 16000 do
            Wide.AddNewPoint(60 + i * 0.005, 0);
        Capped := Task.NewInstanceOfType(StringToGUID(Voigt), 100);
        Capped.SetWindow(Narrow, 0, Narrow.PointsCount - 1);
        Capped.x0 := 100;
        Capped.ValuesByName['sigma'] := 100;
        Capped.ValuesByName['gamma'] := 100;
        Capped.ReCalc;
        Measured := Task.NewInstanceOfType(StringToGUID(Voigt), 100);
        Measured.SetWindow(Wide, 0, Wide.PointsCount - 1);
        Measured.x0 := 100;
        Measured.A := 1;
        Measured.ValuesByName['sigma'] := Capped.ValuesByName['sigma'];
        Measured.ValuesByName['gamma'] := Capped.ValuesByName['gamma'];
        Measured.ReCalc;
        AssertTrue(Format('sigma %g and gamma %g make a peak %g wide',
            [Capped.ValuesByName['sigma'], Capped.ValuesByName['gamma'],
             DrawnFullWidth(Measured)]),
            DrawnFullWidth(Measured) <= 2 * 1.01 + 0.01);
    finally
        Measured.Free;
        Capped.Free;
        Task.Free;
        Wide.Free;
        Narrow.Free;
    end;
end;

function TPeakWidthTest.GaussAt(const AX: double): double;
begin
    Result := 3 * Exp(-Sqr(AX - 5) / (2 * Sqr(2)));
end;

function TPeakWidthTest.NothingAt(const AX: double): double;
begin
    Result := 0 * AX;
end;

function TPeakWidthTest.StepAt(const AX: double): double;
begin
    if AX > 5 then
        Result := 1
    else
        Result := 0;
end;

procedure TPeakWidthTest.APeakIsMeasuredAtHalfItsMaximum;
begin
    AssertEquals('2 sqrt(2 ln 2) standard deviations',
        2 * FULL_WIDTH_PER_STANDARD_DEVIATION,
        MeasuredFullWidth(@GaussAt, 5, 100), 1e-9);
    AssertEquals('wherever in the span it sits',
        2 * FULL_WIDTH_PER_STANDARD_DEVIATION,
        MeasuredFullWidth(@GaussAt, 30, 100), 1e-9);
end;

procedure TPeakWidthTest.WhatIsNotAPeakHasNoWidthToMeasure;
begin
    AssertEquals('nothing', 0, MeasuredFullWidth(@NothingAt, 5, 100), 0);
    AssertEquals('a step, still up at one end', 0,
        MeasuredFullWidth(@StepAt, 5, 100), 0);
    AssertEquals('no span to look in', 0, MeasuredFullWidth(@GaussAt, 5, 0), 0);
    AssertEquals('a peak wider than the span', 0,
        MeasuredFullWidth(@GaussAt, 5, 1), 0);
end;

initialization
    RegisterTest('unit', TPeakWidthTest);
end.
