// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A curve's window: the samples it covers, and where they sit.)

A curve holds only the stretch of the profile it covers, so its own index and the
profile's are no longer the same number. Everything that sums a curve into a
profile translates through the offset recorded when the window was made.

An off-by-one there is the one mistake that would be silent - the fit still
converges, to a slightly wrong answer - so every branch of the window API is
enumerated here rather than left to be exercised incidentally by a fit. The fit
path is covered too, but by integration tests, which answer "does it work" rather
than "is this case checked".

TGaussPointsSet stands in for "a curve": TCurvePointsSet is abstract, and the
window belongs to the base class, so any concrete type exercises the same code.
}
unit testcase_curve_window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    points_set, curve_points_set, gauss_points_set,
    two_branches_pseudo_voigt_points_set;

type
    { A refusal is asserted by CALLING through one of these, so the test says
      which operation must refuse rather than wrapping every case in try/except. }
    TProcedureOfObject = procedure of object;

    TCurveWindowTest = class(TTestCase)
    private
        FProfile: TPointsSet;
        FCurve: TCurvePointsSet;
        function Refuses(AProc: TProcedureOfObject): boolean;
        procedure GrowAfterSealing;
        procedure CopyOntoASealedCurve;
        procedure WindowBeforeStart;
        procedure WindowPastEnd;
        procedure WindowInverted;
        procedure SumWithoutWindow;
        procedure SubtractWithoutWindow;
        procedure SumPastTheTarget;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ACurveIsUnboundedUntilItSaysOtherwise;
        procedure AnUnboundedCurveCoversEverySample;
        procedure ACurveHasNoWindowUntilItIsGivenOne;
        procedure AWindowTakesItsXFromTheProfile;
        procedure AWindowRecordsWhereItStarted;
        procedure AWindowCanBeTheWholeProfile;
        procedure AWindowCanBeASingleSample;
        procedure ARefusedWindowIsRefusedRatherThanClamped;
        procedure ASealedCurveRefusesAnotherPoint;
        procedure ASealedCurveRefusesACopyOverIt;
        procedure AddingPlacesTheValuesAtTheOffset;
        procedure SubtractingRemovesExactlyWhatAddingPut;
        procedure AddingAndSubtractingNeedAWindow;
        procedure ACurveMayNotReachPastTheProfile;
        procedure ACopyCarriesTheWindow;
        procedure AWindowCapsEveryWidthOfTheCurve;
        procedure ACopyKeepsTheCapItsWindowSet;
        procedure ACurveWithASupportOfItsOwnIsNotCappedByIt;
    end;

implementation

type
    { A curve that is exactly zero outside [103, 112], as a module's pattern is:
      its window is its own support rather than the fit interval. }
    TSupportedCurve = class(T2BranchesPseudoVoigtPointsSet)
    public
        function SupportMin: double; override;
        function SupportMax: double; override;
    end;

function TSupportedCurve.SupportMin: double;
begin
    Result := 103;
end;

function TSupportedCurve.SupportMax: double;
begin
    Result := 112;
end;

procedure TCurveWindowTest.SetUp;
var
    i: longint;
begin
    FProfile := TPointsSet.Create(nil);
    //  x values that are not their own indices, so an index used where a
    //  coordinate belongs (or the reverse) cannot pass by coincidence.
    for i := 0 to 9 do
        FProfile.AddNewPoint(100 + i * 3, 0);
    FCurve := TGaussPointsSet.Create(nil);
end;

procedure TCurveWindowTest.TearDown;
begin
    FreeAndNil(FCurve);
    FreeAndNil(FProfile);
end;

function TCurveWindowTest.Refuses(AProc: TProcedureOfObject): boolean;
begin
    Result := False;
    try
        AProc;
    except
        Result := True;
    end;
end;

procedure TCurveWindowTest.ACurveIsUnboundedUntilItSaysOtherwise;
begin
    //  The truth for every curve whose tails are small but real: a Gaussian is
    //  never exactly zero, so it occupies the whole interval and always did.
    AssertTrue('unbounded below', FCurve.SupportMin = NegInfinity);
    AssertTrue('unbounded above', FCurve.SupportMax = Infinity);
end;

procedure TCurveWindowTest.AnUnboundedCurveCoversEverySample;
begin
    AssertTrue('covers a sample far below', FCurve.CoversSample(-1e30));
    AssertTrue('covers one in the middle', FCurve.CoversSample(0));
    AssertTrue('covers a sample far above', FCurve.CoversSample(1e30));
end;

procedure TCurveWindowTest.ACurveHasNoWindowUntilItIsGivenOne;
begin
    AssertFalse('no window yet', FCurve.HasWindow);
    FCurve.SetWindow(FProfile, 2, 5);
    AssertTrue('and one afterwards', FCurve.HasWindow);
end;

procedure TCurveWindowTest.AWindowTakesItsXFromTheProfile;
var
    i: longint;
begin
    FCurve.SetWindow(FProfile, 2, 5);
    AssertEquals('four samples', 4, FCurve.PointsCount);
    //  Copied, not recomputed: they are the same doubles as the profile's, which
    //  is what lets everything else compare them exactly.
    for i := 0 to FCurve.PointsCount - 1 do
        AssertTrue('sample ' + IntToStr(i) + ' is the profile''s',
            FCurve.PointXCoord[i] = FProfile.PointXCoord[2 + i]);
end;

procedure TCurveWindowTest.AWindowRecordsWhereItStarted;
begin
    FCurve.SetWindow(FProfile, 3, 7);
    AssertEquals('the offset is where the window begins', 3, FCurve.FFirstSampleIndex);
end;

procedure TCurveWindowTest.AWindowCanBeTheWholeProfile;
begin
    FCurve.SetWindow(FProfile, 0, FProfile.PointsCount - 1);
    AssertEquals('every sample', FProfile.PointsCount, FCurve.PointsCount);
    AssertEquals('starting at the first', 0, FCurve.FFirstSampleIndex);
end;

procedure TCurveWindowTest.AWindowCanBeASingleSample;
begin
    //  The narrowest legal window. A pattern marked between two adjacent bars
    //  produces one.
    FCurve.SetWindow(FProfile, 4, 4);
    AssertEquals('one sample', 1, FCurve.PointsCount);
    AssertTrue('and it is the right one',
        FCurve.PointXCoord[0] = FProfile.PointXCoord[4]);
end;

procedure TCurveWindowTest.WindowBeforeStart;
begin
    FCurve.SetWindow(FProfile, -1, 4);
end;

procedure TCurveWindowTest.WindowPastEnd;
begin
    FCurve.SetWindow(FProfile, 4, FProfile.PointsCount);
end;

procedure TCurveWindowTest.WindowInverted;
begin
    FCurve.SetWindow(FProfile, 6, 3);
end;

procedure TCurveWindowTest.ARefusedWindowIsRefusedRatherThanClamped;
begin
    //  Clamping would produce a curve that is silently somewhere else.
    AssertTrue('a window starting before the profile is refused',
        Refuses(@Self.WindowBeforeStart));
    AssertTrue('a window ending past it is refused',
        Refuses(@Self.WindowPastEnd));
    AssertTrue('an inverted window is refused', Refuses(@Self.WindowInverted));
end;

procedure TCurveWindowTest.GrowAfterSealing;
begin
    FCurve.AddNewPoint(999, 1);
end;

procedure TCurveWindowTest.CopyOntoASealedCurve;
var
    Other: TCurvePointsSet;
begin
    Other := TGaussPointsSet.Create(nil);
    try
        Other.SetWindow(FProfile, 0, 2);
        Other.CopyParameters(FCurve);
    finally
        Other.Free;
    end;
end;

procedure TCurveWindowTest.ASealedCurveRefusesACopyOverIt;
begin
    //  The copy takes the points as one array now, not through AddNewPoint, so
    //  the refusal AddNewPoint gave a sealed curve is said where the copy is.
    FCurve.SetWindow(FProfile, 2, 5);
    AssertTrue('a copy over a sealed curve is refused',
        Refuses(@Self.CopyOntoASealedCurve));
    AssertEquals('which keeps its own points', 4, FCurve.PointsCount);
end;

procedure TCurveWindowTest.ASealedCurveRefusesAnotherPoint;
begin
    //  Before a window there is nothing to protect - the builders fill a curve
    //  then - and afterwards the points ARE its extent.
    FCurve.AddNewPoint(1, 1);
    AssertEquals('a point before the window is accepted', 1, FCurve.PointsCount);

    FCurve.SetWindow(FProfile, 2, 5);
    AssertTrue('and refused after it', Refuses(@Self.GrowAfterSealing));
    AssertEquals('the curve is unchanged by the refusal', 4, FCurve.PointsCount);
end;

procedure TCurveWindowTest.AddingPlacesTheValuesAtTheOffset;
var
    Target: TPointsSet;
    i: longint;
begin
    Target := TPointsSet.Create(nil);
    try
        for i := 0 to FProfile.PointsCount - 1 do
            Target.AddNewPoint(FProfile.PointXCoord[i], 0);

        FCurve.SetWindow(FProfile, 3, 5);
        for i := 0 to FCurve.PointsCount - 1 do
            FCurve.PointYCoord[i] := (i + 1) * 10;

        FCurve.AddTo(Target);

        //  Exactly the three samples the window covers, and nothing either side.
        //  This is the assertion an off-by-one fails.
        AssertEquals('untouched before the window', 0.0, Target.PointYCoord[2], 1e-12);
        AssertEquals('first value at the offset', 10.0, Target.PointYCoord[3], 1e-12);
        AssertEquals('second', 20.0, Target.PointYCoord[4], 1e-12);
        AssertEquals('third', 30.0, Target.PointYCoord[5], 1e-12);
        AssertEquals('untouched after the window', 0.0, Target.PointYCoord[6], 1e-12);
    finally
        Target.Free;
    end;
end;

procedure TCurveWindowTest.SubtractingRemovesExactlyWhatAddingPut;
var
    Target: TPointsSet;
    i: longint;
begin
    //  The optimiser subtracts a curve, changes it and adds it back on every
    //  step, so the two must be exact inverses.
    Target := TPointsSet.Create(nil);
    try
        for i := 0 to FProfile.PointsCount - 1 do
            Target.AddNewPoint(FProfile.PointXCoord[i], i * 1.5);

        FCurve.SetWindow(FProfile, 1, 6);
        for i := 0 to FCurve.PointsCount - 1 do
            FCurve.PointYCoord[i] := Sin(i) * 7;

        FCurve.AddTo(Target);
        FCurve.SubtractFrom(Target);

        for i := 0 to Target.PointsCount - 1 do
            AssertEquals('point ' + IntToStr(i) + ' is back where it started',
                i * 1.5, Target.PointYCoord[i], 1e-12);
    finally
        Target.Free;
    end;
end;

procedure TCurveWindowTest.SumWithoutWindow;
begin
    FCurve.AddTo(FProfile);
end;

procedure TCurveWindowTest.SubtractWithoutWindow;
begin
    FCurve.SubtractFrom(FProfile);
end;

procedure TCurveWindowTest.AddingAndSubtractingNeedAWindow;
begin
    //  Without one there is no answer to "where do these values go", and
    //  guessing at zero would put the curve at the start of the profile.
    AssertTrue('adding needs a window', Refuses(@Self.SumWithoutWindow));
    AssertTrue('subtracting needs one too', Refuses(@Self.SubtractWithoutWindow));
end;

procedure TCurveWindowTest.SumPastTheTarget;
var
    Short: TPointsSet;
begin
    Short := TPointsSet.Create(nil);
    try
        Short.AddNewPoint(0, 0);
        Short.AddNewPoint(1, 0);
        FCurve.AddTo(Short);
    finally
        Short.Free;
    end;
end;

procedure TCurveWindowTest.ACurveMayNotReachPastTheProfile;
begin
    //  A curve summed into a shorter profile than the one it was cut from would
    //  write past the end of the array.
    FCurve.SetWindow(FProfile, 5, 9);
    AssertTrue('reaching past the target is refused', Refuses(@Self.SumPastTheTarget));
end;

procedure TCurveWindowTest.ACopyCarriesTheWindow;
var
    Copy_: TCurvePointsSet;
begin
    //  The service collects curves by copying them, so a copy that lost its
    //  offset would be drawn and tabulated at the start of the profile.
    FCurve.SetWindow(FProfile, 2, 6);
    Copy_ := TCurvePointsSet(FCurve.GetCopy);
    try
        AssertEquals('the copy has the same samples', FCurve.PointsCount, Copy_.PointsCount);
        AssertEquals('and the same offset',
            FCurve.FFirstSampleIndex, Copy_.FFirstSampleIndex);
        AssertTrue('and is sealed like the original', Copy_.HasWindow);
    finally
        Copy_.Free;
    end;
end;

procedure TCurveWindowTest.AWindowCapsEveryWidthOfTheCurve;
var
    Curve: TCurvePointsSet;
begin
    //  BOTH widths of a two-branch pseudo-Voigt, not only the one the base
    //  class keeps a pointer to: a curve's window is told to every parameter,
    //  and each width answers it.
    Curve := T2BranchesPseudoVoigtPointsSet.Create(nil, 106);
    try
        Curve.ValuesByName['sigma'] := 50;
        Curve.ValuesByName['sigmaright'] := 50;
        //  Samples 1..4: x = 103 .. 112, an interval 9 wide.
        Curve.SetWindow(FProfile, 1, 4);
        AssertEquals('the left width', 9, Curve.ValuesByName['sigma'], 1e-9);
        AssertEquals('the right width', 9, Curve.ValuesByName['sigmaright'], 1e-9);
        Curve.ValuesByName['sigmaright'] := 12;
        AssertEquals('and stays capped', 9, Curve.ValuesByName['sigmaright'], 1e-9);
    finally
        Curve.Free;
    end;
end;

procedure TCurveWindowTest.ACopyKeepsTheCapItsWindowSet;
var
    Curve, Copy_: TCurvePointsSet;
begin
    //  The service collects curves by copying them, and the copy is what a
    //  formula backend is handed its bounds from.
    Curve := T2BranchesPseudoVoigtPointsSet.Create(nil, 109);
    Copy_ := nil;
    try
        Curve.SetWindow(FProfile, 2, 6);
        Copy_ := TCurvePointsSet(Curve.GetCopy);
        Copy_.ValuesByName['sigma'] := 100;
        AssertEquals('x = 106 .. 118', 12, Copy_.ValuesByName['sigma'], 1e-9);
    finally
        Copy_.Free;
        Curve.Free;
    end;
end;

procedure TCurveWindowTest.ACurveWithASupportOfItsOwnIsNotCappedByIt;
var
    Curve: TCurvePointsSet;
begin
    //  THE WINDOW IS THE FIT INTERVAL ONLY FOR A CURVE WITHOUT A SUPPORT. One
    //  with a support of its own is given the samples inside it, and its widths
    //  are what decide that support - capping them at it would stop the curve
    //  ever growing. This is what the cap first did to a module's patterns.
    Curve := TSupportedCurve.Create(nil, 106);
    try
        Curve.ValuesByName['sigma'] := 50;
        Curve.SetWindow(FProfile, 1, 4);
        AssertEquals('not capped at its own support', 50,
            Curve.ValuesByName['sigma'], 1e-9);
    finally
        Curve.Free;
    end;
end;

initialization
    RegisterTest('unit', TCurveWindowTest);
end.
