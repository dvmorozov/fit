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

The test Gaussian stands in for "a curve": TCurvePointsSet is abstract, and the
window belongs to the base class, so any concrete type exercises the same code.
}
unit testcase_curve_window;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    points_set, curve_points_set, test_curve_types;

type
    { A refusal is asserted by CALLING through one of these, so the test says
      which operation must refuse rather than wrapping every case in try/except. }
    TProcedureOfObject = procedure of object;

    TCurveWindowTest = class(TTestCase)
    private
        FProfile: TPointsSet;
        FCurve: TCurvePointsSet;
        function Refuses(AProc: TProcedureOfObject): boolean;
        procedure CopyOntoASealedCurve;
        procedure WindowBeforeStart;
        procedure WindowPastEnd;
        procedure WindowInverted;
        procedure SumWithoutWindow;
        procedure SubtractWithoutWindow;
        procedure SumPastTheTarget;
        procedure TakeSamplesAgain;
        procedure AddAfterWindow;
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
        procedure ACurveGivenOnlyItsXHoldsNoValuesYet;
        procedure ASealedCurveRefusesACopyOverIt;
        procedure AddingPlacesTheValuesAtTheOffset;
        procedure SubtractingRemovesExactlyWhatAddingPut;
        procedure AddingAndSubtractingNeedAWindow;
        procedure ACurveMayNotReachPastTheProfile;
        procedure ACopyCarriesTheWindow;
        procedure AWindowCapsEveryWidthOfTheCurve;
        procedure ACopyKeepsTheCapItsWindowSet;
        procedure ACurveWithASupportOfItsOwnIsNotCappedByIt;
        //  What the service and the client do to a curve's samples, said by
        //  the curve rather than written into it from outside.
        procedure ScalingACurveScalesItsValuesAndNotItsParameters;
        procedure ACurveTakenFromTheWireHoldsItsSamplesSealed;
        procedure AWidthKnownOnlyByNameIsReadAndWrittenAsItIs;
    end;

implementation

uses
    sigma_curve_parameter, named_points_set,
    special_curve_parameter, calculated_curve_parameter, noise_rule;

type
    { A curve with TWO widths, as a two-branch peak has one each side: the
      left one the base class keeps a pointer to (sigma) and a right one it
      does not (sigmaright), so the window must tell every parameter, not the
      one it knows. Both are FULL widths at half maximum, as a two-branch
      pseudo-Voigt's are, so the cap a window puts on each is the window's own
      span rather than that span over a Gaussian's 2.355. }
    TTwoWidthCurve = class(TTestGaussPointsSet)
    public
        constructor Create(AOwner: TComponent; AX0: double); overload;
        function FullWidthPerUnit(const AParameterName: string): double; override;
    end;

    { A curve that is exactly zero outside [103, 112], as a module's pattern is:
      its window is its own support rather than the fit interval. }
    TSupportedCurve = class(TTwoWidthCurve)
    public
        function SupportMin: double; override;
        function SupportMax: double; override;
    end;

    { A CURVE WHOSE WIDTH AND AMPLITUDE ARE KNOWN ONLY BY NAME: parameters
      called 'sigma' and 'A' of a class that is neither a width's nor an
      amplitude's, as a module's own parameter or a user formula's may be. }
    TRolesByNameCurve = class(TNamedPointsSet)
    public
        constructor Create(AOwner: TComponent); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function NoiseRule: TNoiseRuleClass; override;
    end;

class function TRolesByNameCurve.NoiseRule: TNoiseRuleClass;
begin
    //  The test Gaussian's: every curve type names one.
    Result := TTestGaussPointsSet.NoiseRule;
end;

constructor TRolesByNameCurve.Create(AOwner: TComponent);
var
    P: TSpecialCurveParameter;
begin
    inherited Create(AOwner);
    P := TCalculatedCurveParameter.Create;
    P.Name := 'sigma';
    P.Value := 2;
    AddParameter(P);
    P := TCalculatedCurveParameter.Create;
    P.Name := 'A';
    P.Value := 3;
    AddParameter(P);
    InitListOfVariableParameters;
end;

class function TRolesByNameCurve.GetCurveTypeName: string;
begin
    Result := 'Roles by name (test)';
end;

class function TRolesByNameCurve.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{6C1D2E7A-4B3F-4E21-9A8D-0F5C3B7E2D19}');
end;

constructor TTwoWidthCurve.Create(AOwner: TComponent; AX0: double);
var
    Right: TSigmaCurveParameter;
begin
    inherited Create(AOwner, AX0);
    Right := TSigmaCurveParameter.Create;
    Right.Name := 'sigmaright';
    AddParameter(Right);
    InitListOfVariableParameters;
end;

function TTwoWidthCurve.FullWidthPerUnit(const AParameterName: string): double;
begin
    Result := 1;
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
    FCurve := TTestGaussPointsSet.Create(nil);
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

procedure TCurveWindowTest.CopyOntoASealedCurve;
var
    Other: TCurvePointsSet;
begin
    Other := TTestGaussPointsSet.Create(nil);
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

procedure TCurveWindowTest.ACurveGivenOnlyItsXHoldsNoValuesYet;
begin
    //  A BUILDER GIVES A CURVE THE X IT IS TO BE COMPUTED AT, away from any
    //  profile - a module's search trying a shape over a stretch of a series -
    //  and the values are the curve's to compute. Nothing can add a point to a
    //  curve: it is not an editable set, so it has no AddNewPoint, and the
    //  one way to add a sample gives it no value.
    FCurve.AddSample(3);
    FCurve.AddSample(5);
    AssertEquals('both samples', 2, FCurve.PointsCount);
    AssertEquals('at the x given', 5, FCurve.PointXCoord[1], 0);
    AssertEquals('holding no value yet', 0, FCurve.PointYCoord[0], 0);
    AssertEquals('none at all', 0, FCurve.PointYCoord[1], 0);
    AssertFalse('no window over a profile', FCurve.HasWindow);

    //  A window given afterwards replaces them with the profile's, and then
    //  the samples are its extent.
    FCurve.SetWindow(FProfile, 2, 5);
    AssertEquals('the window''s samples instead', 4, FCurve.PointsCount);
    AssertEquals('from the profile', 106, FCurve.PointXCoord[0], 0);
    AssertTrue('nothing added once it has them', Refuses(@Self.AddAfterWindow));
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

        //  THREE DIFFERENT VALUES, falling from the peak at the window's first
        //  sample (x = 109), so a value landing one sample off is told apart.
        //  Computed by the curve, as every value of a curve is: nothing outside
        //  the curve classes writes one.
        FCurve.SetWindow(FProfile, 3, 5);
        FCurve.A := 10;
        FCurve.Sigma := 4;
        FCurve.x0 := 109;
        FCurve.ReCalc;
        AssertTrue('values that fall away from the first',
            (FCurve.PointYCoord[0] > FCurve.PointYCoord[1]) and
            (FCurve.PointYCoord[1] > FCurve.PointYCoord[2]) and
            (FCurve.PointYCoord[2] > 0));

        FCurve.AddTo(Target);

        //  Exactly the three samples the window covers, and nothing either side.
        //  This is the assertion an off-by-one fails.
        AssertEquals('untouched before the window', 0.0, Target.PointYCoord[2], 1e-12);
        AssertEquals('first value at the offset', FCurve.PointYCoord[0], Target.PointYCoord[3], 1e-12);
        AssertEquals('second', FCurve.PointYCoord[1], Target.PointYCoord[4], 1e-12);
        AssertEquals('third', FCurve.PointYCoord[2], Target.PointYCoord[5], 1e-12);
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
        FCurve.A := 7;
        FCurve.Sigma := 5;
        FCurve.x0 := 109;
        FCurve.ReCalc;

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
    //  BOTH widths of a two-branch curve, not only the one the base
    //  class keeps a pointer to: a curve's window is told to every parameter,
    //  and each width answers it.
    Curve := TTwoWidthCurve.Create(nil, 106);
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
    Curve := TTwoWidthCurve.Create(nil, 109);
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

procedure TCurveWindowTest.ScalingACurveScalesItsValuesAndNotItsParameters;
var
    Before: array of double;
    Amplitude: double;
    i: longint;
begin
    //  The service shows a task's curves at the profile's scale: the values
    //  change, the parameters the fit found do not, and neither do the x.
    FCurve.SetWindow(FProfile, 2, 6);
    FCurve.A := 10;
    FCurve.Sigma := 4;
    FCurve.x0 := 112;
    FCurve.ReCalc;
    SetLength(Before, FCurve.PointsCount);
    for i := 0 to FCurve.PointsCount - 1 do
        Before[i] := FCurve.PointYCoord[i];
    Amplitude := FCurve.A;
    FCurve.ScaleValues(2.5);
    for i := 0 to FCurve.PointsCount - 1 do
    begin
        AssertEquals('value ' + IntToStr(i) + ' scaled', 2.5 * Before[i],
            FCurve.PointYCoord[i], 1e-12);
        AssertEquals('at the same x', FProfile.PointXCoord[2 + i],
            FCurve.PointXCoord[i], 0);
    end;
    AssertTrue('a value that can show the scaling', Before[2] <> 0);
    AssertEquals('the amplitude found is kept', Amplitude, FCurve.A, 0);
end;

procedure TCurveWindowTest.AddAfterWindow;
begin
    FCurve.AddSample(999);
end;

procedure TCurveWindowTest.TakeSamplesAgain;
begin
    FCurve.TakeSamples([7], [8]);
end;

procedure TCurveWindowTest.ACurveTakenFromTheWireHoldsItsSamplesSealed;
begin
    //  The client's curves come from the wire: their samples arrive whole,
    //  and nothing may add to them or replace them afterwards.
    FCurve.TakeSamples([1, 2, 4], [10, 20, 40]);
    AssertEquals('every sample taken', 3, FCurve.PointsCount);
    AssertEquals('x as sent', 4, FCurve.PointXCoord[2], 0);
    AssertEquals('y as sent', 20, FCurve.PointYCoord[1], 0);
    AssertFalse('no window over a profile', FCurve.HasWindow);
    AssertTrue('taken once only', Refuses(@Self.TakeSamplesAgain));
    AssertEquals('and unchanged by the refusal', 3, FCurve.PointsCount);
end;

procedure TCurveWindowTest.AWidthKnownOnlyByNameIsReadAndWrittenAsItIs;
var
    C: TRolesByNameCurve;
begin
    //  A ROLE RECOGNISED BY NAME IS NOT A CLASS. The width and the amplitude
    //  were held as TSigmaCurveParameter and TAmplitudeCurveParameter whatever
    //  class the parameter so named was - an invalid cast, red in a build
    //  with object checks (-CR); here they are read and written as they are.
    C := TRolesByNameCurve.Create(nil);
    try
        AssertTrue('a width, by name', C.HasSigma);
        AssertTrue('an amplitude, by name', C.HasA);
        AssertEquals('the width read', 2, C.Sigma, 0);
        AssertEquals('the amplitude read', 3, C.A, 0);
        C.Sigma := 5;
        AssertEquals('the width written', 5, C.ValuesByName['sigma'], 0);
    finally
        C.Free;
    end;
end;

initialization
    RegisterTest('unit', TCurveWindowTest);
end.
