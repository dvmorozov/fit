// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Writing a backend's answer back onto the model.)

THE LAST STEP OF EVERY FIT THAT DOES NOT RUN IN THIS PROCESS.
`ApplyOutcomeToTask` is what turns the numbers a backend sent back into the
curves the user sees: `python_fit_backend.pas:142` and `server_fit_backend.pas:127`
are its only callers, and both hand it a `TFitOutcome` that arrived over a wire.

IT WAS RUN BUT NEVER CHECKED. Its lines were already covered - the backend
tests drive Fit() against a stubbed backend, and that reaches it - so the
coverage figure said it was exercised. Nothing asserted anything it does. A
routine that runs on the way to somebody else's assertion is measured as covered
and is not tested at all, and this is what that looks like: every rule below
could have been broken without one existing test noticing.

THE MEASUREMENT DOES NOT MOVE FOR THIS FILE, therefore, and that is the point
worth remembering when reading a coverage report as a statement about risk.

THE RULES ARE ALL ABOUT NOT LOSING THE ANSWER, and each has a failure that looks
like a working fit:

  * matched BY NAME, not by position, because the backend is free to order its
    parameters however it likes. Matching by index puts sigma's value into the
    amplitude and returns a curve that is a curve, just not the fitted one;

  * set through `ValuesByName`, which flags the curve for recomputation. The
    unit's own comment says what writing `Params[j].Value` directly would cost:
    the cached profile stays at its pre-fit seed, `ComputeProfile` skips the
    curve, and the fit silently never takes effect. That is checked here by
    looking at the computed points, which is the only place the difference shows;

  * the uncertainty carried across separately, because the value setter does not
    touch `Error` - and an uncertainty left behind is a column of stale numbers
    in the parameters table rather than an obvious blank;

  * and a length mismatch between the outcome and the task tolerated in both
    directions. A backend that answers with fewer curves than it was asked about
    must not leave the fit half-applied AND must not fault.
}
unit testcase_outcome_apply;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_task, fit_problem_json, fit_task_marshalling,
    points_set, curve_points_set, self_copied_component,
    gauss_points_set, SimpMath;

type
    TOutcomeApplyTest = class(TTestCase)
    private
        FTask: TFitTask;
        { The one curve the task holds. }
        function Curve: TCurvePointsSet;
        function ValueOf(const AName: string): double;
        function ErrorOf(const AName: string): double;
        { The clamp a parameter imposes on its own value. }
        function MinOf(const AName: string): double;
        function MaxOf(const AName: string): double;
        { An outcome naming one curve, with the given parameters in the given
          order - the order matters, which is the point of several tests. }
        function OutcomeFor(const ANames: array of string;
            const AValues: array of double): TFitOutcome;
        function OutcomeWithError(const AName: string;
            AValue, AError: double): TFitOutcome;
        { The computed profile as one number, so "did the curve change" is one
          comparison rather than a loop in every test. }
        function CalcIntegral: double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The fixture itself.
        procedure TheTaskStartsWithOneSeededCurve;

        //  Applying values.
        procedure AValueReachesTheParameterItNames;
        procedure EveryNamedParameterIsApplied;
        procedure ParametersAreMatchedByNameNotByPosition;
        procedure AParameterTheOutcomeDoesNotMentionIsLeftAlone;
        procedure AnOutcomeParameterThatIsNotOnTheCurveIsIgnored;

        //  The position and the window it may move in.
        procedure APositionTakesAFittedValueWithinItsWindow;
        procedure ItsBoundsAreTheNeighbouringSamples;
        procedure AFittedPositionOutsideTheWindowIsClampedToIt;

        //  Making the answer take effect.
        procedure ApplyingRecomputesTheCurveFromTheNewValues;
        procedure AndTheProfileFollowsTheParameterThatChanged;

        //  The uncertainty.
        procedure TheUncertaintyIsCarriedOntoTheParameter;
        procedure AnUnestimatedUncertaintyStaysNegative;

        //  Mismatched lengths.
        procedure AnOutcomeWithNoCurvesChangesNothingAndDoesNotFault;
        procedure AnOutcomeWithMoreCurvesThanTheTaskIgnoresTheExtra;
        procedure AnOutcomeWithNoParametersLeavesTheCurveAsItWas;
    end;

implementation

const
    { A Gaussian the fixture seeds and never fits: amplitude 100, sigma 1.5,
      centre 10. Nothing here runs the optimiser - applying an outcome is the
      subject, and a real fit would decide the numbers before the code under
      test was reached. }
    SEED_A = 100.0;
    SEED_SIGMA = 1.5;
    SEED_X0 = 10.0;

procedure TOutcomeApplyTest.SetUp;
var
    P: TFitProblem;
    x: double;
    n: longint;
begin
    //  Masked, as every path that computes a curve in this suite does: a
    //  degenerate parameter must produce Inf rather than a raise.
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);

    P := Default(TFitProblem);
    n := 0;
    x := 0;
    while x <= 20 + 1e-9 do
    begin
        SetLength(P.ProfileX, n + 1);
        SetLength(P.ProfileY, n + 1);
        P.ProfileX[n] := x;
        P.ProfileY[n] := GaussPoint(SEED_A, SEED_SIGMA, SEED_X0, x);
        Inc(n);
        x := x + 0.5;
    end;
    //  The y at the position seeds the amplitude, so a zero here would start
    //  from a degenerate curve.
    P.PositionsX := TDoubleArray.Create(SEED_X0);
    P.PositionsY := TDoubleArray.Create(
        GaussPoint(SEED_A, SEED_SIGMA, SEED_X0, SEED_X0));
    P.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
    P.MaxRFactor := 0.01;
    P.BegIndex := 0;
    P.EndIndex := 0;

    FTask := BuildTaskFromProblem(P);
    FTask.ComputeProfile;
end;

procedure TOutcomeApplyTest.TearDown;
begin
    FreeAndNil(FTask);
end;

function TOutcomeApplyTest.Curve: TCurvePointsSet;
begin
    Result := TCurvePointsSet(FTask.GetCurves.Items[0]);
end;

function TOutcomeApplyTest.ValueOf(const AName: string): double;
begin
    Result := Curve.ValuesByName[AName];
end;

function TOutcomeApplyTest.ErrorOf(const AName: string): double;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to Curve.Parameters.Count - 1 do
        if Curve.Parameters[i].Name = AName then
            Exit(Curve.Parameters[i].Error);
    AssertTrue('the curve has a parameter named ' + AName, False);
end;

function TOutcomeApplyTest.MinOf(const AName: string): double;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to Curve.Parameters.Count - 1 do
        if Curve.Parameters[i].Name = AName then
            Exit(Curve.Parameters[i].GetMinValue);
    AssertTrue('the curve has a parameter named ' + AName, False);
end;

function TOutcomeApplyTest.MaxOf(const AName: string): double;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to Curve.Parameters.Count - 1 do
        if AName = Curve.Parameters[i].Name then
            Exit(Curve.Parameters[i].GetMaxValue);
    AssertTrue('the curve has a parameter named ' + AName, False);
end;

function TOutcomeApplyTest.OutcomeFor(const ANames: array of string;
    const AValues: array of double): TFitOutcome;
var
    i: longint;
begin
    Result := Default(TFitOutcome);
    SetLength(Result.Curves, 1);
    SetLength(Result.Curves[0].Params, Length(ANames));
    for i := 0 to High(ANames) do
    begin
        Result.Curves[0].Params[i].Name := ANames[i];
        Result.Curves[0].Params[i].Value := AValues[i];
        //  What a backend that estimated nothing sends; the tests about the
        //  uncertainty set it deliberately.
        Result.Curves[0].Params[i].Error := -1;
    end;
end;

function TOutcomeApplyTest.OutcomeWithError(const AName: string;
    AValue, AError: double): TFitOutcome;
begin
    Result := OutcomeFor([AName], [AValue]);
    Result.Curves[0].Params[0].Error := AError;
end;

function TOutcomeApplyTest.CalcIntegral: double;
var
    P: TPointsSet;
    i: longint;
begin
    //  Summed here rather than through the task's own integral, which is not
    //  public. One number for the whole computed profile is enough: what these
    //  tests ask is whether the curve was recomputed at all and in which
    //  direction, not what its shape is.
    Result := 0;
    P := FTask.GetCalcProfile;
    for i := 0 to P.PointsCount - 1 do
        Result := Result + P.PointYCoord[i];
end;

{ ------------------------------- the fixture -------------------------------- }

procedure TOutcomeApplyTest.TheTaskStartsWithOneSeededCurve;
begin
    //  Guards the fixture rather than the code: every test below reads Curve,
    //  and a task built with no curves would make them all vacuous.
    AssertEquals('one curve', 1, FTask.GetCurves.Count);
    AssertEquals('seeded at the position', SEED_X0, ValueOf('x0'), 1e-9);
end;

{ ------------------------------ applying values ----------------------------- }

procedure TOutcomeApplyTest.AValueReachesTheParameterItNames;
begin
    ApplyOutcomeToTask(FTask, OutcomeFor(['sigma'], [2.25]));
    AssertEquals('sigma took the fitted value', 2.25, ValueOf('sigma'), 1e-9);
end;

procedure TOutcomeApplyTest.EveryNamedParameterIsApplied;
begin
    //  A and sigma, not x0: a position clamps itself to its own window, which is
    //  its rule rather than anything this routine does. See
    //  AFittedPositionOutsideTheWindowIsClampedToIt.
    ApplyOutcomeToTask(FTask, OutcomeFor(['A', 'sigma'], [55.0, 2.25]));
    AssertEquals('A', 55.0, ValueOf('A'), 1e-9);
    AssertEquals('sigma', 2.25, ValueOf('sigma'), 1e-9);
end;

procedure TOutcomeApplyTest.ParametersAreMatchedByNameNotByPosition;
begin
    //  THE ORDER IS THE BACKEND'S, NOT OURS. It is free to answer in whatever
    //  order it likes, and a position match would put these three values on the
    //  wrong parameters - producing a Gaussian that is a Gaussian, just not the
    //  fitted one, which no assertion about "did it change" would catch.
    ApplyOutcomeToTask(FTask, OutcomeFor(['sigma', 'A'], [2.25, 55.0]));
    AssertEquals('sigma named first still got sigma''s value',
        2.25, ValueOf('sigma'), 1e-9);
    AssertEquals('and A got A''s', 55.0, ValueOf('A'), 1e-9);
end;

procedure TOutcomeApplyTest.AParameterTheOutcomeDoesNotMentionIsLeftAlone;
begin
    //  A backend that held a parameter fixed need not send it back. Zeroing
    //  what is absent would move a curve the user pinned.
    ApplyOutcomeToTask(FTask, OutcomeFor(['sigma'], [2.25]));
    AssertEquals('x0 untouched', SEED_X0, ValueOf('x0'), 1e-9);
end;

procedure TOutcomeApplyTest.AnOutcomeParameterThatIsNotOnTheCurveIsIgnored;
var
    Before: double;
begin
    //  A newer backend, or one answering about a different curve type. Skipped
    //  rather than faulting: the rest of the outcome is still usable.
    Before := ValueOf('sigma');
    ApplyOutcomeToTask(FTask, OutcomeFor(['tau', 'sigma'], [9.9, 2.25]));
    AssertEquals('sigma still applied', 2.25, ValueOf('sigma'), 1e-9);
    AssertTrue('and the unknown one changed nothing else', Before <> 2.25);
end;

{ ------------------- the position and the window it moves in ---------------- }

procedure TOutcomeApplyTest.APositionTakesAFittedValueWithinItsWindow;
begin
    //  THIS IS THE TEST THAT USED TO SAY THE OPPOSITE. TPositionCurveParameter
    //  read its boundaries in the CURVE'S CONSTRUCTOR, from the curve itself,
    //  which holds no points at that moment - so no sample was found either
    //  side of the seed, SetBoundaries' own fallback put both bounds onto the
    //  seed, and the clamp was to a single value forever. A fitted position was
    //  discarded in silence: the value went in, the clamp put the seed back,
    //  and the outcome reported a moved peak the model never took.
    //
    //  The boundaries are read on the first assignment instead, by which time
    //  the curve has its window. A quarter of a sample is comfortably inside
    //  the interval, so nothing clamps and the value survives.
    ApplyOutcomeToTask(FTask, OutcomeFor(['x0'], [SEED_X0 + 0.25]));
    AssertEquals('x0 took the fitted value',
        SEED_X0 + 0.25, ValueOf('x0'), 1e-9);
end;

procedure TOutcomeApplyTest.ItsBoundsAreTheNeighbouringSamples;
begin
    //  WHAT THE DECLARATION HAS ALWAYS CLAIMED: the clamp is to "the
    //  neighbouring data points". The fixture's profile is sampled every 0.5
    //  and the seed is 10, so the samples either side are 9.5 and 10.5, and
    //  these are read off the profile rather than restated - a bound equal to
    //  the seed on either side is the old defect back.
    AssertEquals('lower bound is the sample below the seed',
        SEED_X0 - 0.5, MinOf('x0'), 1e-12);
    AssertEquals('upper bound is the sample above it',
        SEED_X0 + 0.5, MaxOf('x0'), 1e-12);
end;

procedure TOutcomeApplyTest.AFittedPositionOutsideTheWindowIsClampedToIt;
begin
    //  THE CLAMP STILL CLAMPS, which is the half of this that must not have
    //  been lost: the window is a constraint, not a formality. A backend that
    //  answers three samples away gets the edge of the window, not its number,
    //  because a peak allowed to wander is a peak that can swap with its
    //  neighbour and leave a fit that converged onto the wrong data.
    ApplyOutcomeToTask(FTask, OutcomeFor(['x0'], [SEED_X0 + 1.5]));
    AssertEquals('clamped to the upper bound',
        SEED_X0 + 0.5, ValueOf('x0'), 1e-9);
end;

{ ------------------------ making the answer take effect --------------------- }

procedure TOutcomeApplyTest.ApplyingRecomputesTheCurveFromTheNewValues;
var
    Before: double;
begin
    //  THE RULE THE UNIT'S OWN COMMENT EXISTS FOR. Values go in through
    //  ValuesByName, which flags the curve for recomputation. Written straight
    //  onto the parameter object instead, the cached profile would stay at the
    //  seed, ComputeProfile would skip the curve, and the fit would silently
    //  never take effect - a green fit and an unchanged chart.
    Before := CalcIntegral;
    ApplyOutcomeToTask(FTask, OutcomeFor(['A'], [ValueOf('A') / 2]));
    AssertTrue('the computed profile changed with the parameter',
        Abs(CalcIntegral - Before) > 1e-6);
end;

procedure TOutcomeApplyTest.AndTheProfileFollowsTheParameterThatChanged;
var
    Before, Was: double;
begin
    //  Direction, not just difference: halving the amplitude of the only curve
    //  must halve what it contributes. A recomputation that ran but read the
    //  old values would leave the integral where it was; one that read them
    //  correctly but from the wrong parameter would move it the wrong way.
    //  Halving what the curve ACTUALLY holds, not what the profile was seeded
    //  from: the engine derives the seed amplitude itself, and assuming it
    //  equals SEED_A would test the seeding rather than the application.
    Was := ValueOf('A');
    Before := CalcIntegral;
    ApplyOutcomeToTask(FTask, OutcomeFor(['A'], [Was / 2]));
    AssertEquals('half the amplitude, half the integral',
        Before / 2, CalcIntegral, Abs(Before) * 1e-6);
end;

{ ------------------------------- the uncertainty ---------------------------- }

procedure TOutcomeApplyTest.TheUncertaintyIsCarriedOntoTheParameter;
begin
    //  Carried separately because the value setter does not touch Error. Left
    //  behind, the parameters table shows the PREVIOUS fit's uncertainty beside
    //  this fit's value - stale numbers rather than an obvious blank.
    ApplyOutcomeToTask(FTask, OutcomeWithError('sigma', 2.25, 0.125));
    AssertEquals('the value', 2.25, ValueOf('sigma'), 1e-9);
    AssertEquals('and its uncertainty', 0.125, ErrorOf('sigma'), 1e-12);
end;

procedure TOutcomeApplyTest.AnUnestimatedUncertaintyStaysNegative;
begin
    //  Negative is how the engine says "none", and it is what the grid reads to
    //  decide whether to print a +/- at all. A backend that estimated nothing
    //  must not end up claiming an uncertainty of zero, which would read as a
    //  perfectly determined parameter.
    ApplyOutcomeToTask(FTask, OutcomeWithError('sigma', 2.25, -1));
    AssertTrue('still says "not estimated"', ErrorOf('sigma') < 0);
end;

{ ----------------------------- mismatched lengths --------------------------- }

procedure TOutcomeApplyTest.AnOutcomeWithNoCurvesChangesNothingAndDoesNotFault;
var
    O: TFitOutcome;
    Before: double;
begin
    //  What a refusal or a truncated reply looks like once it has been parsed.
    //  The loop must stop at the shorter of the two rather than index past it.
    Before := ValueOf('sigma');
    O := Default(TFitOutcome);
    ApplyOutcomeToTask(FTask, O);
    AssertEquals('nothing applied', Before, ValueOf('sigma'), 1e-12);
end;

procedure TOutcomeApplyTest.AnOutcomeWithMoreCurvesThanTheTaskIgnoresTheExtra;
var
    O: TFitOutcome;
begin
    //  The other direction of the same mismatch: a backend answering about more
    //  curves than were placed. The task's own curve is still applied.
    O := OutcomeFor(['sigma'], [2.25]);
    SetLength(O.Curves, 3);
    SetLength(O.Curves[1].Params, 1);
    O.Curves[1].Params[0].Name := 'sigma';
    O.Curves[1].Params[0].Value := 99;
    ApplyOutcomeToTask(FTask, O);
    AssertEquals('the first curve got its own value',
        2.25, ValueOf('sigma'), 1e-9);
end;

procedure TOutcomeApplyTest.AnOutcomeWithNoParametersLeavesTheCurveAsItWas;
var
    O: TFitOutcome;
    Before: double;
begin
    Before := ValueOf('sigma');
    O := Default(TFitOutcome);
    SetLength(O.Curves, 1);
    ApplyOutcomeToTask(FTask, O);
    AssertEquals('unchanged', Before, ValueOf('sigma'), 1e-12);
end;

initialization
    //  Unit tests: the outcome is written by the test, so no optimiser runs and
    //  no backend is reached. The round trip THROUGH a real fit is
    //  testcase_fit_marshalling's integration half.
    RegisterTest('unit', TOutcomeApplyTest);
end.
