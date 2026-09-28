// SPDX-License-Identifier: GPL-3.0-or-later
{ Ties each parameter's declared bounds (GetMinValue/GetMaxValue) to the clamp its
  SetValue actually applies.

  The two are written separately - the clamp is what the native Downhill Simplex
  relies on, the bounds are what is shipped to the gradient (Python/trf) minimizer -
  so they can drift apart. When they do, the gradient minimizer optimises into a
  region the native side clamps away on writeback and the applied model stops being
  the model that was fitted (the Data/2.dat divergence: weighted chi-square 13,345,660
  vs 1,111).

  The invariant is NOT that the bounds equal the clamp. It is that the bounds are a
  subset of the values the clamp leaves untouched:

      every value inside [GetMinValue, GetMaxValue] survives SetValue unchanged

  A tighter bound than the clamp is therefore fine, and sometimes deliberate (sigma
  advertises TINY because a near-zero width blows the model up). }
unit testcase_parameter_bounds;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, fpcunit, testregistry,
  points_set, special_curve_parameter, amplitude_curve_parameter,
  sigma_curve_parameter, eta_curve_parameter, delta_sigma_curve_parameter,
  position_curve_parameter, gamma_curve_parameter, tau_curve_parameter,
  shape_curve_parameter, asymmetry_curve_parameter, skew_curve_parameter,
  calculated_curve_parameter;

type
  TParameterBoundsTest = class(TTestCase)
  private
    { Asserts the invariant on one parameter; frees it. }
    procedure CheckBounds(P: TSpecialCurveParameter; const AWhat: string);
    { A parameter with no declared limits must report none, so nobody declares a
      bound with no clamp behind it. }
    procedure CheckUnbounded(P: TSpecialCurveParameter; const AWhat: string);
        { Fills a points set with a profile sampled every half unit from 0 to 20.
      FILLS RATHER THAN CREATES, because a position parameter borrows the set it
      was constructed with: swapping in a second object would leave it pointing
      at freed memory, and it is the curve filling itself that this models. }
    procedure FillHalfUnitProfile(APoints: TPointsSet);
  published
    procedure AmplitudeRespectsItsBounds;
    procedure SigmaRespectsItsBounds;
    procedure EtaRespectsItsBounds;
    procedure PositionRespectsItsBounds;
    procedure DeltaSigmaIsUnbounded;
    //  EVERY OTHER POLICY, added because five of eleven were covered and the six
    //  that were not are the ones a newer curve type uses: gamma and tau feed the
    //  Voigt and EMG shapes, m the Pearson VII, alpha and beta the skewed forms.
    //  tools/build-tests/parameter_policies.tests.ps1 fails the build when a
    //  policy file exists that this list does not name.
    procedure GammaRespectsItsBounds;
    procedure TauRespectsItsBounds;
    procedure ShapeRespectsItsBounds;
    procedure AsymmetryRespectsItsBounds;
    procedure SkewIsUnbounded;
    procedure ACalculatedParameterIsNotFitted;
    //  THE POSITION'S WINDOW IS NOT KNOWN WHEN THE PARAMETER IS BUILT, which is
    //  a lifecycle rule and not a bounds policy - see the group's comment.
    procedure ThePositionWindowIsUnknownWhileTheCurveHasNoPoints;
    procedure ItIsReadOnceTheCurveHasPointsAndAValueIsAssigned;
    procedure ItIsMeasuredFromTheSeedNotFromTheValueThatArrives;
    procedure AndItDoesNotMoveWithLaterAssignments;
    procedure APositionWithNoPointsAtAllClampsNothing;
    procedure ACopyKeepsTheWindowItWasGiven;
  end;

implementation

{ Samples values across [min, max] and asserts SetValue stores each unchanged, then
  pushes values outside the range and asserts the result lands back inside it. }
procedure TParameterBoundsTest.CheckBounds(P: TSpecialCurveParameter;
  const AWhat: string);
var
  Lo, Hi, V: double;
  i: integer;
begin
  try
    Lo := P.GetMinValue;
    Hi := P.GetMaxValue;
    AssertTrue(AWhat + ': min < max', Lo < Hi);

    //  1. Every value inside the declared range must round-trip unchanged. This is
    //     what makes a bounds-respecting Python result apply losslessly.
    for i := 0 to 4 do
    begin
      if IsInfinite(Hi) then
        //  Open above: sample a spread of magnitudes above the minimum.
        V := Max(Lo, 0) + 0.1 + i * 10.0
      else
        //  Closed: sample strictly inside, avoiding the exact endpoints.
        V := Lo + (Hi - Lo) * (i + 1) / 6.0;
      P.Value := V;
      AssertEquals(AWhat + ': in-bounds value survives unchanged', V, P.Value, 1e-12);
    end;

    //  2. Values outside the range must be brought back inside it, so the declared
    //     box is never looser than the clamp. Containment only - the clamps use Abs,
    //     which reflects (eta -0.3 -> +0.3) rather than stopping at the bound.
    if not IsInfinite(Lo) then
    begin
      P.Value := Lo - 1000.0;
      AssertTrue(AWhat + ': value below min is pulled inside (' +
        FloatToStr(P.Value) + ')', (P.Value >= Lo) and (P.Value <= Hi));
    end;
    if not IsInfinite(Hi) then
    begin
      P.Value := Hi + 1000.0;
      AssertTrue(AWhat + ': value above max is pulled inside (' +
        FloatToStr(P.Value) + ')', (P.Value >= Lo) and (P.Value <= Hi));
    end;
  finally
    P.Free;
  end;
end;

procedure TParameterBoundsTest.CheckUnbounded(P: TSpecialCurveParameter;
  const AWhat: string);
begin
  try
    AssertTrue(AWhat + ': min is -Inf',
      IsInfinite(P.GetMinValue) and (P.GetMinValue < 0));
    AssertTrue(AWhat + ': max is +Inf',
      IsInfinite(P.GetMaxValue) and (P.GetMaxValue > 0));
    //  An unbounded parameter must not silently alter values either.
    P.Value := -12345.678;
    AssertEquals(AWhat + ': stores negatives unchanged', -12345.678, P.Value, 1e-9);
  finally
    P.Free;
  end;
end;

procedure TParameterBoundsTest.AmplitudeRespectsItsBounds;
begin
  CheckBounds(TAmplitudeCurveParameter.Create, 'amplitude');
end;

procedure TParameterBoundsTest.SigmaRespectsItsBounds;
begin
  CheckBounds(TSigmaCurveParameter.Create, 'sigma');
end;

procedure TParameterBoundsTest.EtaRespectsItsBounds;
begin
  CheckBounds(TEtaCurveParameter.Create, 'eta');
end;

procedure TParameterBoundsTest.PositionRespectsItsBounds;
var
  PS: TPointsSet;
  x: double;
begin
  //  The position's window is the neighbouring data points, so it needs a profile.
  PS := TPointsSet.Create(nil);
  try
    x := 0;
    while x <= 20 + 1e-9 do
    begin
      PS.AddNewPoint(x, 0);
      x := x + 0.5;
    end;
    CheckBounds(TPositionCurveParameter.Create(10.0, PS), 'position');
  finally
    PS.Free;
  end;
end;

procedure TParameterBoundsTest.DeltaSigmaIsUnbounded;
begin
  CheckUnbounded(TDeltaSigmaCurveParameter.Create, 'deltasigma');
end;

procedure TParameterBoundsTest.GammaRespectsItsBounds;
begin
  //  The Lorentzian half-width of a Voigt profile: strictly positive, floored just
  //  above zero because a zero width makes the profile a division by zero.
  CheckBounds(TGammaCurveParameter.Create, 'gamma');
end;

procedure TParameterBoundsTest.TauRespectsItsBounds;
begin
  //  The exponential decay constant of an EMG. Same floor, same reason.
  CheckBounds(TTauCurveParameter.Create, 'tau');
end;

procedure TParameterBoundsTest.ShapeRespectsItsBounds;
begin
  //  Pearson VII's m, which interpolates between the Lorentzian limit (m = 1) and
  //  the Gaussian one (m -> infinity), so it is floored at the Lorentzian end
  //  rather than at zero.
  CheckBounds(TShapeCurveParameter.Create, 'm');
end;

procedure TParameterBoundsTest.AsymmetryRespectsItsBounds;
begin
  //  A fraction, like eta: clamped into [0, 1], and the clamp reflects rather than
  //  stops - which CheckBounds allows for deliberately.
  CheckBounds(TAsymmetryCurveParameter.Create, 'alpha');
end;

procedure TParameterBoundsTest.SkewIsUnbounded;
begin
  //  Skew has a sign: a negative value leans the profile the other way, so unlike
  //  its neighbours it must NOT be clamped through Abs. Checked as unbounded for
  //  exactly that reason - a clamp added here would silently make every fitted
  //  profile lean one way.
  CheckUnbounded(TSkewCurveParameter.Create, 'beta');
end;

procedure TParameterBoundsTest.ACalculatedParameterIsNotFitted;
var
  P: TSpecialCurveParameter;
begin
  //  Not a bounds case: a calculated parameter is derived from the others, so the
  //  optimiser must never vary it. Its variation step is zero and it reports the
  //  minimum step as already achieved - which is how the simplex is told to leave
  //  it alone. A non-zero step here would have the fit adjusting a value that is
  //  recomputed from scratch on the next evaluation.
  P := TCalculatedCurveParameter.Create;
  try
    P.InitVariationStep;
    AssertEquals('no variation step', 0.0, P.VariationStep, 1e-12);
    AssertTrue('and the minimum step is already achieved',
      P.MinimumStepAchieved);
    AssertTrue('its type says it is computed', P.Type_ = Calculated);
  finally
    P.Free;
  end;
end;

{ ----------------- the position's window, and when it is read --------------- }

{ EVERY CURVE TYPE CONSTRUCTS ITS POSITION PARAMETER FROM INSIDE ITS OWN
  CONSTRUCTOR AND PASSES ITSELF as the points set - which at that moment holds no
  points. The window used to be read there anyway, so SetBoundaries found no
  sample either side of the seed, its fallback collapsed both bounds onto the
  seed, and the clamp was to a single value permanently: no fit could move a
  peak, and a fitted position was discarded without a word. These pin the
  lifecycle that repairs it. }

procedure TParameterBoundsTest.FillHalfUnitProfile(APoints: TPointsSet);
var
  x: double;
begin
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    APoints.AddNewPoint(x, 0);
    x := x + 0.5;
  end;
end;

procedure TParameterBoundsTest.ThePositionWindowIsUnknownWhileTheCurveHasNoPoints;
var
  PS: TPointsSet;
  P: TPositionCurveParameter;
begin
  //  UNBOUNDED, NOT PINNED. Reporting the seed twice here is what was shipped to
  //  the gradient minimizer as a parameter range, so it was asked to fit a
  //  coordinate it was simultaneously told could not move.
  PS := TPointsSet.Create(nil);
  P := TPositionCurveParameter.Create(10.0, PS);
  try
    AssertTrue('no lower bound yet', P.GetMinValue < -1e99);
    AssertTrue('no upper bound yet', P.GetMaxValue > 1e99);
  finally
    P.Free;
    PS.Free;
  end;
end;

procedure TParameterBoundsTest.ItIsReadOnceTheCurveHasPointsAndAValueIsAssigned;
var
  PS: TPointsSet;
  P: TPositionCurveParameter;
begin
  //  THE ORDER THE ENGINE USES: the curve is given its stretch of the profile
  //  first and its position second, so the assignment is the earliest moment at
  //  which both halves exist. Points added after construction, deliberately.
  PS := TPointsSet.Create(nil);
  P := TPositionCurveParameter.Create(10.0, PS);
  try
    FillHalfUnitProfile(PS);
    P.Value := 10.0;
    AssertEquals('the sample below the seed', 9.5, P.GetMinValue, 1e-12);
    AssertEquals('the sample above it', 10.5, P.GetMaxValue, 1e-12);
  finally
    P.Free;
    PS.Free;
  end;
end;

procedure TParameterBoundsTest.ItIsMeasuredFromTheSeedNotFromTheValueThatArrives;
var
  PS: TPointsSet;
  P: TPositionCurveParameter;
begin
  //  Measured from the seed the constructor was given, so the window does not
  //  depend on whether the caller assigns the seed before the optimiser's first
  //  trial value - which the native and the backend paths do not agree on. A
  //  window centred on the incoming value would bracket it and clamp nothing.
  PS := TPointsSet.Create(nil);
  P := TPositionCurveParameter.Create(10.0, PS);
  try
    FillHalfUnitProfile(PS);
    P.Value := 17.0;
    AssertEquals('still the seed''s neighbour below', 9.5, P.GetMinValue, 1e-12);
    AssertEquals('and above', 10.5, P.GetMaxValue, 1e-12);
    AssertEquals('so the far value was clamped in', 10.5, P.Value, 1e-12);
  finally
    P.Free;
    PS.Free;
  end;
end;

procedure TParameterBoundsTest.AndItDoesNotMoveWithLaterAssignments;
var
  PS: TPointsSet;
  P: TPositionCurveParameter;
begin
  //  READ ONCE. A window recomputed on every assignment would follow the
  //  optimiser downhill, one sample at a time, and the constraint would be gone
  //  while every individual step still looked clamped.
  PS := TPointsSet.Create(nil);
  FillHalfUnitProfile(PS);
  P := TPositionCurveParameter.Create(10.0, PS);
  try
    P.Value := 10.2;
    P.Value := 10.4;
    P.Value := 10.45;
    AssertEquals('lower bound unmoved', 9.5, P.GetMinValue, 1e-12);
    AssertEquals('upper bound unmoved', 10.5, P.GetMaxValue, 1e-12);
  finally
    P.Free;
    PS.Free;
  end;
end;

procedure TParameterBoundsTest.APositionWithNoPointsAtAllClampsNothing;
var
  P: TPositionCurveParameter;
begin
  //  Nil rather than empty: the parameterless constructor is what CreateCopy
  //  uses, and a parameter with nothing to measure against must store what it
  //  is given rather than invent a limit.
  P := TPositionCurveParameter.Create(10.0, nil);
  try
    P.Value := -4321.5;
    AssertEquals('stored unchanged', -4321.5, P.Value, 1e-9);
  finally
    P.Free;
  end;
end;

procedure TParameterBoundsTest.ACopyKeepsTheWindowItWasGiven;
var
  PS: TPointsSet;
  P, C: TSpecialCurveParameter;
begin
  //  A copy belongs to a different curve. It inherits the window and the fact
  //  that the window is settled, and NOT the points - recomputing against the
  //  original's profile would widen or narrow the copy's limits behind the
  //  caller's back, and the copy is what a saved and reloaded model is built of.
  PS := TPointsSet.Create(nil);
  FillHalfUnitProfile(PS);
  P := TPositionCurveParameter.Create(10.0, PS);
  try
    C := P.CreateCopy;
    try
      AssertEquals('lower bound copied', 9.5, C.GetMinValue, 1e-12);
      AssertEquals('upper bound copied', 10.5, C.GetMaxValue, 1e-12);
      C.Value := 17.0;
      AssertEquals('and it still clamps', 10.5, C.Value, 1e-12);
    finally
      C.Free;
    end;
  finally
    P.Free;
    PS.Free;
  end;
end;

initialization
  RegisterTest('unit', TParameterBoundsTest);
end.
