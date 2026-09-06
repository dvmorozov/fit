// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(One sweep over every kind of curve parameter, asserting the policy each
one is.)

A PARAMETER CLASS IS A POLICY AND ALMOST NOTHING ELSE. Eleven of them exist, each
about forty lines, each saying the same five things about a different quantity:
what it is called, what it starts at, how far the optimiser first steps it, how
small a step means "converged", and what range it refuses to leave. There is no
behaviour to speak of beyond those five answers.

WHY A SWEEP AND NOT ELEVEN FIXTURES. Written one fixture per class, the eleventh
gets copied from the tenth and quietly keeps the tenth's numbers - which is how a
new parameter arrives clamped to somebody else's range. Written as one table, the
next parameter added is a row, and a row that is missing is a compile error
rather than a class nobody noticed had no test.

WHAT AN UNTESTED POLICY COSTS. Nothing raises. A width that starts at zero, or a
mixing fraction that is allowed past one, gives the optimiser a starting point or
a feasible region that is wrong for that quantity, and what the user sees is a
fit that will not converge on data that is perfectly good.
}
unit testcase_parameter_policies;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    special_curve_parameter,
    amplitude_curve_parameter, asymmetry_curve_parameter,
    calculated_curve_parameter, delta_sigma_curve_parameter,
    eta_curve_parameter, gamma_curve_parameter, position_curve_parameter,
    shape_curve_parameter, sigma_curve_parameter, skew_curve_parameter,
    tau_curve_parameter, points_set,
    //  For TINY - the smallest value a dividing parameter is allowed to take.
    //  The same unit the parameter classes themselves take it from; three other
    //  units in the tree declare a TINY with a different value.
    SimpMath;

type
    { How a parameter answers a value outside what it allows. }
    TClamp = (
        //  Takes it as given - the quantity is a free real.
        clNone,
        //  Folds the sign away: the quantity has a magnitude and no direction.
        clAbsolute,
        //  Folds the sign away and refuses zero, because the value divides.
        clAbsoluteNonZero,
        //  Folds the sign away and holds a floor above zero.
        clAbsoluteFloored,
        //  Folds the sign away and holds a ceiling: a fraction of one.
        clAbsoluteCapped);

    TPolicy = record
        Title: string;
        Name: string;
        Kind: TParameterType;
        StartsAt: double;
        FirstStep: double;
        Low: double;
        High: double;
        Clamp: TClamp;
        { The step below which the optimiser calls this parameter converged. }
        ConvergedBelow: double;
    end;

    TParameterPolicyTest = class(TTestCase)
    private
        { The parameter for row I of the table. A case, not a metaclass: the
          constructors are not virtual, so a class reference would reach the
          base one - which calls abstract methods. }
        function Make(I: longint): TSpecialCurveParameter;
        function Policy(I: longint): TPolicy;
        function Count: longint;
    published
        procedure EveryParameterIsNamed;
        procedure EveryParameterHasItsKind;
        procedure EveryParameterStartsWhereItSays;
        procedure EveryParameterHasAFirstStep;
        procedure EveryParameterReportsItsRange;
        procedure AnUnboundedParameterSaysSoWithAnInfinity;
        procedure ARangeIsNotInsideOut;
        procedure AValueInsideTheRangeIsTakenAsGiven;
        procedure AValueBelowTheRangeIsBroughtBack;
        procedure AValueAboveTheRangeIsBroughtBack;
        procedure ADividingParameterNeverBecomesZero;
        procedure AFreeParameterKeepsItsSign;

        procedure AStepAboveTheThresholdIsNotConverged;
        procedure AStepBelowTheThresholdIsConverged;
        procedure MultiplyingTheStepScalesIt;

        procedure ACopyIsOfTheSameClass;
        procedure ACopyCarriesTheValueAndTheStep;
        procedure ACopyIsIndependentOfItsOriginal;

        procedure ReInitialisingRestoresTheStartingValue;
        procedure ReInitialisingRestoresTheFirstStep;

        procedure EveryStartingValueIsInsideItsOwnRange;
        procedure EveryParameterStartsNumeric;
    end;

    { The position parameter, which is not in the table above because it is the
      one parameter whose range comes from the DATA rather than from the class -
      the neighbouring measured points on either side of the peak. }
    TPositionParameterTest = class(TTestCase)
    private
        FProfile: TPointsSet;
        function APosition(x0: double): TPositionCurveParameter;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ItIsBoundedByItsNeighbouringPoints;
        procedure ItWillNotLeaveThatWindow;
        procedure ItKeepsAValueInsideTheWindow;
        procedure ItIsNamedAndVariable;
        procedure ACopyCarriesTheWindowAndNotJustTheValue;
        procedure ConstructedWithoutAProfileItIsUnusable;
    end;

implementation

function TParameterPolicyTest.Count: longint;
begin
    Result := 10;
end;

function TParameterPolicyTest.Make(I: longint): TSpecialCurveParameter;
begin
    case I of
        0: Result := TAmplitudeCurveParameter.Create;
        1: Result := TAsymmetryCurveParameter.Create;
        2: Result := TDeltaSigmaCurveParameter.Create;
        3: Result := TEtaCurveParameter.Create;
        4: Result := TGammaCurveParameter.Create;
        5: Result := TShapeCurveParameter.Create;
        6: Result := TSigmaCurveParameter.Create;
        7: Result := TSkewCurveParameter.Create;
        8: Result := TTauCurveParameter.Create;
        9: Result := TCalculatedCurveParameter.Create;
        else
            raise Exception.CreateFmt('no parameter %d', [I]);
    end;
end;

function TParameterPolicyTest.Policy(I: longint): TPolicy;
begin
    Result := Default(TPolicy);
    Result.Kind := Variable;
    //  The base class's answer: a free real. Infinity, not a large finite
    //  number - a bounded backend is handed these verbatim, and 1e300 is a
    //  bound it would try to honour.
    Result.Low := NegInfinity;
    Result.High := Infinity;
    Result.Clamp := clNone;
    //  The common threshold. Amplitude is the one exception, below.
    Result.ConvergedBelow := 0.00001;
    case I of
        0: begin
            //  THE AREA UNDER THE CURVE, not its height - a count of events, so
            //  a negative one has no meaning and is folded away.
            Result.Title := 'amplitude';
            Result.Name := 'A';
            Result.StartsAt := 0;
            Result.FirstStep := 0.1;
            Result.Low := 0;
            Result.Clamp := clAbsolute;
            //  Ten times coarser than the rest: it is measured in counts, where
            //  the last decimal of a step is below the noise.
            Result.ConvergedBelow := 0.0001;
        end;
        1: begin
            //  A fraction: how much of the peak leans one way.
            Result.Title := 'asymmetry';
            Result.Name := 'alpha';
            Result.StartsAt := 0.1;
            Result.FirstStep := 0.05;
            Result.Low := 0;
            Result.High := 1;
            Result.Clamp := clAbsoluteCapped;
        end;
        2: begin
            //  A DIFFERENCE of two widths, so it is signed - which is why it is
            //  the one width-like parameter that is not folded.
            Result.Title := 'delta sigma';
            Result.Name := 'deltasigma';
            Result.StartsAt := 0;
            Result.FirstStep := 0.1;
        end;
        3: begin
            //  The mixing fraction of a pseudo-Voigt: all Gaussian at 0, all
            //  Lorentzian at 1, and nothing outside.
            Result.Title := 'eta';
            Result.Name := 'eta';
            Result.StartsAt := 0;
            Result.FirstStep := 0.1;
            Result.Low := 0;
            Result.High := 1;
            Result.Clamp := clAbsoluteCapped;
        end;
        4: begin
            //  A Lorentzian half-width. It divides, so it may approach zero and
            //  never reach it.
            Result.Title := 'gamma';
            Result.Name := 'gamma';
            Result.StartsAt := 0.25;
            Result.FirstStep := 0.1;
            Result.Low := TINY;
            Result.Clamp := clAbsoluteNonZero;
        end;
        5: begin
            //  The Pearson exponent. Floored well above zero rather than at it:
            //  the shape degenerates long before the arithmetic does.
            Result.Title := 'shape';
            Result.Name := 'm';
            Result.StartsAt := 1.5;
            Result.FirstStep := 0.1;
            Result.Low := ShapeMin;
            Result.Clamp := clAbsoluteFloored;
        end;
        6: begin
            Result.Title := 'sigma';
            Result.Name := 'sigma';
            Result.StartsAt := 0.25;
            Result.FirstStep := 0.1;
            Result.Low := TINY;
            Result.Clamp := clAbsoluteNonZero;
        end;
        7: begin
            //  Skew: zero is symmetric and either sign is a real shape, so this
            //  one must NOT be folded. A copy-paste of a width's SetValue would
            //  make every skewed curve lean the same way.
            Result.Title := 'skew';
            Result.Name := 'beta';
            Result.StartsAt := 0;
            Result.FirstStep := 0.1;
        end;
        8: begin
            //  The decay constant of an exponentially modified Gaussian. It
            //  divides.
            Result.Title := 'tau';
            Result.Name := 'tau';
            Result.StartsAt := 1.0;
            Result.FirstStep := 0.1;
            Result.Low := TINY;
            Result.Clamp := clAbsoluteNonZero;
        end;
        9: begin
            //  Not fitted at all: the application computes it. A zero step is
            //  the statement of that, and it reports converged always.
            Result.Title := 'calculated';
            Result.Name := '';
            Result.Kind := Calculated;
            Result.StartsAt := 0;
            Result.FirstStep := 0;
            Result.ConvergedBelow := Infinity;
        end;
    end;
end;

{ ---- what each one says about itself --------------------------------------- }

procedure TParameterPolicyTest.EveryParameterIsNamed;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  THE NAME IS THE WIRE FORMAT. A fitted parameter is matched back to the
    //  curve it belongs to BY NAME, not by index - so a renamed parameter is a
    //  result silently dropped, and a formula that names it stops resolving.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertEquals(Policy(i).Title, Policy(i).Name, P.Name);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.EveryParameterHasItsKind;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  The kind decides whether the optimiser varies it at all. One that came
    //  back Calculated by mistake is held fixed through every fit, and the user
    //  sees a curve that will not move.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertTrue(Policy(i).Title, Policy(i).Kind = P.Type_);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.EveryParameterStartsWhereItSays;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  The starting point of the search. A downhill simplex finds the nearest
    //  minimum, not the best one, so where a parameter starts decides which fit
    //  the user gets on data with more than one plausible answer.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertEquals(Policy(i).Title, Policy(i).StartsAt, P.Value, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.EveryParameterHasAFirstStep;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  The size of the initial simplex in this parameter's direction. Too small
    //  and the search never leaves its starting point; too large and it steps
    //  straight past the minimum on the first cycle.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertEquals(Policy(i).Title, Policy(i).FirstStep,
                P.VariationStep, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.EveryParameterReportsItsRange;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  REPORTED, not merely enforced. The native engine clamps in SetValue; the
    //  Python backend is handed the bounds instead and searches inside them. The
    //  two must describe the same feasible region or the same model fitted by
    //  the two engines has two different answers.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            //  Skipping the unbounded ends: subtracting one infinity from
            //  another to compare them raises. They have their own test below.
            if not IsInfinite(Policy(i).Low) then
                AssertEquals(Policy(i).Title + ' low',
                    Policy(i).Low, P.GetMinValue, 1E-12);
            if not IsInfinite(Policy(i).High) then
                AssertEquals(Policy(i).Title + ' high',
                    Policy(i).High, P.GetMaxValue, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.AnUnboundedParameterSaysSoWithAnInfinity;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  AN INFINITY, NOT A LARGE NUMBER. The bounded backend is handed these
    //  verbatim as the box it searches in; a finite stand-in like 1e300 is a
    //  bound it would try to honour, and one arithmetic step inside the
    //  optimiser overflows it.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            if IsInfinite(Policy(i).Low) then
            begin
                AssertTrue(Policy(i).Title + ' has no floor',
                    IsInfinite(P.GetMinValue));
                AssertTrue(Policy(i).Title + ' and it is the negative one',
                    P.GetMinValue < 0);
            end;
            if IsInfinite(Policy(i).High) then
            begin
                AssertTrue(Policy(i).Title + ' has no ceiling',
                    IsInfinite(P.GetMaxValue));
                AssertTrue(Policy(i).Title + ' and it is the positive one',
                    P.GetMaxValue > 0);
            end;
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.ARangeIsNotInsideOut;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  A low above its high is an empty feasible region: the bounded backend
    //  refuses the fit outright, with a message about the optimiser rather than
    //  about the parameter that is wrong.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertTrue(Policy(i).Title, P.GetMinValue < P.GetMaxValue);
        finally
            P.Free;
        end;
    end;
end;

{ ---- what each one does with a value --------------------------------------- }

procedure TParameterPolicyTest.AValueInsideTheRangeIsTakenAsGiven;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  0.5 is inside every range in the table, so nothing may alter it. A clamp
    //  that fired here would move a value the optimiser chose, and the optimiser
    //  would read back a point it did not evaluate.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.Value := 0.5;
            AssertEquals(Policy(i).Title, 0.5, P.Value, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.AValueBelowTheRangeIsBroughtBack;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  The optimiser proposes points, including impossible ones - that is what
    //  a simplex reflection through a face does. Each parameter is what stops
    //  an impossible proposal reaching the model.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.Value := -0.5;
            AssertTrue(Policy(i).Title + ': not below its floor',
                P.Value >= P.GetMinValue);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.AValueAboveTheRangeIsBroughtBack;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.Value := 5;
            AssertTrue(Policy(i).Title + ': not above its ceiling',
                P.Value <= P.GetMaxValue);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.ADividingParameterNeverBecomesZero;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  A WIDTH THAT REACHES ZERO DIVIDES BY IT. The result is a NaN in the model
    //  values, the goal function compares NaN and takes the wrong branch, and
    //  the fit wanders for its full cycle budget on a model that is no longer a
    //  number. Asked for zero, these three answer with the smallest value they
    //  have instead.
    for i := 0 to Count - 1 do
        if Policy(i).Clamp = clAbsoluteNonZero then
        begin
            P := Make(i);
            try
                P.Value := 0;
                AssertTrue(Policy(i).Title + ' refused zero', P.Value > 0);
            finally
                P.Free;
            end;
        end;
end;

procedure TParameterPolicyTest.AFreeParameterKeepsItsSign;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  The other half of the same rule, and the one a copy-pasted SetValue
    //  breaks: a skew or a width difference is signed, and folding it makes
    //  every asymmetric curve lean the same way whatever the data says.
    for i := 0 to Count - 1 do
        if Policy(i).Clamp = clNone then
        begin
            P := Make(i);
            try
                P.Value := -0.5;
                AssertEquals(Policy(i).Title + ' kept its sign',
                    -0.5, P.Value, 1E-12);
            finally
                P.Free;
            end;
        end;
end;

{ ---- the step, and when it means converged --------------------------------- }

procedure TParameterPolicyTest.AStepAboveTheThresholdIsNotConverged;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  Reported converged too early, the fit stops while it is still improving
    //  and the user is shown a worse model with no indication anything stopped
    //  short.
    for i := 0 to Count - 1 do
        if not IsInfinite(Policy(i).ConvergedBelow) then
        begin
            P := Make(i);
            try
                P.VariationStep := Policy(i).ConvergedBelow * 10;
                AssertFalse(Policy(i).Title, P.MinimumStepAchieved);
            finally
                P.Free;
            end;
        end;
end;

procedure TParameterPolicyTest.AStepBelowTheThresholdIsConverged;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  And never reported converged, the fit runs its whole cycle budget every
    //  time - which is the difference between a fit that takes a second and one
    //  that takes a minute.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            if IsInfinite(Policy(i).ConvergedBelow) then
                //  The calculated parameter: nothing varies it, so it is done
                //  whatever its step says.
                AssertTrue(Policy(i).Title, P.MinimumStepAchieved)
            else
            begin
                P.VariationStep := Policy(i).ConvergedBelow / 10;
                AssertTrue(Policy(i).Title, P.MinimumStepAchieved);
            end;
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.MultiplyingTheStepScalesIt;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  How the search narrows: after a cycle the engine shrinks every step by a
    //  common factor. One parameter that ignored the factor would keep stepping
    //  coarsely while the rest refined, and the fit would not settle.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.VariationStep := 1;
            P.MultiplyVariationStep(0.25);
            AssertEquals(Policy(i).Title, 0.25, P.VariationStep, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

{ ---- copying --------------------------------------------------------------- }

procedure TParameterPolicyTest.ACopyIsOfTheSameClass;
var
    i: longint;
    P, C: TSpecialCurveParameter;
begin
    //  A COPY OF THE WRONG CLASS IS A COPY WITH THE WRONG POLICY. It carries the
    //  right value and the right name, so nothing looks amiss - and then clamps
    //  to somebody else's range on the first step the optimiser takes.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            C := P.CreateCopy;
            try
                AssertEquals(Policy(i).Title, P.ClassName, C.ClassName);
            finally
                C.Free;
            end;
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.ACopyCarriesTheValueAndTheStep;
var
    i: longint;
    P, C: TSpecialCurveParameter;
begin
    //  Copies are taken between the model and the fit and back on every cycle.
    //  A field left behind is a value that reverts to its default halfway
    //  through a fit, which reads as an optimiser that diverged.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.Value := 0.5;
            P.VariationStep := 0.375;
            P.VariationDisabled := True;
            P.SavedValue := 7;
            C := P.CreateCopy;
            try
                AssertEquals(Policy(i).Title + ' value',
                    0.5, C.Value, 1E-12);
                AssertEquals(Policy(i).Title + ' step',
                    0.375, C.VariationStep, 1E-12);
                AssertTrue(Policy(i).Title + ' variation flag',
                    C.VariationDisabled);
                AssertEquals(Policy(i).Title + ' saved value',
                    7.0, C.SavedValue, 1E-12);
                AssertEquals(Policy(i).Title + ' name', P.Name, C.Name);
                AssertTrue(Policy(i).Title + ' kind', P.Type_ = C.Type_);
            finally
                C.Free;
            end;
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.ACopyIsIndependentOfItsOriginal;
var
    i: longint;
    P, C: TSpecialCurveParameter;
begin
    //  A copy sharing state with its original would have the fit writing into
    //  the model it is fitting, so the reference it compares against moves with
    //  it and every trial looks like an improvement.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.Value := 0.5;
            C := P.CreateCopy;
            try
                C.Value := 0.25;
                AssertEquals(Policy(i).Title, 0.5, P.Value, 1E-12);
            finally
                C.Free;
            end;
        finally
            P.Free;
        end;
    end;
end;

{ ---- going back to the start ----------------------------------------------- }

procedure TParameterPolicyTest.ReInitialisingRestoresTheStartingValue;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  What "start the fit again" means. A parameter that kept the previous
    //  fit's value would make a second run of the same fit give a different
    //  answer from the first.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.Value := 0.5;
            P.InitValue;
            AssertEquals(Policy(i).Title, Policy(i).StartsAt, P.Value, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.ReInitialisingRestoresTheFirstStep;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  And the step, which the previous fit shrank to its convergence threshold
    //  - restarting without restoring it starts the next fit already converged.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            P.VariationStep := 0.000001;
            P.InitVariationStep;
            AssertEquals(Policy(i).Title, Policy(i).FirstStep,
                P.VariationStep, 1E-12);
        finally
            P.Free;
        end;
    end;
end;

{ ---- the two rules that tie the table together ----------------------------- }

procedure TParameterPolicyTest.EveryStartingValueIsInsideItsOwnRange;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  A DEFAULT OUTSIDE ITS OWN BOUNDS is the one inconsistency the table above
    //  cannot show by inspection, because the two numbers are written in
    //  different methods of the same class. The bounded backend would refuse the
    //  starting point of a fit nobody has touched.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertTrue(Policy(i).Title + ': not below its own floor',
                P.Value >= P.GetMinValue);
            AssertTrue(Policy(i).Title + ': not above its own ceiling',
                P.Value <= P.GetMaxValue);
        finally
            P.Free;
        end;
    end;
end;

procedure TParameterPolicyTest.EveryParameterStartsNumeric;
var
    i: longint;
    P: TSpecialCurveParameter;
begin
    //  The value is a Variant, and an unassigned one reports itself non-numeric
    //  - which the persistence layer reads as "this is a label, write it as
    //  text". A parameter that started that way would be saved as a string and
    //  come back as one.
    for i := 0 to Count - 1 do
    begin
        P := Make(i);
        try
            AssertTrue(Policy(i).Title, P.IsNumeric);
        finally
            P.Free;
        end;
    end;
end;

{ ---- the one parameter whose range comes from the data --------------------- }

procedure TPositionParameterTest.SetUp;
begin
    //  Five points a unit apart. A peak sitting at 2 therefore has measured
    //  neighbours at 1 and 3, which is the window it may move in.
    FProfile := TPointsSet.Create(nil);
    FProfile.AddNewPoint(0, 10);
    FProfile.AddNewPoint(1, 20);
    FProfile.AddNewPoint(2, 50);
    FProfile.AddNewPoint(3, 20);
    FProfile.AddNewPoint(4, 10);
end;

procedure TPositionParameterTest.TearDown;
begin
    FreeAndNil(FProfile);
end;

function TPositionParameterTest.APosition(x0: double): TPositionCurveParameter;
begin
    Result := TPositionCurveParameter.Create(x0, FProfile);
end;

procedure TPositionParameterTest.ItIsBoundedByItsNeighbouringPoints;
var
    P: TPositionCurveParameter;
begin
    //  WHY THE DATA AND NOT A CONSTANT. A peak may not slide past the points
    //  that define it: allowed to, two curves in a multi-peak model swap places
    //  mid-fit, and the result is a model where curve 1 fits peak 2. The window
    //  is one measured point either side.
    P := APosition(2);
    try
        AssertEquals('the point below', 1.0, P.GetMinValue, 1E-12);
        AssertEquals('and the point above', 3.0, P.GetMaxValue, 1E-12);
    finally
        P.Free;
    end;
end;

procedure TPositionParameterTest.ItWillNotLeaveThatWindow;
var
    P: TPositionCurveParameter;
begin
    P := APosition(2);
    try
        P.Value := -100;
        AssertEquals('held at the floor', 1.0, P.Value, 1E-12);
        P.Value := 100;
        AssertEquals('and at the ceiling', 3.0, P.Value, 1E-12);
    finally
        P.Free;
    end;
end;

procedure TPositionParameterTest.ItKeepsAValueInsideTheWindow;
var
    P: TPositionCurveParameter;
begin
    //  The other half: a clamp that fired inside its own range would move a
    //  position the optimiser chose, and the optimiser would read back a point
    //  it never evaluated.
    P := APosition(2);
    try
        P.Value := 2.5;
        AssertEquals('taken as given', 2.5, P.Value, 1E-12);
    finally
        P.Free;
    end;
end;

procedure TPositionParameterTest.ItIsNamedAndVariable;
var
    P: TPositionCurveParameter;
begin
    P := APosition(2);
    try
        AssertEquals('the name a formula refers to it by', 'x0', P.Name);
        AssertTrue('and the fit may move it', P.Type_ = VariablePosition);
    finally
        P.Free;
    end;
end;

procedure TPositionParameterTest.ACopyCarriesTheWindowAndNotJustTheValue;
var
    P, C: TSpecialCurveParameter;
begin
    //  THE WINDOW IS THE PART THAT IS EASY TO DROP, because it lives in two
    //  private fields rather than in the base class - which is why this class is
    //  the only parameter that overrides CopyTo. A copy without it is bounded by
    //  [0, 0] and pinned to zero on the first step the optimiser takes.
    P := APosition(2);
    try
        P.Value := 2.5;
        C := P.CreateCopy;
        try
            AssertEquals('the value', 2.5, C.Value, 1E-12);
            AssertEquals('the floor', 1.0, C.GetMinValue, 1E-12);
            AssertEquals('and the ceiling', 3.0, C.GetMaxValue, 1E-12);
        finally
            C.Free;
        end;
    finally
        P.Free;
    end;
end;

procedure TPositionParameterTest.ConstructedWithoutAProfileItIsUnusable;
var
    P: TSpecialCurveParameter;
begin
    //  THE PARAMETERLESS CONSTRUCTOR IS PRIVATE, so `TPositionCurveParameter.
    //  Create` written outside this unit does not raise and does not fail to
    //  compile - it silently reaches the BASE class's constructor, which knows
    //  nothing of names or windows. The result is a parameter called nothing,
    //  typed Calculated, bounded by [0, 0] and therefore pinned to zero.
    //
    //  Pinned rather than reported as a defect: the private constructor is how
    //  the class states that a position needs a profile, and CreateCopy uses it
    //  from inside. What is worth having written down is that the mistake is
    //  silent, so a caller who makes it looks here rather than at the optimiser.
    P := TPositionCurveParameter.Create;
    try
        AssertEquals('no name', '', P.Name);
        AssertEquals('a window of nothing', 0.0, P.GetMinValue, 1E-12);
        AssertEquals('at both ends', 0.0, P.GetMaxValue, 1E-12);
        P.Value := 2.5;
        AssertEquals('so every value is pinned to zero', 0.0, P.Value, 1E-12);
    finally
        P.Free;
    end;
end;

initialization
    //  Unit tests: ten parameters in memory, and one small profile. No curve and
    //  no optimiser.
    RegisterTest('unit', TParameterPolicyTest);
    RegisterTest('unit', TPositionParameterTest);
end.
