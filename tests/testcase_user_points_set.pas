// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The curve whose shape is a formula the user typed.)

EVERY OTHER CURVE TYPE IS A FORMULA IN PASCAL. This one is a formula in a string,
evaluated point by point through the expression engine, with the curve's own
parameters substituted by NAME on every evaluation. That makes it the one curve
type whose behaviour depends on data the user supplied, and the only one where a
mistake shows up as a curve of the wrong shape rather than as a compile error.

THREE THINGS ARE EASY TO GET WRONG HERE AND HARD TO SEE.

The substitution is by name, so a parameter renamed anywhere - in the grid, in a
saved profile - stops reaching the formula, and the formula then evaluates with
that name unbound.

A formula can fail to produce a number at a point the optimiser probes: a zero
denominator, a log of something negative. The engine reports that, and the curve
answers ZERO rather than propagating a NaN, so the fit walks away from the region
instead of poisoning every comparison downstream. That decision is one line and
nothing exercised it.

And the formula has to reach the Python backend too, in numpy's spelling - a
curve that fits under one engine and not the other is a defect the user reports
as "the Python minimizer is broken".
}
unit testcase_user_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    user_points_set, points_set, named_points_set,
    persistent_curve_parameters, persistent_curve_parameter_container,
    special_curve_parameter, user_curve_parameter,
    curve_types_singleton, native_math_expr;

type
    TUserPointsSetTest = class(TTestCase)
    private
        FCurve: TUserPointsSet;
        procedure AddParam(P: Curve_parameters; const AName: string;
            AType: TParameterType; AValue: double);
        { A curve over x = 0..4 with A, x, x0 and SIGMA, and the given formula. }
        procedure GivenTheFormula(const AExpression: string);
        function ValueAt(AIndex: longint): double;
    protected
        procedure TearDown; override;
    published
        //  What the type says it is.
        procedure ItIsNamedForTheUser;
        procedure ItsIdentifierIsFixed;
        procedure ItLooksForPeaksAndTroughsAlike;

        //  Evaluating the formula.
        procedure TheFormulaIsEvaluatedAtEveryPoint;
        procedure ParametersReachTheFormulaByName;
        procedure ChangingAParameterChangesTheCurve;
        procedure AFormulaThatIsNotAFunctionOfXIsFlat;
        procedure APointWhereTheFormulaHasNoValueReadsZero;
        procedure ThatPointDoesNotSpoilTheOthers;
        procedure AnUnboundNameLeavesThePointAtZero;

        //  Which parameters the optimiser may move.
        procedure TheArgumentIsNotOneOfTheFittedParameters;
        procedure TheFittedParametersAreOfferedByName;

        //  Carrying the formula around.
        procedure ACopyCarriesTheFormula;
        procedure TheFormulaIsAlsoOfferedInNumpysSpelling;
        procedure AnEmptyFormulaTranslatesToNothing;
    end;

implementation

procedure TUserPointsSetTest.AddParam(P: Curve_parameters;
    const AName: string; AType: TParameterType; AValue: double);
var
    Param: TSpecialCurveParameter;
    Cont: TPersistentCurveParameterContainer;
begin
    Param := TUserCurveParameter.Create;
    Param.Name := AName;
    Param.Type_ := AType;
    Param.Value := AValue;
    Cont := TPersistentCurveParameterContainer(P.Params.Add);
    Cont.Parameter := Param;
end;

procedure TUserPointsSetTest.GivenTheFormula(const AExpression: string);
var
    Params: Curve_parameters;
    i: longint;
begin
    Params := Curve_parameters.Create(nil);
    //  The collection arrives with one placeholder parameter; a curve built on
    //  top of it would offer the optimiser a parameter no formula names.
    Params.Params.Clear;
    AddParam(Params, 'A', Variable, 2);
    AddParam(Params, 'x', Argument, 0);
    AddParam(Params, 'x0', InvariablePosition, 0);
    AddParam(Params, 'SIGMA', Variable, 1);

    FCurve := TUserPointsSet.Create(nil);
    FCurve.SetParameters(Params);
    FCurve.Expression := AExpression;
    for i := 0 to 4 do
        FCurve.AddNewPoint(i, 0);
end;

function TUserPointsSetTest.ValueAt(AIndex: longint): double;
begin
    FCurve.ReCalc;
    Result := FCurve.PointYCoord[AIndex];
end;

procedure TUserPointsSetTest.TearDown;
begin
    FreeAndNil(FCurve);
end;

{ ---- what the type says it is ---------------------------------------------- }

procedure TUserPointsSetTest.ItIsNamedForTheUser;
begin
    //  The caption in the Add Curve menu. Stated here so a rename is a
    //  deliberate change to a string users recognise.
    AssertEquals('User Defined', TUserPointsSet.GetCurveTypeName);
end;

procedure TUserPointsSetTest.ItsIdentifierIsFixed;
begin
    //  THE ID IS WRITTEN INTO SAVED PROFILES. Changing it makes every profile
    //  ever saved with a user curve fail to reopen - and the failure is a curve
    //  type that cannot be resolved, not a message about a changed identifier.
    AssertEquals('{D8CAFCE5-8B03-4CCE-9E93-EA28ACB8E7CA}',
        GUIDToString(TUserPointsSet.GetCurveTypeId));
end;

procedure TUserPointsSetTest.ItLooksForPeaksAndTroughsAlike;
begin
    //  Every built-in shape is a peak, so the placement helper looks only for
    //  maxima. A user's formula may be anything, so both directions count -
    //  otherwise a formula that dips is unplaceable by clicking on it.
    AssertTrue('both directions',
        TUserPointsSet.GetExtremumMode = MaximumsAndMinimums);
end;

{ ---- evaluating the formula ------------------------------------------------ }

procedure TUserPointsSetTest.TheFormulaIsEvaluatedAtEveryPoint;
var
    i: longint;
begin
    //  EVERY point, with no interval optimisation: the shape is unknown, so
    //  there is no region the curve can be assumed to be zero in. A partial
    //  recalculation would leave stale values from the previous parameters in
    //  the part that was skipped.
    GivenTheFormula('A*x');
    for i := 0 to 4 do
        AssertEquals(Format('point %d', [i]), 2.0 * i, ValueAt(i), 1E-9);
end;

procedure TUserPointsSetTest.ParametersReachTheFormulaByName;
begin
    //  BY NAME, not by position. A parameter renamed in the grid stops reaching
    //  the formula, and the formula then evaluates with that name unbound -
    //  which is the next test.
    GivenTheFormula('SIGMA');
    AssertEquals('SIGMA was substituted', 1.0, ValueAt(0), 1E-9);
end;

procedure TUserPointsSetTest.ChangingAParameterChangesTheCurve;
begin
    //  What the optimiser does on every trial step. A curve that did not follow
    //  its parameters would report the same goal function whatever the
    //  optimiser tried, and the fit would stop after one cycle claiming
    //  convergence.
    GivenTheFormula('A*x');
    AssertEquals('before', 4.0, ValueAt(2), 1E-9);
    FCurve.ValuesByName['A'] := 5;
    AssertEquals('after', 10.0, ValueAt(2), 1E-9);
end;

procedure TUserPointsSetTest.AFormulaThatIsNotAFunctionOfXIsFlat;
begin
    //  A constant is a legitimate user curve - a flat background - and it is
    //  also the smallest formula that does not name the argument at all.
    GivenTheFormula('A');
    AssertEquals('at one end', 2.0, ValueAt(0), 1E-9);
    AssertEquals('and the other', 2.0, ValueAt(4), 1E-9);
end;

procedure TUserPointsSetTest.APointWhereTheFormulaHasNoValueReadsZero;
begin
    //  A ZERO DENOMINATOR AT x = 0. The optimiser probes wherever its simplex
    //  takes it, including parameter values that make the formula undefined
    //  somewhere in the interval - so this is not a formula the user got wrong,
    //  it is an ordinary step of an ordinary fit.
    //
    //  Zero rather than a NaN, because a NaN compares false against everything:
    //  the goal function would take the wrong branch, the fit would neither
    //  improve nor stop, and the user would see it run its whole cycle budget
    //  and report a model that is not a number.
    GivenTheFormula('A/x');
    AssertEquals('no value here', 0.0, ValueAt(0), 1E-9);
end;

procedure TUserPointsSetTest.ThatPointDoesNotSpoilTheOthers;
begin
    //  The other half: one undefined point must not blank the curve. A fit that
    //  lost every point because of one is a fit with nothing to minimise.
    GivenTheFormula('A/x');
    AssertEquals('the next point is real', 2.0, ValueAt(1), 1E-9);
    AssertEquals('and so is the last', 0.5, ValueAt(4), 1E-9);
end;

procedure TUserPointsSetTest.AnUnboundNameLeavesThePointAtZero;
begin
    //  WHAT A RENAMED PARAMETER LOOKS LIKE. The formula names something the
    //  curve does not have, the engine cannot evaluate it, and the curve is
    //  flat zero - which reads as a fit that will not start rather than as a
    //  name that no longer matches.
    GivenTheFormula('A*missing');
    AssertEquals('nothing to evaluate', 0.0, ValueAt(2), 1E-9);
end;

{ ---- which parameters the optimiser may move ------------------------------- }

procedure TUserPointsSetTest.TheArgumentIsNotOneOfTheFittedParameters;
var
    i: longint;
begin
    //  THE ABSCISSA IS NOT A PARAMETER. Offered to the optimiser it would be
    //  varied like any other, and the curve would be evaluated at an x that has
    //  nothing to do with the point being computed - every point taking the
    //  same value.
    GivenTheFormula('A*x');
    for i := 0 to FCurve.VariableCount - 1 do
        AssertTrue('the argument is not offered',
            FCurve.VariableNames[i] <> 'x');
end;

procedure TUserPointsSetTest.TheFittedParametersAreOfferedByName;
var
    i: longint;
    Names: string;
begin
    //  Two of the four: A and SIGMA are Variable; x is the argument and x0 is
    //  an invariable position, both fixed. Asserted by name rather than by
    //  count so that a filter which kept the right NUMBER of parameters but the
    //  wrong ones still fails.
    GivenTheFormula('A*x');
    Names := '';
    for i := 0 to FCurve.VariableCount - 1 do
        Names := Names + FCurve.VariableNames[i] + ' ';
    AssertEquals('exactly the two variable ones', 'A SIGMA ', Names);
end;

{ ---- carrying the formula around ------------------------------------------- }

procedure TUserPointsSetTest.ACopyCarriesTheFormula;
var
    Other: TUserPointsSet;
begin
    //  A COPY WITHOUT THE FORMULA IS A CURVE WITH NO SHAPE, and copies are
    //  taken between the model and the fit. The base class knows nothing about
    //  an expression, so this one line is the whole of it - and a curve type
    //  that forgot to override CopyParameters would produce flat zeros in the
    //  fit while the model on screen still showed the right shape.
    GivenTheFormula('A*x');
    Other := TUserPointsSet.Create(nil);
    try
        FCurve.CopyParameters(Other);
        AssertEquals('the formula came with it', 'A*x', Other.Expression);
    finally
        Other.Free;
    end;
end;

procedure TUserPointsSetTest.TheFormulaIsAlsoOfferedInNumpysSpelling;
begin
    //  THE SAME CURVE UNDER THE OTHER ENGINE. The Python backend evaluates
    //  numpy, not fpexprpars, and a user curve that fitted natively and not
    //  under Python is reported as "the Python minimizer is broken".
    //
    //  The translation itself is pinned in testcase_native_math_expr; what is
    //  asserted here is that the curve offers the translated form rather than
    //  the raw one.
    GivenTheFormula('A*exp(x)');
    AssertEquals('translated', ExpressionToNumpy('A*exp(x)'),
        FCurve.GetCurveExpression);
    AssertTrue('and it is not simply the raw text',
        FCurve.GetCurveExpression <> '');
end;

procedure TUserPointsSetTest.AnEmptyFormulaTranslatesToNothing;
begin
    //  The state a curve is in between being created and being configured. It
    //  has to survive being asked, because the backend marshalling asks every
    //  curve for its expression before any of them are fitted.
    GivenTheFormula('');
    AssertEquals('', FCurve.GetCurveExpression);
end;

initialization
    //  A unit test: one curve with five points and four parameters. The
    //  expression engine runs in process; nothing is spawned and no file is
    //  read.
    RegisterTest('unit', TUserPointsSetTest);
end.
