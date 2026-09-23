// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a user's own formula declares, and the five refusals.)

A HUNDRED AND THIRTY LINES INSIDE A FOUR-THOUSAND-LINE SERVICE, and not one of
them had ever been executed by a test. `TFitService.CreateParameters` decided what
the symbols in a user's formula mean and which formulas are refused; it touched
nothing the service owns - an expression parser, a parameter container and five
strings - and the only thing that made it unreachable was where it was written.

THE FIVE REFUSALS ARE THE REASON THIS MATTERS. Every one of them is read by a
person mid-way through writing a curve, looking at their own text, with no other
information. Each has to say what is wrong AND what to type instead. Nothing
asserted a word of them, and two of the five are near-duplicates that must not
be swapped: "the formula must CONTAIN x" for a formula with no symbols at all,
and "the formula must USE x as its argument" for one with symbols, none of them
the axis. Shown the wrong way round, the first tells someone who wrote `A*b` to
add a variable they can see they have.

THE NAMING CONVENTION IS AN UNDOCUMENTED INTERFACE the user programs against, so
the near-misses are pinned as hard as the hits: `x0` is the position and `x1` is
not, `sigma` is a width and `sigma2` is not. Getting one of those wrong does not
fail - the curve fits, and wanders off its peak or divides by zero.

AND THE TWO HALVES OF THE WIDTH RULE ARE TESTED TOGETHER, because separately they
both look arbitrary. `sigma` starts at 0.25 rather than 0, and a formula is
refused if it cannot be evaluated at its starting values. Either alone is a
detail; together they are why `A*exp(-sqr((x-x0)/sigma))` is accepted and
`A*exp(-sqr((x-x0)/w))` is not - and the difference is the parameter's NAME,
which is the whole convention in one pair of tests.
}
unit testcase_user_formula_parameters;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    MyExceptions, persistent_curve_parameters, special_curve_parameter,
    user_formula_parameters;

type
    TUserFormulaParametersTest = class(TTestCase)
    private
        FParams: Curve_parameters;
        { Discovers AFormula into the fixture's container. }
        procedure Discover(const AFormula: string);
        { The refusal AFormula produces, or '' when it is accepted. }
        function RefusalFor(const AFormula: string): string;
        { The parameter of that name, or nil. }
        function ParamNamed(const AName: string): TSpecialCurveParameter;
        { Its declared kind; fails the test when there is no such parameter. }
        function KindOf(const AName: string): TParameterType;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The naming convention: what each name means.
        procedure XIsTheArgument;
        procedure X0IsThePosition;
        procedure SigmaIsAWidthAndStartsAwayFromZero;
        procedure EveryOtherNameIsAnOrdinaryFittedValue;
        procedure TheNamesAreRecognisedWhateverTheCase;
        procedure ANameThatMerelyStartsWithOneOfThemIsNotIt;

        //  Discovering a formula's parameters.
        procedure EverySymbolBecomesAParameter;
        procedure AndIsClassifiedByItsName;
        procedure TheContainerIsFilledInPlaceNotReplaced;
        procedure DiscoveringTwiceLeavesOnlyTheSecondFormulaParameters;

        //  The five refusals, in the words the user reads.
        procedure AnEmptyFormulaIsRefused;
        procedure AFormulaTheParserCannotReadIsRefused;
        procedure AndTheRefusalQuotesWhatWasTyped;
        procedure AFormulaWithNoSymbolsIsRefused;
        procedure AFormulaWithSymbolsButNoAxisIsRefusedDifferently;
        procedure ThoseTwoRefusalsAreNotInterchangeable;
        procedure AFormulaThatCannotBeEvaluatedAtItsStartIsRefused;
        procedure EveryRefusalIsAUserErrorRatherThanAFault;
        procedure NoTwoRefusalsReadAlike;

        //  A lone symbol.
        procedure ALoneSymbolIsTheAxisWhateverItIsCalled;
        procedure AndIsNotRefusedForNotBeingNamedX;

        //  The width default and the probe, which only make sense together.
        procedure AGaussianWrittenWithSigmaIsAccepted;
        procedure TheSameGaussianWrittenWithWIsNot;

        //  The starting-values list the probe is given.
        procedure TheArgumentContributesARepresentativeSample;
        procedure EveryOtherParameterContributesItsStartingValue;
        procedure TheListIsZeroSeparatedAndEndsWithAnEmptyEntry;
        procedure TheDecimalSeparatorIsAPointWhateverTheLocaleSays;
    end;

implementation

const
    { A formula that passes every rule, for the tests that are about something
      else. }
    GOOD = 'A*exp(-sqr((x-x0)/sigma))';

procedure TUserFormulaParametersTest.SetUp;
begin
    FParams := Curve_parameters.Create(nil);
end;

procedure TUserFormulaParametersTest.TearDown;
begin
    FreeAndNil(FParams);
end;

procedure TUserFormulaParametersTest.Discover(const AFormula: string);
begin
    DiscoverFormulaParameters(AFormula, FParams);
end;

function TUserFormulaParametersTest.RefusalFor(const AFormula: string): string;
begin
    Result := '';
    try
        DiscoverFormulaParameters(AFormula, FParams);
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

function TUserFormulaParametersTest.ParamNamed(
    const AName: string): TSpecialCurveParameter;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to FParams.Count - 1 do
        if SameText(FParams[i].Name, AName) then
            Exit(FParams[i]);
end;

function TUserFormulaParametersTest.KindOf(
    const AName: string): TParameterType;
var
    P: TSpecialCurveParameter;
begin
    Result := Variable;
    P := ParamNamed(AName);
    AssertTrue('the formula declared a parameter named ' + AName,
        Assigned(P));
    Result := P.Type_;
end;

{ ---------------------------- the naming convention ------------------------- }

procedure TUserFormulaParametersTest.XIsTheArgument;
begin
    //  The axis. Not fitted, and given a representative sample when the formula
    //  is probed - so mistaking it for a fitted value would have the optimiser
    //  varying the abscissa.
    AssertTrue('x', FormulaParameterRole('x').Kind = Argument);
end;

procedure TUserFormulaParametersTest.X0IsThePosition;
begin
    //  InvariablePosition rather than VariablePosition, which is what the
    //  built-in curves use for a position the user placed - so a user-defined
    //  curve behaves like one.
    AssertTrue('x0', FormulaParameterRole('x0').Kind = InvariablePosition);
end;

procedure TUserFormulaParametersTest.SigmaIsAWidthAndStartsAwayFromZero;
var
    R: TFormulaParameterRole;
begin
    //  BOTH HALVES. It is an ordinary fitted value in kind; what the name buys
    //  is the starting value, and that is the half that matters - see the pair
    //  of tests at the bottom of this file for what a zero start costs.
    R := FormulaParameterRole('sigma');
    AssertTrue('a fitted value', R.Kind = Variable);
    AssertTrue('with a starting value of its own', R.HasStartingValue);
    AssertTrue('and it is not zero', R.StartingValue <> 0);
    AssertEquals('the same one the built-in curves use',
        SIGMA_STARTING_VALUE, R.StartingValue, 1e-12);
end;

procedure TUserFormulaParametersTest.EveryOtherNameIsAnOrdinaryFittedValue;
var
    Names: array[0..4] of string = ('A', 'w', 'tau', 'gamma', 'q');
    i: longint;
    R: TFormulaParameterRole;
begin
    //  NO SPECIAL CASES BEYOND THE THREE. An amplitude is NOT recognised by
    //  name here even though the built-in curves have one - the roles dialog
    //  assigns that - and a test naming only the three hits would not say so.
    for i := 0 to High(Names) do
    begin
        R := FormulaParameterRole(Names[i]);
        AssertTrue(Names[i] + ' is fitted', R.Kind = Variable);
        AssertFalse(Names[i] + ' gets no starting value of its own',
            R.HasStartingValue);
    end;
end;

procedure TUserFormulaParametersTest.TheNamesAreRecognisedWhateverTheCase;
begin
    //  A USER TYPING X0 MEANS THE POSITION. Refused, the position would become
    //  an ordinary fitted value: the curve would still fit, and would wander
    //  off the peak the user placed it on, with nothing reported.
    AssertTrue('X', FormulaParameterRole('X').Kind = Argument);
    AssertTrue('X0', FormulaParameterRole('X0').Kind = InvariablePosition);
    AssertTrue('Sigma', FormulaParameterRole('Sigma').HasStartingValue);
    AssertTrue('SIGMA', FormulaParameterRole('SIGMA').HasStartingValue);
end;

procedure TUserFormulaParametersTest.ANameThatMerelyStartsWithOneOfThemIsNotIt;
begin
    //  THE NEAR-MISSES, pinned as hard as the hits. A prefix match would make
    //  every symbol beginning with x the argument, and a formula in x and x1
    //  would have two axes and no width.
    AssertTrue('x1 is fitted', FormulaParameterRole('x1').Kind = Variable);
    AssertTrue('xx is fitted', FormulaParameterRole('xx').Kind = Variable);
    AssertFalse('sigma2 gets no starting value',
        FormulaParameterRole('sigma2').HasStartingValue);
    AssertFalse('sig gets none either',
        FormulaParameterRole('sig').HasStartingValue);
end;

{ ------------------------ discovering a formula's symbols ------------------- }

procedure TUserFormulaParametersTest.EverySymbolBecomesAParameter;
begin
    //  COUNTED, so a walk that stopped at the first zero of the parser's list -
    //  which is how that list separates entries - would be caught. It would
    //  yield one parameter and a formula that cannot be fitted.
    Discover(GOOD);
    AssertEquals('A, x, x0 and sigma', 4, FParams.Count);
end;

procedure TUserFormulaParametersTest.AndIsClassifiedByItsName;
begin
    Discover(GOOD);
    AssertTrue('x is the axis', KindOf('x') = Argument);
    AssertTrue('x0 is the position', KindOf('x0') = InvariablePosition);
    AssertTrue('sigma is fitted', KindOf('sigma') = Variable);
    AssertTrue('A is fitted', KindOf('A') = Variable);
    AssertEquals('and sigma carries its starting value',
        SIGMA_STARTING_VALUE, ParamNamed('sigma').Value, 1e-12);
end;

procedure TUserFormulaParametersTest.TheContainerIsFilledInPlaceNotReplaced;
var
    Before: Curve_parameters;
begin
    //  THE CALLER'S CONTAINER IS REFERRED TO ELSEWHERE - the service hands
    //  copies of it to every task - so answering a fresh one would leave those
    //  references on the previous formula's parameters, and the fit would run
    //  the curve the user replaced.
    Before := FParams;
    Discover(GOOD);
    AssertTrue('the same container', FParams = Before);
    AssertTrue('and it has the parameters', FParams.Count > 0);
end;

procedure TUserFormulaParametersTest.DiscoveringTwiceLeavesOnlyTheSecondFormulaParameters;
begin
    //  CLEARED FIRST. Accumulated, a user correcting a formula would fit a
    //  model made of both attempts - and the count is the only place that
    //  shows.
    Discover(GOOD);
    Discover('A*x');
    AssertEquals('A and x, and nothing left over', 2, FParams.Count);
    AssertFalse('sigma is gone', Assigned(ParamNamed('sigma')));
end;

{ ------------------------------- the refusals ------------------------------- }

procedure TUserFormulaParametersTest.AnEmptyFormulaIsRefused;
begin
    AssertEquals('the empty-formula refusal', FORMULA_IS_EMPTY,
        RefusalFor(''));
end;

procedure TUserFormulaParametersTest.AFormulaTheParserCannotReadIsRefused;
begin
    AssertTrue('refused', Pos(FORMULA_NOT_UNDERSTOOD_PREFIX,
        RefusalFor('A*exp(-sqr(')) = 1);
end;

procedure TUserFormulaParametersTest.AndTheRefusalQuotesWhatWasTyped;
var
    R: string;
begin
    //  QUOTED BACK, because the user may be looking at a dialog that has
    //  already closed, or at a formula they pasted - and "could not be
    //  understood" alone does not say which of several they tried.
    R := RefusalFor('A*exp(-sqr(');
    AssertTrue('the formula is in the message: ' + R,
        Pos('A*exp(-sqr(', R) > 0);
end;

procedure TUserFormulaParametersTest.AFormulaWithNoSymbolsIsRefused;
begin
    //  A CONSTANT IS NOT A CURVE. It parses perfectly, evaluates to a number,
    //  and declares nothing to fit.
    AssertEquals('the no-symbols refusal', FORMULA_HAS_NO_SYMBOLS,
        RefusalFor('2+2'));
end;

procedure TUserFormulaParametersTest.AFormulaWithSymbolsButNoAxisIsRefusedDifferently;
begin
    //  Symbols, none of them the axis: constant in x, so it would fit a
    //  horizontal line through the data and report an R-factor for it.
    AssertEquals('the no-argument refusal', FORMULA_HAS_NO_ARGUMENT,
        RefusalFor('A*b + c'));
end;

procedure TUserFormulaParametersTest.ThoseTwoRefusalsAreNotInterchangeable;
begin
    //  THE PAIR MOST EASILY SWAPPED, and swapping them is worse than either
    //  being vague: shown "the formula must CONTAIN the variable x" for
    //  `A*b + c`, the user is being told to add a variable they can see they
    //  already have. Asserted as a difference so that one cannot drift into
    //  the other.
    AssertTrue('they differ', FORMULA_HAS_NO_SYMBOLS <> FORMULA_HAS_NO_ARGUMENT);
    AssertTrue('a constant gets the first',
        RefusalFor('2+2') = FORMULA_HAS_NO_SYMBOLS);
    AssertTrue('a formula with symbols gets the second',
        RefusalFor('A*b + c') = FORMULA_HAS_NO_ARGUMENT);
end;

procedure TUserFormulaParametersTest.AFormulaThatCannotBeEvaluatedAtItsStartIsRefused;
begin
    //  A width named anything but sigma starts at zero, so this divides by zero
    //  at its own starting point. Fitted from there the shape is degenerate and
    //  the fit converges on nothing while reporting a number.
    AssertEquals('the not-finite refusal', FORMULA_NOT_FINITE_AT_START,
        RefusalFor('x/w'));
end;

procedure TUserFormulaParametersTest.EveryRefusalIsAUserErrorRatherThanAFault;
var
    Formulas: array[0..4] of string =
        ('', 'A*exp(-sqr(', '2+2', 'A*b + c', 'x/w');
    i: longint;
    Kind: string;
begin
    //  THE CLASS IS THE DIFFERENCE BETWEEN 400 AND 500. The REST layer maps
    //  EUserException to a client error and everything else to "the server
    //  broke", so a refusal raised as a plain Exception tells a user their
    //  formula is fine and the program is not.
    for i := 0 to High(Formulas) do
    begin
        Kind := '';
        try
            DiscoverFormulaParameters(Formulas[i], FParams);
        except
            on E: EUserException do
                Kind := 'user';
            on E: Exception do
                Kind := E.ClassName;
        end;
        AssertEquals('"' + Formulas[i] + '" is refused as a user error',
            'user', Kind);
    end;
end;

procedure TUserFormulaParametersTest.NoTwoRefusalsReadAlike;
var
    Seen: TStringList;
    Formulas: array[0..4] of string =
        ('', 'A*exp(-sqr(', '2+2', 'A*b + c', 'x/w');
    i: longint;
    R: string;
begin
    //  FIVE DISTINCT ANSWERS. Two refusals reading alike is worse than one of
    //  them missing: the user acts on the advice, it does not help, and nothing
    //  says the program meant something else.
    Seen := TStringList.Create;
    try
        for i := 0 to High(Formulas) do
        begin
            R := RefusalFor(Formulas[i]);
            AssertTrue('"' + Formulas[i] + '" is refused', R <> '');
            AssertTrue('"' + Formulas[i] + '" reads like an earlier refusal',
                Seen.IndexOf(R) < 0);
            Seen.Add(R);
        end;
        AssertEquals('five refusals, five messages', 5, Seen.Count);
    finally
        Seen.Free;
    end;
end;

{ -------------------------------- a lone symbol ----------------------------- }

procedure TUserFormulaParametersTest.ALoneSymbolIsTheAxisWhateverItIsCalled;
begin
    //  `f(t) = t` is a formula whose meaning is not in doubt. Refusing it for
    //  not saying `x` would be pedantry, and the user has no way to know the
    //  convention before they hit it.
    Discover('t');
    AssertEquals('one parameter', 1, FParams.Count);
    AssertTrue('and it is the axis', KindOf('t') = Argument);
end;

procedure TUserFormulaParametersTest.AndIsNotRefusedForNotBeingNamedX;
begin
    AssertEquals('accepted', '', RefusalFor('t'));
end;

{ ----------------- the width default and the probe, together ---------------- }

procedure TUserFormulaParametersTest.AGaussianWrittenWithSigmaIsAccepted;
begin
    //  THE INTERLOCK, first half. sigma starts at 0.25, so the formula
    //  evaluates finitely at its own starting values and passes the probe.
    AssertEquals('accepted', '', RefusalFor('A*exp(-sqr((x-x0)/sigma))'));
end;

procedure TUserFormulaParametersTest.TheSameGaussianWrittenWithWIsNot;
begin
    //  SECOND HALF, AND THE SAME FORMULA. The only difference is the width's
    //  NAME: w starts at zero, so the same shape divides by zero at its
    //  starting point and is refused.
    //
    //  Together these two say what neither says alone - that the starting-value
    //  default and the probe are one rule, and that the naming convention is
    //  what the user has to know. Remove the default and this pair fails on the
    //  first; remove the probe and it fails on the second.
    AssertEquals('refused', FORMULA_NOT_FINITE_AT_START,
        RefusalFor('A*exp(-sqr((x-x0)/w))'));
end;

{ --------------------------- the starting-values list ----------------------- }

procedure TUserFormulaParametersTest.TheArgumentContributesARepresentativeSample;
begin
    //  THE AXIS HAS NO VALUE OF ITS OWN, so the probe needs a stand-in. Left at
    //  zero it would make every formula with x in a denominator unevaluable,
    //  and they would all be refused for a zero the user never chose.
    Discover(GOOD);
    AssertTrue('x contributes 1: ' + StartingValuesList(FParams),
        Pos('x=1' + #0, StartingValuesList(FParams)) > 0);
end;

procedure TUserFormulaParametersTest.EveryOtherParameterContributesItsStartingValue;
begin
    Discover(GOOD);
    AssertTrue('sigma contributes its default: ' + StartingValuesList(FParams),
        Pos('sigma=0.25' + #0, StartingValuesList(FParams)) > 0);
end;

procedure TUserFormulaParametersTest.TheListIsZeroSeparatedAndEndsWithAnEmptyEntry;
var
    L: string;
begin
    //  ONE ZERO ENDS AN ENTRY, TWO END THE LIST. Without the second the
    //  evaluator walks past the end of the string, and what it reads there
    //  decides whether the user's formula is accepted.
    Discover(GOOD);
    L := StartingValuesList(FParams);
    AssertTrue('it ends with two zeros',
        Copy(L, Length(L) - 1, 2) = #0#0);
    AssertEquals('one entry per parameter, plus the terminator',
        FParams.Count + 1, Length(L) - Length(StringReplace(L, #0, '',
        [rfReplaceAll])));
end;

procedure TUserFormulaParametersTest.TheDecimalSeparatorIsAPointWhateverTheLocaleSays;
var
    Saved: char;
    L: string;
begin
    //  THE EVALUATOR READS THESE NUMBERS BACK and does not know the user's
    //  locale. Left to the locale, every formula would be unevaluable for every
    //  user whose separator is a comma - and the refusal shown would be the one
    //  about starting values, sending them to change numbers that were never
    //  the problem.
    //
    //  The locale is forced and restored. That is a process-wide global, which
    //  is why it is done in exactly one test, in a try/finally, and why the
    //  assertion is on the string rather than on the evaluator's answer.
    Discover(GOOD);
    Saved := DefaultFormatSettings.DecimalSeparator;
    try
        DefaultFormatSettings.DecimalSeparator := ',';
        L := StartingValuesList(FParams);
    finally
        DefaultFormatSettings.DecimalSeparator := Saved;
    end;
    AssertTrue('still a point: ' + L, Pos('0.25', L) > 0);
    AssertTrue('and no comma anywhere', Pos(',', L) = 0);
end;

initialization
    //  A unit test: an expression parser, a parameter container and five
    //  strings. No service, no profile, no fit - which is what a hundred and
    //  thirty lines on a four-thousand-line class could not say.
    RegisterTest('unit', TUserFormulaParametersTest);
end.
