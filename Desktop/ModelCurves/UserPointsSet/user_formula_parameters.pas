// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a user's own formula declares, and what is refused.)

WHEN A USER TYPES A FORMULA, this decides what its symbols mean: which one is the
argument running along the axis, which is the peak position, which is a width
that must not start at zero, and which are ordinary fitted values. It also
decides which formulas are refused, and in what words.

FIVE REFUSALS, ALL OF THEM READ BY THE USER while they are looking at their own
text. That is the reason this is worth a unit of its own. They are not internal
diagnostics: each one has to say what is wrong AND what to type instead, because
the person reading it is mid-way through writing a curve and has no other
information. An unhelpful one costs them the feature.

IT WAS A METHOD ON THE COMPUTE SERVICE - a hundred and thirty lines inside four
thousand - and not one of its lines had ever been executed by a test. It touches
nothing the service owns: an expression parser, a parameter container, and five
strings. The only thing that made it unreachable was where it was written.

THE NAMING CONVENTION IS THE INTERFACE THE USER PROGRAMS AGAINST, and it is
undocumented anywhere they can see. Naming a symbol `x0` makes it a position;
naming one `sigma` gives it a non-zero start so a formula dividing by it does not
blow up on the first evaluation. Those are conventions borrowed from the built-in
curves so that a user-defined curve behaves like one, and they are matched
case-insensitively because a user typing `X0` means the same thing.

WHY THE STARTING VALUES ARE PROBED. A curve that cannot be evaluated at its own
starting point would otherwise be fitted from a degenerate shape - a flat line, an
infinity - and the fit would converge on nothing while reporting a number. Better
to refuse it while the user still has the dialog open. That probe is also why
`StartingValuesList` is exported: what it builds goes into the evaluator and comes
back as one boolean, so without a way to look at the string there is no way to
check the decimal separator, and a comma there makes every formula unevaluable
for every user whose locale uses one.
}
unit user_formula_parameters;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, persistent_curve_parameters, special_curve_parameter;

type
    { What a symbol's name says about how it is used. }
    TFormulaParameterRole = record
        Kind: TParameterType;
        { The value the parameter starts at, when its name calls for a particular
          one. Otherwise the parameter class's own initial value stands. }
        StartingValue: double;
        HasStartingValue: boolean;
    end;

const
    { A WIDTH MAY NOT START AT ZERO. It is a denominator in every shape that has
      one, so a formula with a zero width divides by zero at its own starting
      point and is refused by the probe below - which is correct but useless,
      since the user did not choose the zero. The same non-zero start the
      built-in curves use is given instead. }
    SIGMA_STARTING_VALUE = 0.25;

{ How a symbol of this name is used.

  Case-insensitive: a user typing `X0` means the position, and refusing to
  recognise it would silently make the position an ordinary fitted value - the
  curve would still fit, and would wander off its peak. }
function FormulaParameterRole(const AName: string): TFormulaParameterRole;

{ The zero-separated `name=value` list the evaluator is probed with.

  THE ARGUMENT GETS 1, not its own value: it has none - it is the axis - and a
  representative sample is what the probe needs. Everything else contributes its
  starting value.

  THE DECIMAL SEPARATOR IS FORCED TO '.', because the evaluator reads the numbers
  back and does not know the user's locale. Left to the locale, every formula
  would be unevaluable for every user whose separator is a comma, and the refusal
  they would see is the one about starting values - which would send them to
  change numbers that were never the problem. }
function StartingValuesList(AParams: Curve_parameters): string;

{ Fills AParams with one parameter per symbol AFormula uses, classified by name.

  IN PLACE, replacing whatever the container held: the caller's container is
  referred to elsewhere, so a fresh one would leave those references pointing at
  the previous formula's parameters.

  Raises EUserException, and only EUserException, for anything the user can put
  right - which is all five refusals. The class matters: the REST layer maps it
  to 400 and anything else to 500, so it is the difference between "your formula
  is wrong" and "the server broke". }
procedure DiscoverFormulaParameters(const AFormula: string;
    AParams: Curve_parameters);

{ The five refusals, exported so a caller can recognise one and a test can pin
  the words rather than a paraphrase of them. Each names what is wrong and what
  to type instead; see the note at the top of this unit for why that is not
  optional here. }
const
    FORMULA_IS_EMPTY =
        'Enter a formula for the curve, e.g. A*exp(-sqr((x-x0)/w)).';
    FORMULA_NOT_UNDERSTOOD_PREFIX = 'The formula could not be understood: "';
    FORMULA_NOT_UNDERSTOOD_SUFFIX =
        '". Use x as the variable and standard functions, ' +
        'e.g. A*exp(-sqr((x-x0)/w)); check for typos and matching brackets.';
    FORMULA_HAS_NO_SYMBOLS =
        'The formula must contain the variable x as its argument, ' +
        'e.g. A*exp(-sqr((x-x0)/w)).';
    FORMULA_HAS_NO_ARGUMENT =
        'The formula must use x as its argument (the variable running along ' +
        'the axis), e.g. A*exp(-((x-x0)/SIGMA)^2).';
    FORMULA_NOT_FINITE_AT_START =
        'The formula cannot be evaluated at its starting values - a parameter ' +
        'probably needs a non-zero starting value (for example a width used ' +
        'as a denominator must not be 0). Set sensible starting values in the ' +
        'parameters dialog and retry.';

implementation

uses
    checks, MyExceptions, native_math_expr, persistent_curve_parameter_container,
    user_curve_parameter;

function FormulaParameterRole(const AName: string): TFormulaParameterRole;
var
    N: string;
begin
    Result.StartingValue := 0;
    Result.HasStartingValue := False;
    N := UpperCase(AName);
    //  MIRRORS THE BUILT-IN CURVES, so a user-defined curve behaves like one:
    //    x     - the argument;
    //    x0    - the peak position, not varied by the optimiser directly;
    //    sigma - a width, which needs a non-zero start;
    //  and anything else is an ordinary fitted value.
    if N = 'X' then
        Result.Kind := Argument
    else if N = 'X0' then
        Result.Kind := InvariablePosition
    else
    begin
        Result.Kind := Variable;
        if N = 'SIGMA' then
        begin
            Result.StartingValue := SIGMA_STARTING_VALUE;
            Result.HasStartingValue := True;
        end;
    end;
end;

function StartingValuesList(AParams: Curve_parameters): string;
var
    i: longint;
    Fmt: TFormatSettings;
    P: TSpecialCurveParameter;
begin
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    Result := '';
    for i := 0 to AParams.Count - 1 do
    begin
        P := AParams[i];
        if P.Type_ = Argument then
            Result := Result + P.Name + '=1' + #0
        else
            Result := Result + P.Name + '=' + FloatToStr(P.Value, Fmt) + #0;
    end;
    //  A SECOND TERMINATOR ends the list, since one zero only ends an entry.
    Result := Result + #0;
end;

{ True when some parameter is the axis. }
function HasArgument(AParams: Curve_parameters): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to AParams.Count - 1 do
        if AParams[i].Type_ = Argument then
            Exit(True);
end;

{ One parameter per symbol, classified. }
procedure AddSymbols(AParams: Curve_parameters);
var
    Symbols, Saved: PChar;
    Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
    Role: TFormulaParameterRole;
begin
    AParams.Params.Clear;
    //  The parser's symbol list: names separated by zeros, ended by an empty
    //  one. Owned by the parser, so it is handed back whatever happens below.
    Symbols := GetSymbols;
    Saved := Symbols;
    try
        while Assigned(Symbols) and (StrLen(Symbols) <> 0) do
        begin
            Parameter := TUserCurveParameter.Create;
            try
                Parameter.Name := Symbols;
                Role := FormulaParameterRole(Parameter.Name);
                Parameter.Type_ := Role.Kind;
                if Role.HasStartingValue then
                    Parameter.Value := Role.StartingValue;

                Symbols := Symbols + StrLen(Symbols) + 1;

                Container :=
                    TPersistentCurveParameterContainer(AParams.Params.Add);
                try
                    //  The container takes ownership here, which is why the
                    //  outer handler must not free the parameter after this
                    //  point - and why a failure INSIDE the assignment has to
                    //  remove the container it was made for.
                    Container.Parameter := Parameter;
                except
                    AParams.Params.Delete(Container.ID);
                    Container.Free;
                    raise;
                end;
            except
                Parameter.Free;
                raise;
            end;
        end;
    finally
        FreeSymbols(Saved);
    end;
end;

procedure DiscoverFormulaParameters(const AFormula: string;
    AParams: Curve_parameters);
var
    Parsed: longint;
    ExprResult: double;
begin
    CheckAssigned(AParams, 'the parameter container');
    CheckAssigned(AParams.Params, 'the parameter collection');

    if Length(AFormula) = 0 then
        raise EUserException.Create(FORMULA_IS_EMPTY);

    //  1 means fully evaluated, -1 that it parsed but has symbols whose values
    //  are not known yet - which is the ordinary case for a curve. Anything
    //  else is text the parser could not read.
    Parsed := ParseAndCalcExpression(PChar(AFormula), '', @ExprResult);
    if (Parsed <> 1) and (Parsed <> -1) then
        raise EUserException.Create(FORMULA_NOT_UNDERSTOOD_PREFIX + AFormula +
            FORMULA_NOT_UNDERSTOOD_SUFFIX);

    AddSymbols(AParams);

    //  TWO REFUSALS THAT LOOK ALIKE AND ARE NOT. The first is a formula with no
    //  symbols at all - a constant, which is not a curve. The second is a
    //  formula with symbols, none of them the axis, which would also be
    //  constant in x but reads to the user as a real formula that was rejected.
    //  They are worded differently on purpose.
    if AParams.Count = 0 then
        raise EUserException.Create(FORMULA_HAS_NO_SYMBOLS);

    //  A LONE SYMBOL CAN ONLY BE THE AXIS, whatever it is called. Refusing
    //  `f(t) = t` for not saying `x` would be pedantry about a formula whose
    //  meaning is not in doubt.
    if AParams.Count = 1 then
        AParams[0].Type_ := Argument;

    if not HasArgument(AParams) then
        raise EUserException.Create(FORMULA_HAS_NO_ARGUMENT);

    //  LAST, because it needs every starting value to be in place - including
    //  the width's non-zero one, without which a great many ordinary formulas
    //  would be refused here for a zero the user never chose.
    if not EvaluatesFiniteAt(PChar(AFormula), PChar(StartingValuesList(AParams)))
    then
        raise EUserException.Create(FORMULA_NOT_FINITE_AT_START);
end;

end.
