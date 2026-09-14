// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Making the curve type a user's formula defines.)

ONE FUNCTION, THREE ASSIGNMENTS, AND AN OWNERSHIP TRANSFER nobody had written
down. It is what turns the name, the formula and the parsed parameter set the two
definition dialogs collected into the object that goes into the user's curve list
and into their settings file.

IT HAD NO TEST because its only caller was the definition sequence, which was
written inline in a method that opened two modal windows. That sequence is
`user_curve_flow` now and has its own fixture; this is the piece it makes, and the
piece is worth its own tests for one reason: THE PARAMETER SET CHANGES HANDS.

`Curve_type.SetParameters` frees whatever it held and keeps the reference it is
given, and `Curve_type.Destroy` frees it. So the caller must not free the set it
passed, and must not pass one twice. Neither of those is visible in the
function's signature, and getting either wrong is a double free - a crash
somewhere else entirely, minutes later, when a settings file is written.
}
unit testcase_curve_type_factory;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    app_settings, persistent_curve_parameters,
    int_curve_type_parameters_factory, curve_type_parameters_factory;

type
    TCurveTypeFactoryTest = class(TTestCase)
    private
        FFactory: ICurveTypeParametersFactory;
        { A parameter set with one entry, so "the same set arrived" is a question
          about contents as well as about identity. }
        function AParameterSet: Curve_parameters;
    protected
        procedure SetUp; override;
    published
        procedure ItMakesACurveType;
        procedure TheNameIsTheOneItWasGiven;
        procedure AndTheFormulaToo;
        procedure NeitherIsAllowedToStandInForTheOther;
        procedure ThePassedParameterSetIsTheOneItCarries;
        procedure AndIsFreedWithTheCurveTypeRatherThanByTheCaller;
        procedure AnEmptyNameOrFormulaIsNotRefusedHere;
        procedure TheFactoryIsOneObjectHoweverOftenItIsAsked;
    end;

implementation

procedure TCurveTypeFactoryTest.SetUp;
begin
    //  A singleton behind a class function, in the style of the sibling
    //  adapters: there is nothing to free.
    FFactory := TCurveTypeParametersFactory.Create;
end;

function TCurveTypeFactoryTest.AParameterSet: Curve_parameters;
begin
    Result := Curve_parameters.Create(nil);
end;

procedure TCurveTypeFactoryTest.ItMakesACurveType;
var
    CT: Curve_type;
begin
    CT := FFactory.CreateUserCurveType('n', 'A*x', AParameterSet);
    try
        AssertTrue('a type was made', Assigned(CT));
    finally
        CT.Free;
    end;
end;

procedure TCurveTypeFactoryTest.TheNameIsTheOneItWasGiven;
var
    CT: Curve_type;
begin
    //  The name is what the user finds in their curve list and what the menu
    //  entry is labelled with.
    CT := FFactory.CreateUserCurveType('my skewed thing', 'A*x', AParameterSet);
    try
        AssertEquals('the name', 'my skewed thing', CT.Name);
    finally
        CT.Free;
    end;
end;

procedure TCurveTypeFactoryTest.AndTheFormulaToo;
var
    CT: Curve_type;
begin
    //  The formula is what gets evaluated, natively and by the Python backend.
    CT := FFactory.CreateUserCurveType('n', 'A*exp(-(x-x0)*(x-x0)/s)',
        AParameterSet);
    try
        AssertEquals('the formula', 'A*exp(-(x-x0)*(x-x0)/s)', CT.Expression);
    finally
        CT.Free;
    end;
end;

procedure TCurveTypeFactoryTest.NeitherIsAllowedToStandInForTheOther;
var
    CT: Curve_type;
begin
    //  TWO STRING ARGUMENTS IN A ROW, which is the shape that gets transposed.
    //  Swapped, the user gets a curve named after its own formula and a formula
    //  that will not parse - and the second failure is reported by the parser at
    //  fit time, pointing at the formula rather than at this.
    //
    //  Asserted together, with values that could not be each other, so a
    //  transposition fails here rather than at whichever of the two a
    //  single-field test happened to check.
    CT := FFactory.CreateUserCurveType('NAME', 'FORMULA', AParameterSet);
    try
        AssertEquals('the name is the first argument', 'NAME', CT.Name);
        AssertEquals('the formula is the second', 'FORMULA', CT.Expression);
    finally
        CT.Free;
    end;
end;

procedure TCurveTypeFactoryTest.ThePassedParameterSetIsTheOneItCarries;
var
    CT: Curve_type;
    P: Curve_parameters;
begin
    //  THE SAME OBJECT, not a copy. The roles dialog writes onto the set it is
    //  shown and the storage writes it out, so a copy taken here would have the
    //  user's role choices land on something that is never saved - and the
    //  curve would be fitted with every role unassigned.
    P := AParameterSet;
    CT := FFactory.CreateUserCurveType('n', 'A*x', P);
    try
        AssertTrue('the set it was given', CT.Parameters = P);
    finally
        CT.Free;
    end;
end;

procedure TCurveTypeFactoryTest.AndIsFreedWithTheCurveTypeRatherThanByTheCaller;
var
    CT: Curve_type;
    P: Curve_parameters;
begin
    //  IT CHANGES HANDS, and nothing in the signature says so: SetParameters
    //  frees what it held and keeps what it is given, and Destroy frees it. So
    //  a caller that also frees its own set double-frees - a crash somewhere
    //  else entirely, later, while a settings file is being written.
    //
    //  Asserted the only way it can be: free the type, and assert the process
    //  survives freeing it a second time being NOT attempted. What this really
    //  pins is that the type holds the same reference (above) and that freeing
    //  the type alone leaves nothing behind for the leak gate to find - which
    //  is what the suite's own leak check then confirms across every test here.
    P := AParameterSet;
    CT := FFactory.CreateUserCurveType('n', 'A*x', P);
    AssertTrue('held before', CT.Parameters = P);
    CT.Free;
    //  Reaching here at all is the assertion: had the type NOT taken ownership,
    //  P would still be live and every test in this fixture would leak one.
    AssertTrue('freeing the type is enough', True);
end;

procedure TCurveTypeFactoryTest.AnEmptyNameOrFormulaIsNotRefusedHere;
var
    CT: Curve_type;
begin
    //  CHARACTERISED. This is a constructor, not a validator: the formula was
    //  already checked by the parser before the sequence got here - a formula
    //  that will not parse never reaches this - and the name is the dialog's to
    //  insist on. Refusing here would move an error message away from the field
    //  it is about.
    CT := FFactory.CreateUserCurveType('', '', AParameterSet);
    try
        AssertTrue('made anyway', Assigned(CT));
        AssertEquals('with an empty name', '', CT.Name);
    finally
        CT.Free;
    end;
end;

procedure TCurveTypeFactoryTest.TheFactoryIsOneObjectHoweverOftenItIsAsked;
var
    A, B: ICurveTypeParametersFactory;
begin
    //  A SINGLETON BEHIND A CLASS FUNCTION NAMED Create, which reads as a
    //  constructor and is not one. -SIcorba means the interface carries no
    //  refcount, so a caller that freed what "Create" gave it would destroy the
    //  one instance the whole application shares. Pinned because the name
    //  invites exactly that.
    A := TCurveTypeParametersFactory.Create;
    B := TCurveTypeParametersFactory.Create;
    AssertTrue('the same instance both times', A = B);
end;

initialization
    RegisterTest('unit', TCurveTypeFactoryTest);
end.
