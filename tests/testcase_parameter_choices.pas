// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The four lists the user-curve properties dialog offers, as facts about
the parameters.)

FOUR COMBO BOXES AND A CHECK-LIST, each showing a different subset of a curve
type's parameters: which may be the abscissa, which may place the curve, which
may be its height or its width, and which may be held fixed. Each subset is a
RULE about what a parameter is allowed to stand for, and each of them lived
inside an LCL `Fill*` method - so the only way to find out what a combo offered
was to open it.

WHAT AN OFFER THAT IS TOO WIDE COSTS. The user gives one name two meanings: the
parameter that places the curve is also its height, and the fit then moves the
curve every time it changes how tall it is. Nothing refuses it and nothing says
so; what comes back is a fit that will not settle.

WHAT AN OFFER THAT IS TOO NARROW COSTS. The parameter the user needs is simply
not in the list. An absent combo item explains nothing, so they conclude the
formula is wrong.

AND WHAT IS MARKED MATTERS AS MUCH AS WHAT IS OFFERED. A combo showing a
selection nobody made says a role is decided when it is not, and the dialog is
closed on a curve type that has no abscissa.
}
unit testcase_parameter_choices;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    parameter_roles, special_curve_parameter, persistent_curve_parameters,
    persistent_curve_parameter_container,
    //  A concrete parameter class; which one does not matter, because what is
    //  under test is the role each carries.
    amplitude_curve_parameter;

type
    TParameterChoicesTest = class(TTestCase)
    private
        FParams: Curve_parameters;
        function Add(const AName: string;
            AType: TParameterType): TSpecialCurveParameter;
        { The names in a list, joined - so a wrong list fails with a message
          showing what was offered rather than a count. }
        function NamesOf(const AChoices: TParameterChoices): string;
        { The name of the marked row, or '' when none is. }
        function MarkedName(const AChoices: TParameterChoices): string;
        { An abscissa, a fixed position, two free parameters and a shared one. }
        procedure GivenATypicalCurveType;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The abscissa combo.
        procedure TheAbscissaComboOffersEveryParameter;
        procedure ItMarksTheOneThatIsTheAbscissa;
        procedure WithNoAbscissaNothingIsMarked;

        //  The position combo.
        procedure ThePositionComboLeavesOutTheAbscissa;
        procedure ThePositionComboLeavesOutTheAmplitudeAndTheWidth;
        procedure ItMarksEitherKindOfPosition;

        //  A role combo.
        procedure ARoleComboOffersNoneFirst;
        procedure ARoleComboOffersTheFreeParameters;
        procedure ARoleComboLeavesOutTheAbscissaAndThePosition;
        procedure ItMarksTheHolderOfTheRole;
        procedure WithNoHolderItMarksNone;
        procedure NoneIsOfferedEvenWhenTheRoleIsHeld;

        //  The fixed check-list.
        procedure TheCheckListShowsWhatThePositionComboDoes;
        procedure ASharedParameterIsTicked;
        procedure AFixedPositionIsTicked;
        procedure AVariedParameterIsNotTicked;

        //  Rules every list keeps.
        procedure EveryRowCarriesItsOwnParameter;
        procedure OnlyTheNoneRowCarriesNothing;
        procedure AnEmptyParameterSetOffersNothingButNone;
        procedure ANilParameterSetIsNotAFailure;
    end;

implementation

procedure TParameterChoicesTest.SetUp;
begin
    FParams := Curve_parameters.Create(nil);
    //  Curve_parameters seeds a placeholder the filer needs; these tests are
    //  about the parameters a curve type actually declares.
    FParams.Params.Clear;
end;

procedure TParameterChoicesTest.TearDown;
begin
    FreeAndNil(FParams);
end;

function TParameterChoicesTest.Add(const AName: string;
    AType: TParameterType): TSpecialCurveParameter;
var
    Container: TPersistentCurveParameterContainer;
    P: TSpecialCurveParameter;
begin
    P := TAmplitudeCurveParameter.Create;
    P.Name := AName;
    P.Type_ := AType;
    Container := TPersistentCurveParameterContainer(FParams.Params.Add);
    Container.Parameter := P;
    Result := P;
end;

function TParameterChoicesTest.NamesOf(
    const AChoices: TParameterChoices): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AChoices) do
        Result := Result + AChoices[i].Name + ' ';
end;

function TParameterChoicesTest.MarkedName(
    const AChoices: TParameterChoices): string;
begin
    Result := '';
    if MarkedIndex(AChoices) >= 0 then
        Result := AChoices[MarkedIndex(AChoices)].Name;
end;

procedure TParameterChoicesTest.GivenATypicalCurveType;
begin
    Add('x', Argument);
    Add('x0', InvariablePosition);
    Add('A', Variable);
    Add('w', Variable);
    Add('c', Shared);
end;

{ ---- the abscissa combo ---------------------------------------------------- }

procedure TParameterChoicesTest.TheAbscissaComboOffersEveryParameter;
begin
    //  EVERY one, with no filter: the formula decides what it is written in.
    //  A parameter left out is one the user cannot choose, and an absent combo
    //  item explains nothing.
    GivenATypicalCurveType;
    AssertEquals('all five', 'x x0 A w c ', NamesOf(ArgumentChoices(FParams)));
end;

procedure TParameterChoicesTest.ItMarksTheOneThatIsTheAbscissa;
begin
    GivenATypicalCurveType;
    AssertEquals('x', MarkedName(ArgumentChoices(FParams)));
end;

procedure TParameterChoicesTest.WithNoAbscissaNothingIsMarked;
begin
    //  NOTHING, rather than the first row. A combo showing a selection nobody
    //  made is a curve type whose abscissa looks decided when it is not, and
    //  the dialog is closed without one being set.
    Add('A', Variable);
    AssertEquals('nothing marked', -1, MarkedIndex(ArgumentChoices(FParams)));
end;

{ ---- the position combo ---------------------------------------------------- }

procedure TParameterChoicesTest.ThePositionComboLeavesOutTheAbscissa;
begin
    //  The formula's own variable cannot also be where the curve sits.
    GivenATypicalCurveType;
    AssertEquals('x0 A w c ', NamesOf(PositionChoices(FParams)));
end;

procedure TParameterChoicesTest.ThePositionComboLeavesOutTheAmplitudeAndTheWidth;
begin
    //  NARROWER THAN CanHoldRole(prPosition), which says only "not the
    //  abscissa" and would therefore also offer a parameter already holding the
    //  amplitude or the width. This is what the dialog has always shown; the
    //  two rules disagree, and that is recorded in findings.md rather than
    //  quietly resolved by this test.
    Add('x', Argument);
    Add('A', Amplitude);
    Add('w', special_curve_parameter.Width);
    Add('b', Variable);
    AssertEquals('only the free one', 'b ', NamesOf(PositionChoices(FParams)));
end;

procedure TParameterChoicesTest.ItMarksEitherKindOfPosition;
begin
    //  Fixed and varied are one role wearing two hats; a list that marked only
    //  the fixed kind would show no position for a curve that has one.
    Add('a', Variable);
    Add('p', VariablePosition);
    AssertEquals('p', MarkedName(PositionChoices(FParams)));
end;

{ ---- a role combo ---------------------------------------------------------- }

procedure TParameterChoicesTest.ARoleComboOffersNoneFirst;
begin
    //  FIRST, so its index does not move as parameters come and go.
    GivenATypicalCurveType;
    AssertEquals(NoneChoiceCaption, RoleChoices(FParams, prAmplitude)[0].Name);
end;

procedure TParameterChoicesTest.ARoleComboOffersTheFreeParameters;
begin
    GivenATypicalCurveType;
    AssertEquals(NoneChoiceCaption + ' A w ',
        NamesOf(RoleChoices(FParams, prAmplitude)));
end;

procedure TParameterChoicesTest.ARoleComboLeavesOutTheAbscissaAndThePosition;
var
    Offered: string;
begin
    //  A name cannot be two things at once, and offering it would let the user
    //  make it so: a curve whose position is also its height moves whenever the
    //  fit changes how tall it is.
    GivenATypicalCurveType;
    Offered := ' ' + NamesOf(RoleChoices(FParams, prWidth));
    AssertEquals('no abscissa', 0, Pos(' x ', Offered));
    AssertEquals('no position', 0, Pos(' x0 ', Offered));
end;

procedure TParameterChoicesTest.ItMarksTheHolderOfTheRole;
begin
    GivenATypicalCurveType;
    Add('h', Amplitude);
    AssertEquals('h', MarkedName(RoleChoices(FParams, prAmplitude)));
end;

procedure TParameterChoicesTest.WithNoHolderItMarksNone;
begin
    //  "(none)" is the truthful selection when nobody holds the role, and it is
    //  what tells the user they still have to choose one.
    GivenATypicalCurveType;
    AssertEquals(NoneChoiceCaption,
        MarkedName(RoleChoices(FParams, prAmplitude)));
end;

procedure TParameterChoicesTest.NoneIsOfferedEvenWhenTheRoleIsHeld;
begin
    //  HOW THE ROLE IS TAKEN AWAY AGAIN. A list that dropped "(none)" once
    //  somebody held the role would make the first assignment permanent.
    Add('h', Amplitude);
    AssertEquals(NoneChoiceCaption, RoleChoices(FParams, prAmplitude)[0].Name);
end;

{ ---- the fixed check-list -------------------------------------------------- }

procedure TParameterChoicesTest.TheCheckListShowsWhatThePositionComboDoes;
begin
    //  The same filter, deliberately: both are about parameters that describe
    //  the curve rather than its shape. Asserted so the two cannot drift apart
    //  while each still looks right on its own.
    GivenATypicalCurveType;
    AssertEquals(NamesOf(PositionChoices(FParams)),
        NamesOf(FixedChoices(FParams)));
end;

procedure TParameterChoicesTest.ASharedParameterIsTicked;
begin
    //  Shared means one value across the curves of an interval, which is a kind
    //  of being held fixed.
    Add('c', Shared);
    AssertTrue('ticked', FixedChoices(FParams)[0].Marked);
end;

procedure TParameterChoicesTest.AFixedPositionIsTicked;
begin
    Add('x0', InvariablePosition);
    AssertTrue('ticked', FixedChoices(FParams)[0].Marked);
end;

procedure TParameterChoicesTest.AVariedParameterIsNotTicked;
begin
    //  The other half. A box ticked for a parameter the fit does vary tells the
    //  user their model is more constrained than it is.
    Add('A', Variable);
    AssertFalse('not ticked', FixedChoices(FParams)[0].Marked);
end;

{ ---- rules every list keeps ------------------------------------------------ }

procedure TParameterChoicesTest.EveryRowCarriesItsOwnParameter;
var
    Choices: TParameterChoices;
    i: longint;
begin
    //  THE PARAMETER IS ATTACHED TO THE ROW, not matched up by index later: the
    //  combo sorts its items, so the two orders do not correspond, and an index
    //  lookup would give a role to whichever parameter sorted into that slot.
    GivenATypicalCurveType;
    Choices := FixedChoices(FParams);
    for i := 0 to High(Choices) do
    begin
        AssertTrue('row ' + Choices[i].Name + ' has a parameter',
            Assigned(Choices[i].Parameter));
        AssertEquals('and it is the one named',
            Choices[i].Name, Choices[i].Parameter.Name);
    end;
end;

procedure TParameterChoicesTest.OnlyTheNoneRowCarriesNothing;
var
    Choices: TParameterChoices;
    i: longint;
begin
    //  The dialog reads the attached object and hands it to AssignRole, where
    //  nil means "give the role to nobody". A second row carrying nil would
    //  clear the role instead of assigning it, and look like a selection.
    GivenATypicalCurveType;
    Choices := RoleChoices(FParams, prAmplitude);
    for i := 1 to High(Choices) do
        AssertTrue('row ' + Choices[i].Name + ' is a real parameter',
            Assigned(Choices[i].Parameter));
    AssertFalse('and only the first is not', Assigned(Choices[0].Parameter));
end;

procedure TParameterChoicesTest.AnEmptyParameterSetOffersNothingButNone;
begin
    //  The state a curve type is in between being named and having its formula
    //  parsed. The dialog opens on it, so every list has to answer.
    AssertEquals('no abscissa to offer', 0, Length(ArgumentChoices(FParams)));
    AssertEquals('nor a position', 0, Length(PositionChoices(FParams)));
    AssertEquals('nor anything to fix', 0, Length(FixedChoices(FParams)));
    AssertEquals('but a role can still be left unheld',
        1, Length(RoleChoices(FParams, prWidth)));
end;

procedure TParameterChoicesTest.ANilParameterSetIsNotAFailure;
begin
    //  FormActivate asserts the curve type is there, but these are ordinary
    //  functions and a caller may not have one yet. An empty list is a better
    //  answer than a fault inside a paint.
    AssertEquals(0, Length(ArgumentChoices(nil)));
    AssertEquals(0, Length(PositionChoices(nil)));
    AssertEquals(0, Length(FixedChoices(nil)));
    AssertEquals('still just "(none)"', 1,
        Length(RoleChoices(nil, prAmplitude)));
end;

initialization
    //  A unit test: a parameter set in memory. No dialog and no combo box.
    RegisterTest('unit', TParameterChoicesTest);
end.
