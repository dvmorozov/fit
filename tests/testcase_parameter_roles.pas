// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which parameter of a user-defined curve holds which role.)

WHAT GOES WRONG WITHOUT THIS. The engine seeds a curve's amplitude from the data
peak and its width from the fitting interval, and it cannot do either for two
parameters at once. A curve type that ends up with two amplitudes is one the fit
seeds twice from the same peak; with none, its height is never estimated and the
fit starts from a default with nothing to do with the data. Either way the fit
converges on something, and the only sign is that the answer is wrong.

The rule was written out four times, once in each combo-box change handler of an
LCL dialog, and nothing asserted the invariant it exists to keep.
}
unit testcase_parameter_roles;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    parameter_roles, special_curve_parameter, persistent_curve_parameters,
    persistent_curve_parameter_container,
    //  A CONCRETE parameter class. TSpecialCurveParameter declares four
    //  abstract methods - only a descendant knows how to copy itself or seed its
    //  own value - so a bare instance faults the moment anything real touches
    //  it. Which descendant does not matter here: what is being tested is the
    //  role, and every parameter carries one.
    amplitude_curve_parameter;

type
    TParameterRolesTest = class(TTestCase)
    private
        FParams: Curve_parameters;
        { Adds a parameter of the given name and type, and returns it. }
        function Add(const AName: string;
            AType: TParameterType): TSpecialCurveParameter;
        function ByName(const AName: string): TSpecialCurveParameter;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What a role is expressed as.
        procedure EveryRoleHasItsOwnType;
        procedure NoTwoRolesShareAType;

        //  Who may take one.
        procedure AFreeParameterCanBeTheAmplitude;
        procedure TheParameterAlreadyHoldingItIsStillACandidate;
        procedure TheAbscissaCannotAlsoBeTheAmplitude;
        procedure TheAbscissaCannotAlsoPlaceTheCurve;
        procedure ANilParameterHoldsNothing;

        //  Assigning.
        procedure AssigningARoleGivesIt;
        procedure AssigningARoleTakesItFromWhoeverHadIt;
        procedure OnlyEverOneParameterHoldsARole;
        procedure AssigningNilLeavesTheRoleUnheld;
        procedure ReleasingARoleReturnsTheParameterToVaried;
        procedure AssigningOneRoleDoesNotDisturbAnother;
        procedure TheSameParameterCanBeReassignedToItsOwnRole;

        //  The position, which wears two hats.
        procedure EitherKindOfPositionCountsAsThePosition;
        procedure ChangingThePositionClearsBothKinds;

        //  Fixing a parameter.
        procedure FixingAnOrdinaryParameterSharesIt;
        procedure UnfixingAnOrdinaryParameterVariesIt;
        procedure FixingAPositionDoesNotShareIt;
        procedure UnfixingAPositionVariesThePositionNotTheValue;
        procedure FixingIsItsOwnInverseForTheStatesTheBoxCanShow;
        procedure UnfixingAnAlreadyVariedPositionWouldLoseIt;

        //  NO PARAMETERS AT ALL. Every query over a container carries the same
        //  guard, and it is asked of all of them at once - see the group.
        procedure EveryQueryToleratesAnAbsentContainer;
        procedure AndAnEmptyOne;
        procedure AParameterThatDoesNotHoldTheRoleIsSkippedNotAnswered;
    end;

implementation

procedure TParameterRolesTest.SetUp;
begin
    FParams := Curve_parameters.Create(nil);
    //  Curve_parameters seeds a placeholder the filer needs; these tests are
    //  about the parameters a curve type actually declares.
    FParams.Params.Clear;
end;

procedure TParameterRolesTest.TearDown;
begin
    FreeAndNil(FParams);
end;

function TParameterRolesTest.Add(const AName: string;
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

function TParameterRolesTest.ByName(
    const AName: string): TSpecialCurveParameter;
var
    i: longint;
begin
    Result := nil;
    for i := 0 to FParams.Count - 1 do
        if FParams[i].Name = AName then
            Exit(FParams[i]);
end;

{ ---- what a role is expressed as ------------------------------------------- }

procedure TParameterRolesTest.EveryRoleHasItsOwnType;
begin
    AssertTrue('the abscissa', RoleType(prArgument) = Argument);
    AssertTrue('the position', RoleType(prPosition) = InvariablePosition);
    AssertTrue('the amplitude', RoleType(prAmplitude) = Amplitude);
    AssertTrue('the width',
        RoleType(prWidth) = special_curve_parameter.Width);
end;

procedure TParameterRolesTest.NoTwoRolesShareAType;
var
    A, B: TParameterRole;
begin
    //  Two roles under one type would be indistinguishable once assigned, and
    //  clearing one would clear the other.
    for A := Low(TParameterRole) to High(TParameterRole) do
        for B := Low(TParameterRole) to High(TParameterRole) do
            if A <> B then
                AssertTrue(Format('roles %d and %d differ', [Ord(A), Ord(B)]),
                    RoleType(A) <> RoleType(B));
end;

{ ---- who may take one ------------------------------------------------------ }

procedure TParameterRolesTest.AFreeParameterCanBeTheAmplitude;
begin
    AssertTrue('a varied parameter',
        CanHoldRole(Add('A', Variable), prAmplitude));
end;

procedure TParameterRolesTest.TheParameterAlreadyHoldingItIsStillACandidate;
begin
    //  Otherwise the combo could not show what is currently selected, and the
    //  user would see "(none)" over a role that is held.
    AssertTrue('the current amplitude',
        CanHoldRole(Add('A', Amplitude), prAmplitude));
    AssertTrue('the current width',
        CanHoldRole(Add('w', special_curve_parameter.Width), prWidth));
end;

procedure TParameterRolesTest.TheAbscissaCannotAlsoBeTheAmplitude;
begin
    //  A NAME CANNOT BE TWO THINGS AT ONCE. Offering the abscissa here would let
    //  the user make x both the variable the formula is written in and the
    //  height the fit seeds from the peak.
    AssertFalse('the abscissa', CanHoldRole(Add('x', Argument), prAmplitude));
end;

procedure TParameterRolesTest.TheAbscissaCannotAlsoPlaceTheCurve;
begin
    AssertFalse('the abscissa', CanHoldRole(Add('x', Argument), prPosition));
end;

procedure TParameterRolesTest.ANilParameterHoldsNothing;
begin
    //  The combo's "(none)" item carries nil.
    AssertFalse('nil', CanHoldRole(nil, prAmplitude));
end;

{ ---- assigning ------------------------------------------------------------- }

procedure TParameterRolesTest.AssigningARoleGivesIt;
begin
    Add('A', Variable);
    AssignRole(FParams, prAmplitude, ByName('A'));
    AssertTrue('A has it', ByName('A').Type_ = Amplitude);
    AssertTrue('and it is found', ParameterWithRole(FParams, prAmplitude) =
        ByName('A'));
end;

procedure TParameterRolesTest.AssigningARoleTakesItFromWhoeverHadIt;
begin
    Add('A', Amplitude);
    Add('B', Variable);
    AssignRole(FParams, prAmplitude, ByName('B'));
    AssertTrue('B has it', ByName('B').Type_ = Amplitude);
    AssertTrue('and A no longer does', ByName('A').Type_ = Variable);
end;

procedure TParameterRolesTest.OnlyEverOneParameterHoldsARole;
var
    R: TParameterRole;
begin
    //  THE INVARIANT THIS UNIT EXISTS FOR, asserted for every role over a set
    //  that starts out with the role held twice - which is exactly the state
    //  four hand-written copies of the rule could produce.
    Add('A', Amplitude);
    Add('B', Amplitude);
    Add('C', Variable);
    AssignRole(FParams, prAmplitude, ByName('C'));
    for R := Low(TParameterRole) to High(TParameterRole) do
        AssertTrue(Format('role %d is held at most once', [Ord(R)]),
            CountWithRole(FParams, R) <= 1);
    AssertEquals('and the amplitude is held exactly once', 1,
        CountWithRole(FParams, prAmplitude));
end;

procedure TParameterRolesTest.AssigningNilLeavesTheRoleUnheld;
begin
    //  What the combo's "(none)" means. The role is cleared and given to
    //  nobody, which is a legitimate state - not every curve type has a width.
    Add('A', Amplitude);
    AssignRole(FParams, prAmplitude, nil);
    AssertEquals('nobody holds it', 0, CountWithRole(FParams, prAmplitude));
    AssertTrue('and nothing is found',
        ParameterWithRole(FParams, prAmplitude) = nil);
end;

procedure TParameterRolesTest.ReleasingARoleReturnsTheParameterToVaried;
begin
    //  VARIED, not fixed and not shared. A parameter that was the amplitude and
    //  is no longer is still a parameter the fit varies; leaving it as anything
    //  else would silently constrain it.
    Add('A', Amplitude);
    AssignRole(FParams, prAmplitude, nil);
    AssertTrue('varied', ByName('A').Type_ = Variable);
end;

procedure TParameterRolesTest.AssigningOneRoleDoesNotDisturbAnother;
begin
    //  Each role is cleared on its own. Clearing too widely would drop the
    //  abscissa every time the user changed the amplitude, and the formula
    //  would stop having a variable.
    Add('x', Argument);
    Add('A', Variable);
    Add('w', Variable);
    AssignRole(FParams, prAmplitude, ByName('A'));
    AssertTrue('the abscissa is untouched', ByName('x').Type_ = Argument);
    AssignRole(FParams, prWidth, ByName('w'));
    AssertTrue('and so is the amplitude', ByName('A').Type_ = Amplitude);
    AssertTrue('and the abscissa still', ByName('x').Type_ = Argument);
end;

procedure TParameterRolesTest.TheSameParameterCanBeReassignedToItsOwnRole;
begin
    //  Clearing then assigning must not leave it cleared - which is what a
    //  clear-after-assign order would do, and the combos fire on every change.
    Add('A', Amplitude);
    AssignRole(FParams, prAmplitude, ByName('A'));
    AssertTrue('still the amplitude', ByName('A').Type_ = Amplitude);
    AssertEquals('and held once', 1, CountWithRole(FParams, prAmplitude));
end;

{ ---- the position ---------------------------------------------------------- }

procedure TParameterRolesTest.EitherKindOfPositionCountsAsThePosition;
begin
    //  ONE ROLE WEARING TWO HATS: fixed and varied. A search that looked only
    //  for the fixed kind would report no position on a curve whose position
    //  the fit is allowed to move.
    Add('x0', VariablePosition);
    AssertTrue('a varied position is the position',
        ParameterWithRole(FParams, prPosition) = ByName('x0'));
    AssertEquals('and counted once', 1,
        CountWithRole(FParams, prPosition));
end;

procedure TParameterRolesTest.ChangingThePositionClearsBothKinds;
begin
    //  Clearing only the fixed kind would leave a second parameter still
    //  placing the curve, and the engine would have two answers to where it is.
    Add('x0', VariablePosition);
    Add('x1', Variable);
    AssignRole(FParams, prPosition, ByName('x1'));
    AssertTrue('the old one is freed', ByName('x0').Type_ = Variable);
    AssertEquals('and only one places the curve', 1,
        CountWithRole(FParams, prPosition));
end;

{ ---- fixing a parameter ---------------------------------------------------- }

procedure TParameterRolesTest.FixingAnOrdinaryParameterSharesIt;
begin
    //  "Fixed" in this dialog means SHARED - held to one value across the curves
    //  of an interval - which is what the tick has always meant here.
    AssertTrue('shared', TypeAfterFixing(Variable, True) = Shared);
end;

procedure TParameterRolesTest.UnfixingAnOrdinaryParameterVariesIt;
begin
    AssertTrue('varied', TypeAfterFixing(Shared, False) = Variable);
end;

procedure TParameterRolesTest.FixingAPositionDoesNotShareIt;
begin
    //  A POSITION IS ITS OWN PAIR. Folding it into the general case would turn a
    //  fixed position into a shared one - and a shared position is one the fit
    //  moves, jointly, which is the opposite of what the user asked for.
    AssertTrue('fixed, not shared',
        TypeAfterFixing(VariablePosition, True) = InvariablePosition);
end;

procedure TParameterRolesTest.UnfixingAPositionVariesThePositionNotTheValue;
begin
    AssertTrue('a varied position',
        TypeAfterFixing(InvariablePosition, False) = VariablePosition);
end;

procedure TParameterRolesTest.FixingIsItsOwnInverseForTheStatesTheBoxCanShow;
begin
    //  Tick then untick returns the parameter to where it was. A setting the
    //  user cannot undo by undoing it is worse than one they cannot make.
    //
    //  OVER THE PAIRS THE BOX CAN ACTUALLY SHOW: a ticked box means Shared or
    //  InvariablePosition, an unticked one Variable or VariablePosition. The
    //  cross combinations are asserted separately below, because one of them
    //  does not round-trip and that is worth naming rather than burying.
    AssertTrue('an ordinary parameter, ticked then unticked',
        TypeAfterFixing(TypeAfterFixing(Variable, True), False) = Variable);
    AssertTrue('an ordinary parameter, unticked then ticked',
        TypeAfterFixing(TypeAfterFixing(Shared, False), True) = Shared);
    AssertTrue('a position, ticked then unticked',
        TypeAfterFixing(TypeAfterFixing(VariablePosition, True), False) =
        VariablePosition);
    AssertTrue('a position, unticked then ticked',
        TypeAfterFixing(TypeAfterFixing(InvariablePosition, False), True) =
        InvariablePosition);
end;

procedure TParameterRolesTest.UnfixingAnAlreadyVariedPositionWouldLoseIt;
begin
    //  ASSERTED AS IT BEHAVES, AND IT IS A TRAP. Unticking a parameter that is
    //  ALREADY a varied position turns it into an ordinary variable - the curve
    //  silently stops having a position at all.
    //
    //  The interface cannot reach it today: the box is only asked this when its
    //  state changed, and a varied position is what an unticked box already
    //  shows, so "untick an unticked box" does not happen. It is pinned here so
    //  that a caller who starts asking unconditionally - a "reset all" button,
    //  a settings restore - finds this test rather than a curve that lost its
    //  position. See findings.md.
    AssertTrue('it becomes an ordinary variable',
        TypeAfterFixing(VariablePosition, False) = Variable);
    AssertTrue('and no longer places the curve',
        TypeAfterFixing(VariablePosition, False) <> VariablePosition);
end;

{ -------------------------- no parameters at all ---------------------------- }

{ EVERY QUERY OVER A CONTAINER OPENS WITH THE SAME NIL GUARD, six of them, and
  none had been exercised. They are not defensive padding: a curve type is asked
  for its roles while the dialog that will fill it is still being built, so the
  container legitimately does not exist yet - and without the guard the roles
  panel faults while opening rather than showing nothing.

  Asked of the whole family in one test, so a seventh query added later has to
  carry the guard too. Written as one call apiece rather than a loop because the
  signatures differ; what makes it a family test is that a missing guard fails
  here rather than in whichever test happened to touch that function. }

procedure TParameterRolesTest.EveryQueryToleratesAnAbsentContainer;
begin
    AssertTrue('no parameter holds a role',
        ParameterWithRole(nil, prAmplitude) = nil);
    AssertEquals('nothing holds one', 0, CountWithRole(nil, prAmplitude));
    AssertEquals('no argument choices', 0, Length(ArgumentChoices(nil)));
    AssertEquals('no position choices', 0, Length(PositionChoices(nil)));
    AssertEquals('no fixed choices', 0, Length(FixedChoices(nil)));
    //  ONE EXCEPTION, AND IT IS DELIBERATE. RoleChoices always opens with the
    //  "(none)" row, with no parameters or with a hundred: that row is how the
    //  user takes a role away again, and a list without it would make the first
    //  assignment permanent. So the empty answer here is one entry, not none.
    AssertEquals('the role list still offers (none)', 1,
        Length(RoleChoices(nil, prWidth)));
    //  CHARACTERISED: the nil guard exits before the marking pass, so that row
    //  is present and unmarked - a combo built from this shows nothing selected
    //  rather than "(none)". Harmless where it happens, since the container
    //  does not exist yet and the panel is not showing anything; recorded so it
    //  is a known state rather than a surprise.
    AssertEquals('and nothing is marked in it', -1,
        MarkedIndex(RoleChoices(nil, prWidth)));
    AssertEquals('nothing is marked in an empty list', -1,
        MarkedIndex(ArgumentChoices(nil)));
    //  And the one that CHANGES something must not fault either: the panel
    //  assigns a role while rebuilding itself.
    AssignRole(nil, prAmplitude, nil);
    AssertTrue('and nothing raised', True);
end;

procedure TParameterRolesTest.AndAnEmptyOne;
begin
    //  A DIFFERENT STATE FROM NIL, and the one a real curve type reaches: the
    //  container exists and the formula has declared nothing yet. A guard that
    //  only checked for nil would walk an empty collection here, which is
    //  correct - so this says the answers are the same either way rather than
    //  that the guard fired.
    FParams.Params.Clear;
    AssertTrue('no parameter holds a role',
        ParameterWithRole(FParams, prAmplitude) = nil);
    AssertEquals('nothing holds one', 0, CountWithRole(FParams, prAmplitude));
    AssertEquals('no argument choices', 0,
        Length(ArgumentChoices(FParams)));
    AssertEquals('no fixed choices', 0, Length(FixedChoices(FParams)));
end;

procedure TParameterRolesTest.AParameterThatDoesNotHoldTheRoleIsSkippedNotAnswered;
var
    Amp: TSpecialCurveParameter;
begin
    //  THE SKIP EDGE. With two parameters and only one holding the role, a walk
    //  that answered the first thing it looked at would hand back the wrong
    //  parameter - and the roles panel would show the amplitude where the width
    //  belongs, which the user then "corrects" onto the wrong one.
    Add('w', Variable);
    Amp := Add('A', Variable);
    AssignRole(FParams, prAmplitude, Amp);
    AssertTrue('the one that holds it',
        ParameterWithRole(FParams, prAmplitude) = Amp);
    AssertEquals('and only it', 1, CountWithRole(FParams, prAmplitude));
    AssertTrue('the other role is unheld',
        ParameterWithRole(FParams, prWidth) = nil);
end;

initialization
    //  A unit test: a parameter set in memory. No dialog, no combo box.
    RegisterTest('unit', TParameterRolesTest);
end.
