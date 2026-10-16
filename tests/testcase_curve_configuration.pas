// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which curve types ask the user a question before they can be added,
and what the rest promise instead.)

ADDING A CURVE ASKS THE REGISTRY FIRST. Before a curve is placed, the main form
asks the curve's class whether it has parameters the user must configure, and if
so opens its dialog. Every built-in shape - Gaussian, Lorentzian, Voigt and the
rest - answers no: it is a known formula with known parameters. Exactly one type
answers yes, the user-defined curve, because its formula does not exist until
somebody types it.

WHAT GOES WRONG WHEN A TYPE ANSWERS WRONGLY. Answering yes without a dialog to
show gives the user a curve that cannot be added and no message saying why -
the click does nothing. Answering no when configuration is needed adds a curve
with no formula, which reaches the optimiser and fails there, naming the
optimiser.

WHAT IS NOT DRIVEN HERE. The one configurable type's dialog is not opened: it is
two modal windows and a settings write. What is asserted about it is what the
form asks before opening anything - that it says it has something to configure,
and which class the question is answered by.
}
unit testcase_curve_configuration;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    curve_types_singleton, int_curve_type_iterator, named_points_set,
    configurable_points_set, non_configurable_points_set,
    configurable_user_points_set, user_points_set,
    gauss_points_set, curve_type_registration;

type
    TCurveConfigurationTest = class(TTestCase)
    private
        { The registry, walked from the first type. }
        function Types: ICurveTypeIterator;
    published
        procedure EveryTypeAnswersTheQuestionAtAll;
        procedure ExactlyOneTypeNeedsConfiguring;
        procedure ItIsTheUserDefinedCurve;
        procedure AnOrdinaryShapeIsNotConfigurable;

        procedure ATypeWithNothingToConfigureShowsNoDialog;
        procedure ATypeWithNothingToConfigureHasNoDefaults;
        procedure SettingDefaultsOnSuchATypeDoesNothing;

        procedure TheDefaultForANewCurveTypeIsNotConfigurable;
        procedure TheUserCurveAnswersThroughItsOwnClass;
        procedure TheUserCurveHasNoDefaultsToOffer;
        procedure SettingDefaultsOnTheUserCurveDoesNothing;
    end;

implementation

function TCurveConfigurationTest.Types: ICurveTypeIterator;
begin
    Result := TCurveTypesSingleton.CreateCurveTypeIterator;
    Result.FirstCurveType;
end;

{ ---- the question the form asks before adding a curve ---------------------- }

procedure TCurveConfigurationTest.EveryTypeAnswersTheQuestionAtAll;
var
    Iter: ICurveTypeIterator;
begin
    //  A nil configurable class is a nil method call on the click that adds a
    //  curve - the form does not test for it, and it has no reason to: the base
    //  class supplies a default, so only a type that overrode the method to
    //  return nothing could produce one.
    Iter := Types;
    while True do
    begin
        AssertTrue(Iter.GetCurveTypeName + ' must answer',
            Assigned(Iter.GetCurrentCurveClass.GetConfigurablePointsSet));
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
end;

procedure TCurveConfigurationTest.ExactlyOneTypeNeedsConfiguring;
var
    Iter: ICurveTypeIterator;
    Configurable: longint;
begin
    //  ONE, and the count is the assertion rather than the identity - a second
    //  type that started answering yes would open a dialog the user has never
    //  seen before on a shape that has always just been added.
    Configurable := 0;
    Iter := Types;
    while True do
    begin
        if Iter.GetCurrentCurveClass.GetConfigurablePointsSet
            .HasConfigurableParameters then
            Inc(Configurable);
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
    AssertEquals('one type asks the user a question', 1, Configurable);
end;

procedure TCurveConfigurationTest.ItIsTheUserDefinedCurve;
begin
    //  And it is this one, because its formula does not exist until somebody
    //  types it. Every other type is a known shape with known parameters.
    AssertTrue('the user curve is configurable',
        TUserPointsSet.GetConfigurablePointsSet.HasConfigurableParameters);
end;

procedure TCurveConfigurationTest.AnOrdinaryShapeIsNotConfigurable;
begin
    //  A named representative of the other side of the rule, so the count above
    //  cannot be satisfied by everything answering yes but one.
    AssertFalse('a Gaussian needs no dialog',
        TGaussPointsSet.GetConfigurablePointsSet.HasConfigurableParameters);
end;

{ ---- what a type with nothing to configure promises ------------------------ }

procedure TCurveConfigurationTest.ATypeWithNothingToConfigureShowsNoDialog;
var
    Iter: ICurveTypeIterator;
    C: TConfigurablePointsSetClass;
begin
    //  THE SECOND HALF OF THE SAME PROMISE, and the one that is only a promise:
    //  the form checks HasConfigurableParameters first and never calls this on
    //  a type that answered no. A type that opened a window here anyway would
    //  do it from a path nothing guards.
    //
    //  Answering False also means "the user did not confirm anything", which is
    //  the safe reading for a dialog that was never shown.
    Iter := Types;
    while True do
    begin
        C := Iter.GetCurrentCurveClass.GetConfigurablePointsSet;
        if not C.HasConfigurableParameters then
            AssertFalse(Iter.GetCurveTypeName + ' shows nothing',
                C.ShowConfigurationDialog);
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
end;

procedure TCurveConfigurationTest.ATypeWithNothingToConfigureHasNoDefaults;
var
    Iter: ICurveTypeIterator;
    C: TConfigurablePointsSetClass;
begin
    //  Defaults exist for configurable parameters. A type with none cannot have
    //  defaults for them, and one that claimed to would have the caller apply
    //  settings to a curve that has nowhere to put them.
    Iter := Types;
    while True do
    begin
        C := Iter.GetCurrentCurveClass.GetConfigurablePointsSet;
        if not C.HasConfigurableParameters then
            AssertFalse(Iter.GetCurveTypeName + ' has no defaults',
                C.HasDefaults);
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
end;

procedure TCurveConfigurationTest.SettingDefaultsOnSuchATypeDoesNothing;
var
    Iter: ICurveTypeIterator;
begin
    //  Called unconditionally on a fresh curve, so it has to be safe on every
    //  type rather than only on the one that has defaults. What is asserted is
    //  that it returns - an abstract method left unoverridden raises here.
    Iter := Types;
    while True do
    begin
        Iter.GetCurrentCurveClass.GetConfigurablePointsSet.SetDefaults;
        if Iter.EndCurveType then Break
        else Iter.NextCurveType;
    end;
    AssertTrue('every type survived being defaulted', True);
end;

{ ---- what a new curve type inherits ---------------------------------------- }

procedure TCurveConfigurationTest.TheDefaultForANewCurveTypeIsNotConfigurable;
begin
    //  WHAT A CURVE TYPE ADDED TOMORROW GETS FOR FREE. The base class answers
    //  for every type that does not override the method, and it answers "no
    //  dialog" - so a new shape is added by a click, which is what the author of
    //  a new shape expects. Making the default the other way would have every
    //  new type silently unaddable until its author noticed.
    AssertEquals('the inherited answer',
        TNonConfigurablePointsSet.ClassName,
        TGaussPointsSet.GetConfigurablePointsSet.ClassName);
end;

procedure TCurveConfigurationTest.TheUserCurveAnswersThroughItsOwnClass;
begin
    //  And the user curve overrides it. Pinned by class rather than by
    //  behaviour because the behaviour is two modal dialogs.
    AssertEquals('its own configurator',
        TConfigurableUserPointsSet.ClassName,
        TUserPointsSet.GetConfigurablePointsSet.ClassName);
end;

procedure TCurveConfigurationTest.TheUserCurveHasNoDefaultsToOffer;
begin
    //  NO DEFAULTS EVEN THOUGH IT IS CONFIGURABLE - the two answers are
    //  independent. There is no sensible default formula, so the dialog is the
    //  only way to get one, and a caller that took a default instead would add
    //  a curve with an empty expression.
    AssertFalse('nothing to default to',
        TUserPointsSet.GetConfigurablePointsSet.HasDefaults);
end;

procedure TCurveConfigurationTest.SettingDefaultsOnTheUserCurveDoesNothing;
begin
    //  Consistent with the answer above: asked to apply defaults it has none
    //  of, it returns rather than raising.
    TUserPointsSet.GetConfigurablePointsSet.SetDefaults;
    AssertTrue('it returned', True);
end;

initialization
    //  A unit test: the registry and a handful of class methods. No dialog is
    //  opened - see the note at the top of the file.
    RegisterTest('unit', TCurveConfigurationTest);
end.
