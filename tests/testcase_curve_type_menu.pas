// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How the curve-type menu is laid out.)

THE MENU IS THE MODEL LIBRARY. Every curve type a build can fit is in it, and
where each one sits is how a user finds it. The grouping had never been exercised
with more than one group, because the framework ships no curve pack and the only
way to build the menu was to open a window.

The invariant that matters most is the one that is hardest to see: REGISTRATION
ORDER DECIDES NOTHING. A module registering earlier or later must not move the
menu about under the user, and the only way to check that is to ask for the same
types in a different order and compare.
}
unit testcase_curve_type_menu;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, curve_type_menu, named_points_set;

type
    TCurveTypeMenuTest = class(TTestCase)
    private
        FTypes: TCurveTypeInfos;
        FEntries: TCurveMenuEntries;
        FOrder: TStringList;
        { Adds a registered type. An empty group means it declares none. }
        procedure AddType(const AName, AGroup: string; ATag: longint;
            AFactory: boolean = False);
        { Decides, with the type at ASelectedIndex selected (-1 for none). }
        procedure Decide(ASelectedIndex: longint = -1);
        function EntryFor(const ACaption: string): TCurveMenuEntry;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Which group a type belongs to.
        procedure ATypeThatDeclaresNoGroupIsStandard;
        procedure ATypeThatDeclaresOneKeepsIt;
        procedure AGroupOfOnlySpacesIsNoGroup;
        procedure TheUserCurveFactoryHeadsTheUserGroup;
        procedure TheFactorysOwnDeclaredGroupIsIgnored;

        //  The same entries as a FLAT LIST - the second projection, for a
        //  control that has no submenus.
        procedure TheFlatListCarriesAHeaderPerGroup;
        procedure ItKeepsTheMenusGroupOrder;
        procedure AHeaderIsNeverSelectable;
        procedure ATypeCarriesTheSameTagTheMenuGivesIt;
        procedure TheSelectedTypeIsMarkedOnItsRow;
        procedure AClickOnAHeaderResolvesToTheTypeBelowIt;
        procedure AClickOnATypeResolvesToItself;
        procedure NothingSelectedResolvesToTheFirstType;
        procedure APastTheEndClickResolvesToNothing;
        procedure AnEmptyListResolvesToNothing;
        procedure WithNoSelectionNoRowIsMarked;

        //  What an entry says.
        procedure ATypeIsCaptionedWithItsName;
        procedure TheFactoryIsCaptionedAsTheActionItPerforms;
        procedure EveryTypeCanCarryATick;
        procedure TheFactoryNeverCarriesOne;
        procedure TheSelectedTypeIsTicked;
        procedure OnlyTheSelectedTypeIsTicked;
        procedure NothingIsTickedWhenNothingIsSelected;
        procedure TheRegistryHandleIsCarriedThrough;

        //  The order of the groups.
        procedure TheEverydayListComesFirst;
        procedure TheUsersOwnCurvesComeLast;
        procedure ACurvePacksGroupSitsBetweenThem;
        procedure EachGroupIsNamedOnce;
        procedure RegistrationOrderDoesNotMoveTheGroups;
        procedure AnEmptyStandardGroupIsNotShown;
        procedure AnEmptyUserGroupIsNotShown;
        procedure SeveralPacksKeepTheOrderTheyWereFirstSeen;
        procedure NoTypesIsNoGroups;

        //  Whether a stored user curve can be selected at all.
        procedure AUserCurveWithAFormulaIsUsable;
        procedure OneWithNoFormulaIsNot;
        procedure OneWithNothingButSpacesIsNotEither;
        procedure ATabIsNotAFormulaEither;
        procedure AFormulaThatIsJustAConstantIsStillAFormula;
    end;

implementation

procedure TCurveTypeMenuTest.SetUp;
begin
    SetLength(FTypes, 0);
    SetLength(FEntries, 0);
    FOrder := TStringList.Create;
end;

procedure TCurveTypeMenuTest.TearDown;
begin
    FreeAndNil(FOrder);
end;

procedure TCurveTypeMenuTest.AddType(const AName, AGroup: string;
    ATag: longint; AFactory: boolean = False);
var
    Info: TCurveTypeInfo;
begin
    Info := Default(TCurveTypeInfo);
    //  A distinct id per type, built from the tag so the tests can select one.
    Info.Id := StringToGUID(Format('{00000000-0000-0000-0000-%.12d}', [ATag]));
    Info.Name := AName;
    Info.Group := AGroup;
    Info.Tag := ATag;
    Info.IsUserCurveFactory := AFactory;
    SetLength(FTypes, Length(FTypes) + 1);
    FTypes[High(FTypes)] := Info;
end;

procedure TCurveTypeMenuTest.Decide(ASelectedIndex: longint = -1);
var
    Selected: TCurveTypeId;
begin
    if ASelectedIndex >= 0 then
        Selected := FTypes[ASelectedIndex].Id
    else
        Selected := StringToGUID('{FFFFFFFF-0000-0000-0000-000000000000}');
    FEntries := CurveMenuEntries(FTypes, Selected, 'New User Curve...');
    CurveMenuGroupOrder(FEntries, FOrder);
end;

function TCurveTypeMenuTest.EntryFor(
    const ACaption: string): TCurveMenuEntry;
var
    i: longint;
begin
    Result := Default(TCurveMenuEntry);
    for i := 0 to High(FEntries) do
        if FEntries[i].Caption = ACaption then
            Exit(FEntries[i]);
    Fail('no entry captioned ' + ACaption);
end;

{ ---- which group a type belongs to ----------------------------------------- }

procedure TCurveTypeMenuTest.ATypeThatDeclaresNoGroupIsStandard;
begin
    //  Which is every type the framework itself ships.
    AddType('Gaussian', '', 1);
    Decide;
    AssertEquals('standard', StandardCurveGroup, FEntries[0].Group);
end;

procedure TCurveTypeMenuTest.ATypeThatDeclaresOneKeepsIt;
begin
    AddType('Motive', 'Patterns', 1);
    Decide;
    AssertEquals('its own', 'Patterns', FEntries[0].Group);
end;

procedure TCurveTypeMenuTest.AGroupOfOnlySpacesIsNoGroup;
begin
    //  A group named with whitespace would appear in the menu as a blank
    //  submenu, which is indistinguishable from a broken one.
    AddType('Gaussian', '   ', 1);
    Decide;
    AssertEquals('standard', StandardCurveGroup, FEntries[0].Group);
end;

procedure TCurveTypeMenuTest.TheUserCurveFactoryHeadsTheUserGroup;
begin
    //  So that everything about user curves is in one place.
    AddType('User', '', 1, True);
    Decide;
    AssertEquals('the user group', UserCurveGroup, FEntries[0].Group);
end;

procedure TCurveTypeMenuTest.TheFactorysOwnDeclaredGroupIsIgnored;
begin
    //  It belongs with the curves it creates, wherever it says it belongs.
    AddType('User', 'Patterns', 1, True);
    Decide;
    AssertEquals('the user group', UserCurveGroup, FEntries[0].Group);
end;

{ ---- what an entry says ---------------------------------------------------- }

procedure TCurveTypeMenuTest.ATypeIsCaptionedWithItsName;
begin
    AddType('Pseudo-Voigt', '', 1);
    Decide;
    AssertEquals('its name', 'Pseudo-Voigt', FEntries[0].Caption);
end;

procedure TCurveTypeMenuTest.TheFactoryIsCaptionedAsTheActionItPerforms;
begin
    //  It names no curve one can pick: clicking it CREATES one. Captioning it
    //  with a type name would put an entry in the menu that selects nothing.
    AddType('User', '', 1, True);
    Decide;
    AssertEquals('the action', 'New User Curve...', FEntries[0].Caption);
end;

procedure TCurveTypeMenuTest.EveryTypeCanCarryATick;
begin
    //  EVERY one, not only the selected one: which type is selected is a tick
    //  that MOVES, and an entry that was not created as a checkable widget
    //  cannot take it later.
    AddType('Gaussian', '', 1);
    AddType('Lorentzian', '', 2);
    Decide(0);
    AssertTrue('the selected one', FEntries[0].Checkable);
    AssertTrue('and the other one too', FEntries[1].Checkable);
end;

procedure TCurveTypeMenuTest.TheFactoryNeverCarriesOne;
begin
    //  The curve it creates carries the tick instead.
    AddType('User', '', 1, True);
    Decide(0);
    AssertFalse('not checkable', FEntries[0].Checkable);
    AssertFalse('and not checked', FEntries[0].Checked);
end;

procedure TCurveTypeMenuTest.TheSelectedTypeIsTicked;
begin
    AddType('Gaussian', '', 1);
    AddType('Lorentzian', '', 2);
    Decide(1);
    AssertTrue('the second', FEntries[1].Checked);
end;

procedure TCurveTypeMenuTest.OnlyTheSelectedTypeIsTicked;
begin
    //  Two ticks in a radio group says two models are being fitted.
    AddType('Gaussian', '', 1);
    AddType('Lorentzian', '', 2);
    AddType('Voigt', '', 3);
    Decide(1);
    AssertFalse('not the first', FEntries[0].Checked);
    AssertTrue('the second', FEntries[1].Checked);
    AssertFalse('not the third', FEntries[2].Checked);
end;

procedure TCurveTypeMenuTest.NothingIsTickedWhenNothingIsSelected;
begin
    //  A settings file that names a type this build no longer has. The menu must
    //  come up with nothing ticked rather than ticking something arbitrary.
    AddType('Gaussian', '', 1);
    AddType('Lorentzian', '', 2);
    Decide(-1);
    AssertFalse('nor the first', FEntries[0].Checked);
    AssertFalse('nor the second', FEntries[1].Checked);
end;

procedure TCurveTypeMenuTest.TheRegistryHandleIsCarriedThrough;
begin
    //  The tag is what comes back on a click and is how the registry is asked
    //  for the type again. Losing it makes every entry select the same curve.
    AddType('Gaussian', '', 11);
    AddType('Lorentzian', '', 22);
    Decide;
    AssertEquals('the first', 11, FEntries[0].Tag);
    AssertEquals('the second', 22, FEntries[1].Tag);
end;

{ ---- the order of the groups ----------------------------------------------- }

procedure TCurveTypeMenuTest.TheEverydayListComesFirst;
begin
    AddType('User', '', 9, True);
    AddType('Gaussian', '', 1);
    Decide;
    AssertEquals('standard first', StandardCurveGroup, FOrder[0]);
end;

procedure TCurveTypeMenuTest.TheUsersOwnCurvesComeLast;
begin
    AddType('User', '', 9, True);
    AddType('Gaussian', '', 1);
    AddType('Motive', 'Patterns', 2);
    Decide;
    AssertEquals('user last', UserCurveGroup, FOrder[FOrder.Count - 1]);
end;

procedure TCurveTypeMenuTest.ACurvePacksGroupSitsBetweenThem;
begin
    AddType('Gaussian', '', 1);
    AddType('Motive', 'Patterns', 2);
    AddType('User', '', 9, True);
    Decide;
    AssertEquals('three groups', 3, FOrder.Count);
    AssertEquals('standard', StandardCurveGroup, FOrder[0]);
    AssertEquals('the pack', 'Patterns', FOrder[1]);
    AssertEquals('user', UserCurveGroup, FOrder[2]);
end;

procedure TCurveTypeMenuTest.EachGroupIsNamedOnce;
begin
    //  Two submenus of the same name is two places to look for one thing.
    AddType('Gaussian', '', 1);
    AddType('Lorentzian', '', 2);
    AddType('Motive', 'Patterns', 3);
    AddType('Corrective', 'Patterns', 4);
    Decide;
    AssertEquals('two groups', 2, FOrder.Count);
end;

procedure TCurveTypeMenuTest.RegistrationOrderDoesNotMoveTheGroups;
var
    First: string;
begin
    //  THE INVARIANT THAT MATTERS MOST and is hardest to see: a module
    //  registering earlier or later must not rearrange the menu under the user.
    AddType('Gaussian', '', 1);
    AddType('Motive', 'Patterns', 2);
    AddType('User', '', 9, True);
    Decide;
    First := FOrder.CommaText;

    SetLength(FTypes, 0);
    AddType('User', '', 9, True);
    AddType('Motive', 'Patterns', 2);
    AddType('Gaussian', '', 1);
    Decide;
    AssertEquals('the same order', First, FOrder.CommaText);
end;

procedure TCurveTypeMenuTest.AnEmptyStandardGroupIsNotShown;
begin
    //  A build whose every type declares a group of its own must not show an
    //  empty Standard submenu.
    AddType('Motive', 'Patterns', 1);
    AddType('User', '', 9, True);
    Decide;
    AssertEquals('two groups', 2, FOrder.Count);
    AssertEquals('the pack first', 'Patterns', FOrder[0]);
end;

procedure TCurveTypeMenuTest.AnEmptyUserGroupIsNotShown;
begin
    //  The framework build has no user-curve factory registered in some
    //  configurations; an empty User submenu would offer nothing.
    AddType('Gaussian', '', 1);
    Decide;
    AssertEquals('one group', 1, FOrder.Count);
    AssertEquals('standard', StandardCurveGroup, FOrder[0]);
end;

procedure TCurveTypeMenuTest.SeveralPacksKeepTheOrderTheyWereFirstSeen;
begin
    //  Between Standard and User, a pack's group keeps a stable place without
    //  the framework having to know its name - and "stable" means first seen,
    //  not alphabetical, so a pack renamed does not jump.
    AddType('Gaussian', '', 1);
    AddType('Zeta', 'Zeta pack', 2);
    AddType('Alpha', 'Alpha pack', 3);
    AddType('User', '', 9, True);
    Decide;
    AssertEquals('four groups', 4, FOrder.Count);
    AssertEquals('zeta was seen first', 'Zeta pack', FOrder[1]);
    AssertEquals('alpha second', 'Alpha pack', FOrder[2]);
end;

procedure TCurveTypeMenuTest.NoTypesIsNoGroups;
begin
    //  Cannot happen with a real registry, and must not produce a menu of empty
    //  submenus if it ever does.
    Decide;
    AssertEquals('no entries', 0, Length(FEntries));
    AssertEquals('no groups', 0, FOrder.Count);
end;

{ ---- whether a stored user curve can be selected --------------------------- }

procedure TCurveTypeMenuTest.AUserCurveWithAFormulaIsUsable;
begin
    AssertTrue('usable', UserCurveIsUsable('A*exp(-x*x)'));
end;

procedure TCurveTypeMenuTest.OneWithNoFormulaIsNot;
begin
    //  SAVED WITHOUT ITS FORMULA - by an older version, or by a session
    //  interrupted between naming the curve and giving it an expression. It is
    //  a menu entry that cannot become a curve, and selecting it used to fail
    //  an assertion in the optimiser: a source line in the fitting engine
    //  shown for a menu item the user clicked.
    AssertFalse('not usable', UserCurveIsUsable(''));
end;

procedure TCurveTypeMenuTest.OneWithNothingButSpacesIsNotEither;
begin
    //  A formula of spaces evaluates to the same nothing, and the user cannot
    //  see the difference between the two in a menu.
    AssertFalse('not usable', UserCurveIsUsable('   '));
end;

procedure TCurveTypeMenuTest.ATabIsNotAFormulaEither;
begin
    //  What a settings file carries when a value was written from an empty
    //  edit box that had been tabbed through.
    AssertFalse('not usable', UserCurveIsUsable(#9));
end;

procedure TCurveTypeMenuTest.AFormulaThatIsJustAConstantIsStillAFormula;
begin
    //  A flat background is a legitimate user curve, and the rule is about
    //  ABSENCE rather than about the formula being interesting.
    AssertTrue('usable', UserCurveIsUsable('42'));
end;

{ ------------------------------ the flat list ------------------------------ }

procedure TCurveTypeMenuTest.TheFlatListCarriesAHeaderPerGroup;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    AddType('Linear ramp', 'Example', 2);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    //  Two groups, two types: four rows. A menu says this by nesting; a list
    //  has to say it with rows.
    AssertEquals('four rows', 4, Length(Rows));
    AssertTrue('the first is a header', Rows[0].IsHeader);
    AssertFalse('the second is a type', Rows[1].IsHeader);
    AssertTrue('the third is a header', Rows[2].IsHeader);
    AssertFalse('the fourth is a type', Rows[3].IsHeader);
end;

procedure TCurveTypeMenuTest.ItKeepsTheMenusGroupOrder;
var
    Rows: TCurveListRows;
begin
    AddType('Linear ramp', 'Example', 2);
    AddType('Gaussian', '', 1);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    //  Standard first whatever order the types registered in - the same rule
    //  the menu follows, from the same function, so the two cannot disagree.
    AssertEquals(StandardCurveGroup, Rows[0].Caption);
    AssertEquals('Gaussian', Rows[1].Caption);
    AssertEquals('Example', Rows[2].Caption);
    AssertEquals('Linear ramp', Rows[3].Caption);
end;

procedure TCurveTypeMenuTest.AHeaderIsNeverSelectable;
var
    Rows: TCurveListRows;
    i: longint;
begin
    AddType('Gaussian', '', 1);
    Decide(0);
    Rows := CurveTypeListRows(FEntries);
    for i := 0 to High(Rows) do
        if Rows[i].IsHeader then
            //  A header names no curve. Selecting one would ask the engine to
            //  fit a heading.
            AssertFalse('a header is not selected', Rows[i].Selected);
end;

procedure TCurveTypeMenuTest.ATypeCarriesTheSameTagTheMenuGivesIt;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 4242);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    //  The registry's handle, unchanged: the list and the menu hand the same
    //  value back, so one click path serves both.
    AssertEquals('the registry handle', 4242, Rows[1].Tag);
end;

procedure TCurveTypeMenuTest.TheSelectedTypeIsMarkedOnItsRow;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    AddType('Lorentzian', '', 2);
    Decide(1);
    Rows := CurveTypeListRows(FEntries);
    AssertEquals('the second type is selected', 2, SelectedCurveRow(Rows));
    AssertEquals('Lorentzian', Rows[2].Caption);
end;

procedure TCurveTypeMenuTest.WithNoSelectionNoRowIsMarked;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    AssertEquals('nothing marked', -1, SelectedCurveRow(Rows));
end;

procedure TCurveTypeMenuTest.AClickOnAHeaderResolvesToTheTypeBelowIt;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    //  FORWARD from the click: a header is followed by the types it heads, so
    //  the row the user was reaching for is the next one down.
    AssertEquals('the type under the header', 1, NextSelectableRow(Rows, 0));
end;

procedure TCurveTypeMenuTest.AClickOnATypeResolvesToItself;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    AssertEquals('unchanged', 1, NextSelectableRow(Rows, 1));
end;

procedure TCurveTypeMenuTest.NothingSelectedResolvesToTheFirstType;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    //  A list box with no selection reports -1. Answering "nothing" would make
    //  the first real row unreachable.
    AssertEquals('the first type', 1, NextSelectableRow(Rows, -1));
end;

procedure TCurveTypeMenuTest.APastTheEndClickResolvesToNothing;
var
    Rows: TCurveListRows;
begin
    AddType('Gaussian', '', 1);
    Decide;
    Rows := CurveTypeListRows(FEntries);
    AssertEquals('nothing there', -1, NextSelectableRow(Rows, 99));
end;

procedure TCurveTypeMenuTest.AnEmptyListResolvesToNothing;
var
    Rows: TCurveListRows;
begin
    Rows := nil;
    //  A build whose registry is empty. Answering a row index would be
    //  answering about a row that does not exist.
    AssertEquals('nothing to select', -1, NextSelectableRow(Rows, 0));
    AssertEquals('and nothing marked', -1, SelectedCurveRow(Rows));
end;

initialization
    //  A unit test: records in, records out. No menu, no window, and no curve
    //  pack - which is why the grouping had never been tried with two groups.
    RegisterTest('unit', TCurveTypeMenuTest);
end.
