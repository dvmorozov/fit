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
    { MODEL > BACKGROUND > CURVE, decided without a window: which entries there
      are, which is ticked, and which the Enable Variation rule refuses. And the
      other side of it - the peak-type menu no longer offers a background. }
    TBackgroundMenuTest = class(TTestCase)
    private
        FTypes: TCurveTypeInfos;
        procedure AddType(const AName: string; ATag: longint;
            AIsBackground: boolean; const AHint: string = '');
        function IdOf(ATag: longint): TCurveTypeId;
    published
        procedure BackgroundTypesAreNotOfferedAsPeakTypes;
        procedure TheBackgroundMenuOffersNoneThenEveryBackgroundShape;
        procedure NoneCarriesNoRegistryHandle;
        procedure NoneIsTickedWhenTheModelHasNoBackground;
        procedure TheBackgroundTheModelHasIsTicked;
        procedure EachShapeSaysWhatItIs;
        procedure WithVariationOnTheShapesAreRefusedAndSayWhy;
        procedure ButNoneIsNeverRefused;
    end;

type
    { ONE MODEL, ONE MODULE, as the menus show it (fit-performance.md, stage 6):
      once the model holds anything, another module's types are listed but
      greyed - disabled, never hidden, so the layout does not move and the
      user can still read what exists - each saying why and what to do. }
    TCurveTypeModuleMenuTest = class(TTestCase)
    private
        FTypes: TCurveTypeInfos;
        procedure AddType(const AName, AOwner: string; ATag: longint;
            AIsBackground: boolean = False);
        function IdOf(ATag: longint): TCurveTypeId;
        function EntryOf(const AEntries: TCurveMenuEntries;
            const ACaption: string): TCurveMenuEntry;
    published
        procedure WithAnEmptyModelEveryTypeIsOffered;
        procedure WithContentAnotherModulesTypesAreGreyedAndSayWhy;
        procedure ItsOwnModulesTypesStayOffered;
        procedure TheFlatListGreysTheSameRows;
        procedure AModelWhoseTypeTakesNoBackgroundGreysEveryShapeButNone;
    end;

    TCurveTypeMenuTest = class(TTestCase)
    private
        FTypes: TCurveTypeInfos;
        FEntries: TCurveMenuEntries;
        FOrder: TStringList;
        { Adds a registered type. An empty group means it declares none. }
        procedure AddType(const AName, AGroup: string; ATag: longint;
            AFactory: boolean = False; const AHint: string = '');
        { Decides, with the type at ASelectedIndex selected (-1 for none). }
        procedure Decide(ASelectedIndex: longint = -1);
        function EntryFor(const ACaption: string): TCurveMenuEntry;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheHoveredTypeShowsItsOwnSummary;
        procedure AGroupHeadingShowsTheListsOwnHint;
        procedure OffTheRowsTheListsOwnHintIsShown;
        procedure ATypeWithNothingToSayFallsBackToTheListsHint;
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

        //  What a type says about itself before it is chosen.
        procedure AnEntryCarriesItsTypesHint;
        procedure TheFactoryEntryCarriesItsHintToo;
        procedure TheFlatListRowCarriesTheSameHint;
        procedure AHeaderRowCarriesNoHint;
    end;

implementation

procedure TCurveTypeModuleMenuTest.AddType(const AName, AOwner: string;
    ATag: longint; AIsBackground: boolean);
var
    Info: TCurveTypeInfo;
begin
    Info := Default(TCurveTypeInfo);
    Info.Id := IdOf(ATag);
    Info.Name := AName;
    Info.Owner := AOwner;
    Info.Tag := ATag;
    Info.Hint := AName + ' is a shape';
    Info.IsBackground := AIsBackground;
    SetLength(FTypes, Length(FTypes) + 1);
    FTypes[High(FTypes)] := Info;
end;

function TCurveTypeModuleMenuTest.IdOf(ATag: longint): TCurveTypeId;
begin
    Result := StringToGUID(Format('{00000000-0000-0000-0000-%.12d}', [ATag]));
end;

function TCurveTypeModuleMenuTest.EntryOf(const AEntries: TCurveMenuEntries;
    const ACaption: string): TCurveMenuEntry;
var
    i: longint;
begin
    Result := Default(TCurveMenuEntry);
    for i := 0 to High(AEntries) do
        if AEntries[i].Caption = ACaption then
            Exit(AEntries[i]);
    Fail('no entry captioned ' + ACaption);
end;

procedure TCurveTypeModuleMenuTest.WithAnEmptyModelEveryTypeIsOffered;
var
    E: TCurveMenuEntries;
begin
    AddType('Gaussian', 'Standard', 1);
    AddType('Impulse (5)', 'Waves', 2);
    E := CurveMenuEntries(FTypes, IdOf(1), 'New...', False);
    AssertTrue('its own module''s', EntryOf(E, 'Gaussian').Enabled);
    AssertTrue('and another''s: choosing is how a model gets its module',
        EntryOf(E, 'Impulse (5)').Enabled);
    AssertEquals('saying what it is', 'Impulse (5) is a shape',
        EntryOf(E, 'Impulse (5)').Hint);
end;

procedure TCurveTypeModuleMenuTest.WithContentAnotherModulesTypesAreGreyedAndSayWhy;
var
    E: TCurveMenuEntries;
begin
    AddType('Gaussian', 'Standard', 1);
    AddType('Impulse (5)', 'Waves', 2);
    E := CurveMenuEntries(FTypes, IdOf(1), 'New...', True);
    AssertFalse('listed, but greyed', EntryOf(E, 'Impulse (5)').Enabled);
    //  The engine's own words (fit_advice), so the menu and a refusal agree.
    AssertTrue('saying why and what to do: ' + EntryOf(E, 'Impulse (5)').Hint,
        Pos('File > New Project', EntryOf(E, 'Impulse (5)').Hint) > 0);
end;

procedure TCurveTypeModuleMenuTest.ItsOwnModulesTypesStayOffered;
var
    E: TCurveMenuEntries;
begin
    AddType('Impulse (5)', 'Waves', 1);
    AddType('Ramp', 'Waves', 2);
    AddType('Gaussian', 'Standard', 3);
    E := CurveMenuEntries(FTypes, IdOf(1), 'New...', True);
    AssertTrue('the selected one', EntryOf(E, 'Impulse (5)').Enabled);
    AssertTrue('and its siblings', EntryOf(E, 'Ramp').Enabled);
    AssertFalse('but not another module''s', EntryOf(E, 'Gaussian').Enabled);
end;

procedure TCurveTypeModuleMenuTest.TheFlatListGreysTheSameRows;
var
    Rows: TCurveListRows;
    i: longint;
    Seen: boolean;
begin
    AddType('Gaussian', 'Standard', 1);
    AddType('Impulse (5)', 'Waves', 2);
    Rows := CurveTypeListRows(CurveMenuEntries(FTypes, IdOf(1), 'New...', True));
    Seen := False;
    for i := 0 to High(Rows) do
        if Rows[i].Caption = 'Impulse (5)' then
        begin
            Seen := True;
            AssertFalse('greyed in the list too', Rows[i].Enabled);
            AssertTrue('with the reason as its hint',
                Pos('File > New Project', Rows[i].Hint) > 0);
        end
        else if Rows[i].Caption = 'Gaussian' then
            AssertTrue('the model''s own stays offered', Rows[i].Enabled);
    AssertTrue('the row is listed', Seen);
end;

procedure TCurveTypeModuleMenuTest.AModelWhoseTypeTakesNoBackgroundGreysEveryShapeButNone;
var
    E: TBackgroundMenuEntries;
    i: longint;
begin
    AddType('Linear background', 'Standard', 1, True);
    AddType('Quadratic background', 'Standard', 2, True);
    E := BackgroundMenuEntries(FTypes, GUID_NULL, False, 'None', 'No background',
        False, 'Impulse (5)');
    AssertTrue('None stays: it is the way out', E[0].Enabled);
    for i := 1 to High(E) do
    begin
        AssertFalse(E[i].Caption + ' is greyed', E[i].Enabled);
        AssertTrue('naming the model''s type: ' + E[i].Hint,
            Pos('Impulse (5)', E[i].Hint) > 0);
    end;
end;

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
    ATag: longint; AFactory: boolean = False; const AHint: string = '');
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
    Info.Hint := AHint;
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


procedure TCurveTypeMenuTest.AnEntryCarriesItsTypesHint;
begin
    AddType('Gaussian', '', 1, False, 'A symmetric bell-shaped peak.');
    Decide;
    AssertEquals('A symmetric bell-shaped peak.', EntryFor('Gaussian').Hint);
end;

procedure TCurveTypeMenuTest.TheFactoryEntryCarriesItsHintToo;
begin
    AddType('User', '', 2, True, 'A curve whose formula you type yourself.');
    Decide;
    AssertEquals('A curve whose formula you type yourself.',
        EntryFor('New User Curve...').Hint);
end;

procedure TCurveTypeMenuTest.TheFlatListRowCarriesTheSameHint;
var
    Rows: TCurveListRows;
    i: longint;
    Found: boolean;
begin
    AddType('Gaussian', '', 1, False, 'A symmetric bell-shaped peak.');
    Decide;
    Rows := CurveTypeListRows(FEntries);
    Found := False;
    for i := 0 to High(Rows) do
        if Rows[i].Caption = 'Gaussian' then
        begin
            AssertEquals('A symmetric bell-shaped peak.', Rows[i].Hint);
            Found := True;
        end;
    AssertTrue(Found);
end;

procedure TCurveTypeMenuTest.AHeaderRowCarriesNoHint;
var
    Rows: TCurveListRows;
    i: longint;
begin
    AddType('Gaussian', '', 1, False, 'A symmetric bell-shaped peak.');
    Decide;
    Rows := CurveTypeListRows(FEntries);
    for i := 0 to High(Rows) do
        if Rows[i].IsHeader then
            AssertEquals(Rows[i].Caption, '', Rows[i].Hint);
end;


function HintRows: TCurveListRows;
begin
    SetLength(Result, 3);
    Result[0].Caption := 'Peaks'; Result[0].IsHeader := True;
    Result[1].Caption := 'Gaussian'; Result[1].Hint := 'A symmetric bell.';
    Result[2].Caption := 'Nameless'; Result[2].Hint := '';
end;

procedure TCurveTypeMenuTest.TheHoveredTypeShowsItsOwnSummary;
begin
    //  Choosing a type starts from knowing what it is - the same summary its
    //  menu entry carries and the Explain pane opens with.
    AssertEquals('A symmetric bell.', CurveListHintAt(HintRows, 1, 'the list'));
end;

procedure TCurveTypeMenuTest.AGroupHeadingShowsTheListsOwnHint;
begin
    AssertEquals('the list', CurveListHintAt(HintRows, 0, 'the list'));
end;

procedure TCurveTypeMenuTest.OffTheRowsTheListsOwnHintIsShown;
begin
    AssertEquals('below the last row', 'the list', CurveListHintAt(HintRows, 3, 'the list'));
    AssertEquals('no row at all', 'the list', CurveListHintAt(HintRows, -1, 'the list'));
end;

procedure TCurveTypeMenuTest.ATypeWithNothingToSayFallsBackToTheListsHint;
begin
    AssertEquals('the list', CurveListHintAt(HintRows, 2, 'the list'));
end;

{ ---- TBackgroundMenuTest ---- }

procedure TBackgroundMenuTest.AddType(const AName: string; ATag: longint;
    AIsBackground: boolean; const AHint: string);
var
    Info: TCurveTypeInfo;
begin
    Info := Default(TCurveTypeInfo);
    Info.Id := IdOf(ATag);
    Info.Name := AName;
    Info.Tag := ATag;
    Info.IsBackground := AIsBackground;
    Info.Hint := AHint;
    SetLength(FTypes, Length(FTypes) + 1);
    FTypes[High(FTypes)] := Info;
end;

function TBackgroundMenuTest.IdOf(ATag: longint): TCurveTypeId;
begin
    Result := StringToGUID(Format('{00000000-0000-0000-0000-%.12d}', [ATag]));
end;

procedure TBackgroundMenuTest.BackgroundTypesAreNotOfferedAsPeakTypes;
var
    Entries: TCurveMenuEntries;
begin
    //  A background has no position, so as the PEAK type every pick would make
    //  another copy of the same baseline. It is offered where it belongs.
    AddType('Gaussian', 1, False);
    AddType('Linear background', 2, True);
    Entries := CurveMenuEntries(FTypes, IdOf(1), 'New User Curve...');
    AssertEquals('the peak alone', 1, Length(Entries));
    AssertEquals('Gaussian', Entries[0].Caption);
end;

procedure TBackgroundMenuTest.TheBackgroundMenuOffersNoneThenEveryBackgroundShape;
var
    Entries: TBackgroundMenuEntries;
begin
    AddType('Gaussian', 1, False);
    AddType('Linear background', 2, True);
    AddType('Quadratic background', 3, True);
    Entries := BackgroundMenuEntries(FTypes, GUID_NULL, False, 'None', '');
    AssertEquals('none and the two shapes', 3, Length(Entries));
    AssertEquals('None', Entries[0].Caption);
    AssertEquals('Linear background', Entries[1].Caption);
    AssertEquals('Quadratic background', Entries[2].Caption);
end;

procedure TBackgroundMenuTest.NoneCarriesNoRegistryHandle;
var
    Entries: TBackgroundMenuEntries;
begin
    AddType('Linear background', 2, True);
    Entries := BackgroundMenuEntries(FTypes, GUID_NULL, False, 'None', '');
    AssertEquals(NoBackgroundTag, Entries[0].Tag);
    AssertEquals('a shape carries its own', 2, Entries[1].Tag);
end;

procedure TBackgroundMenuTest.NoneIsTickedWhenTheModelHasNoBackground;
var
    Entries: TBackgroundMenuEntries;
begin
    AddType('Linear background', 2, True);
    Entries := BackgroundMenuEntries(FTypes, GUID_NULL, False, 'None', '');
    AssertTrue(Entries[0].Checked);
    AssertFalse(Entries[1].Checked);
end;

procedure TBackgroundMenuTest.TheBackgroundTheModelHasIsTicked;
var
    Entries: TBackgroundMenuEntries;
begin
    AddType('Linear background', 2, True);
    AddType('Quadratic background', 3, True);
    Entries := BackgroundMenuEntries(FTypes, IdOf(3), False, 'None', '');
    AssertFalse(Entries[0].Checked);
    AssertFalse(Entries[1].Checked);
    AssertTrue(Entries[2].Checked);
end;

procedure TBackgroundMenuTest.EachShapeSaysWhatItIs;
var
    Entries: TBackgroundMenuEntries;
begin
    AddType('Linear background', 2, True, 'A straight-line background.');
    Entries := BackgroundMenuEntries(FTypes, GUID_NULL, False, 'None',
        'No background curve.');
    AssertEquals('No background curve.', Entries[0].Hint);
    AssertEquals('A straight-line background.', Entries[1].Hint);
end;

procedure TBackgroundMenuTest.WithVariationOnTheShapesAreRefusedAndSayWhy;
var
    Entries: TBackgroundMenuEntries;
begin
    //  THE SAME RULE THE ENGINE REFUSES WITH, asked of fit_advice - so the menu
    //  greys exactly what the server would refuse, and says the same words.
    AddType('Linear background', 2, True, 'A straight-line background.');
    Entries := BackgroundMenuEntries(FTypes, GUID_NULL, True, 'None', '');
    AssertFalse('refused', Entries[1].Enabled);
    AssertTrue('naming the option: ' + Entries[1].Hint,
        Pos('Enable Variation', Entries[1].Hint) > 0);
end;

procedure TBackgroundMenuTest.ButNoneIsNeverRefused;
var
    Entries: TBackgroundMenuEntries;
begin
    AddType('Linear background', 2, True);
    Entries := BackgroundMenuEntries(FTypes, GUID_NULL, True, 'None', '');
    AssertTrue(Entries[0].Enabled);
end;

initialization
    RegisterTest('unit', TBackgroundMenuTest);
    //  A unit test: records in, records out. No menu, no window, and no curve
    //  pack - which is why the grouping had never been tried with two groups.
    RegisterTest('unit', TCurveTypeMenuTest);
    RegisterTest('unit', TCurveTypeModuleMenuTest);
end.
