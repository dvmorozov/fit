// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The menu a module's declarations describe.)

A MODULE NAMES NO WIDGET - it declares its menu as data and the window builds
whatever a menu is there. That translation is the contract's load-bearing half,
and it had never been run against anything but the single pack that exists, in
the one order that pack happens to declare its entries in. The framework ships no
module, and building the menu needed a window.

What it has to get right is what a module author cannot see: an entry naming a
submenu that does not exist, an entry naming one declared later, a separator, a
radio group, a toggle that must be a checkable widget from birth because it is
ticked from a poll while the menu may be open.
}
unit testcase_module_menu;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, module_menu, int_ui_host,
    mock_ui_module;

type
    TModuleMenuTest = class(TTestCase)
    private
        FDecls: TUiMenuDeclArray;
        FNodes: TModuleMenuNodes;
        procedure Declare(const AId, AParent, ACaption: string;
            AKind: TUiMenuKind; AChecked: boolean = False;
            ARadioGroup: longint = 0);
        procedure Build;
        function NodeOf(const AId: string): TModuleMenuNode;
    protected
        procedure SetUp; override;
    published
        //  The module's own heading.
        procedure TheModuleNameIsCapitalisedForItsMenu;
        procedure ANameAlreadyCapitalisedIsUnchanged;
        procedure AnEmptyNameDoesNotFault;
        procedure OnlyTheFirstLetterIsTouched;

        //  What kind of widget each entry is.
        procedure ACommandIsClickable;
        procedure ASubmenuIsNotClickable;
        procedure ASeparatorGetsTheCaptionThatDrawsALine;
        procedure ARadioEntryCarriesItsGroup;
        procedure AToggleIsCheckableEvenWhenItStartsUnticked;
        procedure ACommandIsNotCheckable;
        procedure OnlyRadiosAndTogglesCanStartTicked;

        //  What hangs from what.
        procedure AnEntryWithNoParentHangsFromTheModuleRoot;
        procedure AnEntryNamingASubmenuHangsFromIt;
        procedure TwoEntriesCanShareASubmenu;
        procedure ASubmenuInsideASubmenuIsResolved;

        //  Damage.
        procedure AnEntryNamingNoSuchSubmenuIsStillShown;
        procedure AnEntryNamingNoSuchSubmenuIsFlagged;
        procedure AnEntryDeclaredBeforeItsParentFallsToTheRoot;
        procedure AnEntryNamingANonSubmenuAsItsParentFallsToTheRoot;
        procedure AWellPlacedEntryIsNotFlagged;

        //  Identity.
        procedure EveryEntryKeepsItsId;
        procedure EveryEntryKeepsItsHint;
        procedure NoDeclarationsIsNoMenu;
    end;

    { WHOSE WORDS A PANEL SPEAKS IN. The window draws panels it does not own,
      and the id it is handed is the only thing tying the rows to the module
      whose language describes them. }
    TModulePanelTextTest = class(TTestCase)
    private
        FA, FB: TMockUiModule;
        FAll: TUiModuleArray;
        procedure GivenTwoModules;
    protected
        procedure TearDown; override;
    published
        procedure APanelTakesItsOwnModulesWording;
        procedure TheOtherModulesWordingIsNotBorrowed;
        procedure ItsDetachedSuffixComesFromTheSameModule;
        procedure AnIdNobodyClaimsIsReportedAsNotFound;
        procedure AndAnsweredWithNothingRatherThanSomethingBorrowed;
        procedure AnEmptyIdMatchesNothing;
        procedure NoModulesAtAllIsNotFound;
    end;

implementation

procedure TModuleMenuTest.SetUp;
begin
    SetLength(FDecls, 0);
    SetLength(FNodes, 0);
end;

procedure TModuleMenuTest.Declare(const AId, AParent, ACaption: string;
    AKind: TUiMenuKind; AChecked: boolean = False;
    ARadioGroup: longint = 0);
var
    D: TUiMenuDecl;
begin
    D := Default(TUiMenuDecl);
    D.Id := AId;
    D.Parent := AParent;
    D.Caption := ACaption;
    D.Hint := 'what ' + AId + ' does';
    D.Kind := AKind;
    D.Checked := AChecked;
    D.RadioGroup := ARadioGroup;
    SetLength(FDecls, Length(FDecls) + 1);
    FDecls[High(FDecls)] := D;
end;

procedure TModuleMenuTest.Build;
begin
    FNodes := ModuleMenuNodes(FDecls);
end;

function TModuleMenuTest.NodeOf(const AId: string): TModuleMenuNode;
var
    i: longint;
begin
    Result := Default(TModuleMenuNode);
    for i := 0 to High(FNodes) do
        if FNodes[i].Id = AId then
            Exit(FNodes[i]);
    Fail('no node with id ' + AId);
end;

{ ---- the module's own heading ---------------------------------------------- }

procedure TModuleMenuTest.TheModuleNameIsCapitalisedForItsMenu;
begin
    //  The name is the module's own - it is an identifier elsewhere - and the
    //  capital is the menu's convention.
    AssertEquals('capitalised', 'Patterns', ModuleRootCaption('patterns'));
end;

procedure TModuleMenuTest.ANameAlreadyCapitalisedIsUnchanged;
begin
    AssertEquals('unchanged', 'Patterns', ModuleRootCaption('Patterns'));
end;

procedure TModuleMenuTest.AnEmptyNameDoesNotFault;
begin
    //  The registry refuses a nameless module, so this cannot arrive - and a
    //  Copy on an empty string must still not fault if it ever does.
    AssertEquals('empty', '', ModuleRootCaption(''));
end;

procedure TModuleMenuTest.OnlyTheFirstLetterIsTouched;
begin
    //  A module whose name is deliberately cased - an abbreviation, a product
    //  name - must not be shouted or flattened.
    AssertEquals('the rest is left alone', 'PPatterns XY',
        ModuleRootCaption('pPatterns XY'));
end;

{ ---- what kind of widget each entry is ------------------------------------- }

procedure TModuleMenuTest.ACommandIsClickable;
begin
    Declare('act', '', 'Do it', mkCommand);
    Build;
    AssertTrue('clickable', NodeOf('act').Clickable);
end;

procedure TModuleMenuTest.ASubmenuIsNotClickable;
begin
    //  A submenu is opened, not chosen. Giving it a click handler runs a command
    //  when the user only meant to see what is inside.
    Declare('sub', '', 'More', mkSubmenu);
    Build;
    AssertFalse('not clickable', NodeOf('sub').Clickable);
end;

procedure TModuleMenuTest.ASeparatorGetsTheCaptionThatDrawsALine;
begin
    //  The widget set draws a line for this caption and for nothing else, so a
    //  separator that kept its declared caption would appear as an entry.
    Declare('sep', '', 'ignored', mkSeparator);
    Build;
    AssertEquals('a line', SeparatorCaption, NodeOf('sep').Caption);
end;

procedure TModuleMenuTest.ARadioEntryCarriesItsGroup;
begin
    //  The group is what makes the set mutually exclusive. Lost, every entry
    //  ticks independently and the user can select two of one thing.
    Declare('a', '', 'A', mkRadio, True, 7);
    Declare('b', '', 'B', mkRadio, False, 7);
    Build;
    AssertTrue('a radio', NodeOf('a').IsRadio);
    AssertEquals('in its group', 7, NodeOf('a').RadioGroup);
    AssertTrue('ticked', NodeOf('a').Checked);
    AssertFalse('and the other is not', NodeOf('b').Checked);
end;

procedure TModuleMenuTest.AToggleIsCheckableEvenWhenItStartsUnticked;
begin
    //  A MENU ENTRY IS ONE OF TWO DIFFERENT WIDGETS, and which it is is decided
    //  when its handle is made. A module's toggle is ticked from the polled
    //  state - which can land while the user is standing in the menu - and
    //  ticking a plain entry then destroys and rebuilds it under the pointer.
    Declare('tog', '', 'Show markers', mkToggle, False);
    Build;
    AssertTrue('checkable from birth', NodeOf('tog').Checkable);
    AssertFalse('but not ticked yet', NodeOf('tog').Checked);
end;

procedure TModuleMenuTest.ACommandIsNotCheckable;
begin
    Declare('act', '', 'Do it', mkCommand);
    Build;
    AssertFalse('not checkable', NodeOf('act').Checkable);
end;

procedure TModuleMenuTest.OnlyRadiosAndTogglesCanStartTicked;
begin
    //  A module that sets Checked on a command has said something meaningless;
    //  carrying it through would put a tick on an entry that can never lose it.
    Declare('act', '', 'Do it', mkCommand, True);
    Declare('sub', '', 'More', mkSubmenu, True);
    Build;
    AssertFalse('a command', NodeOf('act').Checked);
    AssertFalse('a submenu', NodeOf('sub').Checked);
end;

{ ---- what hangs from what -------------------------------------------------- }

procedure TModuleMenuTest.AnEntryWithNoParentHangsFromTheModuleRoot;
begin
    Declare('act', '', 'Do it', mkCommand);
    Build;
    AssertEquals('the root', -1, NodeOf('act').ParentIndex);
end;

procedure TModuleMenuTest.AnEntryNamingASubmenuHangsFromIt;
begin
    Declare('sub', '', 'More', mkSubmenu);
    Declare('act', 'sub', 'Do it', mkCommand);
    Build;
    AssertEquals('inside the submenu', 0, NodeOf('act').ParentIndex);
end;

procedure TModuleMenuTest.TwoEntriesCanShareASubmenu;
begin
    Declare('sub', '', 'More', mkSubmenu);
    Declare('a', 'sub', 'A', mkCommand);
    Declare('b', 'sub', 'B', mkCommand);
    Build;
    AssertEquals('the first', 0, NodeOf('a').ParentIndex);
    AssertEquals('and the second', 0, NodeOf('b').ParentIndex);
end;

procedure TModuleMenuTest.ASubmenuInsideASubmenuIsResolved;
begin
    Declare('outer', '', 'Outer', mkSubmenu);
    Declare('inner', 'outer', 'Inner', mkSubmenu);
    Declare('act', 'inner', 'Do it', mkCommand);
    Build;
    AssertEquals('the inner submenu is in the outer', 0,
        NodeOf('inner').ParentIndex);
    AssertEquals('and the entry is in the inner', 1,
        NodeOf('act').ParentIndex);
end;

{ ---- damage ---------------------------------------------------------------- }

procedure TModuleMenuTest.AnEntryNamingNoSuchSubmenuIsStillShown;
begin
    //  SHOWN, not dropped. A missing menu entry is invisible, and invisible is
    //  how a whole pack was once unreachable with nothing to say why.
    Declare('act', 'nonesuch', 'Do it', mkCommand);
    Build;
    AssertEquals('one entry', 1, Length(FNodes));
    AssertEquals('at the top level', -1, NodeOf('act').ParentIndex);
end;

procedure TModuleMenuTest.AnEntryNamingNoSuchSubmenuIsFlagged;
begin
    //  So that a caller CAN say so. Putting it at the top level silently makes
    //  a broken declaration look like a deliberate one.
    Declare('act', 'nonesuch', 'Do it', mkCommand);
    Build;
    AssertTrue('flagged', NodeOf('act').ParentWasMissing);
end;

procedure TModuleMenuTest.AnEntryDeclaredBeforeItsParentFallsToTheRoot;
begin
    //  A RULE ABOUT DECLARATION ORDER that a module author has no way to
    //  discover, pinned so that it is at least written down: a submenu declared
    //  after its children cannot be their parent.
    Declare('act', 'sub', 'Do it', mkCommand);
    Declare('sub', '', 'More', mkSubmenu);
    Build;
    AssertEquals('at the top level', -1, NodeOf('act').ParentIndex);
    AssertTrue('and flagged as misplaced', NodeOf('act').ParentWasMissing);
end;

procedure TModuleMenuTest.AnEntryNamingANonSubmenuAsItsParentFallsToTheRoot;
begin
    //  Only a submenu can hold entries. Naming a command as a parent is a
    //  module's mistake, and hanging entries off it would be the window's.
    Declare('cmd', '', 'A command', mkCommand);
    Declare('act', 'cmd', 'Do it', mkCommand);
    Build;
    AssertEquals('at the top level', -1, NodeOf('act').ParentIndex);
    AssertTrue('and flagged', NodeOf('act').ParentWasMissing);
end;

procedure TModuleMenuTest.AWellPlacedEntryIsNotFlagged;
begin
    Declare('sub', '', 'More', mkSubmenu);
    Declare('act', 'sub', 'Do it', mkCommand);
    Build;
    AssertFalse('the entry', NodeOf('act').ParentWasMissing);
    AssertFalse('and the submenu itself', NodeOf('sub').ParentWasMissing);
end;

{ ---- identity -------------------------------------------------------------- }

procedure TModuleMenuTest.EveryEntryKeepsItsId;
begin
    //  The id is what comes back on a click and is how the module recognises
    //  its own entry. Losing it makes every command reach the wrong handler.
    Declare('sub', '', 'More', mkSubmenu);
    Declare('act', 'sub', 'Do it', mkCommand);
    Build;
    AssertEquals('the submenu', 'sub', FNodes[0].Id);
    AssertEquals('the command', 'act', FNodes[1].Id);
end;

procedure TModuleMenuTest.EveryEntryKeepsItsHint;
begin
    Declare('act', '', 'Do it', mkCommand);
    Build;
    AssertEquals('the hint', 'what act does', NodeOf('act').Hint);
end;

procedure TModuleMenuTest.NoDeclarationsIsNoMenu;
begin
    //  A module may contribute a panel and no menu at all.
    Build;
    AssertEquals('nothing', 0, Length(FNodes));
end;

{ ---- whose words a panel speaks in ----------------------------------------- }

procedure TModulePanelTextTest.GivenTwoModules;
begin
    //  TWO, because one module cannot show that the right one was chosen. The
    //  mock names itself in both strings for the same reason.
    FA := TMockUiModule.Create('alpha');
    FB := TMockUiModule.Create('beta');
    SetLength(FAll, 2);
    FAll[0] := FA;
    FAll[1] := FB;
end;

procedure TModulePanelTextTest.TearDown;
begin
    //  The array holds interfaces; -SIcorba counts no references, so it is
    //  emptied before the objects go. See mock_support.
    SetLength(FAll, 0);
    FreeAndNil(FB);
    FreeAndNil(FA);
end;

procedure TModulePanelTextTest.APanelTakesItsOwnModulesWording;
begin
    GivenTwoModules;
    AssertTrue('found', PanelTextFor(FAll, 'beta.panel').Found);
    AssertEquals('beta''s own words', 'nothing to show in beta',
        PanelTextFor(FAll, 'beta.panel').EmptyText);
end;

procedure TModulePanelTextTest.TheOtherModulesWordingIsNotBorrowed;
begin
    //  NOT A CRASH - one module's panel explaining itself in another's
    //  language, which reads as the program having lost track of what it is
    //  showing.
    GivenTwoModules;
    AssertEquals('alpha''s own words', 'nothing to show in alpha',
        PanelTextFor(FAll, 'alpha.panel').EmptyText);
end;

procedure TModulePanelTextTest.ItsDetachedSuffixComesFromTheSameModule;
begin
    //  Both strings come from one module or the panel is half in each of two
    //  languages.
    GivenTwoModules;
    AssertEquals(' (detached from beta)',
        PanelTextFor(FAll, 'beta.panel').DetachedSuffix);
end;

procedure TModulePanelTextTest.AnIdNobodyClaimsIsReportedAsNotFound;
begin
    //  A window drawing a panel for a module that is no longer registered.
    //  Reported rather than guessed at.
    GivenTwoModules;
    AssertFalse('not found', PanelTextFor(FAll, 'gamma.panel').Found);
end;

procedure TModulePanelTextTest.AndAnsweredWithNothingRatherThanSomethingBorrowed;
begin
    //  Saying nothing beats saying something borrowed: an empty panel with no
    //  text is a bug the user reports, where a panel confidently explaining
    //  itself in the wrong module's words is one nobody notices.
    GivenTwoModules;
    AssertEquals('', PanelTextFor(FAll, 'gamma.panel').EmptyText);
    AssertEquals('', PanelTextFor(FAll, 'gamma.panel').DetachedSuffix);
end;

procedure TModulePanelTextTest.AnEmptyIdMatchesNothing;
begin
    //  A MODULE THAT DECLARES NO PANEL RETURNS '' FROM PanelId, so a blank id
    //  would otherwise match the first such module and borrow its wording -
    //  from a module that has no panel to have wording about.
    GivenTwoModules;
    AssertFalse('nothing matched', PanelTextFor(FAll, '').Found);
end;

procedure TModulePanelTextTest.NoModulesAtAllIsNotFound;
begin
    //  The published build, which ships no module. The window still draws.
    SetLength(FAll, 0);
    AssertFalse('not found', PanelTextFor(FAll, 'anything').Found);
end;

initialization
    //  A unit test: declarations in, structure out. No menu and no module.
    RegisterTest('unit', TModuleMenuTest);
    RegisterTest('unit', TModulePanelTextTest);
end.
