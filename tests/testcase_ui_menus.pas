// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What ui_menus answers when there is no widget set - which is all of it
that can be reached from a test.)

WHY THIS FILE IS SHORT, AND WHY IT SAYS SO. `ui_menus` holds a real invariant
worth testing: an entry that is BOTH a submenu parent AND tickable is wrong
twice over - it is the entry whose widget the widget set destroys to give it a
check box, and that widget is the one holding the open submenu; and on its own
it is a question with no answer, because there is nothing sensible for a tick on
a submenu parent to mean. `MenuEntriesAtRiskOfDangling` is asserted at start-up
and must never miss one.

NONE OF THAT CAN BE DRIVEN HERE, AND IT WAS TRIED. The suite links the LCL but
never calls `Application.Initialize`, so `WidgetSet` is nil - and `TMenuItem`
reaches it in its own constructor. `TMenuItem.Create(nil)` faults with an access
violation at address 0 before any test code runs, and so does `TMainMenu`. There
is no menu to build, therefore no tree to walk and no entry to tick. The same
obstacle is recorded for the grid in curve_list_grid's header; this is the menu
half of it.

SO WHAT IS LEFT IS THE GUARD CLAUSES, and they are worth having: each is the
answer given when there is nothing to work on, and each is on the path the
start-up check takes before a form exists. They are also the answers that make
the rest of the application's "do it later" rule collapse into "do it now" if
they are ever wrong.

If someone makes the LCL usable in this binary - by initialising a widget set
for the suite - the tests this file could not carry are: ticking a checkable
entry and unticking it, the no-op when nothing changes, a non-checkable entry
still being ticked while no menu is open, and the six shapes
MenuEntriesAtRiskOfDangling has to sort (a plain menu, a tickable submenu
parent, a tickable leaf, a non-tickable parent, several offenders at once, and
one nested two deep).
}
unit testcase_ui_menus;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, ui_menus;

type
    TUiMenusTest = class(TTestCase)
    published
        procedure NoMenuIsOpenWithNoWidgetSet;
        procedure ANilEntryIsIgnoredRatherThanFatal;
        procedure AnOwnerWithNoMenusHasNothingAtRisk;
        procedure NoOwnerAtAllIsAnEmptyAnswerRatherThanAFault;
    end;

implementation

procedure TUiMenusTest.NoMenuIsOpenWithNoWidgetSet;
begin
    //  The only answer this can give outside GTK2, and the one every other
    //  platform has always effectively had. Worth pinning because the whole
    //  "do it later" rule turns into "do it now" when this is False: a build
    //  where it answered True by accident would defer every tick forever, and
    //  the menus would silently stop following the model.
    AssertFalse('nothing is open in a headless run', AMenuIsOpen);
end;

procedure TUiMenusTest.ANilEntryIsIgnoredRatherThanFatal;
begin
    //  Callers reach entries by name through the form, and a renamed or
    //  not-yet-created entry hands over nil. A tick is not worth a fault - and
    //  these arrive from the state poll twice a second, so a fault here would
    //  be a fault on a timer.
    SetMenuEntryChecked(nil, True);
    AssertTrue('returned rather than faulting', True);
end;

procedure TUiMenusTest.AnOwnerWithNoMenusHasNothingAtRisk;
var
    Owner: TComponent;
begin
    //  An empty answer is "nothing wrong", which is the honest reading of "no
    //  menus to examine" - the start-up check must not report a problem for a
    //  component that simply owns no menu.
    Owner := TComponent.Create(nil);
    try
        AssertEquals('nothing named', '', MenuEntriesAtRiskOfDangling(Owner));
    finally
        Owner.Free;
    end;
end;

procedure TUiMenusTest.NoOwnerAtAllIsAnEmptyAnswerRatherThanAFault;
begin
    //  The start-up check can run before the form is assigned, and faulting
    //  there would turn a diagnostic into the failure it exists to diagnose.
    AssertEquals('nothing named', '', MenuEntriesAtRiskOfDangling(nil));
end;

initialization
    //  Unit tests: no widget set is touched, which is precisely the limit
    //  described at the top of this file.
    RegisterTest('unit', TUiMenusTest);
end.
