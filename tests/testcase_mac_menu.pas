// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The accelerator translation that makes the menu read Cmd on a Mac.)

WHY THIS IS TESTED WHEREVER THE SUITE RUNS, macOS or not. `MacShortCut` is the
whole of the rule - the window declares Ctrl accelerators once, for all three
platforms, and Cocoa maps ssCtrl to the CONTROL key, so something has to say
that Ctrl means Command here. Written as a pure function precisely so that the
rule can be checked in a headless binary on any platform: the LCL is linked but
never initialised in this suite (see testcase_ui_menus for what that costs), so
a test that had to build a TMainMenu to check the rule could not exist at all.

WHAT THE FUNCTION MUST NOT DO, and each of these is a test below: add Command
while leaving Control in place (Ctrl-S would become Control-Command-S), lose
Shift or Alt from a two-modifier accelerator, change the key itself, touch an
accelerator that is a bare function key - F2 and F5 run fits and are not
modified on any platform - or turn "no accelerator at all" into one.
}
unit testcase_mac_menu;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, Menus, LCLType, mac_menu;

type
    TMacMenuTest = class(TTestCase)
    private
        { The accelerator's two halves, so a failure says which one moved. A
          method rather than a plain procedure: AssertEquals and AssertTrue are
          TAssert's, and nothing outside a test case can call them unqualified. }
        procedure AssertShortCut(const AMessage: string; AExpectedKey: word;
            AExpectedShift: TShiftState; AActual: TShortCut);
    published
        procedure CtrlBecomesCommand;
        procedure ControlIsReplacedRatherThanAdded;
        procedure ShiftIsKeptAlongsideCommand;
        procedure AltIsKeptAlongsideCommand;
        procedure AFunctionKeyIsLeftAlone;
        procedure AnAcceleratorAlreadyOnCommandIsLeftAlone;
        procedure NoAcceleratorStaysNoAccelerator;
    end;

implementation

procedure TMacMenuTest.AssertShortCut(const AMessage: string; AExpectedKey: word;
    AExpectedShift: TShiftState; AActual: TShortCut);
var
    Key: word;
    Shift: TShiftState;
begin
    ShortCutToKey(AActual, Key, Shift);
    AssertEquals(AMessage + ': key', AExpectedKey, Key);
    AssertTrue(AMessage + ': modifiers', AExpectedShift = Shift);
end;

procedure TMacMenuTest.CtrlBecomesCommand;
begin
    //  Ctrl-O, File > Open Project - the accelerator a Mac user presses as Cmd-O
    //  and which did nothing at all before this translation existed.
    AssertShortCut('Ctrl-O becomes Cmd-O', VK_O, [ssMeta],
        MacShortCut(ShortCut(VK_O, [ssCtrl])));
end;

procedure TMacMenuTest.ControlIsReplacedRatherThanAdded;
var
    Key: word;
    Shift: TShiftState;
begin
    //  THE MISTAKE THIS GUARDS: `Shift + [ssMeta]` instead of
    //  `Shift - [ssCtrl] + [ssMeta]` leaves Control in place, and Ctrl-S then
    //  means Control-Command-S - an accelerator nobody can press by accident and
    //  nobody will press on purpose.
    ShortCutToKey(MacShortCut(ShortCut(VK_S, [ssCtrl])), Key, Shift);
    AssertFalse('Control is gone', ssCtrl in Shift);
    AssertTrue('Command is there', ssMeta in Shift);
end;

procedure TMacMenuTest.ShiftIsKeptAlongsideCommand;
begin
    //  Ctrl-Shift-S, File > Save Project As.
    AssertShortCut('Ctrl-Shift-S becomes Cmd-Shift-S', VK_S, [ssShift, ssMeta],
        MacShortCut(ShortCut(VK_S, [ssShift, ssCtrl])));
end;

procedure TMacMenuTest.AltIsKeptAlongsideCommand;
begin
    AssertShortCut('Ctrl-Alt-X becomes Cmd-Alt-X', VK_X, [ssAlt, ssMeta],
        MacShortCut(ShortCut(VK_X, [ssAlt, ssCtrl])));
end;

procedure TMacMenuTest.AFunctionKeyIsLeftAlone;
begin
    //  F5 minimises the difference and F2 loads a profile. They carry no
    //  modifier on any platform, and a translation that invented one would move
    //  the two commands the window is actually used for.
    AssertShortCut('F5 is unchanged', VK_F5, [], MacShortCut(ShortCut(VK_F5, [])));
end;

procedure TMacMenuTest.AnAcceleratorAlreadyOnCommandIsLeftAlone;
begin
    //  Applying the translation twice must be the same as applying it once: the
    //  function runs over the action list, and nothing guarantees that list is
    //  walked exactly once for the life of a window.
    AssertShortCut('Cmd-O stays Cmd-O', VK_O, [ssMeta],
        MacShortCut(MacShortCut(ShortCut(VK_O, [ssCtrl]))));
end;

procedure TMacMenuTest.NoAcceleratorStaysNoAccelerator;
begin
    //  Most menu entries have none, and 0 is what "none" is. Turning it into
    //  Command-<nothing> would claim an accelerator on the empty key.
    AssertEquals('0 is not an accelerator', 0, MacShortCut(0));
end;

initialization
    //  Unit test: a pure function, no widget set touched - which is why it can
    //  run on a Linux or Windows machine as well as on the Mac it is about.
    RegisterTest('unit', TMacMenuTest);
end.
