// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The two things the LCL does not do for a menu bar on macOS.)

WHAT THE WIDGET SET ALREADY DOES, and must therefore NOT be written here. On
Cocoa the LCL lifts TMainMenu into the screen menu bar and inserts an
application menu of its own in front of it, carrying Services, Hide @italic(Fit)
(Cmd-H), Hide Others, Show All and Quit @italic(Fit) (Cmd-Q) with a working
terminate. All of that is in the widget set
(lcl/interfaces/cocoa/cocoamenus.pas, TCocoaMenuItem.attachAppleMenuItems) and
none of it is the application's business. Note that the menu bar only appears at
all when the process is a FOREGROUND application, which means it has to run from
an .app bundle - see New-MacRunShell in the build scripts.

WHAT IT LEAVES UNDONE, and what this unit is:

  1. THE SHORTCUTS ARE THE WRONG KEY. The menu was written once, for every
     platform, with Ctrl accelerators - Ctrl-O, Ctrl-S, Ctrl-Q. Cocoa's shortcut
     translation maps ssCtrl to the CONTROL key and ssMeta to Command
     (cocoamenus.ShortcutToKeyEquivalent), so on a Mac every one of those
     accelerators is on the wrong key and Cmd-O does nothing. Rewritten here as
     ONE pass over the action list rather than as forty-five edits to the form,
     so the menu keeps saying one thing on all three platforms and the
     translation lives in one place.

  2. THERE IS NO About IN THE APPLICATION MENU. The widget set inserts one only
     if the application hands it a menu item to insert - CocoaConfigMenu's
     appMenu.aboutItem - and it takes the item as it is, without moving it out of
     wherever else it appears. So the item given to it is a STANDALONE one owned
     by the form and never added to the menu bar, and the Help entry it doubles
     is hidden, which leaves About exactly where a Mac user looks for it and
     nowhere else.

Everything here compiles on every platform: the pure part (@link(MacShortCut))
because the test suite checks it wherever the suite runs, and the rest as an
empty body, so no caller needs an IFDEF around it.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit mac_menu;

{$mode objfpc}{$H+}

interface

uses
    Classes, Menus, ActnList;

{ The same accelerator, said in the platform's own modifier: Ctrl becomes
  Command, and everything else - Shift, Alt, the key itself, and an accelerator
  that is already on Command - is returned untouched.

  WHY NOT SIMPLY ADD ssMeta: because Ctrl-S would then mean
  Control-Command-S. The modifier is REPLACED, which is what makes Cmd-S the
  shortcut a Mac user expects, and it is a translation of the same statement
  rather than a second statement to keep in step.

  Pure, and compiled everywhere, so the suite can check it on the machine it
  happens to be running on. }
function MacShortCut(AShortCut: TShortCut): TShortCut;

{ Applies both conventions to a window's menu. Does nothing at all unless this
  is a Cocoa build, so the caller needs no conditional.

  AAboutItem is the menu bar's own About entry - the one in Help. It is hidden,
  and a standalone copy of it (owned by AOwner, never added to a menu) is handed
  to the application menu. Pass nil for it, and the application menu simply has
  no About - which is what happens today.

  CALL IT FROM FormCreate. The application menu is built once, when the menu's
  handle is created, and it latches: anything handed to CocoaConfigMenu after
  that point is never read. }
procedure ApplyMacMenuConventions(AOwner: TComponent; AMenu: TMainMenu;
    AActions: TActionList; AAboutItem: TMenuItem);

implementation

uses
    LCLType
{$IFDEF LCLCocoa}
    //  The record the widget set reads the application menu's items out of.
    , CocoaConfig
{$ENDIF}
    ;

function MacShortCut(AShortCut: TShortCut): TShortCut;
var
    Key: word;
    Shift: TShiftState;
begin
    if AShortCut = 0 then
    begin
        Result := AShortCut;
        Exit;
    end;
    ShortCutToKey(AShortCut, Key, Shift);
    if not (ssCtrl in Shift) then
    begin
        Result := AShortCut;
        Exit;
    end;
    Shift := Shift - [ssCtrl] + [ssMeta];
    Result := Menus.ShortCut(Key, Shift);
end;

{$IFDEF LCLCocoa}
{ Every accelerator the window declares, in the two places one can be declared:
  on an action (which is where all but a handful of this window's live) and on a
  menu item that carries its own. An item with an action is left alone - its
  accelerator IS the action's, and rewriting it here would write the same
  translation twice. }
procedure TranslateItemShortCuts(AItem: TMenuItem);
var
    i: integer;
begin
    if AItem = nil then Exit;
    if (AItem.Action = nil) and (AItem.ShortCut <> 0) then
        AItem.ShortCut := MacShortCut(AItem.ShortCut);
    for i := 0 to AItem.Count - 1 do
        TranslateItemShortCuts(AItem.Items[i]);
end;

procedure TranslateShortCuts(AMenu: TMainMenu; AActions: TActionList);
var
    i: integer;
    Action: TCustomAction;
begin
    if AActions <> nil then
        for i := 0 to AActions.ActionCount - 1 do
            if AActions.Actions[i] is TCustomAction then
            begin
                Action := TCustomAction(AActions.Actions[i]);
                Action.ShortCut := MacShortCut(Action.ShortCut);
            end;
    if AMenu <> nil then
        for i := 0 to AMenu.Items.Count - 1 do
            TranslateItemShortCuts(AMenu.Items[i]);
end;

{ The standalone item the application menu is given. A copy rather than the Help
  entry itself: the widget set builds a second native item for whatever it is
  handed and leaves the original where it is, so handing it the Help entry would
  put About in two places. }
function NewAppMenuAboutItem(AOwner: TComponent; ASource: TMenuItem): TMenuItem;
begin
    Result := TMenuItem.Create(AOwner);
    Result.Name := 'MenuAboutAppMenu';
    //  Cocoa retitles it "About <Application.Title>" itself; the caption is set
    //  anyway, because an item with none is a defect on any other path.
    Result.Caption := ASource.Caption;
    //  The ACTION, where there is one, so the item is enabled and disabled by
    //  the same rule as the entry it replaces. OnClick only when there is not.
    if ASource.Action <> nil then
        Result.Action := ASource.Action
    else
        Result.OnClick := ASource.OnClick;
end;
{$ENDIF}

procedure ApplyMacMenuConventions(AOwner: TComponent; AMenu: TMainMenu;
    AActions: TActionList; AAboutItem: TMenuItem);
{$IFDEF LCLCocoa}
begin
    TranslateShortCuts(AMenu, AActions);
    if AAboutItem <> nil then
    begin
        CocoaConfigMenu.appMenu.aboutItem := NewAppMenuAboutItem(AOwner, AAboutItem);
        //  Hidden, not destroyed: the item is an action's menu entry, the command
        //  table resolves it by name (ui_commands), and the panel that mirrors
        //  the menu still has to find it.
        AAboutItem.Visible := False;
    end;
end;
{$ELSE}
begin
    //  Not a Cocoa build: the menu bar is in the window, Ctrl is Ctrl, and there
    //  is no application menu to put anything in.
end;
{$ENDIF}

end.
