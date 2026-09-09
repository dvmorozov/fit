// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What may and may not be done to a menu while the user is standing in
one.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794,
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit ui_menus;

{$mode objfpc}{$H+}

interface

uses
    Classes, Menus;

{ True while a menu is dropped down - the menu bar, a submenu of it, or a popup.

  WHY ANYONE NEEDS TO ASK. An open menu is a MODAL GRAB the application did not
  open and does not own: the widget set holds the pointer and the keyboard, and
  it hides the menu again when the user picks something or presses Escape. Two
  things break that, and both leave a submenu painted on screen belonging to
  nothing - no pointer hides it, no Escape reaches it, and clicking one of its
  entries runs a command nobody asked for:

    * DESTROYING A MENU ENTRY that is on screen. The widget set does this by
      itself when an entry has to change into a different widget (see
      TFormMain.DeclareCheckableMenuEntries), and this application does it
      wholesale whenever it rebuilds a menu from the model behind it.
    * OPENING A WINDOW OVER IT - a dialog, a message box - which takes the grab
      away from a menu that is still up.

  Neither is a thing the user asked for at that moment: both arrive from a timer,
  from a queued call, or from the calculation thread, at whatever instant the
  main loop happened to reach them. So the rule is not "do it carefully" but DO
  IT LATER: ask this first, and if a menu is open, leave the work for after it
  closes. The user is holding the menu open for a fraction of a second; nothing
  polled twice a second is worth taking it away from them.

  ONLY GTK2 CAN ANSWER, and only GTK2 needs to: it is the widget set that
  recreates a menu entry to give it a check box. Everywhere else this answers
  False, which is what it has always effectively been - the behaviour before this
  unit existed. }
function AMenuIsOpen: boolean;

{ Ticks or unticks a menu entry, and never at the cost of the menu the user is
  looking at.

  An entry that is not a check item cannot be ticked without the widget set
  replacing the widget - so while a menu is open, such a tick is skipped rather
  than performed. Skipping it is safe for the ticks that arrive from the state
  poll, which is where nearly all of them come from: the next poll applies it.

  It is also a declaration defect - the entry should have been declared checkable
  before it ever had a handle - so it is named in the log rather than passed over
  in silence. }
procedure SetMenuEntryChecked(AItem: TMenuItem; AChecked: boolean);

{ The entries that are BOTH a submenu parent AND tickable, named, over every menu
  AOwner owns. Empty when there are none, which is the invariant.

  Such an entry is the worst case of the first hazard above: the widget the
  widget set destroys to give it a check box is the one holding the open submenu.
  It is also, on its own, a question with no answer - what would ticking a
  submenu parent do? - so the entry is wrong twice and this is worth stating in
  one place and checking at startup. }
function MenuEntriesAtRiskOfDangling(AOwner: TComponent): string;

implementation

uses
    SysUtils, log
{$IFDEF LCLGtk2}
    , gtk2
{$ENDIF}
    ;

function AMenuIsOpen: boolean;
{$IFDEF LCLGtk2}
var
    Grabbed: PGtkWidget;
begin
    //  A dropped-down menu grabs; the widget holding the grab is the menu shell
    //  itself (GtkMenu for a submenu or a popup, GtkMenuBar for the bar). A
    //  modal dialog grabs too, which is why the grab is not enough on its own
    //  and its widget has to be asked what it is.
    Grabbed := gtk_grab_get_current;
    Result := (Grabbed <> nil) and GTK_IS_MENU_SHELL(Grabbed);
end;
{$ELSE}
begin
    Result := False;
end;
{$ENDIF}

procedure SetMenuEntryChecked(AItem: TMenuItem; AChecked: boolean);
begin
    if not Assigned(AItem) then
        Exit;
    if AItem.Checked = AChecked then
        Exit;

    if not AItem.IsCheckItem then
    begin
        WriteLog('menu entry "' + AItem.Name + '" (' + AItem.Caption +
            ') is ticked but was not declared checkable', Warning);
        if AMenuIsOpen then
            //  The widget set would destroy it here, under an open menu. The
            //  tick is worth less than the menu.
            Exit;
    end;

    AItem.Checked := AChecked;
end;

{ Names the offending entries under AItem, itself included. }
procedure CollectRisks(AItem: TMenuItem; var AFound: string);
var
    i: longint;
begin
    if AItem.Count = 0 then
        Exit;

    if AItem.IsCheckItem then
    begin
        if AFound <> '' then
            AFound := AFound + ', ';
        AFound := AFound + AItem.Name;
    end;

    for i := 0 to AItem.Count - 1 do
        CollectRisks(AItem.Items[i], AFound);
end;

function MenuEntriesAtRiskOfDangling(AOwner: TComponent): string;
var
    i: longint;
begin
    Result := '';
    if not Assigned(AOwner) then
        Exit;
    //  Every menu of the component, not a list of menus given by hand: a menu
    //  added later - the chart's popup was once added later - is covered by
    //  having been created at all.
    for i := 0 to AOwner.ComponentCount - 1 do
        if AOwner.Components[i] is TMenu then
            CollectRisks(TMenu(AOwner.Components[i]).Items, Result);
end;

end.
