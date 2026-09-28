// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The menu a module's declarations describe.)

A MODULE NAMES NO WIDGET. It declares its menu as data - ids, captions, hints,
kinds, which submenu each entry belongs to - and the window builds whatever a
menu is in that window. That is the whole point of `int_ui_host`: a module
written against it survives the interface being replaced.

Which makes the translation from declarations to structure a decision, and it was
inside the method that creates `TMenuItem`s. The framework ships no module, so it
had never been run against anything but the one pack that exists, in the one
order that pack declares its entries in.

WHAT CAN GO WRONG. An entry whose parent was never declared is still SHOWN, at
the top level, rather than dropped - a missing menu entry is invisible, and
invisible is how a whole pack was once unreachable. An entry declared before the
submenu it names lands at the top level for the same reason, which is a rule
about declaration ORDER that a module author has no way to discover.
}
unit module_menu;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_ui_host;

const
    { What a separator's caption has to be for the widget set to draw a line. }
    SeparatorCaption = '-';

type
    { One entry of the menu, with its parent resolved to a position in the same
      array and everything the window needs to make a widget of it. }
    TModuleMenuNode = record
        { The module's own id, which comes back on a click. }
        Id: string;
        Caption: string;
        Hint: string;
        Kind: TUiMenuKind;
        { Position of this node's parent in the same array, or -1 when it hangs
          from the module's own root. }
        ParentIndex: longint;
        { True when the entry named a parent that no submenu declares - or names
          one declared later. Shown at the top level, and flagged so a caller
          can say so. }
        ParentWasMissing: boolean;
        { A radio entry belongs to a mutually exclusive set. }
        IsRadio: boolean;
        RadioGroup: longint;
        { A toggle is declared checkable whether or not it starts ticked: it is
          ticked from the polled state too, and it can be ticked while the menu
          is open. }
        Checkable: boolean;
        Checked: boolean;
        { Everything but a submenu answers a click. }
        Clickable: boolean;
        { False when the declaration asked for the entry to be shown greyed. }
        Enabled: boolean;
        { The explanation the entry stands for, or ''. }
        Topic: string;
    end;

    TModuleMenuNodes = array of TModuleMenuNode;

{ How a module's own name appears at the head of its menu. The name is the
  module's; the capital is the menu's. }
function ModuleRootCaption(const AName: string): string;

{ The menu the declarations describe. }
function ModuleMenuNodes(const ADecls: TUiMenuDeclArray): TModuleMenuNodes;

type
    { Indexes into a TUiModuleArray, in the order they are to be asked. }
    TModuleIndexes = array of longint;

{ Whose RowMenuItems to ask over a row of the panel APanelId, in order.

  Only the owner, over a module's own rows. EVERY module, over rows no module
  claims - the framework's own - because a curve of the model may still be one a
  module placed, and only that module can say what it offers over it; a module
  answers nothing over a curve it does not own, so the host takes the first
  answer. Asking nobody there was the defect: a count sits beside the other
  curves while a type placed by picks is selected, and right-clicking a pattern
  offered no decompose menu. Nobody is asked over a panel with no id. }
function RowMenuModulesToAsk(const AModules: TUiModuleArray;
    const APanelId: string): TModuleIndexes;

{ Which module's RowMenuItems to ask over the rows of the panel APanelId, or -1
  when no module filled them - the framework's own rows, an empty id, no
  modules. }
function RowMenuModuleIndex(const AModules: TUiModuleArray;
    const APanelId: string): longint;

type
    { The wording a panel needs, which belongs to the module that declared it
      and not to the window drawing it. }
    TModulePanelText = record
        { False when no registered module claims that panel id. }
        Found: boolean;
        { Shown INSTEAD of an empty box, because a blank panel is
          indistinguishable from a broken one. }
        EmptyText: string;
        { Appended to a row whose parent could not be found, so damage stays
          visible rather than being silently promoted to a root. }
        DetachedSuffix: string;
    end;

{ The wording for a panel, from whichever registered module declares it.

  A LOOKUP RATHER THAN A FIELD ON THE WINDOW, because the window draws panels it
  does not own: the id it is handed is the only thing tying the rows it is about
  to draw to the module whose words describe them. Getting the wrong module's
  wording is not a crash - it is one module's panel explaining itself in
  another's language, which reads as the program having lost track of what it is
  showing.

  Found is False when nothing claims the id, and the strings are then empty:
  that is a window drawing a panel for a module that is no longer registered,
  and saying nothing is better than saying something borrowed. }
function PanelTextFor(const AModules: TUiModuleArray;
    const APanelId: string): TModulePanelText;

implementation

function PanelTextFor(const AModules: TUiModuleArray;
    const APanelId: string): TModulePanelText;
var
    i: longint;
begin
    Result := Default(TModulePanelText);
    //  AN EMPTY ID MATCHES NOTHING, deliberately: a module that declares no
    //  panel returns '' from PanelId, so a blank id would otherwise match the
    //  first module that has no panel at all and borrow its wording.
    if APanelId = '' then
        Exit;
    for i := 0 to High(AModules) do
        if AModules[i].PanelId = APanelId then
        begin
            Result.Found := True;
            Result.EmptyText := AModules[i].PanelEmptyText;
            Result.DetachedSuffix := AModules[i].PanelDetachedSuffix;
            Exit;
        end;
end;

function RowMenuModuleIndex(const AModules: TUiModuleArray;
    const APanelId: string): longint;
var
    i: longint;
begin
    Result := -1;
    //  AN EMPTY ID ASKS NOBODY, for the reason PanelTextFor gives: a module
    //  with no panel answers '' and would otherwise be taken for the owner.
    if APanelId = '' then
        Exit;
    for i := 0 to High(AModules) do
        if AModules[i].PanelId = APanelId then
            Exit(i);
end;

function ModuleRootCaption(const AName: string): string;
begin
    Result := UpperCase(Copy(AName, 1, 1)) + Copy(AName, 2, MaxInt);
end;

function ModuleMenuNodes(const ADecls: TUiMenuDeclArray): TModuleMenuNodes;
var
    i, j: longint;

    { The position of the submenu declared under AId, or -1.

      SEARCHED AMONG THE ENTRIES ALREADY PLACED, which is what makes this a rule
      about declaration order: a submenu declared after its children cannot be
      their parent. }
    function SubmenuBefore(ABefore: longint; const AId: string): longint;
    var
        k: longint;
    begin
        Result := -1;
        if AId = '' then
            Exit;
        for k := 0 to ABefore - 1 do
            if (ADecls[k].Kind = mkSubmenu) and (ADecls[k].Id = AId) then
                Result := k;
    end;

begin
    SetLength(Result, Length(ADecls));
    for i := 0 to High(ADecls) do
    begin
        Result[i].Id := ADecls[i].Id;
        Result[i].Kind := ADecls[i].Kind;
        Result[i].Hint := ADecls[i].Hint;
        Result[i].RadioGroup := ADecls[i].RadioGroup;

        if ADecls[i].Kind = mkSeparator then
            //  The widget set draws a line for this caption and nothing else.
            Result[i].Caption := SeparatorCaption
        else
            Result[i].Caption := ADecls[i].Caption;

        Result[i].IsRadio := ADecls[i].Kind = mkRadio;
        Result[i].Checkable := ADecls[i].Kind = mkToggle;
        Result[i].Checked := (ADecls[i].Kind in [mkRadio, mkToggle]) and
            ADecls[i].Checked;
        //  A submenu is opened, not chosen.
        Result[i].Clickable := ADecls[i].Kind <> mkSubmenu;
        //  Greyed when asked for, and still built: an entry that vanishes
        //  teaches nothing, one that is greyed with its topic teaches the rule.
        Result[i].Enabled := not ADecls[i].Disabled;
        Result[i].Topic := ADecls[i].Topic;

        if ADecls[i].Parent = '' then
        begin
            Result[i].ParentIndex := -1;
            Result[i].ParentWasMissing := False;
        end
        else
        begin
            j := SubmenuBefore(i, ADecls[i].Parent);
            Result[i].ParentIndex := j;
            //  STILL SHOWN, at the top level. A missing menu entry is
            //  invisible, and invisible is how a whole pack was once
            //  unreachable - so damage is put where it can be seen.
            Result[i].ParentWasMissing := j < 0;
        end;
    end;
end;

function RowMenuModulesToAsk(const AModules: TUiModuleArray;
    const APanelId: string): TModuleIndexes;
var
    Owner, i: longint;
begin
    Result := nil;
    if APanelId = '' then
        Exit;
    Owner := RowMenuModuleIndex(AModules, APanelId);
    if Owner >= 0 then
    begin
        SetLength(Result, 1);
        Result[0] := Owner;
        Exit;
    end;
    SetLength(Result, Length(AModules));
    for i := 0 to High(AModules) do
        Result[i] := i;
end;

end.
