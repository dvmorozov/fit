// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The contract between a feature module and the application's window.)

WHY AN INTERFACE AND NOT THE FORM. A module that reached into TFormMain would
depend on this particular window - its fields, its widgets, its widget set. The
window is a thin client of a 25-year-old design and is expected to be replaced;
a module written against it would have to be rewritten with it.

So a module is handed an IUiHost. The form implements it. Anything else that can
answer these calls - a different desktop shell, a web front end - can host the
same modules unchanged.

THE MODULE NAMES NO WIDGET. It DECLARES its menu as data - ids, captions, hints,
grouping - and the host builds whatever a menu is in that host. Commands come
back by id. Rows for a panel are plain records. Nothing here mentions the LCL,
and neither does a module written against it.

That is more isolation than the public/private split strictly needs, and it is
deliberate: it is the difference between a module surviving a UI replacement and
being rewritten by it.
}
unit int_ui_host;

{$mode objfpc}{$H+}

interface

uses
    Classes, module_view_types;

type
    TUiMessageKind = (umInfo, umWarning, umError);

    { What a menu entry is. The host decides how each is drawn. }
    TUiMenuKind = (
        mkCommand,     //  an action
        mkRadio,       //  one of a mutually exclusive set (RadioGroup)
        mkToggle,      //  on or off
        mkSubmenu,     //  a container for the entries naming it as Parent
        mkSeparator
        );

    { WHERE A COMMAND IS SHOWN. Defaults to the menu alone, so a module written
      before the Tools pane existed is unaffected by its arrival. }
    TCommandSurface = (
        csMenu,     //  the menu bar only
        csPane,     //  the Tools pane only
        csBoth      //  both
        );

    { WHAT A COMMAND APPLIES TO. A row-scoped command is offered on a
      right-click over the Model panel, with the selected row's id as AData;
      everything else is global. }
    TCommandScope = (scGlobal, scRow);

    TUiMenuDecl = record
        { Unique within the module. Comes back on Command, and addresses the
          entry for enabling or checking it later. }
        Id: string;
        { Empty for a top-level entry of this module, otherwise the Id of the
          submenu it belongs to. }
        Parent: string;
        Caption: string;
        Hint: string;
        Kind: TUiMenuKind;
        { Which mutually exclusive set an mkRadio entry belongs to. Ids are the
          module's own; the host maps them onto whatever it uses. }
        RadioGroup: longint;
        Checked: boolean;

        { Where this entry is shown. Defaults to csMenu, which is what a
          declaration written before the pane existed leaves it as. }
        Surface: TCommandSurface;
        { What it applies to. scRow puts it on the Model panel's context menu. }
        Scope: TCommandScope;
        { The pane heading it sits under; '' means the module's own name. }
        PaneGroup: string;
        { What it reads in the pane, which is narrower than a menu; '' means
          Caption. }
        ShortCaption: string;
    end;

    TUiMenuDeclArray = array of TUiMenuDecl;

    { What the application can do for a module. Extendable: a new method here is
      a new thing hosts must provide, so it is added when a second module needs
      it rather than in anticipation. }
    IUiHost = interface
        ['{7A2E5C90-3B41-4D68-95AF-6C0D2B84E317}']
        { Transient status text. }
        procedure ShowHint(const AText: string);
        { A message the user must acknowledge. }
        procedure ShowMessage(const ATitle, AText: string; AKind: TUiMessageKind);
        { A yes/no question. }
        function Confirm(const ATitle, AText: string): boolean;
        { A single line of input; False when the user cancelled. }
        function AskText(const ATitle, APrompt: string;
            var AValue: string): boolean;

        { Starts collecting picks into a named point set - the module's own,
          declared by its curve types (TNamedPointsSet.PlacedByPointSet). AHint
          says what to pick. Calling it again for the same set leaves the mode.

          AMenuId is the module's own toggle entry for the mode. The host keeps
          its check mark in step with the mode, the way it does for its own
          picking modes - and it has to, because the mode can end without the
          module hearing of it at all: another mode starts, a profile is loaded.
          A tick left behind then says the mode is on when it is off, and the
          next click on the entry reads as "leave" instead of "enter".

          APicksPerGesture is how many picks make ONE thing - two for a pair of
          bounds. The host ends the mode when they have been made, which is what
          puts the pick markers away and unticks the entry, so a finished gesture
          looks finished. Zero means the mode has no natural end and runs until
          the user leaves it. The module states it because only the module knows
          what its picks add up to. }
        procedure BeginPointPicking(const APointSet, AMenuId: string;
            APicksPerGesture: longint; const AHint: string);

        { State of one of this module's declared menu entries. }
        procedure SetMenuEnabled(const AId: string; AEnabled: boolean);
        procedure SetMenuChecked(const AId: string; AChecked: boolean);

        { Fills the module's panel. An empty array clears it; the host shows the
          module's empty text instead of a blank box. }
        procedure ShowModulePanel(const APanelId: string; const ARows: TOutline);
    end;

    { A feature module's contribution to the window. }
    IUiModule = interface
        ['{3E9B14C7-5A28-4D63-B0F1-7C4A2E80D5B6}']
        function Name: string;

        { The entries this module contributes, in the order they appear. }
        function MenuItems: TUiMenuDeclArray;
        { One of them was chosen. AData carries the entry's own payload for a
          radio group (the value it stands for), and is empty otherwise. }
        procedure Command(const AId, AData: string; AHost: IUiHost);

        { WHICH SHARED PANEL this module's structure rows go under - its own
          PlacedByPointSet value, and empty when it contributes none.

          IT NO LONGER BUYS THE MODULE A TAB. The Model panel belongs to the
          framework and there is one of it: it is always on the strip, it is
          captioned in the framework's words, and either contributor fills it -
          the framework for a model built from picks, whoever placed the markup
          for one built from that. So PanelCaption, PanelHint and
          PanelShouldBeVisible are gone from here: nothing consulted them once
          the panel stopped being a module's, and a method nothing calls is a
          method that rots. }
        function PanelId: string;
        { Shown when the panel is open and empty. Never blank: an empty box is
          indistinguishable from a broken one (D26). }
        function PanelEmptyText: string;
        { Appended to a row whose parent could not be found, so damage is
          visible rather than silently promoted to a root. }
        function PanelDetachedSuffix: string;
        { A row was chosen. }
        procedure PanelSelectionChanged(const ARowId, ARowText: string;
            AHost: IUiHost);
    end;

    TUiModuleArray = array of IUiModule;

{ Registers a module's UI. Idempotent by name. }
procedure RegisterUiModule(AModule: IUiModule);
function RegisteredUiModules: TUiModuleArray;
function UiModuleCount: longint;

implementation

uses
    SysUtils;

var
    Modules: TUiModuleArray;

function UiModuleCount: longint;
begin
    Result := Length(Modules);
end;

function RegisteredUiModules: TUiModuleArray;
begin
    Result := Modules;
end;

procedure RegisterUiModule(AModule: IUiModule);
var
    i: longint;
begin
    if not Assigned(AModule) then
        raise Exception.Create('a nil UI module was registered');
    if AModule.Name = '' then
        raise Exception.Create('a UI module was registered with no name');
    for i := 0 to High(Modules) do
        if Modules[i].Name = AModule.Name then
            //  Registered by every host that builds a window; twice is ordinary.
            Exit;

    SetLength(Modules, Length(Modules) + 1);
    Modules[High(Modules)] := AModule;
end;

end.
