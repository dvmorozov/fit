// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Entries a module adds to its own group of Model > Curve Type.)

WHY A SEAM. Model > Curve Type is the window's: one group per owner of the
registered types (curve_type_menu). A module's types arrive there by
registering, but some of what a module offers under its group is not a type -
the scientific module's User group lists the formulas a user saved, with a
submenu to delete them, and those change while the program runs. They were
built by the window itself, which therefore named the module's dialogs and
files, and every product carried that code. A module now declares those
entries here, asked again on every rebuild of the menu, and acts through the
host it is handed (IUiHost) - so the window names none of it.

WHY NOT IUiModule. A UI module's entries hang from a menu of the module's own
under Model; these must sit inside the curve-type group they belong to, beside
the type that creates them, and must be asked again whenever that menu is
rebuilt. The declaration is the same (TUiMenuDecl, laid out by module_menu).

A CURVE TYPE SELECTED IS TOLD TO EVERY EXTENSION (CurveTypeSelected), because
which saved formula is current depends on it: choosing the formula type
creates one, choosing any other type leaves none current.
}
unit curve_type_menu_extension;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_ui_host, named_points_set;

type
    ICurveTypeMenuExtension = interface
        ['{6C3E9A52-1D47-4B8F-A0E6-5F2B7D19C483}']
        { The curve-type group whose submenu the entries are added to, as the
          group is captioned there. Entries for a group the build does not
          show are not asked for. }
        function Group: string;
        { The entries, below the group's types, asked on every rebuild. }
        function MenuItems: TUiMenuDeclArray;
        { One of them was chosen. }
        procedure Command(const AId: string; AHost: IUiHost);
        { The window selected ATypeId from the menu or the Tools list. }
        procedure CurveTypeSelected(const ATypeId: TCurveTypeId);
        { The formula the model is built with now, for a project's record of
          it; '' when none of this extension's is current. }
        function SelectedFormula: string;
    end;

    TCurveTypeMenuExtensions = array of ICurveTypeMenuExtension;

{ Registers an extension. Idempotent: the same object twice is once. }
procedure RegisterCurveTypeMenuExtension(AExtension: ICurveTypeMenuExtension);
function RegisteredCurveTypeMenuExtensions: TCurveTypeMenuExtensions;

{ The first formula an extension reports as current, or ''. }
function SelectedFormulaOf(const AExtensions: TCurveTypeMenuExtensions): string;

implementation

var
    Extensions: TCurveTypeMenuExtensions;

procedure RegisterCurveTypeMenuExtension(AExtension: ICurveTypeMenuExtension);
var
    i: longint;
begin
    if not Assigned(AExtension) then
        raise Exception.Create('a nil curve-type menu extension was registered');
    for i := 0 to High(Extensions) do
        if Extensions[i] = AExtension then
            Exit;
    SetLength(Extensions, Length(Extensions) + 1);
    Extensions[High(Extensions)] := AExtension;
end;

function RegisteredCurveTypeMenuExtensions: TCurveTypeMenuExtensions;
begin
    Result := Extensions;
end;

function SelectedFormulaOf(const AExtensions: TCurveTypeMenuExtensions): string;
var
    i: longint;
begin
    for i := 0 to High(AExtensions) do
    begin
        Result := AExtensions[i].SelectedFormula;
        if Result <> '' then
            Exit;
    end;
    Result := '';
end;

end.
