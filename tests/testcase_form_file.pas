// SPDX-License-Identifier: GPL-3.0-or-later
{ What the main window's form file wires together, read from the file.

  WHY THESE EXIST. A control bound to an action that is not declared is not an
  error anywhere: the reader leaves the binding unresolved, the button draws
  blank and does nothing. Two toolbar buttons, above the Curve Attributes and
  Summary tables, were bound to ActionSaveModelAsText for as long as it had not
  existed. The window cannot be built headless, so the file is read. }
unit testcase_form_file;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, menu_paths;

type
    TFormFileTest = class(TTestCase)
    published
        procedure EveryActionAControlNamesIsDeclared;
    end;

implementation

procedure TFormFileTest.EveryActionAControlNamesIsDeclared;
var
    Missing: TStringArray;
    Text: string;
    i: longint;
begin
    Missing := UndeclaredActionReferences;
    Text := '';
    for i := 0 to High(Missing) do
        Text := Text + ' ' + Missing[i];
    AssertEquals('controls bound to actions the form does not declare:' + Text,
        0, Length(Missing));
end;

initialization
    RegisterTest('unit', TFormFileTest);
end.
