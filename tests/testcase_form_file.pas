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
        procedure TheRolesDialogCallsSharedParametersShared;
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

procedure TFormFileTest.TheRolesDialogCallsSharedParametersShared;
var
    Caption: string;
begin
    //  THE REPORT: the list in Set Curve Type Properties was labelled "Fixed
    //  parameters", and ticking one made it SHARED - one value for every curve
    //  of an interval, which the fit still varies. Only a position ticked there
    //  is held where it is placed. The label says what ticking does.
    Caption := DesignedCaption(
        'Desktop/ModelCurves/UserPointsSet/user_points_set_prop_dialog.lfm',
        'LabelFixedParameters');
    AssertTrue('the label is read', Caption <> '');
    AssertTrue('it says shared: ' + Caption, Pos('Shared', Caption) > 0);
    AssertTrue('and not fixed: ' + Caption, Pos('Fixed', Caption) = 0);
end;

initialization
    RegisterTest('unit', TFormFileTest);
end.
