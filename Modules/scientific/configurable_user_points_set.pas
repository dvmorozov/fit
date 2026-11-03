// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TConfigurableUserPointsSet.)

Copyright (C) Dmitry Morozov
}
unit configurable_user_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, configurable_points_set, SysUtils;

type
    { Special implementation used by TUserPointsSet. }
    TConfigurableUserPointsSet = class(TConfigurablePointsSet)
    public
      { Returns true if curve type has parameters which should be configured
        by user, otherwise returns false. }
        class function HasConfigurableParameters: boolean; override;
{$IF NOT DEFINED(FIT_NO_UI) AND NOT DEFINED(CLIENT_PROXY)}
      { Displays dialog for set up user configurable parameters. Returns true
        if dialog was confirmed and false if it was cancelled. }
        class function ShowConfigurationDialog: boolean; override;
{$ENDIF}
      { Returns true if user configurable parameters have default values,
        otherwise returns false. }
        class function HasDefaults: boolean; override;
      { Sets up default values for user configurable parameters. }
        class procedure SetDefaults; override;
    end;

implementation

{$IF NOT DEFINED(FIT_NO_UI) AND NOT DEFINED(CLIENT_PROXY)}
//  NOTHING OF THE WIDGET SET HERE, and nothing of the application: the windows
//  are the client's (Desktop/ScientificForms), found through the registry they
//  register in. This unit is compiled into the server and the test suites too,
//  which have no such windows.
uses
    curve_type_parameters_factory, curve_type_storage_adapter,
    user_curve_dialog_registry,
    //  The sequence itself, which is where every way through it is tested.
    user_curve_flow;
{$ENDIF}

class function TConfigurableUserPointsSet.HasConfigurableParameters: boolean;
begin
    Result := True;
end;

{$IF NOT DEFINED(FIT_NO_UI) AND NOT DEFINED(CLIENT_PROXY)}
{ WIRING, AND NOTHING ELSE. Which dialog comes next is user_curve_wizard's; what
  each answer implies - parse, create, store, delete - is user_curve_flow's; and
  both are reachable by a test because neither names a window. What is left here
  is the five real collaborators, named once.

  It used to be the sequence itself, twenty-seven lines that no test could reach
  because two of them opened modal dialogs - including the step that removes a
  draft type when the user rejects its roles, whose failure leaves an entry in
  their curve list that they never made. }
class function TConfigurableUserPointsSet.ShowConfigurationDialog: boolean;
begin
    //  A client without the windows defines nothing: as cancelled, which the
    //  window reports as "not created" (curve_type_choice).
    if not UserCurveDialogsRegistered then
        Exit(False);
    Result := RunUserCurveFlow(
        UserCurveFormulaDialog,
        UserCurveRolesDialog,
        UserCurveExpressionParser,
        TCurveTypeParametersFactory.Create,
        TCurveTypeStorageAdapter.Create);
end;

{$ENDIF}

class function TConfigurableUserPointsSet.HasDefaults: boolean;
begin
    Result := False;
end;

class procedure TConfigurableUserPointsSet.SetDefaults;
begin
    //  Do nothing.
end;

end.
