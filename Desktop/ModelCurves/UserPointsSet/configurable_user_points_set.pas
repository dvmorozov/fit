// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of TConfigurableUserPointsSet.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
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
{$IF NOT DEFINED(SERVER) AND NOT DEFINED(CLIENT_PROXY)}
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

uses
    Controls, Dialogs,
{$IF NOT DEFINED(SERVER) AND NOT DEFINED(CLIENT_PROXY)}
    create_user_points_set_dlg_adapter, curve_type_parameters_factory,
    curve_type_storage_adapter, expression_parser_adapter,
    user_curve_roles_dlg_adapter,
    //  The sequence itself, which is where every way through it is tested.
    user_curve_flow,
{$ENDIF}
    app;

class function TConfigurableUserPointsSet.HasConfigurableParameters: boolean;
begin
    Result := True;
end;

{$IF NOT DEFINED(SERVER) AND NOT DEFINED(CLIENT_PROXY)}
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
    Result := RunUserCurveFlow(
        TCreateUserPointsSetDlgAdapter.Create,
        TUserCurveRolesDlgAdapter.Create,
        TExpressionParserAdapter.Create,
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
