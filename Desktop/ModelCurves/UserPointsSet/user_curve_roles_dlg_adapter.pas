// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The roles dialog behind IUserCurveRolesDlg.)

THE MAPPING IS THE WHOLE UNIT: a modal result becomes one of three answers. It
lives here because this is the part that knows about windows, which is what lets
`user_curve_flow` name none of the widget set's constants and be tested with no
window at all.

The dialog is a global object with a public curve-type field, in the style the
rest of these adapters wrap. Setting that field and showing it are one step here,
so the caller can no longer do one without the other.
}
unit user_curve_roles_dlg_adapter;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, app_settings, int_user_curve_dialogs;

type
{$warnings off}
    TUserCurveRolesDlgAdapter = class(TInterfacedObject, IUserCurveRolesDlg)
    private
        constructor Init;
    public
        class function Create: IUserCurveRolesDlg;
        function Ask(ACurveType: Curve_type): TDialogAnswer;
    end;
{$warnings on}

implementation

uses
    Controls, user_points_set_prop_dialog;

{ A singleton, as the sibling adapters are: class members were not available in
  the Lazarus this code started on, and the pattern is kept so all of them read
  alike. }
var
    UserCurveRolesDlgAdapter: TUserCurveRolesDlgAdapter;

constructor TUserCurveRolesDlgAdapter.Init;
begin
    inherited;
end;

class function TUserCurveRolesDlgAdapter.Create: IUserCurveRolesDlg;
begin
    Result := IUserCurveRolesDlg(UserCurveRolesDlgAdapter);
end;

function TUserCurveRolesDlgAdapter.Ask(ACurveType: Curve_type): TDialogAnswer;
begin
    UserPointsSetPropDlg.FCurveType := ACurveType;
    case UserPointsSetPropDlg.ShowModal of
        mrOk: Result := daAccepted;
        //  RETRY IS "GO BACK", not "try this dialog again": the dialog offers it
        //  as a way to change the formula, which is the previous step. That
        //  reading is why the answer is named for what the user wants rather
        //  than for the button.
        mrRetry: Result := daStartAgain;
        else Result := daCancelled;
    end;
end;

initialization
    UserCurveRolesDlgAdapter := TUserCurveRolesDlgAdapter.Init;

finalization
    UserCurveRolesDlgAdapter.Free;

end.
