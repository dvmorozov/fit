// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The name-and-formula dialog behind IUserCurveFormulaDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit create_user_points_set_dlg_adapter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, int_user_curve_dialogs, SysUtils;

type
{$warnings off}
    { Class-adapter implementing basic operations for creating user
      dialog for configuring parameters of custom curve type. }
    { ANSWERS ONE OF THREE NAMED ANSWERS, not a modal result, so that
      user_curve_flow names no widget-set constant and can run with no window.
      Mapping the result is one line and it belongs here, because this is the
      part that knows about windows.

      It used to answer a modal result through ICreateUserPointsSetDlg as well.
      That interface had exactly one caller - the definition sequence - and when
      the sequence moved to the named answers it had none, so it is gone. }
    TCreateUserPointsSetDlgAdapter = class(TInterfacedObject,
        IUserCurveFormulaDlg)
    private
        constructor Init;

    public
        class function Create: IUserCurveFormulaDlg;

        function Ask: TDialogAnswer;
        function GetExpression: string;
        function GetName: string;
    end;

{$warnings on}

implementation

uses
    Controls, create_user_points_set_dlg;

{ Class members aren't supported by Lazarus 0.9.24, global variable are used instead. }
var
    CreateUserPointsSetDlgAdapter: TCreateUserPointsSetDlgAdapter;

constructor TCreateUserPointsSetDlgAdapter.Init;
begin
    inherited;
end;

class function TCreateUserPointsSetDlgAdapter.Create: IUserCurveFormulaDlg;
begin
    Result := IUserCurveFormulaDlg(CreateUserPointsSetDlgAdapter);
end;

function TCreateUserPointsSetDlgAdapter.Ask: TDialogAnswer;
begin
    CreateUserPointsSetDlg.ActiveControl := CreateUserPointsSetDlg.EditExpression;
    //  TWO ANSWERS ONLY. There is no earlier step for this dialog to send the
    //  user back to, so anything but a confirmation abandons the definition.
    if CreateUserPointsSetDlg.ShowModal = mrOk then
        Result := daAccepted
    else
        Result := daCancelled;
end;

function TCreateUserPointsSetDlgAdapter.GetExpression: string;
begin
    Result := CreateUserPointsSetDlg.EditExpression.Text;
end;

function TCreateUserPointsSetDlgAdapter.GetName: string;
begin
    Result := CreateUserPointsSetDlg.EditCurveName.Text;
end;

initialization
    CreateUserPointsSetDlgAdapter := TCreateUserPointsSetDlgAdapter.Init;

finalization
    CreateUserPointsSetDlgAdapter.Free;

end.
