{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of adapter class for ICreateUserPointsSetDlg.)

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
    Classes, int_create_user_points_set_dlg, SysUtils;

type
{$warnings off}
    { Class-adapter implementing basic operations for creating user
      dialog for configuring parameters of custom curve type. }
    TCreateUserPointsSetDlgAdapter = class(TInterfacedObject, ICreateUserPointsSetDlg)
    private
        constructor Init;

    public
        class function Create: ICreateUserPointsSetDlg;

        function ShowModal: integer;
        function GetExpression: string;
        function GetName: string;
    end;

{$warnings on}

implementation

uses
    create_user_points_set_dlg;

{ Class members aren't supported by Lazarus 0.9.24, global variable are used instead. }
var
    CreateUserPointsSetDlgAdapter: TCreateUserPointsSetDlgAdapter;

constructor TCreateUserPointsSetDlgAdapter.Init;
begin
    inherited;
end;

class function TCreateUserPointsSetDlgAdapter.Create: ICreateUserPointsSetDlg;
begin
    Result := ICreateUserPointsSetDlg(CreateUserPointsSetDlgAdapter);
end;

function TCreateUserPointsSetDlgAdapter.ShowModal: integer;
begin
    CreateUserPointsSetDlg.ActiveControl := CreateUserPointsSetDlg.EditExpression;
    Result := CreateUserPointsSetDlg.ShowModal;
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
