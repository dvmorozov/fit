{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of adapter class for ICreateUserPointsSetDlg.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CreateUserPointsSetDlgAdapter;

{$MODE Delphi}

interface

uses Classes, SysUtils, IntCreateUserPointsSetDlg, CBRCComponent;

type
    { Class-adapter implementing basic operations for creating user
      dialog for configuring parameters of custom curve type. }
    TCreateUserPointsSetDlgAdapter = class(TCBRCComponent, ICreateUserPointsSetDlg)
    private
        constructor Init;

    public
        class function Create: TCreateUserPointsSetDlgAdapter;

        function ShowModal: Integer;
        function GetExpression: string;
        function GetName: string;
    end;

implementation

uses CreateUserPointsSetDialog;

{ Class members aren't supported by Lazarus 0.9.24, global variable are used instead. }
var FCreateUserPointsSetDlgAdapter: TCreateUserPointsSetDlgAdapter;

constructor TCreateUserPointsSetDlgAdapter.Init;
begin
    inherited Create(nil);
end;

class function TCreateUserPointsSetDlgAdapter.Create: TCreateUserPointsSetDlgAdapter;
begin
    if FCreateUserPointsSetDlgAdapter = nil then
      FCreateUserPointsSetDlgAdapter := TCreateUserPointsSetDlgAdapter.Init;
    Result := FCreateUserPointsSetDlgAdapter;
end;

function TCreateUserPointsSetDlgAdapter.ShowModal: Integer;
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

end.


