unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RTTIGrids,
  ExtCtrls, StdCtrls, user_service_intf, Buttons, ActnList;

type

  { TfMain }

  TfMain = class(TForm)
    actDelete: TAction;
    actSearch: TAction;
    actUpdate: TAction;
    actNew: TAction;
    AL: TActionList;
    btnAdd1: TButton;
    btnAdd2: TButton;
    btnSearch: TButton;
    btnAdd: TButton;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Grid: TTIGrid;
    procedure actDeleteExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure actUpdateUpdate(Sender: TObject);
    procedure GridGetObject(Sender: TTICustomGrid; Index: integer;
      var TIObject: TPersistent);
    procedure GridGetObjectCount(Sender: TTICustomGrid; ListObject: TObject;
      var ObjCount: integer);
    procedure GridGetObjectName(Sender: TObject; Index: integer;
      TIObject: TPersistent; var ObjName: string);
  private
    FUserService : UserService;
    function CreateObj():UserService;
    function GetObjectIndex() : Integer;
  public
    FUsers : TUserArray;
  end; 

var
  fMain: TfMain;

implementation
uses user_service_intf_proxy, synapse_tcp_protocol, synapse_http_protocol,
     soap_formatter, binary_formatter, user_edit_imp ;

{ TfMain }

procedure TfMain.GridGetObjectCount(Sender: TTICustomGrid;
  ListObject: TObject; var ObjCount: integer);
begin
  if ( FUsers = nil ) then
    ObjCount := 0
  else
    ObjCount := FUsers.Length;
end;

procedure TfMain.GridGetObjectName(Sender: TObject; Index: integer;
  TIObject: TPersistent; var ObjName: string);
begin
  if ( TIObject <> nil ) then
    ObjName := (TIObject as TUser).UserName;
end;

function TfMain.CreateObj(): UserService;
begin
  if ( FUserService = nil ) then begin
    FUserService := TUserService_Proxy.Create(
                      'UserService',
                      'binary:',
                      'TCP:Address=127.0.0.1;Port=1234;target=UserService'
                    );
  end;
  Result := FUserService;
end;

type TCrakGrid = class(TTIGrid);
function TfMain.GetObjectIndex(): Integer;
begin
  Result := TCrakGrid(Grid).Row - 1;
end;

procedure TfMain.GridGetObject(Sender: TTICustomGrid; Index: integer;
  var TIObject: TPersistent);
begin
  TIObject := FUsers[Index];
end;

procedure TfMain.actNewExecute(Sender: TObject);
var
  obj : TUser;
  f : TfUserEdit;
begin
  obj := nil;
  f := TfUserEdit.Create(Application);
  try
    obj := TUser.Create();
    if f.UpdateObject(obj) then begin
      CreateObj().Add(obj);
    end;
  finally
    FreeAndNil(obj);
    f.Release();
    actSearch.Execute();
  end;
end;

procedure TfMain.actSearchExecute(Sender: TObject);
begin
  FreeAndNil(FUsers);
  FUsers := CreateObj().GetList();
  Grid.ListObject := FUsers;
  Grid.Invalidate();
end;

procedure TfMain.actDeleteExecute(Sender: TObject);
var
  i : Integer;
begin
  i := GetObjectIndex();
  if ( i >= 0 ) then begin
    try
      CreateObj().Delete(FUsers[i].UserName);
    finally
      actSearch.Execute();
    end;
  end;
end;

procedure TfMain.actUpdateExecute(Sender: TObject);
var
  i : Integer;
  obj : TUser;
  f : TfUserEdit;
begin
  i := GetObjectIndex();
  if ( i >= 0 ) then begin
    obj := FUsers[i];
    f := TfUserEdit.Create(Application);
    try
      if f.UpdateObject(obj) then begin
        CreateObj().Update(obj);
      end;
    finally
      f.Release();
      actSearch.Execute();
    end;
  end;
end;

procedure TfMain.actUpdateUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ( GetObjectIndex() >= 0 );
end;

initialization
  {$I umain.lrs}

  SYNAPSE_RegisterTCP_Transport();
  SYNAPSE_RegisterHTTP_Transport();
  
end.
