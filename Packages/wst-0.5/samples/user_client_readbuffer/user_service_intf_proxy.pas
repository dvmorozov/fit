{
This unit has been produced by ws_helper.
  Input unit name : "user_service_intf".
  This unit name  : "user_service_intf_proxy".
  Date            : "12/07/2007 11:50:48".
}

Unit user_service_intf_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, user_service_intf;

Type


  TUserService_Proxy=class(TBaseProxy,UserService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function GetList():TUserArray;
    procedure Add(
      const  AUser : TUser_Type
    );
    procedure Update(
      const  AUser : TUser_Type
    );
    function Find(
      const  AName : string
    ):TUser_Type;
    function Delete(
      const  AName : string
    ):boolean;
  End;

  Function wst_CreateInstance_UserService(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):UserService;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_UserService(const AFormat : string; const ATransport : string):UserService;
Begin
  Result := TUserService_Proxy.Create('UserService',AFormat+GetServiceDefaultFormatProperties(TypeInfo(UserService)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(UserService)));
End;

{ TUserService_Proxy implementation }

class function TUserService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(UserService);
end;

function TUserService_Proxy.GetList():TUserArray;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetList', GetTarget(),(Self as ICallContext));
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(TUserArray), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TUserService_Proxy.Add(
  const  AUser : TUser_Type
);
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Add', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AUser', TypeInfo(TUser_Type), AUser);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));

  Finally
    locSerializer.Clear();
  End;
End;

procedure TUserService_Proxy.Update(
  const  AUser : TUser_Type
);
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Update', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AUser', TypeInfo(TUser_Type), AUser);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));

  Finally
    locSerializer.Clear();
  End;
End;

function TUserService_Proxy.Find(
  const  AName : string
):TUser_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Find', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(TUser_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TUserService_Proxy.Delete(
  const  AName : string
):boolean;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Delete', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'result';
      locSerializer.Get(TypeInfo(boolean), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i user_service_intf.wst}

  {$IF DECLARED(Register_user_service_intf_ServiceMetadata)}
  Register_user_service_intf_ServiceMetadata();
  {$IFEND}
End.
