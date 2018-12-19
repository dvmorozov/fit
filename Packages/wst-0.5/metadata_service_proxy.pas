{
This unit has been produced by ws_helper.
  Input unit name : "metadata_service".
  This unit name  : "metadata_service_proxy".
  Date            : "12/11/2006 11:12".
}
{$INCLUDE wst_global.inc}
Unit metadata_service_proxy;

Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, metadata_service;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Type


  TWSTMetadataService_Proxy=class(TBaseProxy,IWSTMetadataService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function GetRepositoryList():TArrayOfStringRemotable;
    function GetRepositoryInfo(
      Const AName : string
    ):TWSTMtdRepository;
  End;

Implementation
uses wst_resources_imp, metadata_repository;

{ TWSTMetadataService_Proxy implementation }

class function TWSTMetadataService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(IWSTMetadataService);
end;

function TWSTMetadataService_Proxy.GetRepositoryList():TArrayOfStringRemotable;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetRepositoryList', GetTarget(),(Self as ICallContext));
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      If ( PTypeInfo(TypeInfo(TArrayOfStringRemotable))^.Kind in [tkClass,tkInterface] ) Then
        Pointer(Result) := Nil;
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(TArrayOfStringRemotable), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;

function TWSTMetadataService_Proxy.GetRepositoryInfo(
  Const AName : string
):TWSTMtdRepository;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetRepositoryInfo', GetTarget(),(Self as ICallContext));
      locSerializer.Put('AName', TypeInfo(string), AName);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(Result) := Nil;
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(TWSTMtdRepository), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i metadata_service.wst}

  {$IF DECLARED(Register_metadata_service_ServiceMetadata)}
  Register_metadata_service_ServiceMetadata();
  {$ENDIF}
End.
