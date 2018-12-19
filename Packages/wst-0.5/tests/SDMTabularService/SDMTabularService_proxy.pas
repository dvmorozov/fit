{
This unit has been produced by ws_helper.
  Input unit name : "SDMTabularService".
  This unit name  : "SDMTabularService_proxy".
  Date            : "11/07/2007 23:11:05".
}

Unit SDMTabularService_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, SDMTabularService;

Type


  TSDMTabularServiceSoap_Proxy=class(TBaseProxy,SDMTabularServiceSoap)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function RunQuery(
      Const RunQueryParam : RunQueryType
    ):RunQueryResponse;
  End;

  Function wst_CreateInstance_SDMTabularServiceSoap(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):SDMTabularServiceSoap;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_SDMTabularServiceSoap(const AFormat : string; const ATransport : string):SDMTabularServiceSoap;
Begin
  Result := TSDMTabularServiceSoap_Proxy.Create('SDMTabularServiceSoap',AFormat+GetServiceDefaultFormatProperties(TypeInfo(SDMTabularServiceSoap)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(SDMTabularServiceSoap)));
End;

{ TSDMTabularServiceSoap_Proxy implementation }

class function TSDMTabularServiceSoap_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(SDMTabularServiceSoap);
end;

function TSDMTabularServiceSoap_Proxy.RunQuery(
  Const RunQueryParam : RunQueryType
):RunQueryResponse;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('RunQuery', GetTarget(),(Self as ICallContext));
      locSerializer.Put('RunQuery', TypeInfo(RunQueryType), RunQueryParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'RunQueryResponse';
      locSerializer.Get(TypeInfo(RunQueryResponse), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i SDMTabularService.wst}

  {$IF DECLARED(Register_SDMTabularService_ServiceMetadata)}
  Register_SDMTabularService_ServiceMetadata();
  {$IFEND}
End.
