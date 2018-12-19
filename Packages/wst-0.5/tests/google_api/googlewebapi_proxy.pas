{
This unit has been produced by ws_helper.
  Input unit name : "googlewebapi".
  This unit name  : "googlewebapi_proxy".
  Date            : "12/11/2006 00:24".
}
Unit googlewebapi_proxy;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, googlewebapi;

Type


  TGoogleSearch_Proxy=class(TBaseProxy,IGoogleSearch)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function doSpellingSuggestion(
      Const key : string; 
      Const phrase : string
    ):string;
    function doGoogleSearch(
      Const key : string; 
      Const q : string; 
      Const start : Integer; 
      Const maxResults : Integer; 
      Const filter : Boolean; 
      Const restrict : string; 
      Const safeSearch : Boolean; 
      Const lr : string; 
      Const ie : string; 
      Const oe : string
    ):TGoogleSearchResult;
  End;

Implementation
uses wst_resources_imp, metadata_repository;

{ TGoogleSearch_Proxy implementation }

class function TGoogleSearch_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(IGoogleSearch);
end;

function TGoogleSearch_Proxy.doSpellingSuggestion(
  Const key : string; 
  Const phrase : string
):string;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('doSpellingSuggestion', GetTarget(),(Self as ICallContext));
      locSerializer.Put('key', TypeInfo(string), key);
      locSerializer.Put('phrase', TypeInfo(string), phrase);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(string), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;

function TGoogleSearch_Proxy.doGoogleSearch(
  Const key : string; 
  Const q : string; 
  Const start : Integer; 
  Const maxResults : Integer; 
  Const filter : Boolean; 
  Const restrict : string; 
  Const safeSearch : Boolean; 
  Const lr : string; 
  Const ie : string; 
  Const oe : string
):TGoogleSearchResult;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('doGoogleSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('key', TypeInfo(string), key);
      locSerializer.Put('q', TypeInfo(string), q);
      locSerializer.Put('start', TypeInfo(Integer), start);
      locSerializer.Put('maxResults', TypeInfo(Integer), maxResults);
      locSerializer.Put('filter', TypeInfo(Boolean), filter);
      locSerializer.Put('restrict', TypeInfo(string), restrict);
      locSerializer.Put('safeSearch', TypeInfo(Boolean), safeSearch);
      locSerializer.Put('lr', TypeInfo(string), lr);
      locSerializer.Put('ie', TypeInfo(string), ie);
      locSerializer.Put('oe', TypeInfo(string), oe);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(Result) := Nil;
      strPrmName := 'return';
      locSerializer.Get(TypeInfo(TGoogleSearchResult), strPrmName, result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i googlewebapi.wst}

  {$IF DECLARED(Register_googlewebapi_ServiceMetadata)}
  Register_googlewebapi_ServiceMetadata();
  {$ENDIF}
End.
