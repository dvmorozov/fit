{
This unit has been produced by ws_helper.
  Input unit name : "googlewebapi".
  This unit name  : "googlewebapi_binder".
  Date            : "12/11/2006 00:24".
}
unit googlewebapi_binder;
{$mode objfpc}{$H+}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, googlewebapi;

type


  TGoogleSearch_ServiceBinder=class(TBaseServiceBinder)
  Protected
    procedure doSpellingSuggestionHandler(AFormatter:IFormatterResponse);
    procedure doGoogleSearchHandler(AFormatter:IFormatterResponse);
  Public
    constructor Create();
  End;

  TGoogleSearch_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

  procedure Server_service_RegisterGoogleSearchService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TGoogleSearch_ServiceBinder implementation }
procedure TGoogleSearch_ServiceBinder.doSpellingSuggestionHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : IGoogleSearch;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  key : string;
  phrase : string;
  returnVal : string;
Begin
  callCtx := GetCallContext();
  
  strPrmName := 'key';  AFormatter.Get(TypeInfo(string),strPrmName,key);
  strPrmName := 'phrase';  AFormatter.Get(TypeInfo(string),strPrmName,phrase);
  
  tmpObj := Self.GetFactory().CreateInstance() as IGoogleSearch;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.doSpellingSuggestion(key,phrase);
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(string),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;

procedure TGoogleSearch_ServiceBinder.doGoogleSearchHandler(AFormatter:IFormatterResponse);
Var
  cllCntrl : ICallControl;
  tmpObj : IGoogleSearch;
  callCtx : ICallContext;
  strPrmName : string;
  procName,trgName : string;
  key : string;
  q : string;
  start : Integer;
  maxResults : Integer;
  filter : Boolean;
  restrict : string;
  safeSearch : Boolean;
  lr : string;
  ie : string;
  oe : string;
  returnVal : TGoogleSearchResult;
Begin
  callCtx := GetCallContext();
  Pointer(returnVal) := Nil;
  
  strPrmName := 'key';  AFormatter.Get(TypeInfo(string),strPrmName,key);
  strPrmName := 'q';  AFormatter.Get(TypeInfo(string),strPrmName,q);
  strPrmName := 'start';  AFormatter.Get(TypeInfo(Integer),strPrmName,start);
  strPrmName := 'maxResults';  AFormatter.Get(TypeInfo(Integer),strPrmName,maxResults);
  strPrmName := 'filter';  AFormatter.Get(TypeInfo(Boolean),strPrmName,filter);
  strPrmName := 'restrict';  AFormatter.Get(TypeInfo(string),strPrmName,restrict);
  strPrmName := 'safeSearch';  AFormatter.Get(TypeInfo(Boolean),strPrmName,safeSearch);
  strPrmName := 'lr';  AFormatter.Get(TypeInfo(string),strPrmName,lr);
  strPrmName := 'ie';  AFormatter.Get(TypeInfo(string),strPrmName,ie);
  strPrmName := 'oe';  AFormatter.Get(TypeInfo(string),strPrmName,oe);
  
  tmpObj := Self.GetFactory().CreateInstance() as IGoogleSearch;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(GetCallContext());
  
  returnVal := tmpObj.doGoogleSearch(key,q,start,maxResults,filter,restrict,safeSearch,lr,ie,oe);
  If Assigned(Pointer(returnVal)) Then
    callCtx.AddObjectToFree(TObject(returnVal));
  
  procName := AFormatter.GetCallProcedureName();
  trgName := AFormatter.GetCallTarget();
  AFormatter.Clear();
  AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.Put('return',TypeInfo(TGoogleSearchResult),returnVal);
  AFormatter.EndCallResponse();
  
  callCtx := Nil;
End;


constructor TGoogleSearch_ServiceBinder.Create();
Begin
  Inherited Create(GetServiceImplementationRegistry().FindFactory('IGoogleSearch'));
  RegisterVerbHandler('doSpellingSuggestion',@doSpellingSuggestionHandler);
  RegisterVerbHandler('doGoogleSearch',@doGoogleSearchHandler);
End;


{ TGoogleSearch_ServiceBinderFactory }
function TGoogleSearch_ServiceBinderFactory.CreateInstance():IInterface;
Begin
  Result := TGoogleSearch_ServiceBinder.Create() as IInterface;
End;


procedure Server_service_RegisterGoogleSearchService();
Begin
  GetServerServiceRegistry().Register('IGoogleSearch',TGoogleSearch_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$IF DECLARED(Register_googlewebapi_NameSpace)}
  Register_googlewebapi_NameSpace();
  {$ENDIF}

  {$i googlewebapi.wst}

End.
