{
This unit has been produced by ws_helper.
  Input unit name : "ebay".
  This unit name  : "ebay_proxy".
  Date            : "12/11/2006 11:44".
}
Unit ebay_proxy;
{$mode objfpc}{$H+}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, ebay;

Type


  TeBayAPIInterfaceService_Proxy=class(TBaseProxy,IeBayAPIInterfaceService)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    procedure GetCategories(
      Const GetCategoriesRequest : TGetCategoriesRequestType; 
      Out GetCategoriesResponse : TGetCategoriesResponseType
    );
    procedure GetPopularKeywords(
      Const GetPopularKeywordsRequest : TGetPopularKeywordsRequestType; 
      Out GetPopularKeywordsResponse : TGetPopularKeywordsResponseType
    );
  End;

Implementation
uses wst_resources_imp, metadata_repository;

{ TeBayAPIInterfaceService_Proxy implementation }

class function TeBayAPIInterfaceService_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(IeBayAPIInterfaceService);
end;

procedure TeBayAPIInterfaceService_Proxy.GetCategories(
  Const GetCategoriesRequest : TGetCategoriesRequestType; 
  Out GetCategoriesResponse : TGetCategoriesResponseType
);
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetCategories', GetTarget(),(Self as ICallContext));
      locSerializer.Put('GetCategoriesRequest', TypeInfo(TGetCategoriesRequestType), GetCategoriesRequest);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(GetCategoriesResponse) := Nil;
      strPrmName := 'GetCategoriesResponse';
      locSerializer.Get(TypeInfo(TGetCategoriesResponseType), strPrmName, GetCategoriesResponse);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TeBayAPIInterfaceService_Proxy.GetPopularKeywords(
  Const GetPopularKeywordsRequest : TGetPopularKeywordsRequestType; 
  Out GetPopularKeywordsResponse : TGetPopularKeywordsResponseType
);
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetPopularKeywords', GetTarget(),(Self as ICallContext));
      locSerializer.Put('GetPopularKeywordsRequest', TypeInfo(TGetPopularKeywordsRequestType), GetPopularKeywordsRequest);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      Pointer(GetPopularKeywordsResponse) := Nil;
      strPrmName := 'GetPopularKeywordsResponse';
      locSerializer.Get(TypeInfo(TGetPopularKeywordsResponseType), strPrmName, GetPopularKeywordsResponse);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i ebay.wst}

  {$IF DECLARED(Register_ebay_ServiceMetadata)}
  Register_ebay_ServiceMetadata();
  {$ENDIF}
End.
