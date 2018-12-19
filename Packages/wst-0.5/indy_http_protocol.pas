{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit indy_http_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf,
  IdHTTP;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Const
  sTRANSPORT_NAME = 'HTTP';
  
Type

{$M+}
  { THTTPTransport }
  THTTPTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FFormat : string;
    FPropMngr : IPropertyManager;
    FConnection : TidHttp;
    FSoapAction: string;
    FContentType: string;
    function GetAddress: string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    procedure SetAddress(const AValue: string);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  Published
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read FSoapAction write FSoapAction;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure INDY_RegisterHTTP_Transport();
  
implementation

{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FConnection.Request.URL;
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyParams.ProxyPassword;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := FConnection.ProxyParams.ProxyPort;
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.ProxyParams.ProxyServer;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyParams.ProxyUsername;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FConnection.Request.URL := AValue;
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyParams.ProxyPassword := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyParams.ProxyPort := AValue;
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.ProxyParams.ProxyServer := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyParams.ProxyUsername := AValue;
end;

constructor THTTPTransport.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FConnection := TidHttp.Create(Nil);
end;

destructor THTTPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  FPropMngr := Nil;
  inherited Destroy();
end;

function THTTPTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure THTTPTransport.SendAndReceive(ARequest, AResponse: TStream);
{$IFDEF WST_DBG}
var
  s : string;
  i : Int64;
{$ENDIF WST_DBG}
begin
  if not ( IsStrEmpty(FConnection.ProxyParams.ProxyUsername) and
           IsStrEmpty(FConnection.ProxyParams.ProxyPassword)
         )
  then begin
    FConnection.ProxyParams.BasicAuthentication := True;
  end;
  FConnection.Request.CustomHeaders.Clear();
  FConnection.Request.CustomHeaders.Values['soapAction'] := SoapAction;
  FConnection.Request.ContentType := ContentType;
  FConnection.Post(Address,ARequest, AResponse);
  {$IFDEF WST_DBG}
  i := AResponse.Size;
  SetLength(s,i);
  Move(TMemoryStream(AResponse).Memory^,s[1],i);
  WriteLn('--------------------------------------------');
  WriteLn(s);
  {$ENDIF WST_DBG}
end;

procedure INDY_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport) as IItemFactory);
end;

end.
