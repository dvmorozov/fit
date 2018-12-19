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
unit synapse_http_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,{$IFDEF WST_DBG}Dialogs,{$ENDIF}
  service_intf, imp_utils, base_service_intf,
  httpsend;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Const
  sTRANSPORT_NAME = 'HTTP';

Type

{$M+}
  { THTTPTransport }
  THTTPTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FPropMngr : IPropertyManager;
    FConnection : THTTPSend;
    FAddress : string;
  private
    FFormat : string;
    FSoapAction: string;
    function GetAddress: string;
    function GetContentType: string;
    function GetProxyPassword: string;
    function GetProxyPort: Integer;
    function GetProxyServer: string;
    function GetProxyUsername: string;
    procedure SetAddress(const AValue: string);
    procedure SetContentType(const AValue: string);
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
    property ContentType : string Read GetContentType Write SetContentType;
    property Address : string Read GetAddress Write SetAddress;
    property ProxyServer : string Read GetProxyServer Write SetProxyServer;
    property ProxyPort : Integer Read GetProxyPort Write SetProxyPort;
    property ProxyUsername : string read GetProxyUsername write SetProxyUsername;
    property ProxyPassword : string read GetProxyPassword write SetProxyPassword;
    property SoapAction : string read FSoapAction write FSoapAction;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure SYNAPSE_RegisterHTTP_Transport();

implementation

{ THTTPTransport }

function THTTPTransport.GetAddress: string;
begin
  Result := FAddress;
end;

function THTTPTransport.GetContentType: string;
begin
  Result := FConnection.MimeType;
end;

function THTTPTransport.GetProxyPassword: string;
begin
  Result := FConnection.ProxyPass;
end;

function THTTPTransport.GetProxyPort: Integer;
begin
  Result := StrToInt(FConnection.ProxyPort);
end;

function THTTPTransport.GetProxyServer: string;
begin
  Result := FConnection.ProxyHost;
end;

function THTTPTransport.GetProxyUsername: string;
begin
  Result := FConnection.ProxyUser;
end;

procedure THTTPTransport.SetAddress(const AValue: string);
begin
  FAddress := AValue;
end;

procedure THTTPTransport.SetContentType(const AValue: string);
begin
  FConnection.MimeType := AValue;
end;

procedure THTTPTransport.SetProxyPassword(const AValue: string);
begin
  FConnection.ProxyPass := AValue;
end;

procedure THTTPTransport.SetProxyPort(const AValue: Integer);
begin
  FConnection.ProxyPort := IntToStr(AValue);
end;

procedure THTTPTransport.SetProxyServer(const AValue: string);
begin
  FConnection.ProxyHost := AValue;
end;

procedure THTTPTransport.SetProxyUsername(const AValue: string);
begin
  FConnection.ProxyUser := AValue;
end;

constructor THTTPTransport.Create();
begin
  inherited Create();
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FConnection := THTTPSend.Create();
  FConnection.Protocol := '1.1';
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
{$ENDIF}
begin
  FConnection.Document.Size := 0;
  FConnection.Headers.Add('soapAction:' + SoapAction);
  FConnection.Document.CopyFrom(ARequest,0);
  FConnection.HTTPMethod('POST',FAddress);
  AResponse.CopyFrom(FConnection.Document,0);
  FConnection.Clear();
{$IFDEF WST_DBG}
  TMemoryStream(ARequest).SaveToFile('request.log');
  SetLength(s,AResponse.Size);
  Move(TMemoryStream(AResponse).Memory^,s[1],Length(s));
  TMemoryStream(AResponse).SaveToFile('response.log');
  if IsConsole then
    WriteLn(s)
  {else
    ShowMessage(s)};
{$ENDIF}
end;

procedure SYNAPSE_RegisterHTTP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(THTTPTransport) as IItemFactory);
end;

end.
