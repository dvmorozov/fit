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
unit synapse_tcp_protocol;

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils, base_service_intf,
  blcksock;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

{$DEFINE WST_DBG}

Const
  sTRANSPORT_NAME = 'TCP';

Type

  ETCPException = class(EServiceException)
  End;
  
{$M+}
  { TTCPTransport }
  TTCPTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FFormat : string;
    FPropMngr : IPropertyManager;
    FConnection : TTCPBlockSocket;
    FContentType : string;
    FTarget: string;
    FAddress : string;
    FPort : string;
    FDefaultTimeOut: Integer;
  private
    procedure Connect();
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  Published
    property Target : string Read FTarget Write FTarget;
    property ContentType : string Read FContentType Write FContentType;
    property Address : string Read FAddress Write FAddress;
    property Port : string Read FPort Write FPort;
    property DefaultTimeOut : Integer read FDefaultTimeOut write FDefaultTimeOut;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure SYNAPSE_RegisterTCP_Transport();

implementation
uses binary_streamer, Math;

{ TTCPTransport }

procedure TTCPTransport.Connect();
var
  locReconnect : Boolean;
begin
  if ( FConnection.Socket = NOT(0) ) then begin
    FConnection.Connect(Address,Port);
  end else begin
    locReconnect := False;
    try
      locReconnect := not FConnection.CanRead(0);
    except
      locReconnect := True;
    end;
    if locReconnect then begin
      FConnection.CloseSocket();
      FConnection.Connect(Address,Port);
    end;
  end;
end;

constructor TTCPTransport.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FConnection := TTCPBlockSocket.Create();
  FConnection.RaiseExcept := True;
  FDefaultTimeOut := 90000;
end;

destructor TTCPTransport.Destroy();
begin
  FreeAndNil(FConnection);
  FPropMngr := Nil;
  inherited Destroy();
end;

function TTCPTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure TTCPTransport.SendAndReceive(ARequest, AResponse: TStream);
Var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  strBuff : string;
  bufferLen : LongInt;
  i, j, c : PtrInt;
begin
  buffStream := TMemoryStream.Create();
  Try
    wrtr := CreateBinaryWriter(buffStream);
    wrtr.WriteInt32S(0);
    wrtr.WriteStr(Target);
    wrtr.WriteStr(ContentType);
    wrtr.WriteStr(Self.Format);
    SetLength(strBuff,ARequest.Size);
    ARequest.Position := 0;
    ARequest.Read(strBuff[1],Length(strBuff));
    wrtr.WriteStr(strBuff);
    buffStream.Position := 0;
    wrtr.WriteInt32S(buffStream.Size-4);

    //if ( FConnection.Socket = NOT(0) ) then
      //FConnection.Connect(Address,Port);
    Connect();
    FConnection.SendBuffer(buffStream.Memory,buffStream.Size);

    bufferLen := 0;
    FConnection.RecvBufferEx(@bufferLen,SizeOf(bufferLen),DefaultTimeOut);
    FConnection.ExceptCheck();
    bufferLen := Reverse_32(bufferLen);
    AResponse.Size := bufferLen;
    if ( bufferLen > 0 ) then begin
      c := 0;
      i := 1024;
      if ( i > bufferLen ) then
        i := bufferLen;
      SetLength(strBuff,i);
      repeat
        j := FConnection.RecvBufferEx(@(strBuff[1]),i,DefaultTimeOut);
        FConnection.ExceptCheck();
        AResponse.Write(strBuff[1],j);
        Inc(c,j);
        i := Min(1024,(bufferLen-c));
      until ( i =0 ) or ( j <= 0 );
    end;
    AResponse.Position := 0;
    {$IFDEF WST_DBG}
    TMemoryStream(AResponse).SaveToFile('response.log');
    {$ENDIF WST_DBG}
  Finally
    buffStream.Free();
  End;
end;

procedure SYNAPSE_RegisterTCP_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TTCPTransport) as IItemFactory);
end;

end.
