{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit indy_tcp_server;

{$INCLUDE wst_global.inc}
//{$DEFINE WST_DBG}
interface

uses
  Classes, SysUtils,
  IdTCPServer,
{$IFDEF INDY_10}
  IdContext, IdCustomTCPServer,
{$ENDIF}
{$IFDEF INDY_9}
  //IdTCPServer,
{$ENDIF}
  IdSocketHandle,
  server_listener;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  sSERVER_PORT = 1234;
  
type

{$IFDEF INDY_9}
  TIdBytes = array of Byte;
{$ENDIF}

  { TwstIndyTcpListener }

  TwstIndyTcpListener = class(TwstListener)
  private
    FTCPServerObject: TIdTCPServer;
    FDefaultTime : PtrInt;
  protected
    procedure Handle_OnExecute(
    {$IFDEF INDY_10}
      AContext : TIdContext
    {$ENDIF}
    {$IFDEF INDY_9}
      AContext : TIdPeerThread
    {$ENDIF}
    );
  public
    constructor Create(
      const AServerIpAddress   : string  = '127.0.0.1';
      const AListningPort      : Integer = sSERVER_PORT;
      const ADefaultClientPort : Integer = 25000
    );
    destructor Destroy(); override;
    class function GetDescription() : string;override;
    procedure Start();override;
    procedure Stop();override;
  end;

implementation
uses
{$IFNDEF FPC}
     ActiveX,
  {$IFDEF INDY_9}
     wst_indy9_utils,
  {$ENDIF}
{$ENDIF}
  IdGlobal, binary_streamer, server_service_intf, server_service_imputils;

{ TwstIndyTcpListener }

procedure TwstIndyTcpListener.Handle_OnExecute(
{$IFDEF INDY_10}
  AContext : TIdContext
{$ENDIF}
{$IFDEF INDY_9}
  AContext : TIdPeerThread
{$ENDIF}
);

  function ReadInputBuffer(AStream : TStream) : Integer;
  var
    strBuff : TIdBytes;
    bufferLen : LongInt;
    i, j, c : PtrInt;
  begin
    Result := 0;
    bufferLen := 0;
    try
      SetLength(strBuff,SizeOf(bufferLen));
{$IFDEF INDY_10}
      AContext.Connection.IOHandler.ReadBytes(strBuff,SizeOf(bufferLen),False);
{$ENDIF}
{$IFDEF INDY_9}
      AContext.Connection.ReadBuffer(strBuff[0],SizeOf(bufferLen));
{$ENDIF}
      Move(strBuff[0],bufferLen,SizeOf(bufferLen));
      bufferLen := Reverse_32(bufferLen);
      AStream.Size := bufferLen;
      if ( bufferLen > 0 ) then begin
        c := 0;
        i := 1024;
        if ( i > bufferLen ) then
          i := bufferLen;
        SetLength(strBuff,i);
        repeat
        {$IFDEF INDY_10}
          AContext.Connection.IOHandler.ReadBytes(strBuff,i,False);
        {$ENDIF}
        {$IFDEF INDY_9}
          AContext.Connection.ReadBuffer(strBuff[0],i);
        {$ENDIF}
          AStream.Write(strBuff[0],i);
          Inc(c,i);
          if ( ( bufferLen - c ) > 1024 ) then
            i := 1024
          else
            i := bufferLen - c;
        until ( i = 0 );
      end;
      AStream.Position := 0;
      Result := AStream.Size;
    finally
      SetLength(strBuff,0);
    end;
  end;

var
  locInStream, locOutStream : TMemoryStream;
  wrtr : IDataStore;
  rdr : IDataStoreReader;
  buff, trgt,ctntyp, frmt : string;
  rqst : IRequestBuffer;
  i : PtrUInt;
begin
{$IFNDEF FPC}
  //CoInitialize(nil);
  //try
{$ENDIF}
    locOutStream := nil;
    locInStream := TMemoryStream.Create();
    try
      locOutStream := TMemoryStream.Create();
    {$IFDEF INDY_10}
      if ( Self.FDefaultTime <> 0 ) then
        AContext.Connection.IOHandler.ReadTimeout := Self.FDefaultTime;
    {$ENDIF}
      if ( ReadInputBuffer(locInStream) >= SizeOf(LongInt) ) then begin
        rdr := CreateBinaryReader(locInStream);
        trgt := rdr.ReadStr();
        ctntyp := rdr.ReadStr();
        frmt := rdr.ReadStr();
        buff := rdr.ReadStr();

{$IFDEF WST_DBG}
        WriteLn(buff);
{$ENDIF}

        rdr := nil;
        locInStream.Size := 0;
        locInStream.Write(buff[1],Length(buff));
        locInStream.Position := 0;
        rqst := TRequestBuffer.Create(trgt,ctntyp,locInStream,locOutStream,frmt);
        HandleServiceRequest(rqst);
        i := locOutStream.Size;
        SetLength(buff,i);
        locOutStream.Position := 0;
        locOutStream.Read(buff[1],i);
        locOutStream.Size := 0;
        wrtr := CreateBinaryWriter(locOutStream);
        wrtr.WriteStr(buff);
        locOutStream.Position := 0;
      {$IFDEF INDY_10}
        AContext.Connection.IOHandler.Write(locOutStream,locOutStream.Size,False);
      {$ENDIF}
      {$IFDEF INDY_9}
        AContext.Connection.WriteStream(locOutStream,True,False,locOutStream.Size);
      {$ENDIF}
      end;
    finally
      FreeAndNil(locOutStream);
      FreeAndNil(locInStream);
    end;
{$IFNDEF FPC}
  //finally
    //CoUninitialize();
  //end;
{$ENDIF}
end;

constructor TwstIndyTcpListener.Create(
  const AServerIpAddress : string;
  const AListningPort : Integer;
  const ADefaultClientPort : Integer
);
var
  b : TIdSocketHandle;
begin
  inherited Create();
  FTCPServerObject := TIdTCPServer.Create({$IFNDEF INDY_10}nil{$ENDIF});
{$IFDEF INDY_9}
  FTCPServerObject.ThreadClass := TwstIndy9Thread;
{$ENDIF}
  b := FTCPServerObject.Bindings.Add();
  b.IP := AServerIpAddress;
  b.port := AListningPort;

  FTCPServerObject.DefaultPort := ADefaultClientPort;
  FTCPServerObject.OnExecute := {$IFDEF FPC}@{$ENDIF}Handle_OnExecute;
end;

destructor TwstIndyTcpListener.Destroy();
begin
  if ( FTCPServerObject <> nil ) then
    Stop();
  FreeAndNil(FTCPServerObject);
  inherited Destroy();
end;

class function TwstIndyTcpListener.GetDescription() : string;
begin
  Result := 'WST Indy TCP Listener';
end;

procedure TwstIndyTcpListener.Start();
begin
  if not FTCPServerObject.Active then
    FTCPServerObject.Active := True;
end;

procedure TwstIndyTcpListener.Stop();
begin
  if FTCPServerObject.Active then
    FTCPServerObject.Active := False;
end;

end.

