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
unit library_protocol;

//{$DEFINE WST_DBG}

interface

uses
  Classes, SysUtils,{$IFDEF WST_DBG}Dialogs,{$ENDIF}
  service_intf, imp_utils, base_service_intf, library_base_intf,
  library_imp_utils;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  sTRANSPORT_NAME = 'LIB';

Type

{$M+}
  { TLIBTransport }
  TLIBTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FPropMngr : IPropertyManager;
    FModule : IwstModule;
    FHandler : TwstLibraryHandlerFunction;
  private
    FContentType: string;
    FFileName: string;
    FTarget: string;
  private
    FFormat : string;
    procedure SetFileName(const AValue: string);
    procedure LoadModule();
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  published
    property ContentType : string read FContentType write FContentType;
    property Target : string read FTarget write FTarget;
    property FileName : string read FFileName write SetFileName;
    property Format : string read FFormat write FFormat;
  end;
{$M+}

  procedure LIB_Register_Transport();

implementation
uses binary_streamer;

type

  { TwstStream }

  TwstStream = class(TInterfacedObject,IwstStream)
  private
    FStream : TStream;
  protected
    function Read(
            ABuffer    : Pointer;
      const ALenToRead : LongWord;
      out   AReadedLen : LongWord
    ):LongInt;
    function Write(
            ABuffer     : Pointer;
      const ALenToWrite : LongWord;
      out   AWrittenLen : LongWord
    ):LongInt;
    function GetSize(out ASize : LongWord):LongInt;
    function SetSize(const ANewSize : LongWord):LongInt;
    function GetPosition(out APos : LongWord):LongWord;
    function SetPosition(const ANewPos : LongWord):LongInt;
  public
    constructor Create(AStream : TStream);
  end;

{ TwstStream }

function TwstStream.Read(
        ABuffer     : Pointer;
  const ALenToRead  : LongWord;
  out   AReadedLen  : LongWord
): LongInt;
begin
  try
    AReadedLen := FStream.Read(ABuffer^,ALenToRead);
    Result := RET_OK;
  except
    Result := RET_FALSE;
  end;
end;

function TwstStream.Write(
        ABuffer      : Pointer;
  const ALenToWrite  : LongWord;
  out   AWrittenLen  : LongWord
): LongInt;
begin
  try
    AWrittenLen := FStream.Write(ABuffer^,ALenToWrite);
    Result := RET_OK;
  except
    Result := RET_FALSE;
  end;
end;

function TwstStream.GetSize(out ASize: LongWord): LongInt;
begin
  ASize := FStream.Size;
  Result := RET_OK;
end;

function TwstStream.SetSize(const ANewSize: LongWord): LongInt;
begin
  FStream.Size := ANewSize;
  Result := RET_OK;
end;

function TwstStream.GetPosition(out APos: LongWord): LongWord;
begin
  APos := FStream.Position;
  Result := RET_OK;
end;

function TwstStream.SetPosition(const ANewPos: LongWord): LongInt;
begin
  FStream.Position := ANewPos;
  Result := RET_OK;
end;

constructor TwstStream.Create(AStream: TStream);
begin
  Assert(Assigned(AStream));
  FStream := AStream;
end;

{ TLIBTransport }

procedure TLIBTransport.SetFileName(const AValue: string);
begin
  FFileName := AValue;
  if Assigned(FModule) and ( not AnsiSameStr(FFileName,FModule.GetFileName()) ) then begin
    FHandler := nil;
    FModule := nil;
  end;
end;

procedure TLIBTransport.LoadModule();
begin
  if ( FModule = nil ) then begin
    FModule := LibraryManager.Get(FFileName);
    FHandler := TwstLibraryHandlerFunction(FModule.GetProc(WST_LIB_HANDLER));
  end;
end;

constructor TLIBTransport.Create();
begin
  inherited Create();
  FPropMngr := TPublishedPropertyManager.Create(Self);
  FModule := nil;
  FHandler := nil
end;

destructor TLIBTransport.Destroy();
begin
  FPropMngr := Nil;
  FModule := nil;
  FHandler := nil;
  inherited Destroy();
end;

function TLIBTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

const MAX_ERR_LEN = 500;
procedure TLIBTransport.SendAndReceive(ARequest, AResponse: TStream);
Var
  wrtr : IDataStore;
  buffStream : TMemoryStream;
  strBuff : string;
  intfBuffer : IwstStream;
  bl : LongInt;
{$IFDEF WST_DBG}
  s : string;
  i : Int64;
{$ENDIF WST_DBG}
begin
  LoadModule();
  buffStream := TMemoryStream.Create();
  try
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

    buffStream.Position := 0;
    intfBuffer := TwstStream.Create(buffStream);
    bl := MAX_ERR_LEN;
    strBuff := StringOfChar(#0,bl);
    if ( FHandler(intfBuffer,Pointer(strBuff),bl) <> RET_OK ) then
      raise Exception.Create(strBuff);

    buffStream.Position := 0;
    AResponse.Size := 0;
    AResponse.CopyFrom(buffStream,0);
    AResponse.Position := 0;
    {$IFDEF WST_DBG}
    i := AResponse.Position;
    SetLength(s,AResponse.Size);
    AResponse.Read(s[1],AResponse.Size);
    if IsConsole then
      WriteLn(s)
    else
      ShowMessage(s);
    {$ENDIF WST_DBG}
  finally
    buffStream.Free();
  end;
end;

procedure LIB_Register_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TLIBTransport) as IItemFactory);
end;

end.
