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
{$RANGECHECKS OFF}
unit binary_streamer;

interface

uses
  Classes, SysUtils, Types;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Const
  MAX_ARRAY_LENGTH = 1024*1024;

Type

  TInt8U  = Byte; TInt8S  = ShortInt;
  TInt16U = Word; TInt16S = SmallInt;
  TInt32U = LongWord; TInt32S = LongInt;
  TInt64S = Int64;TInt64U = QWord;
  TBoolData = Boolean;
  TEnumData = Int64;
  TStringData = AnsiString;
  
  TFloat_Single_4    = Single;
  TFloat_Double_8    = Double;
  TFloat_Extended_10 = Extended;
  TFloat_Currency_8  = Currency;

  IDataStore = Interface
    ['{CA767A0E-7660-4765-9959-6960A69B1660}']
    procedure WriteInt8U(Const AData : TInt8U);
    procedure WriteInt8S(Const AData : TInt8S);
    
    procedure WriteInt16U(Const AData : TInt16U);
    procedure WriteInt16S(Const AData : TInt16S);

    procedure WriteInt32U(Const AData : TInt32U);
    procedure WriteInt32S(Const AData : TInt32S);
    
    procedure WriteInt64U(Const AData : TInt64U);
    procedure WriteInt64S(Const AData : TInt64S);
    
    procedure WriteBool(Const AData : TBoolData);
    procedure WriteEnum(Const AData : TEnumData);
    procedure WriteStr(Const AData : TStringData);
    
    procedure WriteSingle(Const AData : TFloat_Single_4);
    procedure WriteDouble(Const AData : TFloat_Double_8);
    procedure WriteExtended(Const AData : TFloat_Extended_10);
    procedure WriteCurrency(Const AData : TFloat_Currency_8);
  End;

  IDataStoreReader = Interface
    ['{AF50317E-6DD6-40C5-A2F6-3ED5F478564F}']
    function IsAtEof():Boolean;
    function ReadInt8U():TInt8U;
    function ReadInt8S():TInt8S;
    
    function ReadInt16U():TInt16U;
    function ReadInt16S():TInt16S;

    function ReadInt32U():TInt32U;
    function ReadInt32S():TInt32S;

    function ReadInt64U():TInt64U;
    function ReadInt64S():TInt64S;
    
    function ReadBool():TBoolData;
    function ReadEnum():TEnumData;
    function ReadStr():TStringData;

    function ReadSingle():TFloat_Single_4;
    function ReadDouble():TFloat_Double_8;
    function ReadExtended():TFloat_Extended_10;
    function ReadCurrency():TFloat_Currency_8;
  End;

  function CreateBinaryReader(AStream : TStream):IDataStoreReader;
  function CreateBinaryWriter(AStream : TStream):IDataStore;
  
  procedure ReverseBytes(var AData; const ALength : Integer);{$IFDEF USE_INLINE}{$IFDEF ENDIAN_BIG}inline;{$ENDIF}{$ENDIF}
  function Reverse_16(const AValue:Word):Word;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function Reverse_32(const AValue:DWord):DWord;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function Reverse_64(const AValue:QWord):QWord;{$IFDEF USE_INLINE}inline;{$ENDIF}
  
  function Reverse_Single(const AValue:Single):Single;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function Reverse_Double(const AValue:Double):Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function Reverse_Extended(const AValue:Extended):Extended;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function Reverse_Currency(const AValue:Currency):Currency;{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

{$IFDEF ENDIAN_BIG}
procedure ReverseBytes(var AData; const ALength : Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
end;
{$ELSE} // assume ENDIAN_LITTLE
procedure ReverseBytes(var AData; const ALength : Integer);
Var
  i,j : PtrInt;
  c : Byte;
  pDt : {$IFDEF FPC}^Byte{$ELSE}PByteArray{$ENDIF};
begin
  pDt := @AData;
  j := ALength div 2;
  For i := 0 To Pred(j) Do Begin
    c := pDt{$IFNDEF FPC}^{$ENDIF}[i];
    pDt[i] := pDt[(ALength - 1 ) - i];
    pDt[(ALength - 1 ) - i] := c;
  End;
end;
{$ENDIF}

function Reverse_16(const AValue:Word):Word;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,2)
end;

function Reverse_32(const AValue:DWord):DWord;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,4)
end;

function Reverse_64(const AValue:QWord):QWord;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,8)
end;

function Reverse_Single(const AValue:Single):Single;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,4)
end;

function Reverse_Double(const AValue:Double):Double;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,8)
end;

function Reverse_Extended(const AValue:Extended):Extended;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,10);
end;

function Reverse_Currency(const AValue:Currency):Currency;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AValue;
  ReverseBytes(Result,8);
end;

Type
  { TDataStore }

  TDataStore = class(TInterfacedObject,IDataStore)
  Private
    FStream : TStream;
  Protected
    procedure WriteInt8U(Const AData : TInt8U);
    procedure WriteInt8S(Const AData : TInt8S);
    
    procedure WriteInt16U(Const AData : TInt16U);
    procedure WriteInt16S(Const AData : TInt16S);

    procedure WriteInt32U(Const AData : TInt32U);
    procedure WriteInt32S(Const AData : TInt32S);

    procedure WriteInt64U(Const AData : TInt64U);
    procedure WriteInt64S(Const AData : TInt64S);
    
    procedure WriteBool(Const AData : TBoolData);
    procedure WriteEnum(Const AData : TEnumData);
    procedure WriteStr(Const AData : TStringData);
    
    procedure WriteSingle(Const AData : TFloat_Single_4);
    procedure WriteDouble(Const AData : TFloat_Double_8);
    procedure WriteExtended(Const AData : TFloat_Extended_10);
    procedure WriteCurrency(Const AData : TFloat_Currency_8);
  Public
    constructor Create(AStream : TStream);
  End;

  { TDataStoreReader }

  TDataStoreReader = class(TInterfacedObject,IDataStoreReader)
  Private
    FStream : TStream;
  Protected
    function IsAtEof():Boolean;
    function ReadInt8U():TInt8U;
    function ReadInt8S():TInt8S;
    
    function ReadInt16U():TInt16U;
    function ReadInt16S():TInt16S;

    function ReadInt32U():TInt32U;
    function ReadInt32S():TInt32S;
    
    function ReadInt64U():TInt64U;
    function ReadInt64S():TInt64S;
    
    function ReadBool():TBoolData;
    function ReadEnum():TEnumData;
    function ReadStr():TStringData;
    
    function ReadSingle():TFloat_Single_4;
    function ReadDouble():TFloat_Double_8;
    function ReadExtended():TFloat_Extended_10;
    function ReadCurrency():TFloat_Currency_8;
  Public
    constructor Create(AStream : TStream);
  End;

function CreateBinaryWriter(AStream : TStream):IDataStore;
begin
  Result := TDataStore.Create(AStream) As IDataStore;
end;

function CreateBinaryReader(AStream : TStream):IDataStoreReader;
begin
  Result := TDataStoreReader.Create(AStream) As IDataStoreReader;
end;

{ TDataStore }

procedure TDataStore.WriteInt8U(const AData: TInt8U);
begin
  FStream.Write(AData,SizeOf(AData));
end;

procedure TDataStore.WriteInt8S(const AData: TInt8S);
begin
  FStream.Write(AData,SizeOf(AData));
end;

{$IFDEF FPC}
procedure TDataStore.WriteInt16U(const AData: TInt16U);
begin
  FStream.Write(Reverse_16(AData),SizeOf(AData));
end;

procedure TDataStore.WriteInt16S(const AData: TInt16S);
begin
  FStream.Write(Reverse_16(AData),SizeOf(AData));
end;

procedure TDataStore.WriteInt32U(const AData: TInt32U);
begin
  FStream.Write(Reverse_32(AData),SizeOf(AData));
end;

procedure TDataStore.WriteInt32S(const AData: TInt32S);
begin
  FStream.Write(Reverse_32(AData),SizeOf(AData));
end;

procedure TDataStore.WriteInt64U(const AData: TInt64U);
begin
  FStream.Write(Reverse_64(AData),SizeOf(AData));
end;

procedure TDataStore.WriteInt64S(const AData: TInt64S);
begin
  FStream.Write(Reverse_64(AData),SizeOf(AData));
end;
{$ELSE}
procedure TDataStore.WriteInt16U(const AData: TInt16U);
var
  bffr : TInt16U;
begin
  bffr := Reverse_16(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteInt16S(const AData: TInt16S);
var
  bffr : TInt16U;
begin
  bffr := Reverse_16(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteInt32U(const AData: TInt32U);
var
  bffr : TInt32U;
begin
  bffr := Reverse_32(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteInt32S(const AData: TInt32S);
var
  bffr : TInt32U;
begin
  bffr := Reverse_32(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteInt64U(const AData: TInt64U);
var
  bffr : TInt64U;
begin
  bffr := Reverse_64(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteInt64S(const AData: TInt64S);
var
  bffr : TInt64U;
begin
  bffr := Reverse_64(AData);
  FStream.Write(bffr,SizeOf(AData));
end;
{$ENDIF}

procedure TDataStore.WriteBool(const AData: TBoolData);
Var
  i : TInt8U;
begin
  If AData Then
    i := 1
  Else
    i := 0;
  WriteInt8U(i);
end;

procedure TDataStore.WriteEnum(const AData: TEnumData);
begin
  WriteInt64S(AData);
end;

procedure TDataStore.WriteStr(const AData: TStringData);
Var
  i : TInt32S;
begin
  i := Length(AData);
  WriteInt32S(i);
  If ( i > 0 ) Then
    FStream.Write(AData[1],i);
end;

{
procedure TDataStore.WriteSingle(const AData: TFloat_Single_4);
begin
  FStream.Write(Reverse_Single(AData),SizeOf(AData));
end;

procedure TDataStore.WriteDouble(const AData: TFloat_Double_8);
begin
  FStream.Write(Reverse_Double(AData),SizeOf(AData));
end;

procedure TDataStore.WriteExtended(const AData: TFloat_Extended_10);
begin
  FStream.Write(Reverse_Extended(AData),SizeOf(AData));
end;

procedure TDataStore.WriteCurrency(const AData: TFloat_Currency_8);
begin
  FStream.Write(Reverse_Currency(AData),SizeOf(AData));
end;
}
procedure TDataStore.WriteSingle(const AData: TFloat_Single_4);
var
  bffr : TFloat_Single_4;
begin
  bffr := Reverse_Single(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteDouble(const AData: TFloat_Double_8);
var
  bffr : TFloat_Double_8;
begin
  bffr := Reverse_Double(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteExtended(const AData: TFloat_Extended_10);
var
  bffr : TFloat_Extended_10;
begin
  bffr := Reverse_Extended(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

procedure TDataStore.WriteCurrency(const AData: TFloat_Currency_8);
var
  bffr : TFloat_Currency_8;
begin
  bffr := Reverse_Currency(AData);
  FStream.Write(bffr,SizeOf(AData));
end;

constructor TDataStore.Create(AStream: TStream);
begin
  Assert(Assigned(AStream));
  FStream := AStream;
end;

{ TDataStoreReader }

function TDataStoreReader.IsAtEof(): Boolean;
begin
  Result := ( FStream.Position >= FStream.Size );
end;

function TDataStoreReader.ReadInt8U(): TInt8U;
begin
  FStream.Read(Result,SizeOf(Result));
end;

function TDataStoreReader.ReadInt8S(): TInt8S;
begin
  FStream.Read(Result,SizeOf(Result));
end;

function TDataStoreReader.ReadInt16U(): TInt16U;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_16(Result);
end;

function TDataStoreReader.ReadInt16S(): TInt16S;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_16(Result);
end;

function TDataStoreReader.ReadInt32U(): TInt32U;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_32(Result);
end;

function TDataStoreReader.ReadInt32S(): TInt32S;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_32(Result);
end;

function TDataStoreReader.ReadInt64U(): TInt64U;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_64(Result);
end;

function TDataStoreReader.ReadInt64S(): TInt64S;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_64(Result);
end;

function TDataStoreReader.ReadBool(): TBoolData;
begin
  Result := ( ReadInt8U() > 0 );
end;

function TDataStoreReader.ReadEnum(): TEnumData;
begin
  Result := ReadInt64S();
end;

function TDataStoreReader.ReadStr(): TStringData;
Var
  i : TInt32S;
begin
  i := ReadInt32S();
  SetLength(Result,i);
  If ( i > 0 ) Then
    FStream.ReadBuffer(Result[1],i);
end;

function TDataStoreReader.ReadSingle(): TFloat_Single_4;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_Single(Result);
end;

function TDataStoreReader.ReadDouble(): TFloat_Double_8;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_Double(Result);
end;

function TDataStoreReader.ReadExtended(): TFloat_Extended_10;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_Extended(Result);
end;

function TDataStoreReader.ReadCurrency(): TFloat_Currency_8;
begin
  FStream.Read(Result,SizeOf(Result));
  Result := Reverse_Currency(Result);
end;

constructor TDataStoreReader.Create(AStream: TStream);
begin
  Assert(Assigned(AStream));
  FStream := AStream;
end;

end.
