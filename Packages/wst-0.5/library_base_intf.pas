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
unit library_base_intf;

interface
uses base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  RET_OK = 0;
  RET_FALSE = 1;
  WST_LIB_HANDLER = 'wstHandleRequest';

type

  EwstCheckException = class(EServiceException)
  private
    FReturnCode: Integer;
  public
    property ReturnCode : Integer read FReturnCode write FReturnCode;
  end;

  IwstStream = interface
    ['{95700F89-3E36-4678-AD84-347162E39288}']
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
  end;

  TwstLibraryHandlerFunction =
    function(
      ARequestBuffer : IwstStream;
      AErrorBuffer : Pointer;
      var AErrorBufferLen : LongInt
    ):LongInt;

  procedure wstCheck(const AReturn : LongInt);overload;
  procedure wstCheck(const AReturn : LongInt; const AMsg : string);overload;
  
implementation

procedure wstCheck(const AReturn : LongInt);
var
  e : EwstCheckException;
begin
  if ( AReturn <> RET_OK ) then begin
    e := EwstCheckException.CreateFmt('wst Check Exception , return = %d',[AReturn]);
    e.ReturnCode := AReturn;
    raise e;
  end;
end;

procedure wstCheck(const AReturn : LongInt; const AMsg : string);
var
  e : EwstCheckException;
begin
  if ( AReturn <> RET_OK ) then begin
    e := EwstCheckException.Create(AMsg);
    e.ReturnCode := AReturn;
    raise e;
  end;
end;

end.

