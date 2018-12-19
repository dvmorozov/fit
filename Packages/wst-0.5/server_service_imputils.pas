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
unit server_service_imputils;

interface

uses
  Classes, SysUtils, TypInfo,
  server_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}
  
Type

  { TRequestBuffer }

  TRequestBuffer = class(TInterfacedObject,IRequestBuffer)
  private
    FTargetService : string;
    FContentType   : string;
    FFormat        : string;
    FContent       : TStream;
    FResponse      : TStream;
  protected
    function GetTargetService():string;
    function GetContentType():string;
    //function GetLength():Integer;
    function GetContent():TStream;
    function GetResponse():TStream;
    function GetFormat() : string;
  public
    constructor Create(
      const ATargetService : string;
      const AContentType   : string;
            AContent       : TStream;
            AResponse      : TStream;
            AFormat        : string
    );
  end;


  function IsStrEmpty(Const AStr:String):Boolean;

implementation

function IsStrEmpty(Const AStr:String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

{ TRequestBuffer }

function TRequestBuffer.GetTargetService(): string;
begin
  Result := FTargetService;
end;

function TRequestBuffer.GetContentType(): string;
begin
  Result := FContentType;
end;

{function TRequestBuffer.GetLength(): Integer;
begin
  Result := FLength;
end;}

function TRequestBuffer.GetContent(): TStream;
begin
  Result := FContent;
end;

function TRequestBuffer.GetResponse(): TStream;
begin
  Result := FResponse;
end;

function TRequestBuffer.GetFormat(): string;
begin
  Result := FFormat;
end;

constructor TRequestBuffer.Create(
  const ATargetService : string;
  const AContentType   : string;
        AContent       : TStream;
        AResponse      : TStream;
        AFormat        : string
);
begin
  FTargetService := ATargetService;
  FContentType   := AContentType;
  FFormat        := AFormat;
  FContent       := AContent;
  FResponse      := AResponse;
end;




end.
