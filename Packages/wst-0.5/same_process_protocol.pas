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
unit same_process_protocol;

interface

uses
  Classes, SysUtils,
  service_intf, imp_utils,
  server_service_intf, server_service_imputils, base_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

Const
  sTRANSPORT_NAME = 'SAME_PROCESS';

Type

{$M+}

  { TInProcessTransport }

  TInProcessTransport = class(TSimpleFactoryItem,ITransport)
  Private
    FAdress: string;
    FContentType: string;
    FFormat : string;
    FPropMngr : IPropertyManager;
  Public
    constructor Create();override;
    destructor Destroy();override;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
  Published
    property ContentType : string Read FContentType Write FContentType;
    property Adress : string Read FAdress Write FAdress;
    property Format : string read FFormat write FFormat;
  End;
{$M+}

  procedure SAME_PROCESS_Register_Local_Transport();

implementation

{ TInProcessTransport }

constructor TInProcessTransport.Create();
begin
  FPropMngr := TPublishedPropertyManager.Create(Self);
end;

destructor TInProcessTransport.Destroy();
begin
  FPropMngr := Nil;
  inherited Destroy();
end;

function TInProcessTransport.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropMngr;
end;

procedure TInProcessTransport.SendAndReceive(ARequest, AResponse: TStream);
Var
  bffr : IRequestBuffer;
{$IFDEF WST_DBG}
  s : string;
  i : Int64;
{$ENDIF WST_DBG}
begin
  bffr := TRequestBuffer.Create(Adress,ContentType,ARequest,AResponse,Format);
  HandleServiceRequest(bffr);
  {$IFDEF WST_DBG}
  i := AResponse.Position;
  SetLength(s,AResponse.Size);
  AResponse.Read(s[1],AResponse.Size);
  WriteLn(s);
  {$ENDIF WST_DBG}
end;

procedure SAME_PROCESS_Register_Local_Transport();
begin
  GetTransportRegistry().Register(sTRANSPORT_NAME,TSimpleItemFactory.Create(TInProcessTransport) as IItemFactory);
end;

end.
