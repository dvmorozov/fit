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
unit logger_extension;

interface

uses
  Classes, SysUtils, base_service_intf, server_service_intf;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  { TLoggerServiceExtension }

  TLoggerServiceExtension = class(TSimpleFactoryItem,IServiceExtension)
  private
    procedure TraceMessage(const AMsg : string);
  protected
    procedure ProcessMessage(
      const AMessageStage  : TMessageStage;
            ACallContext   : ICallContext;
            AMsgData       : IInterface
              { The "AMsgData" parameter actual type depends on the message state
                on correspond to :
                  - IRequestBuffer on "msBeforeDeserialize" and "msAfterSerialize"
                  - IFormatterResponse on "msAfterDeserialize", "msBeforeSerialize"
              }
    );
  end;
  
implementation
uses TypInfo;

{ TLoggerServiceExtension }

procedure TLoggerServiceExtension.TraceMessage(const AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure TLoggerServiceExtension.ProcessMessage(
  const AMessageStage: TMessageStage;
        ACallContext: ICallContext;
        AMsgData: IInterface
);
var
  s : string;
  rqb : IRequestBuffer;
  frmtr : IFormatterResponse;
begin
  s := GetEnumName(TypeInfo(TMessageStage),Ord(AMessageStage));
  case AMessageStage of
    msBeforeDeserialize, msAfterSerialize :
      begin
        rqb := AMsgData as IRequestBuffer;
        s := Format('Called service : "%s";      Processing stage : "%s"',[rqb.GetTargetService(),s]);
      end;
    msAfterDeserialize, msBeforeSerialize :
      begin
        frmtr := AMsgData as IFormatterResponse;
        s := Format('Called service : "%s";   Target Operation = "%s";    Processing stage : "%s"',[frmtr.GetCallTarget(),frmtr.GetCallProcedureName(),s]);
      end;
  end;
  TraceMessage(s);
end;

initialization
  GetServiceExtensionRegistry().Register('TLoggerServiceExtension',TSimpleItemFactory.Create(TLoggerServiceExtension) as IItemFactory);
  
end.
