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
unit server_listener;

interface
uses
  Classes, SysUtils;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

const
  sSEPARATOR = '/';
  sSERVICES_PREFIXE = 'services';
  sWSDL = 'WSDL';
  
type

  TListnerNotifyMessage = procedure(Sender : TObject; const AMsg : string) of object;

  TwstListener = class(TObject)
  private
    FOnNotifyMessage: TListnerNotifyMessage;
  public
    class function GetDescription() : string;virtual;
    procedure Start();virtual;abstract;
    procedure Stop();virtual;abstract;
    procedure NotifyMessage(const AMsg : string);
    property OnNotifyMessage : TListnerNotifyMessage read FOnNotifyMessage write FOnNotifyMessage;
  end;

  function GenerateWSDLHtmlTable(): string;
  
implementation
uses base_service_intf, metadata_repository,
     metadata_service, metadata_service_binder, metadata_service_imp ;


function GenerateWSDLHtmlTable(): string;
var
  r : IModuleMetadataMngr;
  i : Integer;
begin
  r := GetModuleMetadataMngr();
  Result := '<html>' +
              '<head>'+
                '<title>'+
                  'The Web Services Toolkit generated Metadata table'+
                '</title>'+
                '<body>' +
                  '<p BGCOLOR="#DDEEFF"><FONT FACE="Arial" COLOR="#0000A0" SIZE="+2">The following repositories has available. Click on the link to view the corresponding WSDL.</FONT></p>'+
                  '<table width="100%">';

  for i := 0 to Pred(r.GetCount()) do begin
    Result := Result +
                '<tr>' +
                      '<td align="left">' +
                          Format('<a href="%s">',[sSEPARATOR+sSERVICES_PREFIXE+sSEPARATOR+sWSDL+sSEPARATOR+r.GetRepositoryName(i)])+
                          r.GetRepositoryName(i) +
                          '</a>'+
                      '</td>' +
                '</tr>';
  end;
  Result := Result +

                  '</table>'+
                '</body>'+
              '</head>'+
            '</html>';
end;

{ TwstListener }

class function TwstListener.GetDescription() : string;
begin
  Result := ClassName;
end;

procedure TwstListener.NotifyMessage(const AMsg: string);
begin
  if Assigned(FOnNotifyMessage) then
    FOnNotifyMessage(Self,AMsg);
end;

initialization
  RegisterStdTypes();
  RegisterWSTMetadataServiceImplementationFactory();
  Server_service_RegisterWSTMetadataServiceService();
  
end.
