{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit app_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IdCustomHTTPServer,
  IdHTTPServer, IdContext, IdSocketHandle;

type

  { TwstWebApplication }

  TwstWebApplication = class(TObject)
  private
    FHTTPServerObject: TIdHTTPServer;
    FRootAddress : string;
  private
    function GenerateWSDLTable():string;

    procedure ProcessWSDLRequest(
          AContext        : TIdContext;
          ARequestInfo    : TIdHTTPRequestInfo;
          AResponseInfo   : TIdHTTPResponseInfo;
      var APath           : string
    );
    procedure ProcessServiceRequest(
          AContext        : TIdContext;
          ARequestInfo    : TIdHTTPRequestInfo;
          AResponseInfo   : TIdHTTPResponseInfo;
      var APath           : string
    );
  private
    procedure Handler_CommandGet(
      AContext        : TIdContext;
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo
    );
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Display(const AMsg : string);
  end;


implementation
uses base_service_intf,
     server_service_intf, server_service_imputils,
     server_service_soap, server_binary_formatter,
     metadata_repository, metadata_wsdl, DOM, XMLWrite,
     calculator, calculator_binder, calculator_imp,
     metadata_service, metadata_service_binder, metadata_service_imp,

     user_service_intf, user_service_intf_binder, user_service_intf_imp;

const
  sSEPARATOR = '/';
  sSERVICES_PREFIXE = 'services';
  sWSDL = 'WSDL';

function ExtractNextPathElement(var AFullPath : string):string;
var
  i : SizeInt;
begin
  Result := '';
  if ( Length(AFullPath) > 0 ) then begin
    while ( Length(AFullPath) > 0 ) and ( AFullPath[1] = sSEPARATOR ) do begin
      Delete(AFullPath,1,1);
    end;
    i := Pos(sSEPARATOR,AFullPath);
    if ( i < 1 ) then begin
      Result := AFullPath;
      AFullPath := '';
    end else begin
      Result := Copy(AFullPath,1,Pred(i));
      Delete(AFullPath,1,i);
    end;
  end;
end;

function GetWSDL(const ARepName, ARootAddress: shortstring):string;
var
  strm : TMemoryStream;
  rep : PServiceRepository;
  doc :TXMLDocument;
  i : SizeInt;
  s : string;
begin
  Result := '';
  rep := nil;
  doc := Nil;
  i := GetModuleMetadataMngr().IndexOfName(ARepName);
  if ( i < 0 ) then
    Exit;
  strm := TMemoryStream.Create();
  try
    s := GetModuleMetadataMngr().GetRepositoryName(i);
    GetModuleMetadataMngr().LoadRepositoryName(s,ARootAddress,rep);
    //if ( GetModuleMetadataMngr().LoadRepositoryName(s,rep) > 0 ) then
      //rep^.namespace := 'urn:wst';
    strm.Clear();
    doc := TXMLDocument.Create();
    GenerateWSDL(rep,doc);
    WriteXMLFile(doc,strm);
    i := strm.Size;
    if ( i > 0 ) then begin
      SetLength(Result,i);
      Move(strm.memory^,Result[1],i);
    end;
  finally
    doc.Free();
    strm.Free();
    GetModuleMetadataMngr().ClearRepository(rep);
  end;
end;


{ TwstWebApplication }

function TwstWebApplication.GenerateWSDLTable(): string;
var
  r : IModuleMetadataMngr;
  i : Integer;
begin
  r := GetModuleMetadataMngr();
  Result := '<html>' +
              '<head>'+
                '<title>'+
                  'The Web Service Toolkit generated Metadata table'+
                '</title>'+
                '<body>' +
                  '<p BGCOLOR="#DDEEFF"><FONT FACE="Arial" COLOR="#0000A0" SIZE="+2">The following repositories has available. Click on the link to view the corresponding WSDL.</FONT></p>'+
                  '<table width="100%">' +
                    '<tr>';
                    
  for i := 0 to Pred(r.GetCount()) do
    Result := Result + '<td align="center">' +
                          Format('<a href="%s">',[sSEPARATOR+sSERVICES_PREFIXE+sSEPARATOR+sWSDL+sSEPARATOR+r.GetRepositoryName(i)])+
                          r.GetRepositoryName(i) +
                          '</a>'+
                      '</td>';
    
  Result := Result +
                    '</tr>'+
                  '</table>'+
                '</body>'+
              '</head>'+
            '</html>';
end;

procedure TwstWebApplication.ProcessWSDLRequest(
      AContext        : TIdContext;
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo;
  var APath           : string
);
var
  locRepName, strBuff : string;
  i : Integer;
begin
  locRepName := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(APath);
  strBuff := GetWSDL(locRepName,FRootAddress);
  i := Length(strBuff);
  if ( i > 0 ) then begin
    AResponseInfo.ContentType := 'text/xml';
    if not Assigned(AResponseInfo.ContentStream) then
      AResponseInfo.ContentStream := TMemoryStream.Create();
    AResponseInfo.ContentStream.Write(strBuff[1],i);
    Exit;
  end;
  AResponseInfo.ContentText := GenerateWSDLTable();
  AResponseInfo.ContentType := 'text/html';
end;

procedure TwstWebApplication.ProcessServiceRequest(
      AContext        : TIdContext;
      ARequestInfo    : TIdHTTPRequestInfo;
      AResponseInfo   : TIdHTTPResponseInfo;
  var APath           : string
);
var
  trgt,ctntyp : string;
  rqst : IRequestBuffer;
  inStream: TMemoryStream;
begin
  trgt := ExtractNextPathElement(APath);
  if AnsiSameText(sWSDL,trgt) then begin
    ProcessWSDLRequest(AContext,ARequestInfo,AResponseInfo,APath);
    Exit;
  end;
  inStream := nil;
  try
    try
      inStream := TMemoryStream.Create();
      AResponseInfo.ContentStream := TMemoryStream.Create();

      ctntyp := ARequestInfo.ContentType;
      inStream.CopyFrom(ARequestInfo.PostStream,0);
      inStream.Position := 0;
      AResponseInfo.ContentType := ctntyp;
      rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,AResponseInfo.ContentStream);
      HandleServiceRequest(rqst);
    finally
      inStream.Free();
    end;
  except
    on e : Exception do begin
      Display('ProcessData()>> Exception = '+e.Message);
      raise;
    end;
  end;
end;

procedure TwstWebApplication.Handler_CommandGet(
  AContext       : TIdContext;
  ARequestInfo   : TIdHTTPRequestInfo;
  AResponseInfo  : TIdHTTPResponseInfo
);
var
  locPath, locPathPart, s : string;
  j : SizeInt;
begin
  if Assigned(ARequestInfo.PostStream) and ( ARequestInfo.PostStream.Size > 0 ) then begin
    j := ARequestInfo.PostStream.Size;
    SetLength(s,j);
    ARequestInfo.PostStream.Read(s[1],j);
    Display('----------- QUERY ----------------------');
    Display(s);
  end;
  locPath := ARequestInfo.Document;
  locPathPart := ExtractNextPathElement(locPath);
  if AnsiSameText(sSERVICES_PREFIXE,locPathPart)  then begin
    ProcessServiceRequest(AContext,ARequestInfo,AResponseInfo,locPath);
    if Assigned(AResponseInfo.ContentStream) and ( AResponseInfo.ContentStream.Size > 0 ) then begin
      j := AResponseInfo.ContentStream.Size;
      SetLength(s,j);
      AResponseInfo.ContentStream.Position := 0;
      AResponseInfo.ContentStream.Read(s[1],j);
      Display('--------- RESPONSE ------------------------');
      Display(s);
    end;
    Exit;
  end;

  ProcessWSDLRequest(AContext,ARequestInfo,AResponseInfo,locPath);
end;

constructor TwstWebApplication.Create();
var
  b : TIdSocketHandle;
begin
  inherited Create();
  FHTTPServerObject := TIdHTTPServer.Create();
  b := FHTTPServerObject.Bindings.Add();
  b.IP:='127.0.0.1';
  b.port:=8000;
  FRootAddress := 'http://127.0.0.1:8000/';
  
  FHTTPServerObject.DefaultPort := 25000;
  FHTTPServerObject.ServerSoftware := 'Web Service Toolkit Sample WebServer';
  FHTTPServerObject.Active := True;
  FHTTPServerObject.OnCommandGet := @Handler_CommandGet;
  
  Server_service_RegisterUserServiceService();
  RegisterUserServiceImplementationFactory();
end;

destructor TwstWebApplication.Destroy();
begin
  FreeAndNil(FHTTPServerObject);
  inherited Destroy();
end;

procedure TwstWebApplication.Display(const AMsg: string);
begin
  WriteLn(AMsg);
end;

initialization
  RegisterStdTypes();
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  
  Server_service_RegisterCalculatorService();
  RegisterCalculatorImplementationFactory();
  
  Server_service_RegisterWSTMetadataServiceService();
  RegisterWSTMetadataServiceImplementationFactory();

  
  
end.
