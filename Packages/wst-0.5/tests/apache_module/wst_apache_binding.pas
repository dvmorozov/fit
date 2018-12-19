//{$DEFINE WST_DBG}

unit wst_apache_binding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  httpd, apr, apriconv, aprutil;

const
  sWST_ROOT = 'wst';
  sSEPARATOR = '/';
  sSERVICES_PREFIXE = 'services';
  sWSDL = 'WSDL';
  sHTTP_BINARY_CONTENT_TYPE = 'application/octet-stream';
  sCONTENT_TYPE = 'Content-Type';

  function wst_RequestHandler(r: Prequest_rec): Integer;

implementation
uses base_service_intf,
     server_service_intf, server_service_imputils,
     server_service_soap, server_binary_formatter,
     metadata_repository, metadata_wsdl, DOM, XMLWrite,
     
     calculator, calculator_binder, calculator_imp,
     metadata_service, metadata_service_binder, metadata_service_imp;

type
  TRequestInfo = record
    Root        : string;
    URI         : string;
    ContentType : string;
    Buffer      : string;
  end;
  
  TResponseInfo = record
    ContentText : string;
    ContentType : string;
  end;


procedure SaveStringToFile(const AStr,AFile:string;const AKeepExisting : Boolean);
begin
  with TMemoryStream.Create() do try
    if AKeepExisting and FileExists(AFile) then begin
      LoadFromFile(AFile);
      Position := Size;
    end;
    if ( Length(AStr) > 0 ) then
      Write(AStr[1],Length(AStr));
    SaveToFile(AFile);
  finally
    Free();
  end;
end;

function ReadBuffer(r : Prequest_rec; out rbuf : string ) : Integer;
var
  argsbuffer : string;
  rsize, len_read, rpos : Integer;
  loc_length : Integer;
begin
  rbuf := '';
  Result := ap_setup_client_block(r, REQUEST_CHUNKED_ERROR);
  if ( Result <> OK ) then
    Exit;

  if ( ap_should_client_block(r) <> 0 ) then begin
    SetLength(argsbuffer,HUGE_STRING_LEN);
    FillChar(argsbuffer[1],Length(argsbuffer),0);
    rsize := 0; len_read := 0; rpos := 0;
    loc_length := r^.remaining;
    SetLength(rbuf, loc_length );

     while True do begin
       len_read := ap_get_client_block(r, @(argsbuffer[1]), Length(argsbuffer));
       if ( len_read <= 0 ) then
         Exit;
        if ( ( rpos + len_read ) > loc_length ) then
          rsize := loc_length - rpos
        else
          rsize := len_read;
        Move(argsbuffer[1],rbuf[ ( 1 + rpos ) ], rsize);
        Inc(rpos,rsize);
    end;
  end;
end;

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

function GenerateWSDLTable(): string;
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
                  '<table width="100%">';

  for i := 0 to Pred(r.GetCount()) do
    Result := Result + '<tr>' +
                        '<td>' +
                            Format('<a href="%s">',[sSEPARATOR+sWST_ROOT+sSEPARATOR+sSERVICES_PREFIXE+sSEPARATOR+sWSDL+sSEPARATOR+r.GetRepositoryName(i)])+
                            r.GetRepositoryName(i) +
                            '</a>'+
                        '</td>' +
                      '</tr>';

  Result := Result +
                    '</tr>'+
                  '</table>'+
                '</body>'+
              '</head>'+
            '</html>';
end;

procedure ProcessWSDLRequest(
  const ARequestInfo    : TRequestInfo;
  out   AResponseInfo   : TResponseInfo
);
var
  locRepName, strBuff, locPath : string;
  i : Integer;
begin
  FillChar(AResponseInfo,SizeOf(TResponseInfo),#0);
  locPath := ARequestInfo.URI;
  locRepName := ExtractNextPathElement(locPath);
  if AnsiSameText(sWSDL,locRepName) then
    locRepName := ExtractNextPathElement(locPath);
  strBuff := GetWSDL(locRepName,ARequestInfo.Root);
  i := Length(strBuff);
  if ( i > 0 ) then begin
    AResponseInfo.ContentType := 'text/xml';
    AResponseInfo.ContentText := strBuff;
    Exit;
  end;
  AResponseInfo.ContentText := GenerateWSDLTable();
  AResponseInfo.ContentType := 'text/html';
end;

function ProcessServiceRequest(
  const ARequestInfo    : TRequestInfo;
  out   AResponseInfo   : TResponseInfo
):Boolean;
var
  trgt,ctntyp, loc_path : string;
  rqst : IRequestBuffer;
  inStream, outStream: TMemoryStream;
  i : Integer;
begin
  FillChar(AResponseInfo,SizeOf(TResponseInfo),#0);
  loc_path := ARequestInfo.URI;
  trgt := ExtractNextPathElement(loc_path);
  Result := False;
  if AnsiSameText(sWSDL,trgt) then
    Exit;
  Result := True;
  inStream := nil;
  outStream := nil;
  try
    inStream := TMemoryStream.Create();
    outStream := TMemoryStream.Create();

    ctntyp := ARequestInfo.ContentType;
    i := Length(ARequestInfo.Buffer);
    if ( i > 0 ) then
      inStream.Write(ARequestInfo.Buffer[1],i);
    inStream.Position := 0;
    if AnsiSameText(sBINARY_CONTENT_TYPE,ctntyp) then
      AResponseInfo.ContentType := sHTTP_BINARY_CONTENT_TYPE
    else
      AResponseInfo.ContentType := ctntyp;
    rqst := TRequestBuffer.Create(trgt,ctntyp,inStream,outStream);
    HandleServiceRequest(rqst);
    i := outStream.Size;
    if ( i > 0 ) then begin
      SetLength(AResponseInfo.ContentText,i);
      Move(outStream.Memory^,AResponseInfo.ContentText[1],i);
    end;
  finally
    outStream.Free();
    inStream.Free();
    {$IFDEF WST_DBG}
      {SaveStringToFile('RequestInfo.ContentType=' + ARequestInfo.ContentType + LineEnding,'E:\Inoussa\Sources\lazarus\wst\v0.3\tests\apache_module\log.log',False);
      SaveStringToFile('RequestInfo.Buffer=' + ARequestInfo.Buffer + LineEnding,'E:\Inoussa\Sources\lazarus\wst\v0.3\tests\apache_module\log.log',True);
      SaveStringToFile('RequestInfo.URI=' + ARequestInfo.URI + LineEnding,'E:\Inoussa\Sources\lazarus\wst\v0.3\tests\apache_module\log.log',True);
      SaveStringToFile('ResponseInfo.ContentType=' + AResponseInfo.ContentType + LineEnding,'E:\Inoussa\Sources\lazarus\wst\v0.3\tests\apache_module\log.log',True);
      SaveStringToFile('ResponseInfo.ContentText=' + AResponseInfo.ContentText + LineEnding,'E:\Inoussa\Sources\lazarus\wst\v0.3\tests\apache_module\log.log',True);
      }
    {$ENDIF}
  end;
end;

function wst_RequestHandler(r: Prequest_rec): Integer;

  function FillRequestInfo(var ARequestInfo    : TRequestInfo):Integer;
  begin
    ARequestInfo.ContentType := apr_table_get(r^.headers_in,sCONTENT_TYPE);
    ARequestInfo.Root := ap_get_server_name(r) + sSEPARATOR + sWST_ROOT + sSEPARATOR;
    ARequestInfo.URI := r^.uri;
    Result := ReadBuffer(r,ARequestInfo.Buffer);
  end;

var
  sInputBuffer : string;
  iRet, iLen : Integer;
  loc_RequestInfo    : TRequestInfo;
  loc_ResponseInfo   : TResponseInfo;
begin
  Result := FillRequestInfo(loc_RequestInfo);
  if not AnsiSameText(sWST_ROOT,ExtractNextPathElement(loc_RequestInfo.URI)) then
    Result := DECLINED;
  if ( Result <> OK ) then
    Exit;

  try
    if AnsiSameText(sSERVICES_PREFIXE,ExtractNextPathElement(loc_RequestInfo.URI)) then begin
      if not ProcessServiceRequest(loc_RequestInfo,loc_ResponseInfo) then
        ProcessWSDLRequest(loc_RequestInfo,loc_ResponseInfo);
    end else begin
      ProcessWSDLRequest(loc_RequestInfo,loc_ResponseInfo);
    end;

    ap_set_content_type(r, PCHAR(loc_ResponseInfo.ContentType));
    if AnsiSameText(sHTTP_BINARY_CONTENT_TYPE,loc_ResponseInfo.ContentType) then begin
      ap_set_content_length(r,Length(loc_ResponseInfo.ContentText));
      ap_rwrite(@(loc_ResponseInfo.ContentText[1]),Length(loc_ResponseInfo.ContentText),r);
      ap_rflush(r);
    end else begin
      ap_rputs(PCHAR(loc_ResponseInfo.ContentText), r);
    end;
    Result := OK;
    Exit;
  except
    on e : Exception do begin
      ap_set_content_type(r, 'text/html');
      ap_rputs('<HTML><HEAD> <TITLE>Error</TITLE></HEAD>' + LineEnding, r);
      ap_rputs('<BODY></BODY></HTML>',r);
      ap_rprintf(r, '<BODY><H1>"%s"</H1></BODY></HTML>' + LineEnding, [PCHAR(e.Message)]);
      Exit;
    end;
  end;
end;

initialization
  RegisterStdTypes();
  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();

  RegisterCalculatorImplementationFactory();
  Server_service_RegisterCalculatorService();

  Server_service_RegisterWSTMetadataServiceService();
  RegisterWSTMetadataServiceImplementationFactory();

end.
