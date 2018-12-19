program test_google_api;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  base_service_intf, service_intf, soap_formatter,
  //indy_http_protocol,
  ics_http_protocol,
  synapse_http_protocol,
  googlewebapi, googlewebapi_proxy, wst_resources_imp;

Const
  //sADRESS = 'http:Address=http://api.google.com/search/beta2;Proxy';
  sADDRESS  = 'http:Address=http://api.google.com/search/beta2';//+
              //';ProxyServer=10.0.0.5;ProxyPort=8080';
  sTARGET = 'urn:GoogleSearch';
  sKEY    = <your key here>;
  sSERVICE_PROTOCOL = 'SOAP:style=rpc';
Var
  tmpObj : IGoogleSearch;
  qryRes : TGoogleSearchResult;
  strBuffer : string;
  i , c: Integer;
  resElt : TResultElement;
  resDir : TDirectoryCategory;
begin
  //ICS_RegisterHTTP_Transport();
  SYNAPSE_RegisterHTTP_Transport();
  //INDY_RegisterHTTP_Transport();
  WriteLn();
  WriteLn('Enter phrase to spell :');
  ReadLn(strBuffer);
  tmpObj := TGoogleSearch_Proxy.Create(sTARGET,sSERVICE_PROTOCOL,sADDRESS);
  Try
    strBuffer := tmpObj.doSpellingSuggestion(sKEY,strBuffer);
    WriteLn('google spell >>> ',strBuffer);
  Except
    On E : Exception Do
      WriteLn(E.Message);
  End;

  WriteLn();
  WriteLn('Enter phrase to search :');
  ReadLn(strBuffer);
  Try
    qryRes := tmpObj.doGoogleSearch(sKEY,strBuffer,0,10,True,'',False,'','latin1','latin1');
    Try
      WriteLn('---------------------------------------');
      WriteLn('google Search >>');
        WriteLn('documentFiltering = ',qryRes.documentFiltering);
        WriteLn('startIndex = ',qryRes.startIndex);
        WriteLn('endIndex = ',qryRes.endIndex);
        WriteLn('estimatedTotalResultsCount = ',qryRes.estimatedTotalResultsCount);
        WriteLn('estimateIsExact = ',qryRes.estimateIsExact);
        WriteLn('searchComments = ',qryRes.searchComments);
        WriteLn('searchQuery = ',qryRes.searchQuery);
        WriteLn('searchTime = ',qryRes.searchTime);
        WriteLn('searchTips = ',qryRes.searchTips);

        WriteLn('-------------------------------------------');
        WriteLn('directoryCategories >>');
          WriteLn(' Length = ',qryRes.directoryCategories.Length);
          c := qryRes.directoryCategories.Length;
          For i := 0 To Pred(c) Do Begin
            resDir := qryRes.directoryCategories[i];
            WriteLn('');
            WriteLn('Item[',i,'] >>');
            WriteLn('    fullViewableName = ',resDir.fullViewableName);
            WriteLn('    specialEncoding = ',resDir.specialEncoding);
          End;

        WriteLn('-------------------------------------------');
        WriteLn('resultElements >>');
          WriteLn(' Length = ',qryRes.resultElements.Length);
          c := qryRes.resultElements.Length;
          For i := 0 To Pred(c) Do Begin
            resElt := qryRes.resultElements[i];
            WriteLn('');
            WriteLn('Item[',i,'] >>');
            WriteLn('    cachedSize = ',resElt.cachedSize);
            WriteLn('    directoryTitle = ',resElt.directoryTitle);
            WriteLn('    hostName = ',resElt.hostName);
            WriteLn('    relatedInformationPresent = ',resElt.relatedInformationPresent);
            WriteLn('    snippet = ',resElt.snippet);
            WriteLn('    summary = ',resElt.summary);
            WriteLn('    title = ',resElt.title);
            WriteLn('    URL = ',resElt.URL);
          End;
    Finally
      qryRes.Free();
    End;
  Except
    On E : Exception Do
      WriteLn(E.Message);
  End;

  ReadLn();
end.
