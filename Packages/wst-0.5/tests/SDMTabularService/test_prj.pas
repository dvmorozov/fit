program test_prj;

{$mode objfpc}{$H+}

uses
  SysUtils,
  synapse_http_protocol,
  soap_formatter,
  
  SDMTabularService, SDMTabularService_proxy;

var
  SDMSoap    : SDMTabularServiceSoap;
  QueryBuffer   : RunQueryType;
  QryResult  : RunQueryResponse;
  XmlFileVar : TextFile;
begin
  SYNAPSE_RegisterHTTP_Transport();
  
//  if Assigned(InitProc) then
  //  TProcedure(InitProc);

  QueryBuffer := RunQueryType.Create();
  try
    SDMSoap := wst_CreateInstance_SDMTabularServiceSoap();
    if not Assigned(SDMSoap) then
      begin
      WriteLn('Service not assigned');
      Exit;
      end;

    QueryBuffer.Query :=
     'SELECT ' +
     'saversion, saverest, ' +
     'l.areasymbol, l.areaname, l.lkey, ' +
     'mu.musym, mu.muname, museq, mu.mukey, ' +
     'textcat, textsubcat, ' +
     'flodfreqdcd,  wtdepannmin, ' +
     'comppct_r, compname, localphase, slope_r, slope_l, slope_h, hydgrp, tfact, runoff, drainagecl, wei, majcompflag, slopelenusle_r, c.cokey, ' +
     'reskind, resdept_r, ' +
     'hzdept_r, hzdepb_r, om_l, om_h, kffact, kwfact, ch.chkey, ' +
     'texture ' +
     'FROM sacatalog sac ' +
     'INNER JOIN legend l ON l.areasymbol = sac.areasymbol AND l.areasymbol = ''IN001'' ' +
     'INNER JOIN mapunit mu ON mu.lkey = l.lkey ' +
     'LEFT OUTER JOIN mutext mutx ON mutx.mukey = mu.mukey AND mutx.textcat = ''innitrate'' ' +
     'LEFT OUTER JOIN muaggatt muag ON muag.mukey = mu.mukey ' +
     'LEFT OUTER JOIN component c ON c.mukey = mu.mukey AND c.majcompflag = ''Yes'' ' +
     'LEFT OUTER JOIN corestrictions core on core.cokey = c.cokey AND core.reskind IN (''Densic bedrock'', ''Lithic bedrock'', ''Paralithic bedrock'') ' +
     'LEFT OUTER JOIN chorizon ch ON ch.cokey = c.cokey AND ch.hzdept_r = ''0'' ' +
     'LEFT OUTER JOIN chtexturegrp chtg ON chtg.chkey = ch.chkey ' +
     'ORDER BY l.areasymbol, museq, comppct_r DESC, compname, hydgrp, texture';

  //  WriteLn(QueryStr);

    try
      QryResult := SDMSoap.RunQuery(QueryBuffer);
      if not Assigned(QryResult) then
        WriteLn('QryResult not assigned');
    except on E:Exception do
      begin
      WriteLn(E.Message);
      Exit;
      end;
    end;

  //  WriteLn(QryResult.s_schema);

    WriteLn(Copy(QryResult.RunQueryResult.schema.Data, 1, 500));

    AssignFile(XmlFileVar, 'junk.xml');
    Rewrite(XmlFileVar);
    WriteLn(XmlFileVar, QryResult.RunQueryResult.diffgram.Data);
    CloseFile(XmlFileVar);
  
  finally
    QueryBuffer.Free();
  end;
end.
