program test_ebay;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  service_intf, soap_formatter, base_service_intf, base_soap_formatter,
  //ics_http_protocol,
  //indylaz,
  //indy_http_protocol,
  ebay, ebay_proxy,
  ssl_openssl, synapse_http_protocol;

var
  locService : IeBayAPIInterfaceService;
  locHdr : TCustomSecurityHeaderType;
  r : TGetCategoriesRequestType;
  rsp : TGetCategoriesResponseType;
begin
  //Indy_RegisterHTTP_Transport();
  SYNAPSE_RegisterHTTP_Transport();
  r := nil;
  rsp := nil;
  locHdr := TCustomSecurityHeaderType.Create();
  try
    locHdr.eBayAuthToken :=  '...';

    locHdr.Credentials.AppId := '...';
    locHdr.Credentials.DevId := '...';
    locHdr.Credentials.AuthCert := '...';
    locService := TeBayAPIInterfaceService_Proxy.Create(
                    'eBayAPIInterfaceService',
                    'SOAP:Style=Document;EncodingStyle=Litteral',
                    'http:Address=https://api.sandbox.ebay.com/wsapi?callname=GetCategories&siteid=0&appid=INOUSSAOUEU258CIC9Z5E83UXC1BE5&version=467'
                  );   //https://api.sandbox.ebay.com/wsapi
                       //https://api.sandbox.ebay.com/wsapi
                       //https://api.sandbox.ebay.com/ws/api.dll
    (locService as ICallContext).AddHeader(locHdr,True);
    r := TGetCategoriesRequestType.Create();
    r.Version := '467';
    try
      locService.GetCategories(r,rsp);
    except
      on e : Exception do begin
        WriteLn('Exception : ',e.Message);
        raise;
      end;
    end;
  finally
    r.Free();
    rsp.Free();
  end;
end.

