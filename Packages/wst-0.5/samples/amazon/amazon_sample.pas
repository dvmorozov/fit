program amazon_sample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  soap_formatter,
  synapse_http_protocol,
  metadata_repository,
  AWSECommerceService, AWSECommerceService_proxy;

const sACCES_ID = <your key here>;

function ReadEntry(const APromp : string):string ;
begin
  Result := '';
  Write(APromp);
  while True do begin
    ReadLn(Result);
    Result := Trim(Result);
    if ( Length(Result) > 0 ) then
      Break;
  end;
end;

var
  locService : AWSECommerceServicePortType;
  rqst : ItemSearch_Type;
  rsps : ItemSearchResponse_Type;
  rspsItem : Items_Type;
  i, j, k : Integer;
  itm : Item_Type;
begin
  SYNAPSE_RegisterHTTP_Transport();
  WriteLn('Web Services Toolkit Amazon sample');
  WriteLn('This sample demonstrates the "ItemSearch" method of the Amazon web service');
  WriteLn();
  rqst := ItemSearch_Type.Create();
  try
    locService := wst_CreateInstance_AWSECommerceServicePortType();
    rqst.AWSAccessKeyId := sACCES_ID;
    while True do begin
      rqst.Request.SetLength(1);
      rqst.Request[0].SearchIndex := ReadEntry('Enter the Search Index : ');
      rqst.Request[0].Availability := Available;
      rqst.Request[0].Count := 10;
      rqst.Request[0].MerchantId := 'Amazon';
      rqst.Request[0].ItemPage := 1;
      rqst.Request[0].Keywords := ReadEntry('Enter the Keywords : ');
      rsps := locService.ItemSearch(rqst);
      if ( rsps.OperationRequest.Errors.Length > 0 ) then begin
        WriteLn(Format('Errors ( %d ) : ',[rsps.OperationRequest.Errors.Length]));
        for i := 0 to Pred(rsps.OperationRequest.Errors.Length) do begin
          WriteLn(Format('  Error[%d] :',[i]));
          WriteLn('    ' + rsps.OperationRequest.Errors[i].Code);
          WriteLn('    ' + rsps.OperationRequest.Errors[i].Message);
        end;
      end else begin
        WriteLn(Format('Response ( %d ) : ',[rsps.Items.Length]));
        if Assigned(rsps) then begin
          for i := 0 to Pred(rsps.Items.Length) do begin
            rspsItem := rsps.Items[i];
            WriteLn('    TotalPages :' + IntToStr(rspsItem.TotalPages));
            WriteLn('    TotalResults :' + IntToStr(rspsItem.TotalResults));
            WriteLn('    Items :' + IntToStr(rspsItem._Item.Length));
            WriteLn('');
            for j := 0 to Pred(rspsItem._Item.Length) do begin
              itm := rspsItem._Item[j];;
              WriteLn('        ASIN :' + itm.ASIN);
              WriteLn('        DetailPageURL :' + itm.DetailPageURL);
              if Assigned(itm.ItemAttributes) then begin
                WriteLn('               Title :' + itm.ItemAttributes.Title);
                for k := 0 to Pred(itm.ItemAttributes.Author.Length) do begin
                  WriteLn('               Author[ ' + IntToStr(k) + ' ] ' + itm.ItemAttributes.Author.Item[k]);
                end;
                WriteLn('               Manufacturer :' + itm.ItemAttributes.Manufacturer);
                WriteLn('               ProductGroup :' + itm.ItemAttributes.ProductGroup);
              end;
              WriteLn('');
            end;
          end;
        end else begin
          WriteLn('Unexpected service response : Invalid response');
        end;
      end;
      WriteLn();
      WriteLn();
      if ( UpperCase(ReadEntry('Continue ( Y/N ) :'))[1] <> 'Y' ) then
        Break;
    end;
  finally
    FreeAndNil(rqst);
    FreeAndNil(rsps);
  end;
  ReadLn;
end.

