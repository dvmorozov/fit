unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  AWSECommerceService, StdCtrls, ExtCtrls, Buttons;

type

  { TfMain }

  TfMain = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnSearch: TButton;
    edtAccessID: TEdit;
    edtKeywords: TEdit;
    edtSearchIndex: TEdit;
    edtManufacturer: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mmoRes: TMemo;
    procedure btnSearchClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation
uses soap_formatter,
     synapse_http_protocol,
     AWSECommerceService_proxy,
     metadata_repository;

{ TfMain }

procedure TfMain.btnSearchClick(Sender: TObject);
var
  locService : AWSECommerceServicePortType;
  rqst : ItemSearch_Type;
  rsps : ItemSearchResponse_Type;
  rspsItem : Items_Type;
  i, j, k : Integer;
  itm : Item_Type;
begin
  mmoRes.Clear();
  rsps := nil;
  rqst := ItemSearch_Type.Create();
  try
    Screen.Cursor := crHourGlass;
    locService := wst_CreateInstance_AWSECommerceServicePortType();
    rqst.AWSAccessKeyId := edtAccessID.Text;
    rqst.Request.SetLength(1);
    rqst.Request[0].Manufacturer := edtManufacturer.Text;
    rqst.Request[0].SearchIndex := edtSearchIndex.Text;
    rqst.Request[0].Availability := Available;
    rqst.Request[0].Count := 10;
    rqst.Request[0].MerchantId := 'Amazon';
    rqst.Request[0].ItemPage := 1;
    rqst.Request[0].Keywords := edtKeywords.Text;
    rsps := locService.ItemSearch(rqst);
    if ( rsps.OperationRequest.Errors.Length > 0 ) then begin
      mmoRes.Lines.Add(Format('Errors ( %d ) : ',[rsps.OperationRequest.Errors.Length]));
      for i := 0 to Pred(rsps.OperationRequest.Errors.Length) do begin
        mmoRes.Lines.Add(Format('  Error[%d] :',[i]));
        mmoRes.Lines.Add('    ' + rsps.OperationRequest.Errors[i].Code);
        mmoRes.Lines.Add('    ' + rsps.OperationRequest.Errors[i].Message);
      end;
    end else begin
      mmoRes.Lines.Add(Format('Response ( %d ) : ',[rsps.Items.Length]));
      if Assigned(rsps) then begin
        for i := 0 to Pred(rsps.Items.Length) do begin
          rspsItem := rsps.Items[i];
          mmoRes.Lines.Add('    TotalPages :' + IntToStr(rspsItem.TotalPages));
          mmoRes.Lines.Add('    TotalResults :' + IntToStr(rspsItem.TotalResults));
          mmoRes.Lines.Add('    Items :' + IntToStr(rspsItem._Item.Length));
          mmoRes.Lines.Add('');
          for j := 0 to Pred(rspsItem._Item.Length) do begin
            itm := rspsItem._Item[j];;
            mmoRes.Lines.Add('        ASIN :' + itm.ASIN);
            mmoRes.Lines.Add('        DetailPageURL :' + itm.DetailPageURL);
            if Assigned(itm.ItemAttributes) then begin
              mmoRes.Lines.Add('               Title :' + itm.ItemAttributes.Title);
              for k := 0 to Pred(itm.ItemAttributes.Author.Length) do begin
                mmoRes.Lines.Add('               Author[ ' + IntToStr(k) + ' ] ' + itm.ItemAttributes.Author.Item[k]);
              end;
              mmoRes.Lines.Add('               Manufacturer :' + itm.ItemAttributes.Manufacturer);
              mmoRes.Lines.Add('               ProductGroup :' + itm.ItemAttributes.ProductGroup);
            end;
            mmoRes.Lines.Add('');
          end;
        end;
        mmoRes.SelStart := 0;
      end else begin
        ShowMessage('not Assigned(rsps)');
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(rqst);
    FreeAndNil(rsps);
  end;
end;


initialization
  {$I umain.lrs}

  SYNAPSE_RegisterHTTP_Transport();
  
end.
