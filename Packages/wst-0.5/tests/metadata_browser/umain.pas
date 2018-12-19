unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, metadata_service, ComCtrls;

type

  { TfMain }

  TfMain = class(TForm)
    actGetRepositoryList: TAction;
    actGetRepository: TAction;
    AL: TActionList;
    btnGetRepList: TButton;
    Button1: TButton;
    edtRepositoryList: TComboBox;
    edtAddress: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    pnlClient: TPanel;
    pnlHead: TPanel;
    edtFormat: TRadioGroup;
    tvwMetadata: TTreeView;
    procedure actGetRepositoryExecute(Sender: TObject);
    procedure actGetRepositoryListExecute(Sender: TObject);
    procedure actGetRepositoryUpdate(Sender: TObject);
  private
    function CreateMetadataObject():IWSTMetadataService;
    procedure LoadRepository(ARep : TWSTMtdRepository);
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation
uses base_service_intf, service_intf,
     soap_formatter, binary_formatter,
     synapse_http_protocol, //indy_http_protocol, ics_http_protocol,
     ics_tcp_protocol,
     library_protocol,
     metadata_service_proxy;
     
{ TfMain }

procedure TfMain.actGetRepositoryListExecute(Sender: TObject);
var
  tmpObj : IWSTMetadataService;
  locList : TArrayOfStringRemotable;
  i : Integer;
begin
  tmpObj := CreateMetadataObject();
  locList := tmpObj.GetRepositoryList();
  edtRepositoryList.Items.Clear();
  for i := 0 to Pred(locList.Length) do begin
    edtRepositoryList.Items.Add(locList.Item[i]);
  end;
end;

procedure TfMain.actGetRepositoryExecute(Sender: TObject);
var
  rd : TWSTMtdRepository;
begin
  rd := CreateMetadataObject().GetRepositoryInfo(edtRepositoryList.Items[edtRepositoryList.ItemIndex]);
  try
    LoadRepository(rd);
  finally
    rd.Free();
  end;
end;

procedure TfMain.actGetRepositoryUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ( edtRepositoryList.ItemIndex > -1 );
end;

function TfMain.CreateMetadataObject(): IWSTMetadataService;
const FORMAT_MAP : Array[0..1] of string = ( 'SOAP', 'binary' );
var
  i : Integer;
  s : string;
begin
  i := edtFormat.ItemIndex;
  if not ( i in [0..1] ) then
    i := 0;
  s := FORMAT_MAP[i];
  Result := TWSTMetadataService_Proxy.Create(
              'WSTMetadataService',
              s,
              edtAddress.Text//Format('http:Address=%s',[edtAddress.Text])
            ) as IWSTMetadataService;
//lib:FileName=C:\Programmes\lazarus\wst\tests\library\obj\lib_server.dll;target=IWSTMetadataService
//'http:Address=http://127.0.0.1:8000/services/IWSTMetadataService'
//'TCP:Address=127.0.0.1;Port=1234;target=Calculator'
end;

procedure TfMain.LoadRepository(ARep: TWSTMtdRepository);

  procedure LoadService(ASrvsNd : TTreeNode; AService : TWSTMtdService);
  
    procedure LoadOperation(AOprsNd : TTreeNode; AOper : TWSTMtdServiceOperation);
    
      procedure LoadParam(APrmsNd : TTreeNode; APrm : TWSTMtdOperationParam);
      var
        prmNd : TTreeNode;
      begin
        prmNd := tvwMetadata.Items.AddChild(APrmsNd,APrm.Name);
          tvwMetadata.Items.AddChild(prmNd,Format('Name       = %s',[APrm.Name]));
          tvwMetadata.Items.AddChild(prmNd,Format('Type       = %s',[APrm.TypeName]));
      end;
      
    var
      opNd, prmsNd : TTreeNode;
      ii, cc : Integer;
    begin
      opNd := tvwMetadata.Items.AddChild(AOprsNd,AOper.Name);
        tvwMetadata.Items.AddChild(opNd,Format('Name       = %s',[AOper.Name]));
        cc := AOper.Params.Length;
        prmsNd := tvwMetadata.Items.AddChild(opNd,Format('Parameters = %d',[cc]));
        for ii := 0 to Pred(cc) do
          LoadParam(prmsNd,AOper.Params[ii]);
    end;
    
  var
    svNd, oprsNd : TTreeNode;
    j, k : Integer;
  begin
    svNd := tvwMetadata.Items.AddChild(ASrvsNd,AService.Name);
      tvwMetadata.Items.AddChild(svNd,Format('Name       = %s',[AService.Name]));
      k := AService.Operations.Length;
      oprsNd := tvwMetadata.Items.AddChild(svNd,Format('Operations = %d',[k]));
      for j := 0 to Pred(k) do
        LoadOperation(oprsNd,AService.Operations[j]);
  end;
  
var
  rtNd, srvsNd : TTreeNode;
  i, c : Integer;
begin
  tvwMetadata.Items.Clear();
  if not Assigned(ARep) then
    Exit;
  rtNd := tvwMetadata.Items.AddChild(Nil,ARep.Name);
    tvwMetadata.Items.AddChild(rtNd,Format('Name            = %s',[ARep.Name]));
    tvwMetadata.Items.AddChild(rtNd,Format('Name Space      = %s',[ARep.Name]));
    c := ARep.Services.Length;
    srvsNd := tvwMetadata.Items.AddChild(rtNd,Format('Services Count  = %d',[c]));
    for i := 0 to Pred(c) do begin
      LoadService(srvsNd,ARep.Services[i]);
    end;
end;

initialization
  {$I umain.lrs}

  RegisterStdTypes();
  SYNAPSE_RegisterHTTP_Transport();
  LIB_Register_Transport();
  ICS_RegisterTCP_Transport();
  //ICS_RegisterHTTP_Transport();
  //INDY_RegisterHTTP_Transport();
end.

