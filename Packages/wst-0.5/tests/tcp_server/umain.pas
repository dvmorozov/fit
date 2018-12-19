unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ActnList, ExtCtrls;

type

  { TfMain }

  TfMain = class(TForm)
    actClearLog: TAction;
    actStop: TAction;
    actStart: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtPort: TEdit;
    Label1: TLabel;
    mmoLog: TMemo;
    procedure actClearLogExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStartUpdate(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStopUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure LogMessage(const AMsg : string);
  end;

var
  fMain: TfMain;

implementation
uses server_unit,
     server_service_soap, server_binary_formatter,
     calculator, calculator_imp, calculator_binder,
     metadata_service, metadata_service_imp, metadata_service_binder;

Var
  scktServer : TTcpSrvApp;

{ TfMain }

procedure TfMain.actStartUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Not ( Assigned(scktServer) And scktServer.IsActive() );
end;

procedure TfMain.actStopExecute(Sender: TObject);
begin
  If Assigned(scktServer) Then Begin
    scktServer.Stop();
  End;
end;

procedure TfMain.actStopUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(scktServer) And scktServer.IsActive();
end;

procedure TfMain.actStartExecute(Sender: TObject);
begin
  mmoLog.Clear();
  If Not Assigned(scktServer) Then
    scktServer := TTcpSrvApp.Create();
  If Not scktServer.IsActive() Then
    scktServer.Start();
end;

procedure TfMain.actClearLogExecute(Sender: TObject);
begin
  mmoLog.Clear();
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  Server_service_RegisterCalculatorService();
  Server_service_RegisterCalculatorService();
  RegisterCalculatorImplementationFactory();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterBinaryFormat();
  
  RegisterWSTMetadataServiceImplementationFactory();
  Server_service_RegisterWSTMetadataServiceService
end;

procedure TfMain.LogMessage(const AMsg: string);
begin
  mmoLog.Lines.Add(AMsg);
end;

initialization
  {$I umain.lrs}

end.
