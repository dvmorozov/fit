{ Server application. }
program FitServer_;

{$MODE Delphi}

{$INCLUDE wst.inc}

uses
{$ifdef unix}
  cthreads,     //  for linux
{$endif}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,
  DataLoader,
  Math3d,
  DownhillSimplexContainer,
  AlgorithmContainer,
  Runner,
  ObjSavingStringList,
  Minimizer_S,
  Minimizer,
  MSCRDataClasses, Minimizer_DS, CombEnumerator, MainCalcThread,
  FitServerStub, FitServerApp,
  DataClasses, FitServerWithThread, FitTask, 
  FitServerMultithreaded, TurboPowerIPro,
  FitGrids, FitTaskWithThread
{$ifdef windows}
  ,ta
{$else}
  ,TA_LINUX
{$endif}
  ,
{$IFDEF FPC}
  {$IFDEF UNIX}
    {$DEFINE UseCThreads}
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
{$ENDIF}
  Sysutils, Dialogs, metadata_service_imp, server_listener, fit_server,
  server_binary_formatter, server_service_soap, server_service_xmlrpc,
  fit_server_binder, fit_server_imp, synapse_tcp_server,
  FormServer, FitServer, Main, fit_server_aux;

{$R manifest.res}

var listener : TwstListener;

{$R FitServer_.res}

begin
  Application.Title:='FitServer';
  Application.Initialize;
  //Form1.ApplicationProperties1.Title := 'FitServer';

  Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  Server_service_RegisterXmlRpcFormat();

  RegisterWSTMetadataServiceImplementationFactory();
  //RegisterWSTMetadataServiceImplementationFactory();
  RegisterWSTMetadataServiceImplementationFactory();

  RegisterFitServerImplementationFactory();
  Server_service_RegisterFitServerService();

  //  ispol`zuetsya ShowMessage vmesto MessageBox, poskol`ku
  //  ne zavisit ot platformy
  WriteLog('FitServer listening on TCP-port ' + InternalPort, Notification_);
    
  listener := TwstSynapseTcpListener.Create(
        InternalIP, StrToInt(InternalPort), 25000, 'Fit Service'
        );
  listener.Start();
  //Application.CreateForm(TForm1, Form1);
  Application.Run;  //  neobhodima dlya obrabotki soobschenii sinhronizatsii
  listener.Free;
end.
