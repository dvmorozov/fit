//  servernaya chast'; predusmotret' variant sborki bez GUI
program FitServer_;

{$MODE Delphi}

{$INCLUDE wst.inc}

uses
{$ifdef unix}
  cthreads,     //  neobhodimo dlya linux
{$endif}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,
  DataLoader in 'DataLoader.pas',
  PointSetViewer in 'PointSetViewer.pas',
  Math3d in '..\..\Library\Math\Math3d.pas',
  DownhillSimplexContainer in '..\..\Library\Algorithms\DownhillSimplexContainer.pas',
  DownhillSimplexAlgorithm in '..\..\Library\Algorithms\DownhillSimplexAlgorithm.pas',
  Decisions in '..\..\Library\Algorithms\Decisions.pas',
  Algorithm in '..\..\Library\Algorithms\Algorithm.pas',
  AlgorithmContainer in '..\..\Library\Algorithms\AlgorithmContainer.pas',
  Runner in '..\..\Library\Common\Runner.pas',
  ObjSavingStringList in '..\..\Library\Common\ObjSavingStringList.pas',
  Minimizer_S in 'Minimizer_S.pas',
  Minimizer in 'Minimizer.pas',
  MSCRDataClasses in 'MSCRDataClasses.pas', Minimizer_DS, CombEnumerator,
  FitServer, MainCalcThread,
  FitServerStub, FitServerApp,
  DataClasses, FitServerWithThread, FitTask, 
  FitServerMultithreaded, TurboPowerIPro,
  FitGrids, Tools, CBRCComponent, ClassInheritIDs, ComponentList,
  MyExceptions, SelfCopied, SelfSaved, SimpMath, TableComp,
  TAGraph, FitTaskWithThread
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
  Sysutils, Dialogs,
  fit_server_imp, metadata_service_imp, server_listener, fit_server,
  server_binary_formatter, server_service_soap, server_service_xmlrpc,
  fit_server_binder, synapse_tcp_server, Main, FormServer, NumericGrid,
  GeneralHashFunctions;

{$R manifest.res}

var listener : TwstListener;
begin
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
