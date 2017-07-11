program FitP2P;

{$MODE Delphi}

uses
{$ifdef unix}
  cthreads,     //  neobhodimo dlya linux
{$endif}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,     //  требуется для правильного выполнения
  Unit1 in 'Unit1.pas' {Form1},
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
  Unit12 in 'Unit12.pas' {AboutBox},
  Minimizer_S in 'Minimizer_S.pas',
  Minimizer in 'Minimizer.pas',
  MSCRDataClasses in 'MSCRDataClasses.pas', Minimizer_DS, Unit3,
  Unit5, Unit4, CombEnumerator, FitServer, MainCalcThread, FitClient, FitClientStub,
  FitServerStub, FitServerProxy, FitClientApp, FitServerApp,
  DataClasses, FitServerWithThread, FitTask, 
  FitServerMultithreaded, Unit2, Settings, Unit7, TurboPowerIPro,
  FitGrids, Tools, CBRCComponent, ClassInheritIDs, ComponentList,
  MyExceptions, NumericGrid, SelfCopied, SelfSaved, SimpMath, TableComp,
  TAGraph, FitTaskWithThread
{$ifdef windows}
  ,ta
{$else}
  ,TA_LINUX
{$endif}
  , Main;

{$R manifest.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.ApplicationProperties1.Title := 'FitP2P';
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
  Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
  Application.CreateForm(TInputMaxRFactorDlg, InputMaxRFactorDlg);
  Application.CreateForm(TCreateSpecialCurveDlg, CreateSpecialCurveDlg);
  Application.CreateForm(TSpecialCurvePropDlg, SpecialCurvePropDlg);
  Application.Run;
end.
