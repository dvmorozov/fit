program FitPro;

{$MODE Delphi}

{$INCLUDE wst.inc}

uses
{$IFDEF UNIX}
  cthreads,     //  neobhodimo dlya linux
{$ENDIF}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,     //  требуется для правильного выполнения
  Unit1 in 'Unit1.pas' {Form1},
  DataLoader in 'DataLoader.pas',
  PointSetViewer in 'PointSetViewer.pas',
  ObjSavingStringList in '..\..\Library\Common\ObjSavingStringList.pas',
  Unit12 in 'Unit12.pas' {AboutBox},
  MSCRDataClasses in 'MSCRDataClasses.pas', Minimizer_DS, Unit3,
  Unit5, Unit4, FitServer, FitClient, FitClientApp,
  DataClasses, FitTask, Unit2, Settings, Unit7, TurboPowerIPro,
  FitGrids, Tools, CBRCComponent, ClassInheritIDs, ComponentList,
  MyExceptions, NumericGrid, SelfCopied, SelfSaved, SimpMath, TableComp,
  TAGraph
{$IFDEF WINDOWS}
  , ta
{$ELSE}
  , TA_LINUX
{$ENDIF}
  ,
{$IFDEF FPC}
  {$IFDEF UNIX}
    {$DEFINE UseCThreads}
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
{$ENDIF}
  Main;

{$R manifest.res}

//{$R FitPro.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.ApplicationProperties1.Title := 'FitPro';
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
  Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
  Application.CreateForm(TInputMaxRFactorDlg, InputMaxRFactorDlg);
  Application.CreateForm(TCreateSpecialCurveDlg, CreateSpecialCurveDlg);
  Application.CreateForm(TSpecialCurvePropDlg, SpecialCurvePropDlg);
  Application.Run;
end.
