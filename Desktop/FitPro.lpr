program FitPro;

{$MODE Delphi}

{$INCLUDE wst.inc}

uses
{$IFDEF UNIX}
  cthreads,     //  neobhodimo dlya linux
{$ENDIF}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,
  MainForm,
  DataLoader in 'DataLoader.pas',
  PointSetViewer in 'PointSetViewer.pas',
  ObjSavingStringList in '..\..\Library\Common\ObjSavingStringList.pas',
  AboutBoxDialog,
  MSCRDataClasses in 'MSCRDataClasses.pas', Minimizer_DS, Unit3,
  InputWavelengthDialog, InputBackFactorDialog, InputMaxRFactorDialog,
  FitServer, FitClient, FitClientApp,
  DataClasses, FitTask, Settings, TurboPowerIPro,
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
  Main,
  CreateUserPointsSetDialog, UserPointsSetPropDialog,
  ConfigurablePointsSet, ConfigurableUserPointsSet;

{$R manifest.res}

//{$R FitPro.res}

begin
  Application.Initialize;
  Form1.ApplicationProperties1.Title := 'FitPro';
  Application.Run;
end.
