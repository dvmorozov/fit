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
  AboutBoxDialog,
  Unit3,
  InputWavelengthDialog, InputBackFactorDialog, InputMaxRFactorDialog,
  FitClient, FitClientApp,
  FitTask, Settings, TurboPowerIPro,
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
