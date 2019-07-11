program FitPro;

{$MODE Delphi}

uses
{$IFDEF UNIX}
  cthreads,     //  neobhodimo dlya linux
{$ENDIF}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,
  MainForm,
  DataLoader,
  AboutBoxDialog,
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

{$R *.res}

begin
  Application.Initialize;
  FormMain.ApplicationProperties1.Title := 'FitPro';
  Application.Run;
end.
