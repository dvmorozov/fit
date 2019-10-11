program FitPro;

{$MODE Delphi}

uses
{$IFDEF UNIX}
  cthreads,     //  neobhodimo dlya linux
{$ENDIF}
  Interfaces,   //  LCL widgetset
  Forms,
  Variants,
  form_main,
  about_box_dialog,
  input_wavelength_dialog, input_back_factor_dialog, input_max_rfactor_dialog,
  fit_client, fit_client_app,
  Settings, TurboPowerIPro,
  FitGrids, Tools,
  MyExceptions, NumericGrid, SimpMath,
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
  app,
  create_user_points_set_dlg, user_points_set_prop_dialog,
  configurable_points_set, configurable_user_points_set;

{$R manifest.res}

//{$R FitPro.res}

{$R *.res}

begin
  Application.Initialize;
  FormMain.ApplicationProperties1.Title := 'FitPro';
  Application.Run;
end.
