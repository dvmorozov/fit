program FitPro;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  Interfaces,   //  LCL widgetset
  Forms, Variants, form_main, about_box_dialog, input_wavelength_dialog,
  input_back_factor_dialog, input_max_rfactor_dialog, fit_client,
  fit_client_app, Settings, TurboPowerIPro, FitGrids, Tools, MyExceptions,
  NumericGrid, SimpMath, TAGraph, ta, app, table_components,
  create_user_points_set_dlg, user_points_set_prop_dialog,
  configurable_points_set, configurable_user_points_set, points_set;

{$R manifest.res}

{$IFDEF _WINDOWS}
{$R *.res}
{$ENDIF}

begin
  Application.Title:='FitPro';
  Application.Initialize;
  FormMain.ApplicationProperties1.Title := 'FitClient';
  Application.Run;
end.
