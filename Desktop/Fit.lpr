program Fit;

{$MODE Delphi}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Interfaces,   //  LCL widgetset
  Forms, Variants,
  main_form,
  about_box_dialog,
  input_wavelength_dialog, input_back_factor_dialog, input_max_rfactor_dialog,
  fit_client, fit_client_stub,
  fit_client_app,
  app_settings, TurboPowerIPro,
  FitGrids, SelfCheckedComponentList,
  StrUtils
{$ifdef windows}
  ,ta
{$else}
  ,TA_LINUX
{$endif}
  , common_types, table_components, serialization_ids, GeneralHashFunctions,
  create_user_points_set_dlg, user_points_set_prop_dialog,
  configurable_points_set, configurable_user_points_set, int_fit_service;

{$R manifest.res}

{$ifdef windows}
{$R *.res}
{$endif}

function CmdLineParamFound(ParamName: string): string;
const
  { assume that command line parameters
    start with the "/" character }
  Token = '/';
var
  i, TokenPos: integer;
  Param: string;

begin
  result := '';

  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if (Token = Param[1]) and AnsiContainsStr(Param, ParamName) then
    begin
      TokenPos := Pos('=', Param);
      if TokenPos <> 0 then
      begin
        result := Copy(Param, TokenPos + 1, Length(Param) - TokenPos);
        Exit;
      end;
    end;
  end;
end;

var InFile: string;
begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  FormMain.ApplicationProperties1.Title := 'Fit';
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TInputWavelengthDlg, InputWavelengthDlg);
  Application.CreateForm(TInputBackFactorDlg, InputBackFactorDlg);
  Application.CreateForm(TInputMaxRFactorDlg, InputMaxRFactorDlg);
  Application.CreateForm(TCreateUserPointsSetDlg, CreateUserPointsSetDlg);
  Application.CreateForm(TUserPointsSetPropDlg, UserPointsSetPropDlg);

  InFile := CmdLineParamFound('INFILE');
  if InFile <> '' then
  begin
    FormMain.LoadDataFile(InFile);
  end;

  Application.Run;
end.
