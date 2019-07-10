program Fit;

{$MODE Delphi}

uses
{$ifdef unix}
  cthreads,
{$endif}
  Interfaces,   //  LCL widgetset
  Forms, Variants,
  MainForm,
  AboutBoxDialog,
  InputWavelengthDialog, InputBackFactorDialog, InputMaxRFactorDialog,
  FitClient, FitClientStub,
  FitClientApp,
  Settings, TurboPowerIPro,
  FitGrids, SelfCheckedComponentList,
  StrUtils
{$ifdef windows}
  ,ta
{$else}
  ,TA_LINUX
{$endif}
  , Main, CommonTypes, TableComp, ClassInheritIDs, GeneralHashFunctions,
  CreateUserPointsSetDialog, UserPointsSetPropDialog,
  ConfigurablePointsSet, ConfigurableUserPointsSet, int_fit_service;

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
