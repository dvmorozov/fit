program Fit;

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
  FitGrids, SelfCheckedComponentList,
  FitTaskWithThread, StrUtils
{$ifdef windows}
  ,ta
{$else}
  ,TA_LINUX
{$endif}
  , Main, CommonTypes, GeneralHashFunctions;

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
  Application.CreateForm(TCreateSpecialCurveDlg, CreateSpecialCurveDlg);
  Application.CreateForm(TSpecialCurvePropDlg, SpecialCurvePropDlg);

  InFile := CmdLineParamFound('INFILE');
  if InFile <> '' then
  begin
    FormMain.LoadDataFile(InFile);
  end;

  Application.Run;
end.
