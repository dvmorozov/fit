{$APPTYPE CONSOLE}
program wst_test_suite;
uses
  SysUtils, ActiveX,
  TestFrameWork,
  TextTestRunner,
  test_utilities in '..\test_utilities.pas',
  testformatter_unit in '..\testformatter_unit.pas';

{$R *.res}

begin
  CoInitialize(nil);
  try
    TextTestRunner.RunRegisteredTests(rxbContinue);
  finally
    CoUninitialize();
  end;
end.
