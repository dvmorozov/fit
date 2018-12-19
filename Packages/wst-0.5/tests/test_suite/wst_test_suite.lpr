program wst_test_suite;

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  custapp, classes, sysutils, fpcunit, testreport, testregistry, 
  TestFormatter_unit, testmetadata_unit,
  server_service_soap, soap_formatter, base_binary_formatter,
  base_service_intf, base_soap_formatter, binary_formatter, binary_streamer,
  server_binary_formatter, metadata_repository,
  metadata_generator, parserdefs, server_service_intf, metadata_wsdl,
  test_parserdef, base_xmlrpc_formatter, wst_fpc_xml, test_utilities;

Const
  ShortOpts = 'alh';
  Longopts : Array[1..5] of String = (
  'all','list','format:','suite:','help');
  Version = 'Version 0.1';

Type
  TTestRunner = Class(TCustomApplication)
  private
    FXMLResultsWriter: TXMLResultsWriter;
  protected
    procedure DoRun ; Override;
    procedure doTestRun(aTest: TTest); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLResultsWriter := TXMLResultsWriter.Create;
end;

destructor TTestRunner.Destroy;
begin
  FXMLResultsWriter.Free;
end;

procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.AddListener(FXMLResultsWriter);
    FXMLResultsWriter.WriteHeader;
    aTest.Run(testResult);
    FXMLResultsWriter.WriteResult(testResult);
  finally
    testResult.Free;
  end;
end;

procedure TTestRunner.DoRun;
var
  I : Integer;
  S : String;
begin
  S:=CheckOptions(ShortOpts,LongOpts);
  If (S<>'') then
    Writeln(S);
  if HasOption('h', 'help') or (ParamCount = 0) then
  begin
    writeln(Title);
    writeln(Version);
    writeln('Usage: ');
    writeln('-l or --list to show a list of registered tests');
    writeln('default format is xml, add --format=latex to output the list as latex source');
    writeln('-a or --all to run all the tests and show the results in xml format');
    writeln('The results can be redirected to an xml file,');
    writeln('for example: ./testrunner --all > results.xml');
    writeln('use --suite=MyTestSuiteName to run only the tests in a single test suite class');
  end;
  if HasOption('l', 'list') then
  begin
    if HasOption('format') then
    begin
      if GetOptionValue('format') = 'latex' then
        writeln(GetSuiteAsLatex(GetTestRegistry))
      else
        writeln(GetSuiteAsXML(GetTestRegistry));
    end
    else
      writeln(GetSuiteAsXML(GetTestRegistry));
  end;
  if HasOption('a', 'all') then
  begin
    doTestRun(GetTestRegistry)
  end
  else
    if HasOption('suite') then
    begin
      S := '';
      S:=GetOptionValue('suite');
      if S = '' then
        for I := 0 to GetTestRegistry.Tests.count - 1 do
          writeln(GetTestRegistry[i].TestName)
      else
        for I := 0 to GetTestRegistry.Tests.count - 1 do
          if GetTestRegistry[i].TestName = S then
          begin
            doTestRun(GetTestRegistry[i]);
          end;
    end;
  Terminate;
end;

Var
  App : TTestRunner;

begin
  App:=TTestRunner.Create(Nil);
  App.Initialize;
  App.Title := 'FPCUnit Console Test Case runner.';
  App.Run;
  App.Free;
end.

