// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What is written down, and what is said, when a data source fails.)

WHY THIS EXISTS. A source depends on somebody else's computer, so failure is
ordinary - and a failure met once, in front of a user, and gone by the time
anybody looks is the hardest kind to put right. So every request a source makes
is logged, every failure is logged with what was being done when it happened,
and the sentence the user reads ends by saying where those lines are.

WHAT IS NOT A FAILURE. A source that answers "nothing found" has answered; a
download the user stopped has nothing wrong with it. Both are logged, and
neither sends the user to the log - that would make an ordinary answer read as
a fault.

Two halves: the decisions over plain values (unit), and the lines actually
landing in a log file (integration, because it writes one).
}
unit testcase_data_source_failure;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_failure, data_source_wizard,
    int_web_client, web_client, mock_web_client, mock_record_source,
    curl_client, mock_curl_client, log;

type
    { The words and the lines, with nothing written anywhere. }
    TDataSourceFailureTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        function FailureOfSearch: Exception;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EachOperationSaysWhatWasBeingDone;
        procedure ALogLineNamesTheSourceTheQueryAndTheItem;
        procedure ALogLineSaysWhatKindOfFailureItWas;
        procedure ASuccessLineCountsWhatCameBack;
        procedure TheMessagePointsAtTheLogFile;
        procedure WithNoLogThereIsNothingToPointAt;
        procedure AFaultIsPointedAtTheLog;
        procedure ARefusalIsAnAnswerAndIsNotPointedAtTheLog;
        procedure AStopIsNotAFailureAndIsNotPointedAtTheLog;
        procedure AnUnexpectedErrorSaysWhatWasBeingDone;
        procedure TheLogPathIsTheFileTheLogIsWrittenTo;
        procedure WithNoLogFolderThereIsNoLogPath;
        procedure ATransferLineSaysWhatWasAskedAndWhatCameBack;
        procedure ATransferThatFailedSaysHow;
    end;

    { The same, into a real log file: what a user can actually send. }
    TDataSourceLogFileTest = class(TTestCase)
    private
        FFileName: string;
        function LogContents: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFailedSearchIsWrittenToTheLog;
        procedure EveryTransferIsWrittenToTheLog;
        procedure AFailedTransferIsWrittenAsAWarning;
        procedure ARefusalByStatusIsWrittenAsAWarning;
        procedure ACurlThatCannotStartIsWrittenAsAWarning;
    end;

implementation

{ --------------------------------- words ------------------------------------ }

procedure TDataSourceFailureTest.SetUp;
begin
    inherited SetUp;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.LogPath := '/var/log/fit/log.txt';
    FWizard.ChooseSourceClass(TRecordSource);
    FWizard.SetField(RecordFieldId, 'quartz');
end;

procedure TDataSourceFailureTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TDataSourceFailureTest.FailureOfSearch: Exception;
begin
    Result := nil;
    try
        FWizard.Search;
    except
        on E: Exception do
            //  Kept past the handler: AcquireExceptionObject takes it from the
            //  run-time library, which would otherwise free it on leaving.
            Result := Exception(AcquireExceptionObject);
    end;
end;

procedure TDataSourceFailureTest.EachOperationSaysWhatWasBeingDone;
begin
    AssertEquals('searching Records', SourceOperationText(soSearch, 'Records', ''));
    AssertEquals('listing what Records offers',
        SourceOperationText(soList, 'Records', ''));
    AssertEquals('opening "Quartz" in Records',
        SourceOperationText(soOpen, 'Records', 'Quartz'));
    AssertEquals('downloading "a.xy" from Records',
        SourceOperationText(soDownload, 'Records', 'a.xy'));
    AssertEquals('reading "a.xy"', SourceOperationText(soRead, 'Records', 'a.xy'));
end;

procedure TDataSourceFailureTest.ALogLineNamesTheSourceTheQueryAndTheItem;
var
    Line: string;
begin
    Line := SourceFailureLogLine(soOpen, 'doi', 'Published data', 'Quartz',
        'DOI 10.5281/zenodo.1', 'zenodo:1', 'EWebError', 'Cannot fetch x.');
    AssertTrue(Line, Pos('opening "Quartz" in Published data', Line) > 0);
    AssertTrue('the source by its id: ' + Line, Pos('source doi', Line) > 0);
    AssertTrue('what was asked: ' + Line, Pos('DOI 10.5281/zenodo.1', Line) > 0);
    AssertTrue('the item the source fetches by: ' + Line, Pos('zenodo:1', Line) > 0);
    AssertTrue('and what went wrong: ' + Line, Pos('Cannot fetch x.', Line) > 0);
end;

procedure TDataSourceFailureTest.ALogLineSaysWhatKindOfFailureItWas;
var
    Line: string;
begin
    //  The class is what tells a reader of the log a network failure from a
    //  reply that could not be read from a defect - the message alone is
    //  worded for the user and does not.
    Line := SourceFailureLogLine(soSearch, 'doi', 'Published data', '', 'q', '',
        'EAccessViolation', 'Access violation');
    AssertTrue(Line, Pos('EAccessViolation', Line) > 0);
end;

procedure TDataSourceFailureTest.ASuccessLineCountsWhatCameBack;
var
    Line: string;
begin
    Line := SourceSuccessLogLine(soSearch, 'doi', 'Published data', '',
        'words quartz', 12);
    AssertTrue(Line, Pos('searching Published data', Line) > 0);
    AssertTrue(Line, Pos('12 item', Line) > 0);
    AssertTrue(Line, Pos('words quartz', Line) > 0);
end;

procedure TDataSourceFailureTest.TheMessagePointsAtTheLogFile;
begin
    AssertEquals('Cannot fetch x. The details are in Fit''s log, ' +
        '/var/log/fit/log.txt.',
        WithLogPointer('Cannot fetch x.', '/var/log/fit/log.txt'));
    AssertEquals('a message with no full stop gets one first',
        'Cannot fetch x. The details are in Fit''s log, /l.txt.',
        WithLogPointer('Cannot fetch x', '/l.txt'));
end;

procedure TDataSourceFailureTest.WithNoLogThereIsNothingToPointAt;
begin
    AssertEquals('Cannot fetch x.', WithLogPointer('Cannot fetch x.', ''));
end;

procedure TDataSourceFailureTest.AFaultIsPointedAtTheLog;
var
    E: Exception;
begin
    FMockObject.FailWith('Cannot fetch https://records.example.org/search: ' +
        'the host name was not found.');
    E := FailureOfSearch;
    try
        AssertNotNull('the search failed', E);
        AssertTrue('the user''s words are kept: ' + E.Message,
            Pos('host name was not found', E.Message) > 0);
        AssertTrue('and end where the details are: ' + E.Message,
            Pos('/var/log/fit/log.txt', E.Message) > 0);
    finally
        E.Free;
    end;
end;

procedure TDataSourceFailureTest.ARefusalIsAnAnswerAndIsNotPointedAtTheLog;
var
    E: Exception;
begin
    //  The probe raises EDataSourceError for "nothing found": an answer.
    FMockObject.Reply('records.example.org/search', '');
    E := FailureOfSearch;
    try
        AssertNotNull(E);
        AssertTrue(E.Message, Pos('Nothing was found', E.Message) > 0);
        AssertEquals('not sent to the log: ' + E.Message, 0,
            Pos('log', E.Message));
    finally
        E.Free;
    end;
end;

procedure TDataSourceFailureTest.AStopIsNotAFailureAndIsNotPointedAtTheLog;
var
    E: Exception;
begin
    FMockObject.CancelNext;
    E := FailureOfSearch;
    try
        AssertNotNull(E);
        AssertTrue('still a stop: ' + E.ClassName, E is EWebCancelled);
        AssertEquals('and not sent to the log: ' + E.Message, 0,
            Pos('log', E.Message));
    finally
        E.Free;
    end;
end;

procedure TDataSourceFailureTest.AnUnexpectedErrorSaysWhatWasBeingDone;
begin
    //  A defect's own words name nothing the user did; what was being done,
    //  and which source, is what turns it into a report somebody can act on.
    AssertEquals('Something went wrong while searching Records: ' +
        'Access violation. The details are in Fit''s log, /l.txt.',
        UnexpectedFailureMessage(soSearch, 'Records', '', 'Access violation',
        '/l.txt'));
end;

procedure TDataSourceFailureTest.TheLogPathIsTheFileTheLogIsWrittenTo;
begin
    SetLogFileName('a_test_name.txt');
    try
        AssertEquals(GetLogDir + 'a_test_name.txt', GetLogFilePath);
    finally
        SetLogFileName('log.txt');
    end;
end;

procedure TDataSourceFailureTest.WithNoLogFolderThereIsNoLogPath;
begin
    AssertEquals('', LogFilePathIn('', 'log.txt'));
    AssertEquals('/l/log.txt', LogFilePathIn('/l/', 'log.txt'));
end;

procedure TDataSourceFailureTest.ATransferLineSaysWhatWasAskedAndWhatCameBack;
var
    Line: string;
begin
    Line := TransferLogLine('GET', 'https://zenodo.org/api/records?q=x', 200,
        4096, 520, '');
    AssertTrue(Line, Pos('GET https://zenodo.org/api/records?q=x', Line) > 0);
    AssertTrue('the status: ' + Line, Pos('HTTP 200', Line) > 0);
    AssertTrue('the size: ' + Line, Pos('4096 bytes', Line) > 0);
    AssertTrue('and how long it took: ' + Line, Pos('520 ms', Line) > 0);
end;

procedure TDataSourceFailureTest.ATransferThatFailedSaysHow;
var
    Line: string;
begin
    Line := TransferLogLine('GET', 'https://x.org/a', 0, 0, 30000,
        'curl exit 28: Operation timed out');
    AssertTrue(Line, Pos('curl exit 28: Operation timed out', Line) > 0);
    AssertTrue('no status came: ' + Line, Pos('no answer', Line) > 0);
end;

{ ------------------------------- the file ----------------------------------- }

type
    { The real client with curl's process replaced, as in
      testcase_download_failures. }
    TLoggedCurlClient = class(TWebClient)
    private
        FCurl: TRecordedCurlClient;
    protected
        function CreateCurl: TCurlClient; override;
    public
        destructor Destroy; override;
        property Curl: TRecordedCurlClient read FCurl write FCurl;
    end;

function TLoggedCurlClient.CreateCurl: TCurlClient;
begin
    Result := FCurl;
    FCurl := nil;
end;

destructor TLoggedCurlClient.Destroy;
begin
    FCurl.Free;
    inherited Destroy;
end;

procedure TDataSourceLogFileTest.SetUp;
begin
    FFileName := GetLogDir + 'test_data_source_log.txt';
    DeleteFile(FFileName);
    SetLogFileName('test_data_source_log.txt');
end;

procedure TDataSourceLogFileTest.TearDown;
begin
    SetLogFileName('log.txt');
    DeleteFile(FFileName);
end;

function TDataSourceLogFileTest.LogContents: string;
var
    Lines: TStringList;
begin
    Lines := TStringList.Create;
    try
        //  Handed back first, so the file is closed and complete.
        SetLogFileName('log.txt');
        if FileExists(FFileName) then
            Lines.LoadFromFile(FFileName);
        Result := Lines.Text;
    finally
        Lines.Free;
    end;
end;

procedure TDataSourceLogFileTest.AFailedSearchIsWrittenToTheLog;
var
    Mock: TMockWebClient;
    Web: IWebClient;
    Wizard: TDataSourceWizard;
    Text: string;
begin
    Mock := TMockWebClient.Create;
    Web := Mock;
    Wizard := TDataSourceWizard.Create(Web);
    try
        Wizard.ChooseSourceClass(TRecordSource);
        Wizard.SetField(RecordFieldId, 'quartz');
        Mock.FailWith('Cannot fetch it: nothing answered.');
        try
            Wizard.Search;
        except
            on E: Exception do ;
        end;
    finally
        Wizard.Free;
        Web := nil;
        Mock.Free;
    end;
    Text := LogContents;
    AssertTrue(Text, Pos('searching Record probe', Text) > 0);
    AssertTrue(Text, Pos('nothing answered', Text) > 0);
    AssertTrue('as a warning: ' + Text, Pos('Warning', Text) > 0);
end;

procedure TDataSourceLogFileTest.EveryTransferIsWrittenToTheLog;
var
    Client: TLoggedCurlClient;
    Text: string;
begin
    Client := TLoggedCurlClient.Create;
    try
        Client.Curl := TRecordedCurlClient.Create('curl');
        Client.Curl.Headers := OkDump;
        Client.Curl.AddChunk('1 2');
        Client.GetText('https://example.org/a.xy');
    finally
        Client.Free;
    end;
    Text := LogContents;
    AssertTrue(Text, Pos('GET https://example.org/a.xy', Text) > 0);
    AssertTrue(Text, Pos('HTTP 200', Text) > 0);
end;

procedure TDataSourceLogFileTest.AFailedTransferIsWrittenAsAWarning;
var
    Client: TLoggedCurlClient;
    Text: string;
begin
    Client := TLoggedCurlClient.Create;
    try
        Client.Curl := TRecordedCurlClient.Create('curl');
        Client.Curl.Headers := '';
        Client.Curl.ExitCode := 6;
        Client.Curl.Stderr := 'curl: (6) Could not resolve host: example.org';
        try
            Client.GetText('https://example.org/a.xy');
        except
            on E: EWebError do ;
        end;
    finally
        Client.Free;
    end;
    Text := LogContents;
    AssertTrue(Text, Pos('Could not resolve host', Text) > 0);
    AssertTrue('as a warning: ' + Text, Pos('Warning', Text) > 0);
end;

procedure TDataSourceLogFileTest.ARefusalByStatusIsWrittenAsAWarning;
var
    Client: TLoggedCurlClient;
    Text: string;
begin
    Client := TLoggedCurlClient.Create;
    try
        Client.Curl := TRecordedCurlClient.Create('curl');
        Client.Curl.Headers := 'HTTP/1.1 404 Not Found' + CRLF + CRLF;
        Client.Curl.AddChunk('gone');
        try
            Client.GetText('https://example.org/gone.xy');
        except
            on E: EWebError do ;
        end;
    finally
        Client.Free;
    end;
    Text := LogContents;
    AssertTrue(Text, Pos('HTTP 404', Text) > 0);
    AssertTrue('as a warning: ' + Text, Pos('Warning', Text) > 0);
end;

procedure TDataSourceLogFileTest.ACurlThatCannotStartIsWrittenAsAWarning;
var
    Client: TLoggedCurlClient;
    Text, Message_: string;
begin
    Client := TLoggedCurlClient.Create;
    try
        Client.Curl := TRecordedCurlClient.Create('curl');
        Client.Curl.FailToStart := 'access denied';
        Message_ := '';
        try
            Client.GetText('https://example.org/a.xy');
        except
            on E: EWebError do
                Message_ := E.Message;
        end;
    finally
        Client.Free;
    end;
    AssertTrue('worded for the user: ' + Message_,
        Pos('access denied', Message_) > 0);
    Text := LogContents;
    AssertTrue(Text, Pos('curl could not be run', Text) > 0);
end;

initialization
    RegisterTest('unit', TDataSourceFailureTest);
    RegisterTest('integration', TDataSourceLogFileTest);
end.
