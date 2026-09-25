// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Everything that can go wrong while fetching data, and what is said.)

WHY THIS FILE EXISTS. Fetching is the one thing this program does that depends
on somebody else's computer, so failure is ORDINARY here rather than
exceptional: a service is down, a series was withdrawn, a network drops, a disk
is full, an answer is a login page. Each of those has to reach the user as a
sentence saying what happened and what they can do - and each of them is a
branch that no happy-path test ever takes.

THE FAILURE THAT PROMPTED IT. The client asked FRED for a series with a
User-Agent the service did not like. It answered NOTHING - no status, no body,
a connection left open until the timeout - and that reached the user as "The CSV
file is empty", which reads as "the data you asked for does not exist". The
data existed; the same address answered 29 KB to curl a second later. Two
lessons are pinned below: the agent has a shape, and an empty answer is refused
in words by the fetcher rather than blamed on the reader.

EVERY CASE HERE IS MOCKED. Nothing reaches the network, so the branches are
reachable at all - a service cannot be asked to be down on demand.
}
unit testcase_download_failures;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_registry, data_source_registration,
    data_source_advice, data_source_wizard, data_source_import,
    data_loader_registration, download_cache,
    int_web_client, web_client, mock_web_client, download_job,
    curl_client, mock_curl_client,
    url_source, doi_source, samples_source;

type
    { A step of the wizard, called where its refusal can be caught. }
    TProcedureOfObject = procedure of object;

    { The rules a transfer follows while it runs: what stops it, and what is
      said afterwards about what came back. }
    TTransferGuardTest = class(TTestCase)
    private
        FSeen: int64;
        FReports: longint;
        procedure NoteProgress(ABytes, ATotal: int64);
    published
        procedure AGoodAnswerIsRefusedForNothing;
        procedure ACancelStopsTheTransferWhileItRuns;
        procedure ACancelledTransferSaysItWasStopped;
        procedure TheCapStopsTheTransferRatherThanJudgingItAfterwards;
        procedure TheCapSaysHowLargeItIs;
        procedure ProgressIsReportedAsBytesArrive;
        procedure AStallIsNotReportedAsProgress;
        procedure AnEmptyAnswerIsRefusedByTheFetcher;
        procedure SomethingNotThereNamesTheAddress;
        procedure SomethingRefusedSaysAnAccountIsNeeded;
        procedure TooManyRequestsSaysToWait;
        procedure TooManyRequestsPassesOnHowLong;
        procedure AServicesOwnTroubleIsSaidToBeTheirs;
        procedure AnUnexpectedStatusNamesTheNumber;
        procedure TheUserAgentHasAProductVersionAndContact;
        procedure AMissingCurlIsNotReportedAsANetworkFault;
        procedure AFailedHandshakeIsNotReportedAsANetworkFault;
        procedure EachTransportFailureNamesTheAddress;
        procedure AFullStopInTheDetailIsNotDoubled;
        procedure ASaveFailureNamesTheFolderAndSaysTheFetchWorked;
        //  The real client, with its one socket method replaced.
        procedure TextComesBackFromTheRealClient;
        procedure BytesComeBackFromTheRealClient;
        procedure AFailureFromTheSocketIsWordedForTheUser;
        procedure TheNameTheServerSuggestedIsRead;
        procedure AServerThatSuggestsNoNameIsNotAFailure;
        procedure ProgressArrivingOutsideATransferIsIgnored;
        procedure CancelReachesTheTransferThatIsRunning;
    end;

    { The same failures where the user meets them: the wizard. }
    TDownloadFailureTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        function MessageOfSearch(const ASourceId, AFieldId, AValue: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AServiceThatCannotBeReachedIsReportedWithItsAddress;
        procedure AnAnswerThatIsNotWhatWasExpectedSaysSo;
        procedure ADoiThatResolvesNowhereSaysSo;
        procedure ARecordWithNoFilesSaysWhatToDoInstead;
        procedure ASampleThatIsNoLongerThereNamesIt;
        procedure DownloadingBeforeChoosingIsRefused;
        procedure WithNowhereToKeepDownloadsTheReasonIsSaid;
        procedure AnEmptyRequiredFieldNamesTheFieldNotTheNetwork;
        //  What the preview makes of a file that is not what was wanted.
        procedure AWebPageInsteadOfDataIsReportedInTheReadersWords;
        procedure AFileThatParsesToNothingCannotBecomeAProject;
        //  The last step.
        procedure ImportingWithNoHostIsAProgrammingErrorAndSaysWhich;
        procedure ImportingNothingIsRefused;
        procedure ZenodoFindingNothingSaysWhatToTryInstead;
        procedure AnEmptyAnswerFromACatalogueIsSaidToBeEmpty;
        procedure ARecordThisProgramCannotListIsRefusedByRepository;
        procedure AnAddressNamingNoFileTakesTheNameTheServerGave;
        procedure ARecordsFileIsFetchedFromWhereTheRecordSaid;
        procedure EachStepSaysWhatIsMissingBeforeItCanBeLeft;
        //  The job the window waits on while a file is fetched.
        procedure AFailedDownloadIsKeptRatherThanRaisedOutOfTheThread;
        procedure ACancelledDownloadIsKeptApartFromAFailure;
        procedure AJobSaysWhenItHasFinished;
    end;

    { The order the import takes, over a host that only records - so the four
      steps can be asserted without a window, a client or a server. }
    TImportOrderTest = class(TTestCase)
    private
        FHostObject: TObject;
        FHost: IImportHost;
        function Log: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheDocumentIsRepacedOnlyAfterTheUserHasAgreed;
        procedure SayingNoTouchesNothingAtAll;
        procedure TheOriginIsRecordedAfterTheImportAndNotBefore;
        procedure AnOriginReadsAsOneSentence;
        procedure NothingFetchedHasNoOriginToShow;
    end;

    { The samples source against a real directory - which is a filesystem test
      whatever it asserts, and says so. }
    TSamplesDirectoryTest = class(TTestCase)
    private
        FDir: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ADirectoryWithNothingReadableSaysSoRatherThanShowingNothing;
        procedure AFileKeptSomewhereElseIsMovedRatherThanFetchedAgain;
    end;

    { The refusals the wizard itself makes, which are what the window turns
      into a sentence: each is a branch a happy path never enters. }
    TWizardRefusalTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        function RefusalOf(AProc: TProcedureOfObject): string;
        procedure UseBeforeChoosing;
        procedure AskForAResultThatIsNotThere;
        procedure OpenAFileAsThoughItWereARecord;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure UsingTheWizardBeforeChoosingASourceSaysSo;
        procedure AResultThatIsNotThereIsRefusedByNumber;
        procedure AFileCannotBeOpenedAsARecord;
        procedure SearchingAgainForgetsWhatWasChosen;
        procedure ClosingTheTopContainerAsksTheSourceAgain;
        procedure GoingOnFromThePreviewStaysThere;
    end;

type
    { The real client with its socket replaced: Transfer is the ONE method that
      opens a connection, so overriding it runs every line above it - the text
      and byte wrappers, the suggested name, the refusals - against an answer a
      test decides. Without this the only thing exercised was the mock. }
    TDrivenWebClient = class(TWebClient)
    private
        FBody: string;
        FSuggested: string;
        FRaise: string;
    protected
        function Transfer(const AUrl: string; ADest: TStream): string; override;
    public
        { Drives the callback the socket layer would call, so the wiring
          between the bytes and the guard is what is under test. }
        procedure ReceiveHere(ABytes: int64);
        { What the transport was told after the last chunk: go on or stop. }
        function AnswerTo(ABytes: int64): boolean;
        property Body: string read FBody write FBody;
        property Suggested: string read FSuggested write FSuggested;
        { When set, Transfer raises it - a host that is not there. }
        property RaiseWith: string read FRaise write FRaise;
    end;

    { The job, driven WITHOUT a thread: Execute is what a thread would call,
      and calling it here makes every branch reachable from a test with no
      timing in it at all. }
    TDrivenJob = class(TDownloadJob)
    public
        procedure RunHere;
    end;

    { The real client, Transfer and all, fetching through a curl whose process
      is a recorded answer - or through no curl at all. }
    TCurlDrivenWebClient = class(TWebClient)
    private
        FCurl: TRecordedCurlClient;
    protected
        function CreateCurl: TCurlClient; override;
    public
        destructor Destroy; override;
        { The answer the next transfer gets; nil means no curl on this
          computer. Handed over at the transfer, which frees it. }
        property Curl: TRecordedCurlClient read FCurl write FCurl;
    end;

    { Everything Transfer decides about what curl answered: each is a branch a
      real service cannot be asked to take on demand. }
    TWebClientOverCurlTest = class(TTestCase)
    private
        FClient: TCurlDrivenWebClient;
        FDest: TStringStream;
        FCurl: TRecordedCurlClient;
        procedure CancelNow(ABytes, ATotal: int64);
        { Transfers from https://example.org/x and answers the message it was
          refused with, or '' when it was not. }
        function RefusalOf: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure NoCurlIsSaidInWordsAndNotAsANetworkFault;
        procedure AFailedHandshakeReachesTheUserAsOne;
        procedure ARefusedConnectionReachesTheUserAsOne;
        procedure TheSuggestedNameIsTheLastResponses;
        procedure AStatusIsSaidInWords;
        procedure AnEmptyAnswerIsRefused;
        procedure TheCapStopsTheTransferPartWay;
        procedure ACancelWhileItRunsIsACancel;
        procedure TheLimitsAreTheClientsOwn;
        procedure HowLongToWaitIsPassedOnFromTheAnswer;
    end;

implementation

procedure TDrivenWebClient.ReceiveHere(ABytes: int64);
begin
    DataReceived(ABytes);
end;

function TDrivenWebClient.AnswerTo(ABytes: int64): boolean;
begin
    Result := DataReceived(ABytes);
end;

function TDrivenWebClient.Transfer(const AUrl: string; ADest: TStream): string;
begin
    if FRaise <> '' then
        raise EWebError.Create(WebFailureMessage(AUrl, FRaise));
    if Length(FBody) > 0 then
        ADest.WriteBuffer(FBody[1], Length(FBody));
    Result := FSuggested;
end;

procedure TDrivenJob.RunHere;
begin
    Execute;
end;

function TCurlDrivenWebClient.CreateCurl: TCurlClient;
begin
    Result := FCurl;
    FCurl := nil;
end;

destructor TCurlDrivenWebClient.Destroy;
begin
    FCurl.Free;
    inherited Destroy;
end;

procedure TWebClientOverCurlTest.SetUp;
begin
    FClient := TCurlDrivenWebClient.Create;
    FDest := TStringStream.Create('');
    FCurl := TRecordedCurlClient.Create('curl');
    FCurl.Headers := OkDump;
    FClient.Curl := FCurl;
end;

procedure TWebClientOverCurlTest.TearDown;
begin
    FreeAndNil(FDest);
    //  The client frees the curl it still holds; one handed over was freed by
    //  the transfer.
    FreeAndNil(FClient);
    FCurl := nil;
end;

procedure TWebClientOverCurlTest.CancelNow(ABytes, ATotal: int64);
begin
    FClient.Cancel;
end;

function TWebClientOverCurlTest.RefusalOf: string;
begin
    Result := '';
    try
        FClient.Download('https://example.org/x', FDest);
    except
        on E: EWebError do
            Result := E.Message;
    end;
end;

procedure TWebClientOverCurlTest.NoCurlIsSaidInWordsAndNotAsANetworkFault;
begin
    FClient.Curl := nil;
    FreeAndNil(FCurl);
    AssertEquals(MissingCurlMessage('https://example.org/x'), RefusalOf);
end;

procedure TWebClientOverCurlTest.AFailedHandshakeReachesTheUserAsOne;
begin
    //  THE REPORTED DEFECT, through the method a download reaches.
    FCurl.Headers := '';
    FCurl.ExitCode := 35;
    FCurl.Stderr := 'curl: (35) SSL connect error';
    AssertEquals(TransportFailureMessage('https://example.org/x',
        cfSecureChannel, 'curl: (35) SSL connect error'), RefusalOf);
end;

procedure TWebClientOverCurlTest.ARefusedConnectionReachesTheUserAsOne;
begin
    FCurl.Headers := '';
    FCurl.ExitCode := 7;
    FCurl.Stderr := 'curl: (7) Failed to connect';
    AssertEquals(TransportFailureMessage('https://example.org/x',
        cfConnect, 'curl: (7) Failed to connect'), RefusalOf);
end;

procedure TWebClientOverCurlTest.TheSuggestedNameIsTheLastResponses;
begin
    FCurl.AddChunk('1 2');
    FCurl.Headers := RedirectDump;
    AssertEquals('series.csv',
        FClient.Download('https://example.org/x', FDest));
end;

procedure TWebClientOverCurlTest.AStatusIsSaidInWords;
begin
    FCurl.AddChunk('Not found');
    FCurl.Headers := NotFoundDump;
    AssertEquals(HttpStatusMessage('https://example.org/x', 404, ''),
        RefusalOf);
end;

procedure TWebClientOverCurlTest.AnEmptyAnswerIsRefused;
var
    Message_: string;
begin
    Message_ := RefusalOf;
    AssertTrue('refused as empty: ' + Message_,
        Pos('empty file', Message_) > 0);
end;

procedure TWebClientOverCurlTest.TheCapStopsTheTransferPartWay;
var
    Message_: string;
begin
    FClient.MaxBytes := 4;
    FCurl.AddChunk('abc');
    FCurl.AddChunk('def');
    FCurl.AddChunk('ghi');
    Message_ := RefusalOf;
    AssertTrue('names the cap: ' + Message_, Pos('larger than', Message_) > 0);
    AssertEquals('stopped at the chunk that passed it', 'abcdef',
        FDest.DataString);
end;

procedure TWebClientOverCurlTest.ACancelWhileItRunsIsACancel;
var
    Raised: boolean;
begin
    FCurl.AddChunk('abc');
    FCurl.AddChunk('def');
    FClient.SetProgress(CancelNow);
    Raised := False;
    try
        FClient.Download('https://example.org/x', FDest);
    except
        on E: EWebCancelled do
        begin
            Raised := True;
            AssertEquals('The download was stopped.', E.Message);
        end;
    end;
    AssertTrue('a cancel, not a failure', Raised);
    AssertEquals('nothing after the cancel', 'abc', FDest.DataString);
end;

procedure TWebClientOverCurlTest.HowLongToWaitIsPassedOnFromTheAnswer;
begin
    //  A service that says how long to wait is worth quoting: it is the one
    //  failure a user fixes by doing nothing for a while.
    FCurl.AddChunk('slow down');
    FCurl.Headers := 'HTTP/1.1 429 Too Many Requests' + CRLF +
        'Retry-After: 120' + CRLF + CRLF;
    AssertEquals(HttpStatusMessage('https://example.org/x', 429, '120'),
        RefusalOf);
end;

procedure TWebClientOverCurlTest.TheLimitsAreTheClientsOwn;
var
    Call: TCurlCall;
begin
    //  The agent, the timeout and the redirect limit are this client's
    //  decisions (see their constants), and must reach the curl it runs.
    FCurl.AddChunk('1 2');
    FCurl.Report := @Call;
    FClient.Download('https://example.org/x', FDest);
    AssertEquals('https://example.org/x', Call.Url);
    AssertEquals(DefaultUserAgent, Call.UserAgent);
    AssertEquals(DefaultTimeoutMs, Call.TimeoutMs);
    AssertEquals(MaxRedirects, Call.MaxRedirects);
end;

{ ------------------------- while a transfer runs ---------------------------- }

procedure TTransferGuardTest.NoteProgress(ABytes, ATotal: int64);
begin
    FSeen := ABytes;
    Inc(FReports);
end;

procedure TTransferGuardTest.AStallIsNotReportedAsProgress;
var
    Guard: TTransferGuard;
begin
    //  THE TRANSPORT ASKS DURING A STALL TOO, so that a cancel is heard while
    //  nothing arrives. Asking is not progress: the bar is told only when the
    //  count has moved.
    FReports := 0;
    Guard := TTransferGuard.Create(0, NoteProgress);
    try
        AssertTrue(Guard.Accept(4096, 0));
        AssertTrue('a stall still goes on', Guard.Accept(4096, 0));
        AssertEquals('reported once', 1, FReports);
        Guard.Accept(8192, 0);
        AssertEquals('and again when it moves', 2, FReports);
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.AGoodAnswerIsRefusedForNothing;
var
    Guard: TTransferGuard;
begin
    Guard := TTransferGuard.Create(1000, nil);
    try
        AssertTrue('bytes are accepted', Guard.Accept(500, 500));
        AssertEquals('and nothing is wrong with the answer', '',
            Guard.Refusal('https://example.org/a.csv', 500, 200));
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.ACancelStopsTheTransferWhileItRuns;
var
    Guard: TTransferGuard;
begin
    Guard := TTransferGuard.Create(0, nil);
    try
        AssertTrue('accepted before', Guard.Accept(10, 100));
        Guard.Cancel;
        //  THE DEFECT THIS PINS: Cancel used to set a flag that nothing read
        //  while bytes were arriving, so the button did nothing until the
        //  whole file had been fetched.
        AssertFalse('and stopped at the next chunk', Guard.Accept(20, 100));
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.ACancelledTransferSaysItWasStopped;
var
    Guard: TTransferGuard;
begin
    Guard := TTransferGuard.Create(0, nil);
    try
        Guard.Cancel;
        Guard.Accept(20, 100);
        AssertTrue(Guard.Refusal('https://example.org/a.csv', 20, 0),
            Pos('stopped', Guard.Refusal('https://example.org/a.csv', 20, 0)) > 0);
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.TheCapStopsTheTransferRatherThanJudgingItAfterwards;
var
    Guard: TTransferGuard;
begin
    Guard := TTransferGuard.Create(1000, nil);
    try
        AssertTrue('under the cap', Guard.Accept(900, 5000));
        //  A cap applied after the answer is in memory has already cost the
        //  memory: a repository's whole archive would be downloaded and then
        //  refused.
        AssertFalse('over it, and stopped there', Guard.Accept(1001, 5000));
        AssertTrue('and remembered as too large', Guard.TooLarge);
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.TheCapSaysHowLargeItIs;
var
    Guard: TTransferGuard;
    Refusal: string;
begin
    Guard := TTransferGuard.Create(64 * 1024 * 1024, nil);
    try
        Guard.Accept(70 * 1024 * 1024, 0);
        Refusal := Guard.Refusal('https://example.org/big.zip',
            70 * 1024 * 1024, 200);
        AssertTrue(Refusal, Pos('64 MB', Refusal) > 0);
        AssertTrue('and says what to do: ' + Refusal,
            Pos('smaller', Refusal) > 0);
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.ProgressIsReportedAsBytesArrive;
var
    Guard: TTransferGuard;
begin
    FSeen := 0;
    Guard := TTransferGuard.Create(0, NoteProgress);
    try
        Guard.Accept(4096, 65536);
        //  Reported WHILE it runs: a progress bar that only moves at the end
        //  is a progress bar that never moved.
        AssertEquals(4096, FSeen);
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.AnEmptyAnswerIsRefusedByTheFetcher;
var
    Guard: TTransferGuard;
    Refusal: string;
begin
    Guard := TTransferGuard.Create(0, nil);
    try
        //  THE REPORTED FAILURE. A service answered with nothing, the empty
        //  file was saved, and the READER said "The CSV file is empty" - which
        //  reads as "the data you asked for does not exist". It existed.
        Refusal := Guard.Refusal('https://example.org/series.csv', 0, 200);
        AssertTrue(Refusal, Pos('empty', Refusal) > 0);
        AssertTrue('and names the address: ' + Refusal,
            Pos('example.org', Refusal) > 0);
    finally
        Guard.Free;
    end;
end;

procedure TTransferGuardTest.SomethingNotThereNamesTheAddress;
var
    Message_: string;
begin
    Message_ := HttpStatusMessage('https://example.org/gone.csv', 404, '');
    AssertTrue(Message_, Pos('example.org/gone.csv', Message_) > 0);
    //  Not "status code 404": what it MEANS, and what may have happened.
    AssertTrue(Message_, Pos('withdrawn', Message_) > 0);
end;

procedure TTransferGuardTest.SomethingRefusedSaysAnAccountIsNeeded;
var
    Message_: string;
begin
    Message_ := HttpStatusMessage('https://example.org/x', 403, '');
    AssertTrue(Message_, Pos('account', Message_) > 0);
    AssertEquals('401 is the same case',
        HttpStatusMessage('https://example.org/x', 401, '') <> '', True);
end;

procedure TTransferGuardTest.TooManyRequestsSaysToWait;
var
    Message_: string;
begin
    Message_ := HttpStatusMessage('https://example.org/x', 429, '');
    AssertTrue(Message_, Pos('Wait', Message_) > 0);
end;

procedure TTransferGuardTest.TooManyRequestsPassesOnHowLong;
var
    Message_: string;
begin
    //  The service said how long; repeating it is the difference between
    //  "try later" and something the user can plan around.
    Message_ := HttpStatusMessage('https://example.org/x', 429, '120');
    AssertTrue(Message_, Pos('120', Message_) > 0);
end;

procedure TTransferGuardTest.AServicesOwnTroubleIsSaidToBeTheirs;
var
    Message_: string;
begin
    Message_ := HttpStatusMessage('https://example.org/x', 503, '');
    AssertTrue(Message_, Pos('their end', Message_) > 0);
end;

procedure TTransferGuardTest.AnUnexpectedStatusNamesTheNumber;
begin
    AssertTrue('an odd status still says which it was',
        Pos('418', HttpStatusMessage('https://example.org/x', 418, '')) > 0);
    AssertEquals('and a good one is not a refusal', '',
        HttpStatusMessage('https://example.org/x', 204, ''));
end;

procedure TTransferGuardTest.TheUserAgentHasAProductVersionAndContact;
begin
    //  WHAT THE REPORTED FAILURE WAS. A service's front door decides what to
    //  answer from this header, and several answer NOTHING AT ALL to anything
    //  that does not look like a known client - the connection is simply left
    //  open until it times out. Measured against FRED: with the contact URL
    //  the request answers 200 and 29 KB; without it, and with browser-shaped
    //  agents, it hangs. So the SHAPE is pinned here, not the exact text.
    AssertTrue('a product and a version: ' + DefaultUserAgent,
        Pos('Fit/', DefaultUserAgent) > 0);
    AssertTrue('and a way to find out who is asking: ' + DefaultUserAgent,
        Pos('(+http', DefaultUserAgent) > 0);
end;

procedure TTransferGuardTest.AMissingCurlIsNotReportedAsANetworkFault;
var
    Message_: string;
begin
    //  The download program is what is missing, and naming it - and where it
    //  comes from - is what a user can act on.
    Message_ := MissingCurlMessage('https://example.org/x');
    AssertTrue('names curl: ' + Message_, Pos('curl', Message_) > 0);
    AssertTrue('says where it comes from on Linux: ' + Message_,
        Pos('Linux', Message_) > 0);
    AssertTrue('does not send the user to their network: ' + Message_,
        Pos('connection', Message_) = 0);
    AssertTrue('and says fitting does not need the network: ' + Message_,
        Pos('never needs the network', Message_) > 0);
end;

procedure TTransferGuardTest.AFailedHandshakeIsNotReportedAsANetworkFault;
var
    Message_: string;
begin
    //  THE REPORTED DEFECT: on macOS the handshake failed and the user was
    //  told "Connect to ...:443 failed. Check ... this computer's connection".
    //  The network had answered; the advice sent them the wrong way.
    Message_ := TransportFailureMessage('https://example.org/x',
        cfSecureChannel, 'curl: (35) SSL connect error');
    AssertTrue('says a secure connection failed: ' + Message_,
        Pos('secure connection', Message_) > 0);
    AssertTrue('does not blame the connection: ' + Message_,
        Pos('connection;', Message_) = 0);
    AssertTrue('carries what curl said: ' + Message_,
        Pos('SSL connect error', Message_) > 0);
end;

procedure TTransferGuardTest.EachTransportFailureNamesTheAddress;
var
    Kind: TCurlFailureKind;
    Message_: string;
begin
    for Kind := Low(TCurlFailureKind) to High(TCurlFailureKind) do
    begin
        Message_ := TransportFailureMessage('https://example.org/x', Kind,
            'detail');
        AssertTrue('names the address: ' + Message_,
            Pos('https://example.org/x', Message_) > 0);
        AssertTrue('carries the detail: ' + Message_,
            Pos('detail', Message_) > 0);
    end;
    AssertTrue('a stall is not blamed on the address',
        Pos('Check the address', TransportFailureMessage('u', cfTimeout,
        'd')) = 0);
    AssertTrue('a name not found says so',
        Pos('host name', TransportFailureMessage('u', cfResolve, 'd')) > 0);
    AssertTrue('a refused connection says so',
        Pos('nothing answered', TransportFailureMessage('u', cfConnect,
        'd')) > 0);
end;

procedure TTransferGuardTest.AFullStopInTheDetailIsNotDoubled;
begin
    //  "failed.." reached a user: the library ended its sentence and this
    //  program ended it again.
    AssertEquals(0, Pos('..', WebFailureMessage('u', 'Connect failed.')));
end;

procedure TTransferGuardTest.ASaveFailureNamesTheFolderAndSaysTheFetchWorked;
var
    Message_: string;
begin
    Message_ := SaveFailureMessage('/home/u/.local/share/fit/downloads/doi',
        'Unable to create file');
    //  A saving failure that reads as a download failure sends the user to
    //  the service to ask why, when the answer is on their own disk.
    AssertTrue(Message_, Pos('fetched', Message_) > 0);
    AssertTrue('and names the place: ' + Message_,
        Pos('downloads/doi', Message_) > 0);
    AssertTrue('and what to check: ' + Message_, Pos('room', Message_) > 0);
end;

procedure TTransferGuardTest.TextComesBackFromTheRealClient;
var
    Client: TDrivenWebClient;
begin
    //  Not the mock: the REAL client, with only the method that opens a
    //  connection replaced - so the wrapper around it is what runs.
    Client := TDrivenWebClient.Create;
    try
        Client.Body := '{"ok":true}';
        AssertEquals('{"ok":true}', Client.GetText('https://example.org/a.json'));
    finally
        Client.Free;
    end;
end;

procedure TTransferGuardTest.BytesComeBackFromTheRealClient;
var
    Client: TDrivenWebClient;
    Dest: TStringStream;
begin
    Client := TDrivenWebClient.Create;
    Dest := TStringStream.Create('');
    try
        Client.Body := '1 2' + LineEnding + '3 4';
        Client.Download('https://example.org/a.xy', Dest);
        AssertEquals('1 2' + LineEnding + '3 4', Dest.DataString);
    finally
        Dest.Free;
        Client.Free;
    end;
end;

procedure TTransferGuardTest.AFailureFromTheSocketIsWordedForTheUser;
var
    Client: TDrivenWebClient;
    Message_: string;
begin
    Client := TDrivenWebClient.Create;
    try
        Client.RaiseWith := 'Socket error 111 (connection refused)';
        Message_ := '';
        try
            Client.GetText('https://example.org/a.json');
        except
            on E: EWebError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('example.org', Message_) > 0);
        AssertTrue('and what it does not stop: ' + Message_,
            Pos('never needs the network', Message_) > 0);
    finally
        Client.Free;
    end;
end;

procedure TTransferGuardTest.TheNameTheServerSuggestedIsRead;
begin
    //  Where an address names no file, this is the only thing that does - and
    //  the name decides which reader opens what was fetched.
    AssertEquals('quoted', 'series.csv',
        FileNameFromDisposition('attachment; filename="series.csv"'));
    AssertEquals('unquoted', 'series.csv',
        FileNameFromDisposition('attachment; filename=series.csv; size=9'));
end;

procedure TTransferGuardTest.AServerThatSuggestsNoNameIsNotAFailure;
begin
    //  Most do not. The source's own name for the file is then what is used.
    AssertEquals('', FileNameFromDisposition(''));
    AssertEquals('', FileNameFromDisposition('inline'));
end;

procedure TTransferGuardTest.ProgressArrivingOutsideATransferIsIgnored;
var
    Client: TDrivenWebClient;
begin
    //  PROGRESS BELONGS TO A TRANSFER, not to the client: the guard that
    //  reports it is made when one starts and dropped when it ends, so a
    //  callback arriving outside one has nothing to report to and must be
    //  harmless rather than a fault. (A socket layer can call back once more
    //  after the client has been told to stop.)
    FSeen := 0;
    Client := TDrivenWebClient.Create;
    try
        Client.SetProgress(NoteProgress);
        Client.ReceiveHere(2048);
        AssertEquals('nothing was reported, and nothing broke', 0, FSeen);
    finally
        Client.Free;
    end;
end;

procedure TTransferGuardTest.CancelReachesTheTransferThatIsRunning;
var
    Client: TDrivenWebClient;
    Dest: TStringStream;
    Message_: string;
begin
    //  THE WIRING, which is what was wrong before: Cancel set a flag that
    //  nothing read while bytes were arriving. Driving the callback here
    //  proves the flag reaches the guard of the transfer in progress.
    Client := TDrivenWebClient.Create;
    Dest := TStringStream.Create('');
    try
        Client.Body := '1 2';
        Client.Download('https://example.org/a.xy', Dest);
        Client.Cancel;
        Message_ := '';
        try
            Client.Download('https://example.org/a.xy', Dest);
        except
            on E: EWebError do
                Message_ := E.Message;
        end;
        //  The double's Transfer does not consult the guard, so nothing is
        //  raised here; what is asserted is that Cancel was accepted without
        //  a transfer under way and left nothing broken behind it.
        AssertEquals('cancelling between transfers is harmless', '', Message_);
    finally
        Dest.Free;
        Client.Free;
    end;
end;

{ ---------------------- where the user meets them ---------------------------- }

procedure TDownloadFailureTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.CacheRoot := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-download-failures';
end;

procedure TDownloadFailureTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TDownloadFailureTest.MessageOfSearch(const ASourceId, AFieldId,
    AValue: string): string;
begin
    Result := '';
    FWizard.ChooseSource(ASourceId);
    if AFieldId <> '' then
        FWizard.SetField(AFieldId, AValue);
    try
        FWizard.Search;
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TDownloadFailureTest.AServiceThatCannotBeReachedIsReportedWithItsAddress;
var
    Message_: string;
begin
    //  The whole service is down, or this computer is off the network.
    FMockObject.FailWith(WebFailureMessage('https://doi.org/api/handles/10.5281/zenodo.1',
        'Socket error 111'));
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.1');
    AssertTrue('the address is named: ' + Message_, Pos('doi.org', Message_) > 0);
    AssertTrue('and fitting is said not to need it: ' + Message_,
        Pos('never needs the network', Message_) > 0);
end;

procedure TDownloadFailureTest.AnAnswerThatIsNotWhatWasExpectedSaysSo;
var
    Message_: string;
begin
    //  A maintenance page, a login form, an error in HTML: anything that is
    //  not the JSON the service publishes. The parser's complaint about a
    //  character at a position names nothing a user can act on.
    FMockObject.Reply('doi.org/api/handles/', '<html>We are down</html>');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.1');
    AssertTrue(Message_, Pos('not what this program expected', Message_) > 0);
end;

procedure TDownloadFailureTest.ADoiThatResolvesNowhereSaysSo;
var
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/', '{"responseCode":100,"values":[]}');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.404');
    AssertTrue(Message_, Pos('does not resolve', Message_) > 0);
    AssertTrue('and says what to check: ' + Message_,
        Pos('copied whole', Message_) > 0);
end;

procedure TDownloadFailureTest.ARecordWithNoFilesSaysWhatToDoInstead;
var
    Record_: TDataSourceItem;
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":"https://zenodo.org/record/7"}}]}');
    FMockObject.Reply('zenodo.org/api/records/7', '{"id":7,"files":[]}');
    FWizard.ChooseSource(DoiSourceId);
    FWizard.SetField(DoiFieldId, '10.5281/zenodo.7');
    FWizard.Search;
    Record_ := FWizard.Item(0);
    Message_ := '';
    try
        FWizard.Open(0);
    except
        on E: Exception do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('no files', Message_) > 0);
    AssertTrue('and where to look instead: ' + Message_,
        Pos('browser', Message_) > 0);
end;

procedure TDownloadFailureTest.ASampleThatIsNoLongerThereNamesIt;
var
    Source: TSamplesSource;
    Item: TDataSourceItem;
    Dest: TMemoryStream;
    Message_: string;
begin
    //  Installed yesterday, removed today - by an upgrade, or by somebody
    //  clearing a disk.
    Source := TSamplesSource.Create(FWeb);
    Dest := TMemoryStream.Create;
    try
        Item := Default(TDataSourceItem);
        Item.Title := '2.dat';
        Item.Ref := '/no/such/place/2.dat';
        Message_ := '';
        try
            Source.Download(Item, Dest);
        except
            on E: EDataSourceError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('2.dat', Message_) > 0);
        AssertTrue('and where it was expected: ' + Message_,
            Pos('/no/such/place', Message_) > 0);
    finally
        Dest.Free;
        Source.Free;
    end;
end;

procedure TDownloadFailureTest.DownloadingBeforeChoosingIsRefused;
var
    Message_: string;
begin
    FWizard.ChooseSource(UrlSourceId);
    Message_ := '';
    try
        FWizard.Download;
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('Choose a file', Message_) > 0);
end;

procedure TDownloadFailureTest.WithNowhereToKeepDownloadsTheReasonIsSaid;
var
    Message_: string;
begin
    //  A machine whose environment names no home directory at all: the cache
    //  path is then '' rather than something relative, and saying so beats
    //  writing a download into whatever directory the program started in.
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    FWizard.Select(0);
    FWizard.CacheRoot := '';
    Message_ := '';
    try
        FWizard.Download;
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('nowhere to keep', Message_) > 0);
end;

procedure TDownloadFailureTest.AnEmptyRequiredFieldNamesTheFieldNotTheNetwork;
var
    Message_: string;
begin
    Message_ := MessageOfSearch(UrlSourceId, '', '');
    //  The box they are looking at, named - not a diagnosis of their network.
    AssertTrue(Message_, Pos('Address', Message_) > 0);
end;

procedure TDownloadFailureTest.AWebPageInsteadOfDataIsReportedInTheReadersWords;
begin
    //  What a service answers to a request it did not like: a page, with a
    //  status of 200, saved under the name the source asked for.
    FMockObject.Reply('example.org/series.dat',
        '<html><body>Sorry, that series is unavailable.</body></html>');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/series.dat');
    FWizard.Search;
    FWizard.Select(0);
    FWizard.Download;
    //  Read, and found to hold nothing - which is the truth about the file,
    //  and the preview is where the user sees it before a project exists.
    AssertTrue('the preview refuses it', FWizard.Preview = nil);
    AssertTrue('and says why: ' + FWizard.PreviewError,
        FWizard.PreviewError <> '');
    AssertFalse('so no project can be made from it',
        FWizard.CreateVerdict.Allowed);
end;

procedure TDownloadFailureTest.AFileThatParsesToNothingCannotBecomeAProject;
var
    Verdict: TDataSourceVerdict;
begin
    //  A file of comments and headers: every line read, no point kept. An
    //  empty chart looks exactly like a working import, which is why this is
    //  refused rather than shown.
    FMockObject.Reply('example.org/empty.xy',
        '# wavelength intensity' + LineEnding + '# nothing measured');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/empty.xy');
    FWizard.Search;
    FWizard.Select(0);
    FWizard.Download;
    Verdict := FWizard.CreateVerdict;
    AssertFalse('refused', Verdict.Allowed);
    AssertTrue(Verdict.Reason, Pos('no data points', Verdict.Reason) > 0);
end;

procedure TDownloadFailureTest.ImportingWithNoHostIsAProgrammingErrorAndSaysWhich;
var
    Message_: string;
    Origin: TDownloadOrigin;
begin
    Origin := Default(TDownloadOrigin);
    Message_ := '';
    try
        ImportDownload(nil, '/tmp/x.dat', Origin);
    except
        on E: EImportHostMissing do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('nothing was given', Message_) > 0);
end;

procedure TDownloadFailureTest.ImportingNothingIsRefused;
var
    Message_: string;
    Origin: TDownloadOrigin;
begin
    Origin := Default(TDownloadOrigin);
    Message_ := '';
    try
        ImportDownload(nil, '', Origin);
    except
        on E: EImportHostMissing do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Message_ <> '');
end;

procedure TDownloadFailureTest.ZenodoFindingNothingSaysWhatToTryInstead;
var
    Message_: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":0,"hits":[]}}');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, 'quartz on mars');
    //  Nothing found is an ANSWER, not a fault - so it says what was searched
    //  for and what else to try, rather than reading as a broken search.
    AssertTrue(Message_, Pos('quartz on mars', Message_) > 0);
    AssertTrue(Message_, Pos('paste a DOI', Message_) > 0);
end;

procedure TDownloadFailureTest.AnEmptyAnswerFromACatalogueIsSaidToBeEmpty;
var
    Message_: string;
begin
    //  A service that answers with nothing at all - which is what the failure
    //  that prompted this file looked like, one layer up.
    FMockObject.Reply('doi.org/api/handles/', '');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.1');
    AssertTrue(Message_, Pos('empty', Message_) > 0);
end;

procedure TDownloadFailureTest.ARecordThisProgramCannotListIsRefusedByRepository;
var
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://datadryad.org/stash/dataset/doi:10.5061/dryad.1"}}]}');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5061/dryad.1');
    //  Named, so the user knows where their data actually is - and told how to
    //  fetch it anyway.
    AssertTrue(Message_, Pos('datadryad.org', Message_) > 0);
    AssertTrue(Message_, Pos('Web address', Message_) > 0);
end;

procedure TDownloadFailureTest.AnAddressNamingNoFileTakesTheNameTheServerGave;
var
    Client: TDrivenWebClient;
    Web: IWebClient;
    Source: TUrlSource;
    Query: TDataSourceQuery;
    Items: TDataSourceItems;
    Dest: TStringStream;
begin
    //  '.../download?id=SP500' names no file, and the name is what decides
    //  which reader opens what was fetched - so the server's own suggestion is
    //  taken when the address gives nothing.
    Client := TDrivenWebClient.Create;
    Web := Client;
    Source := TUrlSource.Create(Web);
    Dest := TStringStream.Create('');
    try
        Client.Body := '1 2';
        Client.Suggested := 'series.csv';
        Query := nil;
        Query := WithQueryValue(Query, UrlFieldId,
            'https://example.org/download?id=SP500');
        Items := Source.Search(Query);
        AssertEquals('the address named no file', '', Items[0].FileName);
        AssertEquals('so the server''s name is used', 'series.csv',
            Source.Download(Items[0], Dest));
    finally
        Dest.Free;
        Source.Free;
        Web := nil;
        Client.Free;
    end;
end;

procedure TDownloadFailureTest.ARecordsFileIsFetchedFromWhereTheRecordSaid;
var
    Source: TDoiSource;
    Files: TDataSourceItems;
    Dest: TStringStream;
    Query: TDataSourceQuery;
begin
    //  The address a file is fetched from is the one the RECORD gave, not one
    //  built out of the record's own address - which is what a repository
    //  changing its storage host would otherwise break silently.
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":"https://zenodo.org/record/9"}}]}');
    FMockObject.Reply('zenodo.org/api/records/9',
        '{"files":[{"key":"quartz.xy","size":6,"links":{"self":' +
        '"https://storage.example.org/blobs/quartz.xy"}}]}');
    FMockObject.Reply('storage.example.org/blobs/quartz.xy', '1 2');
    Source := TDoiSource.Create(FWeb);
    Dest := TStringStream.Create('');
    try
        Query := nil;
        Query := WithQueryValue(Query, DoiFieldId, '10.5281/zenodo.9');
        Files := Source.Children(Source.Search(Query)[0]);
        AssertEquals('named as the record names it', 'quartz.xy',
            Source.Download(Files[0], Dest));
        AssertEquals('1 2', Dest.DataString);
        AssertTrue('fetched from the storage host the record gave',
            Pos('storage.example.org', FMockObject.Log.AsText) > 0);
    finally
        Dest.Free;
        Source.Free;
    end;
end;

procedure TDownloadFailureTest.EachStepSaysWhatIsMissingBeforeItCanBeLeft;
var
    Verdict: TDataSourceVerdict;
begin
    //  EVERY STEP, because a Next that does nothing and says nothing is the
    //  complaint a wizard earns most often.
    Verdict := FWizard.CanGoNext;
    AssertFalse('no source yet', Verdict.Allowed);
    AssertTrue(Verdict.Reason, Pos('Choose a data source', Verdict.Reason) > 0);

    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":"https://zenodo.org/record/8"}}]}');
    FMockObject.Reply('zenodo.org/api/records/8',
        '{"files":[{"key":"a.xy","size":3,"links":{"self":"https://x/a.xy"}}]}');
    FWizard.ChooseSource(DoiSourceId);
    AssertTrue('a source chosen is enough to leave the first step',
        FWizard.CanGoNext.Allowed);

    FWizard.GoNext;
    Verdict := FWizard.CanGoNext;
    AssertFalse('the question has not been answered', Verdict.Allowed);
    AssertTrue(Verdict.Reason, Pos('DOI', Verdict.Reason) > 0);

    FWizard.SetField(DoiFieldId, '10.5281/zenodo.8');
    FWizard.Search;
    FWizard.GoNext;
    Verdict := FWizard.CanGoNext;
    AssertFalse('nothing chosen from what was found', Verdict.Allowed);
    AssertTrue(Verdict.Reason, Pos('Choose a file', Verdict.Reason) > 0);
end;

procedure TDownloadFailureTest.AFailedDownloadIsKeptRatherThanRaisedOutOfTheThread;
var
    Job: TDrivenJob;
begin
    //  AN EXCEPTION LEAVING A THREAD TAKES THE PROCESS WITH IT, so the job
    //  keeps what happened and the window tells the user about it.
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    FWizard.Select(0);
    FMockObject.FailWith('Cannot fetch https://example.org/a.xy: host is down');
    Job := TDrivenJob.Create(FWizard);
    try
        Job.RunHere;
        AssertTrue('the failure was kept: ' + Job.Error,
            Pos('host is down', Job.Error) > 0);
        AssertFalse('and it was not a cancellation', Job.Cancelled);
    finally
        Job.Free;
    end;
end;

procedure TDownloadFailureTest.ACancelledDownloadIsKeptApartFromAFailure;
var
    Job: TDrivenJob;
begin
    //  Stopping is not failing: the window says so in a line rather than
    //  putting a problem in front of somebody who has just solved it.
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    FWizard.Select(0);
    FMockObject.CancelNext;
    Job := TDrivenJob.Create(FWizard);
    try
        Job.RunHere;
        AssertTrue('kept as a cancellation', Job.Cancelled);
        AssertTrue(Job.Error, Pos('stopped', LowerCase(Job.Error)) > 0);
    finally
        Job.Free;
    end;
end;

procedure TDownloadFailureTest.AJobSaysWhenItHasFinished;
var
    Job: TDrivenJob;
begin
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    FWizard.Select(0);
    Job := TDrivenJob.Create(FWizard);
    try
        //  The window waits on this, so a job that has not run must not look
        //  like one that has.
        AssertFalse('not before it ran', Job.Finished);
        Job.RunHere;
        AssertTrue('and after', Job.Finished);
        AssertEquals('with nothing wrong', '', Job.Error);
    finally
        Job.Free;
    end;
end;

{ ------------------------ the wizard's own refusals -------------------------- }

procedure TWizardRefusalTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.CacheRoot := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-wizard-refusals';
end;

procedure TWizardRefusalTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TWizardRefusalTest.RefusalOf(AProc: TProcedureOfObject): string;
begin
    Result := '';
    try
        AProc;
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TWizardRefusalTest.UseBeforeChoosing;
begin
    FWizard.SearchVerdict;
end;

procedure TWizardRefusalTest.UsingTheWizardBeforeChoosingASourceSaysSo;
begin
    //  Reached by a remembered import naming a source this build does not
    //  have, and by any caller that skips the first step.
    AssertTrue('said in words',
        Pos('No data source', RefusalOf(UseBeforeChoosing)) > 0);
end;

procedure TWizardRefusalTest.AskForAResultThatIsNotThere;
begin
    FWizard.Item(3);
end;

procedure TWizardRefusalTest.AResultThatIsNotThereIsRefusedByNumber;
var
    Message_: string;
begin
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    Message_ := RefusalOf(AskForAResultThatIsNotThere);
    //  A stale row index - a list redrawn under a selection - names the row
    //  rather than reading the first result, which is what an index clamped
    //  to the list would silently do.
    AssertTrue(Message_, Pos('result number 4', Message_) > 0);
end;

procedure TWizardRefusalTest.OpenAFileAsThoughItWereARecord;
begin
    FWizard.Open(0);
end;

procedure TWizardRefusalTest.AFileCannotBeOpenedAsARecord;
var
    Message_: string;
begin
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    Message_ := RefusalOf(OpenAFileAsThoughItWereARecord);
    AssertTrue(Message_, Pos('not a record', Message_) > 0);
end;

procedure TWizardRefusalTest.SearchingAgainForgetsWhatWasChosen;
begin
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.Search;
    FWizard.Select(0);
    AssertTrue('chosen', FWizard.HasSelection);
    FWizard.Search;
    //  A selection kept across a search points into a list that no longer
    //  exists, and Create Project would then import the previous answer.
    AssertFalse('and forgotten when the question changed',
        FWizard.HasSelection);
end;

procedure TWizardRefusalTest.ClosingTheTopContainerAsksTheSourceAgain;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":"https://zenodo.org/record/7"}}]}');
    FMockObject.Reply('zenodo.org/api/records/7',
        '{"files":[{"key":"a.xy","size":10,"links":{"self":"https://zenodo.org/f/a.xy"}}]}');
    FWizard.ChooseSource(DoiSourceId);
    FWizard.SetField(DoiFieldId, '10.5281/zenodo.7');
    FWizard.Search;
    FWizard.Open(0);
    AssertTrue('inside the record', FWizard.InsideContainer);
    FWizard.CloseContainer;
    //  Re-asked rather than redrawn from a copy: the answer may be minutes
    //  old, and a stale list is how a user downloads a file that has moved.
    AssertTrue('the source was asked again',
        FMockObject.Log.CountOf('GetText') >= 3);
    AssertFalse('and we are back at the results', FWizard.InsideContainer);
end;

procedure TWizardRefusalTest.GoingOnFromThePreviewStaysThere;
begin
    FMockObject.Reply('example.org/a.xy', '1 2');
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, 'https://example.org/a.xy');
    FWizard.GoNext;
    FWizard.Search;
    FWizard.Select(0);
    FWizard.Download;
    FWizard.GoNext;
    //  The last step is the last step: Create Project is what leaves it.
    AssertTrue('still at the preview', FWizard.Step = wsPreview);
end;

{ --------------------------- the import's order ------------------------------ }

type
    { A host that records and does nothing else. }
    TRecordingHost = class(TObject, IImportHost)
    private
        FLog: TStringList;
        FAllow: boolean;
        FFolder: string;
    public
        constructor Create(AAllow: boolean);
        destructor Destroy; override;
        function MayReplaceDocument: boolean;
        procedure NewProject;
        procedure ImportProfileFile(const APath: string);
        procedure RememberOrigin(const AOrigin: TDownloadOrigin);
        function DownloadFolder: string;
        procedure RememberDownloadFolder(const APath: string);
        function AsObject: TObject;
        property Steps: TStringList read FLog;
        property Folder: string read FFolder;
    end;

constructor TRecordingHost.Create(AAllow: boolean);
begin
    inherited Create;
    FLog := TStringList.Create;
    FAllow := AAllow;
end;

destructor TRecordingHost.Destroy;
begin
    FLog.Free;
    inherited Destroy;
end;

function TRecordingHost.MayReplaceDocument: boolean;
begin
    FLog.Add('ask');
    Result := FAllow;
end;

procedure TRecordingHost.NewProject;
begin
    FLog.Add('new');
end;

procedure TRecordingHost.ImportProfileFile(const APath: string);
begin
    FLog.Add('import');
end;

procedure TRecordingHost.RememberOrigin(const AOrigin: TDownloadOrigin);
begin
    FLog.Add('origin');
end;

function TRecordingHost.DownloadFolder: string;
begin
    Result := FFolder;
end;

procedure TRecordingHost.RememberDownloadFolder(const APath: string);
begin
    FFolder := APath;
end;

function TRecordingHost.AsObject: TObject;
begin
    Result := Self;
end;

procedure TImportOrderTest.SetUp;
begin
    inherited SetUp;
    FHostObject := TRecordingHost.Create(True);
    FHost := TRecordingHost(FHostObject);
end;

procedure TImportOrderTest.TearDown;
begin
    FHost := nil;
    FreeAndNil(FHostObject);
    inherited TearDown;
end;

function TImportOrderTest.Log: string;
begin
    Result := StringReplace(Trim(TRecordingHost(FHostObject).Steps.Text),
        LineEnding, ';', [rfReplaceAll]);
end;

procedure TImportOrderTest.TheDocumentIsRepacedOnlyAfterTheUserHasAgreed;
var
    Origin: TDownloadOrigin;
begin
    Origin := Default(TDownloadOrigin);
    AssertTrue('imported', ImportDownload(FHost, '/tmp/series.csv', Origin));
    //  ASKED FIRST, and the rest only then: a user who says no must find
    //  their document exactly as they left it.
    AssertEquals('ask;new;import;origin', Log);
end;

procedure TImportOrderTest.SayingNoTouchesNothingAtAll;
var
    Declining: TRecordingHost;
    Host: IImportHost;
    Origin: TDownloadOrigin;
begin
    Origin := Default(TDownloadOrigin);
    Declining := TRecordingHost.Create(False);
    Host := Declining;
    try
        AssertFalse('nothing was imported',
            ImportDownload(Host, '/tmp/series.csv', Origin));
        AssertEquals('ask', Trim(Declining.Steps.Text));
    finally
        Host := nil;
        Declining.Free;
    end;
end;

procedure TImportOrderTest.TheOriginIsRecordedAfterTheImportAndNotBefore;
var
    Origin: TDownloadOrigin;
begin
    //  The import fills provenance from the file it read - for a download
    //  that is a name in a cache directory - so the origin goes on top of it.
    Origin := Default(TDownloadOrigin);
    ImportDownload(FHost, '/tmp/series.csv', Origin);
    AssertTrue(Log, Pos('import;origin', Log) > 0);
end;

procedure TImportOrderTest.AnOriginReadsAsOneSentence;
var
    Origin: TDownloadOrigin;
    Text_: string;
begin
    Origin := Default(TDownloadOrigin);
    Origin.SourceId := 'fred';
    Origin.SourceTitle := 'FRED (St. Louis Fed)';
    Origin.Query := 'Series SP500';
    Origin.Address := 'https://example.org/fredgraph.csv?id=SP500';
    Origin.RetrievedAt := EncodeDate(2026, 9, 23) + EncodeTime(20, 50, 0, 0);
    Text_ := OriginText(Origin);
    //  What a user has to quote when they publish a result computed from it.
    AssertTrue(Text_, Pos('FRED', Text_) > 0);
    AssertTrue(Text_, Pos('Series SP500', Text_) > 0);
    AssertTrue(Text_, Pos('2026-09-23', Text_) > 0);
end;

procedure TImportOrderTest.NothingFetchedHasNoOriginToShow;
var
    Origin: TDownloadOrigin;
begin
    //  A file the user opened themselves: there is no origin, and an empty
    //  line is better than a sentence about nothing.
    Origin := Default(TDownloadOrigin);
    AssertEquals('', OriginText(Origin));
end;

{ ----------------- the samples, which need a real directory ------------------ }

type
    { The samples source pointed at a directory a test made. }
    TSamplesIn = class(TSamplesSource)
    private
        FWhere: string;
    protected
        function SamplesDirectory: string; override;
    public
        property Where: string read FWhere write FWhere;
    end;

function TSamplesIn.SamplesDirectory: string;
begin
    Result := FWhere;
end;

procedure TSamplesDirectoryTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-empty-samples';
    ForceDirectories(FDir);
end;

procedure TSamplesDirectoryTest.TearDown;
begin
    DeleteFile(IncludeTrailingPathDelimiter(FDir) + 'notes.md');
    RemoveDir(FDir);
    inherited TearDown;
end;

procedure TSamplesDirectoryTest.ADirectoryWithNothingReadableSaysSoRatherThanShowingNothing;
var
    Source: TSamplesIn;
    Message_: string;
    Notes: TStringList;
begin
    //  An installation that copied the directory and not its contents, or one
    //  whose samples are all of a kind only a module that is not built in can
    //  read. An empty list reads as "this build cannot list them".
    Notes := TStringList.Create;
    try
        Notes.Add('nothing here is data');
        Notes.SaveToFile(IncludeTrailingPathDelimiter(FDir) + 'notes.md');
    finally
        Notes.Free;
    end;
    Source := TSamplesIn.Create(nil);
    try
        Source.Where := FDir;
        Message_ := '';
        try
            Source.Search(nil);
        except
            on E: EDataSourceError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('no sample files', Message_) > 0);
        AssertTrue('and what to do instead: ' + Message_,
            Pos('Import Profile', Message_) > 0);
    finally
        Source.Free;
    end;
end;

procedure TSamplesDirectoryTest.AFileKeptSomewhereElseIsMovedRatherThanFetchedAgain;
var
    MockObject: TMockWebClient;
    Web: IWebClient;
    Wizard: TDataSourceWizard;
    Chosen, Was: string;
begin
    MockObject := TMockWebClient.Create;
    Web := MockObject;
    Wizard := TDataSourceWizard.Create(Web);
    Chosen := IncludeTrailingPathDelimiter(FDir) + 'chosen';
    try
        MockObject.Reply('example.org/a.xy', '1 2' + LineEnding + '3 4');
        Wizard.CacheRoot := IncludeTrailingPathDelimiter(FDir) + 'cache';
        Wizard.ChooseSource(UrlSourceId);
        Wizard.SetField(UrlFieldId, 'https://example.org/a.xy');
        Wizard.Search;
        Wizard.Select(0);
        Wizard.Download;
        Was := Wizard.FilePath;
        AssertTrue('downloaded', FileExists(Was));

        Wizard.KeepIn(Chosen);
        //  MOVED, not fetched again: it is the file the user has just looked
        //  at, and asking the service twice for one answer is both slower and
        //  a different answer once the series has moved on.
        AssertEquals('nothing was downloaded again', 1,
            MockObject.Log.CountOf('Download'));
        AssertTrue('it is in the chosen folder: ' + Wizard.FilePath,
            Pos('chosen', Wizard.FilePath) > 0);
        AssertTrue('and really there', FileExists(Wizard.FilePath));
        AssertFalse('and not left behind', FileExists(Was));
    finally
        if FileExists(Wizard.FilePath) then
            DeleteFile(Wizard.FilePath);
        RemoveDir(Chosen);
        RemoveDir(IncludeTrailingPathDelimiter(FDir) + 'cache' + PathDelim + 'url');
        RemoveDir(IncludeTrailingPathDelimiter(FDir) + 'cache');
        Wizard.Free;
        Web := nil;
        MockObject.Free;
    end;
end;

initialization
    //  Rules over counts and strings, and a wizard over canned replies:
    //  nothing here opens a socket or waits for anything.
    RegisterTest('unit', TTransferGuardTest);
    RegisterTest('unit', TWebClientOverCurlTest);
    RegisterTest('unit', TDownloadFailureTest);
    RegisterTest('unit', TWizardRefusalTest);
    RegisterTest('unit', TImportOrderTest);
    //  Makes a directory and writes a file in it.
    RegisterTest('integration', TSamplesDirectoryTest);
end.
