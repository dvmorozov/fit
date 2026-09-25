// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the curl client decides, with no process and no socket.)

Everything here is a rule about text or about a recorded answer: which
arguments curl is given, where curl is looked for, which of several response
blocks is the one that counts, and what an exit code means. TCurlClient itself is
driven through its Run seam with a recorded answer, so Get - the method every
caller reaches - runs exactly as the application runs it. The real Run starts a
process, and is covered by testcase_web_transport, over real sockets.
}
unit testcase_curl_client;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, curl_client, mock_curl_client;

type
    TCurlRulesTest = class(TTestCase)
    published
        procedure TheUsersOwnCurlSettingsAreNotRead;
        procedure TheAddressIsOneArgumentAndNeverAnOption;
        procedure OnlyWebProtocolsAreFollowed;
        procedure TheLimitsReachCurlInItsOwnUnits;
        procedure TheHeaderDumpGoesWhereItIsAsked;
        procedure TheMacLooksAtTheSystemCopyFirst;
        procedure WindowsLooksInSystem32First;
        procedure EveryHostFallsBackToThePath;
        procedure AnEmptyPathAddsNothing;
        procedure TheLastResponseAfterRedirectsIsTheOneThatCounts;
        procedure AHeaderIsFoundWhateverItsCase;
        procedure AHeaderOnlyAnEarlierResponseSentIsNotReported;
        procedure NoResponseAtAllHasNoStatus;
        procedure ANameThatDidNotResolveIsSaidToBeOne;
        procedure ARefusedConnectionIsSaidToBeOne;
        procedure ATimeoutIsSaidToBeOne;
        procedure AFailedHandshakeIsASecureChannelFailure;
        procedure ARejectedCertificateIsASecureChannelFailure;
        procedure SuccessIsNoFailure;
        procedure AnythingElseIsOther;
    end;

    TCurlClientTest = class(TTestCase)
    private
        FClient: TRecordedCurlClient;
        FStopAfter: int64;
        FAsked: longint;
        function StopPastLimit(ABytesSoFar: int64): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheBodyIsWrittenAsItArrives;
        procedure TheStatusIsTheLastResponses;
        procedure TheHeadersAreTheLastResponsesOnly;
        procedure ACurlFailureIsCarriedWithItsWords;
        procedure StoppingIsReported;
        procedure TheAddressReachesTheProcessUnchanged;
    end;

implementation

function IndexOfArg(const AArgs: TStringArray; const AValue: string): integer;
var
    i: integer;
begin
    for i := 0 to High(AArgs) do
        if AArgs[i] = AValue then
            Exit(i);
    Result := -1;
end;

function ArgsFor(const AUrl: string): TStringArray;
begin
    Result := CurlArguments(AUrl, 'Fit/1.0 (+https://x)', 30000, 5, 'hdr.txt');
end;

procedure TCurlRulesTest.TheUsersOwnCurlSettingsAreNotRead;
begin
    //  A ~/.curlrc can add a proxy, --fail, a different output or --insecure,
    //  and none of it would be visible from here. curl only honours --disable
    //  as the FIRST argument.
    AssertEquals('--disable', ArgsFor('https://example.org/')[0]);
end;

procedure TCurlRulesTest.TheAddressIsOneArgumentAndNeverAnOption;
var
    Args: TStringArray;
    i: integer;
begin
    //  NO SHELL, AND --url RATHER THAN A BARE ARGUMENT. An address is whatever a
    //  user typed or a catalogue named; one beginning with '-' must not become
    //  an option, and one holding spaces or quotes must reach curl whole.
    Args := ArgsFor('-o /etc/x "a b"');
    i := IndexOfArg(Args, '--url');
    AssertTrue('the address is introduced by --url', i >= 0);
    AssertEquals('and follows it whole', '-o /etc/x "a b"', Args[i + 1]);
    AssertEquals('it is the last thing said', High(Args), i + 1);
end;

procedure TCurlRulesTest.OnlyWebProtocolsAreFollowed;
var
    Args: TStringArray;
    i: integer;
begin
    //  A redirect to file:// or to some other scheme would read the user's own
    //  disk, or talk a protocol this program never meant to.
    Args := ArgsFor('https://example.org/');
    i := IndexOfArg(Args, '--proto');
    AssertTrue('--proto given', i >= 0);
    AssertEquals('=http,https', Args[i + 1]);
    i := IndexOfArg(Args, '--proto-redir');
    AssertTrue('--proto-redir given', i >= 0);
    AssertEquals('=http,https', Args[i + 1]);
end;

procedure TCurlRulesTest.TheLimitsReachCurlInItsOwnUnits;
var
    Args: TStringArray;
    i: integer;
begin
    Args := ArgsFor('https://example.org/');
    i := IndexOfArg(Args, '--max-redirs');
    AssertEquals('redirects', '5', Args[i + 1]);
    //  curl counts in SECONDS; the client in milliseconds.
    i := IndexOfArg(Args, '--connect-timeout');
    AssertEquals('connect timeout in seconds', '30', Args[i + 1]);
    //  THE INACTIVITY LIMIT, which is what the built-in client's IOTimeout was:
    //  a transfer that stalls is stopped, one that is merely long is not.
    i := IndexOfArg(Args, '--speed-time');
    AssertEquals('stall time in seconds', '30', Args[i + 1]);
    i := IndexOfArg(Args, '--speed-limit');
    AssertEquals('stalled means below one byte a second', '1', Args[i + 1]);
    i := IndexOfArg(Args, '--user-agent');
    AssertEquals('Fit/1.0 (+https://x)', Args[i + 1]);
    AssertTrue('redirects followed', IndexOfArg(Args, '--location') >= 0);
    AssertTrue('quiet, but errors said',
        (IndexOfArg(Args, '--silent') >= 0) and
        (IndexOfArg(Args, '--show-error') >= 0));
end;

procedure TCurlRulesTest.TheHeaderDumpGoesWhereItIsAsked;
var
    Args: TStringArray;
    i: integer;
begin
    Args := ArgsFor('https://example.org/');
    i := IndexOfArg(Args, '--dump-header');
    AssertTrue('--dump-header given', i >= 0);
    AssertEquals('hdr.txt', Args[i + 1]);
end;

procedure TCurlRulesTest.TheMacLooksAtTheSystemCopyFirst;
var
    C: TStringArray;
begin
    C := CurlCandidates(chMac, '', '/opt/bin:/usr/local/bin');
    AssertEquals('/usr/bin/curl', C[0]);
end;

procedure TCurlRulesTest.WindowsLooksInSystem32First;
var
    C: TStringArray;
begin
    //  THE COPY WINDOWS SHIPS, before anything a PATH happens to hold: another
    //  program's curl may be old, or built without the system's TLS.
    C := CurlCandidates(chWindows, 'C:\Windows', 'D:\tools;C:\bin');
    AssertEquals('C:\Windows\System32\curl.exe', C[0]);
    AssertEquals('then the path', 'D:\tools\curl.exe', C[1]);
    AssertEquals('C:\bin\curl.exe', C[2]);
end;

procedure TCurlRulesTest.EveryHostFallsBackToThePath;
var
    C: TStringArray;
begin
    C := CurlCandidates(chUnix, '', '/opt/bin:/usr/bin');
    AssertEquals(2, Length(C));
    AssertEquals('/opt/bin/curl', C[0]);
    AssertEquals('/usr/bin/curl', C[1]);
end;

procedure TCurlRulesTest.AnEmptyPathAddsNothing;
var
    C: TStringArray;
begin
    AssertEquals('no path, no candidates', 0,
        Length(CurlCandidates(chUnix, '', '')));
    AssertEquals('an empty entry is skipped, not read as the current folder',
        1, Length(CurlCandidates(chUnix, '', '::/opt/bin:')));
    C := CurlCandidates(chWindows, '', '');
    AssertEquals('no SystemRoot, no guess at one', 0, Length(C));
end;

procedure TCurlRulesTest.TheLastResponseAfterRedirectsIsTheOneThatCounts;
begin
    AssertEquals(200, LastResponseStatus(RedirectDump));
end;

procedure TCurlRulesTest.AHeaderIsFoundWhateverItsCase;
begin
    //  HTTP/2 sends every name in lower case; HTTP/1.1 servers mostly do not.
    AssertEquals('text/csv', LastResponseHeader(RedirectDump, 'Content-Type'));
    AssertEquals('attachment; filename="series.csv"',
        LastResponseHeader(RedirectDump, 'content-disposition'));
end;

procedure TCurlRulesTest.AHeaderOnlyAnEarlierResponseSentIsNotReported;
begin
    //  The redirect's Retry-After is not the file's.
    AssertEquals('', LastResponseHeader(RedirectDump, 'Retry-After'));
end;

procedure TCurlRulesTest.NoResponseAtAllHasNoStatus;
begin
    AssertEquals(0, LastResponseStatus(''));
    AssertEquals('', LastResponseHeader('', 'Content-Type'));
end;

procedure TCurlRulesTest.ANameThatDidNotResolveIsSaidToBeOne;
begin
    AssertTrue(CurlExitMeaning(6) = cfResolve);
end;

procedure TCurlRulesTest.ARefusedConnectionIsSaidToBeOne;
begin
    AssertTrue(CurlExitMeaning(7) = cfConnect);
end;

procedure TCurlRulesTest.ATimeoutIsSaidToBeOne;
begin
    AssertTrue(CurlExitMeaning(28) = cfTimeout);
end;

procedure TCurlRulesTest.AFailedHandshakeIsASecureChannelFailure;
begin
    //  THE DEFECT THIS UNIT REPLACED: a refused handshake reported in the words
    //  of a refused connection. The two are different kinds here.
    AssertTrue(CurlExitMeaning(35) = cfSecureChannel);
end;

procedure TCurlRulesTest.ARejectedCertificateIsASecureChannelFailure;
begin
    AssertTrue(CurlExitMeaning(60) = cfSecureChannel);
end;

procedure TCurlRulesTest.SuccessIsNoFailure;
begin
    AssertTrue(CurlExitMeaning(0) = cfNone);
end;

procedure TCurlRulesTest.AnythingElseIsOther;
begin
    AssertTrue(CurlExitMeaning(56) = cfOther);
end;

procedure TCurlClientTest.SetUp;
begin
    FClient := TRecordedCurlClient.Create('curl');
    FStopAfter := High(int64);
    FAsked := 0;
end;

procedure TCurlClientTest.TearDown;
begin
    FreeAndNil(FClient);
end;

function TCurlClientTest.StopPastLimit(ABytesSoFar: int64): boolean;
begin
    Inc(FAsked);
    Result := ABytesSoFar <= FStopAfter;
end;

procedure TCurlClientTest.TheBodyIsWrittenAsItArrives;
var
    Dest: TStringStream;
    R: TCurlResult;
begin
    FClient.AddChunk('abc');
    FClient.AddChunk('def');
    FClient.Headers := RedirectDump;
    Dest := TStringStream.Create('');
    try
        R := FClient.Get('https://example.org/a', Dest, @StopPastLimit);
        AssertEquals('abcdef', Dest.DataString);
        AssertEquals('asked once a chunk', 2, FAsked);
        AssertEquals(0, R.ExitCode);
        AssertFalse(R.Stopped);
    finally
        Dest.Free;
    end;
end;

procedure TCurlClientTest.TheStatusIsTheLastResponses;
var
    Dest: TStringStream;
begin
    FClient.Headers := RedirectDump;
    Dest := TStringStream.Create('');
    try
        AssertEquals(200, FClient.Get('https://example.org/a', Dest, nil).Status);
    finally
        Dest.Free;
    end;
end;

procedure TCurlClientTest.TheHeadersAreTheLastResponsesOnly;
var
    Dest: TStringStream;
    R: TCurlResult;
begin
    FClient.Headers := RedirectDump;
    Dest := TStringStream.Create('');
    try
        R := FClient.Get('https://example.org/a', Dest, nil);
        AssertEquals('attachment; filename="series.csv"',
            LastResponseHeader(R.Headers, 'Content-Disposition'));
        AssertEquals('the redirect is not in it', 0,
            Pos('Location', R.Headers));
    finally
        Dest.Free;
    end;
end;

procedure TCurlClientTest.ACurlFailureIsCarriedWithItsWords;
var
    Dest: TStringStream;
    R: TCurlResult;
begin
    FClient.ExitCode := 35;
    FClient.Stderr := 'curl: (35) SSL connect error' + LineEnding;
    Dest := TStringStream.Create('');
    try
        R := FClient.Get('https://example.org/a', Dest, nil);
        AssertEquals(35, R.ExitCode);
        AssertEquals('trimmed', 'curl: (35) SSL connect error', R.ErrorText);
        AssertEquals('no response, no status', 0, R.Status);
    finally
        Dest.Free;
    end;
end;

procedure TCurlClientTest.StoppingIsReported;
var
    Dest: TStringStream;
    R: TCurlResult;
begin
    FClient.AddChunk('abc');
    FClient.AddChunk('def');
    FClient.AddChunk('ghi');
    FStopAfter := 4;
    Dest := TStringStream.Create('');
    try
        R := FClient.Get('https://example.org/a', Dest, @StopPastLimit);
        AssertTrue('stopped', R.Stopped);
        AssertEquals('nothing asked after the answer was no', 2, FAsked);
    finally
        Dest.Free;
    end;
end;

procedure TCurlClientTest.TheAddressReachesTheProcessUnchanged;
var
    Dest: TStringStream;
begin
    Dest := TStringStream.Create('');
    try
        FClient.Get('https://example.org/a?b=c&d=e', Dest, nil);
        AssertEquals('https://example.org/a?b=c&d=e', FClient.UrlSeen);
    finally
        Dest.Free;
    end;
end;

initialization
    RegisterTest('unit', TCurlRulesTest);
    RegisterTest('unit', TCurlClientTest);
end.
