// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The web client's real transport, over real sockets.)

WHY THESE ARE INTEGRATION TESTS. Every one of them opens a socket and starts a
curl process, so none is a unit test, localhost included; the decisions around
the transport are covered as units in testcase_curl_client and
testcase_download_failures.

THE FAILURE THAT PROMPTED THEM. On macOS every data source failed with "Connect
to fred.stlouisfed.org:443 failed" while the network was fine: Free Pascal 3.2.2
cannot load OpenSSL 3, so it silently loaded Apple's legacy OpenSSL 0.9.8, whose
TLS 1.0 handshake the service refused - and the socket layer reports a refused
handshake with the text of a refused connection. Every test of the web client
drove a double in place of Transfer, so all of them stayed green. The first case
below is the one that went red: a real TWebClient, a real public host that
insists on TLS 1.2.

NO SERVER IS INVENTED FOR THIS. The local cases fetch from the real fit_server,
started by the fixture every other process test uses: it answers a large
document, a 404 and a closed port, which is everything a transport can be asked
about without a public host. The one thing it cannot do is keep sending forever,
which is what shows whether a stop really ends the curl process rather than the
process ending first on its own - so TCurlProcessTest hands TCurlClient a
two-line shell script in curl's place.
}
unit testcase_web_transport;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fphttpclient, resolve, process,
    int_web_client, web_client, curl_client, worker_process_harness
    {$IFDEF UNIX}, BaseUnix{$ENDIF};

type
    { A real public host, over https. Ignored - counted and reported - when the
      host does not resolve, so an offline machine is told why rather than
      failed for something that is not a defect. }
    TPublicHttpsTest = class(TTestCase)
    published
        procedure HttpsReachesAServerThatRequiresTls12;
    end;

    { The real client against the real fit_server, on this machine. }
    TWebTransportTest = class(TWorkerProcessTest)
    private
        FClient: TWebClient;
        FSeenBytes: int64;
        procedure CancelOnFirstChunk(ABytes, ATotal: int64);
        procedure NoteBytes(ABytes, ATotal: int64);
        function UrlOf(const APath: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheBodyArrivesExactlyAsTheServerSentIt;
        procedure ProgressIsReportedAsTheBytesArrive;
        procedure AnAddressWithNothingThereIsSaidInWords;
        procedure TheCapStopsALargeAnswer;
        procedure ACancelStopsTheTransferAndSaysSo;
        procedure APortNobodyListensOnIsAConnectionFailure;
    end;

implementation

const
    { OUR OWN SITE, not a third party's: GitHub Pages refuses anything older than
      TLS 1.2, which is exactly what the legacy library could not do, and the
      address is one this project already depends on being there. }
    PublicHost = 'dvmorozov.github.io';
    PublicUrl = 'https://' + PublicHost + '/fit/';

{ How many processes this test binary has as children whose command line holds
  APATTERN, right now. A stop that left one running would be a download nobody
  can see, still writing into a pipe nobody reads. Matched on the command line,
  not the name: the fixture's own fit_server is a child too, and a script stands
  in for curl below. }
function ChildProcessCount(const APattern: string): longint;
var
    Output: string;
    Line: string;
begin
    Result := 0;
    Output := '';
    {$IFDEF WINDOWS}
    RunCommand('powershell', ['-NoProfile', '-Command',
        Format('(Get-CimInstance Win32_Process -Filter ''ParentProcessId=%d'' | ' +
        'Where-Object { $_.CommandLine -like ''*%s*'' } | Measure-Object).Count',
        [GetProcessID, APattern])], Output, [poNoConsole]);
    Result := StrToIntDef(Trim(Output), 0);
    {$ELSE}
    //  pgrep answers 1, and prints nothing, when nothing matches.
    RunCommand('pgrep', ['-P', IntToStr(GetProcessID), '-f', APattern], Output,
        [poNoConsole]);
    for Line in Output.Split([#10]) do
        if Trim(Line) <> '' then
            Inc(Result);
    {$ENDIF}
end;

function HostResolves(const AHost: string): boolean;
var
    Resolver: THostResolver;
begin
    Resolver := THostResolver.Create(nil);
    try
        Result := Resolver.NameLookup(AHost);
    finally
        Resolver.Free;
    end;
end;

procedure TPublicHttpsTest.HttpsReachesAServerThatRequiresTls12;
var
    Client: TWebClient;
    Body: string;
begin
    if not HostResolves(PublicHost) then
        Ignore(PublicHost + ' does not resolve - this machine is offline');
    Client := TWebClient.Create;
    try
        Body := Client.GetText(PublicUrl);
    finally
        Client.Free;
    end;
    AssertTrue('a page came back from ' + PublicUrl,
        Pos('<html', LowerCase(Body)) > 0);
end;

procedure TWebTransportTest.SetUp;
begin
    inherited SetUp;
    FClient := TWebClient.Create;
    FSeenBytes := 0;
end;

procedure TWebTransportTest.TearDown;
begin
    FreeAndNil(FClient);
    inherited TearDown;
end;

function TWebTransportTest.UrlOf(const APath: string): string;
begin
    Result := Format('http://127.0.0.1:%d%s', [WorkerTestPort, APath]);
end;

procedure TWebTransportTest.CancelOnFirstChunk(ABytes, ATotal: int64);
begin
    FClient.Cancel;
end;

procedure TWebTransportTest.NoteBytes(ABytes, ATotal: int64);
begin
    FSeenBytes := ABytes;
end;

procedure TWebTransportTest.TheBodyArrivesExactlyAsTheServerSentIt;
var
    Reference: string;
    Received: TStringStream;
begin
    //  THE REFERENCE IS THE BUILT-IN CLIENT, which is what every loopback call
    //  in this program uses: the two transports must agree byte for byte on the
    //  same answer. The document is over a hundred kilobytes, so it arrives in
    //  many chunks and a chunk dropped or doubled would show.
    Reference := TFPHTTPClient.SimpleGet(UrlOf('/openapi.json'));
    Received := TStringStream.Create('');
    try
        FClient.Download(UrlOf('/openapi.json'), Received);
        AssertEquals('the same number of bytes', Length(Reference),
            Length(Received.DataString));
        AssertTrue('the same bytes', Reference = Received.DataString);
    finally
        Received.Free;
    end;
end;

procedure TWebTransportTest.ProgressIsReportedAsTheBytesArrive;
var
    Body: string;
begin
    FClient.SetProgress(@NoteBytes);
    Body := FClient.GetText(UrlOf('/openapi.json'));
    AssertEquals('the last report is the whole answer', Length(Body),
        FSeenBytes);
end;

procedure TWebTransportTest.AnAddressWithNothingThereIsSaidInWords;
begin
    try
        FClient.GetText(UrlOf('/no-such-route'));
        Fail('a 404 was accepted as data');
    except
        on E: EWebError do
            AssertEquals(HttpStatusMessage(UrlOf('/no-such-route'), 404, ''),
                E.Message);
    end;
end;

procedure TWebTransportTest.TheCapStopsALargeAnswer;
begin
    FClient.MaxBytes := 1024;
    try
        FClient.GetText(UrlOf('/openapi.json'));
        Fail('an answer over the cap was accepted');
    except
        on E: EWebCancelled do
            Fail('the cap was reported as a cancel: ' + E.Message);
        on E: EWebError do
            AssertTrue('names the cap: ' + E.Message,
                Pos('larger than', E.Message) > 0);
    end;
end;

procedure TWebTransportTest.ACancelStopsTheTransferAndSaysSo;
begin
    FClient.SetProgress(@CancelOnFirstChunk);
    try
        FClient.GetText(UrlOf('/openapi.json'));
        Fail('a cancelled transfer completed');
    except
        on E: EWebCancelled do
            AssertEquals('The download was stopped.', E.Message);
    end;
    AssertEquals('no curl is left running', 0, ChildProcessCount('curl'));
end;

procedure TWebTransportTest.APortNobodyListensOnIsAConnectionFailure;
const
    //  Port 1 is reserved (tcpmux) and nothing listens on it on any machine
    //  this suite runs on, so the connection is refused at once.
    ClosedUrl = 'http://127.0.0.1:1/';
begin
    try
        FClient.GetText(ClosedUrl);
        Fail('a closed port answered');
    except
        on E: EWebCancelled do
            Fail('reported as a cancel: ' + E.Message);
        on E: EWebError do
        begin
            AssertTrue('names the address: ' + E.Message,
                Pos(ClosedUrl, E.Message) > 0);
            AssertTrue('is not blamed on a secure connection: ' + E.Message,
                Pos('secure', LowerCase(E.Message)) = 0);
        end;
    end;
end;

{ A stand-in for curl that never stops sending: the one kind of transfer that
  shows whether a stop really ends the process, rather than the process ending
  first on its own. A shell script, so this runs where there is a shell. }
type
    TCurlProcessTest = class(TTestCase)
    private
        FDir: string;
        function StopAfterThreeChunks(ABytesSoFar: int64): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure StoppingEndsTheProcessBeforeGetReturns;
    end;

const
    FakeCurlDir = 'fit-endless-curl';

procedure TCurlProcessTest.SetUp;
begin
    FDir := IncludeTrailingPathDelimiter(GetTempDir(False)) + FakeCurlDir;
    ForceDirectories(FDir);
end;

procedure TCurlProcessTest.TearDown;
begin
    DeleteFile(IncludeTrailingPathDelimiter(FDir) + 'curl');
    RemoveDir(FDir);
end;

function TCurlProcessTest.StopAfterThreeChunks(ABytesSoFar: int64): boolean;
begin
    Result := ABytesSoFar < 3 * Length('chunk' + LineEnding);
end;

procedure TCurlProcessTest.StoppingEndsTheProcessBeforeGetReturns;
{$IFDEF UNIX}
var
    Script: string;
    Lines: TStringList;
    Curl: TCurlClient;
    Dest: TStringStream;
    Answer: TCurlResult;
{$ENDIF}
begin
    {$IFDEF UNIX}
    Script := IncludeTrailingPathDelimiter(FDir) + 'curl';
    Lines := TStringList.Create;
    try
        Lines.Add('#!/bin/sh');
        Lines.Add('while :; do echo chunk; sleep 0.05; done');
        Lines.SaveToFile(Script);
    finally
        Lines.Free;
    end;
    fpChmod(Script, &755);
    Curl := TCurlClient.Create(Script);
    Dest := TStringStream.Create('');
    try
        Answer := Curl.Get('http://example.invalid/', Dest, @StopAfterThreeChunks);
        AssertTrue('the caller''s stop was heard', Answer.Stopped);
        AssertEquals('and the process is gone', 0,
            ChildProcessCount(FakeCurlDir));
    finally
        Dest.Free;
        Curl.Free;
    end;
    {$ELSE}
    Ignore('needs a shell to stand in for curl; the Windows build runs the ' +
        'cancel through real curl in TWebTransportTest');
    {$ENDIF}
end;

initialization
    RegisterTest('integration', TCurlProcessTest);
    RegisterTest('integration', TPublicHttpsTest);
    RegisterTest('integration', TWebTransportTest);
end.
