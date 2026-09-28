// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place this application fetches something from the web.)

ONE METHOD REACHES THE NETWORK, and it is Transfer. Everything else here - the
size cap, the cancel check, what each failure is called in front of a user - is
ordinary code above it, so a test overrides Transfer and exercises all of it
against a canned reply. That is the shape THttpFitService already uses for Fetch
and Send, and the reason is recorded in findings.md: four classes were
untestable while each caller built its own TFPHTTPClient inline.

THE INTERNET IS REACHED THROUGH THE SYSTEM CURL, and only from here
(Common/curl_client.pas says why, and non-negotiable 11 in AGENTS.md makes it a
rule): Free Pascal's own https loaded Apple's legacy OpenSSL 0.9.8 on macOS and
every public service refused its handshake. The built-in fphttpclient stays for
traffic between this program's own processes, which is plain HTTP and where a
process per call would only add latency. TCurlClient answers facts; this
unit decides what they mean to a user.

THE SIZE CAP IS NOT A PERFORMANCE MEASURE. A search that accidentally addresses
a repository's whole archive would otherwise fill the user's disk with no way to
tell what was happening. It refuses with the measured size, so the refusal names
what it saw.

WHY THE CLIENT IS BUILT PER TRANSFER rather than kept alive as http_fit_service
keeps its reader: that one talks to one server for the life of a session, and
this one talks to a different host each time a user picks a different source.

CURL IS NOT PART OF THIS PROGRAM. Every macOS and Windows 10 1803 or later has
it, and the Linux packages depend on it; when it is missing anyway, the user is
told what to install rather than that the network failed.
}
unit web_client;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_web_client, curl_client;

const
    { What this program calls itself when it asks a public service for a file.

      THE FORM MATTERS, AND IT COST A REAL FAILURE. A service's front door
      decides what to answer from this header, and several answer NOTHING AT
      ALL - no status, no body, just a connection that hangs until the timeout
      - to anything that does not look like a known client. 'Fit data source
      client' was one of those: FRED served the very same address 29 KB to
      curl and stalled on us, which reached the user as an empty file and read
      as "that series does not exist".

      What is expected is the conventional robot form - a PRODUCT, a VERSION
      and a way to find out who is asking:

          Fit/1.0 (+https://dvmorozov.github.io/fit)

      Measured, not guessed: with the contact URL the same request answers 200
      and the data; without it, and with browser-shaped agents too, the
      connection is left open until it times out. Keep the shape if this is
      ever changed. }
    DefaultUserAgent = 'Fit/1.0 (+https://dvmorozov.github.io/fit)';
    { How long a single request may take. Public catalogues are occasionally
      slow, and a wizard step that fails after five seconds reads as a broken
      source rather than a busy one. }
    DefaultTimeoutMs = 30000;
    { The largest answer that will be accepted. See the header. }
    DefaultMaxBytes = 64 * 1024 * 1024;
    { How many redirects are followed. Repositories redirect DOI to record to
      storage, which is three; a longer chain is a loop or a login wall. }
    MaxRedirects = 5;

type
    { Raised when the user stopped the download. A kind of its own because it
      is not a failure: nothing is wrong, nobody needs advice, and the window
      says so in one line rather than putting a problem in front of somebody
      who has just solved it. }
    EWebCancelled = class(EWebError);

    { EVERY DECISION A TRANSFER MAKES WHILE IT RUNS, away from the socket.

      WHAT THIS EXISTS TO END. The cap, the cancel and the progress report used
      to be inside the one method that opens a connection, so none of them
      could be tested - and two were simply wrong: Cancel set a flag nothing
      read while bytes were arriving, and the cap was applied AFTER the whole
      answer was in memory, which is not a cap at all. Here they are arithmetic
      over counts, and the socket method does nothing but ask. }
    TTransferGuard = class(TObject)
    private
        FMaxBytes: int64;
        FProgress: TWebProgress;
        FCancelled: boolean;
        FTooLarge: boolean;
        FLargest: int64;
    public
        constructor Create(AMaxBytes: int64; AProgress: TWebProgress);
        { Stops the transfer at the next chunk. Safe from another thread: it
          sets a flag and nothing else. }
        procedure Cancel;
        { Called as bytes arrive. False means stop now - either because the
          user asked or because the answer has passed the cap. }
        function Accept(ABytes, ATotal: int64): boolean;
        { Why what came back cannot be used, or '' when it can. ASTATUS is the
          HTTP status the server answered with, and ARETRYAFTER its Retry-After
          header, quoted when the status asks the user to wait. }
        function Refusal(const AUrl: string; ABytes: int64;
            AStatus: longint; const ARetryAfter: string = ''): string;
        property Cancelled: boolean read FCancelled;
        property TooLarge: boolean read FTooLarge;
    end;

    TWebClient = class(TObject, IWebClient)
    private
        FProgress: TWebProgress;
        FCancelled: boolean;
        { The guard of the transfer running now; nil between transfers. }
        FGuard: TTransferGuard;
        FUserAgent: string;
        FTimeoutMs: longint;
        FMaxBytes: int64;
    protected
        { What the transport calls as bytes arrive: it asks the guard, and
          answers False - stop - when the guard says to.

          PROTECTED RATHER THAN PRIVATE so that a test can drive the wiring -
          that the bytes reach the guard, and that the guard's answer reaches
          the transport - without a process. The wiring is the part that was
          wrong before: a cancel that nothing asked about. }
        function DataReceived(ABytesSoFar: int64): boolean;
        { THE ONLY THING HERE THAT REACHES THE NETWORK. Writes the body at AUrl into
          ADest and answers the file name the server suggested, or ''. Raises
          EWebError, already worded for a user, when the answer never came. }
        function Transfer(const AUrl: string; ADest: TStream): string; virtual;
        { The curl Transfer fetches with, or nil when this computer has none.
          Transfer frees it.

          A SEAM, and the reason it exists: everything Transfer decides - a
          missing curl, a stop, each exit code, each status, the suggested name
          - is then driven by a unit test through a TCurlClient whose process is
          a recorded answer, while Transfer itself runs as the application runs
          it. Only the process is replaced, and testcase_web_transport runs the
          real one. }
        function CreateCurl: TCurlClient; virtual;
        { Reports progress and asks whether to carry on. Called from Transfer as
          bytes arrive; separate so a double can drive cancellation. }
        function Continues(ABytes, ATotal: int64): boolean;
    public
        constructor Create;
        destructor Destroy; override;

        function GetText(const AUrl: string): string;
        function Download(const AUrl: string; ADest: TStream): string;
        procedure SetProgress(AProgress: TWebProgress);
        procedure Cancel;
        { For the fixture that owns this object: CORBA interfaces have no
          reference counting and no cast back, so whoever created it frees it. }
        function AsObject: TObject;

        { The one of the three that anything sets: a test lowers it to see the
          cap refuse. The user agent and the timeout are decided here, by the
          constants above, because no caller has anything better to say. }
        property MaxBytes: int64 read FMaxBytes write FMaxBytes;
    end;

{ The name a server suggested, read from a Content-Disposition header, or ''
  when the header names none. Public because it is a rule about a text, testable
  on its own, and because the URL source needs the same answer. }
function FileNameFromDisposition(const AHeader: string): string;

{ What a user is told when a URL could not be fetched. One function, so the
  wording is the same wherever the failure is met. }
function WebFailureMessage(const AUrl, ADetail: string): string;

{ The same, for a failure curl sorted into a kind. A kind has its own advice:
  a secure-channel failure in particular is NOT the network, and saying
  "check your connection" for it is the defect this replaced. }
function TransportFailureMessage(const AUrl: string; AKind: TCurlFailureKind;
    const ADetail: string): string;

{ What a user is told when this computer has no curl to fetch with. }
function MissingCurlMessage(const AUrl: string): string;

{ What an HTTP status means in words a user can act on, or '' when the answer
  is usable.

  WHY IT IS NOT THE LIBRARY'S SENTENCE. "Unexpected response status code: 404"
  names a number and a fact about a protocol; "that address holds nothing" says
  what happened and implies what to do. The cases here are the ones these
  sources actually meet: gone, refused, asked too often, and a service having
  its own trouble. }
function HttpStatusMessage(const AUrl: string; AStatus: longint;
    const ARetryAfter: string): string;

implementation

uses
    StrUtils;

function FileNameFromDisposition(const AHeader: string): string;
var
    P: longint;
    Rest: string;
begin
    Result := '';
    P := Pos('filename=', LowerCase(AHeader));
    if P = 0 then
        Exit;
    Rest := Trim(Copy(AHeader, P + Length('filename='), MaxInt));
    //  RFC 6266 allows the name to be quoted, and most servers quote it.
    if (Rest <> '') and (Rest[1] = '"') then
    begin
        Delete(Rest, 1, 1);
        P := Pos('"', Rest);
        if P > 0 then
            Rest := Copy(Rest, 1, P - 1);
    end
    else
    begin
        P := Pos(';', Rest);
        if P > 0 then
            Rest := Copy(Rest, 1, P - 1);
    end;
    Result := Trim(Rest);
end;

{ ADetail without the full stop it may already end with, so a sentence built
  around it does not end in two. }
function Clause(const ADetail: string): string;
begin
    Result := Trim(ADetail);
    while (Result <> '') and (Result[Length(Result)] = '.') do
        SetLength(Result, Length(Result) - 1);
end;

function WebFailureMessage(const AUrl, ADetail: string): string;
begin
    Result := 'Cannot fetch ' + AUrl + ': ' + Clause(ADetail) +
        '. Check the address and this computer''s connection; fitting ' +
        'itself never needs the network.';
end;

function TransportFailureMessage(const AUrl: string; AKind: TCurlFailureKind;
    const ADetail: string): string;
begin
    case AKind of
        cfResolve:
            Result := WebFailureMessage(AUrl,
                'the host name was not found (' + Clause(ADetail) + ')');
        cfConnect:
            Result := WebFailureMessage(AUrl,
                'nothing answered at that address (' + Clause(ADetail) + ')');
        cfTimeout:
            Result := 'Cannot fetch ' + AUrl + ': the service stopped ' +
                'answering (' + Clause(ADetail) + '). It may be busy; trying ' +
                'again later usually works. Fitting itself never needs the ' +
                'network.';
        cfSecureChannel:
            //  THE NETWORK ANSWERED. What failed is agreeing on a secure
            //  connection, or trusting the certificate - which is the service,
            //  a proxy that intercepts https, or this computer's clock, and
            //  telling the user to check their connection sends them the wrong
            //  way. That is exactly what the old transport's message did.
            Result := 'Cannot fetch ' + AUrl + ': a secure connection could ' +
                'not be established (' + Clause(ADetail) + '). The service ' +
                'was reached; its certificate, a proxy that inspects https, or ' +
                'this computer''s date and time may be at fault. Fitting ' +
                'itself never needs the network.';
        else
            Result := WebFailureMessage(AUrl, ADetail);
    end;
end;

function MissingCurlMessage(const AUrl: string): string;
begin
    //  Naming the program and where it comes from is what makes this
    //  actionable; "the download failed" would send the user to their network.
    Result := 'Cannot fetch ' + AUrl + ': this computer has no curl program, ' +
        'which this application downloads with. Every macOS and Windows 10 ' +
        '(version 1803) or later includes it; on Linux, install the curl ' +
        'package. A file you already have can still be opened - fitting ' +
        'never needs the network.';
end;

function HttpStatusMessage(const AUrl: string; AStatus: longint;
    const ARetryAfter: string): string;
begin
    Result := '';
    if (AStatus >= 200) and (AStatus <= 299) then
        Exit;
    case AStatus of
        401, 403:
            //  A service that wants an account, or one that has decided this
            //  program may not have the file. Either way the user cannot fix
            //  it here, and the browser is where they can look.
            Result := 'The service refused to hand over ' + AUrl +
                ' without an account or a key (' + IntToStr(AStatus) + '). ' +
                'Opening the address in a browser will say what it wants.';
        404, 410:
            Result := 'There is nothing at ' + AUrl + ' (' +
                IntToStr(AStatus) + '). The record may have been withdrawn, ' +
                'or the address may be out of date.';
        429:
        begin
            //  THE ONE A USER CAN ACT ON BY WAITING, so it says so - and says
            //  how long when the service was specific about it.
            Result := 'The service is asking for fewer requests (429).';
            if Trim(ARetryAfter) <> '' then
                Result := Result + ' It suggests waiting ' + Trim(ARetryAfter) +
                    ' seconds.'
            else
                Result := Result + ' Wait a little and try again.';
        end;
        500..599:
            Result := 'The service had trouble answering (' +
                IntToStr(AStatus) + '). That is at their end; trying again ' +
                'later usually works.';
        else
            Result := 'The address ' + AUrl + ' answered with status ' +
                IntToStr(AStatus) + ', which this program cannot use.';
    end;
end;

constructor TTransferGuard.Create(AMaxBytes: int64; AProgress: TWebProgress);
begin
    inherited Create;
    FMaxBytes := AMaxBytes;
    FProgress := AProgress;
end;

procedure TTransferGuard.Cancel;
begin
    FCancelled := True;
end;

function TTransferGuard.Accept(ABytes, ATotal: int64): boolean;
begin
    //  ASKED DURING A STALL TOO, so that a cancel is heard while nothing
    //  arrives - but asking is not progress, so the bar is told only when the
    //  count has moved.
    if ABytes > FLargest then
    begin
        FLargest := ABytes;
        if Assigned(FProgress) then
            FProgress(ABytes, ATotal);
    end;
    //  STOPPED WHILE IT RUNS, which is the whole point: a cap checked after
    //  the answer is in memory has already cost the memory, and a cancel
    //  nothing reads is a button that does nothing.
    if FCancelled then
        Exit(False);
    if (FMaxBytes > 0) and (ABytes > FMaxBytes) then
    begin
        FTooLarge := True;
        Exit(False);
    end;
    Result := True;
end;

function TTransferGuard.Refusal(const AUrl: string; ABytes: int64;
    AStatus: longint; const ARetryAfter: string): string;
begin
    if FCancelled then
        Exit('The download was stopped.');
    if FTooLarge or ((FMaxBytes > 0) and (ABytes > FMaxBytes)) then
        Exit(Format('The answer from %s is larger than the %d MB this ' +
            'program will download in one go. Choose a smaller file.',
            [AUrl, FMaxBytes div (1024 * 1024)]));
    Result := HttpStatusMessage(AUrl, AStatus, ARetryAfter);
    if Result <> '' then
        Exit;
    if ABytes = 0 then
        //  A SERVER THAT ANSWERS WITH NOTHING. Saved, it becomes a file that
        //  reads as no data points, and the reader gets the blame for it.
        Result := 'The service answered ' + AUrl + ' with an empty file. ' +
            'There may be nothing published under that name.';
end;

destructor TWebClient.Destroy;
begin
    FGuard.Free;
    inherited Destroy;
end;

function TWebClient.DataReceived(ABytesSoFar: int64): boolean;
begin
    if FGuard = nil then
        Exit(True);
    //  THE TOTAL IS UNKNOWN (0) to the transport, which streams the body as it
    //  comes; the guard and the progress bar both treat 0 as "not said".
    Result := FGuard.Accept(ABytesSoFar, 0);
end;

constructor TWebClient.Create;
begin
    inherited Create;
    FUserAgent := DefaultUserAgent;
    FTimeoutMs := DefaultTimeoutMs;
    FMaxBytes := DefaultMaxBytes;
end;

function TWebClient.AsObject: TObject;
begin
    Result := Self;
end;

procedure TWebClient.SetProgress(AProgress: TWebProgress);
begin
    FProgress := AProgress;
end;

procedure TWebClient.Cancel;
begin
    //  A flag and nothing else, so this is safe from the window's thread while
    //  the transfer runs in another - and it reaches the transfer RUNNING NOW
    //  through its guard, which is what the button was doing nothing without.
    FCancelled := True;
    if FGuard <> nil then
        FGuard.Cancel;
end;

function TWebClient.Continues(ABytes, ATotal: int64): boolean;
begin
    if Assigned(FProgress) then
        FProgress(ABytes, ATotal);
    Result := not FCancelled;
end;

function TWebClient.GetText(const AUrl: string): string;
var
    Dest: TStringStream;
begin
    Dest := TStringStream.Create('');
    try
        Transfer(AUrl, Dest);
        Result := Dest.DataString;
    finally
        Dest.Free;
    end;
end;

function TWebClient.Download(const AUrl: string; ADest: TStream): string;
begin
    Result := Transfer(AUrl, ADest);
end;

function TWebClient.CreateCurl: TCurlClient;
var
    Executable: string;
begin
    Executable := TCurlClient.Locate;
    if Executable = '' then
        Exit(nil);
    Result := TCurlClient.Create(Executable);
end;

function TWebClient.Transfer(const AUrl: string; ADest: TStream): string;
var
    Refusal: string;
    Curl: TCurlClient;
    Answer: TCurlResult;
begin
    Result := '';
    FCancelled := False;
    FreeAndNil(FGuard);
    FGuard := TTransferGuard.Create(FMaxBytes, FProgress);
    Curl := CreateCurl;
    if Curl = nil then
        raise EWebError.Create(MissingCurlMessage(AUrl));
    try
        Curl.UserAgent := FUserAgent;
        Curl.TimeoutMs := FTimeoutMs;
        Curl.MaxRedirects := MaxRedirects;
        try
            //  WHERE THE GUARD IS ASKED: as the bytes arrive, which is the only
            //  place a cap or a cancel can act while there is still time.
            Answer := Curl.Get(AUrl, ADest, @DataReceived);
        except
            //  Only starting the process can raise here - a curl that was found
            //  and then could not be run.
            on E: Exception do
                raise EWebError.Create(WebFailureMessage(AUrl, E.Message));
        end;
    finally
        Curl.Free;
    end;

    //  A transfer stopped on purpose ends as a broken one: say what actually
    //  happened instead.
    if Answer.Stopped or FGuard.Cancelled then
    begin
        if FGuard.TooLarge then
            raise EWebError.Create(FGuard.Refusal(AUrl, ADest.Size, 0));
        raise EWebCancelled.Create('The download was stopped.');
    end;
    if CurlExitMeaning(Answer.ExitCode) <> cfNone then
        raise EWebError.Create(TransportFailureMessage(AUrl,
            CurlExitMeaning(Answer.ExitCode), Answer.ErrorText));

    //  What each status MEANS is decided in words by the guard, rather than
    //  reaching a user as a number.
    Refusal := FGuard.Refusal(AUrl, ADest.Size, Answer.Status,
        LastResponseHeader(Answer.Headers, 'Retry-After'));
    if Refusal <> '' then
        raise EWebError.Create(Refusal);

    Result := FileNameFromDisposition(
        LastResponseHeader(Answer.Headers, 'Content-Disposition'));
end;

end.
