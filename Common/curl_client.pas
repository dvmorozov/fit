// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Fetching an address through the system's curl program.)

THE RULE THIS UNIT EXISTS FOR: anything this application fetches from the
internet goes through the system curl, and traffic between its own processes
(client, fit_server, the Python sidecar) stays on the built-in fphttpclient,
plain HTTP. See "The network boundary" in docs/contributing/architecture.md; it
is non-negotiable 11 in AGENTS.md.

WHY NOT THE BUILT-IN CLIENT FOR THE INTERNET. Free Pascal's https goes through
OpenSSL, loaded BY NAME at the first request, and 3.2.2 knows no OpenSSL 3 name
on any platform. On macOS it silently loaded Apple's legacy 0.9.8 instead, whose
TLS 1.0 handshake every public service refuses - and the socket layer reports a
refused handshake in the words of a refused connection, so the user was told to
check a network that was fine. curl ships with macOS (SecureTransport) and with
Windows 10 1803 and later (Schannel), and is one package away on Linux; it uses
the system's own TLS and certificate store, and it verifies certificates, which
fphttpclient did not by default.

REJECTED: bundling OpenSSL 3 and pointing Free Pascal at it. That needs a copy
per architecture inside the app bundle, rewritten install names, re-signing, and
Free Pascal 3.2.2 against OpenSSL 3 was never shown to work. The cost of this
route is one child process per download, which a download does not notice and a
REST call between the processes would - hence the two transports.

PROXIES ARE THE ENVIRONMENT'S. curl honours http_proxy, https_proxy and
no_proxy, which the built-in client never did - so a machine that reaches the
internet through a proxy it names that way now downloads through it. A user's
~/.curlrc is NOT read (--disable), because nothing it says would be visible from
here. Windows' own proxy settings are not read by curl, as they were not read
before.

THIS UNIT KNOWS CURL, NOT POLICY. It answers facts - the status, the headers,
curl's exit code and words, whether the caller stopped it - and leaves the size
cap, the cancel and every sentence a user reads to its caller
(Desktop/DataSources/web_client.pas). So any other part of the program that
needs to fetch from the internet reuses this class and words its own failures.

ONE METHOD STARTS A PROCESS, and it is Run: protected and virtual, so a test
replaces it with a recorded answer and drives Get exactly as the application
does. Everything else here is a rule over text.
}
unit curl_client;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { Called as the answer arrives, with the number of bytes so far, and also
      every tenth of a second while nothing arrives - so a caller's cancel is
      heard during a stall, not only between chunks. False stops the transfer. }
    TCurlChunkEvent = function(ABytesSoFar: int64): boolean of object;

    { What curl's exit code says went wrong, in the kinds a user is told about
      differently. An enumeration rather than a sentence, so each caller words it
      for its own reader. }
    TCurlFailureKind = (
        cfNone,          //  the transfer completed; the status says the rest
        cfResolve,       //  the host name was not found
        cfConnect,       //  nothing answered at that address
        cfTimeout,       //  the connection or the transfer stalled
        cfSecureChannel, //  the TLS handshake or the certificate failed
        cfOther);

    { Which kind of machine curl is looked for on. }
    TCurlHost = (chUnix, chMac, chWindows);

    TCurlResult = record
        { curl's own exit code; 0 when the transfer completed. }
        ExitCode: longint;
        { The status of the LAST response, after redirects; 0 when none came. }
        Status: longint;
        { The last response's header block, raw. }
        Headers: string;
        { What curl said on stderr, trimmed. }
        ErrorText: string;
        { The chunk callback answered False. }
        Stopped: boolean;
    end;

    TCurlClient = class(TObject)
    private
        FExecutable: string;
        FUserAgent: string;
        FTimeoutMs: longint;
        FMaxRedirects: longint;
    protected
        { THE ONLY THING HERE THAT STARTS A PROCESS. Runs curl on AUrl, writes
          the body into ADest as it arrives, asks AOnChunk as it goes and stops
          the process when the answer is False. AHEADERS is curl's whole header
          dump - every response of a redirect chain. Answers curl's exit code.

          Virtual so that a test replays a recorded answer and every line of Get
          still runs; the same seam shape as THttpFitService's Fetch and Send. }
        function Run(const AUrl: string; ADest: TStream;
            AOnChunk: TCurlChunkEvent; out AHeaders, AStderr: string;
            out AStopped: boolean): longint; virtual;
    public
        { AEXECUTABLE is the curl to run; Locate says which one this machine has. }
        constructor Create(const AExecutable: string);
        { The curl this machine has, or '' when it has none. }
        class function Locate: string;
        { Fetches AUrl into ADest. Raises nothing for anything the transfer
          met - what went wrong is in the result. It raises only when the
          executable it was given cannot be started at all (EProcess), which
          is a broken installation rather than a failed download. }
        function Get(const AUrl: string; ADest: TStream;
            AOnChunk: TCurlChunkEvent): TCurlResult;

        property UserAgent: string read FUserAgent write FUserAgent;
        { Both the connect limit and the stall limit: a transfer that stops
          moving for this long is ended, one that is merely long is not. }
        property TimeoutMs: longint read FTimeoutMs write FTimeoutMs;
        property MaxRedirects: longint read FMaxRedirects write FMaxRedirects;
    end;

const
    { The machine this binary runs on, for CurlCandidates. }
    ThisCurlHost = {$IFDEF DARWIN} chMac {$ELSE} {$IFDEF WINDOWS} chWindows
        {$ELSE} chUnix {$ENDIF} {$ENDIF};

{ The arguments curl is started with, one per element - there is no shell, so
  nothing in the address is interpreted. }
function CurlArguments(const AUrl, AUserAgent: string;
    ATimeoutMs, AMaxRedirects: longint;
    const AHeaderFile: string): TStringArray;

{ Where curl is looked for, in order: the copy the system ships, then each entry
  of APATH. }
function CurlCandidates(AHost: TCurlHost;
    const ASystemRoot, APath: string): TStringArray;

{ The last response's header block in a curl header dump, which holds one block
  per response of a redirect chain. }
function LastResponseBlock(const AHeaderDump: string): string;
{ The status of the last response in a header dump, or 0 when there is none. }
function LastResponseStatus(const AHeaderDump: string): longint;
{ A header of the last response, whatever the case of its name, or ''. }
function LastResponseHeader(const AHeaderDump, AName: string): string;

{ What a curl exit code means. }
function CurlExitMeaning(AExitCode: longint): TCurlFailureKind;

implementation

uses
    Math, process;

const
    { The only schemes a fetch or a redirect may use. A redirect to file://
      would read the user's own disk. }
    WebProtocols = '=http,https';
    { How often a stalled transfer asks its caller whether to go on. }
    IdleAskMs = 100;

function Seconds(AMs: longint): string;
begin
    Result := IntToStr(Max(1, (AMs + 999) div 1000));
end;

function CurlArguments(const AUrl, AUserAgent: string;
    ATimeoutMs, AMaxRedirects: longint;
    const AHeaderFile: string): TStringArray;
begin
    Result := [
        //  FIRST OR NOT AT ALL: curl honours --disable only as its first
        //  argument, and without it a user's ~/.curlrc can add a proxy,
        //  --insecure or a different output, none of it visible from here.
        '--disable',
        '--silent', '--show-error',
        '--location', '--max-redirs', IntToStr(AMaxRedirects),
        '--proto', WebProtocols, '--proto-redir', WebProtocols,
        '--connect-timeout', Seconds(ATimeoutMs),
        //  THE STALL LIMIT, which is what the built-in client's IOTimeout was:
        //  below one byte a second for this long ends the transfer. A cap on
        //  the WHOLE transfer would refuse a large file on a slow line.
        '--speed-limit', '1', '--speed-time', Seconds(ATimeoutMs),
        '--user-agent', AUserAgent,
        '--dump-header', AHeaderFile,
        //  --url rather than a bare argument, and last: an address beginning
        //  with '-' is still an address.
        '--url', AUrl];
end;

function CurlCandidates(AHost: TCurlHost;
    const ASystemRoot, APath: string): TStringArray;
var
    Separator, Delimiter: char;
    ExeName, Dir: string;
    Dirs: TStringArray;
begin
    Result := [];
    if AHost = chWindows then
    begin
        Separator := ';';
        Delimiter := '\';
        ExeName := 'curl.exe';
        //  THE COPY WINDOWS SHIPS, before anything on the PATH: another
        //  program's curl may be old, or built without the system's TLS.
        if ASystemRoot <> '' then
            Result := [ExcludeTrailingBackslash(ASystemRoot) +
                '\System32\curl.exe'];
    end
    else
    begin
        Separator := ':';
        Delimiter := '/';
        ExeName := 'curl';
        //  Every macOS ships this one, built on the system's TLS; a GUI app's
        //  PATH is short and may not say so.
        if AHost = chMac then
            Result := ['/usr/bin/curl'];
    end;
    Dirs := APath.Split([Separator]);
    for Dir in Dirs do
    begin
        //  An empty entry means the current folder to a shell. Here it would
        //  mean running whatever 'curl' sits beside the user's data.
        if Dir = '' then
            Continue;
        if Dir[Length(Dir)] = Delimiter then
            Result := Concat(Result, [Dir + ExeName])
        else
            Result := Concat(Result, [Dir + Delimiter + ExeName]);
    end;
end;

function DumpLines(const AHeaderDump: string): TStringArray;
begin
    Result := StringReplace(AHeaderDump, #13, '', [rfReplaceAll]).Split([#10]);
end;

function LastResponseBlock(const AHeaderDump: string): string;
var
    Lines: TStringArray;
    i, Start: integer;
begin
    Result := '';
    Lines := DumpLines(AHeaderDump);
    Start := -1;
    for i := High(Lines) downto 0 do
        if Copy(Lines[i], 1, 5) = 'HTTP/' then
        begin
            Start := i;
            Break;
        end;
    if Start < 0 then
        Exit;
    for i := Start to High(Lines) do
    begin
        if Lines[i] = '' then
            Break;
        Result := Result + Lines[i] + #13#10;
    end;
end;

function LastResponseStatus(const AHeaderDump: string): longint;
var
    Lines, Parts: TStringArray;
begin
    Result := 0;
    Lines := DumpLines(LastResponseBlock(AHeaderDump));
    if Length(Lines) = 0 then
        Exit;
    //  'HTTP/1.1 302 Found' and 'HTTP/2 200' alike: the second word.
    Parts := Lines[0].Split([' ']);
    if Length(Parts) >= 2 then
        Result := StrToIntDef(Parts[1], 0);
end;

function LastResponseHeader(const AHeaderDump, AName: string): string;
var
    Lines: TStringArray;
    i, Colon: integer;
begin
    Result := '';
    Lines := DumpLines(LastResponseBlock(AHeaderDump));
    //  Line 0 is the status line.
    for i := 1 to High(Lines) do
    begin
        Colon := Pos(':', Lines[i]);
        //  HTTP/2 sends every name in lower case, HTTP/1.1 servers mostly do not.
        if (Colon > 0) and SameText(Trim(Copy(Lines[i], 1, Colon - 1)), AName) then
            Exit(Trim(Copy(Lines[i], Colon + 1, MaxInt)));
    end;
end;

function CurlExitMeaning(AExitCode: longint): TCurlFailureKind;
begin
    //  The numbers are curl's documented exit codes (man curl, EXIT CODES).
    case AExitCode of
        0:
            Result := cfNone;
        5, 6:
            Result := cfResolve;
        7:
            Result := cfConnect;
        28:
            Result := cfTimeout;
        35, 51, 53, 54, 58, 59, 60, 64, 66, 77, 80, 83, 90, 91:
            Result := cfSecureChannel;
        else
            Result := cfOther;
    end;
end;

constructor TCurlClient.Create(const AExecutable: string);
begin
    inherited Create;
    FExecutable := AExecutable;
    FTimeoutMs := 30000;
    FMaxRedirects := 5;
end;

class function TCurlClient.Locate: string;
var
    Candidate: string;
begin
    for Candidate in CurlCandidates(ThisCurlHost,
        GetEnvironmentVariable('SystemRoot'), GetEnvironmentVariable('PATH')) do
        if FileExists(Candidate) then
            Exit(Candidate);
    Result := '';
end;

function TCurlClient.Get(const AUrl: string; ADest: TStream;
    AOnChunk: TCurlChunkEvent): TCurlResult;
var
    Dump, Stderr: string;
    Stopped: boolean;
begin
    Result := Default(TCurlResult);
    Result.ExitCode := Run(AUrl, ADest, AOnChunk, Dump, Stderr, Stopped);
    Result.Stopped := Stopped;
    Result.ErrorText := Trim(Stderr);
    Result.Headers := LastResponseBlock(Dump);
    Result.Status := LastResponseStatus(Dump);
end;

function ReadWholeFile(const APath: string): string;
var
    F: TFileStream;
begin
    Result := '';
    if not FileExists(APath) then
        Exit;
    F := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
    try
        SetLength(Result, F.Size);
        if F.Size > 0 then
            F.ReadBuffer(Result[1], F.Size);
    finally
        F.Free;
    end;
end;

function TCurlClient.Run(const AUrl: string; ADest: TStream;
    AOnChunk: TCurlChunkEvent; out AHeaders, AStderr: string;
    out AStopped: boolean): longint;
var
    Proc: TProcess;
    HeaderFile, Arg: string;
    Buffer: array[0..65535] of byte;
    N: longint;
    Total: int64;
    LastAsked: QWord;
    Moved: boolean;

    procedure DrainStderr;
    var
        Chunk: string;
        Available: longint;
    begin
        Available := Proc.Stderr.NumBytesAvailable;
        if Available <= 0 then
            Exit;
        SetLength(Chunk, Available);
        SetLength(Chunk, Proc.Stderr.Read(Chunk[1], Available));
        AStderr := AStderr + Chunk;
    end;

begin
    AHeaders := '';
    AStderr := '';
    AStopped := False;
    Total := 0;
    HeaderFile := GetTempFileName(GetTempDir(False), 'fit-curl');
    Proc := TProcess.Create(nil);
    try
        Proc.Executable := FExecutable;
        for Arg in CurlArguments(AUrl, FUserAgent, FTimeoutMs, FMaxRedirects,
            HeaderFile) do
            Proc.Parameters.Add(Arg);
        //  NO SHELL, and no console window on Windows. Both pipes are drained
        //  in the one loop below, so neither can fill and stall curl.
        Proc.Options := [poUsePipes, poNoConsole];
        Proc.ShowWindow := swoHide;
        Proc.Execute;
        Proc.CloseInput;
        LastAsked := GetTickCount64;
        repeat
            Moved := False;
            while Proc.Output.NumBytesAvailable > 0 do
            begin
                N := Proc.Output.Read(Buffer,
                    Min(SizeOf(Buffer), Proc.Output.NumBytesAvailable));
                if N <= 0 then
                    Break;
                ADest.WriteBuffer(Buffer, N);
                Inc(Total, N);
                Moved := True;
                LastAsked := GetTickCount64;
                if Assigned(AOnChunk) and not AOnChunk(Total) then
                begin
                    AStopped := True;
                    Break;
                end;
            end;
            DrainStderr;
            if AStopped then
                Break;
            if not Moved then
            begin
                if not Proc.Running and (Proc.Output.NumBytesAvailable = 0) then
                    Break;
                //  A STALL IS WHERE A CANCEL MATTERS MOST: a transfer nothing
                //  arrives on would otherwise be deaf until curl's own timeout.
                if Assigned(AOnChunk) and
                    (GetTickCount64 - LastAsked >= IdleAskMs) then
                begin
                    LastAsked := GetTickCount64;
                    if not AOnChunk(Total) then
                    begin
                        AStopped := True;
                        Break;
                    end;
                end;
                Sleep(10);
            end;
        until False;
        if AStopped then
            Proc.Terminate(1);
        Proc.WaitOnExit;
        DrainStderr;
        Result := Proc.ExitCode;
        AHeaders := ReadWholeFile(HeaderFile);
    finally
        Proc.Free;
        DeleteFile(HeaderFile);
    end;
end;

end.
