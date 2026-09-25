// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The real TCurlClient with its process replaced by a recorded answer.)

Run is the one method of TCurlClient that starts a process, so overriding it
leaves every other line - Get's reading of the header dump, the stop, the
result - running exactly as the application runs it. The chunks are handed over
one at a time and the caller is asked after each, as curl's output is read, so
a stop part-way is observable: what the caller refused is not written.

Whoever creates one frees it. When a TWebClient double hands one out from
CreateCurl, the web client frees it after the transfer, as it frees the real one.
}
unit mock_curl_client;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, curl_client;

type
    { What a transfer asked curl for, written where the test can still read it
      after the client that made the call has freed the curl. }
    TCurlCall = record
        Url, UserAgent: string;
        TimeoutMs, MaxRedirects: longint;
    end;
    PCurlCall = ^TCurlCall;

    TRecordedCurlClient = class(TCurlClient)
    private
        FChunks: array of string;
        FHeaders: string;
        FStderr: string;
        FExitCode: longint;
        FUrlSeen: string;
        FReport: PCurlCall;
    protected
        function Run(const AUrl: string; ADest: TStream;
            AOnChunk: TCurlChunkEvent; out AHeaders, AStderr: string;
            out AStopped: boolean): longint; override;
    public
        { One piece of the body, in the order it arrives. }
        procedure AddChunk(const AText: string);
        { The header dump curl would have written: every response of a chain. }
        property Headers: string read FHeaders write FHeaders;
        property Stderr: string read FStderr write FStderr;
        property ExitCode: longint read FExitCode write FExitCode;
        { The address Run was asked for. }
        property UrlSeen: string read FUrlSeen;
        { Where Run reports the call it was asked to make; nil for nowhere. }
        property Report: PCurlCall read FReport write FReport;
    end;

const
    CRLF = #13#10;
    { What curl -L -D writes for a redirect followed to a file: one block per
      response, each ending in a blank line. The redirect's Retry-After belongs
      to the redirect and not to the file. }
    RedirectDump =
        'HTTP/1.1 302 Found' + CRLF +
        'Location: https://example.org/b' + CRLF +
        'Retry-After: 99' + CRLF +
        CRLF +
        'HTTP/2 200' + CRLF +
        'content-type: text/csv' + CRLF +
        'Content-Disposition: attachment; filename="series.csv"' + CRLF +
        CRLF;
    { A plain answer with nothing suggested. }
    OkDump = 'HTTP/1.1 200 OK' + CRLF + 'Content-Type: text/plain' + CRLF + CRLF;
    { An address with nothing there. }
    NotFoundDump = 'HTTP/1.1 404 Not Found' + CRLF + CRLF;

implementation

function TRecordedCurlClient.Run(const AUrl: string; ADest: TStream;
    AOnChunk: TCurlChunkEvent; out AHeaders, AStderr: string;
    out AStopped: boolean): longint;
var
    i: integer;
    Total: int64;
begin
    FUrlSeen := AUrl;
    if FReport <> nil then
    begin
        FReport^.Url := AUrl;
        FReport^.UserAgent := UserAgent;
        FReport^.TimeoutMs := TimeoutMs;
        FReport^.MaxRedirects := MaxRedirects;
    end;
    AStopped := False;
    Total := 0;
    for i := 0 to High(FChunks) do
    begin
        ADest.WriteBuffer(FChunks[i][1], Length(FChunks[i]));
        Inc(Total, Length(FChunks[i]));
        if Assigned(AOnChunk) and not AOnChunk(Total) then
        begin
            AStopped := True;
            Break;
        end;
    end;
    AHeaders := FHeaders;
    AStderr := FStderr;
    Result := FExitCode;
end;

procedure TRecordedCurlClient.AddChunk(const AText: string);
begin
    SetLength(FChunks, Length(FChunks) + 1);
    FChunks[High(FChunks)] := AText;
end;

end.
