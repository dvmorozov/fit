// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A THttpFitService whose replies come from a table instead of a socket.)

WHAT IT UNLOCKS. http_fit_service.pas is some seven hundred lines, and almost all
of them are marshalling: building a URL, encoding a body, reading a reply,
deciding what a missing field means. None of it was reachable by a test, because
each of the three transport call sites constructed its own TFPHTTPClient inline -
so a double could only override the high-level verbs, which is exactly the code it
wanted to exercise. The unit sat at 2 % while being what every client action goes
through.

THttpFitService.Fetch and .Send are the seam that came out of that. Overriding
them here runs every line of the real marshalling against a canned reply.

HOW REPLIES ARE CHOSEN. By the LAST PATH SEGMENT of the URL, not by the whole URL:
the service builds URLs containing a problem id it allocates itself, so a test
matching on a full URL would have to predict that id. Reply with the key
'settings' therefore answers every GET whose URL ends in /settings.

Requests are recorded in order, so a test can assert what was sent as well as what
was made of the answer.
}
unit mock_http_transport;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, http_fit_service, mock_support;

type
    TMockHttpService = class(THttpFitService)
    private
        FReplies: TStringList;
        FLog: TCallLog;
        FFailNext: boolean;
        FFailWith: string;
        FLastBody: string;
        FLastUrl: string;
        { The last segment of AUrl, ignoring any query and trailing slash. }
        function KeyOf(const AUrl: string): string;
        function ReplyFor(const AUrl: string): string;
    protected
        function Fetch(const AUrl: string; ATimeoutMs: integer): string; override;
        function Send(const AMethod, AUrl, ABody: string;
            ATimeoutMs: integer): string; override;
    public
        constructor Create(const ABaseUrl: string);
        destructor Destroy; override;

        { Answers every request whose URL ends in AKey with AJson. }
        procedure Reply(const AKey, AJson: string);
        { The next request raises a transport failure, the way an unreachable
          server does. One-shot, so a test can prove the failure is reported and
          then carry on. }
        procedure FailNextWith(const AMessage: string);

        { What was asked, in order: 'GET /problems/1/settings' and so on. }
        property Log: TCallLog read FLog;
        { The body of the most recent write. Named separately from the log
          because asserting what was SENT is a different question from asserting
          what was asked, and reading it out of a log line means parsing one. }
        property LastBody: string read FLastBody;
        property LastUrl: string read FLastUrl;
    end;

implementation

uses
    MyExceptions;

constructor TMockHttpService.Create(const ABaseUrl: string);
begin
    inherited Create(ABaseUrl);
    FReplies := TStringList.Create;
    FReplies.CaseSensitive := False;
    FLog := TCallLog.Create;
    //  STUBBED BY DEFAULT, because the service creates a problem lazily before
    //  its FIRST call of any kind and refuses to continue without an id. Leaving
    //  this to each test would make every one of them start by stubbing a route it
    //  is not about, and forgetting it fails with "the compute server did not
    //  create a problem" - which reads as a transport fault rather than a missing
    //  stub. A test that cares about problem creation can still override it.
    Reply('problems', '{"ok":true,"id":1}');
end;

destructor TMockHttpService.Destroy;
begin
    FLog.Free;
    FReplies.Free;
    inherited;
end;

procedure TMockHttpService.Reply(const AKey, AJson: string);
begin
    FReplies.Values[AKey] := AJson;
end;

procedure TMockHttpService.FailNextWith(const AMessage: string);
begin
    FFailNext := True;
    FFailWith := AMessage;
end;

function TMockHttpService.KeyOf(const AUrl: string): string;
var
    i: integer;
    P: string;
begin
    P := AUrl;
    i := Pos('?', P);
    if i > 0 then
        P := Copy(P, 1, i - 1);
    while (P <> '') and (P[Length(P)] = '/') do
        SetLength(P, Length(P) - 1);
    Result := P;
    for i := Length(P) downto 1 do
        if P[i] = '/' then
        begin
            Result := Copy(P, i + 1, MaxInt);
            Break;
        end;
end;

function TMockHttpService.ReplyFor(const AUrl: string): string;
var
    Key: string;
begin
    if FFailNext then
    begin
        FFailNext := False;
        //  EUserException is what a rejection BY the server looks like, and the
        //  service is required to let it through rather than relabel it as a
        //  transport fault. Raised from here so that distinction is exercised.
        raise EUserException.Create(FFailWith);
    end;
    Key := KeyOf(AUrl);
    if FReplies.IndexOfName(Key) >= 0 then
        Result := FReplies.Values[Key]
    else
        //  A well-formed empty success, so an unstubbed route does not look like a
        //  transport failure. A test that cares must stub the route.
        Result := '{"ok":true}';
end;

function TMockHttpService.Fetch(const AUrl: string; ATimeoutMs: integer): string;
begin
    FLog.Note('GET', AUrl);
    Result := ReplyFor(AUrl);
end;

function TMockHttpService.Send(const AMethod, AUrl, ABody: string;
    ATimeoutMs: integer): string;
begin
    FLog.Note(AMethod, AUrl + ' ' + ABody);
    FLastUrl := AUrl;
    FLastBody := ABody;
    Result := ReplyFor(AUrl);
end;

end.
