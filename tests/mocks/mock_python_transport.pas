// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A TPythonFitBackend whose POST is answered from the test.)

WHAT IT UNLOCKS. Everything python_fit_backend decides sits either side of one
POST: how a live task becomes a fit problem, what an unreadable reply means, how
the sidecar's own rejection reaches the user, and what is written back into the
task afterwards. With the request built inline none of it was reachable without a
running Python interpreter, and the unit measured zero covered lines.

TPythonFitBackend.Post is the seam that came out of that. Overriding it here runs
the real marshalling against a canned reply, and records the body that was sent -
which is the only way to check what the Python side is being asked to fit.

See mock_support for the -SIcorba lifetime rule.
}
unit mock_python_transport;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, python_fit_backend;

type
    TMockPythonBackend = class(TPythonFitBackend)
    private
        FReply: string;
        FFailWith: string;
        FLastUrl: string;
        FLastBody: string;
        FCalls: longint;
        FGets: longint;
        FLastGetUrl: string;
        FProgressReply: string;
        FHoldPostUntilPolls: longint;
    protected
        function Post(const AUrl, ABody: string): string; override;
        function Get(const AUrl: string): string; override;
    public
        { What the progress route answers. Empty by default, which the relay
          reads as unreadable and stops asking. }
        property ProgressReply: string read FProgressReply write FProgressReply;
        { The POST stays out until the progress route has been asked this many
          times, so a test can see the polls without depending on timing. }
        property HoldPostUntilPolls: longint read FHoldPostUntilPolls
            write FHoldPostUntilPolls;
        property Gets: longint read FGets;
        property LastGetUrl: string read FLastGetUrl;
        { What the next POST answers. }
        procedure Reply(const AJson: string);
        { The next POST raises, the way an unreachable sidecar does. }
        procedure FailWith(const AMessage: string);

        { What was sent, so a test can assert the problem as well as the answer. }
        property LastUrl: string read FLastUrl;
        property LastBody: string read FLastBody;
        property Calls: longint read FCalls;
    end;

implementation

procedure TMockPythonBackend.Reply(const AJson: string);
begin
    FReply := AJson;
    FFailWith := '';
end;

procedure TMockPythonBackend.FailWith(const AMessage: string);
begin
    FFailWith := AMessage;
end;

function TMockPythonBackend.Get(const AUrl: string): string;
begin
    FLastGetUrl := AUrl;
    Inc(FGets);
    Result := FProgressReply;
end;

function TMockPythonBackend.Post(const AUrl, ABody: string): string;
var
    Deadline: TDateTime;
begin
    //  Five seconds at most, so a relay that never polls is a failing assertion
    //  rather than a hang.
    Deadline := Now + 5 / SecsPerDay;
    while (FGets < FHoldPostUntilPolls) and (Now < Deadline) do
        Sleep(5);
    Inc(FCalls);
    FLastUrl := AUrl;
    FLastBody := ABody;
    if FFailWith <> '' then
        raise Exception.Create(FFailWith);
    Result := FReply;
end;

end.
