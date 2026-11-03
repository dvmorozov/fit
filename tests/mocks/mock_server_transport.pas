// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A TServerFitBackend whose HTTP calls are answered from the test.)

The same shape as mock_python_transport, for the same reason: everything
server_fit_backend decides sits either side of one GET and one POST, and with the
client built inline none of it could be reached without a compute server running.

See mock_support for the -SIcorba lifetime rule.
}
unit mock_server_transport;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, server_fit_backend;

type
    TMockServerBackend = class(TServerFitBackend)
    private
        FPostReply: string;
        FGetReply: string;
        FFailGet: string;
        FFailPost: string;
        FLastPostUrl: string;
        FLastGetUrl: string;
        FLastBody: string;
        FPosts: longint;
        FGets: longint;
        FHoldPostUntilGets: longint;
    protected
        function Get(const AUrl: string): string; override;
        function Post(const AUrl, ABody: string): string; override;
    public
        procedure ReplyToPost(const AJson: string);
        procedure ReplyToGet(const ABody: string);
        { The next call of that verb raises, the way an unreachable server does. }
        procedure FailGetWith(const AMessage: string);
        procedure FailPostWith(const AMessage: string);

        property LastPostUrl: string read FLastPostUrl;
        property LastGetUrl: string read FLastGetUrl;
        property LastBody: string read FLastBody;
        property Posts: longint read FPosts;
        property Gets: longint read FGets;
        { The POST stays out until this many GETs have been made - the progress
          polls a fit relays - so a test sees them without depending on timing. }
        property HoldPostUntilGets: longint read FHoldPostUntilGets
            write FHoldPostUntilGets;
    end;

implementation

procedure TMockServerBackend.ReplyToPost(const AJson: string);
begin
    FPostReply := AJson;
    FFailPost := '';
end;

procedure TMockServerBackend.ReplyToGet(const ABody: string);
begin
    FGetReply := ABody;
    FFailGet := '';
end;

procedure TMockServerBackend.FailGetWith(const AMessage: string);
begin
    FFailGet := AMessage;
end;

procedure TMockServerBackend.FailPostWith(const AMessage: string);
begin
    FFailPost := AMessage;
end;

function TMockServerBackend.Get(const AUrl: string): string;
begin
    Inc(FGets);
    FLastGetUrl := AUrl;
    if FFailGet <> '' then
        raise Exception.Create(FFailGet);
    Result := FGetReply;
end;

function TMockServerBackend.Post(const AUrl, ABody: string): string;
var
    Deadline: TDateTime;
begin
    Deadline := Now + 5 / SecsPerDay;
    while (FGets < FHoldPostUntilGets) and (Now < Deadline) do
        Sleep(5);
    Inc(FPosts);
    FLastPostUrl := AUrl;
    FLastBody := ABody;
    if FFailPost <> '' then
        raise Exception.Create(FFailPost);
    Result := FPostReply;
end;

end.
