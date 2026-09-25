// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An IWebClient whose answers come from a table instead of the internet.)

WHY EVERY SOURCE IS TESTED THROUGH THIS. A data source is, in the end, two
things: which address it asks for, and what it makes of the answer. Both are
exactly what a test needs to pin, and neither needs a network - while a test that
did reach the network would fail when a service is slow, rate-limits it, or
changes something nobody here controls, and would pass tomorrow for reasons
nobody here decided.

So recorded replies are the rule: a fixture file recorded once, by hand, with the
date and the command it was recorded with written down beside it.

MATCHING IS BY SUBSTRING of the URL, not by the whole of it. Catalogue addresses
carry encoded queries, sizes and page numbers, and a test that spelled the whole
URL out would be asserting the encoding rather than the behaviour. Asking for a
URL nothing matches is itself a failure the mock records - a source quietly
asking for the wrong address is the defect this is most likely to catch.

LIFETIME: see mock_support. It is a plain object, the fixture owns it, and the
interface reference is nilled before the object is freed.
}
unit mock_web_client;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, int_web_client, web_client, mock_support;

type
    TMockWebClient = class(TMockBase, IWebClient)
    private
        //  Two lists rather than name=value pairs: a reply is a whole JSON or
        //  CSV body with line breaks in it, and Values would keep only the
        //  first line of one.
        FUrlParts: TStringList;
        FBodies: TStringList;
        FFailWith: string;
    FCancelNext: boolean;
        FProgress: TWebProgress;
        FCancelled: boolean;
        function ReplyFor(const AUrl: string): string;
    public
        constructor Create; override;
        destructor Destroy; override;

        function GetText(const AUrl: string): string;
        function Download(const AUrl: string; ADest: TStream): string;
        procedure SetProgress(AProgress: TWebProgress);
        procedure Cancel;

        { Answers every request whose URL contains AUrlPart with ABody. }
        procedure Reply(const AUrlPart, ABody: string);
        { The next request raises EWebError with AMessage, the way an
          unreachable host does. Not one-shot: an unreachable host stays
          unreachable, which is the situation being described. }
        procedure FailWith(const AMessage: string);
        { The next request comes back as one the user stopped - which is a
          kind of its own, because nothing is wrong with it. }
        procedure CancelNext;
        { What the last Download was told the file is called, so a test can
          drive the "the server named it" path. }
        property Cancelled: boolean read FCancelled;
    end;

implementation

constructor TMockWebClient.Create;
begin
    inherited Create;
    FUrlParts := TStringList.Create;
    FBodies := TStringList.Create;
end;

destructor TMockWebClient.Destroy;
begin
    FBodies.Free;
    FUrlParts.Free;
    inherited Destroy;
end;

procedure TMockWebClient.Reply(const AUrlPart, ABody: string);
begin
    FUrlParts.Add(AUrlPart);
    FBodies.Add(ABody);
end;

procedure TMockWebClient.FailWith(const AMessage: string);
begin
    FFailWith := AMessage;
end;

procedure TMockWebClient.CancelNext;
begin
    FCancelNext := True;
end;

function TMockWebClient.ReplyFor(const AUrl: string): string;
var
    i: longint;
begin
    for i := 0 to FUrlParts.Count - 1 do
        if Pos(FUrlParts[i], AUrl) > 0 then
            Exit(FBodies[i]);
    //  Asking for an address no reply matches is a finding, not an empty
    //  answer: a source addressing the wrong service would otherwise look like
    //  a service with nothing to say.
    raise EWebError.Create('the test has no reply for ' + AUrl);
end;

function TMockWebClient.GetText(const AUrl: string): string;
begin
    FLog.Note('GetText', AUrl);
    if FCancelNext then
        raise EWebCancelled.Create('The download was stopped.');
    if FFailWith <> '' then
        raise EWebError.Create(FFailWith);
    Result := ReplyFor(AUrl);
end;

function TMockWebClient.Download(const AUrl: string; ADest: TStream): string;
var
    Body: string;
begin
    FLog.Note('Download', AUrl);
    if FCancelNext then
        raise EWebCancelled.Create('The download was stopped.');
    if FFailWith <> '' then
        raise EWebError.Create(FFailWith);
    Body := ReplyFor(AUrl);
    if Length(Body) > 0 then
        ADest.WriteBuffer(Body[1], Length(Body));
    if Assigned(FProgress) then
        FProgress(Length(Body), Length(Body));
    //  The mock never suggests a name: the sources that rely on one are the
    //  ones whose tests set it explicitly.
    Result := '';
end;

procedure TMockWebClient.SetProgress(AProgress: TWebProgress);
begin
    FProgress := AProgress;
end;

procedure TMockWebClient.Cancel;
begin
    FCancelled := True;
end;

end.
