// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Pointing the client at a compute server, and what follows from that.)

THE CLAIM WORTH THE FIXTURE is the one that is invisible when it is wrong: a
profile loaded while no server was reachable exists only in the client, so naming
a server that answers has to hand it over. Get it wrong and the user has a file on
screen, a server connected, and a fit with nothing to work on - which looks like
the file not having loaded.

The rest is the empty-means-default rule, which lived as two copies: the address
offered to the user and the address actually used. A difference between them shows
one server and uses another.
}
unit testcase_server_connection;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, server_connection;

const
    { Stands in for http_fit_service.DEFAULT_SERVER_URL. Passed in rather than
      read, so these rules can be tested without the transport. }
    A_DEFAULT = 'http://127.0.0.1:8787';

type
    TServerConnectionTest = class(TTestCase)
    published
        procedure AConfiguredUrlIsOfferedAsItIs;
        procedure NothingConfiguredOffersTheDefault;
        procedure BlankConfiguredAlsoOffersTheDefault;
        procedure WhatTheUserTypedIsTrimmed;
        procedure TypingNothingFallsBackToTheDefault;
        procedure TheOfferedAndTheUsedUrlAgree;
        procedure NoAnswerIsReportedRatherThanRetried;
        procedure AProfileLoadedWithoutAServerIsSentOnce;
        procedure WithNoProfileThereIsNothingToSend;
        procedure NothingIsSentToAServerThatDidNotAnswer;
        procedure TheNoticeNamesTheAddressAndTheBinary;
    end;

implementation

procedure TServerConnectionTest.AConfiguredUrlIsOfferedAsItIs;
begin
    AssertEquals('as configured', 'http://box:9000',
        ServerUrlToOffer('http://box:9000', A_DEFAULT));
end;

procedure TServerConnectionTest.NothingConfiguredOffersTheDefault;
begin
    //  Offering an empty box would hide the address that would be used.
    AssertEquals('the default', A_DEFAULT, ServerUrlToOffer('', A_DEFAULT));
end;

procedure TServerConnectionTest.BlankConfiguredAlsoOffersTheDefault;
begin
    //  A settings file can hold whitespace, and whitespace is not an address.
    AssertEquals('the default', A_DEFAULT,
        ServerUrlToOffer('   ', A_DEFAULT));
end;

procedure TServerConnectionTest.WhatTheUserTypedIsTrimmed;
begin
    //  A URL with a stray space resolves to nothing, and the space is invisible
    //  in the dialog it was typed into.
    AssertEquals('trimmed', 'http://box:9000',
        ServerUrlToUse('  http://box:9000 ', A_DEFAULT));
end;

procedure TServerConnectionTest.TypingNothingFallsBackToTheDefault;
begin
    AssertEquals('the default', A_DEFAULT, ServerUrlToUse('', A_DEFAULT));
end;

procedure TServerConnectionTest.TheOfferedAndTheUsedUrlAgree;
var
    Typed: string;
    i: longint;
begin
    //  THEY WERE TWO COPIES OF ONE RULE in two methods. If they can differ, the
    //  dialog shows one server and the client talks to another.
    for i := 0 to 3 do
    begin
        case i of
            0: Typed := '';
            1: Typed := '   ';
            2: Typed := 'http://box:9000';
        else
            Typed := ' http://box:9000 ';
        end;
        AssertEquals('offered and used disagree',
            ServerUrlToOffer(Typed, A_DEFAULT),
            ServerUrlToUse(Typed, A_DEFAULT));
    end;
end;

procedure TServerConnectionTest.NoAnswerIsReportedRatherThanRetried;
begin
    AssertTrue('tell the user',
        StepAfterProbing(False, False) = csTellTheUserNothingAnswered);
end;

procedure TServerConnectionTest.AProfileLoadedWithoutAServerIsSentOnce;
begin
    //  THE INVISIBLE RULE. The file was opened when there was nowhere to send it;
    //  this is the moment it can go.
    AssertTrue('send it', StepAfterProbing(True, True) = csSendTheProfile);
end;

procedure TServerConnectionTest.WithNoProfileThereIsNothingToSend;
begin
    AssertTrue('nothing to do',
        StepAfterProbing(True, False) = csNothingToDo);
end;

procedure TServerConnectionTest.NothingIsSentToAServerThatDidNotAnswer;
begin
    //  There is nowhere to send it, and trying would replace a clear message
    //  about the server with a transport error about the send.
    AssertTrue('the message comes first',
        StepAfterProbing(False, True) = csTellTheUserNothingAnswered);
end;

procedure TServerConnectionTest.TheNoticeNamesTheAddressAndTheBinary;
var
    Msg: string;
begin
    Msg := NoServerAnsweredNotice('http://box:9000');
    //  The only place the user learns which address was tried.
    AssertTrue('the address', Pos('http://box:9000', Msg) > 0);
    //  "Start the server" is not actionable if you do not know its name.
    AssertTrue('the binary', Pos('fit_server', Msg) > 0);
end;

initialization
    RegisterTest('unit', TServerConnectionTest);
end.
