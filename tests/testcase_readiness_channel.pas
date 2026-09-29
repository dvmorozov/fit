// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A parent learns its child is listening, or has died, without polling.)

These open real loopback sockets on ports the operating system chooses, so they
need no free well-known port and no second process: the child's end is played by
a plain socket in the same test.
}
unit testcase_readiness_channel;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry, ssockets,
    readiness_channel;

type
    TReadinessChannelTest = class(TTestCase)
    published
        procedure AListenerHasAPortOfItsOwn;
        procedure AnAnnouncementIsReadiness;
        procedure NothingWithinTheBudgetIsATimeout;
        procedure AChildThatConnectsAndDiesEndsTheWaitAtOnce;
        procedure TheLifelineEndsWhenTheParentLetsGo;
        procedure AnnouncingToNothingSaysSo;
    end;

implementation

function MsSince(AStart: TDateTime): int64;
begin
    Result := MilliSecondsBetween(Now, AStart);
end;

procedure TReadinessChannelTest.AListenerHasAPortOfItsOwn;
var
    L: TReadinessListener;
begin
    L := TReadinessListener.Create;
    try
        AssertTrue('a port the system chose', L.Port > 0);
    finally
        L.Free;
    end;
end;

procedure TReadinessChannelTest.AnAnnouncementIsReadiness;
var
    L: TReadinessListener;
begin
    L := TReadinessListener.Create;
    try
        //  The connection queues on the listening socket before it is accepted,
        //  so the child's end can run first in the same thread.
        AssertTrue('announced', AnnounceReady(L.Port));
        AssertTrue('ready', L.WaitForReady(5000) = rdReady);
    finally
        L.Free;
    end;
end;

procedure TReadinessChannelTest.NothingWithinTheBudgetIsATimeout;
var
    L: TReadinessListener;
    Start: TDateTime;
begin
    L := TReadinessListener.Create;
    try
        Start := Now;
        AssertTrue('timed out', L.WaitForReady(150) = rdTimedOut);
        //  It WAITED - a deadline, not a single look - and did not wait long.
        AssertTrue('waited out the budget', MsSince(Start) >= 100);
        AssertTrue('and no longer', MsSince(Start) < 3000);
    finally
        L.Free;
    end;
end;

procedure TReadinessChannelTest.AChildThatConnectsAndDiesEndsTheWaitAtOnce;
var
    L: TReadinessListener;
    Child: TInetSocket;
    Start: TDateTime;
begin
    //  A MISSING LIBRARY EXITS AT ONCE, and the operating system closes its
    //  connection. The parent must hear that immediately, not after its budget.
    L := TReadinessListener.Create;
    try
        Child := TInetSocket.Create('127.0.0.1', L.Port);
        Child.Free;
        Start := Now;
        AssertTrue('ended', L.WaitForReady(10000) = rdEnded);
        AssertTrue('at once, not after the budget', MsSince(Start) < 3000);
    finally
        L.Free;
    end;
end;

procedure TReadinessChannelTest.TheLifelineEndsWhenTheParentLetsGo;
var
    L: TReadinessListener;
    Child: TInetSocket;
    Line: string;
    Buf: array[0..15] of char;
begin
    L := TReadinessListener.Create;
    Child := TInetSocket.Create('127.0.0.1', L.Port);
    try
        Line := ReadyLine + #10;
        Child.WriteBuffer(Line[1], Length(Line));
        AssertTrue('ready', L.WaitForReady(5000) = rdReady);
        //  The parent goes. The child's blocking read is what notices.
        FreeAndNil(L);
        Child.IOTimeout := 5000;
        AssertEquals('end of file', 0, Child.Read(Buf, SizeOf(Buf)));
    finally
        Child.Free;
        L.Free;
    end;
end;

procedure TReadinessChannelTest.AnnouncingToNothingSaysSo;
var
    L: TReadinessListener;
    Dead: word;
begin
    //  A port that was listening a moment ago and is not now.
    L := TReadinessListener.Create;
    Dead := L.Port;
    L.Free;
    AssertFalse(AnnounceReady(Dead));
end;

initialization
    RegisterTest('unit', TReadinessChannelTest);
end.
