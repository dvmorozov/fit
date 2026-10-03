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
    private
        FAsked: longint;
        FStopAfter: longint;
        { The stop question a waiting caller passes: true from the FStopAfter-th
          time it is asked. }
        function StopWanted: boolean;
    published
        procedure AListenerHasAPortOfItsOwn;
        procedure AnAnnouncementIsReadiness;
        procedure NothingWithinTheBudgetIsATimeout;
        procedure AChildThatConnectsAndDiesEndsTheWaitAtOnce;
        procedure TheLifelineEndsWhenTheParentLetsGo;
        procedure AnnouncingToNothingSaysSo;
        procedure AStopEndsTheWaitWithinASlice;
        procedure AStopIsAskedWhileTheChildIsConnectedToo;
        procedure AReadyLineSplitAcrossTwoWaitsIsStillReady;
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

function TReadinessChannelTest.StopWanted: boolean;
begin
    Inc(FAsked);
    Result := FAsked >= FStopAfter;
end;

procedure TReadinessChannelTest.AStopEndsTheWaitWithinASlice;
var
    L: TReadinessListener;
    Start: TDateTime;
begin
    //  A CALLER THAT NO LONGER WANTS THE CHILD - the user pressed Stop while the
    //  Python engine was still starting - must not sit out a budget of minutes.
    //  It is asked between slices, so it is heard within one.
    L := TReadinessListener.Create;
    try
        FAsked := 0;
        FStopAfter := 2;
        Start := Now;
        AssertTrue('abandoned', L.WaitForReady(60000, @StopWanted) = rdAbandoned);
        AssertTrue('within a slice or two, not the budget', MsSince(Start) < 3000);
        AssertTrue('it asked between slices', FAsked >= 2);
    finally
        L.Free;
    end;
end;

procedure TReadinessChannelTest.AStopIsAskedWhileTheChildIsConnectedToo;
var
    L: TReadinessListener;
    Child: TInetSocket;
    Start: TDateTime;
begin
    //  The slow part of a start comes AFTER the child connects: it connects
    //  first and imports numpy, scipy and lmfit before it writes its line. A
    //  stop that was heard only before the connection would be heard never.
    L := TReadinessListener.Create;
    Child := TInetSocket.Create('127.0.0.1', L.Port);
    try
        FAsked := 0;
        FStopAfter := 3;
        Start := Now;
        AssertTrue('abandoned', L.WaitForReady(60000, @StopWanted) = rdAbandoned);
        AssertTrue('promptly', MsSince(Start) < 3000);
    finally
        Child.Free;
        L.Free;
    end;
end;

procedure TReadinessChannelTest.AReadyLineSplitAcrossTwoWaitsIsStillReady;
var
    L: TReadinessListener;
    Child: TInetSocket;
    Part: string;
begin
    //  A WAIT THAT ENDS IS NOT A START THAT ENDS: the child goes on, and the
    //  next caller resumes waiting on it. What the first wait had already read
    //  of the line belongs to the child, not to that wait - forgotten, a line
    //  that arrived in two pieces either side of the gap would never be seen.
    L := TReadinessListener.Create;
    Child := TInetSocket.Create('127.0.0.1', L.Port);
    try
        Part := Copy(ReadyLine, 1, 2);
        Child.WriteBuffer(Part[1], Length(Part));
        AssertTrue('half a line is not ready', L.WaitForReady(300) = rdTimedOut);
        Part := Copy(ReadyLine, 3, MaxInt) + #10;
        Child.WriteBuffer(Part[1], Length(Part));
        AssertTrue('the rest completes it', L.WaitForReady(5000) = rdReady);
    finally
        Child.Free;
        L.Free;
    end;
end;

initialization
    RegisterTest('unit', TReadinessChannelTest);
end.
