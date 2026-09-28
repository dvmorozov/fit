// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How a process learns that a process it started is listening - and how
a child learns that its parent is gone - without asking again and again.)

THE SHAPE. The parent opens a listening socket on a loopback port of its own and
passes the port to the child. The child connects to it as soon as it starts, and
writes one line, "ready", once it is actually listening for its real work. The
parent blocks on that connection with a deadline:

  * "ready" arrives        - the child is up;
  * the connection closes  - the child died before it got there (a missing
                             library, a syntax error), and the parent knows at
                             once instead of waiting out the whole budget;
  * the deadline passes    - it never became ready.

THE LIFELINE. The parent keeps the accepted connection open for as long as it
owns the child. A child that must not outlive its parent blocks reading it: when
the parent process ends - cleanly, or killed - the operating system closes the
connection and the read returns end-of-file.

WHY A SOCKET, not a pipe or a process id. A pipe the child inherits is created
differently on every platform and fills up if nobody reads it; a process id can
only be asked about again and again. A loopback connection is the same on
macOS, Linux and Windows, is closed by the operating system when either end
dies, and can be waited on with a deadline.
}
unit readiness_channel;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Sockets;

const
    { The line a child writes when it is ready. }
    ReadyLine = 'ready';

type
    TReadiness = (
        { "ready" arrived. }
        rdReady,
        { The child connected and the connection closed before "ready": it died. }
        rdEnded,
        { Nothing arrived within the budget. }
        rdTimedOut);

    { The parent's end: listens on a loopback port of its own. }
    TReadinessListener = class(TObject)
    private
        FListening: Sockets.TSocket;
        FConnection: Sockets.TSocket;
        FPort: word;
    public
        constructor Create;
        destructor Destroy; override;
        { The port to pass to the child. }
        function Port: word;
        { Blocks until the child is ready, has died, or ABudgetMs has passed.
          On rdReady the connection is kept: freeing the listener closes it,
          which is the child's lifeline ending. }
        function WaitForReady(ABudgetMs: longint): TReadiness;
    end;

{ The child's end, for a child that only has to say it is ready: connects to
  APort on loopback, writes ReadyLine and closes. False when nothing listens. }
function AnnounceReady(APort: word): boolean;

implementation

uses
{$IFDEF UNIX}
    BaseUnix,
{$ENDIF}
{$IFDEF WINDOWS}
    WinSock2,
{$ENDIF}
    DateUtils;

const
    //  Sockets.TSocket, NAMED BY ITS UNIT, everywhere in this unit
    //  (testcase_winsock_types holds every source to that). WinSock2 is used
    //  after Sockets, so a bare TSocket here means WinSock2's - an unsigned
    //  64-bit UINT_PTR - while the fields are the Sockets unit's signed longint.
    //  NoSocket then became $FFFFFFFFFFFFFFFF, "FConnection = NoSocket" was never
    //  true for a -1 longint, accept was skipped, and every wait on Windows ended
    //  at once as a timeout. Linux has only the one type, so it never showed there.
    NoSocket = Sockets.TSocket(-1);

function LoopbackAddress(APort: word): TInetSockAddr;
begin
    Result := Default(TInetSockAddr);
    Result.sin_family := AF_INET;
    Result.sin_port := htons(APort);
    Result.sin_addr := StrToNetAddr('127.0.0.1');
end;

{ True when ASocket has something to read - a connection to accept, data, or
  its end - within AMs. The one place the wait happens: a deadline on an
  event, never a sleep between looks. }
function Readable(ASocket: Sockets.TSocket; AMs: longint): boolean;
var
{$IFDEF UNIX}
    Fds: TFDSet;
{$ENDIF}
{$IFDEF WINDOWS}
    Fds: TFDSet;
{$ENDIF}
    Wait: TTimeVal;
begin
    if AMs < 0 then
        AMs := 0;
    Wait.tv_sec := AMs div 1000;
    Wait.tv_usec := (AMs mod 1000) * 1000;
{$IFDEF UNIX}
    fpFD_ZERO(Fds);
    fpFD_SET(ASocket, Fds);
    Result := fpSelect(ASocket + 1, @Fds, nil, nil, @Wait) > 0;
{$ENDIF}
{$IFDEF WINDOWS}
    FD_ZERO(Fds);
    FD_SET(WinSock2.TSocket(ASocket), Fds);
    Result := select(0, @Fds, nil, nil, @Wait) > 0;
{$ENDIF}
end;

constructor TReadinessListener.Create;
var
    Addr: TInetSockAddr;
    Len: TSockLen;
begin
    inherited Create;
    FConnection := NoSocket;
    FListening := fpSocket(AF_INET, SOCK_STREAM, 0);
    if FListening = NoSocket then
        raise Exception.Create('readiness channel: no socket could be created');
    //  PORT 0: the operating system chooses one that is free, so two parents
    //  starting children at once never collide.
    Addr := LoopbackAddress(0);
    if fpBind(FListening, @Addr, SizeOf(Addr)) <> 0 then
        raise Exception.Create('readiness channel: cannot bind a loopback port');
    if fpListen(FListening, 1) <> 0 then
        raise Exception.Create('readiness channel: cannot listen');
    Len := SizeOf(Addr);
    if fpGetSockName(FListening, @Addr, @Len) <> 0 then
        raise Exception.Create('readiness channel: cannot read its own port');
    FPort := ntohs(Addr.sin_port);
end;

destructor TReadinessListener.Destroy;
begin
    //  The lifeline ends here: a child blocked reading its end sees end-of-file.
    if FConnection <> NoSocket then
        CloseSocket(FConnection);
    if FListening <> NoSocket then
        CloseSocket(FListening);
    inherited Destroy;
end;

function TReadinessListener.Port: word;
begin
    Result := FPort;
end;

function TReadinessListener.WaitForReady(ABudgetMs: longint): TReadiness;
var
    Deadline: TDateTime;
    Buf: array[0..63] of char;
    Got: SizeInt;
    Seen, Chunk: string;

    function Remaining: longint;
    begin
        Result := MilliSecondsBetween(Deadline, Now);
        if Now >= Deadline then
            Result := 0;
    end;

begin
    Deadline := IncMilliSecond(Now, ABudgetMs);
    if FConnection = NoSocket then
    begin
        if not Readable(FListening, Remaining) then
            Exit(rdTimedOut);
        FConnection := fpAccept(FListening, nil, nil);
        if FConnection = NoSocket then
            Exit(rdEnded);
    end;
    Seen := '';
    repeat
        if not Readable(FConnection, Remaining) then
            Exit(rdTimedOut);
        Got := fpRecv(FConnection, @Buf, SizeOf(Buf), 0);
        //  Nothing to read on a readable socket is its end: the child died.
        if Got <= 0 then
            Exit(rdEnded);
        SetString(Chunk, PChar(@Buf[0]), Got);
        Seen := Seen + Chunk;
        if Pos(ReadyLine, Seen) > 0 then
            Exit(rdReady);
    until False;
end;

function AnnounceReady(APort: word): boolean;
var
    S: Sockets.TSocket;
    Addr: TInetSockAddr;
    Line: string;
begin
    Result := False;
    S := fpSocket(AF_INET, SOCK_STREAM, 0);
    if S = NoSocket then
        Exit;
    try
        Addr := LoopbackAddress(APort);
        if fpConnect(S, @Addr, SizeOf(Addr)) <> 0 then
            Exit;
        Line := ReadyLine + #10;
        Result := fpSend(S, @Line[1], Length(Line), 0) = Length(Line);
    finally
        CloseSocket(S);
    end;
end;

end.
