// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How fit_server tells whoever started it that it is listening.)

THE HOOK THAT EXISTS. FPC's HTTP server binds, listens and starts accepting inside
one call that does not return until the server stops, and offers no event between
"listening" and "accepting". What it does offer is an idle event raised when no
connection arrives within AcceptIdleTimeout - and the first such event can only
happen once the accept loop is running, which means the port is listening.

So the server is ARMED with a one-millisecond idle timeout and an announcer. The
first idle announces readiness over the readiness channel and then DISARMS: the
announcer is removed and the timeout set to the longest there is. Setting it back
to zero would not do - the idle loop re-reads the timeout on every pass and would
spin - so "never" is spelled as a timeout nobody waits out. It is one event at
start-up, not a check repeated for the life of the server.
}
unit server_readiness;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fphttpserver, readiness_channel;

const
    { The idle timeout the armed server waits before its first idle event. }
    ServerArmedIdleMs = 1;
    { The timeout a disarmed server is left with: the longest there is, so the
      accept loop blocks on connections instead of waking. }
    ServerDisarmedIdleMs = High(Cardinal);

type
    { Announces, once, that a server is listening. }
    TServerReadiness = class(TObject)
    private
        FServer: TFPHTTPServer;
        FReadyPort: word;
    public
        { Arms AServer to announce to AReadyPort on its first idle. A port of 0
          means nobody asked, and nothing is armed. }
        procedure Arm(AServer: TFPHTTPServer; AReadyPort: word);
        { The idle event: announces and disarms. Public so a test can raise it
          without running an accept loop. }
        procedure Announce(Sender: TObject);
    end;

{ The exit code of `fit_server --start-detached`, given how the wait ended. }
function DetachedStartExitCode(AReadiness: TReadiness): integer;

implementation

procedure TServerReadiness.Arm(AServer: TFPHTTPServer; AReadyPort: word);
begin
    //  Nobody asked: a server started by hand is left exactly as it was.
    if (AReadyPort = 0) or not Assigned(AServer) then
        Exit;
    FServer := AServer;
    FReadyPort := AReadyPort;
    AServer.OnAcceptIdle := @Announce;
    AServer.AcceptIdleTimeout := ServerArmedIdleMs;
end;

procedure TServerReadiness.Announce(Sender: TObject);
begin
    //  The socket server raises this with ITSELF as the sender, not the HTTP
    //  server - which is why the server was remembered when it was armed.
    if not Assigned(FServer) then
        Exit;
    //  DISARMED FIRST, so however the announcement goes this runs once.
    FServer.OnAcceptIdle := nil;
    FServer.AcceptIdleTimeout := ServerDisarmedIdleMs;
    AnnounceReady(FReadyPort);
end;

function DetachedStartExitCode(AReadiness: TReadiness): integer;
begin
    if AReadiness = rdReady then
        Result := 0
    else
        Result := 1;
end;

end.
