// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(POST /shutdown: the compute server stops when asked from this
machine, and only from this machine.)

WHY IT EXISTS. An installer replacing the program, or an update about to
install, has to stop the compute server first. Without a route for it the only
way was to kill fit_server - which also takes a server the user started by
hand, and skips the clean-up that stops the Python sidecar with it.

WHY ONLY FROM THIS MACHINE. The server listens on every interface (FPC 3.2.2's
server cannot bind one address), so a route that stopped it for anyone would
let anyone on the network stop it.
}
unit testcase_server_shutdown;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, server_shutdown,
    worker_process_harness;

type
    TServerShutdownRuleTest = class(TTestCase)
    published
        procedure AShutdownIsAPostToItsOwnPath;
        procedure ItIsTakenFromThisMachineOnly;
    end;

    TServerShutdownProcessTest = class(TWorkerProcessTest)
    published
        procedure TheServerStopsWhenAskedAndSaysSoFirst;
        procedure AskingWhenNoServerRunsIsQuietlyNothing;
        procedure WakingAServerLeavesItRunningAndNoneIsNothing;
    end;

implementation

procedure TServerShutdownRuleTest.AShutdownIsAPostToItsOwnPath;
begin
    AssertTrue(IsShutdownRequest('POST', '/shutdown'));
    AssertFalse('not a read', IsShutdownRequest('GET', '/shutdown'));
    AssertFalse(IsShutdownRequest('POST', '/shutdown/now'));
    AssertFalse(IsShutdownRequest('POST', '/problems/1/shutdown'));
end;

procedure TServerShutdownRuleTest.ItIsTakenFromThisMachineOnly;
begin
    AssertTrue(ShutdownAllowedFrom('127.0.0.1'));
    AssertTrue(ShutdownAllowedFrom('::1'));
    AssertTrue(ShutdownAllowedFrom('::ffff:127.0.0.1'));
    AssertFalse(ShutdownAllowedFrom('192.168.1.20'));
    AssertFalse(ShutdownAllowedFrom('127.0.0.1.example.org'));
    AssertFalse('nothing said is not this machine', ShutdownAllowedFrom(''));
end;

{ Through the real binary: the answer arrives, then the process ends by itself. }
procedure TServerShutdownProcessTest.TheServerStopsWhenAskedAndSaysSoFirst;
var
    Waited: integer;
begin
    AssertTrue('the server is running', FProc.Running);
    //  Asked the way the program asks before an update installs.
    AssertTrue('answered first', AskServerToStop('http://127.0.0.1:' +
        IntToStr(WorkerTestPort)));
    Waited := 0;
    while FProc.Running and (Waited < 10000) do
    begin
        Sleep(100);
        Inc(Waited, 100);
    end;
    AssertFalse('and then stopped, by itself', FProc.Running);
    AssertEquals('cleanly', 0, FProc.ExitStatus);
end;

{ An update installs whether or not a server was running: nothing to stop is
  not a failure. }
procedure TServerShutdownProcessTest.AskingWhenNoServerRunsIsQuietlyNothing;
begin
    AssertFalse(AskServerToStop('http://127.0.0.1:' + IntToStr(WorkerTestPort + 17)));
end;

{ WHAT THE SERVER DOES TO ITSELF after answering a shutdown - one request to
  its own port, to wake an accept loop blocked waiting for one - done here to a
  server that is not stopping: it answers and goes on, and a port with nobody
  on it is no error. }
procedure TServerShutdownProcessTest.WakingAServerLeavesItRunningAndNoneIsNothing;
begin
    WakeServer(WorkerTestPort);
    AssertTrue('still running', FProc.Running);
    WakeServer(WorkerTestPort + 17);
end;

initialization
    RegisterTest('unit', TServerShutdownRuleTest);
    RegisterTest('integration', TServerShutdownProcessTest);
end.
