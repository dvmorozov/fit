// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(fit_server says it is listening once, and then gets out of the way.)

No accept loop is run here: the idle event is raised directly, which is exactly
what the server's loop does the first time nothing connects within the armed
timeout. What must hold is that it announces, that it disarms - so the loop blocks
on connections again instead of waking every millisecond - and that a server
nobody asked about is left exactly as it was.
}
unit testcase_server_readiness;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fphttpserver,
    readiness_channel, server_readiness;

type
    TServerReadinessTest = class(TTestCase)
    private
        FServer: TFPHTTPServer;
        FReadiness: TServerReadiness;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AServerNobodyAskedAboutIsLeftAlone;
        procedure AnArmedServerIsWokenByItsFirstIdle;
        procedure ItsFirstIdleSaysItIsReady;
        procedure AndThenItStopsWaking;
        procedure ADetachedStartSucceedsOnlyWhenTheServerSaidReady;
    end;

implementation

procedure TServerReadinessTest.SetUp;
begin
    FServer := TFPHTTPServer.Create(nil);
    FReadiness := TServerReadiness.Create;
end;

procedure TServerReadinessTest.TearDown;
begin
    FreeAndNil(FReadiness);
    FreeAndNil(FServer);
end;

procedure TServerReadinessTest.AServerNobodyAskedAboutIsLeftAlone;
begin
    //  Started by hand, or by a tool that does not wait: no --ready-port.
    FReadiness.Arm(FServer, 0);
    AssertEquals('no idle timeout', 0, FServer.AcceptIdleTimeout);
    AssertFalse('no announcer', Assigned(FServer.OnAcceptIdle));
end;

procedure TServerReadinessTest.AnArmedServerIsWokenByItsFirstIdle;
begin
    FReadiness.Arm(FServer, 4242);
    AssertEquals('woken at once', ServerArmedIdleMs, FServer.AcceptIdleTimeout);
    AssertTrue('by the announcer', Assigned(FServer.OnAcceptIdle));
end;

procedure TServerReadinessTest.ItsFirstIdleSaysItIsReady;
var
    Parent: TReadinessListener;
begin
    Parent := TReadinessListener.Create;
    try
        FReadiness.Arm(FServer, Parent.Port);
        FReadiness.Announce(FServer);
        AssertTrue('the parent heard it', Parent.WaitForReady(5000) = rdReady);
    finally
        Parent.Free;
    end;
end;

procedure TServerReadinessTest.AndThenItStopsWaking;
var
    Parent: TReadinessListener;
begin
    //  Back to zero would spin: the accept loop re-reads the timeout on every
    //  pass. "Never" is the longest timeout there is.
    Parent := TReadinessListener.Create;
    try
        FReadiness.Arm(FServer, Parent.Port);
        FReadiness.Announce(FServer);
        AssertFalse('the announcer is gone', Assigned(FServer.OnAcceptIdle));
        AssertEquals('and the loop blocks on connections',
            int64(ServerDisarmedIdleMs), int64(FServer.AcceptIdleTimeout));
    finally
        Parent.Free;
    end;
end;

procedure TServerReadinessTest.ADetachedStartSucceedsOnlyWhenTheServerSaidReady;
begin
    //  The shell launchers run `fit_server --start-detached` and read its exit
    //  code: only a server that said it was listening is a started server.
    AssertEquals('ready', 0, DetachedStartExitCode(rdReady));
    AssertEquals('died', 1, DetachedStartExitCode(rdEnded));
    AssertEquals('never ready', 1, DetachedStartExitCode(rdTimedOut));
end;

initialization
    RegisterTest('unit', TServerReadinessTest);
end.
