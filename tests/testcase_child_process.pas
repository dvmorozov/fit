// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A process this program starts holds nothing of ours but its standard
streams.)

The decisions - over the options, and over how far the child sweeps - are unit
tests: they start nothing. The rest starts a real process
while a listening socket is open, closes the socket, and asks the port: a child
holding a copy keeps it listening, and a connection then succeeds where it must
be refused. That is the defect itself - a port still bound after its owner let
go - so these are integration tests.
}
unit testcase_child_process;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, process, Sockets, ssockets,
    {$IFDEF UNIX}BaseUnix,{$ENDIF} child_process;

type
    TChildHandlesTest = class(TTestCase)
    published
        procedure OnlyAChildWhoseStreamsArePipesInheritsHandles;
        procedure TheLaunchDecidesFromTheOptionsSetBeforeIt;
        procedure TheSweepStopsAtTheLimitButNeverPastTheCap;
        procedure AnUnreadableOrEmptyLimitSweepsTheDefault;
{$IFDEF UNIX}
        procedure TheSweepIsReadFromThisProcesssOwnLimit;
{$ENDIF}
    end;

    TChildInheritanceTest = class(TTestCase)
    private
        FChild: TChildProcess;
        FListener: Sockets.TSocket;
        FPort: word;
        { A loopback listener the way the RTL makes one: nothing marks it
          close-on-exec or non-inheritable. }
        procedure OpenListener;
        { A process that does nothing for a while, started while the listener is
          open, as the next sidecar or server would be. }
        procedure StartChild(const AOptions: TProcessOptions);
        { Whether something keeps accepting connections on FPort for two
          seconds. Not one probe: the child closes what it was handed between
          its fork and its exec, and a probe made in the microseconds before
          that is queued on the copy it still holds. A copy that is kept keeps
          accepting for as long as the child lives. }
        function PortStillListening: boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AListenerTheParentClosedIsHeldByNoChild;
{$IFDEF UNIX}
        { On Windows a captured child inherits by necessity (child_process); on
          Unix the child closes what it was handed whatever its options. }
        procedure AChildWhoseOutputIsCapturedHoldsNoListenerEither;
{$ENDIF}
    end;

implementation

const
    //  Named by its unit, as every socket type is here (testcase_winsock_types).
    NoSocket = Sockets.TSocket(-1);

type
    { A child that is decided for and never started: Launch is where the
      process would start, so what reaches it is what the child would be
      handed. }
    TUnstartedChild = class(TChildProcess)
    public
        Launched: boolean;
        InheritedAtLaunch: boolean;
        SweepsAtLaunch: boolean;
    protected
        procedure Launch; override;
    end;

procedure TUnstartedChild.Launch;
begin
    Launched := True;
    InheritedAtLaunch := InheritHandles;
{$IFDEF UNIX}
    SweepsAtLaunch := Assigned(OnForkEvent);
{$ENDIF}
end;

procedure TChildHandlesTest.OnlyAChildWhoseStreamsArePipesInheritsHandles;
begin
    AssertFalse('a child with nothing captured', ChildNeedsInheritedHandles([]));
    AssertFalse('a child told to have no console',
        ChildNeedsInheritedHandles([poNoConsole]));
    AssertFalse('a child waited for', ChildNeedsInheritedHandles([poWaitOnExit, poNoConsole]));
    AssertTrue('a child whose streams are pipes',
        ChildNeedsInheritedHandles([poUsePipes]));
    AssertTrue('a captured child with no console',
        ChildNeedsInheritedHandles([poUsePipes, poNoConsole]));
end;

procedure TChildHandlesTest.TheLaunchDecidesFromTheOptionsSetBeforeIt;
var
    Child: TUnstartedChild;
begin
    //  THROUGH Execute, which every launch calls: the decision is made there,
    //  after the caller set its options, and a test calling the decision
    //  beside it could not see it taken out.
    Child := TUnstartedChild.Create(nil);
    try
        //  The opposite of what each decision must reach, so a decision not
        //  made leaves it visibly wrong.
        Child.InheritHandles := True;
        Child.Options := [poNoConsole];
        Child.Execute;
        AssertTrue('launched', Child.Launched);
        AssertFalse('a child with nothing captured inherits nothing',
            Child.InheritedAtLaunch);
        Child.InheritHandles := False;
        Child.Options := [poUsePipes];
        Child.Execute;
        AssertTrue('a captured child inherits its pipes',
            Child.InheritedAtLaunch);
{$IFDEF UNIX}
        AssertTrue('on Unix the child closes what it was handed, between its ' +
            'fork and its exec', Child.SweepsAtLaunch);
{$ENDIF}
    finally
        Child.Free;
    end;
end;

procedure TChildHandlesTest.TheSweepStopsAtTheLimitButNeverPastTheCap;
begin
    AssertEquals('an ordinary limit is swept to', 1024,
        CappedDescriptorCeiling(True, 1024));
    AssertEquals('a small one too', 4, CappedDescriptorCeiling(True, 4));
    AssertEquals('the cap itself', MaxDescriptorSwept,
        CappedDescriptorCeiling(True, MaxDescriptorSwept));
    AssertEquals('a container''s million is capped', MaxDescriptorSwept,
        CappedDescriptorCeiling(True, 1048576));
    AssertEquals('an unlimited one is capped, not wrapped negative',
        MaxDescriptorSwept, CappedDescriptorCeiling(True, High(QWord)));
end;

procedure TChildHandlesTest.AnUnreadableOrEmptyLimitSweepsTheDefault;
begin
    AssertEquals('a limit that could not be read', DefaultDescriptorCeiling,
        CappedDescriptorCeiling(False, 4096));
    AssertEquals('a limit that leaves nothing above the standard streams',
        DefaultDescriptorCeiling, CappedDescriptorCeiling(True, 3));
    AssertEquals('a zero limit', DefaultDescriptorCeiling,
        CappedDescriptorCeiling(True, 0));
end;

{$IFDEF UNIX}
procedure TChildHandlesTest.TheSweepIsReadFromThisProcesssOwnLimit;
var
    Limit: TRLimit;
begin
    AssertEquals('the limit is readable here', 0,
        FpGetRLimit(RLIMIT_NOFILE, @Limit));
    AssertEquals('what the launch sweeps is this process''s limit, capped',
        CappedDescriptorCeiling(True, QWord(Limit.rlim_cur)), DescriptorCeiling);
end;
{$ENDIF}

procedure TChildInheritanceTest.SetUp;
begin
    FChild := nil;
    FListener := NoSocket;
    FPort := 0;
end;

procedure TChildInheritanceTest.TearDown;
begin
    if Assigned(FChild) then
    begin
        if FChild.Running then
            FChild.Terminate(0);
        FreeAndNil(FChild);
    end;
    if FListener <> NoSocket then
        CloseSocket(FListener);
end;

procedure TChildInheritanceTest.OpenListener;
var
    Addr: TInetSockAddr;
    Len: TSockLen;
begin
    FListener := fpSocket(AF_INET, SOCK_STREAM, 0);
    AssertTrue('a socket', FListener <> NoSocket);
    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_family := AF_INET;
    Addr.sin_port := 0;
    Addr.sin_addr := StrToNetAddr('127.0.0.1');
    AssertEquals('bound', 0, fpBind(FListener, @Addr, SizeOf(Addr)));
    AssertEquals('listening', 0, fpListen(FListener, 4));
    Len := SizeOf(Addr);
    AssertEquals('its port', 0, fpGetSockName(FListener, @Addr, @Len));
    FPort := NToHs(Addr.sin_port);
end;

procedure TChildInheritanceTest.StartChild(const AOptions: TProcessOptions);
begin
    FChild := TChildProcess.Create(nil);
{$IFDEF WINDOWS}
    FChild.Executable := 'cmd.exe';
    FChild.Parameters.Add('/c');
    FChild.Parameters.Add('ping -n 11 127.0.0.1 >nul');
{$ELSE}
    FChild.Executable := '/bin/sleep';
    FChild.Parameters.Add('10');
{$ENDIF}
    FChild.Options := AOptions;
    FChild.Execute;
end;

function TChildInheritanceTest.PortStillListening: boolean;
var
    Probe: TInetSocket;
    Deadline: QWord;
begin
    Deadline := GetTickCount64 + 2000;
    repeat
        try
            Probe := TInetSocket.Create('127.0.0.1', FPort);
            Probe.Free;
        except
            Exit(False);
        end;
        Sleep(50);
    until GetTickCount64 >= Deadline;
    Result := True;
end;

procedure TChildInheritanceTest.AListenerTheParentClosedIsHeldByNoChild;
begin
    OpenListener;
    StartChild([]);
    CloseSocket(FListener);
    FListener := NoSocket;
    AssertTrue('the child is still running, so it could be holding the port',
        FChild.Running);
    AssertFalse('nothing listens on the port once the parent closed it',
        PortStillListening);
end;

{$IFDEF UNIX}
procedure TChildInheritanceTest.AChildWhoseOutputIsCapturedHoldsNoListenerEither;
begin
    OpenListener;
    StartChild([poUsePipes]);
    CloseSocket(FListener);
    FListener := NoSocket;
    AssertTrue('the child is still running', FChild.Running);
    AssertFalse('nothing listens on the port once the parent closed it',
        PortStillListening);
end;
{$ENDIF}

initialization
    RegisterTest('unit', TChildHandlesTest);
    RegisterTest('integration', TChildInheritanceTest);
end.
