// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Every process this program starts receives its standard streams and
nothing else of ours.)

THE DEFECT THIS PREVENTS. A child is handed a copy of every descriptor or
handle the parent has open and has not marked otherwise - listening sockets,
accepted connections, files. A copy keeps the thing alive after the parent
closes it, for as long as the child lives: a sidecar that held its parent's
listener ran for four days with no parent (readiness_channel, findings.md), and
a compute server started by a client would keep that client's sockets, and a
sidecar the server's port 8787, bound after the server itself had exited.

ONE PLACE, ONE CLASS. Every production launch creates a TChildProcess rather
than a TProcess (tools/build-tests/child_process.tests.ps1 refuses a bare
TProcess.Create outside this unit), and the decision is made in Execute, after
the caller has set its Options - so a site cannot set the options in the wrong
order, or forget.

WHAT EACH PLATFORM NEEDS, and why one property is not enough:
  * Windows. TProcess.InheritHandles is CreateProcess's bInheritHandles, and it
    is all or nothing: a child's redirected standard streams reach it ONLY by
    inheritance. So a child whose output is captured through pipes must inherit
    (ChildNeedsInheritedHandles), and gets every other inheritable handle with
    them; one that is not captured inherits nothing. Passing only the pipes
    needs PROC_THREAD_ATTRIBUTE_HANDLE_LIST, which fcl-process does not use -
    the captured children here (curl) are short and bounded by a timeout, and
    the long-lived ones (the server, the sidecar) capture nothing.
  * Linux and macOS. fcl-process 3.2.2 IGNORES InheritHandles on Unix: it forks
    and execs with every descriptor that is not close-on-exec, and the RTL marks
    none (a socket from fpSocket is inheritable). So the child closes every
    descriptor above its three standard streams itself, between the fork and
    the exec (OnForkEvent runs there, after the pipes are on 0-2). Marking each
    descriptor close-on-exec where it is made - what readiness_channel does for
    its own two - cannot reach the ones the RTL and fphttpserver open.

BETWEEN FORK AND EXEC only system calls are made: another thread of the parent
may hold a lock (the heap's) at the moment of the fork, and the child has only
the forking thread. Hence close_range on Linux, and otherwise a loop of close
up to a ceiling read in the parent beforehand.
}
unit child_process;

{$mode objfpc}{$H+}

interface

uses
    Classes, process;

type
    { A TProcess that hands the child nothing of this process but its standard
      streams. Use it wherever a TProcess would be created. }
    TChildProcess = class(TProcess)
{$IFDEF UNIX}
    private
        { One past the highest descriptor the child sweeps when the kernel
          cannot close the range at once. Read in the parent, before the fork. }
        FDescriptorCeiling: longint;
        { Runs in the child, between the fork and the exec. }
        procedure CloseInheritedDescriptors(Sender: TObject);
{$ENDIF}
    public
        { Sets what the child is handed, from the Options set by now:
          InheritHandles, and on Unix the sweep the child makes between its fork
          and its exec. Execute calls it before it starts the child; it is
          public so a test can see the decision without starting a process. }
        procedure DecideWhatTheChildInherits;
        procedure Execute; override;
    end;

const
    { THE SWEEP'S CEILING when the descriptor limit is larger or unlimited: a
      container's soft limit can be a million, and a million close calls per
      launch is a pause the user sees. A process of this program has a few
      dozen open; a descriptor above this is not one it made. }
    MaxDescriptorSwept = 65536;
    { What the sweep assumes when the limit cannot be read. }
    DefaultDescriptorCeiling = 1024;

{ Whether a child started with AOptions must inherit this process's inheritable
  handles on Windows: only when its standard streams are pipes, which reach it
  by inheritance alone. }
function ChildNeedsInheritedHandles(const AOptions: TProcessOptions): boolean;

{ One past the highest descriptor a child sweeps on Unix, from whether the soft
  limit on open descriptors could be read and what it is: that limit, capped at
  MaxDescriptorSwept, or DefaultDescriptorCeiling when it could not be read or
  leaves nothing above the standard streams. Apart from the system call so that
  every case can be tested. }
function CappedDescriptorCeiling(ALimitRead: boolean; ASoftLimit: QWord): longint;

{$IFDEF UNIX}
{ The same, for this process's own limit, read now. }
function DescriptorCeiling: longint;
{$ENDIF}

implementation

{$IFDEF UNIX}
uses
    BaseUnix{$IFDEF LINUX}, Syscall{$ENDIF};

{$IFDEF LINUX}
const
    { close_range(2), Linux 5.9: one call closes every descriptor from the first
      argument up. Its number is the same on every architecture, being past the
      point where the tables were unified; Free Pascal 3.2.2 does not name it. }
    SysCloseRange = 436;
    CloseRangeToTheEnd: LongWord = High(LongWord);
{$ENDIF}

function DescriptorCeiling: longint;
var
    Limit: TRLimit;
begin
    if FpGetRLimit(RLIMIT_NOFILE, @Limit) <> 0 then
        Exit(CappedDescriptorCeiling(False, 0));
    Result := CappedDescriptorCeiling(True, QWord(Limit.rlim_cur));
end;

procedure TChildProcess.CloseInheritedDescriptors(Sender: TObject);
var
    Fd: longint;
begin
    //  System calls only, and nothing allocated: see the unit's header.
{$IFDEF LINUX}
    if Do_SysCall(SysCloseRange, 3, TSysParam(CloseRangeToTheEnd), 0) = 0 then
        Exit;
{$ENDIF}
    for Fd := 3 to FDescriptorCeiling - 1 do
        FpClose(Fd);
end;
{$ENDIF}

function ChildNeedsInheritedHandles(const AOptions: TProcessOptions): boolean;
begin
    Result := poUsePipes in AOptions;
end;

function CappedDescriptorCeiling(ALimitRead: boolean; ASoftLimit: QWord): longint;
begin
    Result := DefaultDescriptorCeiling;
    if not ALimitRead then
        Exit;
    //  Compared as an unsigned 64-bit number: an unlimited soft limit is the
    //  largest one, and read as a longint it would be negative.
    if ASoftLimit > MaxDescriptorSwept then
        Result := MaxDescriptorSwept
    else if ASoftLimit > 3 then
        Result := longint(ASoftLimit);
end;

procedure TChildProcess.DecideWhatTheChildInherits;
begin
    InheritHandles := ChildNeedsInheritedHandles(Options);
{$IFDEF UNIX}
    //  InheritHandles means nothing to fcl-process on Unix; the child closes
    //  what it was handed instead.
    FDescriptorCeiling := DescriptorCeiling;
    OnForkEvent := @CloseInheritedDescriptors;
{$ENDIF}
end;

procedure TChildProcess.Execute;
begin
    //  Decided HERE, from the options the caller has set by now, so no site can
    //  decide it before setting them.
    DecideWhatTheChildInherits;
    inherited Execute;
end;

end.
