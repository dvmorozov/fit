// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A write to a peer or a pipe that has gone away fails; it does not end
the process.)

THE DEFECT. On macOS and Linux, writing to a socket whose peer has reset it -
or to a pipe whose reader has gone - raises SIGPIPE, and the signal's default
action ends the process: exit 141, no exception, no log line. The compute server
ends a kept connection that sits idle, and a client may vanish while the server
is answering it; either way the next write is to a dead peer. Both sides were
killed. THttpFitService.Fetch already treats a dropped connection as ordinary,
opening another and asking again, but the process was gone before that code
ran. Found refitting FullProfile.fitproj from the private suite (findings.md).

ONE MECHANISM: EVERY PROGRAM IGNORES IT, FIRST THING IN ITS MAIN. A write that
would have raised the signal then fails with EPIPE, which the code reports or
recovers from like any other transport error. Rejected, each measured failing
or redundant:
  * a per-socket option (SO_NOSIGPIPE on macOS, MSG_NOSIGNAL on Linux) - a
    server meets a socket only after the accept, a client gone by then has
    already reset it, and macOS then refuses the option; and the listening
    socket's option is not inherited by an accepted one. On a client it worked,
    but only beside this, which covers every case - one mechanism is kept
    (decided with the user);
  * blocking SIGPIPE in the writing thread - on macOS a socket's SIGPIPE goes
    to the PROCESS, landing on whichever thread has it unblocked.

EXPLICITLY, NOT INHERITED. How a signal is handled is inherited, and the suites
run under PowerShell, whose runtime ignores SIGPIPE and passes that on - which is
what hid the defect: run from a shell, or the application from the Finder, the
same binaries were killed. tools/build-tests/network_boundary refuses a program
that does not call IgnoreBrokenPipes.
}
unit broken_pipes;

{$mode objfpc}{$H+}

interface

{ Has this process ignore SIGPIPE, so any write to a peer or a pipe that has
  gone away fails with EPIPE rather than ending it. Called first by every
  program's main. Nothing on Windows, which has no SIGPIPE. }
procedure IgnoreBrokenPipes;

implementation

{$ifdef unix}
uses
    BaseUnix;
{$endif}

procedure IgnoreBrokenPipes;
{$ifdef unix}
var
    Ignore: SigActionRec;
{$endif}
begin
{$ifdef unix}
    FillChar(Ignore, SizeOf(Ignore), 0);
    Ignore.sa_Handler := SigActionHandler(SIG_IGN);
    FPSigaction(SIGPIPE, @Ignore, nil);
{$endif}
end;

end.
