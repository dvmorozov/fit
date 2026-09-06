// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Logging conventions for the desktop client.)

The client and the compute server are separate processes, so they must not
append to the same file: the server writes fit_server.log, the client writes
fit_client.log (see CLIENT_LOG_FILE_NAME), both in the config directory
returned by log.GetConfigDir. A bug report is then two files, each telling one
side of the story, and a wire call can be matched by its timestamp on both.

This unit adds no logging facility of its own - it routes to log.WriteLog - and
exists to fix the tiers, the same way a module's own log unit does for it:

  UI action    user-driven: menu picked, file loaded, mode entered    Notification, on
  State        what the client did as a consequence                   Debug, on at Debug
  Server call  one REST call: verb, path, duration, outcome           Debug, on at Debug
  Warning      refusals, unreachable server, recovered inconsistency  Warning, on
  Fatal        an exception that reached the user                     Fatal, on
  Trace        per-repaint / per-point inner loops                    Debug, OFF by default

The trace tier is gated by an explicit boolean (like WriteParamsLog and
a module's own writer). It is on by default together with the Debug tier: the cost
that had to be found - a repaint costing seconds - is invisible at any lower
tier, and a switch nobody passed is a switch that was off during the only run
that mattered. What the boolean still buys is the right to keep genuinely
per-point logging out of the file; log per repaint, never per point.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit client_log;

{$MODE Delphi}

interface

uses
{$IFDEF WINDOWS}
    Windows,
{$ELSE}
    BaseUnix,
{$ENDIF}
    Classes, log, SysUtils, rest_polling,
    //  EUserException: the model declining and saying why. Named here because
    //  telling a refusal from a fault is what IsRefusalRatherThanFault answers.
    MyExceptions;

const
    { The client's own log file. Never the server's - two processes appending to
      one file interleave into something unreadable. }
    CLIENT_LOG_FILE_NAME = 'fit_client.log';

var
    { Gates the per-repaint / per-point trace tier. On unless FIT_QUIET_LOG says
      otherwise - the same key that lowers log.LogLevel, so one define turns the
      whole client quiet. }
{$IFDEF FIT_QUIET_LOG}
    WriteClientTraceLog: boolean = False;
{$ELSE}
    WriteClientTraceLog: boolean = True;
{$ENDIF}

{ Rare, user-driven events: a menu item picked, a file loaded, a selection mode
  entered or left, a fit started. This tier alone must be enough to replay what
  the user did. }
procedure LogUiAction(const AMsg: string);

{ What the client did as a consequence of an action: data pushed, grids
  refilled, overlay rebuilt, a setting applied. }
procedure LogClientState(const AMsg: string);

{ One call to the compute server. ADurationMs makes a slow server visible
  without a profiler; AOutcome is 'ok' or a short failure description.

  A call to one of the polled routes is logged at Trace rather than Debug: the
  client makes those twice a second for as long as it is open, and at the
  default tier they would be the whole file. The server does the same with the
  matching incoming request, from the same rule, so the two halves of a call
  never sit at different levels. }
procedure LogServerCall(const AMethod, APath: string; ADurationMs: int64;
    const AOutcome: string);

{ Refusals, unreachable server, a state the client repaired instead of failing.
  The user may not have noticed, so the log must show it. }
procedure LogClientWarning(const AMsg: string);

{ The same, for an exception caught in an except block: adds the call stack.
  Without it a bare "EAccessViolation: Access violation" names neither the unit
  nor the operation, and the crash can only be guessed at from what the user was
  doing. The frames are addresses unless the binary is built with -gl. }
procedure LogClientFatalException(E: Exception);

{ True when the exception says the PROCESS is no longer sound, rather than that
  one operation failed: a memory fault, wherever it was raised.

  The distinction is the whole point. A refused request, an unreachable server,
  a malformed file are failures of an OPERATION - the client reports them and
  carries on. A memory fault is not: by the time it is caught, the code that
  faulted has been abandoned half-done, and on this platform that code is
  usually not ours. See EndProcessAfterFault. }
function FaultLeavesProcessUnsound(E: Exception): boolean;
{ Whether this exception is the model declining an operation and explaining why,
  rather than a defect. Such a message must never cost the state poll. }
function IsRefusalRatherThanFault(E: Exception): boolean;

{ Writes the memory map of the executable segments, so the addresses in the
  stack LogClientFatalException just logged can still be resolved afterwards.

  A trace of bare addresses is only readable while the process lives: the
  libraries sit wherever the loader put them that run. Every frame of the crash
  this was written for had to be resolved by reading /proc/<pid>/maps of the
  still-hung process by hand - which is possible exactly once, and only if
  nobody closes it. Unix only; that is where these crashes come from. }
procedure LogClientModuleMap;

{ Ends the process at once, saying on stderr why. Never returns.

  NOT Halt, and not Application.Terminate: both hand control back to code that
  goes through the widget set - unit finalization frees the forms, Terminate
  needs one more turn of the message loop - and after the fault that brings us
  here the widget set is the one thing that must not be entered again.

  That is measured, not feared. In the crash this was written for the fault was
  raised inside GTK, from inside a signal emission; unwinding out of C frames
  left GLib's signal mutex locked forever, and the error dialog opened next
  wedged on it while holding the X pointer grab - an empty window, no keyboard,
  nothing but the power button. The process must die instead.

  SIGABRT (TerminateProcess on Windows): immediate, no destructor runs, and
  where cores are enabled the fault is still there to be examined. The log is
  flushed line by line, so everything written already survives. }
procedure EndProcessAfterFault;

{ Per-repaint / per-point detail; silent unless WriteClientTraceLog is set. }
procedure LogClientTrace(const AMsg: string);

{ Directs this process's log to CLIENT_LOG_FILE_NAME. Call before the first
  logged line, i.e. at the very start of the program. }
procedure StartClientLog;

implementation

procedure StartClientLog;
begin
    SetLogFileName(CLIENT_LOG_FILE_NAME);
end;

procedure LogUiAction(const AMsg: string);
begin
    WriteLog('ui: ' + AMsg, Notification);
end;

procedure LogClientState(const AMsg: string);
begin
    WriteLog('client: ' + AMsg, Debug);
end;

procedure LogServerCall(const AMethod, APath: string; ADurationMs: int64;
    const AOutcome: string);
var
    Level: TMsgType;
begin
    if IsPolledRoute(APath) then
        Level := Trace
    else
        Level := Debug;
    WriteLog(Format('http: %s %s (%d ms) %s',
        [AMethod, APath, ADurationMs, AOutcome]), Level);
end;

procedure LogClientWarning(const AMsg: string);
begin
    WriteLog('client: ' + AMsg, Warning);
end;

procedure LogClientFatalException(E: Exception);
begin
    WriteLog(Format('client: unhandled %s: %s', [E.ClassName, E.Message]), Fatal);
    //  A separate line at the same level: a trace is worthless if the level that
    //  records the crash does not also record where it came from.
    WriteLog('client: ' + ExceptionTrace, Fatal);
end;

procedure LogClientTrace(const AMsg: string);
begin
    if WriteClientTraceLog then
        WriteLog('trace: ' + AMsg, Debug);
end;

function FaultLeavesProcessUnsound(E: Exception): boolean;
begin
    //  Named classes rather than their base: EExternal also covers the
    //  arithmetic faults (EDivByZero, EInvalidOp), which bad data can raise and
    //  which the client does recover from. Only memory is unrecoverable.
    Result := (E is EAccessViolation) or (E is EInvalidPointer) or
        (E is EExternalException);
end;

function IsRefusalRatherThanFault(E: Exception): boolean;
begin
    //  A REFUSAL IS THE MODEL DECLINING AND SAYING WHY - "this curve carries no
    //  handle, so it cannot be removed on its own" - and it arrives as
    //  EUserException, whether it was raised in this process or mapped from a
    //  400 by the HTTP service.
    //
    //  WHY THIS IS ASKED AT ALL. The client's last-resort handler treated
    //  everything alike: it logged at Fatal and STOPPED THE STATE POLL. So a
    //  refused delete left the window frozen with "Server polling has been
    //  stopped" stapled to a message that was perfectly correct, and it read as
    //  the compute server having crashed - which it had not. A caller that lets
    //  a refusal reach that handler has a gap in it, and the gap is worth
    //  closing where it is; the CONSEQUENCE must not be a dead application.
    Result := E is EUserException;
end;

{$IFDEF UNIX}
procedure LogClientModuleMap;
var
    Maps: TStringList;
    i: longint;
begin
    if not FileExists('/proc/self/maps') then
        Exit;
    Maps := TStringList.Create;
    try
        try
            Maps.LoadFromFile('/proc/self/maps');
        except
            //  A crash report without its map still beats no crash report.
            Exit;
        end;
        for i := 0 to Maps.Count - 1 do
            //  The executable segments alone: they are the only ones a return
            //  address can point into, and the rest would bury them.
            if Pos(' r-xp ', Maps[i]) > 0 then
                WriteLog('client:   map ' + Maps[i], Fatal);
    finally
        Maps.Free;
    end;
end;
{$ELSE}
procedure LogClientModuleMap;
begin
end;
{$ENDIF}

procedure EndProcessAfterFault;
const
    Reason = 'Fit: a memory fault reached the top level. It cannot be reported ' +
        'through the widget set that raised it, so this process ends here - see ' +
        'fit_client.log for the fault, its stack and the module map.';
begin
    WriteLog('client: ' + Reason, Fatal);
    try
        Writeln(ErrOutput, Reason);
        Flush(ErrOutput);
    except
        //  No stderr (started from a desktop file) changes nothing that follows.
    end;
{$IFDEF WINDOWS}
    TerminateProcess(GetCurrentProcess, 3);
{$ELSE}
    FpKill(FpGetPid, SIGABRT);
{$ENDIF}
end;

end.
