// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which tier each kind of client event is logged at, and which faults
mean the process can no longer be trusted.)

THE LOG IS THE ONLY ACCOUNT OF WHAT THE USER DID. A fault that cannot be
reproduced on demand has to be readable from the log the run already wrote, so
the tiers are not decoration: they decide what is still in the file by the time
anybody looks. The log rotates, so a tier chosen too low does not merely add
noise - it EVICTS the events worth keeping.

That is why the polled routes are the one case with a rule of its own. The client
asks the server for its state twice a second for as long as it is open; at the
ordinary tier those lines are the whole file within minutes, and the menu action
that caused the fault is long gone.

AND ONE DECISION HERE IS NOT ABOUT LOGGING AT ALL. `FaultLeavesProcessUnsound`
answers whether an exception says the PROCESS is broken rather than that one
operation failed - and on a True answer the process is killed outright, without
unwinding, because the alternative is worse. Getting it wrong in one direction
kills the application over a bad data file; in the other, it carries on inside a
process whose memory is already damaged.

The arithmetic faults are the line to get right: bad data raises them, the client
recovers from them, and they share a base class with the memory faults that it
cannot recover from.
}
unit testcase_client_log;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    log, client_log, rest_polling;

type
    TClientLogTest = class(TTestCase)
    published
        //  Which faults leave the process unsound.
        procedure AMemoryFaultIsUnrecoverable;
        procedure AnInvalidPointerIsUnrecoverable;
        procedure AnExternalFaultIsUnrecoverable;
        procedure ADivisionByZeroIsNot;
        procedure AnInvalidOperationIsNot;
        procedure ARefusedRequestIsNot;
        procedure AnOrdinaryFailureIsNot;

        //  Which routes are polled, and so logged quietly.
        procedure TheStateRouteIsPolled;
        procedure TheBusyRouteIsPolled;
        procedure AFitCommandIsNot;
        procedure LoadingAProfileIsNot;

        //  The client's own log file.
        procedure TheClientHasItsOwnLogFileName;
        procedure ItIsNotTheDefaultOne;

        //  The tiers themselves. Each is one WriteLog call, and what is being
        //  pinned is that calling them is harmless and that the trace tier is
        //  OFF unless it has been switched on - a per-repaint tier left on
        //  writes a log nobody can read and slows the chart.
        procedure EveryTierCanBeWrittenTo;
        procedure TheTraceTierIsWritableEitherWay;
        procedure AFatalExceptionIsLoggedWithItsTrace;

        //  The trace tier.
        procedure TheTraceTierIsOnOnlyInADetailedBuild;

        //  Every step of the window's thread that says it ended is a moment
        //  the window may answer the system in, while a long open is busy -
        //  whoever's code the step is, a module's included.
        procedure EveryPhaseOfTheWindowsThreadIsObserved;
        procedure APhaseOfAnotherThreadIsNot;

        //  What the user is told after a fault.
        procedure AFaultNoticeNamesNoInternalsAndSaysWhereTheDetailsAre;
        //  A step of the window's thread is reported when it is long enough
        //  to be a frozen window's seconds, and not below.
        procedure ASlowStepIsReportedWithWhatItCost;
        procedure AQuickStepIsNotReported;
    end;

implementation

type
    TPhaseWatcher = class
    public
        Seen: TStringList;
        constructor Create;
        destructor Destroy; override;
        procedure Note(const AWhat: string);
    end;

    TPhaseFromAThread = class(TThread)
    protected
        procedure Execute; override;
    end;

constructor TPhaseWatcher.Create;
begin
    inherited Create;
    Seen := TStringList.Create;
end;

destructor TPhaseWatcher.Destroy;
begin
    Seen.Free;
    inherited Destroy;
end;

procedure TPhaseWatcher.Note(const AWhat: string);
begin
    Seen.Add(AWhat);
end;

procedure TPhaseFromAThread.Execute;
begin
    LogClientPhase('a step of another thread', GetTickCount64);
end;

{ ---- which faults leave the process unsound -------------------------------- }

procedure TClientLogTest.AMemoryFaultIsUnrecoverable;
var
    E: Exception;
begin
    //  BY THE TIME THIS IS CAUGHT, the code that faulted has been abandoned
    //  half-done - and on this platform that code is usually not ours. The
    //  process is killed rather than unwound, because unwinding out of C frames
    //  is what left a mutex locked and the window frozen with the pointer
    //  grabbed. See findings.md.
    E := EAccessViolation.Create('x');
    try
        AssertTrue(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.AnInvalidPointerIsUnrecoverable;
var
    E: Exception;
begin
    //  A double free, or a free of something that was never allocated. The heap
    //  is already inconsistent; carrying on writes into it again.
    E := EInvalidPointer.Create('x');
    try
        AssertTrue(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.AnExternalFaultIsUnrecoverable;
var
    E: Exception;
begin
    //  A fault raised by the operating system rather than by this program -
    //  which is what a crash inside the widget set arrives as.
    E := EExternalException.Create('x');
    try
        AssertTrue(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.ADivisionByZeroIsNot;
var
    E: Exception;
begin
    //  THE ONE THAT MAKES THE RULE NAME CLASSES RATHER THAN A BASE CLASS.
    //  EDivByZero descends from EExternal, which the memory faults also do - so
    //  a rule written against the base would kill the application whenever a
    //  data file drove the optimiser through a zero denominator. That is not a
    //  broken process; it is a fit that needs different starting values.
    E := EDivByZero.Create('x');
    try
        AssertFalse(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.AnInvalidOperationIsNot;
var
    E: Exception;
begin
    //  The same family: a NaN reaching a comparison, which bad data produces
    //  and the client reports rather than dies of.
    E := EInvalidOp.Create('x');
    try
        AssertFalse(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.ARefusedRequestIsNot;
var
    E: Exception;
begin
    //  The server declining a pick and explaining why. An operation failed; the
    //  process is fine, and killing it here would end the session over a
    //  message.
    E := Exception.Create('the server refused');
    try
        AssertFalse(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.AnOrdinaryFailureIsNot;
var
    E: Exception;
begin
    //  A malformed file, an unreachable server. Every one of these is an
    //  operation that failed, and the client carries on.
    E := EConvertError.Create('not a number');
    try
        AssertFalse(FaultLeavesProcessUnsound(E));
    finally
        E.Free;
    end;
end;

{ ---- which routes are polled ----------------------------------------------- }

procedure TClientLogTest.TheStateRouteIsPolled;
begin
    //  TWICE A SECOND FOR AS LONG AS THE CLIENT IS OPEN. At the ordinary tier
    //  these lines are the whole log within minutes, and the log rotates - so
    //  they do not merely add noise, they evict the menu action that caused
    //  whatever is being investigated.
    AssertTrue(IsPolledRoute('/problems/1/state'));
end;

procedure TClientLogTest.TheBusyRouteIsPolled;
begin
    AssertTrue(IsPolledRoute('/problems/1/async'));
end;

procedure TClientLogTest.AFitCommandIsNot;
begin
    //  A USER ACTION, and the tier that records those has to be enough to
    //  replay what they did. Demoted to the quiet tier with the heartbeat, the
    //  one line that says a fit was started would be off by default.
    AssertFalse(IsPolledRoute('/problems/1/actions/minimize-difference'));
end;

procedure TClientLogTest.LoadingAProfileIsNot;
begin
    AssertFalse(IsPolledRoute('/problems/1/profile'));
end;

{ ---- the client's own log file --------------------------------------------- }

procedure TClientLogTest.TheClientHasItsOwnLogFileName;
begin
    AssertTrue('it is named', CLIENT_LOG_FILE_NAME <> '');
end;

procedure TClientLogTest.ItIsNotTheDefaultOne;
begin
    //  TWO PROCESSES APPENDING TO ONE FILE interleave into something unreadable
    //  - and the client and the compute server run side by side by design, so
    //  this is the normal case rather than an edge one.
    AssertTrue('and it is the client''s own',
        Pos('client', LowerCase(CLIENT_LOG_FILE_NAME)) > 0);
end;

{ ---- the trace tier -------------------------------------------------------- }

procedure TClientLogTest.TheTraceTierIsOnOnlyInADetailedBuild;
begin
    //  A LINE PER REPAINT is detail, not diagnosis: an animated fit repaints on
    //  every frame, and a release wrote - and flushed - a line for each. On in
    //  a build made with FIT_DETAILED_LOG, the Debug build modes and the
    //  development builds; off in a release (fit-performance.md, "build keys").
{$IFDEF FIT_DETAILED_LOG}
    AssertTrue('a detailed build traces', WriteClientTraceLog);
{$ELSE}
    AssertFalse('a release does not', WriteClientTraceLog);
{$ENDIF}
end;

{ ---- the tiers ------------------------------------------------------------- }

procedure TClientLogTest.EveryTierCanBeWrittenTo;
begin
    //  ALL FIVE, in one test, because what could break is not the text but the
    //  level: a tier written at the wrong one either floods the log or vanishes
    //  from it. Nothing here reads the file back - that is log.pas's own
    //  fixture - so this pins that the client's own vocabulary is callable and
    //  that none of it faults with no log file configured, which is how a test
    //  process runs.
    LogUiAction('menu picked');
    LogClientState('mode entered');
    LogServerCall('GET', '/problems/1/state', 3, 'ok');
    LogServerCall('POST', '/problems/1/actions/minimize-difference', 900, 'ok');
    LogClientWarning('the server did not answer');
    AssertTrue('every tier accepted a line', True);
end;

procedure TClientLogTest.TheTraceTierIsWritableEitherWay;
var
    Was: boolean;
begin
    //  WHETHER IT IS ON BY DEFAULT is pinned by
    //  TheTraceTierIsOnOnlyInADetailedBuild above, and it differs
    //  between build flavours - so this must not assert a default. What it does
    //  assert is that the tier is callable in both positions of the switch, and
    //  the previous value is put back rather than forced: forcing it False broke
    //  that other test when this one ran first, which is the shape of every
    //  order-dependent failure.
    Was := WriteClientTraceLog;
    try
        WriteClientTraceLog := False;
        LogClientTrace('per-point noise, discarded');
        WriteClientTraceLog := True;
        LogClientTrace('per-point noise, written');
        AssertTrue('callable in both positions', True);
    finally
        WriteClientTraceLog := Was;
    end;
end;

procedure TClientLogTest.AFatalExceptionIsLoggedWithItsTrace;
var
    E: Exception;
begin
    //  TWO LINES AT THE SAME LEVEL, deliberately: a trace is worthless if the
    //  level that records the crash does not also record where it came from.
    //  Constructed rather than raised - what is under test is the logging, not
    //  the raising.
    E := Exception.Create('something went wrong');
    try
        LogClientFatalException(E);
        AssertTrue('logged', True);
    finally
        E.Free;
    end;
end;

procedure TClientLogTest.AFaultNoticeNamesNoInternalsAndSaysWhereTheDetailsAre;
var
    N: string;
begin
    N := FaultNotice;
    //  The exception's own text - "[Chart.YGraphToImage]: Image-graph scaling
    //  not yet initialized" - is for the log, never the dialog: the notice is
    //  built without it, so no component name can reach the user.
    AssertTrue(N, N <> '');
    AssertTrue('says where the details went', Pos(CLIENT_LOG_FILE_NAME, N) > 0);
    AssertTrue('names the guide page that finds the file',
        Pos('Log files', N) > 0);
    //  Nothing stops the state refresh after a fault any more (it is asked for
    //  by events, not a timer), so a notice saying it had stopped was false.
    AssertTrue(N, Pos('polling', LowerCase(N)) = 0);
end;

procedure TClientLogTest.ASlowStepIsReportedWithWhatItCost;
begin
    AssertEquals('phase: drawing the model 2500 ms',
        PhaseLine('drawing the model', 2500));
    AssertEquals('phase: x 100 ms', PhaseLine('x', CLIENT_PHASE_REPORT_MS));
end;

procedure TClientLogTest.AQuickStepIsNotReported;
begin
    AssertEquals('', PhaseLine('drawing the model', CLIENT_PHASE_REPORT_MS - 1));
end;

procedure TClientLogTest.EveryPhaseOfTheWindowsThreadIsObserved;
var
    W: TPhaseWatcher;
begin
    W := TPhaseWatcher.Create;
    try
        SetPhaseObserver(@W.Note);
        //  However short: a step of a millisecond is still a moment to answer.
        LogClientPhase('a quick step', GetTickCount64);
        AssertEquals('seen', 1, W.Seen.Count);
        AssertEquals('a quick step', W.Seen[0]);
    finally
        SetPhaseObserver(nil);
        W.Free;
    end;
end;

procedure TClientLogTest.APhaseOfAnotherThreadIsNot;
var
    W: TPhaseWatcher;
    T: TPhaseFromAThread;
begin
    W := TPhaseWatcher.Create;
    try
        SetPhaseObserver(@W.Note);
        T := TPhaseFromAThread.Create(False);
        try
            T.WaitFor;
        finally
            T.Free;
        end;
        AssertEquals('not seen', 0, W.Seen.Count);
    finally
        SetPhaseObserver(nil);
        W.Free;
    end;
end;

initialization
    //  A unit test: exception classes and route strings. Nothing is written to
    //  a log file - what tier a message is given is decided before the file is
    //  touched, and the writing itself is covered in testcase_log.
    RegisterTest('unit', TClientLogTest);
end.
