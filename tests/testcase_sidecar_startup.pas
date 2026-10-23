// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Getting the Python worker up, and knowing when it never will be.)

THE SEQUENCE NOBODY COULD DRIVE. Before a fit can run on the Python engine the
worker has to be listening, and `EnsureRunning` is what makes that so: reuse one
that is already answering, refuse if there is nothing to start, start the child,
then wait for it to bind. Every one of those steps needs a Python installation, a
free port and about a second of real time - so the whole sequence was reachable
only from an integration test that skips itself when Python is absent, which is
most machines and every CI runner that matters.

WHAT IT COSTS THE USER WHEN A STEP IS WRONG.

Starting a second worker when one is already answering binds a port that is
taken, fails, and is reported as "the sidecar cannot start" - on a machine where
it is running perfectly.

Returning as soon as the child was launched hands back a URL nothing is
listening on yet: the worker imports numpy, scipy and lmfit before it binds.
The first fit then fails with a connection error, and the second works.

And not noticing that the child DIED - a missing library, a syntax error in a
module's routes - means waiting out the whole ten-second budget on every fit
before falling back to the native engine, for a worker that exited immediately.

So the four things that touch the world are seams here - one HTTP request, two
questions about a child process, and a wait - and what is driven is the decision
around them.

WHY THIS IS A UNIT TEST. Nothing is spawned, no port is opened and no second
passes. The constructor does probe the filesystem for an interpreter, but what it
finds cannot change any outcome below: `IsConfigured` is answered by the fixture,
and so is every other step.
}
unit testcase_sidecar_startup;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    python_sidecar, readiness_channel;

type
    { A sidecar whose world is a handful of counters.

      Not a mock of the class under test - it IS the class under test, with only
      the four calls that reach outside the process replaced. The start-up
      sequence being asserted is the real one. }
    TScriptedSidecar = class(TPythonSidecar)
    private
        FConfigured: boolean;
        { The check on which health first succeeds; 0 for never. }
        FHealthyOn: longint;
        FChecks: longint;
        FStartSucceeds: boolean;
        FStarts: longint;
        { How many times the child may be seen alive before it is found dead;
          -1 for "stays alive". }
        FAliveFor: longint;
        FAliveChecks: longint;
        FWaits: longint;
        { What the started child's readiness wait reports. }
        FReadiness: TReadiness;
        FBudgetSeen: longint;
        FStopSeen: boolean;
        { Something answers on the port, whatever HealthyOn says. }
        FAnswering: boolean;
        { When set, the first wait announces itself on FWaiting and blocks
          until FRelease - a start that is still going on. }
        FHoldFirstWait: boolean;
        FWaiting, FRelease: PRTLEvent;
    protected
        function HealthOk: boolean; override;
        function StartProcess: boolean; override;
        function WaitUntilReady(ABudgetMs: longint;
            AStop: TReadinessStop): TReadiness; override;
    public
        constructor Create;
        destructor Destroy; override;
        function IsConfigured: boolean; override;

        property Configured: boolean read FConfigured write FConfigured;
        property HealthyOn: longint read FHealthyOn write FHealthyOn;
        property StartSucceeds: boolean
            read FStartSucceeds write FStartSucceeds;
        property AliveFor: longint read FAliveFor write FAliveFor;
        property Readiness: TReadiness read FReadiness write FReadiness;
        property BudgetSeen: longint read FBudgetSeen;
        property StopSeen: boolean read FStopSeen;
        property Answering: boolean read FAnswering write FAnswering;
        property HoldFirstWait: boolean read FHoldFirstWait write FHoldFirstWait;
        property Waiting: PRTLEvent read FWaiting;
        property Release: PRTLEvent read FRelease;

        { What it did. }
        property Checks: longint read FChecks;
        property Starts: longint read FStarts;
        property Waits: longint read FWaits;
    end;

    { Calls EnsureRunning on a thread of its own, as a second request would. }
    TEnsureCaller = class(TThread)
    private
        FSidecar: TPythonSidecar;
        FUrl: string;
    protected
        procedure Execute; override;
    public
        constructor Create(ASidecar: TPythonSidecar);
        property Url: string read FUrl;
    end;

    TSidecarStartupTest = class(TTestCase)
    private
        FSidecar: TScriptedSidecar;
        FAsked: longint;
        { A caller that wants to stop on the third time it is asked. }
        function StopOnThirdAsk: boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Reusing what is already there.
        procedure AWorkerThatIsAlreadyAnsweringIsReused;
        procedure ItIsNotStartedASecondTime;
        procedure NothingIsWaitedForWhenItAnswersAtOnce;

        //  Nothing to start.
        procedure AnUnlocatableSidecarReportsNoUrl;
        procedure AndIsNotStarted;

        //  Starting it.
        procedure AWorkerIsStartedWhenNothingAnswers;
        procedure AStartThatFailsReportsNoUrl;
        procedure AStartThatFailsIsNotWaitedOut;

        //  Waiting for it to say it is listening.
        procedure AWorkerThatSaysItIsReadyIsUsed;
        procedure ItWaitsForTheReadySignalOnce;
        procedure AWorkerThatNeverSaysReadyReportsNoUrl;
        procedure TheBudgetIsTheOneTheUnitDeclares;
        procedure TheBudgetCoversTheMeasuredColdStart;
        procedure TheCallersStopQuestionReachesTheWait;

        //  A start outlives the wait that gave up on it.
        procedure AnAbandonedStartIsResumedNotRestarted;
        procedure ATimedOutStartIsResumedNotRestarted;
        procedure ADeadWorkerIsStartedAgain;
        procedure AStartThatLaterAnswersIsNotWaitedOnAgainOnceItDies;

        //  Two callers.
        procedure ASecondCallerDuringAStartDoesNotStartAnother;

        //  Starting ahead of need.
        procedure ABackgroundStartDoesNotWait;
        procedure AndTheNextCallerWaitsOnThatStart;
        procedure ABackgroundStartLeavesAnAnsweringWorkerAlone;

        //  Noticing that it died.
        procedure ADyingWorkerEndsTheWaitAndReportsNoUrl;

        //  What is handed back.
        procedure TheUrlNamesTheSidecarsOwnPort;
    end;

implementation

constructor TEnsureCaller.Create(ASidecar: TPythonSidecar);
begin
    FSidecar := ASidecar;
    inherited Create(False);
end;

procedure TEnsureCaller.Execute;
begin
    FUrl := FSidecar.EnsureRunning;
end;

constructor TScriptedSidecar.Create;
begin
    inherited Create;
    FWaiting := RTLEventCreate;
    FRelease := RTLEventCreate;
    FConfigured := True;
    FStartSucceeds := True;
    //  Never answers and stays alive: the arrangement that exercises the whole
    //  budget, and the one every test narrows from.
    FHealthyOn := 0;
    FAliveFor := -1;
    //  Never says it is ready: the arrangement every test narrows from.
    FReadiness := rdTimedOut;
end;

destructor TScriptedSidecar.Destroy;
begin
    RTLEventDestroy(FWaiting);
    RTLEventDestroy(FRelease);
    inherited Destroy;
end;

function TScriptedSidecar.IsConfigured: boolean;
begin
    Result := FConfigured;
end;

function TScriptedSidecar.HealthOk: boolean;
begin
    Inc(FChecks);
    Result := FAnswering or ((FHealthyOn > 0) and (FChecks >= FHealthyOn));
end;

function TScriptedSidecar.StartProcess: boolean;
begin
    Inc(FStarts);
    Result := FStartSucceeds;
end;

function TScriptedSidecar.WaitUntilReady(ABudgetMs: longint;
    AStop: TReadinessStop): TReadiness;
begin
    Inc(FWaits);
    FBudgetSeen := ABudgetMs;
    FStopSeen := Assigned(AStop);
    if FHoldFirstWait and (FWaits = 1) then
    begin
        RTLEventSetEvent(FWaiting);
        RTLEventWaitFor(FRelease, 10000);
    end;
    Result := FReadiness;
end;

{ ---- the fixture ----------------------------------------------------------- }

procedure TSidecarStartupTest.SetUp;
begin
    FSidecar := TScriptedSidecar.Create;
end;

procedure TSidecarStartupTest.TearDown;
begin
    FreeAndNil(FSidecar);
end;

{ ---- reusing what is already there ----------------------------------------- }

procedure TSidecarStartupTest.AWorkerThatIsAlreadyAnsweringIsReused;
begin
    //  STARTED BY US OR BY HAND. A developer running the worker in a terminal
    //  to watch its log is the case this exists for, and the application has to
    //  use it rather than compete with it.
    FSidecar.HealthyOn := 1;
    AssertTrue('a url came back', FSidecar.EnsureRunning <> '');
end;

procedure TSidecarStartupTest.ItIsNotStartedASecondTime;
begin
    //  A SECOND WORKER WOULD BIND A PORT THAT IS TAKEN, fail, and be reported
    //  as "the sidecar cannot start" - on a machine where it is running
    //  perfectly and answering.
    FSidecar.HealthyOn := 1;
    FSidecar.EnsureRunning;
    AssertEquals('nothing was started', 0, FSidecar.Starts);
end;

procedure TSidecarStartupTest.NothingIsWaitedForWhenItAnswersAtOnce;
begin
    //  The common case, on every fit after the first. A wait here would add a
    //  tenth of a second to each of them for nothing.
    FSidecar.HealthyOn := 1;
    FSidecar.EnsureRunning;
    AssertEquals('no wait', 0, FSidecar.Waits);
end;

{ ---- nothing to start ------------------------------------------------------ }

procedure TSidecarStartupTest.AnUnlocatableSidecarReportsNoUrl;
begin
    //  NO INTERPRETER OR NO SCRIPT - a build without the Python half, which is
    //  a supported way to run this program. An empty URL rather than an
    //  exception is what lets the native engine carry on without the caller
    //  having to know why.
    FSidecar.Configured := False;
    AssertEquals('', FSidecar.EnsureRunning);
end;

procedure TSidecarStartupTest.AndIsNotStarted;
begin
    FSidecar.Configured := False;
    FSidecar.EnsureRunning;
    AssertEquals('nothing to execute', 0, FSidecar.Starts);
end;

{ ---- starting it ----------------------------------------------------------- }

procedure TSidecarStartupTest.AWorkerIsStartedWhenNothingAnswers;
begin
    FSidecar.HealthyOn := 2;
    FSidecar.EnsureRunning;
    AssertEquals('started once', 1, FSidecar.Starts);
end;

procedure TSidecarStartupTest.AStartThatFailsReportsNoUrl;
begin
    //  A refused exec: the interpreter is named but is not executable, or the
    //  path went stale between being located and being run.
    FSidecar.StartSucceeds := False;
    AssertEquals('', FSidecar.EnsureRunning);
end;

procedure TSidecarStartupTest.AStartThatFailsIsNotWaitedOut;
begin
    //  TEN SECONDS SAVED on every fit against a broken installation. Waiting
    //  for a process that was never launched is the same mistake as waiting for
    //  one that died, and costs the same.
    FSidecar.StartSucceeds := False;
    FSidecar.EnsureRunning;
    AssertEquals('no wait at all', 0, FSidecar.Waits);
end;

{ ---- waiting for it to bind ------------------------------------------------ }

procedure TSidecarStartupTest.AWorkerThatSaysItIsReadyIsUsed;
begin
    //  THE WORKER BINDS LATE - it imports numpy, scipy and lmfit first - so the
    //  URL is handed back only once it has said, over the lifeline, that it is
    //  listening.
    FSidecar.Readiness := rdReady;
    AssertTrue('it came up', FSidecar.EnsureRunning <> '');
end;

procedure TSidecarStartupTest.ItWaitsForTheReadySignalOnce;
begin
    //  ONE WAIT ON AN EVENT, not a probe repeated a hundred times.
    FSidecar.Readiness := rdReady;
    FSidecar.EnsureRunning;
    AssertEquals('one wait', 1, FSidecar.Waits);
end;

procedure TSidecarStartupTest.AWorkerThatNeverSaysReadyReportsNoUrl;
begin
    //  Alive and never listening - a route package that imports but never
    //  finishes binding. Giving up is the only way the native engine gets a turn.
    FSidecar.Readiness := rdTimedOut;
    AssertEquals('', FSidecar.EnsureRunning);
end;

procedure TSidecarStartupTest.TheBudgetIsTheOneTheUnitDeclares;
begin
    //  PINNED TO THE CONSTANT: the pause between asking for a Python fit and
    //  being told there is no Python is one edit, and this test follows it.
    FSidecar.EnsureRunning;
    AssertEquals('the declared budget', SidecarReadyBudgetMs, FSidecar.BudgetSeen);
end;

procedure TSidecarStartupTest.TheBudgetCoversTheMeasuredColdStart;
begin
    //  THE FIRST START OF A FROZEN SIDECAR on a fresh install took 114 s on an
    //  Intel Mac, while macOS looked over each of its libraries once; the ten
    //  seconds this was refused it every time. A budget that long is bearable
    //  only because the wait is part of the run now - shown, and ended by Stop.
    AssertTrue('at least three minutes', SidecarReadyBudgetMs >= 180000);
end;

function TSidecarStartupTest.StopOnThirdAsk: boolean;
begin
    Inc(FAsked);
    Result := FAsked >= 3;
end;

procedure TSidecarStartupTest.TheCallersStopQuestionReachesTheWait;
begin
    //  The Stop the user presses is heard by the wait only if it is handed to
    //  it: a minutes-long wait that cannot be ended is what the budget above
    //  would otherwise buy.
    FSidecar.EnsureRunning(@StopOnThirdAsk);
    AssertTrue('the wait was given it', FSidecar.StopSeen);
end;

{ ---- a start outlives the wait that gave up on it -------------------------- }

procedure TSidecarStartupTest.AnAbandonedStartIsResumedNotRestarted;
begin
    //  STOP ENDS THE WAIT, NOT THE START. The child goes on importing, and the
    //  next fit waits for THAT child: killed and started again, a start longer
    //  than one patience would never finish at all.
    FSidecar.Readiness := rdAbandoned;
    AssertEquals('no url yet', '', FSidecar.EnsureRunning(@StopOnThirdAsk));
    FSidecar.Readiness := rdReady;
    AssertTrue('the next caller gets it', FSidecar.EnsureRunning <> '');
    AssertEquals('one child', 1, FSidecar.Starts);
    AssertEquals('waited on twice', 2, FSidecar.Waits);
end;

procedure TSidecarStartupTest.ATimedOutStartIsResumedNotRestarted;
begin
    //  The defect this replaced: on a machine whose first start takes two
    //  minutes, every fit killed the child the last one had started.
    FSidecar.Readiness := rdTimedOut;
    FSidecar.EnsureRunning;
    FSidecar.EnsureRunning;
    AssertEquals('one child', 1, FSidecar.Starts);
end;

procedure TSidecarStartupTest.ADeadWorkerIsStartedAgain;
begin
    //  A child that DIED is not coming back: the next caller starts another.
    FSidecar.Readiness := rdEnded;
    FSidecar.EnsureRunning;
    FSidecar.EnsureRunning;
    AssertEquals('started again', 2, FSidecar.Starts);
end;

procedure TSidecarStartupTest.AStartThatLaterAnswersIsNotWaitedOnAgainOnceItDies;
begin
    //  A start that timed out, then answered: it is running. Should it die
    //  later, its lifeline still holds the "ready" it wrote - waiting on it
    //  again would read that and hand back a URL nothing listens on.
    FSidecar.Readiness := rdTimedOut;
    FSidecar.EnsureRunning;
    FSidecar.Answering := True;
    AssertTrue('used once it answers', FSidecar.EnsureRunning <> '');
    FSidecar.Answering := False;
    FSidecar.Readiness := rdReady;
    FSidecar.EnsureRunning;
    AssertEquals('a dead worker is replaced, not waited on', 2, FSidecar.Starts);
end;

{ ---- two callers ------------------------------------------------------------ }

procedure TSidecarStartupTest.ASecondCallerDuringAStartDoesNotStartAnother;
var
    First: TEnsureCaller;
    Second: string;
begin
    //  TWO REQUESTS, ONE CHILD. Pressing Stop during the wait used to reach
    //  this object on a second thread, which started a second child - freeing
    //  the lifeline the first thread was blocked on. Now the second caller waits
    //  its turn, and gives it up when its own caller stops.
    FSidecar.HoldFirstWait := True;
    FSidecar.Readiness := rdReady;
    First := TEnsureCaller.Create(FSidecar);
    try
        //  Until the first is inside its wait, holding the start.
        RTLEventWaitFor(FSidecar.Waiting, 10000);
        AssertEquals('the first is waiting', 1, FSidecar.Waits);
        FAsked := 0;
        Second := FSidecar.EnsureRunning(@StopOnThirdAsk);
        AssertEquals('the second gave up its turn', '', Second);
        AssertEquals('and started nothing', 1, FSidecar.Starts);
        RTLEventSetEvent(FSidecar.Release);
        First.WaitFor;
        AssertTrue('the first got its worker', First.Url <> '');
    finally
        RTLEventSetEvent(FSidecar.Release);
        First.Free;
    end;
end;

{ ---- starting ahead of need ------------------------------------------------- }

procedure TSidecarStartupTest.ABackgroundStartDoesNotWait;
begin
    //  CHOOSING THE PYTHON ENGINE STARTS IT, so the minute a cold start takes
    //  is spent while the user is still loading data. Choosing must not wait.
    FSidecar.StartInBackground;
    AssertEquals('started', 1, FSidecar.Starts);
    AssertEquals('not waited for', 0, FSidecar.Waits);
end;

procedure TSidecarStartupTest.AndTheNextCallerWaitsOnThatStart;
begin
    FSidecar.StartInBackground;
    FSidecar.Readiness := rdReady;
    AssertTrue('it came up', FSidecar.EnsureRunning <> '');
    AssertEquals('the same child', 1, FSidecar.Starts);
end;

procedure TSidecarStartupTest.ABackgroundStartLeavesAnAnsweringWorkerAlone;
begin
    FSidecar.Answering := True;
    FSidecar.StartInBackground;
    AssertEquals('nothing started', 0, FSidecar.Starts);
end;

{ ---- noticing that it died ------------------------------------------------- }

procedure TSidecarStartupTest.ADyingWorkerEndsTheWaitAndReportsNoUrl;
begin
    //  A MISSING LIBRARY EXITS AT ONCE, and its end of the lifeline closes: the
    //  wait ends then, not after the budget (readiness_channel), and there is no
    //  URL to hand back.
    FSidecar.Readiness := rdEnded;
    AssertEquals('', FSidecar.EnsureRunning);
    AssertEquals('and it was one wait, not the budget', 1, FSidecar.Waits);
end;


{ ---- what is handed back --------------------------------------------------- }

procedure TSidecarStartupTest.TheUrlNamesTheSidecarsOwnPort;
begin
    //  The caller posts its fit problem to this address, so a URL naming any
    //  other port reaches either nothing or somebody else's server.
    FSidecar.HealthyOn := 1;
    AssertTrue('the port is in it',
        Pos(IntToStr(FSidecar.Port), FSidecar.EnsureRunning) > 0);
end;

initialization
    //  A unit test: counters and a loop. No process is spawned, no port is
    //  opened and no second passes - see the note at the top of the file.
    RegisterTest('unit', TSidecarStartupTest);
end.
