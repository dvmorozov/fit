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
    protected
        function HealthOk: boolean; override;
        function StartProcess: boolean; override;
        function WaitUntilReady(ABudgetMs: longint): TReadiness; override;
    public
        constructor Create;
        function IsConfigured: boolean; override;

        property Configured: boolean read FConfigured write FConfigured;
        property HealthyOn: longint read FHealthyOn write FHealthyOn;
        property StartSucceeds: boolean
            read FStartSucceeds write FStartSucceeds;
        property AliveFor: longint read FAliveFor write FAliveFor;
        property Readiness: TReadiness read FReadiness write FReadiness;
        property BudgetSeen: longint read FBudgetSeen;

        { What it did. }
        property Checks: longint read FChecks;
        property Starts: longint read FStarts;
        property Waits: longint read FWaits;
    end;

    TSidecarStartupTest = class(TTestCase)
    private
        FSidecar: TScriptedSidecar;
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

        //  Noticing that it died.
        procedure ADyingWorkerEndsTheWaitAndReportsNoUrl;

        //  What is handed back.
        procedure TheUrlNamesTheSidecarsOwnPort;
    end;

implementation

constructor TScriptedSidecar.Create;
begin
    inherited Create;
    FConfigured := True;
    FStartSucceeds := True;
    //  Never answers and stays alive: the arrangement that exercises the whole
    //  budget, and the one every test narrows from.
    FHealthyOn := 0;
    FAliveFor := -1;
    //  Never says it is ready: the arrangement every test narrows from.
    FReadiness := rdTimedOut;
end;

function TScriptedSidecar.IsConfigured: boolean;
begin
    Result := FConfigured;
end;

function TScriptedSidecar.HealthOk: boolean;
begin
    Inc(FChecks);
    Result := (FHealthyOn > 0) and (FChecks >= FHealthyOn);
end;

function TScriptedSidecar.StartProcess: boolean;
begin
    Inc(FStarts);
    Result := FStartSucceeds;
end;

function TScriptedSidecar.WaitUntilReady(ABudgetMs: longint): TReadiness;
begin
    Inc(FWaits);
    FBudgetSeen := ABudgetMs;
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
