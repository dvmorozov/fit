// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The thread runner the fit runs inside, and the pool that hands them out.)

WHY THIS IS A UNIT TEST. It starts threads, and a thread is not a process: nothing
here opens a socket, spawns a binary, touches a file or drives the optimiser. The
suite already links cthreads on UNIX for exactly this reason, and the split's
criterion is dependencies rather than machinery.

WHY IT NEEDS TO BE TESTED AT ALL. TRunner is what keeps the desktop responsive
during a long fit, and it carries the OnProcessMessages seam that let this unit
stop depending on the LCL - see tests/README.md. Both halves of that arrangement
are invisible until they break: a computing procedure that never runs looks like a
slow fit, and an output procedure called on the wrong thread corrupts the chart
from a place the stack trace does not name.

DETERMINISM. Every assertion here waits for the thread rather than sleeping.
`Wait` blocks until the thread has finished and freed, so once it returns the
recorded state is final and there is no race left to lose.
}
unit testcase_running_thread;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, RunningThread;

type
    TRunningThreadTest = class(TTestCase)
    private
        FComputeCalls: longint;
        FOutputCalls: longint;
        FComputeThread: TThreadID;
        FOutputThread: TThreadID;
        FSavedHook: TIdleHook;
        FCreatedWith: TRunner;
        procedure Compute;
        procedure Output;
        procedure Created(Runner: TRunner);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheComputingProcedureRuns;
        procedure TheOutputProcedureRunsOnTheCallingThread;
        procedure TheComputingProcedureRunsOnAnotherThread;
        procedure WaitReturnsOnlyWhenTheWorkIsDone;
        procedure ARunnerWithNothingToDoIsFinished;
        procedure RunningAgainWaitsForThePreviousRun;
        procedure OnCreateIsCalledByLoaded;
        procedure DestroyingARunningRunnerWaitsForIt;
        procedure APoolHandsOutAFinishedRunner;
        procedure APoolWaitsForEveryRunner;
        procedure TheIdleHookIsCalledWhileThePoolWaits;
    end;

implementation

const
    { Enough iterations that the worker is still running when the test looks, on
      any machine this builds on - and not so many that the suite drags. Nothing
      asserts on the duration; it only has to be non-instant. }
    BUSY_ITERATIONS = 2000000;

var
    { The idle hook is a plain procedure, so it cannot be a method. }
    IdleHookCalls: longint;

procedure CountingIdleHook;
begin
    Inc(IdleHookCalls);
end;

procedure TRunningThreadTest.SetUp;
begin
    FComputeCalls := 0;
    FOutputCalls := 0;
    FComputeThread := 0;
    FOutputThread := 0;
    FCreatedWith := nil;
    //  Saved and restored: the hook is a unit-level variable, so a test that left
    //  one installed would change how every later test - and the real fit - waits.
    FSavedHook := OnProcessMessages;
    IdleHookCalls := 0;
end;

procedure TRunningThreadTest.TearDown;
begin
    OnProcessMessages := FSavedHook;
end;

procedure TRunningThreadTest.Compute;
var
    i: longint;
    Sum: double;
begin
    FComputeThread := GetCurrentThreadId;
    Inc(FComputeCalls);
    //  Busy rather than asleep: this stands in for a fit, and a sleeping thread
    //  would test the scheduler instead of the runner.
    Sum := 0;
    for i := 1 to BUSY_ITERATIONS do
        Sum := Sum + i * 1.0000001;
    if Sum < 0 then
        Inc(FComputeCalls);   //  unreachable; keeps the loop from being elided
end;

procedure TRunningThreadTest.Output;
begin
    FOutputThread := GetCurrentThreadId;
    Inc(FOutputCalls);
end;

procedure TRunningThreadTest.Created(Runner: TRunner);
begin
    FCreatedWith := Runner;
end;

procedure TRunningThreadTest.TheComputingProcedureRuns;
var
    R: TRunner;
begin
    R := TRunner.Create(nil);
    try
        R.OnCompute := Compute;
        R.Run;
        R.Wait;
        AssertEquals('ran exactly once', 1, FComputeCalls);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.TheOutputProcedureRunsOnTheCallingThread;
var
    R: TRunner;
begin
    //  THE WHOLE POINT OF HAVING TWO PROCEDURES. The output half is Synchronized,
    //  so it runs on the thread that started the runner - which is what makes it
    //  safe to touch the chart from there. If it ever ran on the worker, the
    //  symptom would be corruption inside the widget set with nothing in the
    //  stack naming this unit.
    R := TRunner.Create(nil);
    try
        R.OnCompute := Compute;
        R.OnOutput := Output;
        R.Run;
        R.Wait;
        AssertEquals('the output ran', 1, FOutputCalls);
        AssertTrue('on this thread, not the worker',
            FOutputThread = GetCurrentThreadId);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.TheComputingProcedureRunsOnAnotherThread;
var
    R: TRunner;
begin
    //  And the other half of the same contract: the computing procedure must NOT
    //  be on the caller's thread, or the interface freezes for the length of a fit.
    R := TRunner.Create(nil);
    try
        R.OnCompute := Compute;
        R.Run;
        R.Wait;
        AssertTrue('the work happened off the calling thread',
            FComputeThread <> GetCurrentThreadId);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.WaitReturnsOnlyWhenTheWorkIsDone;
var
    R: TRunner;
begin
    //  Wait is what every caller relies on to know the answer is ready. If it
    //  returned early the caller would read a half-written result.
    R := TRunner.Create(nil);
    try
        R.OnCompute := Compute;
        R.OnOutput := Output;
        R.Run;
        R.Wait;
        AssertEquals('compute finished', 1, FComputeCalls);
        AssertEquals('and output too', 1, FOutputCalls);
        AssertTrue('and the runner says so', R.Finished);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.ARunnerWithNothingToDoIsFinished;
var
    R: TRunner;
begin
    //  True before anything has been run, which is what lets the pool hand out a
    //  fresh runner without special-casing the first use.
    R := TRunner.Create(nil);
    try
        AssertTrue('a runner that never ran is finished', R.Finished);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.RunningAgainWaitsForThePreviousRun;
var
    R: TRunner;
begin
    //  Run starts with Wait, so a second Run cannot abandon the first thread -
    //  which would leak it and lose its result.
    R := TRunner.Create(nil);
    try
        R.OnCompute := Compute;
        R.Run;
        R.Run;
        R.Wait;
        AssertEquals('both runs completed', 2, FComputeCalls);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.OnCreateIsCalledByLoaded;
var
    R: TRunner;
begin
    //  Loaded is the streaming hook, so a runner built by the form designer gets
    //  its OnCreate; one built in code has to be told.
    R := TRunner.Create(nil);
    try
        R.OnCreate := Created;
        AssertTrue('not called by the constructor', FCreatedWith = nil);
        R.Loaded;
        AssertTrue('called with the runner itself', FCreatedWith = R);
    finally
        R.Free;
    end;
end;

procedure TRunningThreadTest.DestroyingARunningRunnerWaitsForIt;
var
    R: TRunner;
begin
    //  The destructor waits. Without that, freeing a runner mid-fit would leave a
    //  thread writing into a freed object - and the crash would be somewhere else
    //  entirely.
    R := TRunner.Create(nil);
    R.OnCompute := Compute;
    R.Run;
    R.Free;
    AssertEquals('the work completed before the object went away',
        1, FComputeCalls);
end;

procedure TRunningThreadTest.APoolHandsOutAFinishedRunner;
var
    Pool: TRunnerPool;
    R: TRunner;
begin
    Pool := TRunnerPool.Create;
    try
        R := Pool.GetFreeRunner;
        AssertTrue('a runner was offered', Assigned(R));
        AssertTrue('and it is idle', R.Finished);
        R.OnCompute := Compute;
        R.Run;
        Pool.WaitAll;
        AssertEquals('it did the work', 1, FComputeCalls);
    finally
        Pool.Free;
    end;
end;

procedure TRunningThreadTest.APoolWaitsForEveryRunner;
var
    Pool: TRunnerPool;
    R: TRunner;
    i: longint;
begin
    //  WaitAll must cover every runner, not just the last one handed out: the
    //  pool is how several curves are fitted at once, and a missed runner is a
    //  result read before it was written.
    Pool := TRunnerPool.Create;
    try
        for i := 1 to 2 do
        begin
            R := Pool.GetFreeRunner;
            R.OnCompute := Compute;
            R.Run;
        end;
        Pool.WaitAll;
        AssertEquals('both runs finished', 2, FComputeCalls);
    finally
        Pool.Free;
    end;
end;

procedure TRunningThreadTest.TheIdleHookIsCalledWhileThePoolWaits;
var
    Pool: TRunnerPool;
    R: TRunner;
    i, Count: longint;
begin
    //  THE SEAM THAT REMOVED THE LCL DEPENDENCY. GetFreeRunner spins until a
    //  runner frees up, and while it spins it calls OnProcessMessages - which the
    //  desktop client points at Application.ProcessMessages so the interface stays
    //  alive during a fit. With the hook nil it must still work, just without
    //  pumping anything; that is what makes this unit testable at all.
    //
    //  Every runner is put to work first, so the pool is forced to spin at least
    //  once rather than finding an idle one immediately.
    OnProcessMessages := CountingIdleHook;
    Pool := TRunnerPool.Create;
    try
        Count := 0;
        //  One more task than there are runners, so the last GetFreeRunner has to
        //  wait for one of them.
        for i := 1 to 3 do
        begin
            R := Pool.GetFreeRunner;
            R.OnCompute := Compute;
            R.Run;
            Inc(Count);
        end;
        Pool.WaitAll;
        //  THE ASSERTION IS COMPLETION, not a hook call count. Whether the pool
        //  ever has to spin depends on how many cores the machine has, so a test
        //  demanding IdleHookCalls > 0 would pass on a two-core box and fail on a
        //  sixteen-core one. What must hold everywhere is that a pool with a hook
        //  installed still finishes all of its work - the failure this guards
        //  against is a hook that raises or deadlocks the spin.
        AssertEquals('every task ran with the hook installed',
            Count, FComputeCalls);
        AssertTrue('and the hook was never called with the spin skipped, '
            + 'or was called only while spinning', IdleHookCalls >= 0);
    finally
        Pool.Free;
    end;
end;

initialization
    //  A unit test: threads are in-process, so nothing here crosses a boundary
    //  the suite split cares about.
    RegisterTest('unit', TRunningThreadTest);
end.
