// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The pool the fit intervals run on, as a pool: jobs, workers, order,
failures.)

FIT INTERVALS ARE INDEPENDENT BY CONSTRUCTION, each its own task over its own
stretch, so they can be fitted side by side - on a BOUNDED pool, not a thread
each, because more threads than processors only take turns
(docs/internal/fit-performance.md, stage 8). What the pool promises is tested
here on plain jobs, with no fit in sight: every job runs exactly once, the
longest first, the work really is concurrent, a failure is reported after the
others finished, and each worker computes as the caller would - the same FPU
state, which is what keeps a parallel fit bit-identical to a sequential one.
}
unit testcase_job_pool;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, job_pool;

type
    TJobPoolTest = class(TTestCase)
    private
        FRuns: array of longint;
        FOrder: array of longint;
        FOrderCount: longint;
        FArrived: longint;
        FMet: longint;
        FMasks: array of TFPUExceptionMask;
        FThreads: array of TThreadID;
        procedure CountRun(AIndex: longint);
        procedure RecordOrder(AIndex: longint);
        procedure WaitForAnother(AIndex: longint);
        procedure FailOnTwo(AIndex: longint);
        procedure RecordMask(AIndex: longint);
    published
        procedure EveryJobRunsExactlyOnce;
        procedure TheLongestJobsAreTakenFirst;
        procedure TheJobsReallyRunSideBySide;
        procedure AFailureIsRaisedOnceTheOthersHaveFinished;
        procedure EachWorkerComputesWithTheCallersFloatingPointState;
        procedure AtMostOneWorkerPerJob;
        procedure TheMachineHasAtLeastOneProcessor;
        procedure EachWorkerThreadCleansUpAsItEnds;
    end;

implementation

procedure TJobPoolTest.CountRun(AIndex: longint);
begin
    InterLockedIncrement(FRuns[AIndex]);
end;

procedure TJobPoolTest.RecordOrder(AIndex: longint);
begin
    FOrder[FOrderCount] := AIndex;
    Inc(FOrderCount);
end;

procedure TJobPoolTest.WaitForAnother(AIndex: longint);
var
    Deadline: QWord;
begin
    //  A BARRIER OF TWO: each job waits until another one is running too. Run
    //  one after another, the first would wait for ever - here, until the
    //  deadline - so meeting proves two ran at once.
    InterLockedIncrement(FArrived);
    Deadline := GetTickCount64 + 2000;
    while (FArrived < 2) and (GetTickCount64 < Deadline) do
        Sleep(1);
    //  COUNTED, not flagged: run one after another, the second job finds the
    //  first already arrived and would meet it, so one meeting proves nothing.
    //  Both meeting does.
    if FArrived >= 2 then
        InterLockedIncrement(FMet);
end;

procedure TJobPoolTest.FailOnTwo(AIndex: longint);
begin
    InterLockedIncrement(FRuns[AIndex]);
    if AIndex = 2 then
        raise Exception.Create('job two failed');
end;

procedure TJobPoolTest.RecordMask(AIndex: longint);
begin
    FMasks[AIndex] := GetExceptionMask;
    FThreads[AIndex] := GetCurrentThreadId;
    //  Long enough that the workers take some of the jobs: a pool whose
    //  calling thread did everything would prove nothing about them.
    Sleep(20);
end;

procedure TJobPoolTest.EveryJobRunsExactlyOnce;
var
    i: longint;
begin
    SetLength(FRuns, 50);
    RunJobs(50, 4, [], @CountRun);
    for i := 0 to 49 do
        AssertEquals(Format('job %d', [i]), 1, FRuns[i]);
end;

procedure TJobPoolTest.TheLongestJobsAreTakenFirst;
begin
    //  One worker, so the order taken is the order run.
    SetLength(FOrder, 4);
    FOrderCount := 0;
    RunJobs(4, 1, [1.0, 9.0, 3.0, 5.0], @RecordOrder);
    AssertEquals('the longest', 1, FOrder[0]);
    AssertEquals(3, FOrder[1]);
    AssertEquals(2, FOrder[2]);
    AssertEquals('the shortest last', 0, FOrder[3]);
end;

procedure TJobPoolTest.TheJobsReallyRunSideBySide;
begin
    FArrived := 0;
    FMet := 0;
    RunJobs(2, 2, [], @WaitForAnother);
    AssertEquals('two jobs were running at once', 2, FMet);
end;

procedure TJobPoolTest.AFailureIsRaisedOnceTheOthersHaveFinished;
var
    i: longint;
    Raised: string;
begin
    SetLength(FRuns, 8);
    Raised := '';
    try
        RunJobs(8, 3, [], @FailOnTwo);
    except
        on E: Exception do
            Raised := E.Message;
    end;
    AssertEquals('the failure reaches the caller', 'job two failed', Raised);
    for i := 0 to 7 do
        AssertEquals(Format('and job %d still ran', [i]), 1, FRuns[i]);
end;

{ THE OUTCOME, not the mechanism. On FPC 3.2 SetExceptionMask also sets the
  default every new thread starts from, so a worker would get the caller's mask
  even without the pool's own copy - which is why removing that copy does not
  fail this test here. The copy stays: it is what the guarantee rests on where
  the RTL does not do this, and this test is what says the guarantee holds. }
procedure TJobPoolTest.EachWorkerComputesWithTheCallersFloatingPointState;
var
    Saved, Wanted: TFPUExceptionMask;
    i, Others: longint;
begin
    Saved := GetExceptionMask;
    //  Something no new thread starts with.
    Wanted := [exInvalidOp, exPrecision];
    SetExceptionMask(Wanted);
    //  ONLY WHERE THE MACHINE KEEPS IT. Valgrind - which the coverage run is
    //  measured under - emulates the SSE control register and drops a mask set
    //  this way even on the CALLING thread, so the test failed there on job 0
    //  with no worker involved, and passed in every plain run. A probe built
    //  in the coverage image showed it: kept natively, not kept under
    //  callgrind. Nothing about workers can be said where the caller's own
    //  state does not stick, so the test says so instead of failing.
    if GetExceptionMask <> Wanted then
    begin
        SetExceptionMask(Saved);
        Ignore('this machine does not keep a floating-point exception mask ' +
            'even on the calling thread (valgrind emulates it away), so ' +
            'what the workers inherit cannot be judged here');
    end;
    try
        SetLength(FMasks, 8);
        SetLength(FThreads, 8);
        RunJobs(8, 4, [], @RecordMask);
    finally
        SetExceptionMask(Saved);
    end;
    Others := 0;
    for i := 0 to 7 do
        if FThreads[i] <> GetCurrentThreadId then
            Inc(Others);
    AssertTrue('some jobs ran on the workers', Others > 0);
    for i := 0 to 7 do
        AssertTrue(Format('job %d ran with the caller''s mask', [i]),
            FMasks[i] = Wanted);
end;

procedure TJobPoolTest.AtMostOneWorkerPerJob;
begin
    AssertEquals('three jobs, three workers', 3, WorkersFor(3, 8));
    AssertEquals('as many as asked for', 2, WorkersFor(5, 2));
    AssertEquals('one job, one worker', 1, WorkersFor(1, 8));
    AssertEquals('none asked: one', 1, WorkersFor(4, 0));
end;

procedure TJobPoolTest.TheMachineHasAtLeastOneProcessor;
begin
    AssertTrue(LogicalProcessorCount >= 1);
end;

var
    GCleanups: longint;

procedure CountCleanup;
begin
    InterLockedIncrement(GCleanups);
end;

{ WHAT A WORKER MADE FOR ITSELF IT FREES as it ends - a thread's own formula
  parser (native_math_expr), which would otherwise outlive the thread. Only the
  workers the pool made: the calling thread keeps its own. }
procedure TJobPoolTest.EachWorkerThreadCleansUpAsItEnds;
begin
    GCleanups := 0;
    SetLength(FRuns, 6);
    RunJobs(6, 3, [], @CountRun, @CountCleanup);
    AssertEquals('one cleanup per worker the pool made', 2, GCleanups);
end;

initialization
    RegisterTest('unit', TJobPoolTest);
end.
