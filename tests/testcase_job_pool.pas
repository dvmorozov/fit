// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The pool the fit intervals run on, as a pool: jobs, workers, order,
failures.)

FIT INTERVALS ARE INDEPENDENT BY CONSTRUCTION, each its own task over its own
stretch, so they can be fitted side by side - on a BOUNDED pool, not a thread
each, because more threads than processors only take turns
(docs/internal/fit-performance.md, "parallel fit intervals"). What the pool promises is tested
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
        FPriorities: array of TThreadPriority;
        FIdleAtTheTail: longint;
        FInnerWorkers: array of longint;
        FRunning, FPeak: longint;
        procedure CountRun(AIndex: longint);
        procedure RecordOrder(AIndex: longint);
        procedure WaitForAnother(AIndex: longint);
        procedure FailOnTwo(AIndex: longint);
        procedure RecordMask(AIndex: longint);
        procedure RecordPriority(AIndex: longint);
        procedure QuickThenOneLong(AIndex: longint);
        procedure OuterJobWaitsForAllThenRunsInner(AIndex: longint);
        procedure OuterJobRunsInner(AIndex: longint);
        procedure InnerJobCountsConcurrency(AIndex: longint);
        procedure FailOnThree(AIndex: longint);
        procedure RecordThread(AIndex: longint);
    published
        procedure EveryJobRunsExactlyOnce;
        procedure TheLongestJobsAreTakenFirst;
        procedure TheJobsReallyRunSideBySide;
        procedure AFailureIsRaisedOnceTheOthersHaveFinished;
        procedure EachWorkerComputesWithTheCallersFloatingPointState;
        procedure WorkersRunBelowNormalSoTheWindowStaysResponsive;
        procedure AtMostOneWorkerPerJob;
        procedure TheMachineHasAtLeastOneProcessor;
        procedure AKeptWorkerCleansUpWhenItsThreadEndsNotBetweenRuns;
        procedure AllButTheCallerAreIdleAtRest;
        procedure AWorkerWithNothingLeftToDoGivesItsProcessorBack;
        procedure JobsOnIdleProcessorsEachRunExactlyOnce;
        procedure WithNoProcessorIdleAnInnerRunIsTheCallingThreadAlone;
        procedure InnerRunsNeverPutMoreThreadsThanProcessorsToWork;
        procedure AFailedInnerRunGivesItsThreadsBack;
        procedure WorkerThreadsAreKeptAndReusedAcrossRuns;
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

procedure TJobPoolTest.RecordPriority(AIndex: longint);
begin
    FThreads[AIndex] := GetCurrentThreadId;
    FPriorities[AIndex] := TThread.CurrentThread.Priority;
    Sleep(20);
end;

{ A pool that fills every processor starved the server's request threads and the
  window: a progress poll took 2 to 10 s while a fit ran. The thread priority
  is the platform's to honour - Windows does; an unprivileged Linux process
  cannot lower it, and reads it back as normal - so only the machine this was
  found on can check it. }
procedure TJobPoolTest.WorkersRunBelowNormalSoTheWindowStaysResponsive;
{$IFDEF MSWINDOWS}
var
    i, Others: longint;
begin
    SetLength(FThreads, 8);
    SetLength(FPriorities, 8);
    RunJobs(8, 4, [], @RecordPriority);
    Others := 0;
    for i := 0 to 7 do
        if FThreads[i] <> GetCurrentThreadId then
        begin
            Inc(Others);
            AssertTrue(Format('job %d on a worker runs below normal', [i]),
                FPriorities[i] = tpLower);
        end;
    AssertTrue('some jobs ran on the workers', Others > 0);
end;
{$ELSE}
begin
    Ignore('thread priority is only observable on Windows, where the window ' +
        'that stopped responding runs');
end;
{$ENDIF}

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

{ WHAT A WORKER MADE FOR ITSELF IS FREED WHEN ITS THREAD ENDS - a thread's own
  formula parser (native_math_expr), which would otherwise outlive the thread.
  Not after each run: the thread is kept, and so is the parser it evaluates
  with. Only the workers the pool made: the calling thread keeps its own. }
procedure TJobPoolTest.AKeptWorkerCleansUpWhenItsThreadEndsNotBetweenRuns;
begin
    //  No workers kept from the tests before, so only these are counted.
    StopPoolWorkers;
    GCleanups := 0;
    SetLength(FRuns, 6);
    RunJobs(6, 3, [], @CountRun, @CountCleanup);
    RunJobs(6, 3, [], @CountRun, @CountCleanup);
    AssertEquals('no cleanup while the workers are kept', 0, GCleanups);
    StopPoolWorkers;
    AssertEquals('one cleanup per worker, as its thread ended', 2, GCleanups);
end;

procedure TJobPoolTest.QuickThenOneLong(AIndex: longint);
begin
    if AIndex <> 0 then
        Exit;
    //  THE LONG TAIL: every other job is over, and the workers that ran them
    //  have nothing left to claim. Read while this one runs on.
    Sleep(300);
    FIdleAtTheTail := IdleProcessorCount;
end;

{ A step of 35 fit intervals on 24 workers ended with a handful of giant
  intervals running, and the processors the finished workers left were never
  offered to the evaluation of a curve - the count held every worker until the
  whole step was joined - so six intervals used seven processors of 24. }
procedure TJobPoolTest.AWorkerWithNothingLeftToDoGivesItsProcessorBack;
begin
    if LogicalProcessorCount < 4 then
        Ignore('needs at least four processors to tell workers from the caller');
    //  Costliest first: job 0 is the long one, claimed first by one worker.
    RunJobs(8, 8, [100, 1, 1, 1, 1, 1, 1, 1], @QuickThenOneLong);
    //  Only the job's own thread (and perhaps the caller) is still busy.
    AssertTrue(Format('%d idle of %d processors at the tail', [FIdleAtTheTail,
        LogicalProcessorCount]), FIdleAtTheTail >= LogicalProcessorCount - 3);
end;

procedure TJobPoolTest.AllButTheCallerAreIdleAtRest;
begin
    AssertEquals(LogicalProcessorCount - 1, IdleProcessorCount);
end;

procedure TJobPoolTest.JobsOnIdleProcessorsEachRunExactlyOnce;
var
    i, Workers: longint;
begin
    SetLength(FRuns, 40);
    Workers := RunJobsOnIdleProcessors(40, 8, @CountRun);
    for i := 0 to 39 do
        AssertEquals(Format('job %d', [i]), 1, FRuns[i]);
    AssertTrue('never more than asked for or than there are processors',
        (Workers >= 1) and (Workers <= Min(8, LogicalProcessorCount)));
    AssertEquals('and the threads are given back', LogicalProcessorCount - 1,
        IdleProcessorCount);
end;

procedure TJobPoolTest.OuterJobWaitsForAllThenRunsInner(AIndex: longint);
var
    Deadline: QWord;
begin
    //  Every outer worker is running before any asks, so the processors that
    //  RunJobs was asked for are all taken.
    InterLockedIncrement(FArrived);
    Deadline := GetTickCount64 + 5000;
    while (FArrived < Length(FInnerWorkers)) and (GetTickCount64 < Deadline) do
        Sleep(1);
    FInnerWorkers[AIndex] := RunJobsOnIdleProcessors(4, 64, @CountRun);
    //  AND NONE ENDS BEFORE EVERY ONE HAS ASKED: a worker that has finished is
    //  an idle processor, rightly offered to the next to ask, so the first to
    //  end would hand its thread to a slower neighbour.
    InterLockedIncrement(FMet);
    Deadline := GetTickCount64 + 5000;
    while (FMet < Length(FInnerWorkers)) and (GetTickCount64 < Deadline) do
        Sleep(1);
end;

{ NO THREAD IS WAITED FOR. With every processor already running a job, an inner
  run does not queue for one: it is its calling thread, and so cannot deadlock. }
procedure TJobPoolTest.WithNoProcessorIdleAnInnerRunIsTheCallingThreadAlone;
var
    P, i: longint;
begin
    P := Min(LogicalProcessorCount, 64);
    SetLength(FInnerWorkers, P);
    SetLength(FRuns, 4);
    FArrived := 0;
    FMet := 0;
    RunJobs(P, P, [], @OuterJobWaitsForAllThenRunsInner);
    for i := 0 to P - 1 do
        AssertEquals(Format('outer job %d had no extra worker', [i]), 1,
            FInnerWorkers[i]);
end;

procedure TJobPoolTest.InnerJobCountsConcurrency(AIndex: longint);
var
    Now_: longint;
begin
    Now_ := InterLockedIncrement(FRunning);
    //  The highest it reached; a lost race only under-reports by a thread, and
    //  the test asks for an upper bound.
    while FPeak < Now_ do
        InterLockedExchange(FPeak, Now_);
    Sleep(5);
    InterLockedDecrement(FRunning);
end;

procedure TJobPoolTest.OuterJobRunsInner(AIndex: longint);
begin
    RunJobsOnIdleProcessors(16, 64, @InnerJobCountsConcurrency);
end;

procedure TJobPoolTest.InnerRunsNeverPutMoreThreadsThanProcessorsToWork;
const
    OUTER = 4;
begin
    FRunning := 0;
    FPeak := 0;
    RunJobs(OUTER, OUTER, [], @OuterJobRunsInner);
    AssertTrue(Format('%d at once on %d processors', [FPeak,
        LogicalProcessorCount]), FPeak <= Max(LogicalProcessorCount, OUTER));
    AssertEquals('all given back', LogicalProcessorCount - 1, IdleProcessorCount);
end;

procedure TJobPoolTest.FailOnThree(AIndex: longint);
begin
    if AIndex = 3 then
        raise Exception.Create('inner job three failed');
end;

procedure TJobPoolTest.AFailedInnerRunGivesItsThreadsBack;
var
    Raised: string;
begin
    Raised := '';
    try
        RunJobsOnIdleProcessors(8, 8, @FailOnThree);
    except
        on E: Exception do
            Raised := E.Message;
    end;
    AssertEquals('inner job three failed', Raised);
    AssertEquals(LogicalProcessorCount - 1, IdleProcessorCount);
end;

procedure TJobPoolTest.RecordThread(AIndex: longint);
begin
    FThreads[AIndex] := GetCurrentThreadId;
    //  Long enough that the workers, not the caller alone, take jobs.
    Sleep(2);
end;

{ A POOL KEEPS ITS THREADS. A fit evaluates its model hundreds of times a
  second and shares each evaluation out; a thread made and ended per evaluation
  cost as much as the computation it shared, and the last interval of a 70
  interval fit ran on one processor of 24 for an hour. So fifty runs of three
  workers must be carried by the same few threads, not by a hundred and fifty. }
procedure TJobPoolTest.WorkerThreadsAreKeptAndReusedAcrossRuns;
var
    Round_, i, j, Distinct: longint;
    Seen: array of TThreadID;
    Known: boolean;
begin
    if LogicalProcessorCount < 4 then
        Ignore('needs at least four processors to have workers at all');
    SetLength(FThreads, 12);
    SetLength(Seen, 0);
    for Round_ := 1 to 50 do
    begin
        RunJobs(12, 4, [], @RecordThread);
        for i := 0 to 11 do
        begin
            Known := False;
            for j := 0 to High(Seen) do
                if Seen[j] = FThreads[i] then
                    Known := True;
            if not Known then
            begin
                SetLength(Seen, Length(Seen) + 1);
                Seen[High(Seen)] := FThreads[i];
            end;
        end;
    end;
    Distinct := Length(Seen);
    //  The caller and three workers; a pool that made a thread per run would
    //  show about a hundred and fifty.
    AssertTrue(Format('%d distinct threads carried 50 runs of 4 workers',
        [Distinct]), Distinct <= 4);
end;

initialization
    RegisterTest('unit', TJobPoolTest);
end.
