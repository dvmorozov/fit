// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Runs independent jobs side by side on a bounded set of workers.)

WHAT IT IS FOR. Fit intervals are independent by construction - each its own
task over its own stretch of the profile - so they can be fitted at once
(docs/internal/fit-performance.md, "parallel fit intervals"). On a BOUNDED pool: at most one
worker per processor, because more threads than processors only take turns, and
never more workers than jobs.

HOW, and why each choice:

  * The CALLING THREAD IS ONE OF THE WORKERS, so a pool of one is the old loop
    exactly, on the thread that always ran it, with no thread made at all.
  * THE THREADS ARE KEPT. A worker is made the first time a run needs one, and
    when its run is over it PARKS on its own event and waits to be lent to the
    next. An evaluation is run hundreds of times a second, and a thread made
    and ended for each cost as much as the computation it shared out: the last
    interval of a 70 interval fit ran for an hour on one processor of 24 with
    its evaluation "shared" that way. The pool grows to the most workers any
    run has had at once and no further, and ends them in StopPoolWorkers (the
    unit's finalization).
  * A run is joined by a count, not by TThread.WaitFor: each worker, as its
    last act for the run, takes itself off the count and the last one sets an
    event the caller waits on. WaitFor made the joins of an evaluation -
    dozens a second - cost milliseconds of waking each, when the caller is the
    main thread (a test, or a host that runs fits there). A worker parks BEFORE
    it tells the run it is done, so a caller that starts the next run at once
    finds it parked instead of making another thread. Nothing uses Synchronize
    or Queue: a headless server never runs the main thread's queue, which is
    why the previous parallel design could not work there (findings.md, "Fit
    intervals ran one after another, and three things stood in the way").
  * WHAT A THREAD MADE FOR ITSELF IS FREED WHEN THE THREAD ENDS, not after each
    run: a formula parser kept from one evaluation to the next is the point of
    keeping the thread, and the parser is keyed by its formula, so a run of
    another fit loses nothing by finding it there. The cleanup a run names is
    remembered by every worker lent to it and run, on that thread, as it ends.
  * LONGEST FIRST, claimed with an atomic counter: a worker takes the next
    index of a list sorted by cost, so the longest job does not start last and
    leave every other worker idle while it runs, and the queue needs no lock.
  * Every worker first takes the caller's FLOATING-POINT STATE - exception mask,
    rounding, precision. A new thread starts with the platform's defaults, and
    a different mask or precision would compute a different fit.
  * A FAILURE IS RAISED AFTER THE JOIN, and the first one only: the other jobs
    finish, nothing is left running, and the caller sees the exception object
    itself - so a refusal (EUserException) is still a refusal.

ONE BUDGET OF THREADS, SHARED BY JOBS THAT START JOBS. A job may itself have
work to split - one fit interval's objective evaluation computing its curves
side by side (RunJobsOnIdleProcessors). Those inner runs must not add their own
full complement of threads on top of the intervals' (24 intervals each starting
24 threads), and must not wait for a thread that is not there. So every extra
thread a run makes is counted, RunJobs' always (its caller asked for them), and
an inner run takes only what the processors still have idle - maybe none, and
then it runs on its own thread alone, which is the serial loop. Nothing ever
waits for a free thread, so nothing can deadlock.

A THREAD IS COUNTED WHILE IT LIVES, NOT WHILE ITS RUN DOES. Each worker takes
itself off the count as it ends, when it has no job left to claim. Counting a
run's workers until the run was joined held every processor of a step of 35
fit intervals on 24 workers until the last interval finished: the ones that
ran out of work were never offered to an evaluation, and the long tail - six
giant intervals - used seven processors of 24.

No job may touch another's state, or anything shared without its own lock;
that is the caller's contract, stated where the jobs are (TFitService).
}
unit job_pool;

{$mode objfpc}{$H+}

interface

uses
    //  Windows first, so every unit after it wins over it (windows_unit_first.tests.ps1).
{$IFDEF WINDOWS}
    Windows,
{$ENDIF}
    Classes, SysUtils, Math;

type
    { One job, by its index in the run. }
    TJobProc = procedure(AIndex: longint) of object;
    { Run on each worker thread that served a run naming it, as the thread ends
      (StopPoolWorkers): what the thread made for itself - a formula parser - is
      freed by the thread that made it. }
    TWorkerCleanup = procedure;

{ Runs jobs 0..ACount-1, each exactly once, on at most AWorkers workers (the
  calling thread among them), the costliest first by ACosts - which may be
  empty for "in index order". Returns when every job has finished; raises the
  first job's exception, if any, after that. }
procedure RunJobs(ACount, AWorkers: longint; const ACosts: array of double;
    AJob: TJobProc; AWorkerCleanup: TWorkerCleanup = nil);

{ How many workers ACount jobs get when at most AMaxWorkers are allowed: never
  more than the jobs, never fewer than one. }
function WorkersFor(ACount, AMaxWorkers: longint): longint;

{ The processors this machine offers to run threads on - what an unbounded pool
  would be bounded by. TThread.ProcessorCount is not trusted: on macOS with FPC
  3.2 it answers 1. }
function LogicalProcessorCount: longint;

{ As RunJobs, for a job that splits its own work while others run: takes the
  threads the processors still have idle, up to AMaxWorkers in all, and runs the
  jobs on those and on the calling thread - on the calling thread alone when
  none is idle. Answers how many workers it had (1 when none was added). The
  jobs are taken in index order. }
function RunJobsOnIdleProcessors(ACount, AMaxWorkers: longint;
    AJob: TJobProc; AWorkerCleanup: TWorkerCleanup = nil): longint;

{ How many more threads the processors can take: one less than their number (the
  caller is the other), less every extra thread lent to a run that has not
  finished with it. Never negative. }
function IdleProcessorCount: longint;

{ Ends every kept worker, each running the cleanups of the runs it served on its
  own thread, and waits for them. For the end of the process (the unit's
  finalization) and for a test of that cleanup; never while a run is in flight.
  The pool makes workers again, lazily, if a run asks later. }
procedure StopPoolWorkers;

implementation

{$IFDEF UNIX}
uses
    ctypes;

function sysconf(AName: cint): clong; cdecl; external 'c' name 'sysconf';

const
{$IFDEF DARWIN}
    SC_NPROCESSORS_ONLN = 58;
{$ELSE}
    SC_NPROCESSORS_ONLN = 84;
{$ENDIF}
{$ENDIF}

var
    { The threads, besides their callers, that are lent to a run which has not
      finished with them. Read and changed atomically; no lock. }
    ExtraThreads: longint = 0;

type
    { What every worker of one run shares: the order, the next index to claim,
      the caller's floating-point state and the first failure. }
    TJobRun = class
    public
        Order: array of longint;
        Next: longint;
        Job: TJobProc;
        Mask: TFPUExceptionMask;
        Rounding: TFPURoundingMode;
        Precision: TFPUPrecisionMode;
        Lock: TRTLCriticalSection;
        FirstError: TObject;
        Cleanup: TWorkerCleanup;
        { The caller and every worker still running; the event is set when the
          last of them takes itself off. }
        Pending: longint;
        Finished: PRTLEvent;
        constructor Create;
        { Takes one off Pending; the last sets Finished. }
        procedure Leave;
        destructor Destroy; override;
        { Claims and runs jobs until none is left. Called on every worker. }
        procedure Work;
    end;

    { A kept thread: waits parked on its own event, is lent a run, works it,
      parks again. }
    TPoolWorker = class(TThread)
    private
        FWake: PRTLEvent;
        FRun: TJobRun;
        FCleanups: array of TWorkerCleanup;
        procedure Remember(ACleanup: TWorkerCleanup);
    protected
        procedure Execute; override;
    public
        constructor Create;
        destructor Destroy; override;
        { Hands the worker a run and wakes it. }
        procedure Lend(ARun: TJobRun);
        { Asks the worker to end; it runs its cleanups first. }
        procedure Retire;
    end;

var
    { Every worker made, and those of them now parked, newest last: the one
      parked last is lent first, so a run of the same size keeps meeting the
      same threads. }
    PoolLock: TRTLCriticalSection;
    PoolWorkers: array of TPoolWorker;
    ParkedWorkers: array of TPoolWorker;

procedure Park(AWorker: TPoolWorker);
begin
    EnterCriticalSection(PoolLock);
    try
        SetLength(ParkedWorkers, Length(ParkedWorkers) + 1);
        ParkedWorkers[High(ParkedWorkers)] := AWorker;
    finally
        LeaveCriticalSection(PoolLock);
    end;
end;

{ A parked worker, or a new one when none is: the pool grows only to what runs
  have needed at once. Raises when no thread can be made. }
function TakeWorker: TPoolWorker;
begin
    EnterCriticalSection(PoolLock);
    try
        if Length(ParkedWorkers) > 0 then
        begin
            Result := ParkedWorkers[High(ParkedWorkers)];
            SetLength(ParkedWorkers, Length(ParkedWorkers) - 1);
        end
        else
        begin
            Result := TPoolWorker.Create;
            SetLength(PoolWorkers, Length(PoolWorkers) + 1);
            PoolWorkers[High(PoolWorkers)] := Result;
        end;
    finally
        LeaveCriticalSection(PoolLock);
    end;
end;

constructor TJobRun.Create;
begin
    inherited Create;
    InitCriticalSection(Lock);
    Next := -1;
    Pending := 1;
    Finished := RTLEventCreate;
end;

destructor TJobRun.Destroy;
begin
    RTLEventDestroy(Finished);
    DoneCriticalSection(Lock);
    inherited Destroy;
end;

procedure TJobRun.Leave;
begin
    if InterLockedDecrement(Pending) = 0 then
        RTLEventSetEvent(Finished);
end;

procedure TJobRun.Work;
var
    Claimed: longint;
begin
    while True do
    begin
        Claimed := InterLockedIncrement(Next);
        if Claimed > High(Order) then
            Break;
        try
            Job(Order[Claimed]);
        except
            //  KEPT, the first only, and raised by the caller after the join.
            //  The object itself, so its class - a refusal or a fault - is
            //  what the caller sees.
            EnterCriticalSection(Lock);
            try
                if FirstError = nil then
                    FirstError := TObject(AcquireExceptionObject);
            finally
                LeaveCriticalSection(Lock);
            end;
        end;
    end;
end;

constructor TPoolWorker.Create;
begin
    FWake := RTLEventCreate;
    //  Joined by StopPoolWorkers, not freed by itself: the pool owns it.
    FreeOnTerminate := False;
    //  BELOW NORMAL, because a pool that fills every processor must not starve
    //  what answers the window: with one worker per processor the server's own
    //  request threads waited seconds for a turn - a progress poll took 2 to 10
    //  s - and the client's window, on the same machine, stopped responding.
    //  The pool still uses every idle processor, and a lower priority costs the
    //  fit nothing while the machine has nothing else to do. Set before the
    //  thread starts, so it never runs at the default.
    inherited Create(True);
    Priority := tpLower;
    Start;
end;

destructor TPoolWorker.Destroy;
begin
    inherited Destroy;
    RTLEventDestroy(FWake);
end;

procedure TPoolWorker.Remember(ACleanup: TWorkerCleanup);
var
    i: longint;
begin
    if not Assigned(ACleanup) then
        Exit;
    for i := 0 to High(FCleanups) do
        if Pointer(FCleanups[i]) = Pointer(ACleanup) then
            Exit;
    SetLength(FCleanups, Length(FCleanups) + 1);
    FCleanups[High(FCleanups)] := ACleanup;
end;

procedure TPoolWorker.Lend(ARun: TJobRun);
begin
    //  The run is written before the event is set, and read after it is waited
    //  for: the event is what hands it across.
    FRun := ARun;
    RTLEventSetEvent(FWake);
end;

procedure TPoolWorker.Retire;
begin
    Terminate;
    RTLEventSetEvent(FWake);
end;

procedure TPoolWorker.Execute;
var
    Run: TJobRun;
    i: longint;
begin
    while True do
    begin
        RTLEventWaitFor(FWake);
        if Terminated then
            Break;
        Run := FRun;
        FRun := nil;
        if Run = nil then
            Continue;
        //  The caller's floating-point state before anything is computed: a
        //  kept thread has the last run's, which may not be this one's.
        SetExceptionMask(Run.Mask);
        SetRoundMode(Run.Rounding);
        SetPrecisionMode(Run.Precision);
        try
            Run.Work;
        finally
            Remember(Run.Cleanup);
            //  OFF THE COUNT before the run is told: with nothing left to claim
            //  this thread is a processor an evaluation may take. The caller
            //  reads the count after the join, so it is settled by then.
            InterLockedDecrement(ExtraThreads);
            //  PARKED before the run is told, so the caller's next run finds
            //  this thread instead of making another.
            Park(Self);
            //  The last touch of the run: the caller may free it after this.
            Run.Leave;
        end;
    end;
    for i := 0 to High(FCleanups) do
        try
            FCleanups[i]();
        except
            //  The thread is ending; a cleanup that fails has nowhere to report.
        end;
end;

function WorkersFor(ACount, AMaxWorkers: longint): longint;
begin
    Result := Max(1, Min(ACount, AMaxWorkers));
end;

function LogicalProcessorCount: longint;
{$IFDEF WINDOWS}
var
    Info: TSystemInfo;
{$ENDIF}
begin
{$IFDEF UNIX}
    Result := sysconf(SC_NPROCESSORS_ONLN);
{$ELSE}
{$IFDEF WINDOWS}
    GetSystemInfo(Info);
    Result := Info.dwNumberOfProcessors;
{$ELSE}
    Result := TThread.ProcessorCount;
{$ENDIF}
{$ENDIF}
    if Result < 1 then
        Result := 1;
end;

{ The run itself. Its callers have ALREADY added the extra threads they want to
  ExtraThreads; each worker takes itself off as it ends, and what could not be
  made is taken off here. }
procedure RunReserved(ACount, AWorkers: longint; const ACosts: array of double;
    AJob: TJobProc; AWorkerCleanup: TWorkerCleanup);
var
    Run: TJobRun;
    i, j, T, Made: longint;
    Error: TObject;
begin
    if ACount <= 0 then
        Exit;
    Run := TJobRun.Create;
    try
        Run.Job := AJob;
        Run.Cleanup := AWorkerCleanup;
        Run.Mask := GetExceptionMask;
        Run.Rounding := GetRoundMode;
        Run.Precision := GetPrecisionMode;
        //  Costliest first; a stable insertion sort, so equal costs keep their
        //  index order and a run is reproducible.
        SetLength(Run.Order, ACount);
        for i := 0 to ACount - 1 do
            Run.Order[i] := i;
        if Length(ACosts) = ACount then
            for i := 1 to ACount - 1 do
            begin
                T := Run.Order[i];
                j := i - 1;
                while (j >= 0) and (ACosts[Run.Order[j]] < ACosts[T]) do
                begin
                    Run.Order[j + 1] := Run.Order[j];
                    Dec(j);
                end;
                Run.Order[j + 1] := T;
            end;

        //  The calling thread is a worker too, so one worker makes no thread.
        try
            Made := 0;
            for i := 1 to WorkersFor(ACount, AWorkers) - 1 do
            begin
                //  Counted before it can run, taken back if it cannot be made.
                InterLockedIncrement(Run.Pending);
                try
                    TakeWorker.Lend(Run);
                    Inc(Made);
                except
                    Run.Leave;
                    //  Fewer workers is only slower: the caller does the rest.
                    Break;
                end;
            end;
            //  The reservation of a worker that was never made.
            if WorkersFor(ACount, AWorkers) - 1 - Made > 0 then
                InterLockedExchangeAdd(ExtraThreads,
                    -(WorkersFor(ACount, AWorkers) - 1 - Made));
            Run.Work;
        finally
            Run.Leave;
            RTLEventWaitFor(Run.Finished);
        end;
        Error := Run.FirstError;
        Run.FirstError := nil;
    finally
        Run.Free;
    end;
    if Assigned(Error) then
        raise Error;
end;

function IdleProcessorCount: longint;
begin
    Result := Max(0, LogicalProcessorCount - 1 - InterLockedExchangeAdd(ExtraThreads, 0));
end;

procedure StopPoolWorkers;
var
    Workers: array of TPoolWorker;
    i: longint;
begin
    EnterCriticalSection(PoolLock);
    try
        Workers := PoolWorkers;
        PoolWorkers := nil;
        ParkedWorkers := nil;
    finally
        LeaveCriticalSection(PoolLock);
    end;
    for i := 0 to High(Workers) do
        Workers[i].Retire;
    for i := 0 to High(Workers) do
    begin
        Workers[i].WaitFor;
        Workers[i].Free;
    end;
end;

procedure RunJobs(ACount, AWorkers: longint; const ACosts: array of double;
    AJob: TJobProc; AWorkerCleanup: TWorkerCleanup);
var
    Extra: longint;
begin
    if ACount <= 0 then
        Exit;
    //  Counted whether or not the processors can spare them: the caller asked.
    //  Each worker takes itself off as it ends.
    Extra := WorkersFor(ACount, AWorkers) - 1;
    InterLockedExchangeAdd(ExtraThreads, Extra);
    RunReserved(ACount, AWorkers, ACosts, AJob, AWorkerCleanup);
end;

function RunJobsOnIdleProcessors(ACount, AMaxWorkers: longint;
    AJob: TJobProc; AWorkerCleanup: TWorkerCleanup): longint;
var
    Wanted, Taken, Seen: longint;
begin
    Result := 1;
    if ACount <= 0 then
        Exit;
    Wanted := WorkersFor(ACount, AMaxWorkers) - 1;
    //  TAKE what is idle with a compare-and-swap, so two jobs asking at once
    //  never take the same thread twice.
    Taken := 0;
    while Wanted > 0 do
    begin
        Seen := InterLockedExchangeAdd(ExtraThreads, 0);
        Taken := Min(Wanted, Max(0, LogicalProcessorCount - 1 - Seen));
        if Taken = 0 then
            Break;
        if InterlockedCompareExchange(ExtraThreads, Seen + Taken, Seen) = Seen then
            Break;
        Taken := 0;
    end;
    Result := Taken + 1;
    RunReserved(ACount, Result, [], AJob, AWorkerCleanup);
end;

initialization
    InitCriticalSection(PoolLock);

finalization
    StopPoolWorkers;
    DoneCriticalSection(PoolLock);

end.
