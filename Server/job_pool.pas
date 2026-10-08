// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Runs independent jobs side by side on a bounded set of workers.)

WHAT IT IS FOR. Fit intervals are independent by construction - each its own
task over its own stretch of the profile - so they can be fitted at once
(docs/internal/fit-performance.md, stage 8). On a BOUNDED pool: at most one
worker per processor, because more threads than processors only take turns, and
never more workers than jobs.

HOW, and why each choice:

  * The CALLING THREAD IS ONE OF THE WORKERS, so a pool of one is the old loop
    exactly, on the thread that always ran it, with no thread made at all.
  * Workers are made per run and joined with WaitFor. Nothing uses Synchronize
    or Queue: a headless server never runs the main thread's queue, which is
    why the previous parallel design could not work there (findings.md, "Fit
    intervals ran one after another, and three things stood in the way").
  * LONGEST FIRST, claimed with an atomic counter: a worker takes the next
    index of a list sorted by cost, so the longest job does not start last and
    leave every other worker idle while it runs, and the queue needs no lock.
  * Every worker first takes the caller's FLOATING-POINT STATE - exception mask,
    rounding, precision. A new thread starts with the platform's defaults, and
    a different mask or precision would compute a different fit.
  * A FAILURE IS RAISED AFTER THE JOIN, and the first one only: the other jobs
    finish, nothing is left running, and the caller sees the exception object
    itself - so a refusal (EUserException) is still a refusal.

No job may touch another's state, or anything shared without its own lock;
that is the caller's contract, stated where the jobs are (TFitService).
}
unit job_pool;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math;

type
    { One job, by its index in the run. }
    TJobProc = procedure(AIndex: longint) of object;
    { Run on each worker thread the pool made, as it ends: what the thread made
      for itself - a formula parser - is freed by the thread that made it. }
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
{$IFDEF WINDOWS}
uses
    Windows;
{$ENDIF}

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
        constructor Create;
        destructor Destroy; override;
        { Claims and runs jobs until none is left. Called on every worker. }
        procedure Work;
    end;

    TJobWorker = class(TThread)
    private
        FRun: TJobRun;
    protected
        procedure Execute; override;
    public
        constructor Create(ARun: TJobRun);
    end;

constructor TJobRun.Create;
begin
    inherited Create;
    InitCriticalSection(Lock);
    Next := -1;
end;

destructor TJobRun.Destroy;
begin
    DoneCriticalSection(Lock);
    inherited Destroy;
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

constructor TJobWorker.Create(ARun: TJobRun);
begin
    FRun := ARun;
    FreeOnTerminate := False;
    inherited Create(False);
end;

procedure TJobWorker.Execute;
begin
    //  The caller's floating-point state before anything is computed.
    SetExceptionMask(FRun.Mask);
    SetRoundMode(FRun.Rounding);
    SetPrecisionMode(FRun.Precision);
    try
        FRun.Work;
    finally
        if Assigned(FRun.Cleanup) then
            FRun.Cleanup;
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

procedure RunJobs(ACount, AWorkers: longint; const ACosts: array of double;
    AJob: TJobProc; AWorkerCleanup: TWorkerCleanup);
var
    Run: TJobRun;
    Workers: array of TJobWorker;
    i, j, T: longint;
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
        SetLength(Workers, WorkersFor(ACount, AWorkers) - 1);
        for i := 0 to High(Workers) do
            Workers[i] := TJobWorker.Create(Run);
        try
            Run.Work;
        finally
            for i := 0 to High(Workers) do
            begin
                Workers[i].WaitFor;
                Workers[i].Free;
            end;
        end;
        Error := Run.FirstError;
        Run.FirstError := nil;
    finally
        Run.Free;
    end;
    if Assigned(Error) then
        raise Error;
end;

end.
