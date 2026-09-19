// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the engine records about a running fit, for whoever polls it.)

WHY THIS EXISTS. A fit on the compute server holds its problem's lock from the
first iteration to the last, so every route that reads the model - curves, the
computed profile, the statistics - waits until the fit is over. That is what left
the client's chart empty for the length of every fit. This log is the one part of
a problem that is written by the fit and read by somebody else AT THE SAME TIME,
through a route that takes no problem lock (rest_polling), and so it guards
itself with a lock of its own that is held for a copy and nothing more.

WHAT IT RECORDS, and for which backend. Every backend reports through the same
funnel - TFitTask.ShowCurMin, reached by the native minimizer on each
improvement and by a remote backend each time it relays one - and the service
adds a sample here from that one place. So the log is the same for every
minimizer and every model, and nothing here knows which produced a number.

SAMPLES ARE NUMBERED ACROSS OPERATIONS. A poll that belongs to the previous fit
may still be in flight when the next one starts; numbering from zero again would
let it skip the new fit's first samples. Counting on means a stale poll can only
ever ask for too few.

A LONG FIT IS THINNED, NOT TRUNCATED. The first sample is where the improvement is
measured from and the latest is what the fit has reached, so both always survive;
everything between is halved when the log fills, which keeps a chart's shape at
any length for a bounded cost.

SNAPSHOTS ARE RATIONED. A snapshot rebuilds the whole computed profile and copies
every curve. It is taken only while somebody has asked for one in the last
SNAPSHOT_INTEREST_SECONDS, and no more often than SNAPSHOT_INTERVAL_SECONDS: a
minimizer can improve hundreds of times a second, and a client that switched
animation off simply stops asking.

THE CLOCK IS AN ARGUMENT, so every one of those rules is testable without
waiting for it. It is the run clock (run_clock), in seconds, not the wall
clock: the time a machine spent asleep is not time the fit ran.
}
unit fit_progress_log;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, fit_points_json, fit_progress_json, run_clock;

const
    { Samples kept before the log thins itself. At a sample per improvement a
      typical fit reports a few hundred. }
    PROGRESS_LOG_CAPACITY = 4000;
    { How recently a client must have asked for a snapshot for one to be taken. }
    SNAPSHOT_INTEREST_SECONDS: double = 2.0;
    { The least time between two snapshots. }
    SNAPSHOT_INTERVAL_SECONDS: double = 0.5;

type
    TFitProgressLog = class(TObject)
    private
        FLock: TRTLCriticalSection;
        FCapacity: longint;
        FSamples: TFitProgressSamples;
        FCount: longint;
        FNextSeq: longint;
        FBusy: boolean;
        { What this operation is minimising, and with what. }
        FLossName, FEngineName: string;
        FStarted: TRunTime;
        FFinished: TRunTime;
        FEverStarted: boolean;
        FSnapshotAskedAt: TRunTime;
        FSnapshotTakenAt: TRunTime;
        FHasSnapshot: boolean;
        FSnapshot: TFitProgressSnapshot;
        { Keeps every other sample, and the latest whatever its position. }
        procedure Thin;
        function SecondsBetween(AFrom, ATo: TRunTime): double;
    public
        constructor Create(ACapacity: longint = PROGRESS_LOG_CAPACITY);
        destructor Destroy; override;
        { An operation begins - a fit, or a computation timed like one: the
          previous operation's samples and snapshot go. }
        { ALossName and AEngineName are what this operation is minimising and
          what is minimising it. Recorded here, on the thread that starts the
          operation, because Report is answered on another one without the
          problem's lock and must read nothing off the engine. }
        procedure Start(ANow: TRunTime; const ALossName: string = '';
            const AEngineName: string = '');
        { An operation ends. Harmless when none was started - every operation
          finishes through one path, and not every one starts. }
        procedure Finish(ANow: TRunTime);
        { The engine reached AValue. Ignored outside an operation, and ignored
          when it is not a finite number, which strict JSON cannot carry. }
        procedure Add(ANow: TRunTime; AValue: double);
        { A client wants to see the model as the fit goes. }
        procedure AskForSnapshot(ANow: TRunTime);
        { Whether the engine should take a snapshot now. }
        function SnapshotDue(ANow: TRunTime): boolean;
        { AStarting marks the model as the fit found it, stored before any
          improvement: a frame for the first poll, which does not count against
          the ration - the first improvement is still due at once. }
        procedure StoreSnapshot(ANow: TRunTime;
            const AComputedProfile, ADeltaProfile: TPointsData;
            const ACurves: TFitProgressCurves; AStarting: boolean = False);
        { The samples from ASince on, and the snapshot when AWithSnapshot. }
        function Report(ANow: TRunTime; ASince: longint;
            AWithSnapshot: boolean): TFitProgressReport;
    end;

implementation

constructor TFitProgressLog.Create(ACapacity: longint);
begin
    inherited Create;
    InitCriticalSection(FLock);
    //  Room for the first, the latest and something between, whatever is asked.
    FCapacity := Max(ACapacity, 4);
    SetLength(FSamples, FCapacity);
end;

destructor TFitProgressLog.Destroy;
begin
    DoneCriticalSection(FLock);
    inherited Destroy;
end;

function TFitProgressLog.SecondsBetween(AFrom, ATo: TRunTime): double;
begin
    Result := ATo - AFrom;
end;

procedure TFitProgressLog.Start(ANow: TRunTime; const ALossName: string;
    const AEngineName: string);
begin
    EnterCriticalSection(FLock);
    try
        FCount := 0;
        FLossName := ALossName;
        FEngineName := AEngineName;
        FBusy := True;
        FEverStarted := True;
        FStarted := ANow;
        FFinished := ANow;
        FHasSnapshot := False;
        FSnapshot := Default(TFitProgressSnapshot);
        FSnapshotTakenAt := 0;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TFitProgressLog.Finish(ANow: TRunTime);
begin
    EnterCriticalSection(FLock);
    try
        if FBusy then
            FFinished := ANow;
        FBusy := False;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TFitProgressLog.Thin;
var
    i, Kept: longint;
    Latest: TFitProgressSample;
begin
    Latest := FSamples[FCount - 1];
    Kept := 0;
    i := 0;
    while i < FCount - 1 do
    begin
        FSamples[Kept] := FSamples[i];
        Inc(Kept);
        Inc(i, 2);
    end;
    FSamples[Kept] := Latest;
    FCount := Kept + 1;
end;

procedure TFitProgressLog.Add(ANow: TRunTime; AValue: double);
begin
    if IsNan(AValue) or IsInfinite(AValue) then
        Exit;
    EnterCriticalSection(FLock);
    try
        if not FBusy then
            Exit;
        if FCount = FCapacity then
            Thin;
        FSamples[FCount].Seq := FNextSeq;
        FSamples[FCount].Elapsed := SecondsBetween(FStarted, ANow);
        FSamples[FCount].Value := AValue;
        Inc(FCount);
        Inc(FNextSeq);
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TFitProgressLog.AskForSnapshot(ANow: TRunTime);
begin
    EnterCriticalSection(FLock);
    try
        FSnapshotAskedAt := ANow;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TFitProgressLog.SnapshotDue(ANow: TRunTime): boolean;
begin
    EnterCriticalSection(FLock);
    try
        Result := FBusy and (FSnapshotAskedAt <> 0) and
            (SecondsBetween(FSnapshotAskedAt, ANow) <= SNAPSHOT_INTEREST_SECONDS) and
            ((not FHasSnapshot) or
             (SecondsBetween(FSnapshotTakenAt, ANow) >= SNAPSHOT_INTERVAL_SECONDS));
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TFitProgressLog.StoreSnapshot(ANow: TRunTime;
    const AComputedProfile, ADeltaProfile: TPointsData;
    const ACurves: TFitProgressCurves; AStarting: boolean);
begin
    EnterCriticalSection(FLock);
    try
        //  The arrays are taken by reference and never written again: a new
        //  snapshot replaces this one whole, so a report holding the previous
        //  one keeps a consistent picture.
        FSnapshot.Seq := FNextSeq - 1;
        FSnapshot.ComputedProfile := AComputedProfile;
        FSnapshot.DeltaProfile := ADeltaProfile;
        FSnapshot.Curves := ACurves;
        //  NOT A TAKING, for the ration: zero is long enough ago that the
        //  first improvement is due whatever the interval.
        if AStarting then
            FSnapshotTakenAt := 0
        else
            FSnapshotTakenAt := ANow;
        FHasSnapshot := True;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TFitProgressLog.Report(ANow: TRunTime; ASince: longint;
    AWithSnapshot: boolean): TFitProgressReport;
var
    i, First: longint;
begin
    Result := Default(TFitProgressReport);
    EnterCriticalSection(FLock);
    try
        Result.Busy := FBusy;
        Result.LossName := FLossName;
        Result.EngineName := FEngineName;
        if FBusy then
            Result.Elapsed := SecondsBetween(FStarted, ANow)
        else if FEverStarted then
            Result.Elapsed := SecondsBetween(FStarted, FFinished);
        Result.NextSeq := FNextSeq;
        First := 0;
        while (First < FCount) and (FSamples[First].Seq < ASince) do
            Inc(First);
        SetLength(Result.Samples, FCount - First);
        for i := First to FCount - 1 do
            Result.Samples[i - First] := FSamples[i];
        if AWithSnapshot and FHasSnapshot then
        begin
            Result.HasSnapshot := True;
            Result.Snapshot := FSnapshot;
        end;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

end.
