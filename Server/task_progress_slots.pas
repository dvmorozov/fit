// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the workers of a parallel fit report to, and the thread that
reads it once in a while.)

WHY THIS EXISTS. Fit intervals fit side by side, and every worker reported each
improvement through ONE lock: it summed the figures of every interval and, when
a client was watching, copied every curve of every interval and drew the model
- all under that lock, so the other workers waited for the copying. On a model
of a thousand curves the waits were most of the run (a CPU load of 70 per cent
with 24 busy workers) and the window was handed a huge frame it could barely
read.

WHAT IT IS NOW. Each interval has a SLOT that only its own worker writes: its
latest loss figures and the last frame it drew of its own curves, a copy that
nobody changes afterwards. A worker never waits for another worker. One BUILDER
thread reads every slot a few times a second - sums the figures, appends to the
loss curve, and when a client wants a frame, assembles the model from the
frames the slots hold - so that cost is paid once, by a thread that is nobody's
fit.

WHY NOT A LOCK-FREE POINTER SWAP. The frame is a copy that a reader may be
holding when its owner publishes the next one; freeing the old one safely needs
a reference count taken at the same moment the pointer is read, which a bare
atomic exchange cannot do. A slot therefore has a lock of its own - taken for
the length of a pointer swap or a four-number copy, only ever by its own worker
and, a few times a second, by the builder - so it is never contended in any way
that matters, and no lock is held across anything else. It is a leaf of the lock
order (fit_service, "Fit intervals side by side").
}
unit task_progress_slots;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fit_loss;

const
    { HOW OFTEN THE BUILDER READS THE SLOTS, in milliseconds: the loss curve
      gains a point this often while a step runs, and a frame is asked for and
      put together on every fifth tick at most (the progress log's
      SNAPSHOT_INTERVAL_SECONDS). A window that refreshes once a second gains
      nothing from more, and the workers lose nothing to it. }
    PROGRESS_TICK_MS = 200;

type
    { A frame as its worker drew it, immutable once published and kept alive
      until the last reader lets go of it. A descendant carries what a frame
      holds; this base is only the lifetime. }
    TPublishedFrame = class
    private
        FRefs: longint;
    public
        constructor Create;
    end;

    { One interval's place in the report: written by its worker alone, read by
      the builder. }
    TTaskSlot = class
    private
        FLock: TRTLCriticalSection;
        FParts: TLossParts;
        FHasParts: boolean;
        FFrame: TPublishedFrame;
        FFrameDemand: longint;
        { How many frames the worker has published: what the builder compares
          with the count it saw, to know the interval's curves moved. }
        FFrameSerial: longint;
        FReports: longint;
        FFirstReportUs: int64;
    public
        constructor Create;
        destructor Destroy; override;
        { THE FIGURES THE INTERVAL STARTS FROM, stored before its worker runs:
          not a report, so the step's record of first reports does not count it. }
        procedure SeedParts(const AParts: TLossParts);
        { THE WORKER'S REPORT: its latest figures. AAtUs is when, for the step's
          record of when each interval first reported. }
        procedure StoreParts(const AParts: TLossParts; AAtUs: int64);
        { The figures as last stored, whole - never half of one report and half
          of the next - and False before the first. }
        function ReadParts(out AParts: TLossParts): boolean;
        { THE WORKER'S FRAME, taken over by the slot: the one it replaces is let
          go, and freed once no reader holds it. ADemand is the demand this
          frame answers (TProgressDemand). }
        procedure PublishFrame(AFrame: TPublishedFrame; ADemand: longint);
        { How many frames this interval has published: it rises with each one, so
          a builder that remembers the number it saw knows whether the interval
          drew a new frame since. }
        function FrameSerial: longint;
        { The frame as last published, held for the caller until Release; nil
          before the first. }
        function AcquireFrame: TPublishedFrame;
        { Whether the slot still owes an answer to demand ADemand. }
        function Owes(ADemand: longint): boolean;
        { How many reports the worker made, and when it made the first (0: it
          never did). }
        function Reports: longint;
        function FirstReportUs: int64;
    end;

    { THE REPORT OF ONE STEP: a slot for each interval, and the demand for
      frames that the builder raises. }
    TTaskSlots = class
    private
        FSlots: array of TTaskSlot;
        FDemand: longint;
        function GetSlot(AIndex: longint): TTaskSlot;
        function GetCount: longint;
    public
        constructor Create(ACount: longint);
        destructor Destroy; override;
        property Count: longint read GetCount;
        property Slots[AIndex: longint]: TTaskSlot read GetSlot; default;
        { THE FIGURES OF EVERY INTERVAL, pooled as one run's loss is (the
          framework's own AddLossParts): an interval that has not reported
          counts as it was published before the workers started. }
        function Total: TLossParts;
        { A FRESH FRAME IS WANTED from every interval: each worker draws one at
          its next report. Answers the new demand. }
        function RaiseDemand: longint;
        { The demand now in force; 0 until one is raised. }
        function Demand: longint;
    end;

{ Lets go of a frame acquired from a slot; frees it when it is the last holder. }
procedure ReleaseFrame(var AFrame: TPublishedFrame);

type
    { THE THREAD THAT READS THE SLOTS: calls ATick every AIntervalMs until it is
      stopped. A thread of its own, below normal priority, because the work -
      summing, appending to the loss curve, assembling a frame - is nobody's fit
      and must neither hold a worker up nor take a processor from one that is
      fitting. An exception in a tick is the tick's, never the thread's: a
      picture must not stop the fit it is a picture of. }
    TProgressBuilder = class(TThread)
    private
        FTick: TThreadMethod;
        FIntervalMs: longint;
        FWake: PRTLEvent;
        FTicks: longint;
        FOnFault: TNotifyEvent;
        FOnEnd: TProcedure;
    protected
        procedure Execute; override;
    public
        constructor Create(AIntervalMs: longint; ATick: TThreadMethod);
        destructor Destroy; override;
        { Ends the thread and waits for it: no tick runs once this returns. }
        procedure Stop;
        { How many ticks ran. }
        property Ticks: longint read FTicks;
        { Called, on the builder's thread, with the exception a tick raised. }
        property OnFault: TNotifyEvent read FOnFault write FOnFault;
        { Called on the builder's thread as it ends: what the thread made for
          itself - a formula parser - is freed by the thread that made it, as
          the pool's workers do (job_pool). }
        property OnEnd: TProcedure read FOnEnd write FOnEnd;
    end;

implementation

constructor TPublishedFrame.Create;
begin
    inherited Create;
    //  The creator's own hold, handed to the slot by PublishFrame.
    FRefs := 1;
end;

procedure ReleaseFrame(var AFrame: TPublishedFrame);
var
    Frame: TPublishedFrame;
begin
    Frame := AFrame;
    AFrame := nil;
    if Assigned(Frame) and (InterLockedDecrement(Frame.FRefs) = 0) then
        Frame.Free;
end;

constructor TProgressBuilder.Create(AIntervalMs: longint; ATick: TThreadMethod);
begin
    FTick := ATick;
    FIntervalMs := AIntervalMs;
    FWake := RTLEventCreate;
    inherited Create(True);
    Priority := tpLower;
    Start;
end;

destructor TProgressBuilder.Destroy;
begin
    RTLEventDestroy(FWake);
    inherited Destroy;
end;

procedure TProgressBuilder.Execute;
begin
    while not Terminated do
    begin
        RTLEventWaitFor(FWake, FIntervalMs);
        if Terminated then
            Break;
        try
            FTick;
        except
            on E: Exception do
                if Assigned(FOnFault) then
                    FOnFault(E);
        end;
        InterLockedIncrement(FTicks);
    end;
    if Assigned(FOnEnd) then
        FOnEnd;
end;

procedure TProgressBuilder.Stop;
begin
    Terminate;
    RTLEventSetEvent(FWake);
    WaitFor;
end;

constructor TTaskSlot.Create;
begin
    inherited Create;
    InitCriticalSection(FLock);
end;

destructor TTaskSlot.Destroy;
begin
    ReleaseFrame(FFrame);
    DoneCriticalSection(FLock);
    inherited Destroy;
end;

procedure TTaskSlot.SeedParts(const AParts: TLossParts);
begin
    EnterCriticalSection(FLock);
    try
        FParts := AParts;
        FHasParts := True;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TTaskSlot.StoreParts(const AParts: TLossParts; AAtUs: int64);
begin
    EnterCriticalSection(FLock);
    try
        FParts := AParts;
        FHasParts := True;
        Inc(FReports);
        //  Never 0 once reported: 0 is how "never" is written.
        if FFirstReportUs = 0 then
            if AAtUs > 0 then
                FFirstReportUs := AAtUs
            else
                FFirstReportUs := 1;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TTaskSlot.ReadParts(out AParts: TLossParts): boolean;
begin
    EnterCriticalSection(FLock);
    try
        AParts := FParts;
        Result := FHasParts;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TTaskSlot.PublishFrame(AFrame: TPublishedFrame; ADemand: longint);
var
    Old: TPublishedFrame;
begin
    EnterCriticalSection(FLock);
    try
        Old := FFrame;
        FFrame := AFrame;
        FFrameDemand := ADemand;
        Inc(FFrameSerial);
    finally
        LeaveCriticalSection(FLock);
    end;
    //  Outside the lock: freeing may be slow, and no reader needs it.
    ReleaseFrame(Old);
end;

function TTaskSlot.FrameSerial: longint;
begin
    EnterCriticalSection(FLock);
    try
        Result := FFrameSerial;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TTaskSlot.AcquireFrame: TPublishedFrame;
begin
    EnterCriticalSection(FLock);
    try
        Result := FFrame;
        //  Counted under the lock that publishes, so a frame being replaced is
        //  held before its owner can let it go.
        if Assigned(Result) then
            InterLockedIncrement(Result.FRefs);
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TTaskSlot.Owes(ADemand: longint): boolean;
begin
    EnterCriticalSection(FLock);
    try
        Result := FFrameDemand <> ADemand;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TTaskSlot.Reports: longint;
begin
    EnterCriticalSection(FLock);
    try
        Result := FReports;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TTaskSlot.FirstReportUs: int64;
begin
    EnterCriticalSection(FLock);
    try
        Result := FFirstReportUs;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

constructor TTaskSlots.Create(ACount: longint);
var
    i: longint;
begin
    inherited Create;
    SetLength(FSlots, ACount);
    for i := 0 to ACount - 1 do
        FSlots[i] := TTaskSlot.Create;
end;

destructor TTaskSlots.Destroy;
var
    i: longint;
begin
    for i := 0 to High(FSlots) do
        FSlots[i].Free;
    inherited Destroy;
end;

function TTaskSlots.GetSlot(AIndex: longint): TTaskSlot;
begin
    Result := FSlots[AIndex];
end;

function TTaskSlots.GetCount: longint;
begin
    Result := Length(FSlots);
end;

function TTaskSlots.Total: TLossParts;
var
    i: longint;
    Parts: TLossParts;
begin
    Result := Default(TLossParts);
    for i := 0 to High(FSlots) do
        if FSlots[i].ReadParts(Parts) then
            AddLossParts(Result, Parts);
end;

function TTaskSlots.RaiseDemand: longint;
begin
    Result := InterLockedIncrement(FDemand);
end;

function TTaskSlots.Demand: longint;
begin
    Result := InterLockedExchangeAdd(FDemand, 0);
end;

end.
