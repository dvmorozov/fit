// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the workers of a parallel fit report to, and the thread that
reads it.)

THE CLAIM: a worker reports into a slot of its own and never waits for another
worker, and what the builder reads out of the slots is whole - never half of
one report and half of the next, never a frame freed under its reader. The
fit's answer does not depend on any of it (testcase_parallel_fit); what these
tests hold is the mechanics, which a model too small to contend would never
exercise.
}
unit testcase_task_progress_slots;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_loss, task_progress_slots;

type
    { A frame that counts the ones freed. }
    TCountedFrame = class(TPublishedFrame)
    public
        Tag: longint;
        destructor Destroy; override;
    end;

    { Writes figures whose four numbers are always equal, so a read of two
      different reports is told at once. }
    TParts = class(TThread)
    private
        FSlot: TTaskSlot;
        FStopAt: longint;
    protected
        procedure Execute; override;
    public
        constructor Create(ASlot: TTaskSlot; AStopAt: longint);
    end;

    TTaskProgressSlotsTest = class(TTestCase)
    private
        FTicks: longint;
        FBusyMs, FFaults: longint;
        procedure SeeFault(ASender: TObject);
        procedure CountTick;
        procedure SlowTick;
        procedure FailingTick;
    protected
        procedure SetUp; override;
    published
        procedure AReadOfAWorkersFiguresIsNeverTornBetweenTwoReports;
        procedure AnIntervalThatHasNotReportedIsNotInTheTotal;
        procedure TheTotalPoolsTheIntervalsAsOneRunsLossIs;
        procedure SeededFiguresAreNotAReport;
        procedure TheFirstReportIsWhenItWasMadeAndNeverZero;
        procedure AFrameStaysAliveWhileAReaderHoldsItAndGoesWhenItLetsGo;
        procedure PublishingLetsGoOfTheFrameItReplaces;
        procedure ASlotWithNoFrameAnswersNil;
        procedure EveryIntervalOwesAFreshFrameOnceDemandIsRaised;
        procedure AnIntervalAnswersADemandOnceAndOnlyThatOne;
        procedure TheBuilderTicksAboutOnceEveryInterval;
        procedure NoTickRunsOnceTheBuilderIsStopped;
        procedure AStopDoesNotWaitOutTheInterval;
        procedure AFaultInATickIsReportedAndTheBuilderGoesOn;
        procedure TheBuilderRunsBelowNormalPriority;
    end;

implementation

var
    FreedFrames: longint;

destructor TCountedFrame.Destroy;
begin
    InterLockedIncrement(FreedFrames);
    inherited Destroy;
end;

constructor TParts.Create(ASlot: TTaskSlot; AStopAt: longint);
begin
    FSlot := ASlot;
    FStopAt := AStopAt;
    inherited Create(False);
end;

procedure TParts.Execute;
var
    i: longint;
    P: TLossParts;
begin
    for i := 1 to FStopAt do
    begin
        P.SumSq := i;
        P.SumAbs := i;
        P.SumObs := i;
        P.SumAbsObs := i;
        FSlot.StoreParts(P, 1);
    end;
end;

procedure TTaskProgressSlotsTest.SetUp;
begin
    FreedFrames := 0;
    FTicks := 0;
    FBusyMs := 0;
    FFaults := 0;
end;

procedure TTaskProgressSlotsTest.CountTick;
begin
    InterLockedIncrement(FTicks);
end;

procedure TTaskProgressSlotsTest.SlowTick;
begin
    InterLockedIncrement(FTicks);
    Sleep(FBusyMs);
end;

procedure TTaskProgressSlotsTest.SeeFault(ASender: TObject);
begin
    InterLockedIncrement(FFaults);
end;

procedure TTaskProgressSlotsTest.FailingTick;
begin
    InterLockedIncrement(FTicks);
    raise Exception.Create('a tick that fails');
end;

procedure TTaskProgressSlotsTest.AReadOfAWorkersFiguresIsNeverTornBetweenTwoReports;
var
    Slot: TTaskSlot;
    Writer: TParts;
    P: TLossParts;
    Reads, Torn: longint;
begin
    Slot := TTaskSlot.Create;
    try
        Writer := TParts.Create(Slot, 300000);
        try
            Reads := 0;
            Torn := 0;
            while not Writer.Finished do
                if Slot.ReadParts(P) then
                begin
                    Inc(Reads);
                    if (P.SumSq <> P.SumAbs) or (P.SumAbs <> P.SumObs) or
                       (P.SumObs <> P.SumAbsObs) then
                        Inc(Torn);
                end;
            Writer.WaitFor;
        finally
            Writer.Free;
        end;
        AssertEquals('reads made while the worker wrote', True, Reads > 0);
        AssertEquals('reads that saw two reports at once', 0, Torn);
        Slot.ReadParts(P);
        AssertEquals('and the last report is what remains', 300000, P.SumSq, 0);
    finally
        Slot.Free;
    end;
end;

procedure TTaskProgressSlotsTest.AnIntervalThatHasNotReportedIsNotInTheTotal;
var
    Slots: TTaskSlots;
    P: TLossParts;
begin
    Slots := TTaskSlots.Create(2);
    try
        P := Default(TLossParts);
        AssertFalse('nothing yet', Slots[0].ReadParts(P));
        P.SumSq := 3;
        Slots[1].StoreParts(P, 1);
        AssertEquals(3, Slots.Total.SumSq, 0);
    finally
        Slots.Free;
    end;
end;

procedure TTaskProgressSlotsTest.TheTotalPoolsTheIntervalsAsOneRunsLossIs;
var
    Slots: TTaskSlots;
    A, B, Want: TLossParts;
begin
    Slots := TTaskSlots.Create(2);
    try
        A := Default(TLossParts);
        B := Default(TLossParts);
        A.SumSq := 1;
        A.SumObs := 10;
        B.SumSq := 2;
        B.SumObs := 20;
        Slots[0].StoreParts(A, 1);
        Slots[1].StoreParts(B, 1);
        //  The framework's own pooling, applied the way a run does.
        Want := A;
        AddLossParts(Want, B);
        AssertEquals(Want.SumSq, Slots.Total.SumSq, 0);
        AssertEquals(Want.SumObs, Slots.Total.SumObs, 0);
        AssertEquals('the loss of the pool, not the sum of the losses',
            LossFromParts(LOSS_KIND_RFACTOR, Want),
            LossFromParts(LOSS_KIND_RFACTOR, Slots.Total), 0);
    finally
        Slots.Free;
    end;
end;

procedure TTaskProgressSlotsTest.SeededFiguresAreNotAReport;
var
    Slot: TTaskSlot;
    P: TLossParts;
begin
    Slot := TTaskSlot.Create;
    try
        P := Default(TLossParts);
        P.SumSq := 5;
        Slot.SeedParts(P);
        AssertTrue('counted in the total', Slot.ReadParts(P) and (P.SumSq = 5));
        AssertEquals('but no report', 0, Slot.Reports);
        AssertEquals('and no first report', 0, Slot.FirstReportUs);
    finally
        Slot.Free;
    end;
end;

procedure TTaskProgressSlotsTest.TheFirstReportIsWhenItWasMadeAndNeverZero;
var
    Slot: TTaskSlot;
    P: TLossParts;
begin
    Slot := TTaskSlot.Create;
    try
        P := Default(TLossParts);
        Slot.StoreParts(P, 0);
        AssertEquals('0 is how "never" is written, so a report at 0 is 1', 1,
            Slot.FirstReportUs);
        Slot.StoreParts(P, 500);
        AssertEquals('the first stays', 1, Slot.FirstReportUs);
        AssertEquals(2, Slot.Reports);
    finally
        Slot.Free;
    end;
end;

procedure TTaskProgressSlotsTest.AFrameStaysAliveWhileAReaderHoldsItAndGoesWhenItLetsGo;
var
    Slot: TTaskSlot;
    Held: TPublishedFrame;
begin
    Slot := TTaskSlot.Create;
    try
        Slot.PublishFrame(TCountedFrame.Create, 0);
        Held := Slot.AcquireFrame;
        AssertTrue('read', Assigned(Held));
        //  Replaced while it is held.
        Slot.PublishFrame(TCountedFrame.Create, 1);
        AssertEquals('not freed under its reader', 0, FreedFrames);
        ReleaseFrame(Held);
        AssertEquals('freed when the reader lets go', 1, FreedFrames);
        AssertTrue('the reader no longer holds it', not Assigned(Held));
    finally
        Slot.Free;
    end;
    AssertEquals('and the one still in the slot goes with it', 2, FreedFrames);
end;

procedure TTaskProgressSlotsTest.PublishingLetsGoOfTheFrameItReplaces;
var
    Slot: TTaskSlot;
begin
    Slot := TTaskSlot.Create;
    try
        Slot.PublishFrame(TCountedFrame.Create, 0);
        Slot.PublishFrame(TCountedFrame.Create, 1);
        Slot.PublishFrame(TCountedFrame.Create, 2);
        AssertEquals('every replaced frame freed, none held', 2, FreedFrames);
    finally
        Slot.Free;
    end;
end;

procedure TTaskProgressSlotsTest.ASlotWithNoFrameAnswersNil;
var
    Slot: TTaskSlot;
begin
    Slot := TTaskSlot.Create;
    try
        AssertTrue(not Assigned(Slot.AcquireFrame));
    finally
        Slot.Free;
    end;
end;

procedure TTaskProgressSlotsTest.EveryIntervalOwesAFreshFrameOnceDemandIsRaised;
var
    Slots: TTaskSlots;
    i: longint;
begin
    Slots := TTaskSlots.Create(3);
    try
        for i := 0 to 2 do
            AssertFalse('no demand yet', Slots[i].Owes(Slots.Demand));
        Slots.RaiseDemand;
        for i := 0 to 2 do
            AssertTrue('every interval owes one', Slots[i].Owes(Slots.Demand));
    finally
        Slots.Free;
    end;
end;

procedure TTaskProgressSlotsTest.AnIntervalAnswersADemandOnceAndOnlyThatOne;
var
    Slots: TTaskSlots;
    D: longint;
begin
    Slots := TTaskSlots.Create(2);
    try
        D := Slots.RaiseDemand;
        Slots[0].PublishFrame(TCountedFrame.Create, D);
        AssertFalse('answered', Slots[0].Owes(Slots.Demand));
        AssertTrue('the other has not', Slots[1].Owes(Slots.Demand));
        Slots.RaiseDemand;
        AssertTrue('a new demand is owed again', Slots[0].Owes(Slots.Demand));
    finally
        Slots.Free;
    end;
end;

procedure TTaskProgressSlotsTest.TheBuilderTicksAboutOnceEveryInterval;
var
    B: TProgressBuilder;
begin
    B := TProgressBuilder.Create(25, @CountTick);
    try
        Sleep(330);
        B.Stop;
        //  The cadence, with the slack a loaded machine needs.
        AssertTrue(Format('%d ticks in 330 ms at 25', [FTicks]),
            (FTicks >= 4) and (FTicks <= 14));
        AssertEquals('as many as it counted', FTicks, B.Ticks);
    finally
        B.Free;
    end;
end;

procedure TTaskProgressSlotsTest.NoTickRunsOnceTheBuilderIsStopped;
var
    B: TProgressBuilder;
    After: longint;
begin
    FBusyMs := 40;
    B := TProgressBuilder.Create(10, @SlowTick);
    try
        Sleep(120);
        B.Stop;
        After := FTicks;
        Sleep(120);
        AssertEquals('nothing after Stop returned', After, FTicks);
    finally
        B.Free;
    end;
end;

procedure TTaskProgressSlotsTest.AStopDoesNotWaitOutTheInterval;
var
    B: TProgressBuilder;
    From: QWord;
begin
    B := TProgressBuilder.Create(30000, @CountTick);
    try
        From := GetTickCount64;
        B.Stop;
        AssertTrue('stopped at once, not after thirty seconds',
            GetTickCount64 - From < 2000);
        AssertEquals('and it never ticked', 0, FTicks);
    finally
        B.Free;
    end;
end;

procedure TTaskProgressSlotsTest.AFaultInATickIsReportedAndTheBuilderGoesOn;
var
    B: TProgressBuilder;
begin
    B := TProgressBuilder.Create(15, @FailingTick);
    B.OnFault := @SeeFault;
    try
        Sleep(150);
        B.Stop;
        AssertTrue('it kept ticking after the first fault', FTicks >= 3);
        AssertEquals('and said so each time', FTicks, FFaults);
    finally
        B.Free;
    end;
end;

procedure TTaskProgressSlotsTest.TheBuilderRunsBelowNormalPriority;
var
    B: TProgressBuilder;
begin
    B := TProgressBuilder.Create(1000, @CountTick);
    try
        //  Set where the platform honours it; read back where it does not, it
        //  is the platform's normal (a Linux process cannot lower it).
        B.Stop;
        {$IFDEF MSWINDOWS}
        AssertTrue('below normal', B.Priority = tpLower);
        {$ELSE}
        Ignore('thread priority is only observable on Windows');
        {$ENDIF}
    finally
        B.Free;
    end;
end;

initialization
    RegisterTest('unit', TTaskProgressSlotsTest);
end.
