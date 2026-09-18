// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the engine records about a running fit, for whoever polls it.)

THE LOG IS READ FROM ANOTHER THREAD. A fit holds its problem's lock for as long
as it runs, and the progress route deliberately does not take that lock - it
would learn a fit had finished only after it had. So the log is the one piece of
a problem that two threads touch at once, and it guards itself.

WHY THE CLOCK IS PASSED IN. Every rule here is about time: when a sample
arrived, how long the operation has run, whether a snapshot is due. With Now
read inside, each of those tests would sleep, and the ones about "not again
within half a second" would be timing tests - slow when they pass and flaky when
the machine is busy.

WHY SNAPSHOTS ARE RATIONED. A snapshot rebuilds the model's computed profile and
copies every curve. The engine used to do that on every improvement of every fit
for an animation nobody was watching; now it does it only while somebody asks,
and no more than twice a second however fast the minimizer improves.
}
unit testcase_fit_progress_log;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_points_json, fit_progress_json, fit_progress_log, run_clock;

type
    TFitProgressLogTest = class(TTestCase)
    private
        FLog: TFitProgressLog;
        { A moment ASeconds after an arbitrary fixed origin. }
        function At(ASeconds: double): TRunTime;
        function ReportAt(ASeconds: double; ASince: longint = 0;
            AWithSnapshot: boolean = False): TFitProgressReport;
        procedure StoreOneCurveSnapshot(ASeconds: double);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What the operation is minimising, and with what.
        procedure AnOperationRecordsItsObjectiveAndEngine;
        procedure AndTheyAreForgottenWithTheOperationThatSetThem;
        //  The operation.
        procedure NothingIsReportedBeforeAnOperationStarts;
        procedure StartingMakesItBusy;
        procedure TheClockRunsWhileItIsBusy;
        procedure FinishingStopsTheClock;
        procedure FinishingWithNothingStartedIsHarmless;

        //  Samples.
        procedure ASampleRecordsWhenItArrived;
        procedure ASampleOutsideAnOperationIsIgnored;
        procedure ANonFiniteValueIsNotRecorded;
        procedure ARiseIsRecordedLikeAFall;
        procedure OnlyTheSamplesSinceASeqAreReported;
        procedure TheNextSeqIsOnePastTheLastSample;
        procedure SeqKeepsCountingAcrossOperations;
        procedure StartingAgainForgetsThePreviousSamples;
        procedure TheSamplesOutliveTheOperationThatMadeThem;

        //  A long fit.
        procedure WhenFullTheLogThinsWithoutExceedingItsCapacity;
        procedure ThinningKeepsTheFirstAndTheLatestSample;
        procedure ThinningKeepsTheOrder;

        //  Snapshots.
        procedure ASnapshotIsNotDueUnlessSomeoneAsked;
        procedure ItIsDueAtOnceWhenSomebodyHas;
        procedure ButNotAgainWithinTheInterval;
        procedure AndAgainOnceTheIntervalHasPassed;
        procedure NotOnceTheWatcherHasGoneQuiet;
        procedure NorOutsideAnOperation;
        procedure ASnapshotIsReportedOnlyToWhoeverAsksForOne;
        procedure ItNamesTheSampleItWasTakenAt;
        procedure StartingAgainDropsThePreviousSnapshot;
        //  The starting model, stored before the first improvement.
        procedure TheStartingModelIsReportedAsAFrame;
        procedure AndDoesNotHoldUpTheFirstImprovementsFrame;
    end;

implementation

const
    ORIGIN = 45000.0;

function TFitProgressLogTest.At(ASeconds: double): TRunTime;
begin
    Result := ORIGIN + ASeconds;
end;

function TFitProgressLogTest.ReportAt(ASeconds: double; ASince: longint;
    AWithSnapshot: boolean): TFitProgressReport;
begin
    Result := FLog.Report(At(ASeconds), ASince, AWithSnapshot);
end;

procedure TFitProgressLogTest.StoreOneCurveSnapshot(ASeconds: double);
var
    Profile, Delta: TPointsData;
    Curves: TFitProgressCurves;
begin
    Profile := Default(TPointsData);
    SetLength(Profile.X, 1);
    SetLength(Profile.Y, 1);
    Delta := Default(TPointsData);
    SetLength(Curves, 1);
    Curves[0].Id := 'C1';
    FLog.StoreSnapshot(At(ASeconds), Profile, Delta, Curves);
end;

procedure TFitProgressLogTest.SetUp;
begin
    FLog := TFitProgressLog.Create(8);
end;

procedure TFitProgressLogTest.TearDown;
begin
    FreeAndNil(FLog);
end;

{ The starting model's frame at ASeconds: one curve, marked as the start. }
procedure StoreStartingSnapshot(ALog: TFitProgressLog; ANow: TRunTime);
var
    Profile, Delta: TPointsData;
    Curves: TFitProgressCurves;
begin
    Profile := Default(TPointsData);
    Delta := Default(TPointsData);
    SetLength(Curves, 1);
    Curves[0].Id := 'C1';
    ALog.StoreSnapshot(ANow, Profile, Delta, Curves, True);
end;

procedure TFitProgressLogTest.TheStartingModelIsReportedAsAFrame;
begin
    //  A FIT SHORTER THAN TWO POLLS was drawn or not by chance: its frame
    //  came from its first improvement, and whether a poll landed between
    //  that and the end was a race. The starting model is there from the
    //  first moment, so any poll that finds the fit running has a frame.
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(0));
    StoreStartingSnapshot(FLog, At(0.01));
    AssertTrue('a frame', ReportAt(0.02, 0, True).HasSnapshot);
end;

procedure TFitProgressLogTest.AndDoesNotHoldUpTheFirstImprovementsFrame;
begin
    //  Taken before any improvement, it must not count against the ration:
    //  the frame the user most wants is the first one that moved.
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(0));
    StoreStartingSnapshot(FLog, At(0.01));
    AssertTrue('the first improvement still gets one',
        FLog.SnapshotDue(At(0.05)));
end;

{ ------------------------------- the operation ------------------------------ }

procedure TFitProgressLogTest.NothingIsReportedBeforeAnOperationStarts;
var
    R: TFitProgressReport;
begin
    R := ReportAt(5);
    AssertFalse('not busy', R.Busy);
    AssertEquals('no samples', 0, Length(R.Samples));
    AssertEquals('no time has passed', 0.0, R.Elapsed, 1e-9);
    AssertEquals('no seq issued', 0, R.NextSeq);
end;

procedure TFitProgressLogTest.StartingMakesItBusy;
begin
    FLog.Start(At(0));
    AssertTrue('busy', ReportAt(0).Busy);
end;

procedure TFitProgressLogTest.TheClockRunsWhileItIsBusy;
begin
    FLog.Start(At(10));
    AssertEquals('measured from the start', 2.5, ReportAt(12.5).Elapsed, 1e-6);
end;

procedure TFitProgressLogTest.FinishingStopsTheClock;
var
    R: TFitProgressReport;
begin
    //  A poll that arrives after the fit must say how long the fit TOOK, not how
    //  long ago it started.
    FLog.Start(At(10));
    FLog.Finish(At(13));
    R := ReportAt(60);
    AssertFalse('no longer busy', R.Busy);
    AssertEquals('frozen at the finish', 3.0, R.Elapsed, 1e-6);
end;

procedure TFitProgressLogTest.FinishingWithNothingStartedIsHarmless;
begin
    //  Every operation finishes through one path, fits or not; only fits start.
    FLog.Finish(At(3));
    AssertFalse('not busy', ReportAt(4).Busy);
    AssertEquals('no time', 0.0, ReportAt(4).Elapsed, 1e-9);
end;

{ --------------------------------- samples ---------------------------------- }

procedure TFitProgressLogTest.ASampleRecordsWhenItArrived;
var
    R: TFitProgressReport;
begin
    FLog.Start(At(100));
    FLog.Add(At(101.5), 0.25);
    R := ReportAt(102);
    AssertEquals('one', 1, Length(R.Samples));
    AssertEquals('its value', 0.25, R.Samples[0].Value, 1e-12);
    AssertEquals('when, from the start', 1.5, R.Samples[0].Elapsed, 1e-6);
    AssertEquals('the first seq', 0, R.Samples[0].Seq);
end;

procedure TFitProgressLogTest.ASampleOutsideAnOperationIsIgnored;
begin
    //  There is no operation to place it in, and a value with no start to
    //  measure its time from would be drawn somewhere arbitrary.
    FLog.Add(At(1), 0.5);
    AssertEquals('before any start', 0, Length(ReportAt(2).Samples));
    FLog.Start(At(3));
    FLog.Finish(At(4));
    FLog.Add(At(5), 0.5);
    AssertEquals('after the finish', 0, Length(ReportAt(6).Samples));
end;

procedure TFitProgressLogTest.ANonFiniteValueIsNotRecorded;
begin
    //  Strict JSON cannot carry one, and a reply the client cannot parse would
    //  blank the whole chart over a single bad evaluation.
    FLog.Start(At(0));
    FLog.Add(At(1), NaN);
    FLog.Add(At(2), Infinity);
    FLog.Add(At(3), 0.5);
    AssertEquals('only the finite one', 1, Length(ReportAt(4).Samples));
end;

procedure TFitProgressLogTest.ARiseIsRecordedLikeAFall;
begin
    //  An automatic run starts each stage afresh; the log records what each
    //  stage reported, not only what beat the stage before.
    FLog.Start(At(0));
    FLog.Add(At(1), 0.1);
    FLog.Add(At(2), 0.5);
    AssertEquals('both', 2, Length(ReportAt(3).Samples));
end;

procedure TFitProgressLogTest.OnlyTheSamplesSinceASeqAreReported;
var
    R: TFitProgressReport;
begin
    FLog.Start(At(0));
    FLog.Add(At(1), 0.4);
    FLog.Add(At(2), 0.3);
    FLog.Add(At(3), 0.2);
    R := ReportAt(4, 2);
    AssertEquals('only the ones from seq 2', 1, Length(R.Samples));
    AssertEquals('which is the last', 0.2, R.Samples[0].Value, 1e-12);
end;

procedure TFitProgressLogTest.TheNextSeqIsOnePastTheLastSample;
begin
    FLog.Start(At(0));
    FLog.Add(At(1), 0.4);
    FLog.Add(At(2), 0.3);
    AssertEquals('two issued', 2, ReportAt(3).NextSeq);
    AssertEquals('and asking from there gets nothing', 0,
        Length(ReportAt(3, 2).Samples));
end;

procedure TFitProgressLogTest.SeqKeepsCountingAcrossOperations;
var
    R: TFitProgressReport;
begin
    //  A poll from the previous fit still in flight carries that fit's seq. If
    //  the next fit numbered from zero again, that poll would skip its first
    //  samples; counting on means it can only ever ask for too few, never lose
    //  the new ones.
    FLog.Start(At(0));
    FLog.Add(At(1), 0.4);
    FLog.Add(At(2), 0.3);
    FLog.Finish(At(3));
    FLog.Start(At(4));
    FLog.Add(At(5), 0.9);
    R := ReportAt(6);
    AssertEquals('one sample in this operation', 1, Length(R.Samples));
    AssertEquals('numbered after the last one', 2, R.Samples[0].Seq);
end;

procedure TFitProgressLogTest.StartingAgainForgetsThePreviousSamples;
begin
    FLog.Start(At(0));
    FLog.Add(At(1), 0.4);
    FLog.Start(At(2));
    AssertEquals('a new operation has no history', 0,
        Length(ReportAt(3).Samples));
end;

procedure TFitProgressLogTest.TheSamplesOutliveTheOperationThatMadeThem;
begin
    //  The client reads the last of them after the fit returns - that final
    //  poll is how the chart's last point is the result the user is shown.
    FLog.Start(At(0));
    FLog.Add(At(1), 0.4);
    FLog.Finish(At(2));
    AssertEquals('still there', 1, Length(ReportAt(3).Samples));
end;

{ ---------------------------------- a long fit ------------------------------ }

procedure TFitProgressLogTest.WhenFullTheLogThinsWithoutExceedingItsCapacity;
var
    i: longint;
begin
    FLog.Start(At(0));
    for i := 1 to 100 do
    begin
        FLog.Add(At(i), 1 / i);
        AssertTrue(Format('within capacity after %d samples', [i]),
            Length(ReportAt(i).Samples) <= 8);
    end;
    AssertTrue('and not emptied either', Length(ReportAt(101).Samples) >= 4);
end;

procedure TFitProgressLogTest.ThinningKeepsTheFirstAndTheLatestSample;
var
    i: longint;
    R: TFitProgressReport;
begin
    //  THE TWO THAT MUST SURVIVE. The first is where the improvement is measured
    //  from; the latest is what the fit has reached. Losing either would draw a
    //  chart that says something the fit did not do.
    FLog.Start(At(0));
    for i := 1 to 100 do
        FLog.Add(At(i), 1 / i);
    R := ReportAt(101);
    AssertEquals('the first', 1.0, R.Samples[0].Value, 1e-12);
    AssertEquals('the latest', 0.01, R.Samples[High(R.Samples)].Value, 1e-12);
    AssertEquals('and the next seq still counts every one', 100, R.NextSeq);
end;

procedure TFitProgressLogTest.ThinningKeepsTheOrder;
var
    i: longint;
    R: TFitProgressReport;
begin
    FLog.Start(At(0));
    for i := 1 to 50 do
        FLog.Add(At(i), 1 / i);
    R := ReportAt(51);
    for i := 1 to High(R.Samples) do
        AssertTrue(Format('sample %d follows the one before', [i]),
            (R.Samples[i].Seq > R.Samples[i - 1].Seq) and
            (R.Samples[i].Elapsed > R.Samples[i - 1].Elapsed));
end;

{ --------------------------------- snapshots -------------------------------- }

procedure TFitProgressLogTest.ASnapshotIsNotDueUnlessSomeoneAsked;
begin
    FLog.Start(At(0));
    AssertFalse('nobody is animating', FLog.SnapshotDue(At(1)));
end;

procedure TFitProgressLogTest.ItIsDueAtOnceWhenSomebodyHas;
begin
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(1));
    AssertTrue('the first improvement after asking', FLog.SnapshotDue(At(1.1)));
end;

procedure TFitProgressLogTest.ButNotAgainWithinTheInterval;
begin
    //  Rationed, because a minimizer can improve hundreds of times a second and
    //  each snapshot rebuilds the whole computed profile.
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(1));
    StoreOneCurveSnapshot(1.1);
    FLog.AskForSnapshot(At(1.2));
    AssertFalse('too soon', FLog.SnapshotDue(At(1.3)));
end;

procedure TFitProgressLogTest.AndAgainOnceTheIntervalHasPassed;
begin
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(1));
    StoreOneCurveSnapshot(1.1);
    FLog.AskForSnapshot(At(1.5));
    AssertTrue('half a second on', FLog.SnapshotDue(At(1.7)));
end;

procedure TFitProgressLogTest.NotOnceTheWatcherHasGoneQuiet;
begin
    //  A client that switched animation off stops asking. The engine must stop
    //  paying for it without being told.
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(1));
    AssertFalse('nobody has asked for three seconds', FLog.SnapshotDue(At(4)));
end;

procedure TFitProgressLogTest.NorOutsideAnOperation;
begin
    FLog.AskForSnapshot(At(1));
    AssertFalse('nothing is running', FLog.SnapshotDue(At(1.1)));
end;

procedure TFitProgressLogTest.ASnapshotIsReportedOnlyToWhoeverAsksForOne;
begin
    FLog.Start(At(0));
    FLog.Add(At(0.5), 0.5);
    FLog.AskForSnapshot(At(1));
    StoreOneCurveSnapshot(1.1);
    AssertFalse('not to a plain poll', ReportAt(1.2).HasSnapshot);
    AssertTrue('to one that asks', ReportAt(1.2, 0, True).HasSnapshot);
    AssertEquals('with its curve', 'C1',
        ReportAt(1.2, 0, True).Snapshot.Curves[0].Id);
end;

procedure TFitProgressLogTest.ItNamesTheSampleItWasTakenAt;
begin
    FLog.Start(At(0));
    FLog.Add(At(0.5), 0.5);
    FLog.Add(At(0.6), 0.4);
    FLog.AskForSnapshot(At(1));
    StoreOneCurveSnapshot(1.1);
    AssertEquals('the latest sample when it was taken', 1,
        ReportAt(1.2, 0, True).Snapshot.Seq);
end;

procedure TFitProgressLogTest.StartingAgainDropsThePreviousSnapshot;
begin
    //  The previous fit's model is not a picture of this one.
    FLog.Start(At(0));
    FLog.AskForSnapshot(At(1));
    StoreOneCurveSnapshot(1.1);
    FLog.Start(At(2));
    AssertFalse('gone', ReportAt(2.1, 0, True).HasSnapshot);
end;


procedure TFitProgressLogTest.AnOperationRecordsItsObjectiveAndEngine;
var
    R: TFitProgressReport;
begin
    //  RECORDED WHERE THE OPERATION STARTS, which is on the fit's own thread.
    //  The progress route is answered on another one without the problem's
    //  lock, so it must not read them off the engine as it replies.
    FLog.Start(At(0), 'R-factor', 'Downhill Simplex');
    R := ReportAt(1, 0, False);
    AssertEquals('the objective', 'R-factor', R.LossName);
    AssertEquals('the engine', 'Downhill Simplex', R.EngineName);
end;

procedure TFitProgressLogTest.AndTheyAreForgottenWithTheOperationThatSetThem;
var
    R: TFitProgressReport;
begin
    //  A later operation under another objective must not be described by the
    //  previous one's.
    FLog.Start(At(0), 'R-factor', 'Downhill Simplex');
    FLog.Finish(At(1));
    FLog.Start(At(2), 'Sum of squares', 'Python lmfit');
    R := ReportAt(3, 0, False);
    AssertEquals('the objective', 'Sum of squares', R.LossName);
    AssertEquals('the engine', 'Python lmfit', R.EngineName);
end;

initialization
    //  A class over plain values and an injected clock.
    RegisterTest('unit', TFitProgressLogTest);
end.
