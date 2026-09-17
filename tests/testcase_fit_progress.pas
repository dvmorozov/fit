// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the chart area shows while a fit runs.)

THE RULE THIS DEFENDS: THE USER NEVER WATCHES AN EMPTY CHART. A fit used to clear
the chart and draw nothing until it was over, which on a long fit is a window
that looks broken. Every decision about what is shown instead - the live loss
curve, the model redrawn as it improves, or a plain statement that this engine
reports nothing until it finishes - is made here, over plain values, so each can
be asserted without a chart, a form or a running fit.

THE NUMBERS AGREE WITH THE ONES SHOWN AFTERWARDS. The elapsed time and the
R-factor are formatted exactly as the server formats them once the fit is over,
so the status bar does not change its shape the moment a fit ends.
}
unit testcase_fit_progress;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_progress_json, fit_progress;

type
    TFitProgressTest = class(TTestCase)
    private
        FModel: TFitProgressModel;
        function Report(const AValues: array of double; AFirstSeq: longint;
            AElapsed: double = 1): TFitProgressReport;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The header names what is being minimised, and by what.
        procedure TheHeaderNamesTheObjectiveAndTheEngine;
        procedure OrOnlyTheObjectiveWhenThatIsAllTheServerSays;
        procedure OrOnlyTheEngine;
        procedure ALogThatHasThinnedItsSamplesStillMovesTheSeqOn;
        procedure WithNothingRecordedThereIsNoLatestValue;
        procedure AnUnnamedObjectiveLeavesTheHeaderAsItWas;
        procedure AndTheModelCarriesThemFromTheReport;
        //  What a finished fit says about itself in the log.
        procedure AFitThatWasWatchedSaysWhatItDrew;
        procedure AFitTooShortToDrawAnythingIsNotComplainedAbout;
        procedure ALongFitThatDrewNoFrameIsAComplaint;
        procedure AndTheComplaintSaysTheFitWasNeverPolled;
        procedure PolledButNeverAnsweredSaysThatInstead;
        procedure AnsweredButWithoutAFrameOfTheModelSaysThat;
        procedure WithoutAnimationNoFrameOfTheModelIsExpected;
        procedure AnOperationWithNoViewSaysNothingAtAll;
        procedure TheWindowKeepsItsOwnClockAsWellAsTheEngines;
        procedure AndItRunsOnWhenTheEngineStopsReporting;
        procedure AnInvisibleFitSaysHowOftenTheWindowTried;
        procedure AFitDrawnFarSlowerThanItIsPolledIsAComplaint;
        procedure AndAVisibleOneDoesNotCarryTheCounts;
        //  The model of one fit's progress.
        procedure NothingIsInProgressToBeginWith;
        procedure StartingMakesItActiveWithNothingRecorded;
        procedure AReportsSamplesAreKept;
        procedure TheNextPollAsksFromWhereTheReportLeftOff;
        procedure ASampleAlreadyHeldIsNotKeptTwice;
        procedure ALateReportDoesNotMoveTheNextSeqBack;
        procedure FinishingEndsIt;
        procedure StartingAgainForgetsThePreviousFit;
        procedure TheClockFollowsTheReport;
        procedure AWorseSampleIsKeptAndDrawnToo;
        procedure AReportFromBeforeThisFitBeganIsNotDrawn;
        procedure OnceThisFitHasBeenSeenRunningItsLastReportIsKept;
        procedure AComputationIsTimedWithoutAView;

        //  The numbers the user reads.
        procedure TheElapsedTimeReadsAsTheServerWritesIt;
        procedure TheRFactorReadsAsTheServerWritesIt;
        procedure WithNothingRecordedThereIsNoRFactorToShow;
        procedure TheImprovementIsMeasuredFromTheFirstSample;
        procedure OneSampleIsNoImprovement;

        //  What the chart area shows.
        procedure TheLossIsDrawnOnALogarithmicScale;
        procedure ALossOfZeroOrLessIsDrawnAtTheFloor;
        procedure BeforeAnythingIsRecordedTheFitIsStarting;
        procedure AnEngineThatStaysSilentIsSaidToBeSo;
        procedure OnceALossArrivesItIsDrawn;
        procedure InAnimationTheModelIsDrawnInstead;
        procedure AnimationStartsAndFallsSilentLikeAnythingElse;
        procedure TheLossChartIsShownOnlyWhenThereIsALossToDraw;
        procedure TheChartPlotsEverySampleAgainstItsTime;
        procedure TheAxesSayWhatTheyShow;

        //  What the header says.
        procedure EveryModeHasAHeaderOfItsOwn;
        procedure ARunningFitSaysHowToStopIt;
        procedure TheLossHeaderCarriesTheClockTheValueAndTheImprovement;
        procedure WithOneSampleTheHeaderClaimsNoImprovement;
    end;

implementation

procedure TFitProgressTest.SetUp;
begin
    FModel := TFitProgressModel.Create;
end;

procedure TFitProgressTest.TearDown;
begin
    FreeAndNil(FModel);
end;

function TFitProgressTest.Report(const AValues: array of double;
    AFirstSeq: longint; AElapsed: double): TFitProgressReport;
var
    i: longint;
begin
    Result := Default(TFitProgressReport);
    Result.Busy := True;
    Result.Elapsed := AElapsed;
    SetLength(Result.Samples, Length(AValues));
    for i := 0 to High(AValues) do
    begin
        Result.Samples[i].Seq := AFirstSeq + i;
        Result.Samples[i].Elapsed := (i + 1) * 0.5;
        Result.Samples[i].Value := AValues[i];
    end;
    Result.NextSeq := AFirstSeq + Length(AValues);
end;

{ ------------------------------ the model ---------------------------------- }

procedure TFitProgressTest.NothingIsInProgressToBeginWith;
begin
    AssertFalse('not active', FModel.Active);
    AssertFalse('no samples', FModel.HasSamples);
end;

procedure TFitProgressTest.StartingMakesItActiveWithNothingRecorded;
begin
    FModel.Start;
    AssertTrue('active', FModel.Active);
    AssertFalse('no samples', FModel.HasSamples);
    AssertEquals('asks from the beginning', 0, FModel.NextSeq);
end;

procedure TFitProgressTest.AReportsSamplesAreKept;
begin
    FModel.Start;
    FModel.Apply(Report([0.5, 0.25], 0));
    AssertEquals('both', 2, FModel.SampleCount);
    AssertEquals('the latest', 0.25, FModel.LatestValue, 1e-12);
end;

procedure TFitProgressTest.TheNextPollAsksFromWhereTheReportLeftOff;
begin
    FModel.Start;
    FModel.Apply(Report([0.5, 0.25], 4));
    AssertEquals('one past the last', 6, FModel.NextSeq);
end;

procedure TFitProgressTest.ASampleAlreadyHeldIsNotKeptTwice;
begin
    //  Two polls can overlap - one sent before the previous answer arrived -
    //  and a sample kept twice draws the curve doubling back on itself.
    FModel.Start;
    FModel.Apply(Report([0.5, 0.25], 0));
    FModel.Apply(Report([0.25, 0.125], 1));
    AssertEquals('three distinct samples', 3, FModel.SampleCount);
    AssertEquals('ending on the newest', 0.125, FModel.LatestValue, 1e-12);
end;

procedure TFitProgressTest.ALateReportDoesNotMoveTheNextSeqBack;
begin
    FModel.Start;
    FModel.Apply(Report([0.5, 0.25, 0.125], 0));
    FModel.Apply(Report([0.5], 0));
    AssertEquals('still asking from the newest', 3, FModel.NextSeq);
end;

procedure TFitProgressTest.FinishingEndsIt;
begin
    FModel.Start;
    FModel.Finish;
    AssertFalse('no longer active', FModel.Active);
end;

procedure TFitProgressTest.StartingAgainForgetsThePreviousFit;
begin
    FModel.Start;
    FModel.Apply(Report([0.5, 0.25], 0));
    FModel.Finish;
    FModel.Start;
    AssertFalse('a new fit has drawn nothing yet', FModel.HasSamples);
end;

procedure TFitProgressTest.TheClockFollowsTheReport;
begin
    //  THE SERVER'S CLOCK, not this machine's: the fit may run elsewhere, and
    //  the two clocks need not agree about when it began.
    FModel.Start;
    FModel.Apply(Report([0.5], 0, 12.5));
    AssertEquals('as reported', 12.5, FModel.Elapsed, 1e-9);
end;

procedure TFitProgressTest.AWorseSampleIsKeptAndDrawnToo;
begin
    //  NO MONOTONE ASSUMPTION. An automatic run fits in stages and each stage
    //  starts afresh, so the loss rises before it falls again; dropping the rise
    //  would draw a fit that never happened.
    FModel.Start;
    FModel.Apply(Report([0.1, 0.5, 0.2], 0));
    AssertEquals('all three', 3, FModel.SampleCount);
    AssertEquals('ending where the fit is', 0.2, FModel.LatestValue, 1e-12);
    AssertEquals('the rise is drawn', 3, Length(FModel.View(False).Y));
end;

procedure TFitProgressTest.AReportFromBeforeThisFitBeganIsNotDrawn;
var
    R: TFitProgressReport;
begin
    //  THE PREVIOUS FIT'S SAMPLES. The first poll can reach the server before the
    //  request that starts this fit has, and the log still holds the last one.
    //  Drawn, they would be this fit's first second - and its R-factor.
    FModel.Start;
    R := Report([0.5, 0.25], 7);
    R.Busy := False;
    FModel.Apply(R);
    AssertFalse('not this fit''s', FModel.HasSamples);
    AssertEquals('and the next poll still asks from the start', 0, FModel.NextSeq);
end;

procedure TFitProgressTest.OnceThisFitHasBeenSeenRunningItsLastReportIsKept;
var
    R: TFitProgressReport;
begin
    //  The other side of the rule above: the poll that finds this fit just over
    //  carries its last samples, and those are this fit's.
    FModel.Start;
    FModel.Apply(Report([0.5], 0));
    R := Report([0.25], 1);
    R.Busy := False;
    FModel.Apply(R);
    AssertEquals('both', 2, FModel.SampleCount);
end;

procedure TFitProgressTest.AComputationIsTimedWithoutAView;
begin
    //  Proposing bounds, background or positions runs on the server too, and its
    //  clock is worth showing - but a progress view over the data it is
    //  proposing on is not.
    FModel.Start(False);
    AssertTrue('timed', FModel.Active);
    AssertFalse('without a view', FModel.ShowsView);
    FModel.Start;
    AssertTrue('a fit has one', FModel.ShowsView);
end;

{ --------------------------- what the user reads ---------------------------- }

procedure TFitProgressTest.TheElapsedTimeReadsAsTheServerWritesIt;
begin
    //  The same shape GetCalcTimeStr produces when the fit is over, so the
    //  status bar does not change its format as the fit ends.
    AssertEquals('0 day(s) 01:02:05', FormatElapsed(3725.9));
    AssertEquals('0 day(s) 00:00:00', FormatElapsed(0));
    AssertEquals('1 day(s) 00:00:01', FormatElapsed(86401));
end;

procedure TFitProgressTest.TheRFactorReadsAsTheServerWritesIt;
begin
    FModel.Start;
    FModel.Apply(Report([0.0123], 0));
    AssertEquals(FloatToStrF(0.0123, ffFixed, 10, 8), FModel.RFactorText);
end;

procedure TFitProgressTest.WithNothingRecordedThereIsNoRFactorToShow;
begin
    //  EMPTY, not zero. Zero is a perfect fit, and a fit that has not reported
    //  yet is the opposite of one.
    FModel.Start;
    AssertEquals('', FModel.RFactorText);
end;

procedure TFitProgressTest.TheImprovementIsMeasuredFromTheFirstSample;
begin
    FModel.Start;
    FModel.Apply(Report([0.5, 0.3, 0.125], 0));
    AssertEquals('three quarters below the start', 75.0,
        FModel.ImprovementPercent, 1e-9);
end;

procedure TFitProgressTest.OneSampleIsNoImprovement;
begin
    FModel.Start;
    FModel.Apply(Report([0.5], 0));
    AssertFalse('nothing to compare with', FModel.HasImprovement);
end;

{ ------------------------- what the chart area shows ------------------------ }

procedure TFitProgressTest.TheLossIsDrawnOnALogarithmicScale;
begin
    //  A loss falls through decades; on a linear scale everything after the
    //  first second is a flat line at the bottom.
    AssertEquals(-2.0, LossChartValue(0.01), 1e-12);
    AssertEquals(1.0, LossChartValue(10), 1e-12);
end;

procedure TFitProgressTest.ALossOfZeroOrLessIsDrawnAtTheFloor;
begin
    //  A perfect fit reaches zero, and log 0 is not a number a chart can place.
    AssertEquals('zero', Log10(LOSS_CHART_FLOOR), LossChartValue(0), 1e-12);
    AssertEquals('negative', Log10(LOSS_CHART_FLOOR), LossChartValue(-1), 1e-12);
end;

procedure TFitProgressTest.BeforeAnythingIsRecordedTheFitIsStarting;
begin
    AssertTrue(ProgressViewModeFor(False, False, 0.5) = pvStarting);
end;

procedure TFitProgressTest.AnEngineThatStaysSilentIsSaidToBeSo;
begin
    //  A backend that reports nothing until it finishes. Saying so is honest;
    //  "Starting..." for a minute is not.
    AssertTrue(ProgressViewModeFor(False, False,
        NO_PROGRESS_GRACE_SECONDS + 1) = pvNoIntermediateProgress);
end;

procedure TFitProgressTest.OnceALossArrivesItIsDrawn;
begin
    AssertTrue(ProgressViewModeFor(False, True, 0.5) = pvLossChart);
    AssertTrue('however long it took',
        ProgressViewModeFor(False, True, 100) = pvLossChart);
end;

procedure TFitProgressTest.InAnimationTheModelIsDrawnInstead;
begin
    AssertTrue(ProgressViewModeFor(True, True, 1) = pvAnimatedCurves);
end;

procedure TFitProgressTest.AnimationStartsAndFallsSilentLikeAnythingElse;
begin
    AssertTrue('starting', ProgressViewModeFor(True, False, 0.5) = pvStarting);
    AssertTrue('silent', ProgressViewModeFor(True, False,
        NO_PROGRESS_GRACE_SECONDS + 1) = pvNoIntermediateProgress);
end;

procedure TFitProgressTest.TheLossChartIsShownOnlyWhenThereIsALossToDraw;
begin
    //  An empty loss chart is the empty screen this exists to replace. Until a
    //  sample arrives the data stays on screen under the header.
    FModel.Start;
    AssertFalse('nothing to draw yet', FModel.View(False).LossChartVisible);
    FModel.Apply(Report([0.5], 0));
    AssertTrue('something to draw', FModel.View(False).LossChartVisible);
    AssertFalse('but not over an animated model',
        FModel.View(True).LossChartVisible);
end;

procedure TFitProgressTest.TheChartPlotsEverySampleAgainstItsTime;
var
    V: TFitProgressView;
begin
    FModel.Start;
    FModel.Apply(Report([0.1, 0.01], 0));
    V := FModel.View(False);
    AssertEquals('every sample', 2, Length(V.X));
    AssertEquals('the same count of values', 2, Length(V.Y));
    AssertEquals('its time', 0.5, V.X[0], 1e-12);
    AssertEquals('its time', 1.0, V.X[1], 1e-12);
    AssertEquals('its loss, logarithmically', -1.0, V.Y[0], 1e-12);
    AssertEquals('its loss, logarithmically', -2.0, V.Y[1], 1e-12);
end;

procedure TFitProgressTest.TheAxesSayWhatTheyShow;
var
    V: TFitProgressView;
begin
    V := FModel.View(False);
    AssertTrue('time', Pos('s', V.XAxisLabel) > 0);
    AssertTrue('a logarithm of the R-factor',
        (Pos('log', V.YAxisLabel) > 0) and (Pos('R-factor', V.YAxisLabel) > 0));
end;

{ ------------------------------ the header ---------------------------------- }

procedure TFitProgressTest.EveryModeHasAHeaderOfItsOwn;
var
    M, N: TFitProgressViewMode;
begin
    for M := Low(TFitProgressViewMode) to High(TFitProgressViewMode) do
    begin
        AssertTrue('a header for every mode',
            ProgressHeaderText(M, '0 day(s) 00:00:01', '0.5', 0, False) <> '');
        for N := Succ(M) to High(TFitProgressViewMode) do
            if not ((M = pvLossChart) and (N = pvAnimatedCurves)) then
                AssertTrue('distinct headers',
                    ProgressHeaderText(M, 't', 'v', 0, False) <>
                    ProgressHeaderText(N, 't', 'v', 0, False));
    end;
end;

procedure TFitProgressTest.ARunningFitSaysHowToStopIt;
var
    M: TFitProgressViewMode;
begin
    for M := Low(TFitProgressViewMode) to High(TFitProgressViewMode) do
        AssertTrue('the way out is named',
            Pos('Stop', ProgressHeaderText(M, 't', 'v', 0, False)) > 0);
end;

procedure TFitProgressTest.TheLossHeaderCarriesTheClockTheValueAndTheImprovement;
var
    H: string;
begin
    H := ProgressHeaderText(pvLossChart, '0 day(s) 00:00:12', '0.01234567',
        87.4, True);
    AssertTrue('the clock: ' + H, Pos('00:00:12', H) > 0);
    AssertTrue('the value: ' + H, Pos('0.01234567', H) > 0);
    AssertTrue('the improvement, rounded: ' + H, Pos('87 %', H) > 0);
end;

procedure TFitProgressTest.WithOneSampleTheHeaderClaimsNoImprovement;
var
    H: string;
begin
    H := ProgressHeaderText(pvLossChart, 't', '0.5', 0, False);
    AssertEquals('no percentage at all: ' + H, 0, Pos('%', H));
end;


{ ---- what a finished fit says about itself -------------------------------- }

procedure TFitProgressTest.AFitThatWasWatchedSaysWhatItDrew;
var
    S: string;
begin
    S := FitEpilogueText(True, True, 4.0, 40, 30, 8, 0, 0, 0);
    AssertTrue('the seconds: ' + S, Pos('4', S) > 0);
    AssertTrue('the frames of the model: ' + S, Pos('8', S) > 0);
end;

procedure TFitProgressTest.AFitTooShortToDrawAnythingIsNotComplainedAbout;
begin
    //  A fit that was over before the first tick showed nothing because there
    //  was nothing to show. Said in the epilogue, never as a complaint.
    AssertFalse(FitWasInvisible(True, True, 0.2, 0, 0, 0));
end;

procedure TFitProgressTest.ALongFitThatDrewNoFrameIsAComplaint;
begin
    //  THE DEFECT ITS USER REPORTED: several seconds of a window that did not
    //  change. Whatever the cause, the program should say so itself rather than
    //  leave the only record in the user's memory.
    AssertTrue(FitWasInvisible(True, True, 6.0, 0, 0, 0));
end;

procedure TFitProgressTest.AndTheComplaintSaysTheFitWasNeverPolled;
var
    S: string;
begin
    //  WHICH STAGE FAILED IS THE WHOLE VALUE OF THE LINE. No view drawn at all
    //  means the window never asked - a timer that did not fire, or a main
    //  thread that never came back to it.
    S := FitEpilogueText(True, True, 6.0, 0, 0, 0, 0, 0, 0);
    AssertTrue('names the stage: ' + S, Pos('never asked', S) > 0);
end;

procedure TFitProgressTest.PolledButNeverAnsweredSaysThatInstead;
var
    S: string;
begin
    //  Asked, and the engine had nothing: the fit reported no improvement, or
    //  the reply never carried one.
    S := FitEpilogueText(True, True, 6.0, 40, 0, 0, 0, 0, 0);
    AssertTrue('names the stage: ' + S, Pos('no value', S) > 0);
end;

procedure TFitProgressTest.AnsweredButWithoutAFrameOfTheModelSaysThat;
var
    S: string;
begin
    //  The loss chart was alive and the curves still did not move, which is
    //  Animation Mode failing on its own.
    S := FitEpilogueText(True, True, 6.0, 40, 30, 0, 0, 0, 0);
    AssertTrue('names the stage: ' + S, Pos('no frame of the model', S) > 0);
    AssertTrue('and it is a complaint', FitWasInvisible(True, True, 6.0, 40, 30, 0));
end;

procedure TFitProgressTest.WithoutAnimationNoFrameOfTheModelIsExpected;
begin
    //  The default mode deliberately leaves the model alone.
    AssertFalse(FitWasInvisible(True, False, 6.0, 40, 30, 0));
end;

procedure TFitProgressTest.AnOperationWithNoViewSaysNothingAtAll;
begin
    //  Computing curve bounds shows no progress view by design.
    AssertEquals('', FitEpilogueText(False, False, 6.0, 0, 0, 0, 0, 0, 0));
    AssertFalse(FitWasInvisible(False, False, 6.0, 0, 0, 0));
end;


procedure TFitProgressTest.AnInvisibleFitSaysHowOftenTheWindowTried;
var
    S: string;
begin
    //  WHICH HALF FAILED IS THE QUESTION. A timer that never ticked is a window
    //  problem; ticks without polls is a poll refusing itself; polls that failed
    //  is the engine or the connection. The numbers separate the three, and
    //  without them the line says only that nothing was drawn.
    S := FitEpilogueText(True, True, 6.0, 0, 0, 0, 3, 1, 2);
    AssertTrue('the ticks: ' + S, Pos('3 tick', S) > 0);
    AssertTrue('the polls: ' + S, Pos('1 poll', S) > 0);
    AssertTrue('the failures: ' + S, Pos('2 fail', S) > 0);
end;

procedure TFitProgressTest.AndAVisibleOneDoesNotCarryTheCounts;
var
    S: string;
begin
    //  A fit too short to judge says so and carries no diagnosis with it.
    S := FitEpilogueText(True, True, 0.2, 1, 0, 0, 2, 1, 0);
    AssertTrue('no tick count: ' + S, Pos('tick', S) = 0);
    //  One long enough carries them whatever the verdict, because "it worked"
    //  is worth as much as "it did not" when the next report arrives.
    S := FitEpilogueText(True, True, 6.0, 40, 30, 8, 60, 60, 0);
    AssertTrue('the ticks: ' + S, Pos('60 tick', S) > 0);
end;

procedure TFitProgressTest.AFitDrawnFarSlowerThanItIsPolledIsAComplaint;
begin
    //  THE SHAPE THE DEFECT ACTUALLY TOOK. Not zero frames - two, over
    //  twenty-two seconds, which is a still picture with a twitch in it. A rule
    //  that only fires at zero calls that a working fit, so the measure is the
    //  cadence the window promises: it polls ten times a second, and drawing
    //  under a tenth of what that would give is a window that stopped.
    AssertTrue('two frames in twenty-two seconds',
        FitWasInvisible(True, True, 22.0, 2, 3, 1));
    //  And the cadence met, or nearly, is not.
    AssertFalse('two hundred frames in twenty-two seconds',
        FitWasInvisible(True, True, 22.0, 200, 190, 180));
    AssertFalse('half of them is still a moving picture',
        FitWasInvisible(True, True, 22.0, 110, 100, 90));
end;


procedure TFitProgressTest.TheWindowKeepsItsOwnClockAsWellAsTheEngines;
var
    M: TFitProgressModel;
begin
    M := TFitProgressModel.Create;
    try
        M.Start(True, EncodeTime(10, 0, 0, 0));
        AssertEquals('two seconds later', 2.0,
            M.WatchedSecondsAt(EncodeTime(10, 0, 2, 0)), 0.01);
    finally
        M.Free;
    end;
end;

procedure TFitProgressTest.AndItRunsOnWhenTheEngineStopsReporting;
var
    M: TFitProgressModel;
    R: TFitProgressReport;
begin
    //  THE DIFFERENCE THAT MATTERS. The engine's clock arrives in the reports,
    //  so when the polls stop it stops - and the one moment the window needs a
    //  duration is exactly that one: to say how long a fit went unwatched.
    M := TFitProgressModel.Create;
    try
        M.Start(True, EncodeTime(10, 0, 0, 0));
        R := Default(TFitProgressReport);
        R.Busy := True;
        R.Elapsed := 0.2;
        M.Apply(R);
        AssertEquals('the engine stopped at a fifth of a second', 0.2,
            M.Elapsed, 0.01);
        AssertEquals('the window knows better', 22.0,
            M.WatchedSecondsAt(EncodeTime(10, 0, 22, 0)), 0.01);
    finally
        M.Free;
    end;
end;


procedure TFitProgressTest.TheHeaderNamesTheObjectiveAndTheEngine;
var
    S: string;
begin
    //  The same model fitted under another objective, or by another engine,
    //  converges differently - so a chart of the convergence that does not say
    //  which of them is being watched is a number without a unit.
    S := ProgressHeaderText(pvLossChart, '4 s', '0.012', 0, False,
        'Sum of squares', 'Python lmfit');
    //  AND THE TWO ROLES STAY APART. The number beside them is the R-factor,
    //  whatever the objective is - it is the quantity the status bar shows once
    //  the fit ends, and the live chart must end where that begins. A header
    //  that merely put the objective's name next to it would read as labelling
    //  it, so the objective is named as what is being MINIMISED.
    AssertTrue('the objective: ' + S, Pos('minimising Sum of squares', S) > 0);
    AssertTrue('the engine: ' + S, Pos('Python lmfit', S) > 0);
    AssertTrue('the number is still the R-factor: ' + S,
        Pos('R-factor 0.012', S) > 0);
    //  And it still says the things it always said.
    AssertTrue('the elapsed time: ' + S, Pos('4 s', S) > 0);
    AssertTrue('how to end it: ' + S, Pos('Stop', S) > 0);
end;

procedure TFitProgressTest.AnUnnamedObjectiveLeavesTheHeaderAsItWas;
var
    S: string;
begin
    //  An older server names neither, and the line must still read as a
    //  sentence rather than as one with a hole in it.
    S := ProgressHeaderText(pvLossChart, '4 s', '0.012', 0, False, '', '');
    AssertTrue('no empty brackets: ' + S, Pos('()', S) = 0);
    AssertTrue('no stray comma: ' + S, Pos(' ,', S) = 0);
    AssertTrue('still says what it is doing: ' + S, Pos('Fitting', S) > 0);
end;

procedure TFitProgressTest.AndTheModelCarriesThemFromTheReport;
var
    M: TFitProgressModel;
    R: TFitProgressReport;
begin
    M := TFitProgressModel.Create;
    try
        M.Start;
        R := Default(TFitProgressReport);
        R.Busy := True;
        R.LossName := 'Sum of squares';
        R.EngineName := 'Python lmfit';
        M.Apply(R);
        AssertTrue('the view names them: ' + M.View(False).Header,
            Pos('Python lmfit', M.View(False).Header) > 0);
    finally
        M.Free;
    end;
end;


procedure TFitProgressTest.OrOnlyTheObjectiveWhenThatIsAllTheServerSays;
var
    S: string;
begin
    //  A build whose minimiser kind is not in its registry names no engine, and
    //  the objective it does know must still reach the user.
    S := ProgressHeaderText(pvLossChart, '4 s', '0.012', 0, False,
        'Sum of squares', '');
    AssertTrue('the objective: ' + S, Pos('minimising Sum of squares', S) > 0);
    AssertTrue('and nothing dangling: ' + S, Pos(' with )', S) = 0);
end;

procedure TFitProgressTest.OrOnlyTheEngine;
var
    S: string;
begin
    S := ProgressHeaderText(pvLossChart, '4 s', '0.012', 0, False,
        '', 'Downhill Simplex');
    AssertTrue('the engine: ' + S, Pos('with Downhill Simplex', S) > 0);
    AssertTrue('and no empty objective: ' + S, Pos('minimising ,', S) = 0);
end;

procedure TFitProgressTest.ALogThatHasThinnedItsSamplesStillMovesTheSeqOn;
var
    M: TFitProgressModel;
    R: TFitProgressReport;
begin
    //  THE SEQ COMES FROM THE SERVER, not from what arrived. A long fit's log
    //  is thinned, so the samples in a reply can stop short of the seq it has
    //  issued - and asking from the last sample seen would then ask for
    //  everything the thinning dropped, over and over, for the rest of the fit.
    M := TFitProgressModel.Create;
    try
        M.Start;
        R := Default(TFitProgressReport);
        R.Busy := True;
        SetLength(R.Samples, 1);
        R.Samples[0].Seq := 0;
        R.Samples[0].Value := 1.0;
        R.NextSeq := 500;
        M.Apply(R);
        AssertEquals('asks from where the server is', 500, M.NextSeq);
    finally
        M.Free;
    end;
end;

procedure TFitProgressTest.WithNothingRecordedThereIsNoLatestValue;
var
    M: TFitProgressModel;
begin
    //  Read before the first report arrives - the state every fit starts in.
    M := TFitProgressModel.Create;
    try
        M.Start;
        AssertEquals('nothing yet', 0.0, M.LatestValue, 1e-12);
    finally
        M.Free;
    end;
end;

initialization
    //  Plain values: no chart, no service, no clock.
    RegisterTest('unit', TFitProgressTest);
end.
