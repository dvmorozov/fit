// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the chart area shows while a fit runs.)

THE RULE: THE USER NEVER WATCHES AN EMPTY CHART. A fit used to clear the chart and
draw nothing until it returned, which for a long fit is a window that looks
broken. What is shown instead is decided here, over plain values, and the window
only puts the answer on screen:

  * STARTING - the first moment, before the engine has said anything. The data
    stays drawn under a line saying the fit has begun.
  * LOSS CHART - the default once the engine reports. The R-factor reached so
    far against the time it took, on a logarithmic scale because a loss falls
    through decades and a linear axis flattens everything after the first
    second into the floor.
  * ANIMATED CURVES - when the user ticked Animation Mode. The model is redrawn
    over the data as the fit improves, and the loss chart stays out of the way.
  * NO INTERMEDIATE PROGRESS - an engine that says nothing until it is done
    (an older remote peer, say). Saying so is honest; "Starting" for a minute is
    not. The clock keeps running.

WHY THE LOSS CHART WAITS FOR A SAMPLE. An empty loss chart is the empty screen
this replaces. Until the first sample the data stays on screen under the header.

THE NUMBERS READ AS THE SERVER WRITES THEM. The elapsed time and the R-factor are
formatted exactly as GetCalcTimeStr and GetRFactorStr format them once the fit is
over, so the status bar does not change its shape the moment a fit ends.

THE CLOCK IS THE SERVER'S. The fit may run on another machine, and the two clocks
need not agree about when it began.
}
unit fit_progress;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, fit_progress_json, rfactor_text, run_clock;

const
    { How often the window asks.

      A HUNDRED MILLISECONDS, and the number is load-bearing rather than a taste:
      nothing can be shown of a fit that ends before the first poll, and an
      ordinary fit - a few curves over a few hundred points - is over in a few
      hundred. At a quarter of a second such a fit drew NOTHING, which is what
      Animation Mode looked like to the person who ticked it. The engine still
      builds a frame at most twice a second, so this costs polls, not model
      rebuilds, and the route is answered without waiting for the fit. }
    PROGRESS_POLL_INTERVAL_MS = 100;
    { How long a fit may report nothing before it is said to report nothing. }
    NO_PROGRESS_GRACE_SECONDS: double = 3.0;
    { Where a loss of zero or less is drawn: log 0 is not a number a chart can
      place, and a perfect fit does reach zero. }
    LOSS_CHART_FLOOR: double = 1e-12;

type
    TFitProgressViewMode = (pvStarting, pvLossChart, pvNoIntermediateProgress,
        pvAnimatedCurves);

    { Everything the window draws for one moment of a fit, in the framework's own
      terms rather than the charting component's. }
    TFitProgressView = record
        Mode: TFitProgressViewMode;
        { The loss chart replaces the model's chart; otherwise the model's chart
          stays, under the header. Never both and never neither. }
        LossChartVisible: boolean;
        { The line over the chart. }
        Header: string;
        XAxisLabel, YAxisLabel: string;
        { The loss chart's points: elapsed seconds against log10 of the loss. }
        X, Y: array of double;
    end;

    { One fit's progress, as the polls have delivered it. }
    TFitProgressModel = class(TObject)
    private
        FActive: boolean;
        FShowsView: boolean;
        { A report of this operation running has been seen. Until then a report
          that is not busy is the PREVIOUS operation's, still in the server's log
          because the request starting this one has not reached it yet. }
        FSeenBusy: boolean;
        FSamples: TFitProgressSamples;
        FNextSeq: longint;
        FElapsed: double;
        { As the last report named them; empty until one does. }
        FLossName, FEngineName: string;
        { When the window started watching, by its own clock. The engine's
          elapsed arrives in the reports and therefore stops when the polls do,
          which is the one case a duration is most needed for. }
        FStartedAt: TRunTime;
    public
        { An operation begins: nothing recorded, and the next poll asks from the
          start. AShowsView is False for a computation that is not a fit - timed
          like one, with no progress view over the data it works on. }
        procedure Start(AShowsView: boolean = True; ANow: TRunTime = -1);
        { How long the window has been watching, by its own clock - the run
          clock, which leaves out the time the machine slept. }
        function WatchedSecondsAt(ANow: TRunTime): double;
        function ShowsView: boolean;
        procedure Finish;
        { Keeps the samples it does not already hold, and the server's clock. A
          late or overlapping reply adds nothing twice and never moves the next
          poll back. }
        procedure Apply(const AReport: TFitProgressReport);
        function HasSamples: boolean;
        function SampleCount: longint;
        function LatestValue: double;
        { There is a first sample to measure from, and a later one. }
        function HasImprovement: boolean;
        { How far below the first sample the latest is, in percent. }
        function ImprovementPercent: double;
        function ElapsedText: string;
        { Empty until something is recorded: zero would read as a perfect fit. }
        function RFactorText: string;
        function View(AAnimationMode: boolean): TFitProgressView;
        property Active: boolean read FActive;
        property NextSeq: longint read FNextSeq;
        property Elapsed: double read FElapsed;
    end;

function ProgressViewModeFor(AAnimationMode, AHasSamples: boolean;
    AElapsed: double): TFitProgressViewMode;
{ A loss as the chart plots it: log10, with the floor for zero or less. }
function LossChartValue(AValue: double): double;
{ Seconds in the shape TFitService.GetCalcTimeStr writes. }
function FormatElapsed(ASeconds: double): string;
{ The line over the chart. AImprovement is shown only when AHasImprovement. }
function ProgressHeaderText(AMode: TFitProgressViewMode;
    const AElapsed, AValue: string; AImprovement: double;
    AHasImprovement: boolean; const ALossName: string = '';
    const AEngineName: string = ''): string;

{ What a finished operation says about itself in the log, or '' when it had no
  progress view to draw.

  WRITTEN AFTER EVERY FIT, and it exists because the failure it describes is
  invisible to everything else. A window that draws nothing during a fit leaves
  no trace at all: no exception, no failed request, nothing in any suite - the
  user watches a still chart and the program has no opinion about it. The line
  names the stage that stopped, because the three of them have nothing to do
  with each other: never asked (the window's timer), asked and answered with
  nothing (the engine's reporting), answered without a frame of the model (the
  snapshot). Anyone reading a log can then say which half to look at. }
function FitEpilogueText(AShowedView, AAnimating: boolean; ASeconds: double;
    AViews, ASamples, AModelFrames: longint;
    ATicks, APolls, AFailures: longint): string;

{ How many progress views the window's own cadence would have drawn in that
  time, had every tick asked and been answered. }
function ExpectedViews(ASeconds: double): double;

{ Whether that line is a complaint rather than a record: a fit long enough to
  have been watched, whose window showed the user nothing of it. }
function FitWasInvisible(AShowedView, AAnimating: boolean; ASeconds: double;
    AViews, ASamples, AModelFrames: longint): boolean;

implementation

function TFitProgressModel.ShowsView: boolean;
begin
    Result := FShowsView;
end;

procedure TFitProgressModel.Start(AShowsView: boolean; ANow: TRunTime);
begin
    FActive := True;
    FShowsView := AShowsView;
    FSeenBusy := False;
    FSamples := nil;
    FNextSeq := 0;
    FElapsed := 0;
    FLossName := '';
    FEngineName := '';
    //  Negative means "read the clock": the production caller has no reason
    //  to name a moment, and a test has every reason to.
    if ANow < 0 then
        FStartedAt := RunTime
    else
        FStartedAt := ANow;
end;

function TFitProgressModel.WatchedSecondsAt(ANow: TRunTime): double;
begin
    Result := ANow - FStartedAt;
end;

procedure TFitProgressModel.Finish;
begin
    FActive := False;
end;

procedure TFitProgressModel.Apply(const AReport: TFitProgressReport);
var
    i, n: longint;
begin
    //  NOT YET THIS OPERATION'S. Ignored whole: its samples, its clock and its
    //  seq all belong to the one before. A fit so short that its only report is
    //  already over loses nothing by this - Done draws its result at once.
    if AReport.Busy then
        FSeenBusy := True
    else if not FSeenBusy then
        Exit;
    //  Only forward: a late reply carries an earlier moment.
    if AReport.Elapsed > FElapsed then
        FElapsed := AReport.Elapsed;
    //  KEPT ONCE NAMED: a server that names them does so in every reply, and
    //  one that does not leaves what was there rather than blanking the header.
    if AReport.LossName <> '' then
        FLossName := AReport.LossName;
    if AReport.EngineName <> '' then
        FEngineName := AReport.EngineName;
    for i := 0 to High(AReport.Samples) do
    begin
        //  Seqs are issued in order and never reused, so anything below the next
        //  one expected is already held.
        if AReport.Samples[i].Seq < FNextSeq then
            Continue;
        n := Length(FSamples);
        SetLength(FSamples, n + 1);
        FSamples[n] := AReport.Samples[i];
        FNextSeq := AReport.Samples[i].Seq + 1;
    end;
    if AReport.NextSeq > FNextSeq then
        FNextSeq := AReport.NextSeq;
end;

function TFitProgressModel.HasSamples: boolean;
begin
    Result := Length(FSamples) > 0;
end;

function TFitProgressModel.SampleCount: longint;
begin
    Result := Length(FSamples);
end;

function TFitProgressModel.LatestValue: double;
begin
    if HasSamples then
        Result := FSamples[High(FSamples)].Value
    else
        Result := 0;
end;

function TFitProgressModel.HasImprovement: boolean;
begin
    Result := (Length(FSamples) >= 2) and (FSamples[0].Value > 0);
end;

function TFitProgressModel.ImprovementPercent: double;
begin
    if not HasImprovement then
        Exit(0);
    Result := (FSamples[0].Value - LatestValue) / FSamples[0].Value * 100;
end;

function TFitProgressModel.ElapsedText: string;
begin
    Result := FormatElapsed(FElapsed);
end;

function TFitProgressModel.RFactorText: string;
begin
    if not HasSamples then
        Exit('');
    //  As TFitService.GetRFactorStr writes it.
    Result := rfactor_text.RFactorText(LatestValue);
end;

function TFitProgressModel.View(AAnimationMode: boolean): TFitProgressView;
var
    i: longint;
begin
    Result := Default(TFitProgressView);
    Result.Mode := ProgressViewModeFor(AAnimationMode, HasSamples, FElapsed);
    Result.LossChartVisible := (not AAnimationMode) and HasSamples;
    Result.Header := ProgressHeaderText(Result.Mode, ElapsedText, RFactorText,
        ImprovementPercent, HasImprovement, FLossName, FEngineName);
    //  No unit: the window marks this axis with coordinate_axis's TDurationAxis,
    //  whose every mark names its own - 30 s, 5 min, 2 h.
    Result.XAxisLabel := 'Elapsed time';
    Result.YAxisLabel := 'log10 R-factor';
    SetLength(Result.X, Length(FSamples));
    SetLength(Result.Y, Length(FSamples));
    for i := 0 to High(FSamples) do
    begin
        Result.X[i] := FSamples[i].Elapsed;
        Result.Y[i] := LossChartValue(FSamples[i].Value);
    end;
end;

function ProgressViewModeFor(AAnimationMode, AHasSamples: boolean;
    AElapsed: double): TFitProgressViewMode;
begin
    if AHasSamples then
    begin
        if AAnimationMode then
            Result := pvAnimatedCurves
        else
            Result := pvLossChart;
    end
    else if AElapsed < NO_PROGRESS_GRACE_SECONDS then
        Result := pvStarting
    else
        Result := pvNoIntermediateProgress;
end;

function LossChartValue(AValue: double): double;
begin
    if AValue <= LOSS_CHART_FLOOR then
        Result := Log10(LOSS_CHART_FLOOR)
    else
        Result := Log10(AValue);
end;

function FormatElapsed(ASeconds: double): string;
var
    Sec, Day, Hour, Min: int64;
begin
    Sec := Trunc(Max(ASeconds, 0));
    Day := Sec div 86400;
    Sec := Sec mod 86400;
    Hour := Sec div 3600;
    Sec := Sec mod 3600;
    Min := Sec div 60;
    Sec := Sec mod 60;
    Result := Format('%d day(s) %.2d:%.2d:%.2d', [Day, Hour, Min, Sec]);
end;

function ProgressHeaderText(AMode: TFitProgressViewMode;
    const AElapsed, AValue: string; AImprovement: double;
    AHasImprovement: boolean; const ALossName: string = '';
    const AEngineName: string = ''): string;
var
    Improvement, By: string;
begin
    Improvement := '';
    if AHasImprovement then
        Improvement := Format(', %d %% below the start', [Round(AImprovement)]);
    //  WHAT IS BEING MINIMISED AND BY WHAT. The same model under another
    //  objective, or fitted by another engine, converges differently - so a
    //  chart of the convergence that does not say which is being watched is a
    //  number without a unit. Said only when the server says: an older one
    //  names neither, and the line then reads as it always did rather than
    //  carrying empty brackets.
    //  NAMED AS ROLES, not as a label for the number beside them: what is
    //  plotted is the R-factor whatever the objective is, because that is the
    //  quantity the status bar shows once the fit ends and the live chart has
    //  to end where that begins.
    By := '';
    if (ALossName <> '') and (AEngineName <> '') then
        By := Format(' (minimising %s with %s)', [ALossName, AEngineName])
    else if ALossName <> '' then
        By := Format(' (minimising %s)', [ALossName])
    else if AEngineName <> '' then
        By := Format(' (with %s)', [AEngineName]);
    case AMode of
        pvStarting:
            Result := Format('Starting the fit%s. Use Stop to end it.', [By]);
        pvLossChart:
            Result := Format('Fitting%s: %s elapsed, R-factor %s%s. ' +
                'Use Stop to end it.', [By, AElapsed, AValue, Improvement]);
        pvAnimatedCurves:
            Result := Format('Fitting%s, the model redrawn as it improves: %s ' +
                'elapsed, R-factor %s%s. Use Stop to end it.',
                [By, AElapsed, AValue, Improvement]);
        pvNoIntermediateProgress:
            Result := Format('Fitting%s: %s elapsed. This engine reports no ' +
                'intermediate progress; the result appears when it finishes. ' +
                'Use Stop to end it.', [By, AElapsed]);
    end;
end;


function ExpectedViews(ASeconds: double): double;
begin
    Result := ASeconds * 1000 / PROGRESS_POLL_INTERVAL_MS;
end;

function FitWasInvisible(AShowedView, AAnimating: boolean; ASeconds: double;
    AViews, ASamples, AModelFrames: longint): boolean;
begin
    Result := False;
    if not AShowedView then
        Exit;
    //  A fit shorter than the grace had nothing to show, and calling that a
    //  defect would put a warning in the log of every quick fit.
    if ASeconds < NO_PROGRESS_GRACE_SECONDS then
        Exit;
    if (AViews = 0) or (ASamples = 0) then
        Exit(True);
    //  FAR FEWER FRAMES THAN THE CADENCE PROMISES is the same defect wearing a
    //  number: the window polls ten times a second, so a long fit that drew a
    //  handful of frames stood as still as one that drew none.
    if AViews < ExpectedViews(ASeconds) / 10 then
        Exit(True);
    //  Only Animation Mode promises the model itself moving.
    Result := AAnimating and (AModelFrames = 0);
end;

function FitEpilogueText(AShowedView, AAnimating: boolean; ASeconds: double;
    AViews, ASamples, AModelFrames: longint;
    ATicks, APolls, AFailures: longint): string;
var
    Stage: string;
begin
    Result := '';
    if not AShowedView then
        Exit;
    Stage := '';
    if ASeconds >= NO_PROGRESS_GRACE_SECONDS then
    begin
        if AViews < ExpectedViews(ASeconds) / 10 then
            Stage := Format(' - far fewer than the %d the window''s cadence ' +
                'would draw', [Round(ExpectedViews(ASeconds))]);
        if AViews = 0 then
            Stage := ' - the window never asked for progress'
        else if ASamples = 0 then
            Stage := ' - the window asked and the engine reported no value'
        else if AAnimating and (AModelFrames = 0) then
            Stage := ' - the engine reported values but no frame of the model';
        //  HOW OFTEN THE WINDOW TRIED, on every fit long enough to be judged:
        //  ticks without polls is a poll that refused itself, no ticks at all
        //  is a timer that never ran, and failures are the connection. A fit
        //  that went well is worth the same three numbers, since the next
        //  report to read is usually one that did not.
        Stage := Stage + Format(' (%d tick(s), %d poll(s), %d failure(s))',
            [ATicks, APolls, AFailures]);
    end;
    //  Seconds with a decimal, not the status bar's day-hour-minute shape:
    //  this line is about fractions of a second as often as not.
    Result := Format('a fit of %.1f s drew %d progress view(s), %d value(s) ' +
        'and %d frame(s) of the model (animation %s)%s',
        [ASeconds, AViews, ASamples, AModelFrames,
        BoolToStr(AAnimating, 'on', 'off'), Stage]);
end;

end.
