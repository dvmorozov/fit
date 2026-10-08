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
    far against the time it took, on the axis the user chose for it (View >
    R-factor Scale) - logarithmic unless they chose otherwise, because a loss
    falls through decades and a linear axis flattens everything after the
    first second into the floor.
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

THE CHART AND THE LINE ABOVE IT FOLLOW ONE AXIS, the one the window names for
the R-factor (adLoss in axis_mode_registry): the line gives the value that axis
draws, under the name of what it draws - the R-factor on the linear scale, its
logarithm on the logarithmic one, the user's quantity on a custom one - and
nothing else. Two earlier forms were reported: the logarithmic scale once printed
the R-factor alone, so switching scales changed nothing in the line; then it
printed both, which was too long and did not say what the menu said. The
R-factor as it is, and the elapsed time, are on the status bar under the chart,
so the line does not repeat them.

THE CLOCK IS THE SERVER'S. The fit may run on another machine, and the two clocks
need not agree about when it began.
}
unit fit_progress;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math, fit_progress_json, rfactor_text, run_clock, coordinate_axis,
    elapsed_text;

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
    { What the status bar says from the moment a fit starts until it ends.

      HOW TO END IT, SAID HERE AND ONLY HERE. It used to read "Please wait"
      while every version of the line over the chart closed with "Use Stop to
      end it" - the longest line on screen made longer by a sentence that never
      changed, and a wait that offered no way out. }
    FitRunningHint = 'Calculation started. Use Stop to end it.';
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
        { The loss chart's points: elapsed seconds against the loss as the
          R-factor axis shows it. }
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
        { What the run is doing before it can report a value, as the LATEST
          report says - not kept once named like the two above: a stage ends,
          and the server says so by no longer naming one. }
        FStage: string;
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
        { There is a first sample to measure from, and a later one, both above
          zero: zero is no number of orders of magnitude below anything. }
        function HasImprovement: boolean;
        { How far below the first sample the latest is, in orders of magnitude
          (log10 of first over latest); negative when the fit got worse.

          NOT A PERCENTAGE, which it was. A percentage stops at 100: past 99 %
          a fit still falling tenfold read as standing still, which is the
          stretch a long fit spends most of its time in. Each tenfold drop is
          one more order of magnitude however far the fit has come. }
        function ImprovementDecades: double;
        function ElapsedText: string;
        { Empty until something is recorded: zero would read as a perfect fit. }
        function RFactorText: string;
        { What to draw, with the R-factor on ALossAxis - the window's, never
          owned here. Nil draws it as it is. }
        function View(AAnimationMode: boolean;
            ALossAxis: TCoordinateAxis): TFitProgressView;
        property Active: boolean read FActive;
        property NextSeq: longint read FNextSeq;
        property Elapsed: double read FElapsed;
    end;

function ProgressViewModeFor(AAnimationMode, AHasSamples: boolean;
    AElapsed: double): TFitProgressViewMode;
{ A loss as the chart plots it on AAxis, with the floor for zero or less; as it
  is when AAxis is nil. NaN - a gap - where the axis has no value for it. }
function LossChartValue(AValue: double; AAxis: TCoordinateAxis): double;
{ The loss as AAxis draws it, named for what it draws - "log10 R-factor
  -4.98714". The R-factor as it is when there is no axis, or the axis has no
  value there. }
function LossValueText(AValue: double; AAxis: TCoordinateAxis): string;
{ The line over the chart. AImprovement, in orders of magnitude below the start
  (ImprovementDecades), is shown only when AHasImprovement. }
function ProgressHeaderText(AMode: TFitProgressViewMode;
    const AElapsed, AValue: string; AImprovement: double;
    AHasImprovement: boolean; const ALossName: string = '';
    const AEngineName: string = ''; const AStage: string = ''): string;

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

uses
    axis_mode_registration;

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
    FStage := '';
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
    FStage := AReport.Stage;
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
    Result := (Length(FSamples) >= 2) and (FSamples[0].Value > 0) and
        (LatestValue > 0);
end;

function TFitProgressModel.ImprovementDecades: double;
begin
    if not HasImprovement then
        Exit(0);
    Result := Log10(FSamples[0].Value / LatestValue);
end;

function TFitProgressModel.ElapsedText: string;
begin
    //  As TFitService.GetCalcTimeStr writes it: the same function.
    Result := elapsed_text.ElapsedText(FElapsed);
end;

function TFitProgressModel.RFactorText: string;
begin
    if not HasSamples then
        Exit('');
    //  As TFitService.GetRFactorStr writes it.
    Result := rfactor_text.RFactorText(LatestValue);
end;

function TFitProgressModel.View(AAnimationMode: boolean;
    ALossAxis: TCoordinateAxis): TFitProgressView;
var
    i: longint;
    Value: string;
begin
    Result := Default(TFitProgressView);
    Result.Mode := ProgressViewModeFor(AAnimationMode, HasSamples, FElapsed);
    Result.LossChartVisible := (not AAnimationMode) and HasSamples;
    Value := '';
    if HasSamples then
        Value := LossValueText(LatestValue, ALossAxis);
    Result.Header := ProgressHeaderText(Result.Mode, ElapsedText, Value,
        ImprovementDecades, HasImprovement, FLossName, FEngineName, FStage);
    //  No unit: the window marks this axis with coordinate_axis's TDurationAxis,
    //  whose every mark names its own - 30 s, 5 min, 2 h.
    Result.XAxisLabel := 'Elapsed time';
    if Assigned(ALossAxis) then
        Result.YAxisLabel := ALossAxis.Title
    else
        Result.YAxisLabel := GeneralQuantityName(adLoss);
    SetLength(Result.X, Length(FSamples));
    SetLength(Result.Y, Length(FSamples));
    for i := 0 to High(FSamples) do
    begin
        Result.X[i] := FSamples[i].Elapsed;
        Result.Y[i] := LossChartValue(FSamples[i].Value, ALossAxis);
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

function LossChartValue(AValue: double; AAxis: TCoordinateAxis): double;
begin
    if not Assigned(AAxis) then
        Exit(AValue);
    try
        Result := AAxis.ToDisplay(Max(AValue, LOSS_CHART_FLOOR));
    except
        //  THE USER'S FORMULA MAY HAVE NO VALUE HERE, and it raises rather than
        //  answering NaN. A gap, as an axis leaves anywhere a value has no
        //  place on it (coordinate_axis) - never an exception out of a poll,
        //  which would stop the window asking.
        on Exception do
            Result := NaN;
    end;
end;

function LossValueText(AValue: double; AAxis: TCoordinateAxis): string;
var
    Shown: double;
begin
    //  THE R-FACTOR AS IT IS, where nothing else can be said: no axis, or a
    //  formula with no value here.
    Result := GeneralQuantityName(adLoss) + ' ' + rfactor_text.RFactorText(AValue);
    if not Assigned(AAxis) then
        Exit;
    Shown := LossChartValue(AValue, AAxis);
    if IsNan(Shown) or IsInfinite(Shown) then
        Exit;
    //  WHAT IT DRAWS, not what it is read in: a logarithmic axis is read in
    //  the R-factor and draws its logarithm. SIX DIGITS, like the R-factor: a
    //  logarithm is chosen to see the small late changes, and the readout's two
    //  decimals would hide them. On the linear scale this is the R-factor,
    //  written as the status bar writes it.
    Result := AAxis.DrawnQuantityName + ' ' + rfactor_text.RFactorText(Shown);
end;

function ProgressHeaderText(AMode: TFitProgressViewMode;
    const AElapsed, AValue: string; AImprovement: double;
    AHasImprovement: boolean; const ALossName: string = '';
    const AEngineName: string = ''; const AStage: string = ''): string;
var
    Improvement, By, Lead: string;
begin
    //  THE RUN SAYS WHAT IT IS DOING before the engine can report anything -
    //  starting the engine, which the first time after installing can take
    //  minutes. Neither "Starting the fit" nor "this engine reports no
    //  intermediate progress" is true then, and the second, after a minute,
    //  reads as a hang. Never over a sample: once the engine reports, the
    //  stage is over whatever a late report says.
    if (AStage <> '') and (AMode in [pvStarting, pvNoIntermediateProgress]) then
        Exit(Format('%s: %s elapsed.', [AStage, AElapsed]));
    Improvement := '';
    //  TWO DECIMALS: a hundredth of an order of magnitude is a change of about
    //  2 %, which is as fine as a glance at a moving line can use; the
    //  R-factor beside it carries the rest. A fit that got worse says so in
    //  words rather than as a drop of minus something.
    if AHasImprovement then
    begin
        if AImprovement >= 0 then
            Improvement := Format(', down %.2f orders of magnitude', [AImprovement])
        else
            Improvement := Format(', up %.2f orders of magnitude', [-AImprovement]);
    end;
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
    Lead := 'Fitting';
    if (ALossName <> '') and (AEngineName <> '') then
    begin
        By := Format(' (minimising %s with %s)', [ALossName, AEngineName]);
        Lead := Format('Minimising %s with %s', [ALossName, AEngineName]);
    end
    else if ALossName <> '' then
    begin
        By := Format(' (minimising %s)', [ALossName]);
        Lead := Format('Minimising %s', [ALossName]);
    end
    else if AEngineName <> '' then
    begin
        By := Format(' (with %s)', [AEngineName]);
        Lead := Format('Fitting with %s', [AEngineName]);
    end;
    //  WHILE IT RUNS, ONLY WHAT NOTHING ELSE ON SCREEN SAYS: what is minimised
    //  and by what, the value the chart draws, and how far it has fallen. The
    //  clock and the plain R-factor are on the status bar, and that the model
    //  is redrawn is what the chart shows; the line said all three and was
    //  reported too long. AValue is named by the caller (LossValueText).
    case AMode of
        pvStarting:
            Result := Format('Starting the fit%s.', [By]);
        pvLossChart, pvAnimatedCurves:
            Result := Format('%s: %s%s.', [Lead, AValue, Improvement]);
        pvNoIntermediateProgress:
            Result := Format('Fitting%s: %s elapsed. This engine reports no ' +
                'intermediate progress; the result appears when it finishes.',
                [By, AElapsed]);
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
