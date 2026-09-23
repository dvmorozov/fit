// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Animation Mode against a real compute server: the model redrawn while
it is being fitted.)

WHY THIS EXISTS, AND WHY THE OTHER ANIMATION TESTS WERE NOT ENOUGH. Every other
test of this feature answers the client from a canned reply
(mock_http_transport): they prove what the client makes of a progress report,
and nothing at all about whether a running server produces one. The path the
user actually takes is the whole of it at once - a fit running in another
process, a poll on this side asking for a snapshot while it runs, the engine
building one, and the client drawing it - and that path is exactly where this
program's defects have always been.

So this starts the real fit_server binary, drives the real TFitClient the way
the window does (its own worker thread for the fit, its own polls in between),
and asks the one question the user asks: did the curves move BEFORE the fit
finished?

The fit is made big enough to watch - eight curves over four hundred points,
each seeded off its own peak - because a fit that returns inside one polling
interval cannot be observed by anything, including the user.
}
unit testcase_live_animation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    worker_process_harness, http_fit_service, fit_client, int_fit_viewer,
    fit_progress, title_points_set, points_set, gauss_points_set,
    curve_types_singleton, int_curve_type_selector, SimpMath,
    mock_fit_viewer;

type
    TLiveAnimationTest = class(TWorkerProcessTest)
    private
        FClient: TFitClient;
        FView: TMockFitViewer;
        { A profile of eight peaks, and a pick beside each - a fit that takes
          seconds rather than milliseconds. }
        procedure SeedAFitWorthWatching;
        { Three curves over a short profile: a few hundred milliseconds, which is
          the everyday fit - and the case that decides whether Animation Mode
          does anything a user can see. }
        procedure SeedAnEverydayFit;
        { Drives the fit the way the window does: the completion arrives through
          Synchronize, and the polls happen in between. Answers how many frames
          were drawn before the fit finished. }
        function RunFitCountingFrames: longint;
    published
        procedure AnimationRedrawsTheModelWhileTheServerFits;
        procedure WithoutAnimationTheLossChartIsDrawnInstead;
        procedure AnEverydayFitIsAnimatedAtTheWindowsOwnCadence;
        procedure AnAutomaticFitShowsTheProfileWithoutItsBackgroundWhileItRuns;
        procedure ThePollsOfOneFitShareOneConnection;
    end;

implementation

const
    PEAKS = 8;
    { Long enough for several polls, short enough for a suite. }
    FIT_BUDGET_SECONDS = 120;

procedure TLiveAnimationTest.SeedAFitWorthWatching;
var
    Profile: TTitlePointsSet;
    Positions: TPointsSet;
    Selector: ICurveTypeSelector;
    x, Sum: double;
    i: longint;
begin
    //  Asked for by name: the curve type is a process-wide selection, so a
    //  suite that ran before this one must not decide what is fitted here.
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);

    Profile := TTitlePointsSet.Create(nil);
    try
        x := 0;
        while x <= 100 + 1e-9 do
        begin
            Sum := 0;
            for i := 0 to PEAKS - 1 do
                Sum := Sum + GaussPoint(100, 1.5, 6 + i * 12.0, x);
            Profile.AddNewPoint(x, Sum);
            x := x + 0.25;
        end;
        FSvc.SetProfilePointsSet(Profile);
    finally
        //  The profile setter does not take ownership; the positions one does.
        Profile.Free;
    end;

    Positions := TPointsSet.Create(nil);
    for i := 0 to PEAKS - 1 do
        //  BESIDE its peak, so the optimiser has real work to do.
        Positions.AddNewPoint(6 + i * 12.0 + 2.5,
            GaussPoint(100, 1.5, 6 + i * 12.0, 6 + i * 12.0 + 2.5));
    FSvc.SetCurvePositions(Positions);
end;

procedure TLiveAnimationTest.SeedAnEverydayFit;
var
    Profile: TTitlePointsSet;
    Positions: TPointsSet;
    Selector: ICurveTypeSelector;
    x: double;
begin
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);

    Profile := TTitlePointsSet.Create(nil);
    try
        x := 0;
        while x <= 30 + 1e-9 do
        begin
            Profile.AddNewPoint(x, GaussPoint(100, 1.5, 6, x) +
                GaussPoint(100, 1.5, 15, x) + GaussPoint(100, 1.5, 24, x));
            //  FINE ENOUGH TO LAST A FEW POLLS. At 0.1 the whole minimisation
            //  was sometimes over inside one poll interval, and a fit nobody
            //  asked about while it ran has nothing to animate at any cadence.
            x := x + 0.02;
        end;
        FSvc.SetProfilePointsSet(Profile);
    finally
        Profile.Free;
    end;

    Positions := TPointsSet.Create(nil);
    Positions.AddNewPoint(7.5, GaussPoint(100, 1.5, 6, 7.5));
    Positions.AddNewPoint(16.5, GaussPoint(100, 1.5, 15, 16.5));
    Positions.AddNewPoint(25.5, GaussPoint(100, 1.5, 24, 25.5));
    FSvc.SetCurvePositions(Positions);
end;

function TLiveAnimationTest.RunFitCountingFrames: longint;
var
    Deadline: TDateTime;
begin
    Result := 0;
    FClient.MinimizeDifference;
    Deadline := Now + FIT_BUDGET_SECONDS / SecsPerDay;
    while (FView.ProgressHidden = 0) and (Now < Deadline) do
    begin
        //  The main thread's half of Synchronize: this is what lets the fit's
        //  completion arrive, exactly as the widget set does it.
        CheckSynchronize(10);
        if FView.ProgressHidden > 0 then
            Break;
        FClient.PollProgress;
        //  Counted only while the fit is still running - a frame drawn after it
        //  finished would be the result, not an animation.
        if (FView.ProgressHidden = 0) and FView.Plotted('PlotCurves') then
        begin
            Inc(Result);
            //  Cleared so the next frame is counted as its own.
            FView.Log.Clear;
        end;
        //  AT THE WINDOW'S OWN CADENCE. Polling as fast as the loop can go
        //  would prove something no user ever experiences: the window asks on a
        //  timer, and how much it can show depends on that interval.
        Sleep(PROGRESS_POLL_INTERVAL_MS);
    end;
    AssertTrue('the fit finished within the budget', FView.ProgressHidden > 0);
end;

procedure TLiveAnimationTest.AnimationRedrawsTheModelWhileTheServerFits;
var
    Frames: longint;
begin
    SeedAFitWorthWatching;
    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    try
        FClient.FitService := FSvc;
        FClient.FFitViewer := FView;
        FClient.FProgressView := FView;
        FClient.AnimationMode := True;

        Frames := RunFitCountingFrames;
        AssertTrue(Format('the model was redrawn while the server fitted ' +
            '(%d frames)', [Frames]), Frames > 0);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveAnimationTest.WithoutAnimationTheLossChartIsDrawnInstead;
var
    Frames: longint;
begin
    //  The other half of the same run: with animation off the model is NOT
    //  redrawn mid-fit, and what the chart area shows is the loss.
    SeedAFitWorthWatching;
    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    try
        FClient.FitService := FSvc;
        FClient.FFitViewer := FView;
        FClient.FProgressView := FView;
        FClient.AnimationMode := False;

        Frames := RunFitCountingFrames;
        AssertEquals('no model was redrawn mid-fit', 0, Frames);
        AssertTrue('and the loss chart was shown',
            FView.ProgressShown > 0);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

{ THE FIT EVERY USER ACTUALLY RUNS. One curve on one peak is over in a moment,
  and that is the case Animation Mode has to work for: a feature that only shows
  itself on a fit big enough to watch is, to the person who ticked it, a feature
  that does nothing at all.

  What makes it possible is that the first frame is asked for when the fit
  STARTS rather than on the first poll a quarter of a second later: the engine
  then has a snapshot ready from its first improvement, and the first poll finds
  one waiting instead of asking for one. }
procedure TLiveAnimationTest.AnEverydayFitIsAnimatedAtTheWindowsOwnCadence;
var
    Frames: longint;
begin
    SeedAnEverydayFit;
    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    try
        FClient.FitService := FSvc;
        FClient.FFitViewer := FView;
        FClient.FProgressView := FView;
        FClient.AnimationMode := True;

        Frames := RunFitCountingFrames;
        AssertTrue(Format('an ordinary fit was animated too (%d frames)',
            [Frames]), Frames > 0);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

{ FIT > AUTOMATICALLY TAKES THE BACKGROUND OFF THE PROFILE before it fits, and
  the engine says so through a callback that reaches no client across HTTP. So
  the animated fit drew curves sitting on zero against the profile with its
  background still in it, and only the finished fit showed the two together -
  which is how it was found: in a recording of the application, not by a test.

  A profile on a flat background of 30, and nothing else placed: the automatic
  fit finds the background, the peaks and the curves itself. Asked while the
  fit still runs: is the profile the client draws the one without it? }
procedure TLiveAnimationTest.AnAutomaticFitShowsTheProfileWithoutItsBackgroundWhileItRuns;
const
    BACKGROUND = 30.0;
var
    Profile: TTitlePointsSet;
    Selector: ICurveTypeSelector;
    Shown: TTitlePointsSet;
    x, Lowest: double;
    Deadline: TDateTime;
    SeenWithout: boolean;
    i: longint;
begin
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    Profile := TTitlePointsSet.Create(nil);
    try
        //  COARSE, so the automatic fit - which seeds a curve on every point of
        //  every peak - is seconds long rather than minutes: long enough to be
        //  polled while it runs, short enough for a suite.
        x := 0;
        while x <= 30 + 1e-9 do
        begin
            Profile.AddNewPoint(x, BACKGROUND + GaussPoint(100, 1.5, 8, x) +
                GaussPoint(100, 1.5, 20, x));
            x := x + 1.0;
        end;
        FSvc.SetProfilePointsSet(Profile);
    finally
        Profile.Free;
    end;

    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    try
        FClient.FitService := FSvc;
        FClient.FFitViewer := FView;
        FClient.FProgressView := FView;
        FClient.AnimationMode := True;
        //  The profile as the client loaded it, background and all.
        FClient.ShowProfile;

        SeenWithout := False;
        FClient.DoAllAutomatically;
        Deadline := Now + FIT_BUDGET_SECONDS / SecsPerDay;
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            if FView.ProgressHidden > 0 then
                Break;
            FClient.PollProgress;
            Shown := FClient.GetProfilePoints;
            if Assigned(Shown) and (Shown.PointsCount > 0) then
            begin
                Lowest := Shown.PointYCoord[0];
                for i := 1 to Shown.PointsCount - 1 do
                    Lowest := Min(Lowest, Shown.PointYCoord[i]);
                if Lowest < BACKGROUND / 2 then
                    SeenWithout := True;
            end;
            if SeenWithout then
                Break;
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        AssertTrue('while the fit ran, the profile was drawn without its background',
            SeenWithout);
        //  Seen: the rest of the fit - minutes of it - answers nothing more.
        //  Fit > Stop, as the window does, and the stop waited for before the
        //  server goes.
        FClient.StopAsyncOper;
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(20);
        end;
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

{ THE WINDOW'S THREAD MUST NOT OPEN A SOCKET PER FRAME. Every poll used to
  connect anew, and a connect to a server busy with the fit blocked the window
  itself - caught in the act, the main thread sat in connect() while the fit ran
  and the window drew three frames in fifty seconds. The polls of one fit go
  over one connection, so the window connects once. }
procedure TLiveAnimationTest.ThePollsOfOneFitShareOneConnection;
var
    Opened: longint;
begin
    FSvc.GetFitProgress(0, False);
    Opened := FSvc.ConnectionsOpened;
    AssertTrue('the first poll connected', Opened > 0);
    FSvc.GetFitProgress(0, False);
    FSvc.GetFitProgress(0, True);
    AssertEquals('and the next ones did not', Opened, FSvc.ConnectionsOpened);
end;

initialization
    //  Starts the real compute server: integration by every part of the rule.
    RegisterTest('integration', TLiveAnimationTest);
end.
