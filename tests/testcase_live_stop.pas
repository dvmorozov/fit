// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Stop against a real compute server: a fit ends when it is asked to.)

WHY THIS IS AN INTEGRATION TEST AND CANNOT BE ANYTHING ELSE. Stop is the one
command whose whole purpose is to reach a problem that is BUSY. Every other
request is served while the problem is idle, or waits; this one is worthless
unless it is answered while an operation holds the problem. Nothing driving a
single thread can show that: it needs a fit really running, in another process,
while the request goes in.

WHAT IT USED TO DO, which is what these tests are made of. The stop action was
served on the locked path, so the request waited for the very fit it was meant
to interrupt - and the wait was the whole fit. By the time it was let in the
operation was over, and the server answered "the calculation not started". The
user pressed Stop, watched the window hang for the rest of the fit, and was then
told nothing was running. Underneath that, the service's Stop did not stop
anything even when it did get in: it recovered a stale busy flag and never
reached the running task.
}
unit testcase_live_stop;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fphttpclient,
    worker_process_harness, http_fit_service, fit_client, int_fit_viewer,
    fit_service,
    fit_progress, fit_progress_json, title_points_set, points_set, gauss_points_set,
    self_copied_component, curve_types_singleton, int_curve_type_selector,
    SimpMath, mock_fit_viewer;

type
    TLiveStopTest = class(TWorkerProcessTest)
    private
        FClient: TFitClient;
        FView: TMockFitViewer;
        { A fit of seconds rather than milliseconds: sixteen peaks with a pick
          beside each, so there is a middle to press Stop in. }
        procedure SeedALongFit;
        { Four fit intervals of sixteen peaks each, a pick beside every peak -
          so the run is several tasks, one after another, each long enough to
          press Stop in, as the user's twenty-two were. }
        procedure SeedFourIntervals;
        procedure GiveTheClientItsViewer;
        { Whether the engine has actually begun improving: asked of the server
          itself rather than of the client, so this says what the ENGINE is
          doing at the moment Stop is pressed. }
        function TheEngineIsFitting: boolean;
        { Starts the fit, waits until the engine has reported a value - which is
          how this knows the fit is running rather than starting - then presses
          Stop and answers how long the whole thing took. }
        function StopItMidwayAndTimeIt: double;
        { Runs AStart, stops it once it has halved its loss, runs AStart again
          and asserts the second run began where the first was stopped. }
        procedure AssertTheNextRunResumes(AStart: TThreadMethod;
            ATolerance: double);
    published
        procedure StopEndsAFitThatWouldHaveRunForMuchLonger;
        procedure TheStopRequestItselfIsAnsweredWhileTheFitRuns;
        procedure AndWhatTheFitReachedIsKept;
        procedure StopWithNothingRunningIsStillRefused;
        procedure StopEndsEveryIntervalNotJustTheOneRunning;
        procedure ARunOfSeveralIntervalsReportsProgressFromTheFirst;
        procedure TheWindowHasTheResultSoonAfterStop;
        procedure TheNextRunStartsWhereStopLeftTheModel;
        procedure DeletingAProblemWhileItFitsEndsTheFitFirst;
        procedure ARequestWithABodyIsNotStalled;
    end;

implementation

type
    TDoubleArrayOfSamples = array of double;

const
    PEAKS = 16;
    { The unstopped fit runs far longer than this; a stopped one must be well
      inside it. }
    STOP_BUDGET_SECONDS = 30;
    { How long a stopped run may take to end: the cycle it was in the middle
      of, and nothing after it. }
    STOPPED_WITHIN_SECONDS = 5;
    RUN_UNDER_WAY_MS = 1000;
    { How soon a fit must report its first improvement. The first interval of
      the four-interval run alone takes far longer than this. }
    FIRST_PROGRESS_SECONDS = 5;

procedure TLiveStopTest.SeedALongFit;
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
        while x <= 200 + 1e-9 do
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
        Positions.AddNewPoint(6 + i * 12.0 + 3.0,
            GaussPoint(100, 1.5, 6 + i * 12.0, 6 + i * 12.0 + 3.0));
    FSvc.SetCurvePositions(Positions);
end;

procedure TLiveStopTest.SeedFourIntervals;
const
    INTERVALS = 4;
    SPACING = 12.0;
    INTERVAL_WIDTH = PEAKS * SPACING;
var
    Profile: TTitlePointsSet;
    Positions, Bounds: TPointsSet;
    Selector: ICurveTypeSelector;
    x, Sum, Centre: double;
    i, k: longint;
begin
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);

    Profile := TTitlePointsSet.Create(nil);
    try
        x := 0;
        while x < INTERVALS * INTERVAL_WIDTH - 1e-9 do
        begin
            Sum := 0;
            for i := 0 to INTERVALS * PEAKS - 1 do
                Sum := Sum + GaussPoint(100, 1.5, 6 + i * SPACING, x);
            Profile.AddNewPoint(x, Sum);
            x := x + 0.25;
        end;
        FSvc.SetProfilePointsSet(Profile);
    finally
        Profile.Free;
    end;

    Positions := TPointsSet.Create(nil);
    for i := 0 to INTERVALS * PEAKS - 1 do
    begin
        Centre := 6 + i * SPACING;
        Positions.AddNewPoint(Centre + 3.0,
            GaussPoint(100, 1.5, Centre, Centre + 3.0));
    end;
    FSvc.SetCurvePositions(Positions);

    //  Disjoint, each on a sample: the last sample before the next begins.
    Bounds := TPointsSet.Create(nil);
    for k := 0 to INTERVALS - 1 do
    begin
        Bounds.AddNewPoint(k * INTERVAL_WIDTH, 0);
        Bounds.AddNewPoint((k + 1) * INTERVAL_WIDTH - 0.25, 0);
    end;
    FSvc.SetRFactorBounds(Bounds);
end;

procedure TLiveStopTest.GiveTheClientItsViewer;
begin
    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FClient.FProgressView := FView;
end;

function TLiveStopTest.TheEngineIsFitting: boolean;
var
    Report: TFitProgressReport;
begin
    Report := FSvc.GetFitProgress(0, False);
    Result := Report.Busy and (Length(Report.Samples) > 0);
end;

function TLiveStopTest.StopItMidwayAndTimeIt: double;
var
    Started, Deadline: TDateTime;
    Asked: boolean;
begin
    Asked := False;
    Started := Now;
    Deadline := Started + STOP_BUDGET_SECONDS / SecsPerDay;
    FClient.MinimizeDifference;
    while (FView.ProgressHidden = 0) and (Now < Deadline) do
    begin
        //  The main thread's half of Synchronize: this is what lets the fit's
        //  completion arrive, exactly as the widget set does it.
        CheckSynchronize(10);
        if FView.ProgressHidden > 0 then
            Break;
        FClient.PollProgress;
        //  PRESSED IN THE MIDDLE, the only place it means anything: once the
        //  engine has reported a value the fit is certainly running.
        if (not Asked) and TheEngineIsFitting then
        begin
            FClient.StopAsyncOper;
            Asked := True;
        end;
        Sleep(PROGRESS_POLL_INTERVAL_MS);
    end;
    AssertTrue('the fit was really running when Stop was pressed', Asked);
    AssertTrue('the fit ended', FView.ProgressHidden > 0);
    Result := (Now - Started) * SecsPerDay;
end;

procedure TLiveStopTest.StopEndsAFitThatWouldHaveRunForMuchLonger;
var
    Took: double;
begin
    SeedALongFit;
    GiveTheClientItsViewer;
    try
        Took := StopItMidwayAndTimeIt;
        AssertTrue(Format('the fit ended when it was asked to, in %.1f s',
            [Took]), Took < STOP_BUDGET_SECONDS);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveStopTest.TheStopRequestItselfIsAnsweredWhileTheFitRuns;
var
    Asked, Deadline: TDateTime;
    Waited: double;
begin
    //  THE HALF THE USER FELT AS A FREEZE. Stop is pressed on the window's own
    //  thread, so a request that waits for the fit's lock takes the window with
    //  it for the rest of the fit.
    SeedALongFit;
    GiveTheClientItsViewer;
    try
        FClient.MinimizeDifference;
        Deadline := Now + STOP_BUDGET_SECONDS / SecsPerDay;
        while (not TheEngineIsFitting) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            FClient.PollProgress;
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        AssertTrue('the fit is running', TheEngineIsFitting);

        Asked := Now;
        FClient.StopAsyncOper;
        Waited := (Now - Asked) * SecsPerDay;
        AssertTrue(Format('the window was not held: %.2f s', [Waited]),
            Waited < 2.0);

        //  Left to finish, so the next test starts against an idle problem.
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveStopTest.AndWhatTheFitReachedIsKept;
var
    Curves: TSelfCopiedCompList;
begin
    //  STOPPING IS NOT CANCELLING: what the fit reached stays, which is what
    //  the command has always promised - "keeping what it has reached".
    SeedALongFit;
    GiveTheClientItsViewer;
    try
        StopItMidwayAndTimeIt;
        Curves := FSvc.GetCurves;
        try
            AssertTrue('the curves the fit reached are there',
                Assigned(Curves) and (Curves.Count > 0));
        finally
            Curves.Free;
        end;
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveStopTest.StopWithNothingRunningIsStillRefused;
var
    Refused: string;
begin
    //  The refusal the user should see is the one for pressing Stop when there
    //  is nothing to stop - and it must not be what they are shown mid-fit.
    SeedALongFit;
    Refused := '';
    try
        FSvc.StopAsyncOper;
    except
        on E: Exception do
            Refused := E.Message;
    end;
    AssertTrue('refused: ' + Refused, Pos('not started', Refused) > 0);
end;

procedure TLiveStopTest.StopEndsEveryIntervalNotJustTheOneRunning;
var
    Deadline, Asked: TDateTime;
    Took: double;
begin
    //  STOP ENDED ONE INTERVAL AND THE RUN WENT ON. Every task was told, and
    //  each ignored it: the one running ended its cycle and began its final
    //  optimisation, and the ones not yet reached started as if nothing had
    //  been said. The user pressed Stop five times over a minute and an
    //  automatic run went on through twenty-two intervals.
    //
    //  Reducing the number of curves, as the automatic run does, because that
    //  is the run with a pass AFTER the one a stop lands in. TIMED, because
    //  ending promptly is what Stop promises: a count of samples could not tell
    //  a pass that was skipped from one that improved nothing.
    SeedFourIntervals;
    GiveTheClientItsViewer;
    try
        FClient.MinimizeNumberOfCurves;
        Deadline := Now + STOP_BUDGET_SECONDS / SecsPerDay;
        while (not FSvc.GetFitProgress(0, False).Busy) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        //  Well into it: past the stages that only place the model.
        Sleep(RUN_UNDER_WAY_MS);
        AssertTrue('the run is still going', FView.ProgressHidden = 0);

        Asked := Now;
        FClient.StopAsyncOper;
        //  THE ENGINE'S END, timed: what the client then reads back and draws
        //  is the same after any fit, and is not what Stop is for.
        while FSvc.GetFitProgress(0, False).Busy and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        Took := (Now - Asked) * SecsPerDay;
        AssertTrue(Format('the run ended when it was asked to, %.1f s later',
            [Took]), Took < STOPPED_WITHIN_SECONDS);
        //  And the window hears of it, as after any fit.
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        AssertTrue('the window was told the run ended',
            FView.ProgressHidden > 0);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveStopTest.ARunOfSeveralIntervalsReportsProgressFromTheFirst;
var
    Started, Deadline: TDateTime;
    Took: double;
begin
    //  "THIS ENGINE REPORTS NO INTERMEDIATE PROGRESS" - said of the native
    //  engine, which reports every improvement. The service recorded one only
    //  once EVERY task had reported, and the tasks run one after another, so a
    //  run of twenty-two intervals showed nothing until its last began. Stop
    //  is here because the fixture is, and because it ends the run.
    SeedFourIntervals;
    GiveTheClientItsViewer;
    try
        Started := Now;
        Deadline := Started + STOP_BUDGET_SECONDS / SecsPerDay;
        FClient.MinimizeNumberOfCurves;
        while (not TheEngineIsFitting) and (FView.ProgressHidden = 0) and
            (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        Took := (Now - Started) * SecsPerDay;
        AssertTrue(Format('progress was reported from the first interval, ' +
            'after %.1f s', [Took]),
            TheEngineIsFitting and (Took < FIRST_PROGRESS_SECONDS));

        FClient.StopAsyncOper;
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveStopTest.TheWindowHasTheResultSoonAfterStop;
const
    { Taking in sixty-four curves over localhost. Unfixed, every request
      stalled 40 ms and the four intervals' curves took several seconds; the
      user's five hundred took fifty-five. }
    WINDOW_BACK_WITHIN_SECONDS = 2;
var
    Deadline, Asked: TDateTime;
    Took: double;
begin
    //  THE WINDOW, NOT THE ENGINE. The fit ends at once now; what the user
    //  waited for afterwards was the window taking in what it reached.
    SeedFourIntervals;
    GiveTheClientItsViewer;
    try
        FClient.MinimizeNumberOfCurves;
        Deadline := Now + STOP_BUDGET_SECONDS / SecsPerDay;
        while (not FSvc.GetFitProgress(0, False).Busy) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        Sleep(RUN_UNDER_WAY_MS);
        FClient.StopAsyncOper;
        while FSvc.GetFitProgress(0, False).Busy and (Now < Deadline) do
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        //  From the engine's end to the window having taken it in: the Done
        //  handler, which is where the curves are fetched.
        Asked := Now;
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
            CheckSynchronize(10);
        Took := (Now - Asked) * SecsPerDay;
        AssertTrue('the window was told the run ended', FView.ProgressHidden > 0);
        AssertTrue(Format('the window had the result %.1f s after the run ' +
            'ended', [Took]), Took < WINDOW_BACK_WITHIN_SECONDS);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

{ THE PROBLEM WAS FREED UNDER ITS OWN FIT. The client releases its problem when it
  lets go of the service - closing the project, closing the window - and the
  route that does it is answered without the problem's lock, because freeing the
  problem frees that lock. So a release that arrived during a fit freed the
  model the fit was writing into: the fit died with an access violation, and the
  release itself answered with a thread error. }
procedure TLiveStopTest.DeletingAProblemWhileItFitsEndsTheFitFirst;
var
    Raw: TFPHTTPClient;
    Body: TStringStream;
    Base: string;
    Deadline: TDateTime;
begin
    SeedALongFit;
    GiveTheClientItsViewer;
    Raw := TFPHTTPClient.Create(nil);
    try
        Base := Format('http://127.0.0.1:%d', [WorkerTestPort]);
        //  A fresh server per test: the fixture's problem is the first.
        Raw.Get(Base + '/problems/1/state');
        AssertEquals('the fixture''s problem is number 1', 200,
            Raw.ResponseStatusCode);

        FClient.MinimizeDifference;
        Deadline := Now + STOP_BUDGET_SECONDS / SecsPerDay;
        while (not TheEngineIsFitting) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        AssertTrue('the fit is running', TheEngineIsFitting);

        Body := TStringStream.Create('');
        try
            Raw.HTTPMethod('DELETE', Base + '/problems/1', Body, []);
        finally
            Body.Free;
        end;
        AssertEquals('the release was answered, not faulted', 200,
            Raw.ResponseStatusCode);
        Raw.Get(Base + '/health');
        AssertEquals('and the server is still there', 200,
            Raw.ResponseStatusCode);

        //  The client's own fit call comes back one way or the other.
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
    finally
        Raw.Free;
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

{ THE CLIENT'S HALF OF THE 40 MS STALL. The server's replies were held by Nagle's
  algorithm until the client acknowledged their headers (fixed with TCP_NODELAY
  on the server). The client writes a request's headers and body separately too,
  so a request with a body could wait on the server's delayed ACK the same way. }
procedure TLiveStopTest.ARequestWithABodyIsNotStalled;
const
    REQUESTS = 20;
    { A stalled request costs 40 ms; an unstalled one on localhost, a few. }
    BUDGET_MS = REQUESTS * 15;
var
    i: longint;
    Started, Took: QWord;
begin
    FSvc.SetMaxRFactor(0.001);
    Started := GetTickCount64;
    for i := 1 to REQUESTS do
        FSvc.SetMaxRFactor(0.001 + i * 1e-6);
    Took := GetTickCount64 - Started;
    AssertTrue(Format('%d requests with a body took %d ms', [REQUESTS, Took]),
        Took < BUDGET_MS);
end;

{ Every loss the engine has reported in the current operation, in order. }
function LossesSoFar(ASvc: THttpFitService): TDoubleArrayOfSamples;
var
    Report: TFitProgressReport;
    i: longint;
begin
    Report := ASvc.GetFitProgress(0, False);
    Result := nil;
    SetLength(Result, Length(Report.Samples));
    for i := 0 to High(Report.Samples) do
        Result[i] := Report.Samples[i].Value;
end;

procedure TLiveStopTest.TheNextRunStartsWhereStopLeftTheModel;
begin
    SeedALongFit;
    GiveTheClientItsViewer;
    try
        //  The same objective in both runs, so the same number: to the rounding
        //  of the last cycle.
        AssertTheNextRunResumes(@FClient.MinimizeDifference, 0.001);
    finally
        FreeAndNil(FClient);
        FreeAndNil(FView);
    end;
end;

procedure TLiveStopTest.AssertTheNextRunResumes(AStart: TThreadMethod;
    ATolerance: double);
var
    Deadline: TDateTime;
    First, Second: TDoubleArrayOfSamples;
    StartedAt, StoppedAt, ResumedAt: double;
    Hidden: longint;
    LeftAt: string;
begin
    //  RUNS ARE INCREMENTAL, and Stop is one way a run ends. What it reached is
    //  the model - unfinished, but complete - and the next run carries on from
    //  it rather than from the seeds. A Stop that threw the progress away, or a
    //  next run that rebuilt its curves from their starting guesses, would
    //  make stopping a long fit and resuming it cost the whole fit again.
    begin
        Deadline := Now + STOP_BUDGET_SECONDS / SecsPerDay;

        //  The first run, stopped once it has improved on where it began.
        AStart;
        repeat
            CheckSynchronize(10);
            FClient.PollProgress;
            Sleep(PROGRESS_POLL_INTERVAL_MS);
            First := LossesSoFar(FSvc);
        until ((Length(First) > 3) and (First[High(First)] < 0.5 * First[0]))
            or (FView.ProgressHidden > 0) or (Now > Deadline);
        AssertTrue('the first run improved before it was stopped',
            (Length(First) > 3) and (FView.ProgressHidden = 0));
        FClient.StopAsyncOper;
        while (FView.ProgressHidden = 0) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
        First := LossesSoFar(FSvc);
        StartedAt := First[0];
        StoppedAt := First[High(First)];
        LeftAt := FSvc.GetRFactorStr;
        //  A COMPLETE MODEL, measured: not "Not calculated".
        AssertTrue('the model Stop left reports its R-factor: ' + LeftAt,
            LeftAt <> RFactorStillNotCalculated);

        //  The next run, watched until it reports.
        Hidden := FView.ProgressHidden;
        AStart;
        repeat
            CheckSynchronize(10);
            FClient.PollProgress;
            Sleep(PROGRESS_POLL_INTERVAL_MS);
            Second := LossesSoFar(FSvc);
        until (Length(Second) > 0) or (FView.ProgressHidden > Hidden) or
            (Now > Deadline);
        AssertTrue('the next run reported', Length(Second) > 0);
        ResumedAt := Second[0];
        AssertTrue(Format('the next run began at %g, where the stopped one ' +
            'ended (%g; the model it left reads %s), not where that one ' +
            'began (%g)', [ResumedAt, StoppedAt, LeftAt, StartedAt]),
            ResumedAt <= StoppedAt * (1 + ATolerance));

        if FView.ProgressHidden = Hidden then
            FClient.StopAsyncOper;
        while (FView.ProgressHidden = Hidden) and (Now < Deadline) do
        begin
            CheckSynchronize(10);
            Sleep(PROGRESS_POLL_INTERVAL_MS);
        end;
    end;
end;

initialization
    //  INTEGRATION: a real compute server process, with a fit running in it.
    RegisterTest('integration', TLiveStopTest);
end.
