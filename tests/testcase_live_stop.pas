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
    worker_process_harness, http_fit_service, fit_client, int_fit_viewer,
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
        procedure GiveTheClientItsViewer;
        { Whether the engine has actually begun improving: asked of the server
          itself rather than of the client, so this says what the ENGINE is
          doing at the moment Stop is pressed. }
        function TheEngineIsFitting: boolean;
        { Starts the fit, waits until the engine has reported a value - which is
          how this knows the fit is running rather than starting - then presses
          Stop and answers how long the whole thing took. }
        function StopItMidwayAndTimeIt: double;
    published
        procedure StopEndsAFitThatWouldHaveRunForMuchLonger;
        procedure TheStopRequestItselfIsAnsweredWhileTheFitRuns;
        procedure AndWhatTheFitReachedIsKept;
        procedure StopWithNothingRunningIsStillRefused;
    end;

implementation

const
    PEAKS = 16;
    { The unstopped fit runs far longer than this; a stopped one must be well
      inside it. }
    STOP_BUDGET_SECONDS = 30;

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

initialization
    //  INTEGRATION: a real compute server process, with a fit running in it.
    RegisterTest('integration', TLiveStopTest);
end.
