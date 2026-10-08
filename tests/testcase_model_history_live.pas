// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The model history through the surface the user reaches: a real
fit_server, a real TFitClient over HTTP, the window's own wiring from a run's end
to the document's history - and Make Current putting back the model a fit really
found.)

WHY THIS ONE EXISTS BESIDE THE UNIT TESTS. Every piece has its own test in
process; this codebase's recurring failure is a green suite over a path the user
never takes. Here the fit is a real optimiser run in another process, the capture
and the restore cross HTTP verb by verb, the run's end reaches the history the way
the window wires it (TFitClient.OnFitRunEnded -> TProjectWorkflow.RecordRun), and
the R-factor of the restored model comes back through the evaluate-model action -
so a step missing from any of them fails here even if its own unit is green.

INTEGRATION: a process, a socket and an optimiser run to convergence.
}
unit testcase_model_history_live;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    worker_process_harness, http_fit_service, fit_client, int_fit_service,
    title_points_set, points_set, gauss_points_set, curve_types_singleton,
    int_curve_type_selector, mock_fit_viewer, mock_project_host,
    int_project_host, project_workflow, model_history, fit_progress_json,
    SimpMath;

type
    TModelHistoryLiveTest = class(TWorkerProcessTest)
    private
        FClient: TFitClient;
        FView: TMockFitViewer;
        FHostObj: TMockProjectHost;
        FHost: IProjectHost;
        FFlow: TProjectWorkflow;
        procedure FitRunEnded(Sender: TObject; AKind: THistoryRunKind;
            AStopped: boolean);
        { A Gaussian, one fit interval over it and one pick at AX. }
        procedure GivenAModelPickedAt(AX: double);
        procedure MovePickTo(AX: double);
        { Presses Fit as the window does and waits for the run to end. }
        procedure Fit;
        { Sixteen peaks, a pick beside each: a fit of seconds, with a middle
          to press Stop in (as testcase_live_stop seeds it). }
        procedure GivenALongFit;
        function SigmaNow: double;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EachFitThatEndsIsRecordedFromTheRealRun;
        procedure MakingTheFirstCurrentPutsBackWhatItsFitFound;
        procedure AndTheRestoredModelReportsTheRFactorItWasRecordedWith;
        procedure AndTheNextFitBranchesFromIt;
        procedure ARunCutShortByStopIsRecordedAsStopped;
    end;

implementation

const
    RUN_BUDGET_SECONDS = 60;

procedure TModelHistoryLiveTest.SetUp;
var
    Selector: ICurveTypeSelector;
begin
    inherited SetUp;
    //  By name: the curve type is a process-wide selection.
    Selector := TCurveTypesSingleton.CreateCurveTypeSelector;
    Selector.SelectCurveType(TGaussPointsSet.GetCurveTypeId);
    FClient := TFitClient.Create;
    FView := TMockFitViewer.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FClient.FProgressView := FView;
    FHostObj := TMockProjectHost.Create;
    FHost := FHostObj;
    FFlow := TProjectWorkflow.Create(FSvc, FHost);
    //  THE WINDOW'S WIRING (TFormMain.FitRunEnded), minus the redraw.
    FClient.OnFitRunEnded := @FitRunEnded;
end;

procedure TModelHistoryLiveTest.TearDown;
begin
    FreeAndNil(FFlow);
    FHost := nil;
    FreeAndNil(FHostObj);
    FreeAndNil(FClient);
    FreeAndNil(FView);
    inherited TearDown;
end;

procedure TModelHistoryLiveTest.FitRunEnded(Sender: TObject;
    AKind: THistoryRunKind; AStopped: boolean);
begin
    FFlow.RecordRun(AKind, AStopped);
end;

procedure TModelHistoryLiveTest.GivenAModelPickedAt(AX: double);
var
    Bounds, Profile: TTitlePointsSet;
begin
    FSvc.SetCurveType(TGaussPointsSet.GetCurveTypeId);
    Profile := GaussianProfile;
    try
        FSvc.SetProfilePointsSet(Profile);
    finally
        //  The profile setter does not take ownership; the others do.
        Profile.Free;
    end;
    Bounds := TTitlePointsSet.Create(nil);
    Bounds.AddNewPoint(0, 0);
    Bounds.AddNewPoint(20, 0);
    FSvc.SetRFactorBounds(Bounds);
    MovePickTo(AX);
end;

procedure TModelHistoryLiveTest.MovePickTo(AX: double);
var
    Picks: TTitlePointsSet;
begin
    Picks := TTitlePointsSet.Create(nil);
    Picks.AddNewPoint(AX, 50);
    FSvc.SetCurvePositions(Picks);
end;

procedure TModelHistoryLiveTest.Fit;
var
    Deadline: TDateTime;
    Hidden: longint;
begin
    Hidden := FView.ProgressHidden;
    Deadline := Now + RUN_BUDGET_SECONDS / SecsPerDay;
    FClient.MinimizeDifference;
    while (FView.ProgressHidden = Hidden) and (Now < Deadline) do
    begin
        //  The main thread's half of Synchronize: how the run's end arrives.
        CheckSynchronize(10);
        Sleep(20);
    end;
    AssertTrue('the fit ended', FView.ProgressHidden > Hidden);
end;

function TModelHistoryLiveTest.SigmaNow: double;
var
    j: longint;
    Nm: string;
    V: double;
    T: longint;
begin
    Result := -1;
    for j := 0 to FSvc.GetCurveParameterCount(0) - 1 do
    begin
        FSvc.GetCurveParameter(0, j, Nm, V, T);
        if Nm = 'sigma' then
            Exit(V);
    end;
end;

procedure TModelHistoryLiveTest.EachFitThatEndsIsRecordedFromTheRealRun;
var
    RFactor: double;
begin
    GivenAModelPickedAt(9);
    Fit;
    AssertEquals('the run is in the history', 1, FFlow.History.Count);
    AssertEquals('as the run it was', Ord(hrkMinimizeDifference),
        Ord(FFlow.History[0].RunKind));
    AssertFalse('not stopped', FFlow.History[0].Stopped);
    AssertTrue('with the R-factor it reached: ' +
        FloatToStr(FFlow.History[0].Model.RFactor),
        FFlow.History[0].Model.RFactor >= 0);
    AssertTrue('the same the server reports',
        TryStrToFloat(Trim(FSvc.GetRFactorStr), RFactor));
    AssertEquals(RFactor, FFlow.History[0].Model.RFactor, 1e-12);
end;

procedure TModelHistoryLiveTest.MakingTheFirstCurrentPutsBackWhatItsFitFound;
var
    First: string;
    Sigma: double;
    Picks: TTitlePointsSet;
begin
    GivenAModelPickedAt(9);
    Fit;
    First := FFlow.History[0].Id;
    Sigma := SigmaNow;
    MovePickTo(12);
    Fit;
    AssertEquals('two models', 2, FFlow.History.Count);

    AssertTrue('made current', FFlow.MakeHistoryEntryCurrent(First));
    Picks := FSvc.GetCurvePositions;
    try
        AssertEquals('the pick it was fitted from', 9.0, Picks.PointXCoord[0],
            1e-12);
    finally
        Picks.Free;
    end;
    AssertEquals('the width its fit found, back over HTTP', Sigma, SigmaNow,
        1e-9);
    AssertTrue('and fitted, as it was', FSvc.IsCurveFitted(0));
    AssertEquals('the live model was the second entry: nothing kept', 2,
        FFlow.History.Count);
end;

procedure TModelHistoryLiveTest.AndTheRestoredModelReportsTheRFactorItWasRecordedWith;
var
    First: string;
    Recorded, Restored: double;
begin
    GivenAModelPickedAt(9);
    Fit;
    First := FFlow.History[0].Id;
    Recorded := FFlow.History[0].Model.RFactor;
    MovePickTo(12);
    Fit;
    FFlow.MakeHistoryEntryCurrent(First);
    AssertTrue('measured after the restore, not "Not calculated": ' +
        FSvc.GetRFactorStr, TryStrToFloat(Trim(FSvc.GetRFactorStr), Restored));
    AssertEquals('the figure it was recorded with', Recorded, Restored, 1e-12);
end;

procedure TModelHistoryLiveTest.AndTheNextFitBranchesFromIt;
var
    First, Second: string;
begin
    GivenAModelPickedAt(9);
    Fit;
    First := FFlow.History[0].Id;
    MovePickTo(12);
    Fit;
    Second := FFlow.History[1].Id;
    FFlow.MakeHistoryEntryCurrent(First);
    MovePickTo(10);
    Fit;
    AssertEquals('three models', 3, FFlow.History.Count);
    AssertEquals('the second descends from the first', First,
        FFlow.History[1].ParentId);
    AssertEquals('and so does the third - a branch', First,
        FFlow.History[2].ParentId);
    AssertFalse('not from the second', FFlow.History[2].ParentId = Second);
end;

procedure TModelHistoryLiveTest.GivenALongFit;
const
    PEAKS = 16;
var
    Profile: TTitlePointsSet;
    Positions: TPointsSet;
    x, Sum: double;
    i: longint;
begin
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
        Profile.Free;
    end;
    Positions := TPointsSet.Create(nil);
    for i := 0 to PEAKS - 1 do
        Positions.AddNewPoint(6 + i * 12.0 + 3.0,
            GaussPoint(100, 1.5, 6 + i * 12.0, 6 + i * 12.0 + 3.0));
    FSvc.SetCurvePositions(Positions);
end;

procedure TModelHistoryLiveTest.ARunCutShortByStopIsRecordedAsStopped;
var
    Deadline: TDateTime;
    Hidden: longint;
    Asked: boolean;
    Report: TFitProgressReport;
begin
    //  WHAT STOP REACHED IS THE MODEL from then on, so it is recorded - and
    //  said to be stopped, because a model a run did not finish is not the
    //  claim a finished one is. Pressed in the middle of a real run, as the
    //  user presses it.
    GivenALongFit;
    Hidden := FView.ProgressHidden;
    Asked := False;
    Deadline := Now + RUN_BUDGET_SECONDS / SecsPerDay;
    FClient.MinimizeDifference;
    while (FView.ProgressHidden = Hidden) and (Now < Deadline) do
    begin
        CheckSynchronize(10);
        if not Asked then
        begin
            Report := FSvc.GetFitProgress(0, False);
            if Report.Busy and (Length(Report.Samples) > 0) then
            begin
                FClient.StopAsyncOper;
                Asked := True;
            end;
        end;
        Sleep(20);
    end;
    AssertTrue('Stop was pressed while the fit ran', Asked);
    AssertTrue('the run ended', FView.ProgressHidden > Hidden);
    AssertEquals('it is in the history', 1, FFlow.History.Count);
    AssertTrue('as a stopped run', FFlow.History[0].Stopped);
    AssertTrue('with the R-factor it reached',
        FFlow.History[0].Model.RFactor >= 0);
end;

initialization
    //  A real worker process and optimiser runs.
    RegisterTest('integration', TModelHistoryLiveTest);
end.
