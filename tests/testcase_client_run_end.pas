// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the client says when a fit ends: which run it was, whether Stop cut
it short, and whether it ended at all - the facts the model history records.)

WHY THE CLIENT AND NOT THE ENGINE. The engine's DoneProc runs once per STAGE: an
automatic run ends its reduction and then its last fit, and each reaches
"all tasks done". Recorded there, one press of Automatically would leave two
models in the history, the first of them a half-way state nobody asked for. The
client's Done runs once per run the user started, which is the unit the history
is made of.

HOW IT IS REACHED. TFitClient.RunAsync is virtual, and the double below runs the
operation and the completion in place - and, like the real thread
(TServerCallThread.Execute), swallows a failing operation and completes anyway,
because that is the case a recorded history must not mistake for a result.
}
unit testcase_client_run_end;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_client, mock_fit_viewer, mock_http_transport, model_history;

type
    { Runs what RunAsync would have threaded, in place, as the thread would. }
    TRunEndClient = class(TFitClient)
    private
        FStopDuringRun: boolean;
    protected
        procedure RunAsync(AOp: TServerOp; ADone: TThreadMethod); override;
    public
        { The user presses Stop while the operation runs. }
        property StopDuringRun: boolean read FStopDuringRun write FStopDuringRun;
    end;

    TClientRunEndTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        FView: TMockFitViewer;
        FClient: TRunEndClient;
        FEnded: longint;
        FKind: THistoryRunKind;
        FStopped: boolean;
        procedure FitRunEnded(Sender: TObject; AKind: THistoryRunKind;
            AStopped: boolean);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFitThatEndedSaysWhichRunItWas;
        procedure SoDoesAReduction;
        procedure AndAnAutomaticRunOnceForAllItsStages;
        procedure AStoppedRunSaysItWasStopped;
        procedure TheNextRunIsNotStoppedBecauseTheLastOneWas;
        procedure ARunThatFailedEndsWithoutAResult;
        procedure AComputationIsNotAFit;
    end;

implementation

const
    BASE = 'http://localhost:8080';

procedure TRunEndClient.RunAsync(AOp: TServerOp; ADone: TThreadMethod);
begin
    try
        if FStopDuringRun then
            StopAsyncOper;
        if Assigned(AOp) then
            AOp;
    except
        //  AS TServerCallThread.Execute: reported, and the completion still
        //  runs - the run is over either way.
        on E: Exception do ;
    end;
    if Assigned(ADone) then
        ADone;
end;

procedure TClientRunEndTest.FitRunEnded(Sender: TObject; AKind: THistoryRunKind;
    AStopped: boolean);
begin
    Inc(FEnded);
    FKind := AKind;
    FStopped := AStopped;
end;

procedure TClientRunEndTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
    FView := TMockFitViewer.Create;
    FClient := TRunEndClient.Create;
    FClient.FitService := FSvc;
    FClient.FFitViewer := FView;
    FClient.OnFitRunEnded := @FitRunEnded;
    FSvc.Reply('profile', '{"title":"p","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('curves', '{"curves":[]}');
    FEnded := 0;
end;

procedure TClientRunEndTest.TearDown;
begin
    FreeAndNil(FClient);
    FreeAndNil(FView);
    FreeAndNil(FSvc);
end;

procedure TClientRunEndTest.AFitThatEndedSaysWhichRunItWas;
begin
    FClient.MinimizeDifference;
    AssertEquals('told once', 1, FEnded);
    AssertEquals('which run', Ord(hrkMinimizeDifference), Ord(FKind));
    AssertFalse('not stopped', FStopped);
end;

procedure TClientRunEndTest.SoDoesAReduction;
begin
    FClient.MinimizeNumberOfCurves;
    AssertEquals('told once', 1, FEnded);
    AssertEquals('which run', Ord(hrkMinimizeNumberOfCurves), Ord(FKind));
end;

procedure TClientRunEndTest.AndAnAutomaticRunOnceForAllItsStages;
begin
    FClient.DoAllAutomatically;
    AssertEquals('told once, not per stage', 1, FEnded);
    AssertEquals('which run', Ord(hrkAutomatically), Ord(FKind));
end;

procedure TClientRunEndTest.AStoppedRunSaysItWasStopped;
begin
    //  WHAT STOP REACHED IS THE MODEL (AGENTS.md, "Runs are incremental"), so
    //  it is recorded - and said to have been stopped, because a model a run
    //  did not finish is not the same claim as one it did.
    FClient.StopDuringRun := True;
    FClient.MinimizeDifference;
    AssertEquals('still told', 1, FEnded);
    AssertTrue('and stopped', FStopped);
end;

procedure TClientRunEndTest.TheNextRunIsNotStoppedBecauseTheLastOneWas;
begin
    //  A stop belongs to one operation, never to the client.
    FClient.StopDuringRun := True;
    FClient.MinimizeDifference;
    FClient.StopDuringRun := False;
    FClient.MinimizeDifference;
    AssertFalse('the second ran to its end', FStopped);
end;

procedure TClientRunEndTest.ARunThatFailedEndsWithoutAResult;
begin
    //  A REFUSED OR BROKEN RUN changed nothing a history should keep, and the
    //  model it would record is the one before it - recorded again as if a run
    //  had produced it.
    FSvc.FailNextSendWith('the server refused');
    FClient.MinimizeDifference;
    AssertEquals('nothing to record', 0, FEnded);
end;

procedure TClientRunEndTest.AComputationIsNotAFit;
begin
    FSvc.Reply('rfactor-bounds', '{"title":"s","x":[1,2],"y":[1,1]}');
    FClient.ComputeCurveBounds;
    AssertEquals('proposing bounds is not a run that reaches a model', 0,
        FEnded);
end;

initialization
    //  In place: no thread, no socket.
    RegisterTest('unit', TClientRunEndTest);
end.
