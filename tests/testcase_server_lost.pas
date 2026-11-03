// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A run whose compute server dies still ends for whatever waits on it.)

THE DEFECT (2026-10-27, the publishing pipeline). A script waits for each
step's run to end (IAutomationHost.WhenIdle), and the window told it so in the
last lines of its end-of-run handler. That handler first reads the server - the
run's time for the status bar - and with the server dead the read raised, the
handler stopped there, and the script waited for ever: the window idle in its
event loop, no CPU, for seven and a half hours, with nothing in any log the
pipeline kept.

THROUGH THE REAL THINGS: a real fit_server, a run the client starts on its
own thread and its own end of that run. The run is Model > Background >
Compute rather than the pipeline's Fit > Automatically: every run of the
client ends through the same EndRun, and an automatic fit of the synthetic
peak took longer than this test may wait on a busy test machine. What stands in for the
window is the one thing a test cannot build - an LCL form - and it does what the
window's handler does first: it asks the server for the run's time. The server
is killed right before that read, so the order does not depend on how fast the
fit is: a server dying during the fit reaches the same read the same way.

Integration: it starts a server, fits, and kills the server.
}
unit testcase_server_lost;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    {$IFDEF UNIX}BaseUnix, {$ENDIF}
    worker_process_harness, fit_client, fit_client_app, main_calc_thread,
    mock_fit_viewer, data_loader_registration, title_points_set;

type
    TServerLostTest = class(TWorkerProcessTest)
    private
        FApp: TFitClientApp;
        FView: TMockFitViewer;
        FDataPath: string;
        FRunOver: boolean;
        { Whether the window's handler was reached at all, so a failure says
          which half of the end did not come. }
        FEndTakenIn: boolean;
        { The window's end-of-run handler, as far as it matters here: the
          server is killed, and then asked for the run's time. }
        procedure WindowTakesInTheEnd(Sender: TObject);
        procedure RunIsOver(Sender: TObject);
        procedure RememberError(const AMessage: string);
        procedure KillTheServer;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ARunWhoseServerDiedIsStillOverForWhatWaitsOnIt;
    end;

implementation

var
    { What OnCalcError was told. Unit-level because the hook is process-wide. }
    ReportedError: string;

procedure TServerLostTest.SetUp;
var
    Profile: TTitlePointsSet;
    Lines: TStringList;
    i: integer;
    Dot: TFormatSettings;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    //  The harness's synthetic peak, as a file the ordinary import reads.
    FDataPath := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-server-lost.dat';
    Profile := GaussianProfile;
    Dot := DefaultFormatSettings;
    Dot.DecimalSeparator := '.';
    Lines := TStringList.Create;
    try
        for i := 0 to Profile.PointsCount - 1 do
            Lines.Add(Format('%g %g', [Profile.PointXCoord[i],
                Profile.PointYCoord[i]], Dot));
        Lines.SaveToFile(FDataPath);
    finally
        Lines.Free;
        Profile.Free;
    end;
    FApp := TFitClientApp.Create;
    FView := TMockFitViewer.Create;
    FApp.FitClient.FFitViewer := FView;
    FApp.FitClient.FProgressView := FView;
    FApp.FitClient.FitService := FSvc;
    FApp.FitClient.OnAsyncOperationFinished := @WindowTakesInTheEnd;
    FApp.FitClient.OnRunOver := @RunIsOver;
    FRunOver := False;
    FEndTakenIn := False;
    ReportedError := '';
    main_calc_thread.OnCalcError := @RememberError;
end;

procedure TServerLostTest.TearDown;
begin
    //  Unhooked first: the hook is process-wide.
    main_calc_thread.OnCalcError := nil;
    if Assigned(FApp) then
    begin
        FApp.FitClient.OnAsyncOperationFinished := nil;
        FApp.FitClient.OnRunOver := nil;
    end;
    FreeAndNil(FApp);
    FreeAndNil(FView);
    DeleteFile(FDataPath);
    inherited TearDown;
end;

procedure TServerLostTest.KillTheServer;
begin
    //  SIGKILL, as the kernel's out-of-memory killer ends a process: no
    //  shutdown of its own, no last line in its log.
{$IFDEF UNIX}
    fpKill(FProc.ProcessID, SIGKILL);
{$ELSE}
    FProc.Terminate(9);
{$ENDIF}
    FProc.WaitOnExit;
end;

procedure TServerLostTest.WindowTakesInTheEnd(Sender: TObject);
begin
    FEndTakenIn := True;
    KillTheServer;
    //  What TFormMain.AsyncOperationFinished reads first (ShowTime).
    FApp.FitClient.GetCalcTimeStr;
end;

procedure TServerLostTest.RunIsOver(Sender: TObject);
begin
    FRunOver := True;
end;

procedure TServerLostTest.RememberError(const AMessage: string);
begin
    ReportedError := AMessage;
end;

procedure TServerLostTest.ARunWhoseServerDiedIsStillOverForWhatWaitsOnIt;
var
    Deadline: TDateTime;
begin
    FApp.FitClient.LoadDataSet(FDataPath);
    FApp.FitClient.ComputeBackgroundPoints;
    //  The run's end comes through Synchronize, which only this loop runs.
    Deadline := Now + 60 / SecsPerDay;
    while not FRunOver and (Now < Deadline) do
        CheckSynchronize(10);
    AssertTrue('the run ended and the window was told (error so far: ' +
        ReportedError + ')', FEndTakenIn);
    AssertTrue('the run is over for whatever waits on it, though the ' +
        'window''s account of its end failed with the server gone', FRunOver);
    AssertTrue('the failure names the compute server: ' + ReportedError,
        Pos('compute server', ReportedError) > 0);
end;

initialization
    //  It starts a compute server, fits, and kills the server.
    RegisterTest('integration', TServerLostTest);
end.
