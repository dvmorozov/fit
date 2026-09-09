// SPDX-License-Identifier: GPL-3.0-or-later
{ Integration test: starts the actual compute-server executable and talks to it
  over HTTP, covering fit_server's request handling end to end (the unit tests in
  testcase_worker_protocol cover the JSON wire format only). }
unit testcase_worker_process;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, fpjson, process, fphttpclient,
  fit_worker_protocol, fit_problem_json, fit_task, fit_task_marshalling,
  server_fit_backend, int_fit_backend, curve_points_set, self_copied_component,
  gauss_points_set, SimpMath, worker_process_harness;
type
  TWorkerProcessTest = class(TTestCase)
  published
    procedure ServesHealthAndRejectsUnknown;
    procedure FitsAGaussianOverHttp;
    procedure ClientBackendFitsViaRunningServer;
    procedure TheServerToStartIsNamedByTheEnvironment;
    procedure WithNothingNamedTheServerBesideTheSuiteIsStarted;
  end;

implementation

const
  //  A fixed, uncommon port for the test server.
  TEST_PORT = 8799;

procedure TWorkerProcessTest.ServesHealthAndRejectsUnknown;
var
  P: TProcess;
  C: TFPHTTPClient;
  Body, Url: string;
  Tries: integer;
  Health: TJSONObject;
  Raised: boolean;
begin
  AssertTrue('server binary exists: ' + WorkerServerPath, FileExists(WorkerServerPath));

  //  Clear the port before binding it - see KillStaleWorker.
  KillStaleWorker(TEST_PORT);

  P := TProcess.Create(nil);
  try
    P.Executable := WorkerServerPath;
    P.Parameters.Add('--port');
    P.Parameters.Add(IntToStr(TEST_PORT));
    P.Execute;

    C := TFPHTTPClient.Create(nil);
    try
      Url := Format('http://127.0.0.1:%d', [TEST_PORT]);

      //  Poll /health until the server is accepting connections.
      Body := '';
      for Tries := 1 to 50 do
      begin
        try
          Body := C.Get(Url + '/health');
          Break;
        except
          Sleep(100);
        end;
      end;

      Health := ParseMessage(Body);
      try
        AssertTrue('health responded with JSON', Assigned(Health));
        AssertTrue('ok is true', Health.Get('ok', False));
        AssertEquals('reports protocol version', WORKER_PROTOCOL_VERSION,
          Health.Get('version', -1));
      finally
        Health.Free;
      end;

      //  An unknown endpoint must yield an HTTP error (the client raises on 4xx);
      //  the server is already up, so this cannot be a connection failure.
      Raised := False;
      try
        C.Get(Url + '/nonsense');
      except
        Raised := True;
      end;
      AssertTrue('unknown endpoint returns an error status', Raised);
    finally
      C.Free;
    end;
  finally
    P.Terminate(0);
    P.WaitOnExit;
    P.Free;
  end;
end;

procedure TWorkerProcessTest.FitsAGaussianOverHttp;
var
  P: TProcess;
  C: TFPHTTPClient;
  Req: TStringStream;
  Problem: TFitProblem;
  Outcome: TFitOutcome;
  Body, Url: string;
  Tries, n, j: integer;
  x, a: double;
begin
  AssertTrue('server binary exists: ' + WorkerServerPath, FileExists(WorkerServerPath));

  //  A synthetic Gaussian, placed at its peak (the position's y seeds amplitude).
  Problem := Default(TFitProblem);
  n := 0;
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    SetLength(Problem.ProfileX, n + 1);
    SetLength(Problem.ProfileY, n + 1);
    Problem.ProfileX[n] := x;
    Problem.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
    Inc(n);
    x := x + 0.2;
  end;
  Problem.PositionsX := TDoubleArray.Create(10);
  Problem.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
  Problem.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
  Problem.MaxRFactor := 0.01;

  P := TProcess.Create(nil);
  try
    P.Executable := WorkerServerPath;
    P.Parameters.Add('--port');
    P.Parameters.Add(IntToStr(TEST_PORT));
    P.Execute;

    C := TFPHTTPClient.Create(nil);
    Req := TStringStream.Create(FitProblemToJson(Problem));
    try
      Url := Format('http://127.0.0.1:%d', [TEST_PORT]);
      //  Wait for the server to listen.
      for Tries := 1 to 50 do
      begin
        try
          C.Get(Url + '/health');
          Break;
        except
          Sleep(100);
        end;
      end;

      C.RequestBody := Req;
      C.AddHeader('Content-Type', 'application/json');
      Body := C.Post(Url + '/fit');
    finally
      Req.Free;
      C.Free;
    end;
  finally
    P.Terminate(0);
    P.WaitOnExit;
    P.Free;
  end;

  AssertTrue('outcome parsed', FitOutcomeFromJson(Body, Outcome));
  AssertEquals('errorCode', 0, Outcome.ErrorCode);
  //  Strictly > 0: a 0 R-factor would mean the optimizer never ran.
  AssertTrue('server fitted (R=' + FloatToStr(Outcome.RFactor) + ')',
    (Outcome.RFactor > 0) and (Outcome.RFactor < 0.05));
  AssertEquals('one fitted curve', 1, Length(Outcome.Curves));

  //  The engine's Gaussian has its own parameterisation, so assert the curve is
  //  centred on the true peak rather than pinning A/sigma to the generating form.
  a := -1;
  for j := 0 to High(Outcome.Curves[0].Params) do
    if Outcome.Curves[0].Params[j].Name = 'x0' then
      a := Outcome.Curves[0].Params[j].Value;
  AssertTrue('fitted centre near 10 (' + FloatToStr(a) + ')', Abs(a - 10) < 0.2);
end;

procedure TWorkerProcessTest.ClientBackendFitsViaRunningServer;
var
  P: TProcess;
  C: TFPHTTPClient;
  Backend: TServerFitBackend;
  Problem: TFitProblem;
  Task: TFitTask;
  Res: TFitResult;
  Tries, n, j: integer;
  x, x0: double;
begin
  //  The whole client->server loop: TServerFitBackend marshals a live TFitTask to
  //  an independently-running server, and writes the fitted values back into it.
  AssertTrue('server binary exists: ' + WorkerServerPath, FileExists(WorkerServerPath));

  Problem := Default(TFitProblem);
  n := 0;
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    SetLength(Problem.ProfileX, n + 1);
    SetLength(Problem.ProfileY, n + 1);
    Problem.ProfileX[n] := x;
    Problem.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
    Inc(n);
    x := x + 0.2;
  end;
  Problem.PositionsX := TDoubleArray.Create(10);
  Problem.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
  Problem.CurveTypeId := GUIDToString(TGaussPointsSet.GetCurveTypeId);
  Problem.MaxRFactor := 0.01;

  P := TProcess.Create(nil);
  try
    P.Executable := WorkerServerPath;
    P.Parameters.Add('--port');
    P.Parameters.Add(IntToStr(TEST_PORT));
    P.Execute;

    C := TFPHTTPClient.Create(nil);
    try
      for Tries := 1 to 50 do
      begin
        try
          C.Get(Format('http://127.0.0.1:%d/health', [TEST_PORT]));
          Break;
        except
          Sleep(100);
        end;
      end;
    finally
      C.Free;
    end;

    //  A live in-process task, fitted by the remote server.
    Task := BuildTaskFromProblem(Problem);
    try
      Backend := TServerFitBackend.Create(
        Format('http://127.0.0.1:%d', [TEST_PORT]));
      try
        AssertTrue('server is available', Backend.IsAvailable);
        Res := Backend.Fit(Task);
      finally
        Backend.Free;
      end;

      AssertEquals('errorCode', 0, Res.ErrorCode);
      AssertTrue('remote fit R-factor (' + FloatToStr(Res.RFactor) + ')',
        (Res.RFactor > 0) and (Res.RFactor < 0.05));

      //  The fitted values must have been written back into the live task.
      x0 := TCurvePointsSet(Task.GetCurves.Items[0]).x0;
      AssertTrue('fitted x0 applied to the local task (' + FloatToStr(x0) + ')',
        Abs(x0 - 10) < 0.2);
    finally
      Task.Free;
    end;
  finally
    P.Terminate(0);
    P.WaitOnExit;
    P.Free;
  end;
end;

procedure TWorkerProcessTest.TheServerToStartIsNamedByTheEnvironment;
begin
  //  The private suite runs against the server that CONTAINS the module, which
  //  is built in the module's own repository - the framework's Worker/o holds no
  //  binary at all there. These tests once hard-coded that path and so failed
  //  every private run on a missing file, which is why FIT_SERVER is honoured in
  //  ONE place and asserted here.
  AssertEquals('FIT_SERVER names the binary outright',
    '/somewhere/else/fit_server', WorkerServerPathFrom('/somewhere/else/fit_server'));
end;

procedure TWorkerProcessTest.WithNothingNamedTheServerBesideTheSuiteIsStarted;
var
  Fallback: string;
begin
  //  The public build: nothing names a server, and the one built beside the
  //  suite is the one to run.
  Fallback := WorkerServerPathFrom('');
  AssertTrue('falls back to the server built beside the suite (' + Fallback + ')',
    Pos('fit_server', Fallback) > 0);
  AssertEquals('and it is an absolute path', ExpandFileName(Fallback), Fallback);
end;

initialization
  RegisterTest('integration', TWorkerProcessTest);
end.
