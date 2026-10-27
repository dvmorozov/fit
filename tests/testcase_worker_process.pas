// SPDX-License-Identifier: GPL-3.0-or-later
{ Integration test: starts the actual compute-server executable and talks to it
  over HTTP, covering fit_server's request handling end to end (the unit tests in
  testcase_worker_protocol cover the JSON wire format only).

  WHAT A FIT OVER THE WIRE NEEDS IS ELSEWHERE. The server started here is a
  product's, and a fit sent to it names a curve type that server must build -
  which only a module provides, the framework shipping none. So the three tests
  that fit through the started server live with the scientific module
  (testcase_worker_process_peaks); what is left here is the transport. }
unit testcase_worker_process;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry, fpjson, process, child_process, fphttpclient,
  ssockets, fit_worker_protocol, fit_problem_json, fit_task, fit_task_marshalling,
  server_fit_backend, int_fit_backend, curve_points_set, self_copied_component,
  worker_process_harness;
type
  TWorkerProcessTest = class(TTestCase)
  published
    procedure ServesHealthAndRejectsUnknown;
    procedure TwoRequestsAreAnsweredOnOneConnection;
    procedure AKeptConnectionAnswersWithoutADelayedAckStall;
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

  P := TChildProcess.Create(nil);
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

{ Reads exactly one HTTP response from AStream: its headers, then the body
  Content-Length announces. Returns what arrived, or '' when the peer closed
  without answering - which is the case this test exists to tell apart. }
function ReadOneResponse(AStream: TSocketStream): string;
var
  Buf: array[0..2047] of char;
  R, HeaderEnd, Len, Q: longint;
  Got, Chunk, Line: string;
begin
  Result := '';
  Got := '';
  HeaderEnd := 0;
  repeat
    R := AStream.Read(Buf, SizeOf(Buf));
    if R <= 0 then
      Break;
    SetString(Chunk, PChar(@Buf[0]), R);
    Got := Got + Chunk;
    HeaderEnd := Pos(#13#10#13#10, Got);
  until HeaderEnd > 0;
  if HeaderEnd = 0 then
    Exit;
  Len := 0;
  Q := Pos('content-length:', LowerCase(Got));
  if Q > 0 then
  begin
    Line := Copy(Got, Q, MaxInt);
    R := Pos(#13#10, Line);
    if R > 0 then
      Len := StrToIntDef(Trim(Copy(Line, 16, R - 16)), 0);
  end;
  while Length(Got) < HeaderEnd + 3 + Len do
  begin
    R := AStream.Read(Buf, SizeOf(Buf));
    if R <= 0 then
      Break;
    SetString(Chunk, PChar(@Buf[0]), R);
    Got := Got + Chunk;
  end;
  Result := Got;
end;

{ THE SERVER MUST NOT CLOSE THE CONNECTION AFTER EVERY REPLY. The window polls
  the running fit several times a second, and each poll that has to connect
  anew puts the UI thread on the accept queue of a server busy with the fit -
  which is how the window once drew three frames in fifty seconds. The client
  asks for the connection to be kept; this asks the server, over a raw socket,
  whether it honours that. }
procedure TWorkerProcessTest.TwoRequestsAreAnsweredOnOneConnection;
const
  KEEP_PORT = 8798;
var
  P: TProcess;
  C: TFPHTTPClient;
  S: TInetSocket;
  Req, First, Second, Url: string;
  Tries: integer;
begin
  AssertTrue('server binary exists: ' + WorkerServerPath, FileExists(WorkerServerPath));
  KillStaleWorker(KEEP_PORT);

  P := TChildProcess.Create(nil);
  try
    P.Executable := WorkerServerPath;
    P.Parameters.Add('--port');
    P.Parameters.Add(IntToStr(KEEP_PORT));
    P.Execute;

    //  Wait for the accept loop, through an ordinary client.
    C := TFPHTTPClient.Create(nil);
    try
      Url := Format('http://127.0.0.1:%d', [KEEP_PORT]);
      for Tries := 1 to 50 do
      begin
        try
          C.Get(Url + '/health');
          Break;
        except
          Sleep(100);
        end;
      end;
    finally
      C.Free;
    end;

    Req := 'GET /health HTTP/1.1'#13#10 + 'Host: 127.0.0.1'#13#10 +
      'Connection: keep-alive'#13#10#13#10;
    S := TInetSocket.Create('127.0.0.1', KEEP_PORT);
    try
      S.IOTimeout := 5000;
      S.WriteBuffer(Req[1], Length(Req));
      First := ReadOneResponse(S);
      AssertTrue('the first request was answered', Pos(' 200 ', First) > 0);
      //  The same socket, a second time. A server that closed after its first
      //  reply answers nothing at all here.
      S.WriteBuffer(Req[1], Length(Req));
      Second := ReadOneResponse(S);
      AssertTrue('the second request was answered on the same connection',
        Pos(' 200 ', Second) > 0);
    finally
      S.Free;
    end;
  finally
    P.Terminate(0);
    P.WaitOnExit;
    P.Free;
  end;
end;

{ FORTY MILLISECONDS A REQUEST, on localhost, for a reply the server had ready in
  none. The server writes a reply's headers and its body separately; with
  Nagle's algorithm on, the body waited for the client to acknowledge the
  headers, and the client delays that acknowledgement by 40 ms. Unseen on an
  ordinary fit; after Stop in an automatic run the window fetches every curve
  it left - over a thousand requests - and sat for fifty-five seconds. }
procedure TWorkerProcessTest.AKeptConnectionAnswersWithoutADelayedAckStall;
const
  NODELAY_PORT = 8799;
  REQUESTS = 20;
  { A stalled request costs 40 ms; an unstalled one on localhost, one or two. }
  BUDGET_MS = REQUESTS * 15;
var
  P: TProcess;
  C: TFPHTTPClient;
  S: TInetSocket;
  Req, Url: string;
  Tries, i: integer;
  Started: QWord;
  Took: QWord;
begin
  AssertTrue('server binary exists: ' + WorkerServerPath, FileExists(WorkerServerPath));
  KillStaleWorker(NODELAY_PORT);

  P := TChildProcess.Create(nil);
  try
    P.Executable := WorkerServerPath;
    P.Parameters.Add('--port');
    P.Parameters.Add(IntToStr(NODELAY_PORT));
    P.Execute;

    C := TFPHTTPClient.Create(nil);
    try
      Url := Format('http://127.0.0.1:%d', [NODELAY_PORT]);
      for Tries := 1 to 50 do
      begin
        try
          C.Get(Url + '/health');
          Break;
        except
          Sleep(100);
        end;
      end;
    finally
      C.Free;
    end;

    Req := 'GET /health HTTP/1.1'#13#10 + 'Host: 127.0.0.1'#13#10 +
      'Connection: keep-alive'#13#10#13#10;
    S := TInetSocket.Create('127.0.0.1', NODELAY_PORT);
    try
      S.IOTimeout := 5000;
      Started := GetTickCount64;
      for i := 1 to REQUESTS do
      begin
        S.WriteBuffer(Req[1], Length(Req));
        AssertTrue('answered', Pos(' 200 ', ReadOneResponse(S)) > 0);
      end;
      Took := GetTickCount64 - Started;
      AssertTrue(Format('%d requests on one connection took %d ms',
        [REQUESTS, Took]), Took < BUDGET_MS);
    finally
      S.Free;
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
