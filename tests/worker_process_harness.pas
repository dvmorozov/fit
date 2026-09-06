// SPDX-License-Identifier: GPL-3.0-or-later
{ A fixture that runs the real fit_server binary for the length of one test.

  Public test infrastructure, not a test: a defect that only exists ACROSS the
  process boundary - a unit linked into the test binary but not into the server -
  cannot be caught inside one process, so any suite that needs to prove something
  about the deployed configuration needs a real worker. A module's suite needs
  exactly that and must not reach into the framework's own fixture for it, so the
  harness is a base class both descend from.

  It publishes nothing, so descending from it adds no tests. }
unit worker_process_harness;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, process, http_fit_service, title_points_set;

type
  TWorkerProcessTest = class(TTestCase)
  protected
    { The worker this test started, and a client bound to it. }
    FProc: TProcess;
    FSvc: THttpFitService;
    function ServerPath: string;
    { A synthetic Gaussian profile; the caller takes ownership. }
    function GaussianProfile: TTitlePointsSet;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

{ Which fit_server binary a test must start. Public, and the ONLY answer to that
  question in the suite: a build containing a module produces a different binary,
  so a test that hard-codes the framework's own path passes in the public build
  and fails in every other one. That is exactly how it failed - the private suite
  looked for a public server that the private build never produces. }
function WorkerServerPath: string;
{ The same answer for a GIVEN value of FIT_SERVER, so the rule can be tested
  without a test writing to its own environment. }
function WorkerServerPathFrom(const AFitServer: string): string;
{ Kills any worker left listening on APort by an earlier run - see the comment on
  the implementation. Public for the same reason: one implementation. }
procedure KillStaleWorker(APort: longint);

implementation

uses Math, SimpMath;

{ Kills any worker left listening on this suite's port by an earlier run.

  Each test starts its own worker on a FIXED port. If a previous run was
  interrupted - a timeout, a killed build - the old process keeps the port, the
  new one fails to bind, and every later test silently talks to the STALE server
  instead. The symptom is not "cannot connect": it is an unrelated assertion
  failing with a stable, believable-looking value (an R-squared just below its
  threshold), which reads exactly like a real regression. That cost a wrong
  diagnosis before the stray process was found, so the suite now clears the port
  itself.

  On Unix the match is the full command line INCLUDING the port, so a developer's
  own fit_server on another port is untouched. }
procedure KillStaleWorker(APort: longint);
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    P.Executable := 'taskkill';
    P.Parameters.Add('/F');
    P.Parameters.Add('/IM');
    P.Parameters.Add('fit_server.exe');
    {$ELSE}
    P.Executable := '/usr/bin/pkill';
    P.Parameters.Add('-f');
    P.Parameters.Add(Format('fit_server --port %d', [APort]));
    {$ENDIF}
    P.Options := [poWaitOnExit, poNoConsole];
    try
      P.Execute;
    except
      //  pkill exits non-zero when nothing matched, and may be absent entirely.
      //  Neither is a reason to fail a test: this is a best-effort cleanup, and
      //  SetUp still verifies the port afterwards.
    end;
  finally
    P.Free;
  end;
end;


const
  TEST_PORT = 8811;

function WorkerServerPathFrom(const AFitServer: string): string;
begin
  //  A build containing a module produces a DIFFERENT server binary, and the
  //  point of these tests is which one is running - so which binary to start is
  //  an input, not a constant. FIT_SERVER names it; without it the suite starts
  //  the server built beside it, which is what the public build has.
  if AFitServer <> '' then
    Exit(AFitServer);
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../Worker/o/fit_server'
    {$IFDEF WINDOWS} + '.exe' {$ENDIF});
end;

function WorkerServerPath: string;
begin
  Result := WorkerServerPathFrom(GetEnvironmentVariable('FIT_SERVER'));
end;

function TWorkerProcessTest.ServerPath: string;
begin
  Result := WorkerServerPath;
end;

procedure TWorkerProcessTest.SetUp;
var
  Tries: integer;
begin
  AssertTrue('server binary exists: ' + ServerPath, FileExists(ServerPath));

  //  Clear the port before binding it - see KillStaleWorker.
  KillStaleWorker(TEST_PORT);

  FProc := TProcess.Create(nil);
  FProc.Executable := ServerPath;
  FProc.Parameters.Add('--port');
  FProc.Parameters.Add(IntToStr(TEST_PORT));
  FProc.Execute;

  FSvc := THttpFitService.Create(Format('http://127.0.0.1:%d', [TEST_PORT]));
  for Tries := 1 to 50 do
  begin
    if FSvc.IsAvailable then
      Break;
    Sleep(100);
  end;

  //  If OUR process died immediately, anything answering on this port is not
  //  ours. Failing here names the real problem, instead of letting a stale
  //  server answer every later call and surface as an unrelated assertion.
  AssertTrue('the worker this test started is running (a stale fit_server on '
    + 'port ' + IntToStr(TEST_PORT) + ' would answer in its place)',
    FProc.Running);
end;

procedure TWorkerProcessTest.TearDown;
begin
  FreeAndNil(FSvc);
  if Assigned(FProc) then
  begin
    FProc.Terminate(0);
    FProc.WaitOnExit;
    FreeAndNil(FProc);
  end;
end;

function TWorkerProcessTest.GaussianProfile: TTitlePointsSet;
var
  x: double;
begin
  Result := TTitlePointsSet.Create(nil);
  Result.FTitle := 'profile';
  x := 0;
  while x <= 20 + 1e-9 do
  begin
    Result.AddNewPoint(x, GaussPoint(100, 1.5, 10, x));
    x := x + 0.2;
  end;
end;

end.
