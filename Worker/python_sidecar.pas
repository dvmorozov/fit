// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Supervises the Python (lmfit) compute sidecar as a child process.)

The Python backend is a subprocess sidecar of fit_server (decision D4): the
desktop client talks only to fit_server, and fit_server - when the Python backend
is used - owns the Python worker. This unit starts that worker on demand, checks
it is alive, and stops it on shutdown. The client never connects to it.

The worker is Worker/py/fit_backend.py, run with the virtualenv Python in
Worker/py/.venv (docs/user-guide/building-from-source.md creates it). If neither
that venv nor
the script can be found, the sidecar is simply unavailable and fit_server falls
back to (or reports) that - the native engine never depends on Python.
}
unit python_sidecar;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, process, fphttpclient, log, sidecar_launch;

const
    { HOW LONG START-UP IS GIVEN: ten seconds, as a hundred tenths. Long enough
      for a cold import of numpy, scipy and lmfit; short enough that a sidecar
      which will never answer does not hold up the first fit for a minute.

      Stated here rather than buried in the loop because it is a budget the user
      pays: it is the pause between asking for a Python fit and being told there
      is no Python. The wait ends early either way - on the first successful
      health check, or as soon as the child is seen to have died. }
    SidecarStartupTries = 100;
    SidecarStartupPollMs = 100;

type
    { Owns one Python sidecar process. }
    TPythonSidecar = class(TObject)
    private
        FProcess: TProcess;
        FPort:    word;
        FPyExe:   string;
        FScript:  string;
        function BaseUrl: string;
    protected
        { THE FOUR THINGS EnsureRunning DOES THAT ARE NOT DECISIONS - one HTTP
          request, two questions about a child process, and a wait. They are
          separated so the start-up sequence around them can be driven without
          a Python installation, a port, or a second of real time.

          Each is the smallest possible piece: the decision of what to do with
          the answer stays in EnsureRunning, which is the part that has never
          been exercised. }

        { True when something is already answering on the sidecar's port. }
        function HealthOk: boolean; virtual;
        { Starts the child process. False when it would not start at all -
          a missing interpreter, a refused exec. }
        function StartProcess: boolean; virtual;
        { True while the child is alive. A worker that died during start-up -
          missing numpy, a syntax error in a module's routes - is the case that
          separates "not ready yet" from "never will be". }
        function ProcessIsRunning: boolean; virtual;
        { One interval of the start-up wait. }
        procedure WaitForStartup; virtual;
    public
        constructor Create;
        destructor Destroy; override;

        { True when the sidecar could be located (venv + script present). }
        function IsConfigured: boolean; virtual;
        { Ensures the worker is running and answering; returns its base URL, or
          '' when it cannot be started. Reuses an already-running worker. }
        function EnsureRunning: string;
        { Sends a fit-problem JSON to the worker's /fit; returns the reply body.
          Raises on transport failure. }
        function Fit(const AProblemJson: string): string;
        { Stops the worker if this object started it. }
        procedure Stop;

        property Port: word read FPort;
    end;

{ Names a module's sidecar route package, so the sidecar imports it on start.

  Called from the module's own registration. A build with no module registers
  nothing, the sidecar is started with no --modules argument, and the generic
  backend answers only the routes it declares itself - which is exactly what the
  public build must do.

  Only the NAME travels: the sidecar finds <name>_routes.py itself - beside its
  own script when installed, and in the Worker/py of a repository checked out
  beside this one while developing - so a module's Python needs no file, and no
  path, in this tree. }
procedure RegisterSidecarModule(const APackage: string);
{ The registered packages, path-separated, or empty. }
function SidecarModules: string;

implementation

const
    //  The sidecar's loopback port. fit_server owns it; the client never uses it.
    SIDECAR_PORT = 8788;
    //  HOW LONG START-UP IS GIVEN: ten seconds, in tenths. Long enough for a
    //  cold import of numpy, scipy and lmfit; short enough that a sidecar which
    //  will never answer does not hold up the first fit for a minute. The wait
    //  ends early either way - on the first successful health check, or as soon
    //  as the child is seen to have died.


{ Locates the sidecar's Python executable and script, relative to the running
  fit_server binary (dev layout Worker/o[/arch]/fit_server, script Worker/py/). }
procedure LocatePython(out APyExe, AScript: string);

var
    BinDir: string;

    { The first of ACandidates that is on disk, or ''. The probing is here and
      the candidate list is in sidecar_launch, because which paths to try is a
      decision and asking the disk is not. }
    function FirstExisting(ACandidates: TStrings): string;
    var
        i: integer;
    begin
        Result := '';
        for i := 0 to ACandidates.Count - 1 do
            if (ACandidates[i] <> '') and FileExists(ACandidates[i]) then
                Exit(ACandidates[i]);
    end;

var
    PyDir: string;
    Venv: string;
    Candidates: TStringList;
begin
    APyExe := '';
    AScript := '';
    BinDir := ExtractFilePath(ExpandFileName(ParamStr(0)));

    Candidates := TStringList.Create;
    try
        SidecarScriptCandidates(Candidates, BinDir);
        PyDir := FirstExisting(Candidates);
    finally
        Candidates.Free;
    end;
    if PyDir = '' then
        Exit;
    AScript := ExpandFileName(PyDir);
    PyDir := ExtractFilePath(AScript);

    Venv := VenvPython(PyDir);
    if FileExists(Venv) then
        APyExe := ExpandFileName(Venv)
    else
        //  Fall back to a system Python; may still have the libraries.
        APyExe := SystemPython;
end;

constructor TPythonSidecar.Create;
begin
    inherited Create;
    FPort := SIDECAR_PORT;
    LocatePython(FPyExe, FScript);
end;

destructor TPythonSidecar.Destroy;
begin
    Stop;
    inherited Destroy;
end;

var
    ModulePackages: string = '';

procedure RegisterSidecarModule(const APackage: string);
begin
    //  The rule itself is in sidecar_launch, where it can be tested. All this
    //  unit owns is the one process-wide list it applies to.
    ModulePackages := AddSidecarModule(ModulePackages, APackage);
end;

function SidecarModules: string;
begin
    Result := ModulePackages;
end;

function TPythonSidecar.IsConfigured: boolean;
begin
    Result := (FScript <> '') and FileExists(FScript);
end;

function TPythonSidecar.BaseUrl: string;
begin
    Result := SidecarBaseUrl(FPort);
end;

function TPythonSidecar.HealthOk: boolean;
var
    C: TFPHTTPClient;
begin
    Result := False;
    C := TFPHTTPClient.Create(nil);
    try
        C.ConnectTimeout := 2000;
        try
            C.Get(SidecarHealthUrl(FPort));
            Result := True;
        except
            Result := False;
        end;
    finally
        C.Free;
    end;
end;

function TPythonSidecar.StartProcess: boolean;
begin
    if not Assigned(FProcess) then
    begin
        FProcess := TProcess.Create(nil);
        FProcess.Executable := FPyExe;
        //  The argument list is built in sidecar_launch. What is passed to a
        //  child process cannot be observed after the fact, so the decision is
        //  kept somewhere a test can read it back.
        BuildSidecarArgs(FProcess.Parameters, FScript, FPort, GetProcessID,
            GetConfigDir + 'fit_sidecar_log.txt', SidecarModules);
        //  Detach its output so it does not fill a pipe and block.
        FProcess.Options := [];
        FProcess.ShowWindow := swoHIDE;
    end;
    Result := True;
    if not FProcess.Running then
        try
            FProcess.Execute;
        except
            Result := False;
        end;
end;

function TPythonSidecar.ProcessIsRunning: boolean;
begin
    Result := Assigned(FProcess) and FProcess.Running;
end;

procedure TPythonSidecar.WaitForStartup;
begin
    Sleep(SidecarStartupPollMs);
end;

function TPythonSidecar.EnsureRunning: string;
var
    Tries: integer;
begin
    Result := '';
    //  REUSE A WORKER THAT IS ALREADY ANSWERING, whether this object started it
    //  or the developer did by hand. Starting a second one would bind the same
    //  port, fail, and be reported as "the sidecar cannot start".
    if HealthOk then
        Exit(BaseUrl);
    //  Nothing to start: no interpreter or no script was found. Answering ''
    //  rather than raising is what lets the native engine carry on.
    if not IsConfigured then
        Exit;
    if not StartProcess then
        Exit;

    //  WAITING IS NOT OPTIONAL: the worker imports numpy, scipy and lmfit
    //  before it binds, which takes about a second on a warm machine and
    //  longer on a cold one. Returning as soon as Execute succeeded would hand
    //  back a URL nothing is listening on yet.
    for Tries := 1 to SidecarStartupTries do
    begin
        if HealthOk then
            Exit(BaseUrl);
        //  IT DIED, so waiting the rest of the budget changes nothing - a
        //  missing library or a syntax error in a module's routes exits at
        //  once. This is what separates "not ready yet" from "never will be",
        //  and without it a misconfigured sidecar costs ten seconds on every
        //  fit before the native engine takes over.
        if not ProcessIsRunning then
            Exit;
        WaitForStartup;
    end;
end;

function TPythonSidecar.Fit(const AProblemJson: string): string;
var
    C: TFPHTTPClient;
    Req, Resp: TStringStream;
begin
    C := TFPHTTPClient.Create(nil);
    Req := TStringStream.Create(AProblemJson);
    Resp := TStringStream.Create('');
    try
        C.RequestBody := Req;
        C.AddHeader('Content-Type', 'application/json');
        C.HTTPMethod('POST', SidecarFitUrl(FPort), Resp, []);
        Result := Resp.DataString;
    finally
        Resp.Free;
        Req.Free;
        C.Free;
    end;
end;

procedure TPythonSidecar.Stop;
begin
    if Assigned(FProcess) then
    begin
        try
            if FProcess.Running then
                FProcess.Terminate(0);
        except
        end;
        FreeAndNil(FProcess);
    end;
end;

end.
