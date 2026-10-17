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

The worker is Worker/py/fit_backend.py, run with the Python of the virtual
environment the user guide's 'Setting up the Python engine' sets up. That environment is per
MACHINE and shared by every checkout on it - see sidecar_launch.SidecarPyHome -
with the old in-tree Worker/py/.venv still honoured where one is left. If no
interpreter and no script can be found, the sidecar is simply unavailable and
fit_server falls back to (or reports) that - the native engine never depends on
Python.
}
unit python_sidecar;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, DateUtils, process, fphttpclient, log, sidecar_launch,
    readiness_channel;

const
    { HOW LONG ONE CALLER WAITS FOR A STARTED SIDECAR TO SAY IT IS LISTENING:
      five minutes. It was ten seconds, "long enough for a cold import of numpy,
      scipy and lmfit" - but the FROZEN sidecar an installer carries took 114 s
      on its first start on an Intel Mac, while macOS looked over each of its
      two hundred new libraries once (4 s on every start after). Ten seconds
      refused it every time.

      A long budget is bearable because the wait is no longer hidden: it runs
      inside the fit, the progress view says what it is waiting for, and Stop
      ends it (TFitService.PythonUrlForTask). And it ends early by itself: when
      the sidecar says it is ready, or the moment it dies and its end of the
      lifeline closes. A wait that runs out ends only the wait - the child goes
      on, and the next caller resumes waiting on it. }
    SidecarReadyBudgetMs = 300000;

type
    { Owns one Python sidecar process. }
    TPythonSidecar = class(TObject)
    private
        FProcess: TProcess;
        FPort:    word;
        FPyExe:   string;
        FScript:  string;
        { The lifeline to the running sidecar: freeing it ends the sidecar. }
        FListener: TReadinessListener;
        { A child has been started and has not yet been heard to be ready, nor
          to have died. Such a child is waited on, never started again: a
          start is slow, and a second one only starts the clock over. }
        FStarting: boolean;
        { ONE CALLER AT A TIME. The server answers each request on a thread of
          its own, and two of them used to reach this object at once - a fit
          waiting for the start, and the Stop pressed during it, whose request
          started a second child and freed the lifeline the first was blocked
          on. Taken with TryEnter, so a caller waiting for its turn can still
          be told to stop. }
        FLock: TRTLCriticalSection;
        function BaseUrl: string;
        { Takes FLock unless AStop says to stop first, or ADeadline passes. }
        function TakeTurn(AStop: TReadinessStop; ADeadline: TDateTime): boolean;
    protected
        { THE THREE THINGS EnsureRunning DOES THAT ARE NOT DECISIONS - one HTTP
          request, starting a child, and waiting for it to say it is listening.
          They are separated so the start-up sequence around them can be driven
          without a Python installation, a port, or a second of real time. }
        { True when something is already answering on the sidecar's port. }
        function HealthOk: boolean; virtual;
        { Starts the child process with a fresh lifeline. False when it would
          not start at all - a missing interpreter, a refused exec. }
        function StartProcess: boolean; virtual;
        { Blocks until the started child says it is listening, dies, or
          ABudgetMs passes - an event with a deadline, not a probe repeated. }
        function WaitUntilReady(ABudgetMs: longint;
            AStop: TReadinessStop): TReadiness; virtual;
    public
        constructor Create;
        destructor Destroy; override;

        { True when the sidecar could be located (venv + script present). }
        function IsConfigured: boolean; virtual;
        { Ensures the worker is running and answering; returns its base URL, or
          '' when it cannot be started or AStop said to stop waiting. Reuses an
          already-running worker, and a start already under way - its own or an
          earlier caller's. }
        function EnsureRunning(AStop: TReadinessStop = nil): string;
        { Starts the worker without waiting for it, unless it is answering or
          already starting: the slow first start is then spent before anything
          needs it. Returns at once, also when another caller holds the turn -
          that caller is starting it. }
        procedure StartInBackground;
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

{ Where the sidecar's interpreter and script are, for a server binary in ABinDir
  and a machine whose environments are under APyHome (sidecar_launch.SidecarPyHome
  answers that). Either output is '' when it could not be found.

  THE PROBING HALF, and it takes both of its inputs as ARGUMENTS rather than
  asking the process for them. Which paths to try is a decision and lives in
  sidecar_launch, where it is a pure function and tested as one; what is left
  here has to open directories, so the only way to state what it does is to
  build a tree and let it look. ParamStr(0) and the environment would each make
  that tree unreachable, so neither is read below this line - LocatePython
  supplies them, and is the one caller that does. }
procedure LocateSidecarIn(const ABinDir, APyHome: string;
    out APyExe, AScript: string);

{ Whether what LocateSidecarIn found can be started: a script that exists, or
  - with no script - a frozen sidecar that does. }
function SidecarConfigured(const APyExe, AScript: string): boolean;

implementation

const
    //  The sidecar's loopback port. fit_server owns it; the client never uses it.
    SIDECAR_PORT = 8788;
    //  How long a caller waiting for its turn sleeps between asking for it.
    TurnPollMs = 50;


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

{ The script in a repository checked out beside the one a binary in ABinDir was
  built in, or ''. Which paths those are is a decision and lives in
  sidecar_launch; listing a directory to learn the names is not, so it is done
  here. }
function SiblingScript(const ABinDir: string): string;
var
    Umbrellas, Names, Candidates: TStringList;
    Rec: TSearchRec;
    i: integer;
begin
    Result := '';
    Umbrellas := TStringList.Create;
    try
        SidecarUmbrellaCandidates(Umbrellas, ABinDir);
        for i := 0 to Umbrellas.Count - 1 do
        begin
            Names := TStringList.Create;
            Candidates := TStringList.Create;
            try
                if FindFirst(Umbrellas[i] + '*', faDirectory, Rec) = 0 then
                begin
                    repeat
                        if ((Rec.Attr and faDirectory) <> 0) and
                           (Rec.Name <> '.') and (Rec.Name <> '..') then
                            Names.Add(Rec.Name);
                    until FindNext(Rec) <> 0;
                    FindClose(Rec);
                end;
                //  SORTED, so which repository answers does not depend on the
                //  order the file system happens to return entries in - two
                //  machines with the same checkout would otherwise run
                //  different scripts.
                Names.Sort;
                SidecarSiblingCandidates(Candidates, Umbrellas[i], Names);
                Result := FirstExisting(Candidates);
            finally
                Candidates.Free;
                Names.Free;
            end;
            if Result <> '' then
                Exit;
        end;
    finally
        Umbrellas.Free;
    end;
end;

procedure LocateSidecarIn(const ABinDir, APyHome: string;
    out APyExe, AScript: string);
var
    PyDir: string;
    Venv: string;
    Candidates: TStringList;
begin
    APyExe := '';
    AScript := '';

    //  AN INSTALLED COPY CARRIES ITS SIDECAR FROZEN, and it wins: it is the
    //  one built with the modules this build has, and it needs no Python on
    //  the machine. Nothing below is looked at when it is there.
    Candidates := TStringList.Create;
    try
        FrozenSidecarCandidates(Candidates, ABinDir);
        APyExe := FirstExisting(Candidates);
    finally
        Candidates.Free;
    end;
    if APyExe <> '' then
    begin
        APyExe := ExpandFileName(APyExe);
        Exit;
    end;

    Candidates := TStringList.Create;
    try
        SidecarScriptCandidates(Candidates, ABinDir);
        PyDir := FirstExisting(Candidates);
    finally
        Candidates.Free;
    end;
    //  A MODULE BUILD'S SERVER runs from its own repository, which holds the
    //  pack's sidecar files but not the framework's script - so none of the
    //  candidates above can match, and without this the sidecar is unavailable in
    //  every private build: the features that need it answer "the Python
    //  component could not be started" on a machine where it works.
    if PyDir = '' then
        PyDir := SiblingScript(ABinDir);
    if PyDir = '' then
        Exit;
    AScript := ExpandFileName(PyDir);
    PyDir := ExtractFilePath(AScript);

    //  The same treatment the script gets: a list of candidates from
    //  sidecar_launch, probed here. There is more than one place to look since
    //  the environment moved out of the checkout - the shared one first, the
    //  in-tree .venv a checkout may still have second.
    Candidates := TStringList.Create;
    try
        SidecarPythonCandidates(Candidates, APyHome, PyDir);
        Venv := FirstExisting(Candidates);
    finally
        Candidates.Free;
    end;
    if Venv <> '' then
        APyExe := ExpandFileName(Venv)
    else
        //  Fall back to a system Python; may still have the libraries.
        APyExe := SystemPython;
end;

{ Locates the sidecar's Python executable and script, relative to the running
  fit_server binary (dev layout Worker/o[/arch]/fit_server, script Worker/py/). }
procedure LocatePython(out APyExe, AScript: string);
begin
    //  THE ONLY PLACE the two inputs are read from the process. Everything the
    //  answer depends on is an argument from here down, which is what lets the
    //  search be stated over a tree a test builds instead of the one this
    //  binary happens to be sitting in.
    LocateSidecarIn(ExtractFilePath(ExpandFileName(ParamStr(0))), SidecarPyHome,
                    APyExe, AScript);
end;

constructor TPythonSidecar.Create;
begin
    inherited Create;
    InitCriticalSection(FLock);
    FPort := SIDECAR_PORT;
    LocatePython(FPyExe, FScript);
end;

destructor TPythonSidecar.Destroy;
begin
    Stop;
    DoneCriticalSection(FLock);
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

function SidecarConfigured(const APyExe, AScript: string): boolean;
begin
    if AScript <> '' then
        Result := FileExists(AScript)
    else
        Result := (APyExe <> '') and FileExists(APyExe);
end;

function TPythonSidecar.IsConfigured: boolean;
begin
    Result := SidecarConfigured(FPyExe, FScript);
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
    //  A FRESH LIFELINE AND A FRESH COMMAND LINE for every start: the port is
    //  chosen by the operating system each time, and a child restarted with an
    //  old port would announce to nobody.
    Stop;
    FListener := TReadinessListener.Create;
    FProcess := TProcess.Create(nil);
    FProcess.Executable := FPyExe;
    //  The argument list is built in sidecar_launch. What is passed to a child
    //  process cannot be observed after the fact, so the decision is kept
    //  somewhere a test can read it back.
    BuildSidecarArgs(FProcess.Parameters, FScript, FPort, FListener.Port,
        GetLogDir + 'fit_sidecar_log.txt', SidecarModules);
    //  Detach its output so it does not fill a pipe and block.
    FProcess.Options := [];
    FProcess.ShowWindow := swoHIDE;
    Result := True;
    try
        FProcess.Execute;
    except
        Result := False;
    end;
end;

function TPythonSidecar.WaitUntilReady(ABudgetMs: longint;
    AStop: TReadinessStop): TReadiness;
begin
    if not Assigned(FListener) then
        Exit(rdEnded);
    Result := FListener.WaitForReady(ABudgetMs, AStop);
end;

function TPythonSidecar.TakeTurn(AStop: TReadinessStop;
    ADeadline: TDateTime): boolean;
begin
    Result := True;
    while TryEnterCriticalSection(FLock) = 0 do
    begin
        if (Assigned(AStop) and AStop()) or (Now >= ADeadline) then
            Exit(False);
        Sleep(TurnPollMs);
    end;
end;

procedure TPythonSidecar.StartInBackground;
begin
    //  SOMEONE HOLDS THE TURN: they are starting it, or using it. Either way
    //  there is nothing to do, and a request that only chose an engine must not
    //  wait minutes behind a fit's start.
    if TryEnterCriticalSection(FLock) = 0 then
        Exit;
    try
        if FStarting or HealthOk or not IsConfigured then
            Exit;
        FStarting := StartProcess;
        if FStarting then
            WriteLog('Python sidecar started ahead of need', Notification);
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TPythonSidecar.EnsureRunning(AStop: TReadinessStop): string;
begin
    Result := '';
    if not TakeTurn(AStop, IncMilliSecond(Now, SidecarReadyBudgetMs)) then
    begin
        WriteLog('Python sidecar: stopped waiting for another caller''s start',
            Notification);
        Exit;
    end;
    try
        //  REUSE A WORKER THAT IS ALREADY ANSWERING, whether this object
        //  started it or the developer did by hand. Starting a second one would
        //  bind the same port, fail, and be reported as "the sidecar cannot
        //  start".
        if HealthOk then
        begin
            //  AND A START THAT TIMED OUT IS OVER once it answers. Left
            //  "starting", a child that later died would be waited on again -
            //  and its lifeline still holds the "ready" it wrote, so the wait
            //  would hand back a URL nothing listens on.
            FStarting := False;
            Exit(BaseUrl);
        end;
        //  Nothing to start: no interpreter or no script was found. Answering
        //  '' rather than raising is what lets the native engine carry on.
        if not IsConfigured then
            Exit;
        //  A START UNDER WAY IS WAITED ON, NOT REPEATED: started ahead of need,
        //  or by a caller that stopped waiting or ran out of patience.
        if not FStarting then
        begin
            if not StartProcess then
                Exit;
            FStarting := True;
        end;

        //  WAITING IS NOT OPTIONAL: the worker imports numpy, scipy and lmfit
        //  before it binds. It says when it is listening, over the lifeline; a
        //  sidecar that dies importing closes it, and the wait ends at once.
        case WaitUntilReady(SidecarReadyBudgetMs, AStop) of
            rdReady:
            begin
                FStarting := False;
                Result := BaseUrl;
            end;
            rdEnded:
            begin
                //  Dead: the next caller starts another.
                FStarting := False;
                WriteLog('Python sidecar exited before it was listening', Warning);
            end;
            rdTimedOut:
                WriteLog(Format('Python sidecar did not say it was listening ' +
                    'within %d ms; it is left starting', [SidecarReadyBudgetMs]),
                    Warning);
            rdAbandoned:
                WriteLog('Python sidecar: the caller stopped waiting; it is ' +
                    'left starting', Notification);
        end;
    finally
        LeaveCriticalSection(FLock);
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
    FStarting := False;
    //  The lifeline first: its end is what tells the sidecar to exit, which it
    //  does even if the terminate below never reaches it.
    FreeAndNil(FListener);
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
