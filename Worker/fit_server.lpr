// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The compute server executable.)

An independent HTTP+JSON server that runs the fitting engine (decisions D7/D12):
the desktop client connects to it by a configured URL (default
http://127.0.0.1:8787), and the server can equally be deployed on another
machine. It is started separately from the client, not spawned by it.

Endpoints:
  GET  /health -> ok flag + protocol version, as JSON
  POST /fit    -> body is a TFitProblem JSON; reply is a TFitOutcome JSON

Usage: fit_server [--host H] [--port N] [--log-level L] [--verbose]

Logging: the server writes fit_server_log.txt in the config directory (the
client keeps log.txt, so the two processes never share a file). --log-level
takes fatal|warning|notification|debug - debug adds engine state transitions,
progress callbacks and exception stack traces. --verbose also echoes the log to
stderr.
}
program fit_server;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    //  Must come first: the threaded HTTP server needs a thread manager, and
    //  without it the process dies with runtime error 232 on the first connection.
    cthreads,
    {$ENDIF}
    Interfaces,   //  LCL nogui widgetset (the engine links LCL headlessly)
    SysUtils, Classes, fphttpserver, httpdefs, fpjson,
    fit_worker_protocol, fit_problem_json, fit_task, fit_task_marshalling,
    fit_rest_api, python_sidecar, log,
    //  One unit names every curve type, and RegisterAllCurveTypes below CHECKS
    //  that they are all present. This list used to be maintained by hand right
    //  here, and it was missing a pack's units - so this server silently had
    //  none of its types at all, and selecting one did nothing.
    curve_type_registration, minimizer_registration, app_modules;

const
    DEFAULT_HOST = '127.0.0.1';
    DEFAULT_PORT = 8787;
    LOG_FILE_NAME = 'fit_server_log.txt';

var
    { The REST surface (problems, profiles, actions...). Global so the request
      handler can reach it; the server owns one for its lifetime. }
    GApi: TFitRestApi;
    { The Python (lmfit) sidecar this server owns and supervises. }
    GSidecar: TPythonSidecar;

type
    { Handles HTTP requests for the compute server. }
    TFitServer = class(TObject)
        { Starts the owned Python sidecar on demand and returns its URL, so the
          engine can reach it through the IFitBackend seam (TEnsurePythonSidecar). }
        function EnsurePythonSidecar(out AUrl: string): boolean;
        procedure HandleRequest(Sender: TObject;
            var ARequest: TFPHTTPConnectionRequest;
            var AResponse: TFPHTTPConnectionResponse);
    end;

{ Runs one whole fit: rebuild the task from the problem, optimize, report back. }
function RunFit(const ABody: string): string;
var
    Problem: TFitProblem;
    Outcome: TFitOutcome;
    Task:    TFitTask;
begin
    if not FitProblemFromJson(ABody, Problem) then
    begin
        WriteLog('POST /fit: malformed fit problem', Warning);
        Exit(ErrorResponse('malformed fit problem'));
    end;

    WriteLog(Format('POST /fit: %d profile points, %d curve(s)',
        [Length(Problem.ProfileX), Length(Problem.PositionsX)]), Notification);
    Task := BuildTaskFromProblem(Problem);
    try
        Task.MinimizeDifference;
        Outcome := ReadOutcomeFromTask(Task);
    finally
        Task.Free;
    end;
    WriteLog(Format('POST /fit: done, R-factor %g', [Outcome.RFactor]), Notification);
    Result := FitOutcomeToJson(Outcome);
end;

{ Starts the sidecar if needed and runs one fit on it. Returns False when the
  sidecar cannot be started (no Python / libraries), so the API reports it. }
function TFitServer.EnsurePythonSidecar(out AUrl: string): boolean;
begin
    AUrl := GSidecar.EnsureRunning;
    Result := AUrl <> '';
    if not Result then
        WriteLog('Python sidecar unavailable (not configured or failed to start)',
            Warning);
end;

procedure TFitServer.HandleRequest(Sender: TObject;
    var ARequest: TFPHTTPConnectionRequest;
    var AResponse: TFPHTTPConnectionResponse);
var
    Code: longint;
    Body: string;
begin
    AResponse.ContentType := 'application/json';
    try
        //  The stateless whole-problem fit (used by the curve-fitting backend).
        if (ARequest.Method = 'POST') and (ARequest.URI = '/fit') then
        begin
            AResponse.Content := RunFit(ARequest.Content);
            AResponse.Code := 200;
            Exit;
        end;
        //  Everything else is the REST surface (the IFitService verbs).
        GApi.Handle(ARequest.Method, ARequest.URI, ARequest.Content, Code, Body);
        AResponse.Code := Code;
        AResponse.Content := Body;
    except
        on E: Exception do
        begin
            AResponse.Code := 500;
            AResponse.Content := ErrorResponse(E.Message);
            WriteLog(Format('!!! %s %s -> %s: %s', [ARequest.Method,
                ARequest.URI, E.ClassName, E.Message]), Fatal);
        end;
    end;
end;

{ Reads --host / --port from the command line, falling back to the defaults. }
procedure ParseArgs(out AHost: string; out APort: word;
    out ALevel: TMsgType; out AEcho: boolean);
var
    i: integer;
    Arg: string;

    { The value following a flag, or an empty string when it is the last one. }
    function ValueAfter(var AIndex: integer): string;
    begin
        if AIndex < ParamCount then
        begin
            Inc(AIndex);
            Result := ParamStr(AIndex);
        end
        else
            Result := '';
    end;

begin
    AHost := DEFAULT_HOST;
    APort := DEFAULT_PORT;
    //  Everything, unless asked for less: see log.LogLevel. A server that
    //  logged only Notification during the run that misbehaved cannot be
    //  asked again.
    ALevel := Debug;
    AEcho := False;
    i := 1;
    while i <= ParamCount do
    begin
        Arg := ParamStr(i);
        if Arg = '--host' then
            AHost := ValueAfter(i)
        else if Arg = '--port' then
            APort := StrToIntDef(ValueAfter(i), DEFAULT_PORT)
        else if Arg = '--log-level' then
        begin
            //  Only ever turns the log down; the default is already the loudest
            //  tier. An unreadable name falls back to that default rather than to
            //  silence.
            if not TryParseLogLevel(ValueAfter(i), ALevel) then
                ALevel := Debug;
        end
        else if (Arg = '--verbose') or (Arg = '-v') then
            AEcho := True;
        Inc(i);
    end;
end;

var
    Server:  TFPHTTPServer;
    Handler: TFitServer;
    Host:    string;
    Port:    word;
    Level:   TMsgType;
    Echo:    boolean;
    Banner:  string;
begin
    ParseArgs(Host, Port, Level, Echo);

    //  Its own file: the desktop client keeps log.txt, and two processes must
    //  not append to one log.
    SetLogFileName(LOG_FILE_NAME);
    SetLogLevel(Level);
    SetLogEcho(Echo);

    //  Before anything can create a curve. Raises, naming what is missing,
    //  rather than letting the server run with a curve type it cannot build -
    //  which is what produced the "hang" that turned out to be 101 auto-curves.
    //  The modules this build contains. The server creates curves, so it must
    //  have exactly the set the client offers - that mismatch is what once left
    //  a whole vertical dead here while the client looked fine.
    RegisterAppModules;
    RegisterAllCurveTypes;
    //  The engines this build offers. Registered here as well as in the client so
    //  the two cannot present different sets - a fit accepted by one and run by
    //  something else on the other is exactly the class of defect that made the
    //  curve types a single declared list in the first place.
    RegisterAllMinimizers;

    GApi := TFitRestApi.Create;
    Handler := TFitServer.Create;
    //  The Python (lmfit) sidecar this server owns; started on first use.
    GSidecar := TPythonSidecar.Create;
    GApi.EnsurePythonSidecar := @Handler.EnsurePythonSidecar;
    if GSidecar.IsConfigured then
        WriteLog('Python sidecar available (started on first use)', Notification)
    else
        WriteLog('Python sidecar not found (native engine only)', Notification);
    Server := TFPHTTPServer.Create(nil);
    try
        //  TODO: bind to Host only (TFPHTTPServer in FPC 3.2.2 has no bind-address
        //  property, so it currently listens on all interfaces); restrict once we
        //  move to a newer server or a descendant. Default connection stays localhost.
        Server.Port := Port;
        //  One connection per thread. Without this a single long action (a fit)
        //  blocks every other request - including the client's state polling,
        //  which runs on its UI thread and would freeze the whole application.
        Server.Threaded := True;
        Server.OnRequest := @Handler.HandleRequest;

        Banner := Format('fit_server (protocol %s) serving http://%s:%d (all interfaces), pid %d',
            [IntToStr(WORKER_PROTOCOL_VERSION), Host, Port, GetProcessID]);
        WriteLog(Banner, Notification);
        WriteLog('logging to ' + GetConfigDir + LOG_FILE_NAME, Notification);
        Writeln(ErrOutput, Banner);
        Writeln(ErrOutput, 'log: ' + GetConfigDir + LOG_FILE_NAME);
        Flush(ErrOutput);

        Server.Active := True;    //  blocks in the accept loop until terminated
    except
        on E: Exception do
        begin
            WriteLog(Format('fit_server failed to start: %s: %s',
                [E.ClassName, E.Message]), Fatal);
            Writeln(ErrOutput, 'fit_server failed to start: ' + E.Message);
            ExitCode := 1;
        end;
    end;
    WriteLog('fit_server stopping', Notification);
    Server.Free;
    GSidecar.Free;   //  stops the sidecar child if it was started
    Handler.Free;
    GApi.Free;
end.
