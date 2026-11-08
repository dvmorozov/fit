// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The stateless fit, and the progress it publishes while it runs.)

POST /fit IS HOW A COMPUTE SERVER IS USED AS A REMOTE BACKEND: another server
sends it a whole problem and waits for the outcome. That wait used to be silent.
Given a progressId, the fit now publishes the best outcome it has reached, and
GET /fit/progress?id= answers with it while the POST is still out - the same
contract the Python sidecar keeps, so the relay on the other side
(remote_fit_progress) cannot tell the two apart.

MOVED OUT OF fit_server.lpr, where RunFit could only be reached by starting the
process. The program now only dispatches to these.

THE STORE IS SHARED BY CONNECTIONS. The HTTP server is threaded: the POST running
the fit and the GETs watching it are served at once, so the store has a lock of
its own, held for a list operation and nothing more. An entry lives exactly as
long as its fit - it is discarded when the fit returns, however it returns.

NEVER A 404 FOR AN ID NOT SEEN. The first poll usually arrives before the first
improvement, and a 404 is how a server WITHOUT this route answers - which is what
tells the relay to stop asking.
}
unit stateless_fit;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, fit_task;

const
    FIT_PROGRESS_ROUTE = '/fit/progress';
    { The least time between two publications. The engine improves far more
      often than anyone polls. }
    STATELESS_PROGRESS_INTERVAL_MS = 100;

type
    { The best outcome each running stateless fit has reached, by progress id. }
    TFitProgressStore = class(TObject)
    private
        FLock: TRTLCriticalSection;
        FItems: TStringList;
    public
        constructor Create;
        destructor Destroy; override;
        { Replaces what the fit AId has reached. Virtual so a test can count
          what a fit publishes. }
        procedure Publish(const AId, AOutcomeJson: string); virtual;
        function Latest(const AId: string; out AOutcomeJson: string): boolean;
        procedure Discard(const AId: string);
    end;

{ GET /fit/progress, with or without its query. }
function IsStatelessFitProgressRoute(const AMethod, AUri: string): boolean;
{ What GET /fit/progress?id= answers: the outcome reached, or nothing yet. }
function StatelessFitProgressReply(AStore: TFitProgressStore;
    const AUri: string): string;
{ Runs one whole fit: rebuild the task from the problem, optimize, report back -
  publishing into AStore as it goes when the problem carries a progress id. }
function RunStatelessFit(const ABody: string; AStore: TFitProgressStore): string;

type
    { The task's progress callback, publishing the outcome it has reached.

      DECLARED HERE rather than in the implementation because it is the half of
      a remote peer's reporting that can be exercised without one: the throttle
      decides how much of a fit a watcher sees, and reading the task decides
      what they see, and neither needs the optimiser to run. RunStatelessFit,
      which is the only other caller, does fit - so it belongs to the
      integration suite and this does not have to. }
    TStatelessProgressHook = class(TObject)
    private
        FTask: TFitTask;
        FStore: TFitProgressStore;
        FId: string;
        FLast: TDateTime;
    public
        constructor Create(ATask: TFitTask; AStore: TFitProgressStore;
            const AId: string);
        procedure Publish;
    end;

implementation

uses
    DateUtils, fit_problem_json, fit_task_marshalling,
    fit_worker_protocol, rest_routes, log;

{ ------------------------------- the store ---------------------------------- }

constructor TFitProgressStore.Create;
begin
    inherited Create;
    InitCriticalSection(FLock);
    FItems := TStringList.Create;
end;

destructor TFitProgressStore.Destroy;
begin
    FItems.Free;
    DoneCriticalSection(FLock);
    inherited Destroy;
end;

procedure TFitProgressStore.Publish(const AId, AOutcomeJson: string);
var
    i: longint;
begin
    EnterCriticalSection(FLock);
    try
        i := FItems.IndexOfName(AId);
        if i >= 0 then
            FItems.ValueFromIndex[i] := AOutcomeJson
        else
            FItems.Add(AId + FItems.NameValueSeparator + AOutcomeJson);
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TFitProgressStore.Latest(const AId: string;
    out AOutcomeJson: string): boolean;
var
    i: longint;
begin
    AOutcomeJson := '';
    EnterCriticalSection(FLock);
    try
        i := FItems.IndexOfName(AId);
        Result := i >= 0;
        if Result then
            AOutcomeJson := FItems.ValueFromIndex[i];
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TFitProgressStore.Discard(const AId: string);
var
    i: longint;
begin
    EnterCriticalSection(FLock);
    try
        i := FItems.IndexOfName(AId);
        if i >= 0 then
            FItems.Delete(i);
    finally
        LeaveCriticalSection(FLock);
    end;
end;

{ ------------------------------- the hook ----------------------------------- }

constructor TStatelessProgressHook.Create(ATask: TFitTask;
    AStore: TFitProgressStore; const AId: string);
begin
    inherited Create;
    FTask := ATask;
    FStore := AStore;
    FId := AId;
end;

procedure TStatelessProgressHook.Publish;
begin
    if (FLast <> 0) and
        (MilliSecondsBetween(Now, FLast) < STATELESS_PROGRESS_INTERVAL_MS) then
        Exit;
    FLast := Now;
    FStore.Publish(FId, FitOutcomeToJson(ReadOutcomeFromTask(FTask)));
end;

{ ------------------------------- the routes --------------------------------- }

function IsStatelessFitProgressRoute(const AMethod, AUri: string): boolean;
var
    Path: string;
    q: longint;
begin
    Path := AUri;
    q := Pos('?', Path);
    if q > 0 then
        Path := Copy(Path, 1, q - 1);
    Result := (AMethod = 'GET') and (Path = FIT_PROGRESS_ROUTE);
end;

function StatelessFitProgressReply(AStore: TFitProgressStore;
    const AUri: string): string;
var
    Id, Json: string;
begin
    Id := QueryParam(AUri, 'id', '');
    if (Id <> '') and AStore.Latest(Id, Json) then
        Result := Json
    else
        Result := '{"ok":true,"found":false,"curves":[]}';
end;

function RunStatelessFit(const ABody: string; AStore: TFitProgressStore): string;
var
    Problem: TFitProblem;
    Outcome: TFitOutcome;
    Task:    TFitTask;
    Hook:    TStatelessProgressHook;
begin
    if not FitProblemFromJson(ABody, Problem) then
    begin
        WriteLog('POST /fit: malformed fit problem', Warning);
        Exit(ErrorResponse('malformed fit problem'));
    end;

    WriteLog(Format('POST /fit: %d profile points, %d curve(s)',
        [Length(Problem.ProfileX), Length(Problem.PositionsX)]), Notification);
    Task := BuildTaskFromProblem(Problem);
    Hook := nil;
    try
        if Problem.ProgressId <> '' then
        begin
            Hook := TStatelessProgressHook.Create(Task, AStore, Problem.ProgressId);
            Task.ServerShowCurMin := @Hook.Publish;
        end;
        Task.MinimizeDifference;
        Outcome := ReadOutcomeFromTask(Task);
    finally
        if Problem.ProgressId <> '' then
            AStore.Discard(Problem.ProgressId);
        Hook.Free;
        Task.Free;
    end;
    WriteLog(Format('POST /fit: done, R-factor %g', [Outcome.RFactor]), Notification);
    Result := FitOutcomeToJson(Outcome);
end;

end.
