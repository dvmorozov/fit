// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Server-side problems (sessions) hosted by the REST compute server.)

The original XML-RPC API was stateful: CreateProblem returned a ProblemID and
every later call carried it. The REST API keeps that model - a problem is a
resource under /problems/id - so several clients (or several documents) can be
served at once.

Each problem owns a plain TFitService. That base class is deliberately the
synchronous one: its RecreateMainCalcThread runs the operation inline, so a REST
call performs the work and returns when it is done. The threaded subclasses use
TThread.Synchronize, which would never be pumped in a headless server.

The session also acts as the service's IClientCallback sink, recording progress
(current R-factor, completion) so a client can poll it.
}
unit fit_server_session;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, Contnrs, int_client_callback, fit_service, log;

type
    { One problem: a fitting service plus the progress it has reported. }
    TFitSession = class(TObject, IClientCallback)
    private
        FId:      longint;
        FService: TFitService;
        FCurMin:  double;
        FDone:    boolean;
        FLock:    TRTLCriticalSection;
        { The settings reply as it stood when the action now running began -
          see PublishSettings. Guarded by FSettingsLock, never by FLock, which
          the running action holds. }
        FSettingsWhileBusy: string;
        FSettingsLock: TRTLCriticalSection;
    public
        constructor Create(AId: longint);
        destructor Destroy; override;

        { The server is threaded, so one problem may be reached by several
          connections at once: the client polls progress on its UI thread while
          an action runs on a worker thread. Everything that touches the engine
          takes this lock; the progress reads (state, async, R-factor) deliberately
          do not, or polling would block behind the very operation it watches. }
        procedure Lock;
        procedure Unlock;
        { Takes the lock if it is free, and answers whether it did. }
        function TryLock: boolean;

        { WHAT GET /settings ANSWERS WHILE AN ACTION HOLDS THE PROBLEM. The
          window refreshes its menus and tool pane while a fit runs, and every
          one of them reads the settings; served under the lock, the route
          waited for the whole fit and the window with it, on its own thread
          (findings.md, "The window froze for a whole automatic run"). The
          settings cannot change while an action holds the problem - a write
          waits for it - so the reply taken as the action began is the answer
          for as long as it runs. Published under the lock, as the action
          starts; withdrawn as it ends. }
        procedure PublishSettings(const AReply: string);
        procedure WithdrawSettings;
        { The published reply, if an action is running. }
        function SettingsWhileBusy(out AReply: string): boolean;

        { IClientCallback - the service reports progress here. Interfaces are
          CORBA-style in this project, so no reference counting is involved. }
        procedure ShowCurMin(Min: double);
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;

        { Clears progress before a new operation. }
        procedure ResetProgress;

        property Id: longint read FId;
        property Service: TFitService read FService;
        { Last R-factor reported by the engine. }
        property CurMin: double read FCurMin;
        { True once an operation has completed. }
        property IsDone: boolean read FDone;
    end;

    { The live problems. Thread-safe: the HTTP server may serve connections from
      several threads. }
    TSessionRegistry = class(TObject)
    private
        FItems:  TObjectList;
        FNextId: longint;
        FLock:   TRTLCriticalSection;
    public
        constructor Create;
        destructor Destroy; override;
        { Creates a new problem and returns its id. }
        function CreateProblem: longint;
        { The problem with this id, or nil. }
        function Find(AId: longint): TFitSession;
        { Destroys the problem; harmless when the id is unknown. }
        procedure Discard(AId: longint);
        function Count: longint;
    end;

implementation

{ TFitSession }

constructor TFitSession.Create(AId: longint);
begin
    inherited Create;
    FId := AId;
    InitCriticalSection(FLock);
    InitCriticalSection(FSettingsLock);
    FService := TFitService.Create;
    //  Receive the engine's progress callbacks.
    FService.FitProxy := Self;
    WriteLog(Format('problem %d created', [FId]), Notification);
end;

destructor TFitSession.Destroy;
begin
    WriteLog(Format('problem %d destroyed', [FId]), Notification);
    FService.FitProxy := nil;
    FService.Free;
    DoneCriticalSection(FSettingsLock);
    DoneCriticalSection(FLock);
    inherited Destroy;
end;

procedure TFitSession.Lock;
begin
    EnterCriticalSection(FLock);
end;

procedure TFitSession.Unlock;
begin
    LeaveCriticalSection(FLock);
end;

function TFitSession.TryLock: boolean;
begin
    Result := TryEnterCriticalSection(FLock) <> 0;
end;

procedure TFitSession.PublishSettings(const AReply: string);
begin
    EnterCriticalSection(FSettingsLock);
    try
        FSettingsWhileBusy := AReply;
    finally
        LeaveCriticalSection(FSettingsLock);
    end;
end;

procedure TFitSession.WithdrawSettings;
begin
    PublishSettings('');
end;

function TFitSession.SettingsWhileBusy(out AReply: string): boolean;
begin
    EnterCriticalSection(FSettingsLock);
    try
        AReply := FSettingsWhileBusy;
    finally
        LeaveCriticalSection(FSettingsLock);
    end;
    Result := AReply <> '';
end;

procedure TFitSession.ResetProgress;
begin
    FCurMin := 0;
    FDone := False;
end;

procedure TFitSession.ShowCurMin(Min: double);
begin
    FCurMin := Min;
    //  Once per minimizer iteration: Trace, not Debug. A single three-second fit
    //  raises this well over a hundred times, which at the default tier would be
    //  most of the log and, because the log rotates, would push out the events
    //  that say what the user did. Kept in full at --log-level trace, where the
    //  convergence history is what you are actually looking for. See log.pas.
    //  Built only when it will be written: the Format would otherwise run on
    //  every improvement, whatever the tier.
    if LogEnabled(Trace) then
        WriteLog(Format('problem %d: R-factor %g', [FId, Min]), Trace);
end;

procedure TFitSession.ShowProfile;
begin
    //  Nothing to do server-side: the client fetches the profile when it wants it.
end;

procedure TFitSession.Done;
begin
    FDone := True;
    WriteLog(Format('problem %d: Done', [FId]), Notification);
end;

procedure TFitSession.ComputeCurveBoundsDone;
begin
    FDone := True;
    WriteLog(Format('problem %d: ComputeCurveBoundsDone', [FId]), Notification);
end;

procedure TFitSession.ComputeBackgroundPointsDone;
begin
    FDone := True;
    WriteLog(Format('problem %d: ComputeBackgroundPointsDone', [FId]), Notification);
end;

procedure TFitSession.ComputeCurvePositionsDone;
begin
    FDone := True;
    WriteLog(Format('problem %d: ComputeCurvePositionsDone', [FId]), Notification);
end;

{ TSessionRegistry }

constructor TSessionRegistry.Create;
begin
    inherited Create;
    FItems := TObjectList.Create(True);   //  owns the sessions
    FNextId := 1;
    InitCriticalSection(FLock);
end;

destructor TSessionRegistry.Destroy;
begin
    FItems.Free;
    DoneCriticalSection(FLock);
    inherited Destroy;
end;

function TSessionRegistry.CreateProblem: longint;
var
    S: TFitSession;
begin
    EnterCriticalSection(FLock);
    try
        Result := FNextId;
        Inc(FNextId);
        S := TFitSession.Create(Result);
        FItems.Add(S);
        WriteLog(Format('%d problem(s) open', [FItems.Count]), Notification);
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TSessionRegistry.Find(AId: longint): TFitSession;
var
    i: integer;
begin
    Result := nil;
    EnterCriticalSection(FLock);
    try
        for i := 0 to FItems.Count - 1 do
            if TFitSession(FItems[i]).Id = AId then
                Exit(TFitSession(FItems[i]));
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TSessionRegistry.Discard(AId: longint);
var
    i: integer;
    Session: TFitSession;
begin
    //  OUT OF THE REGISTRY FIRST, so no request that arrives from here on can
    //  find it - and not freed yet.
    Session := nil;
    EnterCriticalSection(FLock);
    try
        for i := 0 to FItems.Count - 1 do
            if TFitSession(FItems[i]).Id = AId then
            begin
                Session := TFitSession(FItems.Extract(FItems[i]));
                Break;
            end;
    finally
        LeaveCriticalSection(FLock);
    end;
    if not Assigned(Session) then
        Exit;

    //  NOT UNDER ITS OWN FIT. This route is answered without the problem's lock
    //  - freeing the problem frees the lock, so it cannot be held - and a
    //  release that arrived during a fit freed the model the fit was writing
    //  into: the fit died with an access violation and this answered with a
    //  thread error. The fit is stopped, as Stop stops it, and waited for: the
    //  request running it holds the lock until the fit has ended.
    if Session.Service.AsyncOper then
        try
            Session.Service.StopAsyncOper;
        except
            //  It ended between the question and the stop: nothing to stop.
            on E: Exception do
                WriteLog('releasing problem ' + IntToStr(AId) +
                    ': ' + E.Message, Debug);
        end;
    Session.Lock;
    Session.Unlock;
    Session.Free;
end;

function TSessionRegistry.Count: longint;
begin
    EnterCriticalSection(FLock);
    try
        Result := FItems.Count;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

end.
