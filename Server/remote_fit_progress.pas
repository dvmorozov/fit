// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A fit running in another process, reporting its progress back.)

ANY ENGINE, ANY MODEL. The native minimizer reports every improvement through
TFitTask.ShowCurMin, and that one call is where the service records a running
fit's progress (TFitService.ShowCurMin, fit_progress_log). A backend that fits
in another process - the Python sidecar, a compute server elsewhere - used to be
silent until its reply came back, so a fit on either left the chart empty for
its whole length.

This relays what the far side has reached into the SAME task, through the SAME
call, so nothing downstream knows or asks which engine ran:

  * the fit problem carries a progressId the far side publishes under;
  * the POST runs on a helper thread, because it blocks for the whole fit;
  * meanwhile THIS thread - the one that owns the task - asks
    GET <backend>/fit/progress?id=<progressId>, applies each reply with the
    same ApplyOutcomeToTask the final reply goes through, and calls
    ShowCurMin.

The task is only ever touched on this thread: the POST's body is built before
the helper starts, and the helper touches nothing but the socket.

AN OLDER PEER has no progress route and answers 404, which the HTTP client
raises. That is a peer without the feature, not a failed fit: asking stops, the
view says the engine reports no intermediate progress, and the fit's own reply
arrives as it always did. A reply that cannot be read stops asking too - a peer
that answers something else at this path will not start answering this.
}
unit remote_fit_progress;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, fit_task;

const
    { How often the far side is asked. The desktop reads ten times a second, so
      relaying faster than that would be work nobody sees. }
    REMOTE_PROGRESS_INTERVAL_MS = 250;
    FIT_PROGRESS_PATH = '/fit/progress';

type
    { The two HTTP calls, as the backend's own transport seams. }
    TRemotePost = function(const AUrl, ABody: string): string of object;
    TRemoteGet = function(const AUrl: string): string of object;

{ A fresh id for one fit's progress: unique, and nothing a query splits on. }
function NewProgressId: string;
{ Where a backend at ABaseUrl answers the progress of the fit AId. }
function ProgressUrlFor(const ABaseUrl, AId: string): string;
{ Applies one progress reply to ATask and tells the engine it improved. False
  when the reply cannot be read, which means stop asking; a reply with nothing in
  it yet means ask again. }
function RelayProgress(ATask: TFitTask; const AReply: string): boolean;
{ Runs APost on a helper thread and, until it returns, relays AGet of
  AProgressUrl into ATask every AIntervalMs on this thread. Returns the POST's
  reply, or raises with its failure's message. }
function PostRelayingProgress(ATask: TFitTask; APost: TRemotePost;
    AGet: TRemoteGet; const APostUrl, ABody, AProgressUrl: string;
    AIntervalMs: longint = REMOTE_PROGRESS_INTERVAL_MS): string;

implementation

uses
    fit_problem_json, fit_task_marshalling, log;

type
    { Makes the one blocking call, so the thread that owns the task can poll. }
    TRemotePostThread = class(TThread)
    private
        FPost: TRemotePost;
        FUrl, FBody: string;
    protected
        procedure Execute; override;
    public
        Reply: string;
        Failure: string;
        Failed: boolean;
        constructor Create(APost: TRemotePost; const AUrl, ABody: string);
    end;

constructor TRemotePostThread.Create(APost: TRemotePost;
    const AUrl, ABody: string);
begin
    FPost := APost;
    FUrl := AUrl;
    FBody := ABody;
    inherited Create(True);
    FreeOnTerminate := False;
end;

procedure TRemotePostThread.Execute;
begin
    //  CAUGHT HERE and handed back: an exception that escapes a thread's Execute
    //  is lost with the thread, and the caller would then read an empty reply as
    //  an unreadable one and report the wrong thing.
    try
        Reply := FPost(FUrl, FBody);
    except
        on E: Exception do
        begin
            Failed := True;
            Failure := E.Message;
        end;
    end;
end;

function NewProgressId: string;
var
    G: TGUID;
begin
    CreateGUID(G);
    //  Without the braces, which a URL would have to escape.
    Result := LowerCase(Copy(GUIDToString(G), 2, 36));
end;

function ProgressUrlFor(const ABaseUrl, AId: string): string;
begin
    Result := ABaseUrl + FIT_PROGRESS_PATH + '?id=' + AId;
end;

function RelayProgress(ATask: TFitTask; const AReply: string): boolean;
var
    O: TFitOutcome;
begin
    if not FitOutcomeFromJson(AReply, O) then
        Exit(False);
    Result := True;
    //  Nothing reached yet: the poll came before the first improvement.
    if Length(O.Curves) = 0 then
        Exit;
    ApplyOutcomeToTask(ATask, O);
    //  THE FUNNEL EVERY ENGINE REPORTS THROUGH. The task recomputes its own
    //  R-factor from the curves it now holds, so the number recorded is the one
    //  this program computes, whichever engine found the parameters.
    ATask.ShowCurMin;
end;

function PostRelayingProgress(ATask: TFitTask; APost: TRemotePost;
    AGet: TRemoteGet; const APostUrl, ABody, AProgressUrl: string;
    AIntervalMs: longint): string;
var
    Call: TRemotePostThread;
    Asking: boolean;
    Waited: longint;
begin
    Call := TRemotePostThread.Create(APost, APostUrl, ABody);
    try
        Call.Start;
        Asking := True;
        while not Call.Finished do
        begin
            //  In small steps, so a fit that returns between polls is not kept
            //  waiting for the rest of the interval.
            Waited := 0;
            while (not Call.Finished) and (Waited < AIntervalMs) do
            begin
                Sleep(5);
                Inc(Waited, 5);
            end;
            if Call.Finished or not Asking then
                Continue;
            try
                Asking := RelayProgress(ATask, AGet(AProgressUrl));
            except
                on E: Exception do
                begin
                    Asking := False;
                    WriteLog('the backend reports no intermediate progress, so ' +
                        'none is shown until it finishes: ' + E.Message,
                        Notification);
                end;
            end;
        end;
        Call.WaitFor;
        if Call.Failed then
            raise Exception.Create(Call.Failure);
        Result := Call.Reply;
    finally
        Call.Free;
    end;
end;

end.
