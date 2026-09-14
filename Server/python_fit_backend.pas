// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(IFitBackend that runs the fit on the Python (lmfit) sidecar.)

Python is integrated the same way as every other compute backend: as an
IFitBackend (decision D3/D7). It marshals the task to the shared fit-problem
contract, sends it to the sidecar fit_server owns, and writes the fitted
parameters back into the task - exactly like TServerFitBackend does for a remote
compute server. The engine selects it by the minimizer kind (MIN_KIND_PYTHON_LM);
nothing about the fit path is special-cased.

The sidecar's URL is supplied by fit_server (the single integration point), which
starts and stops the worker; this adapter only speaks to it.
}
unit python_fit_backend;

{$MODE Delphi}

interface

uses
    SysUtils, Classes, int_fit_backend, fit_task;

const
    { Connecting to a loopback sidecar is instant or not happening. }
    CONNECT_TIMEOUT_MS = 5000;
    { Upper bound on one fit. The sidecar caps its own evaluation budget, so a
      reply that takes longer than this means it is wedged rather than working. }
    FIT_TIMEOUT_MS = 300000;   //  5 minutes

type
    { IFitBackend backed by the Python/lmfit sidecar at a given URL. }
    TPythonFitBackend = class(TInterfacedObject, IFitBackend)
    private
        FUrl: string;
    protected
        { THE TRANSPORT SEAM. Everything this class decides - how a task becomes
          a problem, what an unreadable reply means, how the sidecar's own
          rejection reaches the user - sits either side of one POST. With the
          request built inline there was no way to reach any of it without a
          running Python interpreter, and the unit measured zero covered lines.

          Overridden by a test double (see tests/mocks/mock_python_transport) to
          run the real marshalling against a canned reply. The timeouts are set
          here and not by the caller: never blocking forever is a property of this
          adapter, not of one request. }
        function Post(const AUrl, ABody: string): string; virtual;
    public
        constructor Create(const AUrl: string);
        function Name: string;
        function Fit(ATask: TFitTask): TFitResult;
    end;

implementation

uses
    fphttpclient, fpjson, jsonparser, fit_problem_json, fit_task_marshalling, log;

{ The sidecar's error text from its reply body, for a useful message. }
function ErrorMessageOf(const ABody: string): string;
var
    D: TJSONData;
begin
    Result := ABody;
    try
        D := GetJSON(ABody);
        try
            if D is TJSONObject then
                Result := TJSONObject(D).Get('error', ABody);
        finally
            D.Free;
        end;
    except
    end;
end;

constructor TPythonFitBackend.Create(const AUrl: string);
begin
    inherited Create;
    FUrl := AUrl;
    while (FUrl <> '') and (FUrl[Length(FUrl)] = '/') do
        SetLength(FUrl, Length(FUrl) - 1);
end;

function TPythonFitBackend.Name: string;
begin
    Result := 'Python (lmfit Trust Region)';
end;

function TPythonFitBackend.Post(const AUrl, ABody: string): string;
var
    C: TFPHTTPClient;
    Req: TStringStream;
begin
    C := TFPHTTPClient.Create(nil);
    Req := TStringStream.Create(ABody);
    try
        //  Never block forever: the sidecar bounds its own solver effort, so a
        //  reply that never comes means it is wedged. Failing with a message
        //  beats an application that looks hung.
        C.ConnectTimeout := CONNECT_TIMEOUT_MS;
        C.IOTimeout := FIT_TIMEOUT_MS;
        C.RequestBody := Req;
        C.AddHeader('Content-Type', 'application/json');
        Result := C.Post(AUrl);
    finally
        Req.Free;
        C.Free;
    end;
end;

function TPythonFitBackend.Fit(ATask: TFitTask): TFitResult;
var
    Body: string;
    Problem: TFitProblem;
    Outcome: TFitOutcome;
begin
    Problem := BuildProblemFromTask(ATask);
    WriteLog(Format('Python fit: %d curve(s), weighting=%s, POST %s',
        [Length(Problem.Curves), Problem.Weighting, FUrl]), Notification);

    Body := Post(FUrl + '/fit', FitProblemToJson(Problem));

    if not FitOutcomeFromJson(Body, Outcome) then
        raise Exception.Create('The Python backend returned an unreadable result.');
    if Outcome.ErrorCode <> 0 then
    begin
        //  Record the reason server-side too (the sidecar log has it, but the
        //  server log is where a support request looks first), then surface it.
        WriteLog('Python fit rejected: ' + ErrorMessageOf(Body), Warning);
        raise Exception.CreateFmt('The Python backend failed the fit: %s',
            [ErrorMessageOf(Body)]);
    end;

    //  Write the fitted parameters (and their uncertainties, once the wire
    //  contract carries them) back into the live task, so the same downstream
    //  path builds the curves, plot and statistics as for any other backend.
    ApplyOutcomeToTask(ATask, Outcome);

    Result.ErrorCode := Outcome.ErrorCode;
    Result.RFactor   := Outcome.RFactor;
    WriteLog(Format('Python fit: done, R-factor %g', [Outcome.RFactor]),
        Notification);
end;

end.
