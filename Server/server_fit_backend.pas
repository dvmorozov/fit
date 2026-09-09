// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Compute backend that runs the fit on a remote/standalone fit_server.)

The client does NOT start or supervise the server: the server is an independent
process, started separately and possibly on another machine. This backend simply
POSTs the whole fit problem to a configured URL (default http://127.0.0.1:8787)
and writes the returned parameters back into the task.
}
unit server_fit_backend;

{$MODE Delphi}

interface

uses
    SysUtils, Classes, int_fit_backend, fit_task;

const
    DEFAULT_SERVER_URL = 'http://127.0.0.1:8787';

type
    { IFitBackend talking HTTP+JSON to a standalone compute server. }
    TServerFitBackend = class(TInterfacedObject, IFitBackend)
    private
        FUrl: string;
    protected
        { THE TRANSPORT SEAM, the same one http_fit_service and
          python_fit_backend have. Everything this class decides sits either side
          of these two calls - how a task becomes a problem, what an unreadable
          reply means, whether a server that answers is a server that works - and
          with the client built inline none of it was reachable without a running
          compute server. See tests/mocks/mock_server_transport. }
        function Get(const AUrl: string): string; virtual;
        function Post(const AUrl, ABody: string): string; virtual;
    public
        constructor Create(const AUrl: string);
        function Name: string;
        function Fit(ATask: TFitTask): TFitResult;
        { Returns True when the configured server answers /health. }
        function IsAvailable: boolean;
        property Url: string read FUrl;
    end;

implementation

uses
    fphttpclient, fit_problem_json, fit_task_marshalling;

constructor TServerFitBackend.Create(const AUrl: string);
begin
    inherited Create;
    FUrl := AUrl;
    if FUrl = '' then
        FUrl := DEFAULT_SERVER_URL;
    //  Tolerate a trailing slash in the configured URL.
    while (FUrl <> '') and (FUrl[Length(FUrl)] = '/') do
        SetLength(FUrl, Length(FUrl) - 1);
end;

function TServerFitBackend.Name: string;
begin
    Result := 'Compute server (' + FUrl + ')';
end;

function TServerFitBackend.Get(const AUrl: string): string;
var
    C: TFPHTTPClient;
begin
    C := TFPHTTPClient.Create(nil);
    try
        Result := C.Get(AUrl);
    finally
        C.Free;
    end;
end;

function TServerFitBackend.Post(const AUrl, ABody: string): string;
var
    C: TFPHTTPClient;
    Req: TStringStream;
begin
    C := TFPHTTPClient.Create(nil);
    Req := TStringStream.Create(ABody);
    try
        C.RequestBody := Req;
        C.AddHeader('Content-Type', 'application/json');
        Result := C.Post(AUrl);
    finally
        Req.Free;
        C.Free;
    end;
end;

function TServerFitBackend.IsAvailable: boolean;
begin
    Result := False;
    try
        Get(FUrl + '/health');
        Result := True;
    except
        Result := False;
    end;
end;

function TServerFitBackend.Fit(ATask: TFitTask): TFitResult;
var
    Body: string;
    Problem: TFitProblem;
    Outcome: TFitOutcome;
begin
    Problem := BuildProblemFromTask(ATask);

    Body := Post(FUrl + '/fit', FitProblemToJson(Problem));

    if not FitOutcomeFromJson(Body, Outcome) then
        raise Exception.Create('The compute server returned an unreadable result.');
    if Outcome.ErrorCode <> 0 then
        raise Exception.CreateFmt('The compute server failed the fit (code %d).',
            [Outcome.ErrorCode]);

    //  Write the fitted parameters back into the live task so the UI shows them.
    ApplyOutcomeToTask(ATask, Outcome);

    Result.ErrorCode := Outcome.ErrorCode;
    Result.RFactor   := Outcome.RFactor;
end;

end.
