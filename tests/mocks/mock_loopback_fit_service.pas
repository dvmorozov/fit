// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The client's own transport, answered by the server's own router, in
process.)

THE WIRE CONTRACT THE APPLICATION ACTUALLY USES, with no socket: the requests
are the ones THttpFitService writes, and they are routed by the TFitRestApi
fit_server runs. A test that drives TFitClient through this enters where the
user's gesture enters and reaches the engine through the real REST surface -
which is what "one test through the surface the user reaches" asks for, at unit
speed.

MOVED HERE from testcase_model_clearing_engine, where it was born, because a
second suite (testcase_background_model) needs the same thing; a second copy
would be the kind of parallel helper that drifts.
}
unit mock_loopback_fit_service;

{$mode objfpc}{$H+}

interface

uses
    http_fit_service, fit_rest_api;

type
    TLoopbackFitService = class(THttpFitService)
    private
        FApi: TFitRestApi;
        function PathOf(const AUrl: string): string;
    protected
        //  LONGINT, not integer: http_fit_service is a Delphi-mode unit, where
        //  integer is 32 bits, and in this objfpc unit integer is 16.
        function Fetch(const AUrl: string; ATimeoutMs: longint): string; override;
        function Send(const AMethod, AUrl, ABody: string;
            ATimeoutMs: longint): string; override;
    public
        constructor Create(AApi: TFitRestApi);
    end;

implementation

constructor TLoopbackFitService.Create(AApi: TFitRestApi);
begin
    inherited Create('http://loopback');
    FApi := AApi;
end;

function TLoopbackFitService.PathOf(const AUrl: string): string;
begin
    Result := Copy(AUrl, Length('http://loopback') + 1, MaxInt);
end;

function TLoopbackFitService.Fetch(const AUrl: string;
    ATimeoutMs: longint): string;
var
    Code: longint;
begin
    FApi.Handle('GET', PathOf(AUrl), '', Code, Result);
end;

function TLoopbackFitService.Send(const AMethod, AUrl, ABody: string;
    ATimeoutMs: longint): string;
var
    Code: longint;
begin
    FApi.Handle(AMethod, PathOf(AUrl), ABody, Code, Result);
end;

end.
