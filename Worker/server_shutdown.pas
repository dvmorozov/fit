// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which request stops the compute server, and from where it may come.)

POST /shutdown stops fit_server cleanly: an installer replacing the program, or
an update about to install, asks it rather than killing the process - killing
also takes a server the user started by hand, and skips the clean-up that stops
the Python sidecar. Only from this machine: the server listens on every
interface (FPC 3.2.2's server cannot bind one address), and a route that
stopped it for anyone would let anyone on the network do so.

The decision is here, where a test reaches it; fit_server only acts on it. So
are the two requests that go with it - the program asking its server to stop,
and the server waking its own accept loop - so that the loopback HTTP they need
lives in one unit the network-boundary test allows, not in two program files.

Copyright (C) Dmitry Morozov
}
unit server_shutdown;

{$mode objfpc}{$H+}

interface

const
    ShutdownPath = '/shutdown';

{ Whether AMethod AUri is the request to stop. }
function IsShutdownRequest(const AMethod, AUri: string): boolean;

{ Whether a request from ARemoteAddress may stop the server: this machine's
  own addresses only. }
function ShutdownAllowedFrom(const ARemoteAddress: string): boolean;

{ Asks the server at ABaseUrl (no trailing slash) to stop; True when it
  answered that it will. No server there is False, not an error: an update
  installs whether or not one was running. }
function AskServerToStop(const ABaseUrl: string): boolean;

{ One connection to the server on APort, on this machine, to wake an accept
  loop that is blocked waiting for one. Whatever happens is ignored. }
procedure WakeServer(APort: integer);

implementation

uses
    SysUtils, fphttpclient;

function IsShutdownRequest(const AMethod, AUri: string): boolean;
begin
    Result := (AMethod = 'POST') and (AUri = ShutdownPath);
end;

function ShutdownAllowedFrom(const ARemoteAddress: string): boolean;
begin
    Result := (ARemoteAddress = '127.0.0.1') or (ARemoteAddress = '::1') or
        (ARemoteAddress = '::ffff:127.0.0.1');
end;

function AskServerToStop(const ABaseUrl: string): boolean;
var
    C: TFPHTTPClient;
begin
    Result := False;
    C := TFPHTTPClient.Create(nil);
    try
        C.IOTimeout := 3000;
        try
            C.FormPost(ABaseUrl + ShutdownPath, '');
            Result := C.ResponseStatusCode = 200;
        except
            //  Not running, or too old to have the route: nothing to stop.
        end;
    finally
        C.Free;
    end;
end;

procedure WakeServer(APort: integer);
var
    C: TFPHTTPClient;
begin
    C := TFPHTTPClient.Create(nil);
    try
        C.IOTimeout := 2000;
        try
            C.Get('http://127.0.0.1:' + IntToStr(APort) + '/health');
        except
            //  Nothing to say: the connection only wakes the loop.
        end;
    finally
        C.Free;
    end;
end;

end.
