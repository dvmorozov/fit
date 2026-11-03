// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How each declared fitting engine is built - server side only.)

minimizer_registry says WHICH engines exist and what each needs; this unit says
how to BUILD one, keyed by the same kind. They are two units because the desktop
client needs the first to offer the engines and must not link the second: a
backend's contract takes a TFitTask, and the client contains no fitting engine
(non-negotiable 1). The factory used to be a field of the declaration, and the
Minimizer menu linked the whole engine into the client through it.
}
unit minimizer_backends;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_fit_backend, minimizer_registry;

type
    { What a backend needs to be built: the addresses of the out-of-process
      engines this build can reach. Empty means "not available", which is the
      ordinary case rather than an error - a desktop with no sidecar installed
      still fits, natively. }
    TBackendContext = record
        PythonUrl: string;
        ServerUrl: string;
    end;

    { Builds the backend for one engine, or returns nil when this context cannot
      support it (the sidecar is not running, say). Nil is not a failure: the
      caller falls back to the default engine, which is the behaviour a user
      wants and the reason the application still works with no Python at all. }
    TBackendFactory = function(const AContext: TBackendContext): IFitBackend;

{ Says how the DECLARED engine AKind is built. Refused for a kind nothing
  declared - a backend nothing can select - and for a nil factory. Binding again
  replaces, so registration stays idempotent. }
procedure BindMinimizerBackend(AKind: longint; AFactory: TBackendFactory);

{ The backend for AKind in AContext, or nil when none is bound or the factory
  answers that it cannot run here. The caller falls back to the default engine. }
function CreateMinimizerBackend(AKind: longint;
    const AContext: TBackendContext): IFitBackend;

{ Refuses, naming each, every declared engine with no backend bound. Run where a
  build finishes registering: such an engine IS offered, the user selects it,
  and the fit it starts would silently run a different one. }
procedure RequireEveryMinimizerBuildable;

{ The names, comma-separated, of the engines in AAll with no backend bound; empty
  when every one has. What RequireEveryMinimizerBuildable refuses, answered over
  a list the caller gives so it can be asked without registering an engine that
  no test could then take back out. }
function UnbuildableMinimizers(const AAll: TMinimizerInfoArray): string;

implementation

type
    TBinding = record
        Kind: longint;
        Factory: TBackendFactory;
    end;

var
    Bindings: array of TBinding;

function IndexOfBinding(AKind: longint): longint;
var
    i: longint;
begin
    for i := 0 to High(Bindings) do
        if Bindings[i].Kind = AKind then
            Exit(i);
    Result := -1;
end;

procedure BindMinimizerBackend(AKind: longint; AFactory: TBackendFactory);
var
    Info: TMinimizerInfo;
    i: longint;
begin
    if not FindMinimizer(AKind, Info) then
        raise EMinimizerRegistration.CreateFmt(
            'a backend was bound to minimizer kind %d, which nothing declared',
            [AKind]);
    if not Assigned(AFactory) then
        raise EMinimizerRegistration.Create(Info.Name +
            ' was given no way to build its backend');
    i := IndexOfBinding(AKind);
    if i < 0 then
    begin
        i := Length(Bindings);
        SetLength(Bindings, i + 1);
        Bindings[i].Kind := AKind;
    end;
    Bindings[i].Factory := AFactory;
end;

function CreateMinimizerBackend(AKind: longint;
    const AContext: TBackendContext): IFitBackend;
var
    i: longint;
begin
    Result := nil;
    i := IndexOfBinding(AKind);
    if i >= 0 then
        Result := Bindings[i].Factory(AContext);
end;

function UnbuildableMinimizers(const AAll: TMinimizerInfoArray): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AAll) do
        if IndexOfBinding(AAll[i].Kind) < 0 then
        begin
            if Result <> '' then
                Result := Result + ', ';
            Result := Result + AAll[i].Name;
        end;
end;

procedure RequireEveryMinimizerBuildable;
var
    Missing: string;
begin
    Missing := UnbuildableMinimizers(RegisteredMinimizers);
    if Missing <> '' then
        raise EMinimizerRegistration.Create(
            'declared with no way to build its backend: ' + Missing);
end;

end.
