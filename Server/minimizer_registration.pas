// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place that says which fitting engines this build ships, and
how each is built.)

Same shape as curve_type_registration and data_loader_registration, and for the
same reason: an engine is present in a binary only if something references its
unit, and nothing connected "the menu offers Python/lmfit" to "this binary can
build that backend".

SERVER SIDE ONLY. The declarations are minimizer_declarations, which the desktop
client uses for its menu; this unit adds the backends, and a backend is a fitting
engine the client must not contain (non-negotiable 1). The client used this unit
until tools/build-tests/client_without_engine.tests.ps1 said so.
}
unit minimizer_registration;

{$mode objfpc}{$H+}

interface

uses
    int_fit_backend, minimizer_backends;

{ Declares every engine this build ships and binds each to its backend.
  Idempotent, so a second call is harmless rather than a duplicate-kind
  failure. Called at start-up by the compute server and by every fit. }
procedure RegisterAllMinimizers;

{ The engine a fit uses when the SELECTED one cannot run here - no sidecar URL,
  say - or when nothing is selected at all. Honours a configured compute server,
  because "the default engine, wherever it is configured to run" is what the user
  asked for.

  Exported because the fallback must exist even in a binary where registration
  was somehow skipped: falling back is what keeps the application fitting, and it
  must not depend on the very list whose absence is the problem. }
function DefaultFitBackend(const AContext: TBackendContext): IFitBackend;

{ The native engine, in THIS process, whatever is configured.

  A different question from DefaultFitBackend, and the difference is load-bearing:
  this is what a fit falls back to when the model cannot be given to a
  formula-based backend at all - a curve type with no closed form. A remote
  compute server is such a backend (it marshals the same formula-bearing
  contract), so honouring a server URL here would send the model to precisely the
  thing the guard exists to avoid, and the failure would look like a connection
  error rather than a wrong decision. }
function NativeInProcessBackend: IFitBackend;

implementation

uses
    SysUtils, int_fit_service, minimizer_declarations, native_fit_backend,
    python_fit_backend, server_fit_backend;

{ The default: the native Downhill Simplex, in whichever process is configured to
  run it. A server URL means the same algorithm on another machine, which is a
  TRANSPORT choice rather than a different engine - so it belongs here, not in a
  separate registration. }
function CreateNativeBackend(const AContext: TBackendContext): IFitBackend;
begin
    if AContext.ServerUrl <> '' then
        Result := TServerFitBackend.Create(AContext.ServerUrl)
    else
        Result := TNativeFitBackend.Create;
end;

function DefaultFitBackend(const AContext: TBackendContext): IFitBackend;
begin
    Result := CreateNativeBackend(AContext);
end;

function NativeInProcessBackend: IFitBackend;
begin
    Result := TNativeFitBackend.Create;
end;

{ Python/lmfit through the sidecar. Returns nil when no sidecar URL is known, and
  the caller then falls back to the default engine: an application with no Python
  installed must still fit, which is the whole point of the native engine being
  the default (D4). }
function CreatePythonBackend(const AContext: TBackendContext): IFitBackend;
begin
    Result := nil;
    if AContext.PythonUrl <> '' then
        Result := TPythonFitBackend.Create(AContext.PythonUrl);
end;

procedure RegisterAllMinimizers;
begin
    DeclareAllMinimizers;
    BindMinimizerBackend(MIN_KIND_DHS, @CreateNativeBackend);
    BindMinimizerBackend(MIN_KIND_PYTHON_LM, @CreatePythonBackend);
    //  After binding, so an engine declared here and not bound above fails at
    //  start-up by name rather than being offered and run as something else.
    RequireEveryMinimizerBuildable;
end;

end.
