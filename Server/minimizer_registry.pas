// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which fitting engines this build offers, and what each one needs.)

WHAT THIS REPLACES. A minimizer was an integer constant, and four unrelated
decisions were made by comparing against it:

    fit_task.UsesFormulaBackend    kind = PYTHON_LM  -> needs a closed form
    fit_task.Optimization          kind = PYTHON_LM  -> which backend to build
    fit_service.CreateTask         kind = DHS        -> may curve scaling apply
    fit_rest_api                   kind = PYTHON_LM  -> start the sidecar first

Four files, none of which mentions the others, all of which have to be found and
edited to add a third engine - and any one of them forgotten is a defect that
appears only for that engine. The old comment on the constants said as much:
"engines append their own MIN_KIND_* value here", which is precisely the edit
that does not scale.

CAPABILITIES, NOT COMPARISONS (D18). An engine now declares what it needs -
a formula, the sidecar, whether it can be weighted, whether the native engine's
curve-scaling trick applies - and each of the four sites asks the question it
actually cares about. Adding an engine is one registration; nothing above it
changes.

THE KIND STAYS AN INTEGER, and stays stable: it is persisted in settings and
travels over REST. The registry is keyed by it rather than replacing it.
}
unit minimizer_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_fit_backend;

type
    EMinimizerRegistration = class(Exception);

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

    TMinimizerInfo = record
        { Persisted in settings and sent over REST, so it must never change for
          an engine that already shipped. }
        Kind: longint;
        { Menu caption and its tooltip. }
        Name: string;
        Description: string;
        { True when the engine fits by evaluating a curve's expression, so a
          curve type with no closed form cannot be fitted by it. Drives both the
          server's fallback and the client greying the choice out. }
        NeedsFormula: boolean;
        { True when a fit needs the Python sidecar running first. }
        NeedsPythonSidecar: boolean;
        { True when residual weighting is meaningful. The native engine always
          fits unweighted, so offering the choice under it would be a control
          that does nothing. }
        SupportsWeighting: boolean;
        { True when the native engine's curve-scaling trick - fit the shape, then
          scale the amplitude to the observed integral - may be applied. An
          out-of-process engine fits the amplitude itself, so scaling afterwards
          would scale an already-fitted amplitude. }
        SupportsCurveScaling: boolean;
        CreateBackend: TBackendFactory;
    end;

    TMinimizerInfoArray = array of TMinimizerInfo;

{ Registers an engine. Raises on a duplicate kind: two engines answering to one
  id would be resolved by registration order, and the loser would be selectable
  in the menu while a different engine actually ran. }
procedure RegisterMinimizer(const AInfo: TMinimizerInfo);

{ Everything registered, in registration order - which is the order the menu
  shows, so the default engine registers first. }
function RegisteredMinimizers: TMinimizerInfoArray;
function MinimizerCount: longint;

{ True when AKind names a registered engine; AInfo receives its declaration. }
function FindMinimizer(AKind: longint; out AInfo: TMinimizerInfo): boolean;
function IsKnownMinimizer(AKind: longint): boolean;

{ AKind if it names a registered engine, and the default one otherwise.

  WHAT A PERSISTED KIND MEANS ON THE NEXT START. A settings file can name an
  engine this build does not have - written by a newer build, or by one with a
  plug-in this one lacks - and the answer must be a fit rather than a refusal.
  The default is the FIRST REGISTERED engine, which is the always-available
  native one: registration order is the order the menu shows, and the default
  registers first.

  Asked rather than restated, because the restatement was wrong in a way that
  only a third engine would reveal: the window used to name the one alternative
  it knew about and fall back to the native engine for everything else, so a
  user who chose a third would silently be given the first on every start. }
function MinimizerKindOrDefault(AKind: longint): longint;

{ The four questions the engine actually asks. Each answers False - the safe,
  native-engine answer - for an unknown kind rather than raising: an id from a
  newer build's settings file must not stop this one from fitting, and the fit
  it then performs is the ordinary native one.

  MinimizerSupportsCurveScaling is the exception and answers TRUE for an unknown
  kind, because the unknown kind falls back to the native engine, which is
  exactly the engine scaling belongs to. }
function MinimizerNeedsFormula(AKind: longint): boolean;
function MinimizerNeedsPythonSidecar(AKind: longint): boolean;
function MinimizerSupportsWeighting(AKind: longint): boolean;
function MinimizerSupportsCurveScaling(AKind: longint): boolean;

{ The engine to fall back to: the first registered, which is why the default
  registers first. Falls back to MIN_KIND_DHS's value when nothing is registered,
  so a caller always has something to select. }
function DefaultMinimizerKind: longint;


implementation

var
    Registry: TMinimizerInfoArray;

function MinimizerCount: longint;
begin
    Result := Length(Registry);
end;

function RegisteredMinimizers: TMinimizerInfoArray;
begin
    Result := Registry;
end;

function FindMinimizer(AKind: longint; out AInfo: TMinimizerInfo): boolean;
var
    i: longint;
begin
    Result := False;
    AInfo := Default(TMinimizerInfo);
    for i := 0 to High(Registry) do
        if Registry[i].Kind = AKind then
        begin
            AInfo := Registry[i];
            Exit(True);
        end;
end;

function IsKnownMinimizer(AKind: longint): boolean;
var
    Info: TMinimizerInfo;
begin
    Result := FindMinimizer(AKind, Info);
end;

function MinimizerKindOrDefault(AKind: longint): longint;
var
    All: TMinimizerInfoArray;
begin
    if IsKnownMinimizer(AKind) then
        Exit(AKind);
    All := RegisteredMinimizers;
    //  Nothing registered at all is not a state the application reaches -
    //  minimizer_registration runs on linkage - but answering AKind rather
    //  than an invented id keeps the caller's value visible in the log
    //  instead of replacing it with something no engine claims either.
    if Length(All) = 0 then
        Exit(AKind);
    Result := All[0].Kind;
end;

procedure RegisterMinimizer(const AInfo: TMinimizerInfo);
var
    Existing: TMinimizerInfo;
begin
    if AInfo.Name = '' then
        raise EMinimizerRegistration.Create(
            'a minimizer was registered with no name, so nothing could offer it');
    if FindMinimizer(AInfo.Kind, Existing) then
        raise EMinimizerRegistration.Create(Format(
            'minimizer kind %d is claimed by both "%s" and "%s"',
            [AInfo.Kind, Existing.Name, AInfo.Name]));
    if not Assigned(AInfo.CreateBackend) then
        raise EMinimizerRegistration.Create(AInfo.Name +
            ' was registered without a way to build its backend');

    SetLength(Registry, Length(Registry) + 1);
    Registry[High(Registry)] := AInfo;
end;

function MinimizerNeedsFormula(AKind: longint): boolean;
var
    Info: TMinimizerInfo;
begin
    Result := FindMinimizer(AKind, Info) and Info.NeedsFormula;
end;

function MinimizerNeedsPythonSidecar(AKind: longint): boolean;
var
    Info: TMinimizerInfo;
begin
    Result := FindMinimizer(AKind, Info) and Info.NeedsPythonSidecar;
end;

function MinimizerSupportsWeighting(AKind: longint): boolean;
var
    Info: TMinimizerInfo;
begin
    Result := FindMinimizer(AKind, Info) and Info.SupportsWeighting;
end;

function MinimizerSupportsCurveScaling(AKind: longint): boolean;
var
    Info: TMinimizerInfo;
begin
    if not FindMinimizer(AKind, Info) then
        //  An unknown kind is fitted natively, and scaling is the native
        //  engine's own trick - so the answer that matches what will actually
        //  run is True.
        Exit(True);
    Result := Info.SupportsCurveScaling;
end;

function DefaultMinimizerKind: longint;
begin
    if Length(Registry) > 0 then
        Result := Registry[0].Kind
    else
        //  Nothing registered is a broken build rather than a configuration, but
        //  returning an id keeps the caller simple; the fit itself then takes
        //  the native fallback in fit_task.
        Result := 0;
end;

end.
