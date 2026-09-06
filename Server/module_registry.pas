// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which modules this build contains.)

The same shape as the curve-type, loader, engine, objective and action
registries, and the last of them: a module registers itself from its own front
door, and everything that needs to know what a build can do asks here.

WHY A RESOURCE LOOKUP LIVES HERE and not only on the module: the policy a
resource needs - start the sidecar first, allow a long reply - is applied by the
router and by the HTTP client, on both sides of the wire, before any module code
runs. Both ask this registry, so the two cannot disagree about a resource that
only one of them has to get right.
}
unit module_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_app_module;

type
    EModuleRegistration = class(Exception);

    TAppModuleArray = array of IAppModule;

{ Registers a module.

  IDEMPOTENT BY NAME: registering the same name twice registers it once and is
  not an error, because a module is registered by both the client and the compute
  server and a test may register it again. (This comment used to say a duplicate
  name RAISED, which the body has never done - testcase_module_registry now pins
  what actually happens.)

  RAISES on a nil module, on a module with no name, on a resource with no name,
  and on two DIFFERENT modules claiming one resource - that last would otherwise
  be resolved by registration order, leaving the loser reachable in name only. }
procedure RegisterAppModule(AModule: IAppModule);

function RegisteredModules: TAppModuleArray;
function ModuleCount: longint;
function FindModule(const AName: string; out AModule: IAppModule): boolean;

{ What a resource needs, and whether it exists at all. Answered from the
  declarations, so the router, the client and a test all get the same answer. }
function FindModuleResource(const AResource: string;
    out AInfo: TModuleResource): boolean;
{ Names of every resource this build answers, for an error that says what could
  have been asked instead. }
function KnownModuleResources: string;

implementation

var
    Modules: TAppModuleArray;

function ModuleCount: longint;
begin
    Result := Length(Modules);
end;

function RegisteredModules: TAppModuleArray;
begin
    Result := Modules;
end;

function FindModule(const AName: string; out AModule: IAppModule): boolean;
var
    i: longint;
begin
    Result := False;
    AModule := nil;
    for i := 0 to High(Modules) do
        if Modules[i].Name = AName then
        begin
            AModule := Modules[i];
            Exit(True);
        end;
end;

function FindModuleResource(const AResource: string;
    out AInfo: TModuleResource): boolean;
var
    i, j: longint;
    Res: TModuleResourceArray;
begin
    Result := False;
    AInfo := Default(TModuleResource);
    for i := 0 to High(Modules) do
    begin
        Res := Modules[i].Resources;
        for j := 0 to High(Res) do
            if Res[j].Name = AResource then
            begin
                AInfo := Res[j];
                Exit(True);
            end;
    end;
end;

function KnownModuleResources: string;
var
    i, j: longint;
    Res: TModuleResourceArray;
begin
    Result := '';
    for i := 0 to High(Modules) do
    begin
        Res := Modules[i].Resources;
        for j := 0 to High(Res) do
        begin
            if Result <> '' then
                Result := Result + ', ';
            Result := Result + Res[j].Name;
        end;
    end;
    if Result = '' then
        //  Said plainly. "unknown resource: x. This build offers: ." would read
        //  as a formatting bug rather than as the actual answer.
        Result := '(this build contains no modules)';
end;

procedure RegisterAppModule(AModule: IAppModule);
var
    Existing: IAppModule;
    Res: TModuleResourceArray;
    Info: TModuleResource;
    i: longint;
begin
    if not Assigned(AModule) then
        raise EModuleRegistration.Create('a nil module was registered');
    if AModule.Name = '' then
        raise EModuleRegistration.Create('a module was registered with no name');
    if FindModule(AModule.Name, Existing) then
        //  Idempotent by name: a module registered by both the client and the
        //  compute server, or twice by a test, is registered once.
        Exit;

    Res := AModule.Resources;
    for i := 0 to High(Res) do
    begin
        if Res[i].Name = '' then
            raise EModuleRegistration.CreateFmt(
                'module "%s" declared a resource with no name', [AModule.Name]);
        if FindModuleResource(Res[i].Name, Info) then
            raise EModuleRegistration.CreateFmt(
                'resource "%s" is claimed by more than one module', [Res[i].Name]);
    end;

    SetLength(Modules, Length(Modules) + 1);
    Modules[High(Modules)] := AModule;
end;

end.
