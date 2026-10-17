// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's per-problem state on the server: the
wavelength the diffraction angles' axis divides by.)

THE WAVELENGTH IS THIS MODULE'S since 2026-10-17 (axis_parameter_store has
why): read and written as scientific/wavelength, and kept in the project as
this module's own part through scientific/project-state, which the framework
collects from every module's session and hands back on open without reading
it. The JSON is one number, under the key "waveLength".

THE SESSION HOLDS A NUMBER AND NOTHING ELSE, and reaches no engine code, so
the module makes it itself - no server-only factory, as a session that builds
a model needs - and every other question a session is asked it answers as a
module with no markup does: nothing to add, nothing to object to.

A NEW PROFILE KEEPS IT (Reset): the wavelength belongs to the radiation, not
to one data file, as the service's setting did.
}
unit scientific_app_module;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, int_app_module;

const
    ScientificModuleName = 'scientific';
    WavelengthResource = 'scientific/wavelength';
    ScientificProjectStateResource = 'scientific/project-state';

{ The module, registered by its front door in both hosts. }
function ScientificModule: IAppModule;

implementation

uses
    fpjson, jsonparser;

type
    TScientificSession = class(TObject, IModuleSession)
    private
        FWaveLength: double;
        function WavelengthJson: string;
        procedure TakeWavelength(const APayload: string);
    public
        function Kind: string;
        function PointSink: IModulePointSink;
        function TryGet(const AResource: string; out AJson: string): boolean;
        function TryPost(const AResource, APayload: string;
            out AJson: string): boolean;
        procedure Reset;
        function ContributesFitReadiness: boolean;
        procedure CheckFitAllowed;
        procedure CheckIntervalAllowed(ALoX, AHiX, AHiLimitX: double);
        function SliceForInterval(ALoX, AHiX, AHiLimitX: double): IModuleTaskState;
        function ProposeFitIntervals(const ASampleX: array of double;
            out ABounds: TIntervalBoundsX): boolean;
        function SearchFitIntervals(const ASampleX, ASampleY: array of double;
            out ABounds: TIntervalBoundsX; out ANote: string): boolean;
        function PlaceInstancesFromData(const ASampleX, ASampleY: array of double;
            const AIntervals: TIntervalBoundsX; out ANote: string): boolean;
        function TryRemoveInstance(const AInstanceId: string;
            out ARemoved: TInstanceHandles): boolean;
    end;

    TScientificModule = class(TObject, IAppModule)
    public
        function Name: string;
        function Resources: TModuleResourceArray;
        function CreateSession(AHost: TObject): IModuleSession;
    end;

var
    ModuleInstance: TScientificModule;

function ScientificModule: IAppModule;
begin
    if not Assigned(ModuleInstance) then
        ModuleInstance := TScientificModule.Create;
    Result := ModuleInstance;
end;

{ TScientificModule }

function TScientificModule.Name: string;
begin
    Result := ScientificModuleName;
end;

function TScientificModule.Resources: TModuleResourceArray;
begin
    Result := nil;
    SetLength(Result, 2);
    Result[0] := Default(TModuleResource);
    Result[0].Name := WavelengthResource;
    Result[1] := Default(TModuleResource);
    Result[1].Name := ScientificProjectStateResource;
end;

function TScientificModule.CreateSession(AHost: TObject): IModuleSession;
begin
    Result := TScientificSession.Create;
end;

{ TScientificSession }

function TScientificSession.WavelengthJson: string;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add('waveLength', FWaveLength);
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

procedure TScientificSession.TakeWavelength(const APayload: string);
var
    D: TJSONData;
begin
    D := GetJSON(APayload);
    try
        if D is TJSONObject then
            FWaveLength := TJSONObject(D).Get('waveLength', FWaveLength);
    finally
        D.Free;
    end;
end;

function TScientificSession.Kind: string;
begin
    Result := ScientificModuleName;
end;

function TScientificSession.PointSink: IModulePointSink;
begin
    Result := nil;
end;

function TScientificSession.TryGet(const AResource: string;
    out AJson: string): boolean;
begin
    AJson := '';
    Result := SameText(AResource, WavelengthResource) or
        SameText(AResource, ScientificProjectStateResource);
    if Result then
        AJson := WavelengthJson;
end;

function TScientificSession.TryPost(const AResource, APayload: string;
    out AJson: string): boolean;
begin
    AJson := '';
    Result := SameText(AResource, WavelengthResource) or
        SameText(AResource, ScientificProjectStateResource);
    if Result then
    begin
        TakeWavelength(APayload);
        AJson := WavelengthJson;
    end;
end;

procedure TScientificSession.Reset;
begin
    //  The wavelength is the radiation's, not the data file's: kept.
end;

function TScientificSession.ContributesFitReadiness: boolean;
begin
    Result := False;
end;

procedure TScientificSession.CheckFitAllowed;
begin
end;

procedure TScientificSession.CheckIntervalAllowed(ALoX, AHiX, AHiLimitX: double);
begin
end;

function TScientificSession.SliceForInterval(ALoX, AHiX,
    AHiLimitX: double): IModuleTaskState;
begin
    Result := nil;
end;

function TScientificSession.ProposeFitIntervals(const ASampleX: array of double;
    out ABounds: TIntervalBoundsX): boolean;
begin
    ABounds := nil;
    Result := False;
end;

function TScientificSession.SearchFitIntervals(const ASampleX,
    ASampleY: array of double; out ABounds: TIntervalBoundsX;
    out ANote: string): boolean;
begin
    ABounds := nil;
    ANote := '';
    Result := False;
end;

function TScientificSession.PlaceInstancesFromData(const ASampleX,
    ASampleY: array of double; const AIntervals: TIntervalBoundsX;
    out ANote: string): boolean;
begin
    ANote := '';
    Result := False;
end;

function TScientificSession.TryRemoveInstance(const AInstanceId: string;
    out ARemoved: TInstanceHandles): boolean;
begin
    ARemoved := nil;
    Result := False;
end;

finalization
    ModuleInstance.Free;
end.
