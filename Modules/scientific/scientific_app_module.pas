// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's per-problem state on the server: the
wavelength the diffraction angles' axis divides by, and the background
fraction its peak search stops at.)

THE WAVELENGTH IS THIS MODULE'S since 2026-10-17, THE BACKGROUND FRACTION
since 2026-10-18 (problem_value_store has why): read and written as
scientific/wavelength and scientific/background-fraction, and kept in the
project as this module's own part through scientific/project-state, which the
framework collects from every module's session and hands back on open without
reading it. Every resource answers the one JSON object, with members
"waveLength" and "backFactor", and a post takes whichever members it carries.

THE FRACTION IS READ BY THIS MODULE'S PEAK SEARCH (peak_search), which the
engine calls with itself as the host: BackFactorFor finds the session the
module made for that host. 30 - a thirtieth of the highest point - where none
was made, the engine's default before it moved here.

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
    BackgroundFractionResource = 'scientific/background-fraction';
    ScientificProjectStateResource = 'scientific/project-state';
    { "Down to a thirtieth of the highest point". }
    DefaultBackFactor = 30;

{ The module, registered by its front door in both hosts. }
function ScientificModule: IAppModule;

{ The background fraction of the problem served by AHost - the object a
  session was made for - or DefaultBackFactor where none was. }
function BackFactorFor(AHost: TObject): double;

implementation

uses
    fpjson, jsonparser;

type
    TScientificSession = class(TObject, IModuleSession)
    private
        FWaveLength: double;
        FBackFactor: double;
        FHost: TObject;
        function WavelengthJson: string;
        procedure TakeWavelength(const APayload: string);
    public
        constructor Create(AHost: TObject);
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
    { Every session made, for BackFactorFor. Not freed, as the service frees
      none of the sessions it makes; a later session for the same host
      supersedes an earlier one. }
    Sessions: array of TScientificSession;

function BackFactorFor(AHost: TObject): double;
var
    i: longint;
begin
    for i := High(Sessions) downto 0 do
        if Sessions[i].FHost = AHost then
            Exit(Sessions[i].FBackFactor);
    Result := DefaultBackFactor;
end;

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
    SetLength(Result, 3);
    Result[0] := Default(TModuleResource);
    Result[0].Name := WavelengthResource;
    Result[1] := Default(TModuleResource);
    Result[1].Name := ScientificProjectStateResource;
    Result[2] := Default(TModuleResource);
    Result[2].Name := BackgroundFractionResource;
end;

function TScientificModule.CreateSession(AHost: TObject): IModuleSession;
var
    Session: TScientificSession;
begin
    Session := TScientificSession.Create(AHost);
    SetLength(Sessions, Length(Sessions) + 1);
    Sessions[High(Sessions)] := Session;
    Result := Session;
end;

{ TScientificSession }

constructor TScientificSession.Create(AHost: TObject);
begin
    inherited Create;
    FHost := AHost;
    FBackFactor := DefaultBackFactor;
end;

function TScientificSession.WavelengthJson: string;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add('waveLength', FWaveLength);
        O.Add('backFactor', FBackFactor);
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
        begin
            FWaveLength := TJSONObject(D).Get('waveLength', FWaveLength);
            FBackFactor := TJSONObject(D).Get('backFactor', FBackFactor);
        end;
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
        SameText(AResource, BackgroundFractionResource) or
        SameText(AResource, ScientificProjectStateResource);
    if Result then
        AJson := WavelengthJson;
end;

function TScientificSession.TryPost(const AResource, APayload: string;
    out AJson: string): boolean;
begin
    AJson := '';
    Result := SameText(AResource, WavelengthResource) or
        SameText(AResource, BackgroundFractionResource) or
        SameText(AResource, ScientificProjectStateResource);
    if Result then
    begin
        TakeWavelength(APayload);
        AJson := WavelengthJson;
    end;
end;

procedure TScientificSession.Reset;
begin
    //  The wavelength is the radiation's, not the data file's: kept. So is
    //  the background fraction, a choice of the user's, as the service's
    //  setting was.
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
