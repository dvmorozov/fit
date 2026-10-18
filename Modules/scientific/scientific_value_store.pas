// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How the client reads and writes the numbers this module keeps for a
problem: the wavelength and the background fraction.)

Each is a member of a JSON object one of this module's resources answers
(scientific_app_module), registered by name in the framework's
problem_value_store by the module's front door. One class for both, as one
mechanism: a resource and the member it carries.
}
unit scientific_value_store;

{$mode objfpc}{$H+}

interface

uses
    int_fit_service, problem_value_store;

type
    TScientificValueStore = class(TObject, IProblemValueStore)
    private
        FResource, FMember: string;
    public
        constructor Create(const AResource, AMember: string);
        function Get(AService: IFitService): double;
        procedure Put(AService: IFitService; AValue: double);
    end;

{ The two instances the front door registers. }
function WavelengthStore: TScientificValueStore;
function BackgroundFractionStore: TScientificValueStore;

implementation

uses
    SysUtils, fpjson, jsonparser, scientific_app_module;

var
    Wavelength, BackgroundFraction: TScientificValueStore;

function WavelengthStore: TScientificValueStore;
begin
    if not Assigned(Wavelength) then
        Wavelength := TScientificValueStore.Create(WavelengthResource,
            'waveLength');
    Result := Wavelength;
end;

function BackgroundFractionStore: TScientificValueStore;
begin
    if not Assigned(BackgroundFraction) then
        BackgroundFraction := TScientificValueStore.Create(
            BackgroundFractionResource, 'backFactor');
    Result := BackgroundFraction;
end;

constructor TScientificValueStore.Create(const AResource, AMember: string);
begin
    inherited Create;
    FResource := AResource;
    FMember := AMember;
end;

function TScientificValueStore.Get(AService: IFitService): double;
var
    D: TJSONData;
begin
    Result := 0;
    D := GetJSON(AService.ModuleGet(FResource));
    try
        if D is TJSONObject then
            Result := TJSONObject(D).Get(FMember, 0.0);
    finally
        D.Free;
    end;
end;

procedure TScientificValueStore.Put(AService: IFitService; AValue: double);
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add(FMember, AValue);
        AService.ModulePost(FResource, O.AsJSON);
    finally
        O.Free;
    end;
end;

finalization
    Wavelength.Free;
    BackgroundFraction.Free;
end.
