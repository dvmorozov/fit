// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How the client reads and writes the problem's wavelength: through
this module's resource on the server.)

Registered by the module's front door for the axis parameter the diffraction
angles read (axis_parameter_store, apWaveLength), so the client, the window
and the project restore reach the wavelength without naming this module. The
resource is the module's session's (scientific_app_module).
}
unit wavelength_store;

{$mode objfpc}{$H+}

interface

uses
    int_fit_service, axis_parameter_store;

type
    TWavelengthStore = class(TObject, IAxisParameterStore)
    public
        function Get(AService: IFitService): double;
        procedure Put(AService: IFitService; AValue: double);
    end;

{ The one instance the front door registers. }
function WavelengthStore: TWavelengthStore;

implementation

uses
    SysUtils, fpjson, jsonparser, scientific_app_module;

var
    Instance: TWavelengthStore;

function WavelengthStore: TWavelengthStore;
begin
    if not Assigned(Instance) then
        Instance := TWavelengthStore.Create;
    Result := Instance;
end;

function TWavelengthStore.Get(AService: IFitService): double;
var
    D: TJSONData;
begin
    Result := 0;
    D := GetJSON(AService.ModuleGet(WavelengthResource));
    try
        if D is TJSONObject then
            Result := TJSONObject(D).Get('waveLength', 0.0);
    finally
        D.Free;
    end;
end;

procedure TWavelengthStore.Put(AService: IFitService; AValue: double);
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        O.Add('waveLength', AValue);
        AService.ModulePost(WavelengthResource, O.AsJSON);
    finally
        O.Free;
    end;
end;

finalization
    Instance.Free;
end.
