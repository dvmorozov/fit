// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where a number an axis mode reads (TAxisParameter) is kept for a
problem, as the client reaches it.)

THE WAVELENGTH LEFT THE FRAMEWORK'S CONTRACT (2026-10-17). It was a setting
of the compute service (GetWaveLength/SetWaveLength, the REST settings, the
project's settings) that fitting never read; only the sin(theta)/lambda axis
divides by it, and that axis is the scientific module's. It is now that
module's per-problem state, on the server and in its own part of the project
file, and the module registers here how the client reads and writes it - so
the client, the window and the project restore ask this, and name no module.

IT ALSO FIXED A ROUND TRIP (the user, 2026-10-17): the window's dialog set the
wavelength in the client alone, a save took the server's, and an open set the
server's but not the client's - so a wavelength never survived saving and
reopening a project. Written through here, it is the problem's.
}
unit axis_parameter_store;

{$mode objfpc}{$H+}

interface

uses
    axis_mode_registry, int_fit_service;

type
    IAxisParameterStore = interface
        ['{3A6E8D14-9B27-4C50-A1F3-6D2E7B94C058}']
        { The value the problem AService holds; 0 for none. }
        function Get(AService: IFitService): double;
        { Makes AValue the problem's. }
        procedure Put(AService: IFitService; AValue: double);
    end;

{ Registers where AParameter is kept. The last registration wins. }
procedure RegisterAxisParameterStore(AParameter: TAxisParameter;
    AStore: IAxisParameterStore);
{ Where AParameter is kept, or nil in a build that keeps it nowhere. }
function AxisParameterStore(AParameter: TAxisParameter): IAxisParameterStore;

implementation

var
    Stores: array[TAxisParameter] of IAxisParameterStore;

procedure RegisterAxisParameterStore(AParameter: TAxisParameter;
    AStore: IAxisParameterStore);
begin
    Stores[AParameter] := AStore;
end;

function AxisParameterStore(AParameter: TAxisParameter): IAxisParameterStore;
begin
    Result := Stores[AParameter];
end;

end.
