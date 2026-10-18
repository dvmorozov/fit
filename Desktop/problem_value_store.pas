// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where a number a module keeps for a problem is reached from the
client, by name.)

A MODULE'S PER-PROBLEM NUMBERS that the framework's window and project restore
still set: the wavelength the diffraction axis divides by, and the background
fraction the peak and background searches stop at. Each was a setting of the
compute service (the REST settings, the project's settings) that the engine
itself never needed; each is now the module's state, on the server and in its
own part of the project file, and the module registers here how the client
reads and writes it - so the client, the window and the restore ask this by
name and name no module.

ONE MECHANISM, KEYED BY NAME. It was keyed by an axis mode's parameter
(axis_parameter_store, 2026-10-17); the background fraction is no axis's, and
a second registry of the same shape for it would have been a second way to do
one thing (2026-10-18).

IT ALSO FIXED A ROUND TRIP (the user, 2026-10-17): the window's dialog set the
wavelength in the client alone, a save took the server's, and an open set the
server's but not the client's. Written through here, a value is the problem's.
}
unit problem_value_store;

{$mode objfpc}{$H+}

interface

uses
    int_fit_service;

const
    { The wavelength an axis divides by (axis_mode_registry.apWaveLength). }
    WavelengthValue = 'wavelength';
    { The fraction of the highest point the peak and background searches stop
      at, as its reciprocal: 30 is "down to a thirtieth". }
    BackgroundFractionValue = 'background-fraction';

type
    IProblemValueStore = interface
        ['{3A6E8D14-9B27-4C50-A1F3-6D2E7B94C058}']
        { The value the problem AService holds; 0 for none. }
        function Get(AService: IFitService): double;
        { Makes AValue the problem's. }
        procedure Put(AService: IFitService; AValue: double);
    end;

{ Registers where the value named AName is kept. The last registration wins. }
procedure RegisterProblemValueStore(const AName: string;
    AStore: IProblemValueStore);
{ Where the value named AName is kept, or nil in a build that keeps it
  nowhere. }
function ProblemValueStore(const AName: string): IProblemValueStore;

implementation

uses
    SysUtils;

var
    Names: array of string;
    Stores: array of IProblemValueStore;

procedure RegisterProblemValueStore(const AName: string;
    AStore: IProblemValueStore);
var
    i: longint;
begin
    for i := 0 to High(Names) do
        if SameText(Names[i], AName) then
        begin
            Stores[i] := AStore;
            Exit;
        end;
    SetLength(Names, Length(Names) + 1);
    SetLength(Stores, Length(Names));
    Names[High(Names)] := AName;
    Stores[High(Stores)] := AStore;
end;

function ProblemValueStore(const AName: string): IProblemValueStore;
var
    i: longint;
begin
    for i := 0 to High(Names) do
        if SameText(Names[i], AName) then
            Exit(Stores[i]);
    Result := nil;
end;

end.
