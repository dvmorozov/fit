// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a build can ever offer, decided from what registered.)

HIDDEN WHEN THE BUILD CAN NEVER USE IT, GREYED WHEN THE MODEL DOES NOT YET.
action_state decides the second, on every change; this decides the first, once,
from the curve types and axis modes the build registered. A product that links
no background shape - one for a field whose models describe their own level -
has no use for Model > Background, and a greyed menu there would say "not now"
about something that will never be.

CAPABILITIES, NOT A LIST OF PRODUCTS (AGENTS.md, non-negotiable 5): each answer
is a property the types and modes declare - TNamedPointsSet.IsBackground, its
PlacedByPointSet, an axis mode's Parameter - so a module's own background shape
or wavelength-reading axis turns the commands on with no edit here.

WHICH WIDGETS a command drives is ui_commands' table; which DESIGNED CONTROLS
serve nothing else - a menu holding only such commands, the tab of the
background points - is ControlOffered's, by component name, the way the table
names its targets - so this unit names no widget type and is tested without a
window. (It answered for menus alone, as MenuOffered, until the Background
Points tab stayed on show in a build with no background: an empty table
beside the model's says there could be points to list.)
}
unit build_capabilities;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, action_state, int_curve_factory, axis_mode_registry;

type
    TBuildOffer = (
        { A background shape is registered: Model > Background, its points and
          its subtraction. }
        boBackground,
        { A type the framework places at curve positions is registered: Model >
          Curve Positions. A type a module places from its own point set (a
          markup) is not one. }
        boCurvePositions,
        { An axis mode reads a wavelength: Data > Argument Transformation > Set
          Rule Parameters. }
        boWaveLength,
        { A type that IS a formula someone writes is registered
          (IsDefinedByFormula): Model > Curve Type > User, New User Curve and
          the formulas saved under it. }
        boUserFormula
        );
    TBuildOffers = set of TBuildOffer;

    TAxisModeInfos = array of TAxisModeInfo;

{ What a build of ACurves and AModes offers. }
function BuildOffersOf(const ACurves: array of TCurveClass;
    const AModes: TAxisModeInfos): TBuildOffers;

{ What THIS build offers: every registered curve type and axis mode. Asked once
  the modules have registered. }
function RegisteredBuildOffers: TBuildOffers;

{ Whether a build offering AOffers has ACommand at all. Every command not
  named here is offered by every build. }
function CommandOffered(ACommand: TUiCommand; AOffers: TBuildOffers): boolean;

{ Whether the designed control called AControlName - a menu, or the tab of
  a table - serves anything a build offering AOffers can use. A submenu whose
  every entry is hidden must go too, or it opens on nothing. Every control not
  named here is offered. }
function ControlOffered(const AControlName: string; AOffers: TBuildOffers): boolean;

{ The designed controls ControlOffered answers for, by component name: what the
  window hides when the build cannot use them. }
function CapabilityControls: TStringArray;

implementation

uses
    named_points_set, curve_types_singleton, int_curve_type_iterator;

function BuildOffersOf(const ACurves: array of TCurveClass;
    const AModes: TAxisModeInfos): TBuildOffers;
var
    C: TCurveClass;
    i: longint;
begin
    Result := [];
    for C in ACurves do
    begin
        if C.IsDefinedByFormula then
            Include(Result, boUserFormula);
        if C.IsBackground then
            Include(Result, boBackground)
        else if C.PlacedByPointSet = '' then
            Include(Result, boCurvePositions);
    end;
    for i := 0 to High(AModes) do
        if AModes[i].Parameter = apWaveLength then
            Include(Result, boWaveLength);
end;

function RegisteredBuildOffers: TBuildOffers;
var
    Curves: array of TCurveClass;
    Modes: TAxisModeInfos;
    Registered: TAxisModeClasses;
    Iter: ICurveTypeIterator;
    i: longint;
begin
    Curves := nil;
    Iter := TCurveTypesSingleton.CreateCurveTypeIterator;
    Iter.FirstCurveType;
    while True do
    begin
        //  An empty registry has no current class, and says so by raising.
        try
            SetLength(Curves, Length(Curves) + 1);
            Curves[High(Curves)] := Iter.GetCurrentCurveClass;
        except
            on EListError do
            begin
                SetLength(Curves, Length(Curves) - 1);
                Break;
            end;
        end;
        if Iter.EndCurveType then
            Break;
        Iter.NextCurveType;
    end;
    Registered := RegisteredAxisModes;
    Modes := nil;
    SetLength(Modes, Length(Registered));
    for i := 0 to High(Registered) do
        Modes[i] := Registered[i].Info;
    Result := BuildOffersOf(Curves, Modes);
end;

function CommandOffered(ACommand: TUiCommand; AOffers: TBuildOffers): boolean;
begin
    case ACommand of
        ucSubtractBackground, ucSubtractBackgroundBySelectedPoints, ucBackground:
            Result := boBackground in AOffers;
        ucCurvePositions:
            Result := boCurvePositions in AOffers;
    else
        Result := True;
    end;
end;

function CapabilityControls: TStringArray;
begin
    Result := nil;
    SetLength(Result, 4);
    Result[0] := 'MenuBackground';
    Result[1] := 'MenuCurvePositions';
    Result[2] := 'MenuSetRuleParameters';
    Result[3] := 'TabSheetBackground';
end;

function ControlOffered(const AControlName: string; AOffers: TBuildOffers): boolean;
begin
    if SameText(AControlName, 'MenuBackground') or
        SameText(AControlName, 'TabSheetBackground') then
        Result := boBackground in AOffers
    else if SameText(AControlName, 'MenuCurvePositions') then
        Result := boCurvePositions in AOffers
    else if SameText(AControlName, 'MenuSetRuleParameters') then
        Result := boWaveLength in AOffers
    else
        Result := True;
end;

end.
