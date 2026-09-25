// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which axis mode is in force for a coordinate, and why.)

THE AUTOMATIC RULE, for each coordinate on its own, asks in this order and takes
the first answer that names a mode this build has:

  1. THE MODEL. What the curves the model holds prefer - when every curve that
     states a preference states the same one. Curves that disagree say nothing
     between them, and the rule goes on.
  2. THE DATA. What the loaded data says its coordinates are: the format it was
     read as, or what its reader chose (a price series is bars or dates
     against price).
  2a. THE MODEL'S FALLBACK. What the curves assume when the data says nothing -
     a wave pattern's argument is a bar number - asked AFTER the data, so a
     series read by date is still captioned Date.
  3. THE SELECTED CURVE TYPE, ONLY WHILE THE MODEL IS EMPTY. The Tools list
     says what the user will place NEXT, not what is placed. Asking it over a
     model that holds curves is what captioned a model of wave patterns in
     2*Theta, because the list still had a diffraction peak selected.
  4. The general name: Position, or Value.

Asking the model first is the user's decision, recorded here so it is not
reopened: a curve type knows what its coordinate means better than a file
format does whenever the two disagree. Per coordinate, because a curve type often
knows one and not the other - a wave pattern is drawn on a price, but whether
its argument is a bar or a date is the data's to say.

A CHOICE THE USER MADE outranks all of it, and is kept between sessions - but
only while it can still be honoured (UsableModeId).
}
unit axis_choice;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, coordinate_axis, axis_mode_registry;

const
    { The menu entry that hands the choice back to the rule above. Not a
      registered mode: it is how one is chosen. }
    AutomaticAxisModeId = 'automatic';
    { The former display-mode integer that meant "the axis the selected curve
      type defines" - what a settings file said when nothing had been chosen,
      and what the automatic entry replaces. }
    FromCurveTypeViewMode = 5;

type
    { What the automatic rule asks, for one coordinate. }
    TAxisPreferences = record
        { One entry per curve the model holds: the mode its type prefers, or
          '' when it states none. Empty when the model is empty. }
        Model: TStringArray;
        { One entry per curve the model holds: the mode its type assumes when
          the data says nothing (TNamedPointsSet.FallbackAxisMode), or ''. }
        ModelFallback: TStringArray;
        { What the loaded data says the coordinate is, or ''. }
        Data: string;
        { What the curve type selected in the Tools list prefers, or ''. }
        SelectedType: string;
    end;

{ The mode the automatic rule resolves to for ADimension. Always a registered
  mode that shows ADimension. }
function AutomaticModeId(ADimension: TAxisDimension;
    const APreferences: TAxisPreferences): string;

{ The mode a REMEMBERED choice resolves to once what it depends on is known.

  A remembered mode is a wish, not a fact. One the user never chose is whatever
  the last session left behind; one no longer registered belongs to a module
  this build does not contain; one for the other coordinate is a corrupt file;
  and one that cannot draw without a parameter - sin(theta)/lambda with no
  wavelength, a custom axis without both formulas - would block start-up on a
  dialog before the window is even up. Each resolves to the automatic entry,
  which can always be drawn. }
function UsableModeId(ADimension: TAxisDimension; const AStoredId: string;
    AChosenByUser: boolean; AWaveLength: double;
    const ADefinition: TAxisDefinition): string;

{ Whether AModeId can be drawn with what is known: its required parameter, if it
  has one, is there. }
function ModeCanBeDrawn(const AModeId: string; AWaveLength: double;
    const ADefinition: TAxisDefinition): boolean;

{ The mode id a setting or project written before the registry means by its
  display-mode integer (the former XCM_* constants). An integer that meant
  nothing then means the automatic entry now. }
function LegacyArgumentModeId(AViewMode: longint): string;

{ The mode a settings file or project REMEMBERS: its id when it has one, and
  otherwise - a file written before ids - what its display-mode integer meant,
  when it has one of those. Nothing at all is the automatic entry. }
function StoredModeId(const AModeId: string; AHasLegacyViewMode: boolean;
    ALegacyViewMode: longint): string;

{ AModeId, with the automatic entry resolved. }
function ResolvedModeId(ADimension: TAxisDimension; const AModeId: string;
    const APreferences: TAxisPreferences): string;

{ Everything a mode builds its axis from: the parameters as given, and the name
  of the quantity the coordinate is, from what the automatic rule resolves to. }
function AxisContextFor(ADimension: TAxisDimension;
    const APreferences: TAxisPreferences; AWaveLength: double;
    const ADefinition: TAxisDefinition;
    const ADates: TAxisDates = nil): TAxisContext;

{ The axis AModeId shows ADimension on. The caller owns the result. Raises when
  the mode is not registered - resolution only ever yields registered modes, so
  that is a build that forgot to register its own, not a user's mistake. }
function CreateAxisFor(ADimension: TAxisDimension; const AModeId: string;
    const APreferences: TAxisPreferences; AWaveLength: double;
    const ADefinition: TAxisDefinition;
    const ADates: TAxisDates = nil): TCoordinateAxis;

implementation

uses
    axis_mode_registration, diffraction_axis_modes;

function DefaultModeId(ADimension: TAxisDimension): string;
begin
    case ADimension of
        adArgument: Result := PositionAxisModeId;
        adValue:    Result := ValueAxisModeId;
    end;
end;

{ The one mode every curve that states a preference agrees on, or ''. }
function ModelModeId(ADimension: TAxisDimension;
    const AModel: TStringArray): string;
var
    i: longint;
    Id: string;
begin
    Result := '';
    for i := 0 to High(AModel) do
    begin
        Id := Trim(AModel[i]);
        //  A curve from a module this build lacks, or naming a mode for the
        //  other coordinate, says nothing - it does not veto the others.
        if not AxisModeShows(Id, ADimension) then
            Continue;
        if Result = '' then
            Result := Id
        else if not SameText(Result, Id) then
            Exit('');
    end;
end;

function AutomaticModeId(ADimension: TAxisDimension;
    const APreferences: TAxisPreferences): string;
begin
    Result := ModelModeId(ADimension, APreferences.Model);
    if Result <> '' then
        Exit;
    if AxisModeShows(APreferences.Data, ADimension) then
        Exit(Trim(APreferences.Data));
    Result := ModelModeId(ADimension, APreferences.ModelFallback);
    if Result <> '' then
        Exit;
    if (Length(APreferences.Model) = 0) and
        AxisModeShows(APreferences.SelectedType, ADimension) then
        Exit(Trim(APreferences.SelectedType));
    Result := DefaultModeId(ADimension);
end;

function ModeCanBeDrawn(const AModeId: string; AWaveLength: double;
    const ADefinition: TAxisDefinition): boolean;
var
    Mode: TAxisModeClass;
    Info: TAxisModeInfo;
begin
    Mode := FindAxisMode(AModeId);
    if not Assigned(Mode) then
        Exit(False);
    Info := Mode.Info;
    Result := True;
    if not Info.ParameterRequired then
        Exit;
    case Info.Parameter of
        apWaveLength:
            Result := AWaveLength <> 0;
        //  BOTH formulas, not either: the axis converts in each direction, and
        //  one alone is a value that can be shown and not read back, or read
        //  back and not shown.
        apDefinition:
            Result := (Trim(ADefinition.Forward) <> '') and
                (Trim(ADefinition.Inverse) <> '');
    end;
end;

function UsableModeId(ADimension: TAxisDimension; const AStoredId: string;
    AChosenByUser: boolean; AWaveLength: double;
    const ADefinition: TAxisDefinition): string;
begin
    //  WHETHER THE SETTING COUNTS AT ALL comes first.
    Result := AutomaticAxisModeId;
    if not AChosenByUser then
        Exit;
    if SameText(Trim(AStoredId), AutomaticAxisModeId) then
        Exit;
    if not AxisModeShows(AStoredId, ADimension) then
        Exit;
    if not ModeCanBeDrawn(AStoredId, AWaveLength, ADefinition) then
        Exit;
    Result := FindAxisMode(AStoredId).Info.Id;
end;

function LegacyArgumentModeId(AViewMode: longint): string;
begin
    //  The former XCM_* values, which files written before the registry hold.
    case AViewMode of
        0: Result := TwoThetaAxisModeId;
        1: Result := ThetaAxisModeId;
        2: Result := SinThetaOverLambdaAxisModeId;
        3: Result := PositionAxisModeId;
        4: Result := CustomAxisModeId;
    else
        //  5 was "from the curve type", which the automatic entry replaces.
        Result := AutomaticAxisModeId;
    end;
end;

function StoredModeId(const AModeId: string; AHasLegacyViewMode: boolean;
    ALegacyViewMode: longint): string;
begin
    Result := Trim(AModeId);
    if Result <> '' then
        Exit;
    if AHasLegacyViewMode then
        Result := LegacyArgumentModeId(ALegacyViewMode)
    else
        Result := AutomaticAxisModeId;
end;

function ResolvedModeId(ADimension: TAxisDimension; const AModeId: string;
    const APreferences: TAxisPreferences): string;
begin
    if (Trim(AModeId) = '') or SameText(Trim(AModeId), AutomaticAxisModeId) then
        Result := AutomaticModeId(ADimension, APreferences)
    else
        Result := Trim(AModeId);
end;

{ The mode registered under AId, or a build fault in words: resolution only
  ever yields registered modes, so reaching an unregistered one is a build that
  did not register its own, not a user's mistake. One place for both callers,
  so neither keeps a branch of its own that nothing can reach. }
function RegisteredModeOf(ADimension: TAxisDimension;
    const AId: string): TAxisModeClass;
begin
    Result := FindAxisMode(AId);
    if not Assigned(Result) then
        raise EAxisModeRegistration.Create('No axis mode is registered under "' +
            AId + '", so the ' + DimensionWord(ADimension) + ' cannot be shown. ' +
            'RegisterAllAxisModes has not been called in this build.');
end;

function AxisContextFor(ADimension: TAxisDimension;
    const APreferences: TAxisPreferences; AWaveLength: double;
    const ADefinition: TAxisDefinition;
    const ADates: TAxisDates): TAxisContext;
var
    Quantity: TCoordinateAxis;
    Mode: TAxisModeClass;
begin
    Result.WaveLength := AWaveLength;
    Result.Definition := ADefinition;
    Result.ArgumentDates := ADates;
    Result.QuantityName := '';
    Result.QuantityUnit := '';
    //  The quantity is what the automatic mode names, built with no quantity
    //  of its own - which is what keeps this from asking itself.
    Mode := RegisteredModeOf(ADimension, AutomaticModeId(ADimension, APreferences));
    Quantity := Mode.CreateAxis(ADimension, Result);
    try
        Result.QuantityName := Quantity.DisplayName;
        Result.QuantityUnit := Quantity.UnitName;
    finally
        Quantity.Free;
    end;
end;

function CreateAxisFor(ADimension: TAxisDimension; const AModeId: string;
    const APreferences: TAxisPreferences; AWaveLength: double;
    const ADefinition: TAxisDefinition;
    const ADates: TAxisDates): TCoordinateAxis;
var
    Id: string;
    Mode: TAxisModeClass;
begin
    Id := ResolvedModeId(ADimension, AModeId, APreferences);
    Mode := RegisteredModeOf(ADimension, Id);
    Result := Mode.CreateAxis(ADimension,
        AxisContextFor(ADimension, APreferences, AWaveLength, ADefinition,
            ADates));
end;

end.
