// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The two axes the chart is drawn on: what the user chose for each
coordinate, what the automatic rule would answer, and the axes that result.)

ONE OBJECT FOR BOTH COORDINATES. The argument's mode lived in the viewer, the
curve list and the window at once, each holding the integer and the custom
formulas and each building its own axis from them; the value had no mode. Now
the window owns one of these, the viewer draws through it, and the chart's
titles, the pointer's readout and the menu's ticks are all read off it - so
the four can no longer disagree about which axis is in force.

LOGIC HERE, NOT IN THE FORM. Everything the window decides about the axes is
answered by this class, headlessly; the form reads controls and forwards
(AGENTS.md, "logic does not live in UI classes").

THE AXES ARE BUILT ON DEMAND and rebuilt after any change, so a caller holds
the object, never an axis it handed out.
}
unit chart_axes;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, coordinate_axis, axis_mode_registry, axis_choice;

type
    { What was chosen for one coordinate. }
    TAxisChoice = record
        { A registered mode id, or the automatic entry. }
        ModeId: string;
        { Whether the user made the choice, which is what makes it worth
          remembering. }
        ChosenByUser: boolean;
        { The user's own axis for this coordinate, kept whether or not it is
          the one in force, so choosing it again reopens what was given. }
        Definition: TAxisDefinition;
    end;

    TChartAxes = class
    private
        FChoices: array[TAxisDimension] of TAxisChoice;
        FPreferences: array[TAxisDimension] of TAxisPreferences;
        FValues: TAxisValues;
        FArgumentDates: TAxisDates;
        FAxes: array[TAxisDimension] of TCoordinateAxis;
        procedure Invalidate;
    public
        constructor Create;
        destructor Destroy; override;

        { Both axes back on the automatic entry, chosen by nobody and with no
          definitions: what a new document starts on. }
        procedure Reset;
        { The user picked AModeId from a menu, the automatic entry included. }
        procedure Choose(ADimension: TAxisDimension; const AModeId: string);
        { A remembered choice, applied only as far as it can be honoured
          (axis_choice.UsableModeId). Its definition is kept either way. }
        procedure Restore(ADimension: TAxisDimension; const AStoredId: string;
            AChosenByUser: boolean; const ADefinition: TAxisDefinition);
        procedure SetDefinition(ADimension: TAxisDimension;
            const ADefinition: TAxisDefinition);
        { What the model, the data and the selection prefer for ADimension. }
        procedure SetPreferences(ADimension: TAxisDimension;
            const APreferences: TAxisPreferences);
        { The same, answering whether anything a user can see changed: the mode
          the automatic rule resolves to, or the title - a logarithmic axis
          keeps the quantity's name, which the data may have just changed. What
          lets the window redraw only when it has to. }
        function UpdatePreferences(ADimension: TAxisDimension;
            const APreferences: TAxisPreferences): boolean;
        { A number the problem holds that a mode may read (apValue), by the
          name its module gave it; 0 is "none known". }
        procedure SetAxisValue(const AName: string; AValue: double);
        { Every such number at once, as the client holds them. }
        procedure SetAxisValues(const AValues: TAxisValues);
        function AxisValue(const AName: string): double;
        { The date of each bar the argument counts, or none: what lets a date
          axis name a bar (TBarDateAxis) instead of reading its number as a
          day count. }
        procedure SetArgumentDates(const ADates: TAxisDates);

        function Choice(ADimension: TAxisDimension): TAxisChoice;
        { The mode in force, with the automatic entry resolved. }
        function ResolvedModeId(ADimension: TAxisDimension): string;
        { The axis in force. Owned here, and valid until the next change. }
        function Axis(ADimension: TAxisDimension): TCoordinateAxis;
        { Whether the menu entry for AEntryId is the one ticked: the choice as
          made, so Automatic stays ticked while it resolves to something else. }
        function IsTicked(ADimension: TAxisDimension;
            const AEntryId: string): boolean;
        { Whether the argument axis in force reads a number the problem holds
          - which is when setting one means anything. }
        function ReadsAxisValue: boolean;
        { Whether choosing AModeId needs a parameter first that is not known -
          and so a dialog for it before the choice can be applied. }
        function NeedsParameter(const AModeId: string): TAxisParameter;
        { The name of the number AModeId reads (apValue), or ''. }
        function ValueReadBy(const AModeId: string): string;
        { Everything that decides where a stored point is drawn, as one string:
          equal for two states that put every point in the same place. What
          lets the chart skip replotting every series when the axes it is
          handed again are the ones it already draws on - an open puts the
          project's axes back after its refresh has drawn everything, and that
          replot was 1.6 s of the S&P 500 project's open. }
        function MappingSignature: string;
    end;

implementation

constructor TChartAxes.Create;
var
    D: TAxisDimension;
begin
    inherited Create;
    for D := Low(TAxisDimension) to High(TAxisDimension) do
    begin
        FChoices[D].ModeId := AutomaticAxisModeId;
        FChoices[D].ChosenByUser := False;
        FChoices[D].Definition := Default(TAxisDefinition);
        FPreferences[D] := Default(TAxisPreferences);
        FAxes[D] := nil;
    end;
    FValues := nil;
end;

destructor TChartAxes.Destroy;
begin
    Invalidate;
    inherited;
end;

procedure TChartAxes.Invalidate;
var
    D: TAxisDimension;
begin
    for D := Low(TAxisDimension) to High(TAxisDimension) do
        FreeAndNil(FAxes[D]);
end;

procedure TChartAxes.Reset;
var
    D: TAxisDimension;
begin
    for D := Low(TAxisDimension) to High(TAxisDimension) do
    begin
        FChoices[D].ModeId := AutomaticAxisModeId;
        FChoices[D].ChosenByUser := False;
        FChoices[D].Definition := Default(TAxisDefinition);
    end;
    Invalidate;
end;

procedure TChartAxes.Choose(ADimension: TAxisDimension; const AModeId: string);
begin
    FChoices[ADimension].ModeId := Trim(AModeId);
    if FChoices[ADimension].ModeId = '' then
        FChoices[ADimension].ModeId := AutomaticAxisModeId;
    FChoices[ADimension].ChosenByUser := True;
    Invalidate;
end;

procedure TChartAxes.Restore(ADimension: TAxisDimension;
    const AStoredId: string; AChosenByUser: boolean;
    const ADefinition: TAxisDefinition);
begin
    FChoices[ADimension].Definition := ADefinition;
    FChoices[ADimension].ModeId := UsableModeId(ADimension, AStoredId,
        AChosenByUser, FValues, ADefinition);
    //  A choice that could not be honoured is not the user's any more: saving
    //  now must not write back a mode the session never showed.
    FChoices[ADimension].ChosenByUser := AChosenByUser and
        (FChoices[ADimension].ModeId <> AutomaticAxisModeId);
    Invalidate;
end;

procedure TChartAxes.SetDefinition(ADimension: TAxisDimension;
    const ADefinition: TAxisDefinition);
begin
    FChoices[ADimension].Definition := ADefinition;
    Invalidate;
end;

procedure TChartAxes.SetPreferences(ADimension: TAxisDimension;
    const APreferences: TAxisPreferences);
begin
    FPreferences[ADimension] := APreferences;
    Invalidate;
end;

function TChartAxes.UpdatePreferences(ADimension: TAxisDimension;
    const APreferences: TAxisPreferences): boolean;
var
    OldMode, OldTitle: string;
begin
    OldMode := ResolvedModeId(ADimension);
    OldTitle := Axis(ADimension).Title;
    SetPreferences(ADimension, APreferences);
    Result := (ResolvedModeId(ADimension) <> OldMode) or
        (Axis(ADimension).Title <> OldTitle);
end;

procedure TChartAxes.SetAxisValue(const AName: string; AValue: double);
begin
    PutAxisValue(FValues, AName, AValue);
    Invalidate;
end;

procedure TChartAxes.SetAxisValues(const AValues: TAxisValues);
begin
    FValues := Copy(AValues);
    Invalidate;
end;

function TChartAxes.AxisValue(const AName: string): double;
begin
    Result := AxisValueOf(FValues, AName);
end;

procedure TChartAxes.SetArgumentDates(const ADates: TAxisDates);
begin
    FArgumentDates := Copy(ADates);
    Invalidate;
end;

function TChartAxes.Choice(ADimension: TAxisDimension): TAxisChoice;
begin
    Result := FChoices[ADimension];
end;

function TChartAxes.ResolvedModeId(ADimension: TAxisDimension): string;
begin
    Result := axis_choice.ResolvedModeId(ADimension, FChoices[ADimension].ModeId,
        FPreferences[ADimension]);
end;

function TChartAxes.Axis(ADimension: TAxisDimension): TCoordinateAxis;
begin
    if not Assigned(FAxes[ADimension]) then
    begin
        //  Only the argument counts bars; a value is never named by a date.
        if ADimension = adArgument then
            FAxes[ADimension] := CreateAxisFor(ADimension,
                FChoices[ADimension].ModeId, FPreferences[ADimension],
                FValues, FChoices[ADimension].Definition, FArgumentDates)
        else
            FAxes[ADimension] := CreateAxisFor(ADimension,
                FChoices[ADimension].ModeId, FPreferences[ADimension],
                FValues, FChoices[ADimension].Definition);
    end;
    Result := FAxes[ADimension];
end;

function TChartAxes.IsTicked(ADimension: TAxisDimension;
    const AEntryId: string): boolean;
begin
    Result := SameText(FChoices[ADimension].ModeId, Trim(AEntryId));
end;

function TChartAxes.ReadsAxisValue: boolean;
var
    Mode: TAxisModeClass;
begin
    Mode := FindAxisMode(ResolvedModeId(adArgument));
    Result := Assigned(Mode) and (Mode.Info.Parameter = apValue);
end;

function TChartAxes.ValueReadBy(const AModeId: string): string;
var
    Mode: TAxisModeClass;
begin
    Result := '';
    Mode := FindAxisMode(AModeId);
    if Assigned(Mode) and (Mode.Info.Parameter = apValue) then
        Result := Mode.Info.ParameterName;
end;

function TChartAxes.MappingSignature: string;
var
    D: TAxisDimension;
    Mode: TAxisModeClass;
    FS: TFormatSettings;
    n: longint;
begin
    //  EXACT TEXT OF EVERY NUMBER (round-trip format), so two values a digit
    //  apart are two mappings.
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    //  ONLY WHAT THE MODE IN FORCE READS: a definition is kept whether or not
    //  its custom axis is in force, and a number the problem holds whether or
    //  not a mode reading it is - and an open that puts back an unused
    //  definition moves no point.
    Result := '';
    for D := Low(TAxisDimension) to High(TAxisDimension) do
    begin
        Mode := FindAxisMode(ResolvedModeId(D));
        Result := Result + #0 + ResolvedModeId(D);
        if Assigned(Mode) and (Mode.Info.Parameter = apDefinition) then
            Result := Result + #1 + FChoices[D].Definition.Forward + #1 +
                FChoices[D].Definition.Inverse;
        if Assigned(Mode) and (Mode.Info.Parameter = apValue) then
            Result := Result + #1 + Mode.Info.ParameterName + '=' +
                FloatToStrF(AxisValueOf(FValues, Mode.Info.ParameterName),
                    ffExponent, 17, 0, FS);
    end;
    //  THE DATES BY THEIR COUNT AND ENDS: a date axis places a bar by them,
    //  and they change only with the data, which brings a new count or new
    //  ends. Not every date: a hundred years of daily bars, compared on every
    //  axis change.
    n := Length(FArgumentDates);
    Result := Result + #0 + 'dates=' + IntToStr(n);
    if n > 0 then
        Result := Result + #1 + FloatToStrF(FArgumentDates[0], ffExponent, 17, 0, FS) +
            #1 + FloatToStrF(FArgumentDates[n - 1], ffExponent, 17, 0, FS);
end;

function TChartAxes.NeedsParameter(const AModeId: string): TAxisParameter;
var
    Mode: TAxisModeClass;
begin
    Result := apNone;
    Mode := FindAxisMode(AModeId);
    if not Assigned(Mode) then
        Exit;
    //  A custom axis always opens its dialog - that is how its formulas are
    //  given or changed - and a required parameter only when it is missing.
    if Mode.Info.Parameter = apDefinition then
        Exit(apDefinition);
    if Mode.Info.ParameterRequired and (Mode.Info.Parameter = apValue) and
        (AxisValueOf(FValues, Mode.Info.ParameterName) = 0) then
        Exit(apValue);
end;

end.
