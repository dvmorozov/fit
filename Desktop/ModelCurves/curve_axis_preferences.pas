// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the model, the data and the selected curve type prefer for a
coordinate, gathered for the automatic axis rule.)

THE MODEL'S CURVES BY TITLE. A curve's type is not on the wire - 'curveType' is
the model's selected type, and a model may hold several - so it is read back
from the title the engine gives every curve, exactly as the Model panel reads it
(model_outline.CurveTypeNameOfTitle). A title naming no registered type, or one
two types share, prefers nothing: guessing would caption the chart for the
wrong field.
}
unit curve_axis_preferences;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, coordinate_axis, axis_mode_registry, axis_choice,
    named_points_set;

{ The preferences for ADimension of a model whose curves carry ACurveTitles,
  over data declaring ADataModeId, with ASelectedTypeId selected in the Tools
  list (GUID_NULL for none). }
function ModelAxisPreferences(ADimension: TAxisDimension;
    const ACurveTitles: TStringArray; const ADataModeId: string;
    const ASelectedTypeId: TCurveTypeId): TAxisPreferences;

implementation

uses
    curve_types_singleton, int_curve_factory, model_outline;

function ModelAxisPreferences(ADimension: TAxisDimension;
    const ACurveTitles: TStringArray; const ADataModeId: string;
    const ASelectedTypeId: TCurveTypeId): TAxisPreferences;
var
    CurveClass: TCurveClass;
    i: longint;
begin
    Result := Default(TAxisPreferences);
    SetLength(Result.Model, Length(ACurveTitles));
    SetLength(Result.ModelFallback, Length(ACurveTitles));
    for i := 0 to High(ACurveTitles) do
    begin
        CurveClass := FindCurveClassByName(CurveTypeNameOfTitle(ACurveTitles[i]));
        Result.Model[i] := '';
        Result.ModelFallback[i] := '';
        if Assigned(CurveClass) then
        begin
            Result.Model[i] := CurveClass.PreferredAxisMode(ADimension);
            Result.ModelFallback[i] := CurveClass.FallbackAxisMode(ADimension);
        end;
    end;
    Result.Data := ADataModeId;
    Result.SelectedType := '';
    CurveClass := FindCurveClassById(ASelectedTypeId);
    if Assigned(CurveClass) then
        Result.SelectedType := CurveClass.PreferredAxisMode(ADimension);
end;

end.
