// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The axis modes this build ships: the framework's own field-neutral
ones, and the two fields still in this tree.)

THE FRAMEWORK'S OWN MODES name no field: the coordinate as loaded (General
Position, General Value), its decimal logarithm, and an axis of the user's own
formulas. Each serves either coordinate the same way, which is what lets the
value axis be logarithmic without a line written for it.

ONE FIELD IS STILL REGISTERED FROM HERE, and that is recorded debt, not
precedent (docs/internal/diffraction-extraction.md): the diffraction angles and
intensity, beside the peak shapes that prefer them. They are registered through
exactly the call a module makes, from their own unit, so moving them out is
moving the unit and the one line below that names it - which is how the
price-series coordinates left, for the module that analyses price series.

EXPLICIT, NOT AN initialization SECTION, for data_loader_registration's reason:
a unit that was never linked never runs its initialization, which is the very
thing that would go unnoticed.
}
unit axis_mode_registration;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, coordinate_axis, axis_mode_registry;

const
    PositionAxisModeId = 'position';
    ValueAxisModeId = 'value';
    LogarithmicAxisModeId = 'logarithmic';
    CustomAxisModeId = 'custom';

type
    { The argument as loaded, under the field-neutral name Position. }
    TPositionAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    { The value as loaded, under the field-neutral name Value. }
    TValueAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    { The decimal logarithm of whatever the coordinate is, read in that
      quantity. For either coordinate: a log value axis is the ordinary
      logarithmic chart, and a log argument is how a dose or a concentration
      is plotted. }
    TLogarithmicAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    { The user's own pair of formulas, one pair per coordinate. }
    TCustomAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CaptionFor(ADimension: TAxisDimension): string; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

{ Registers every mode this build ships and checks the result. Idempotent. The
  framework's own come first, so they head every menu; a module's follow, from
  its front door. }
procedure RegisterAllAxisModes;

{ The name a coordinate goes by when nothing says what it is. }
function GeneralQuantityName(ADimension: TAxisDimension): string;

implementation

uses
    guide_data, diffraction_axis_modes;

function GeneralQuantityName(ADimension: TAxisDimension): string;
begin
    case ADimension of
        adArgument: Result := 'Position';
        adValue:    Result := 'Value';
    end;
end;

function NewInfo(const AId, ACaption, ATopic: string;
    ADimensions: TAxisDimensions): TAxisModeInfo;
begin
    Result.Id := AId;
    Result.Caption := ACaption;
    Result.Topic := ATopic;
    Result.Dimensions := ADimensions;
    Result.Parameter := apNone;
    Result.ParameterRequired := False;
end;

{ TPositionAxisMode }

class function TPositionAxisMode.Info: TAxisModeInfo;
begin
    Result := NewInfo(PositionAxisModeId, 'General Position', ArgumentAxesTopic,
        [adArgument]);
end;

{$hints off}
class function TPositionAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TNamedAxis.Create(GeneralQuantityName(adArgument), '');
end;
{$hints on}

{ TValueAxisMode }

class function TValueAxisMode.Info: TAxisModeInfo;
begin
    Result := NewInfo(ValueAxisModeId, 'General Value', ValueAxesTopic,
        [adValue]);
end;

{$hints off}
class function TValueAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TNamedAxis.Create(GeneralQuantityName(adValue), '');
end;
{$hints on}

{ TLogarithmicAxisMode }

class function TLogarithmicAxisMode.Info: TAxisModeInfo;
begin
    Result := NewInfo(LogarithmicAxisModeId, 'Logarithmic', LogarithmicAxisTopic,
        [adArgument, adValue]);
end;

class function TLogarithmicAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
var
    Name_: string;
begin
    //  The quantity's own name when the automatic rule found one, so a log
    //  intensity is still captioned Intensity; the general name otherwise.
    Name_ := Trim(AContext.QuantityName);
    if Name_ = '' then
        Name_ := GeneralQuantityName(ADimension);
    Result := TLogarithmicAxis.Create(Name_, AContext.QuantityUnit);
end;

{ TCustomAxisMode }

class function TCustomAxisMode.Info: TAxisModeInfo;
begin
    Result := NewInfo(CustomAxisModeId, 'Custom...', CustomAxisTopic,
        [adArgument, adValue]);
    Result.Parameter := apDefinition;
    Result.ParameterRequired := True;
end;

class function TCustomAxisMode.CaptionFor(ADimension: TAxisDimension): string;
begin
    //  The argument's entry kept the caption it has always had, so the guide
    //  and every user who learnt it still find it.
    case ADimension of
        adArgument: Result := 'Custom Position...';
        adValue:    Result := 'Custom Value...';
    end;
end;

{$hints off}
class function TCustomAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TExpressionAxis.Create(AContext.Definition.Name,
        AContext.Definition.UnitName, AContext.Definition.Forward,
        AContext.Definition.Inverse);
end;
{$hints on}

procedure RegisterAllAxisModes;
begin
    RegisterAxisMode(TPositionAxisMode);
    RegisterAxisMode(TValueAxisMode);
    RegisterAxisMode(TLogarithmicAxisMode);
    RegisterAxisMode(TCustomAxisMode);
    //  The one field still in this tree, through the call a module makes.
    RegisterDiffractionAxisModes;
end;

end.
