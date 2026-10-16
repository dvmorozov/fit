// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The coordinates of a diffraction pattern: the scattering angle in
three forms, and intensity.)

DIFFRACTION'S, NOT THE FRAMEWORK'S. These were the closed modes XCM_2T, XCM_T
and XCM_SINTL; they are now registered through the same call a module uses, and
this unit leaves the framework with the peak shapes that prefer them
(docs/internal/diffraction-extraction.md, stage 1). Their ids are what a
settings file or project written before the registry is migrated to
(axis_choice.LegacyArgumentModeId), so they must not change.
}
unit diffraction_axis_modes;

{$mode objfpc}{$H+}

interface

uses
    coordinate_axis, axis_mode_registry;

const
    TwoThetaAxisModeId = 'diffraction.two-theta';
    ThetaAxisModeId = 'diffraction.theta';
    SinThetaOverLambdaAxisModeId = 'diffraction.sin-theta-over-lambda';
    IntensityAxisModeId = 'diffraction.intensity';

type
    TTwoThetaAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    TThetaAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    TSinThetaOverLambdaAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

    { The measured intensity, as loaded. Only a name: what the detector
      counted is what is drawn. }
    TIntensityAxisMode = class(TAxisMode)
    public
        class function Info: TAxisModeInfo; override;
        class function CreateAxis(ADimension: TAxisDimension;
            const AContext: TAxisContext): TCoordinateAxis; override;
    end;

procedure RegisterDiffractionAxisModes;

{ What a diffraction peak shape prefers for ADimension: 2 Theta for the
  argument, intensity for the value. One answer for every peak type, so the
  preference is stated once rather than per shape. }
function DiffractionPreference(ADimension: TAxisDimension): string;

implementation

uses
    guide_data;

function AngleInfo(const AId, ACaption: string): TAxisModeInfo;
begin
    Result.Id := AId;
    Result.Caption := ACaption;
    Result.Topic := DiffractionAnglesTopic;
    Result.Dimensions := [adArgument];
    //  Every angle READS the wavelength - it is what Set Rule Parameters is
    //  offered for - but only sin(theta)/lambda cannot be drawn without one.
    Result.Parameter := apWaveLength;
    Result.ParameterRequired := False;
end;

{ TTwoThetaAxisMode }

class function TTwoThetaAxisMode.Info: TAxisModeInfo;
begin
    Result := AngleInfo(TwoThetaAxisModeId, '2 * Theta');
end;

{$hints off}
class function TTwoThetaAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TDiffractionAngleAxis.Create(dmTwoTheta, AContext.WaveLength);
end;
{$hints on}

{ TThetaAxisMode }

class function TThetaAxisMode.Info: TAxisModeInfo;
begin
    Result := AngleInfo(ThetaAxisModeId, 'Theta');
end;

{$hints off}
class function TThetaAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TDiffractionAngleAxis.Create(dmTheta, AContext.WaveLength);
end;
{$hints on}

{ TSinThetaOverLambdaAxisMode }

class function TSinThetaOverLambdaAxisMode.Info: TAxisModeInfo;
begin
    Result := AngleInfo(SinThetaOverLambdaAxisModeId, 'Sin Theta / Lambda');
    Result.ParameterRequired := True;
end;

{$hints off}
class function TSinThetaOverLambdaAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TDiffractionAngleAxis.Create(dmSinThetaOverLambda,
        AContext.WaveLength);
end;
{$hints on}

{ TIntensityAxisMode }

class function TIntensityAxisMode.Info: TAxisModeInfo;
begin
    Result.Id := IntensityAxisModeId;
    Result.Caption := 'Intensity';
    Result.Topic := DiffractionAnglesTopic;
    Result.Dimensions := [adValue];
    Result.Parameter := apNone;
    Result.ParameterRequired := False;
end;

{$hints off}
class function TIntensityAxisMode.CreateAxis(ADimension: TAxisDimension;
    const AContext: TAxisContext): TCoordinateAxis;
begin
    Result := TNamedAxis.Create('Intensity', '');
end;
{$hints on}

function DiffractionPreference(ADimension: TAxisDimension): string;
begin
    case ADimension of
        adArgument: Result := TwoThetaAxisModeId;
        adValue:    Result := IntensityAxisModeId;
    end;
end;

procedure RegisterDiffractionAxisModes;
begin
    //  In the order the menu has always listed them.
    RegisterAxisMode(TThetaAxisMode);
    RegisterAxisMode(TTwoThetaAxisMode);
    RegisterAxisMode(TSinThetaOverLambdaAxisMode);
    RegisterAxisMode(TIntensityAxisMode);
end;

end.
