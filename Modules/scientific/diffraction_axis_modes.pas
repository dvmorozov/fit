// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The coordinates of a diffraction pattern: the scattering angle in
three forms, and intensity.)

DIFFRACTION'S, NOT THE FRAMEWORK'S. These were the closed modes XCM_2T, XCM_T
and XCM_SINTL; they are now registered through the same call any module uses,
from the scientific module's front door, beside the peak shapes that prefer
them - the framework registers no field's coordinates. Their ids are what a
settings file or project written before the registry is migrated to
(axis_choice.LegacyArgumentModeId, through the map this unit registers), so
they must not change.

THE WAVELENGTH IS THIS MODULE'S NUMBER: the angles read it by name
(WavelengthParameter), and this unit describes it to the framework
(RegisterAxisParameter), which then asks for it, keeps it in the axes and hands
it to the angles without a word of it in the framework's own code.
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
    { The number the angles read: the radiation's wavelength, in angstrom.
      The name the problem keeps it under (scientific_value_store) and its
      dialog is registered as (scientific_forms). Also the name a project
      written before 2026-10-17 hands its wavelength to
      (fit_project_document.FormerWaveLengthValue) - so it never changes. }
    WavelengthParameter = 'wavelength';
    { Where every one of these modes is explained: the scientific module's
      chapter (scientific_guide), which owns the angles. Declared with the
      modes rather than taken from that chapter, so this unit - still in the
      core until the diffraction extraction moves it - names no module. }
    DiffractionAnglesTopic = 'peaks-and-diffraction/diffraction-angles';

type
    //  THE AXIS THE THREE ANGLES DRAW ON, moved here from coordinate_axis with
    //  the modes that build it (2026-10-17): nothing else draws one, and the
    //  framework's own axes name no field.
    { Selectable diffraction display unit. Raw value is 2*Theta (degrees). }
    TDiffractionMode = (dmTwoTheta, dmTheta, dmSinThetaOverLambda);

    { Preserves the legacy 2*Theta / Theta / Sin(Theta)/Lambda family as one axis. }
    TDiffractionAngleAxis = class(TCoordinateAxis)
    private
        FMode: TDiffractionMode;
        FWaveLength: double;
    public
        constructor Create(AMode: TDiffractionMode; AWaveLength: double);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
    end;

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

{ Registers the angles and the intensity, the wavelength they read, and what a
  file written before the registry meant by display modes 0, 1 and 2. }
procedure RegisterDiffractionAxisModes;

{ What the wavelength is, as the framework asks for it. }
function WavelengthParameterInfo: TAxisParameterInfo;

{ What a diffraction peak shape prefers for ADimension: 2 Theta for the
  argument, intensity for the value. One answer for every peak type, so the
  preference is stated once rather than per shape. }
function DiffractionPreference(ADimension: TAxisDimension): string;

implementation

uses
    Math, checks, axis_choice;

{ TDiffractionAngleAxis }

constructor TDiffractionAngleAxis.Create(AMode: TDiffractionMode; AWaveLength: double);
begin
    inherited Create;
    FMode := AMode;
    FWaveLength := AWaveLength;
end;

function TDiffractionAngleAxis.DisplayName: string;
begin
    case FMode of
        dmTwoTheta:            Result := '2*Theta';
        dmTheta:               Result := 'Theta';
        dmSinThetaOverLambda:  Result := 'Sin(Theta)/Lambda';
    end;
end;

function TDiffractionAngleAxis.UnitName: string;
begin
    case FMode of
        dmTwoTheta, dmTheta:   Result := 'deg';
        dmSinThetaOverLambda:  Result := '1/A';
    end;
end;

function TDiffractionAngleAxis.ToDisplay(const RawValue: double): double;
begin
    case FMode of
        dmTwoTheta: Result := RawValue;
        dmTheta:    Result := RawValue / 2;
        dmSinThetaOverLambda:
        begin
            CheckThat(FWaveLength <> 0, 'a sin(theta)/lambda axis needs a nonzero wavelength');
            Result := Sin((RawValue * pi) / (2 * 180)) / FWaveLength;
        end;
    end;
end;

function TDiffractionAngleAxis.FromDisplay(const DisplayValue: double): double;
begin
    case FMode of
        dmTwoTheta: Result := DisplayValue;
        dmTheta:    Result := DisplayValue * 2;
        dmSinThetaOverLambda:
        begin
            CheckThat(FWaveLength <> 0, 'a sin(theta)/lambda axis needs a nonzero wavelength');
            Result := 2 * (180 / pi) * ArcSin(DisplayValue * FWaveLength);
        end;
    end;
end;


function AngleInfo(const AId, ACaption: string): TAxisModeInfo;
begin
    Result.Id := AId;
    Result.Caption := ACaption;
    Result.Topic := DiffractionAnglesTopic;
    Result.Dimensions := [adArgument];
    //  Every angle READS the wavelength - it is what Set Rule Parameters is
    //  offered for - but only sin(theta)/lambda cannot be drawn without one.
    Result.Parameter := apValue;
    Result.ParameterRequired := False;
    Result.ParameterName := WavelengthParameter;
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
    Result := TDiffractionAngleAxis.Create(dmTwoTheta, AxisValueOf(AContext.Values, WavelengthParameter));
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
    Result := TDiffractionAngleAxis.Create(dmTheta, AxisValueOf(AContext.Values, WavelengthParameter));
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
        AxisValueOf(AContext.Values, WavelengthParameter));
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
    Result.ParameterName := '';
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

function WavelengthParameterInfo: TAxisParameterInfo;
begin
    Result.Name := WavelengthParameter;
    //  The words the window asks and refuses in, as they always were: "The
    //  wavelength must be a number, written with a full stop - for example
    //  1.5406." CuKa1, the wavelength most laboratory diffractometers use.
    Result.Caption := 'Wavelength';
    Result.Example := '1.5406';
end;

procedure RegisterDiffractionAxisModes;
begin
    //  What the angles read, before the angles: a mode naming a number nobody
    //  describes is a finding (AxisModeFindings).
    RegisterAxisParameter(WavelengthParameterInfo);
    //  In the order the menu has always listed them.
    RegisterAxisMode(TThetaAxisMode);
    RegisterAxisMode(TTwoThetaAxisMode);
    RegisterAxisMode(TSinThetaOverLambdaAxisMode);
    RegisterAxisMode(TIntensityAxisMode);
    //  WHAT AN OLD FILE'S 0, 1 AND 2 MEANT: the former XCM_2T, XCM_T and
    //  XCM_SINTL, which settings and projects written before the registry
    //  hold. The framework answers only for its own integers.
    RegisterLegacyArgumentMode(0, TwoThetaAxisModeId);
    RegisterLegacyArgumentMode(1, ThetaAxisModeId);
    RegisterLegacyArgumentMode(2, SinThetaOverLambdaAxisModeId);
end;

end.
