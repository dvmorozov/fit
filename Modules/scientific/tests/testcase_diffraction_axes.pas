// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The diffraction angles this module draws a profile on.)

What the framework does with a module's axis mode - prefers it, remembers it,
asks for the number it reads - is the framework's suite's, tested there with
stand-in modes. What is left is this module's own: the angles' arithmetic, the
captions a user reads over a diffraction pattern, and the preference its peak
shapes state, so that a placed peak is drawn on 2 Theta and Intensity.
}
unit testcase_diffraction_axes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, coordinate_axis,
    axis_mode_registry, axis_mode_registration, axis_choice, chart_axes,
    diffraction_axis_modes, gauss_points_set, pearson7_points_set;

type
    TDiffractionAxesTest = class(TTestCase)
    private
        FAxes: TChartAxes;
        function TitleOf(ADimension: TAxisDimension; const AModeId: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The arithmetic: raw is 2 Theta in degrees.
        procedure TwoThetaShowsTheStoredAngle;
        procedure ThetaIsHalfTheStoredAngle;
        procedure SinThetaOverLambdaDividesByTheWavelengthAndRoundTrips;
        //  What a user reads over a diffraction pattern.
        procedure TwoThetaModeLabelsDegrees;
        procedure ThetaModeLabelsDegrees;
        procedure SinThetaOverLambdaModeLabelsReciprocalAngstrom;
        procedure TheValueOfADiffractionPatternIsAnIntensity;
        { A peak shape, and one defined through the shared formula base, both
          prefer the scattering angle - so the preference is inherited rather
          than repeated per type. }
        procedure ThePeakShapesPreferTheScatteringAngle;
    end;

implementation

const
    //  CuKa1, the wavelength most laboratory diffractometers use.
    CuKa = 1.54056;

procedure TDiffractionAxesTest.SetUp;
begin
    RegisterAllAxisModes;
    RegisterDiffractionAxisModes;
    FAxes := TChartAxes.Create;
    FAxes.SetAxisValue(WavelengthParameter, CuKa);
end;

procedure TDiffractionAxesTest.TearDown;
begin
    FreeAndNil(FAxes);
end;

function TDiffractionAxesTest.TitleOf(ADimension: TAxisDimension;
    const AModeId: string): string;
begin
    FAxes.Choose(ADimension, AModeId);
    Result := FAxes.Axis(ADimension).Title;
end;

procedure TDiffractionAxesTest.TwoThetaShowsTheStoredAngle;
var
    Axis: TDiffractionAngleAxis;
begin
    Axis := TDiffractionAngleAxis.Create(dmTwoTheta, 1.5);
    try
        AssertEquals('2theta to-display', 30.0, Axis.ToDisplay(30.0), 1e-12);
        AssertEquals('2theta from-display', 30.0, Axis.FromDisplay(30.0), 1e-12);
    finally
        Axis.Free;
    end;
end;

procedure TDiffractionAxesTest.ThetaIsHalfTheStoredAngle;
var
    Axis: TDiffractionAngleAxis;
begin
    Axis := TDiffractionAngleAxis.Create(dmTheta, 1.5);
    try
        AssertEquals('theta = 2theta/2', 15.0, Axis.ToDisplay(30.0), 1e-12);
        AssertEquals('inverse theta', 30.0, Axis.FromDisplay(15.0), 1e-12);
    finally
        Axis.Free;
    end;
end;

procedure TDiffractionAxesTest.SinThetaOverLambdaDividesByTheWavelengthAndRoundTrips;
var
    Axis: TDiffractionAngleAxis;
    D: double;
begin
    Axis := TDiffractionAngleAxis.Create(dmSinThetaOverLambda, 1.5);
    try
        //  2theta = 30 -> sin(15 deg) / 1.5 = 0.2588190 / 1.5 = 0.1725460
        D := Axis.ToDisplay(30.0);
        AssertEquals('sin(theta)/lambda', 0.1725460, D, 1e-6);
        AssertEquals('round-trip to 2theta', 30.0, Axis.FromDisplay(D), 1e-9);
    finally
        Axis.Free;
    end;
end;

procedure TDiffractionAxesTest.TwoThetaModeLabelsDegrees;
begin
    AssertEquals('2*Theta [deg]', TitleOf(adArgument, TwoThetaAxisModeId));
end;

procedure TDiffractionAxesTest.ThetaModeLabelsDegrees;
begin
    AssertEquals('Theta [deg]', TitleOf(adArgument, ThetaAxisModeId));
end;

procedure TDiffractionAxesTest.SinThetaOverLambdaModeLabelsReciprocalAngstrom;
begin
    AssertEquals('Sin(Theta)/Lambda [1/A]',
        TitleOf(adArgument, SinThetaOverLambdaAxisModeId));
end;

procedure TDiffractionAxesTest.TheValueOfADiffractionPatternIsAnIntensity;
begin
    AssertEquals('Intensity', TitleOf(adValue, IntensityAxisModeId));
end;

procedure TDiffractionAxesTest.ThePeakShapesPreferTheScatteringAngle;
begin
    AssertEquals('a Gaussian', TwoThetaAxisModeId,
        TGaussPointsSet.PreferredAxisMode(adArgument));
    AssertEquals('and its value', IntensityAxisModeId,
        TGaussPointsSet.PreferredAxisMode(adValue));
    AssertEquals('a formula lineshape', TwoThetaAxisModeId,
        TPearson7PointsSet.PreferredAxisMode(adArgument));
    AssertEquals('and its value', IntensityAxisModeId,
        TPearson7PointsSet.PreferredAxisMode(adValue));
end;

initialization
    RegisterTest('unit', TDiffractionAxesTest);
end.
