// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Gauss form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit gauss_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    explanation, amplitude_curve_parameter, coordinate_axis, Classes, curve_points_set,
    curve_types_singleton, named_points_set, points_set, position_curve_parameter, sigma_curve_parameter,
    SimpMath, special_curve_parameter, SysUtils;

type
    { Curve having Gauss form. }
    TGaussPointsSet = class(TNamedPointsSet)
    protected
        { Performs recalculation of all points of function. }
        procedure DoCalc; override;

    public
        constructor Create(AOwner: TComponent; x0: double); overload;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function Explanation: TExplanation; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        { A diffraction lineshape: its argument is the scattering angle and its
          value the measured intensity. }
        class function PreferredAxisMode(ADimension: TAxisDimension): string;
            override;
        function GetCurveExpression: string; override;
    end;

implementation

uses
    int_curve_factory, checks, diffraction_axis_modes;

{=========================== TGaussPointsSet ==================================}

constructor TGaussPointsSet.Create(AOwner: TComponent; x0: double);
var
    Parameter: TSpecialCurveParameter;
    Count:     longint;
begin
    inherited Create(AOwner);
    Parameter := TAmplitudeCurveParameter.Create;
    AddParameter(Parameter);

    Parameter := TPositionCurveParameter.Create(x0, Self);
    AddParameter(Parameter);

    Parameter := TSigmaCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 3, 'the Gaussian curve must have built exactly its three variable parameters');
end;

class function TGaussPointsSet.GetCurveTypeName: string;
begin
    Result := 'Gaussian';
end;

class function TGaussPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{ff4e399c-c33c-482e-84d7-952700bcd4ae}');
end;

class function TGaussPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

class function TGaussPointsSet.PreferredAxisMode(
    ADimension: TAxisDimension): string;
begin
    //  Coordinates of a diffraction pattern are stored in 2*Theta.
    Result := DiffractionPreference(ADimension);
end;

function TGaussPointsSet.GetCurveExpression: string;
begin
    //  Mirrors GaussPoint in SimpMath.pas (area-normalized by A).
    Result := 'A/(sigma*sqrt(2*pi))*exp(-(x0-x)**2/(2*sigma**2))';
end;

procedure TGaussPointsSet.DoCalc;
begin
    Gauss(FPoints, A, Sigma, x0);
end;

var
    CTS: ICurveFactory;

class function TGaussPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A symmetric bell-shaped peak whose area is A, centred at x0, ' +
        'with standard deviation sigma.',
        esCanonical);
    AddParagraph(Result,
        'The normal (Gaussian) density scaled by A: A / (sigma sqrt(2 ' +
        'pi)) exp(-(x - x0)^2 / (2 sigma^2)). A is therefore the area ' +
        'under the peak, not its height; the height is A / (sigma sqrt(2 ' +
        'pi)).');
    AddParagraph(Result,
        'x0 is the position of the maximum and sigma the standard ' +
        'deviation; the full width at half maximum is 2 sqrt(2 ln 2) ' +
        'sigma, about 2.355 sigma.');
    AddParagraph(Result,
        'It is the shape of broadening made of many small independent ' +
        'contributions, such as instrumental resolution.');
    Result.Quote := 'f(x) = exp(-(x - mu)^2 / (2 sigma^2)) / (sigma sqrt(2 pi))';
    AddLimitation(Result,
        'Its tails fall off very quickly, so it underestimates the wings ' +
        'of a peak that also has lifetime or size (Lorentzian) broadening ' +
        '- use Voigt or Pseudo-Voigt there.');
    AddLimitation(Result,
        'It is symmetric; an asymmetric peak fitted with it leaves a ' +
        'systematic residual on one side.');
    AddReference(Result,
        'NIST/SEMATECH e-Handbook of Statistical Methods',
        'section 1.3.6.6.1, Normal Distribution',
        'https://www.itl.nist.gov/div898/handbook/eda/section3/eda3661.htm');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TGaussPointsSet);
end.
