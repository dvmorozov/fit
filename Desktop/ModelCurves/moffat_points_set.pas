// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Moffat form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit moffat_points_set;

{$mode delphi}

interface

uses
    explanation, amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, named_points_set, position_curve_parameter,
    shape_curve_parameter, sigma_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Curve having Moffat form: A/(1+((x-x0)/sigma)^2)^m. sigma is the core
      width (HWHM = sigma*sqrt(2^(1/m)-1)) and m (the Moffat beta) sets how heavy
      the tails are. Peak-height-normalised (A = peak). At m = 1 it is a
      Lorentzian with sigma as its half-width. }
    TMoffatPointsSet = class(TFormulaPointsSet)
    protected
        function GetNativeExpression: string; override;

    public
        constructor Create(AOwner: TComponent; x0: double); overload;
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function Explanation: TExplanation; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
    end;

implementation

uses
    int_curve_factory, checks;

{=========================== TMoffatPointsSet ================================}

constructor TMoffatPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TShapeCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the Moffat curve must have built exactly its four variable parameters');
end;

function TMoffatPointsSet.GetNativeExpression: string;
begin
    Result := 'A/(1+((x-x0)/sigma)^2)^m';
end;

class function TMoffatPointsSet.GetCurveTypeName: string;
begin
    Result := 'Moffat';
end;

class function TMoffatPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{e6e51486-6970-4ab3-8010-908664116379}');
end;

class function TMoffatPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

class function TMoffatPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A symmetric peak of height A with core width sigma and power-law ' +
        'wings set by m, used for stellar images.',
        esCanonical);
    AddParagraph(Result,
        'A / (1 + ((x - x0) / sigma)^2)^m. m = 1 is a Lorentzian; larger ' +
        'm gives weaker wings.');
    AddParagraph(Result,
        'It was introduced for the point-spread function of stars, whose ' +
        'wings are heavier than a Gaussian predicts.');
    Result.Quote := 'I(r) = I0 (1 + (r / alpha)^2)^(-beta)';
    AddLimitation(Result,
        'sigma is a core scale, not the full width at half maximum; the ' +
        'half width depends on m as well.');
    AddLimitation(Result,
        'A is the height, not the area.');
    AddReference(Result,
        'A. F. J. Moffat, A theoretical investigation of focal stellar ' +
        'images in the photographic emulsion and application to ' +
        'photographic photometry, Astron. Astrophys. 3 (1969)',
        'pp. 455-461',
        '');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TMoffatPointsSet);
end.
