// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having true Voigt form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit voigt_points_set;

{$mode delphi}

interface

uses
    explanation, amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, gamma_curve_parameter, named_points_set,
    position_curve_parameter, sigma_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Curve having true Voigt form - a Gaussian (std sigma) convolved with a
      Lorentzian (HWHM gamma), evaluated through the Faddeeva function. A is the
      area. Distinct from the existing Pseudo-Voigt (a weighted sum
      approximation): this is the exact convolution. Reduces to a Gaussian as
      gamma -> 0 and to a Lorentzian as sigma -> 0. }
    TVoigtPointsSet = class(TFormulaPointsSet)
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

{=========================== TVoigtPointsSet =================================}

constructor TVoigtPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TGammaCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the Voigt curve must have built exactly its four variable parameters');
end;

function TVoigtPointsSet.GetNativeExpression: string;
begin
    //  voigt(u, sigma, gamma) is the area-normalised Voigt profile (Faddeeva),
    //  provided by both engines (native_math_expr / scipy.special).
    Result := 'A*voigt(x-x0,sigma,gamma)';
end;

class function TVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Voigt';
end;

class function TVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{eeed2ec3-d036-473e-81e1-8e40943d8158}');
end;

class function TVoigtPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

class function TVoigtPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'The convolution of a Gaussian of width sigma with a Lorentzian ' +
        'of half-width gamma, scaled by A.',
        esCanonical);
    AddParagraph(Result,
        'The exact profile of a peak broadened by two independent ' +
        'mechanisms at once: a Gaussian one (instrument, strain) with ' +
        'standard deviation sigma and a Lorentzian one (lifetime, size) ' +
        'with half-width gamma.');
    AddParagraph(Result,
        'It is what Pseudo-Voigt approximates; use it when the two widths ' +
        'matter separately.');
    Result.Quote := 'U(x, t) + i V(x, t) = sqrt(pi / (4 t)) exp(z^2) erfc(z), z = (1 ' +
        '- i x) / (2 sqrt(t))';
    AddLimitation(Result,
        'It is more expensive to evaluate than Pseudo-Voigt.');
    AddLimitation(Result,
        'When one mechanism dominates, sigma and gamma become strongly ' +
        'correlated and the smaller one is poorly determined.');
    AddReference(Result,
        'NIST Digital Library of Mathematical Functions',
        'section 7.19, Voigt Functions',
        'https://dlmf.nist.gov/7.19');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TVoigtPointsSet);
end.
