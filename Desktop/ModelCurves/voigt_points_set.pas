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
    amplitude_curve_parameter, Classes, curve_types_singleton,
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

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TVoigtPointsSet);
end.
