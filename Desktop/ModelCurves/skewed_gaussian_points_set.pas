// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of skewed Gaussian curve.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit skewed_gaussian_points_set;

{$mode delphi}

interface

uses
    amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, named_points_set, position_curve_parameter,
    sigma_curve_parameter, skew_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Skewed Gaussian: a Gaussian modulated by an error-function skew factor
      (1 + erf(beta*(x-x0)/(sigma*sqrt2))). beta sets the asymmetry (0 symmetric,
      >0 skewed right, <0 left); A is a linear scale. Reduces to a Gaussian as
      beta -> 0. }
    TSkewedGaussianPointsSet = class(TFormulaPointsSet)
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

{======================= TSkewedGaussianPointsSet ===========================}

constructor TSkewedGaussianPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TSkewCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the skewed Gaussian curve must have built exactly its four variable parameters');
end;

function TSkewedGaussianPointsSet.GetNativeExpression: string;
begin
    Result := 'A/(sigma*sqrt(2*pi))*exp(-(x-x0)^2/(2*sigma^2))' +
        '*(1+erf(beta*(x-x0)/(sigma*sqrt(2))))';
end;

class function TSkewedGaussianPointsSet.GetCurveTypeName: string;
begin
    Result := 'Skewed Gaussian';
end;

class function TSkewedGaussianPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{a32763f7-18dc-4378-a8e6-c0cce28b5fd6}');
end;

class function TSkewedGaussianPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TSkewedGaussianPointsSet);
end.
