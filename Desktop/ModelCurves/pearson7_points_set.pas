// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Pearson VII form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit pearson7_points_set;

{$mode delphi}

interface

uses
    amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, named_points_set, position_curve_parameter,
    shape_curve_parameter, sigma_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Curve having Pearson VII form. Parameterised by FWHM (sigma) and shape
      exponent m, peak-height-normalised so A is the peak height. At m = 1 it is
      a Lorentzian of the same FWHM; as m -> infinity it tends to a Gaussian. }
    TPearson7PointsSet = class(TFormulaPointsSet)
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

{========================== TPearson7PointsSet ===============================}

constructor TPearson7PointsSet.Create(AOwner: TComponent; x0: double);
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
    CheckThat(Count = 4, 'the Pearson VII curve must have built exactly its four variable parameters');
end;

function TPearson7PointsSet.GetNativeExpression: string;
begin
    //  sigma = FWHM, m = shape. The (2^(1/m)-1) factor makes the half-maximum
    //  fall at |x-x0| = sigma/2 for every m; peak-height-normalised (A = peak).
    Result := 'A/(1+(2^(1/m)-1)*(2*(x-x0)/sigma)^2)^m';
end;

class function TPearson7PointsSet.GetCurveTypeName: string;
begin
    Result := 'Pearson VII';
end;

class function TPearson7PointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{195b2208-9aa0-4281-8cab-7d29d959ac4d}');
end;

class function TPearson7PointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TPearson7PointsSet);
end.
