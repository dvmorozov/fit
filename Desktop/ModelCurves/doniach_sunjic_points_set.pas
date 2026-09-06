// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Doniach-Sunjic form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit doniach_sunjic_points_set;

{$mode delphi}

interface

uses
    amplitude_curve_parameter, asymmetry_curve_parameter, Classes,
    curve_types_singleton, formula_points_set, named_points_set,
    position_curve_parameter, sigma_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Curve having Doniach-Sunjic form - the asymmetric core-level lineshape of
      X-ray photoelectron spectroscopy. sigma is the width, alpha the singularity
      (asymmetry) index; A is a linear scale. At alpha = 0 it is a Lorentzian with
      sigma as its half-width. }
    TDoniachSunjicPointsSet = class(TFormulaPointsSet)
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

{======================= TDoniachSunjicPointsSet =============================}

constructor TDoniachSunjicPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TAsymmetryCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the Doniach-Sunjic curve must have built exactly its four variable parameters');
end;

function TDoniachSunjicPointsSet.GetNativeExpression: string;
begin
    Result := 'A*cos(pi*alpha/2+(1-alpha)*arctan((x-x0)/sigma))' +
        '/(sigma^2+(x-x0)^2)^((1-alpha)/2)';
end;

class function TDoniachSunjicPointsSet.GetCurveTypeName: string;
begin
    Result := 'Doniach-Sunjic';
end;

class function TDoniachSunjicPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{ec663a56-0e89-4bc3-91fd-f243aadb253e}');
end;

class function TDoniachSunjicPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TDoniachSunjicPointsSet);
end.
