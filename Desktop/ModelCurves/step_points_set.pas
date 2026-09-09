// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of an error-function step (edge) curve.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit step_points_set;

{$mode delphi}

interface

uses
    amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, named_points_set, position_curve_parameter,
    sigma_curve_parameter, special_curve_parameter, SysUtils;

type
    { An error-function step (smoothed edge): (A/2)*(1 + erf((x-x0)/(sigma*sqrt2))).
      A is the step height, x0 the edge centre (value A/2 there), sigma the edge
      width. Rises from 0 (x << x0) to A (x >> x0). }
    TStepPointsSet = class(TFormulaPointsSet)
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

{============================ TStepPointsSet =================================}

constructor TStepPointsSet.Create(AOwner: TComponent; x0: double);
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
    CheckThat(Count = 3, 'the step curve must have built exactly its three variable parameters');
end;

function TStepPointsSet.GetNativeExpression: string;
begin
    Result := '(A/2)*(1+erf((x-x0)/(sigma*sqrt(2))))';
end;

class function TStepPointsSet.GetCurveTypeName: string;
begin
    Result := 'Step (erf)';
end;

class function TStepPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{3fc3b846-3a44-4ace-b91b-cf84304391f9}');
end;

class function TStepPointsSet.GetExtremumMode: TExtremumMode;
begin
    //  A monotone edge, not a peak: no single extremum to search for.
    Result := MaximumsAndMinimums;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TStepPointsSet);
end.
