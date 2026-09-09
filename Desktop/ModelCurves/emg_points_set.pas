// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of exponentially modified Gaussian curve.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit emg_points_set;

{$mode delphi}

interface

uses
    amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, named_points_set, position_curve_parameter,
    sigma_curve_parameter, special_curve_parameter, tau_curve_parameter,
    SysUtils;

type
    { Exponentially modified Gaussian - a Gaussian (sigma) convolved with a
      one-sided exponential (relaxation time tau), the standard skewed peak of
      chromatography. A is the area. Written with the scaled complementary error
      function erfcx so it stays finite as tau -> 0, where it becomes the plain
      Gaussian. }
    TEmgPointsSet = class(TFormulaPointsSet)
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

{============================ TEmgPointsSet ==================================}

constructor TEmgPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TTauCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the exponentially modified Gaussian curve must have built exactly its four variable parameters');
end;

function TEmgPointsSet.GetNativeExpression: string;
begin
    //  emg(u, sigma, tau) is the area-normalised EMG, provided by both engines
    //  (special_functions.EmgProfile / the sidecar's emg) via a numerically stable
    //  branch-wise evaluation. -> Gaussian as tau -> 0.
    Result := 'A*emg(x-x0,sigma,tau)';
end;

class function TEmgPointsSet.GetCurveTypeName: string;
begin
    Result := 'Exponentially Modified Gaussian';
end;

class function TEmgPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{4d320bbe-1000-4d70-a860-3ac0d7076ff5}');
end;

class function TEmgPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TEmgPointsSet);
end.
