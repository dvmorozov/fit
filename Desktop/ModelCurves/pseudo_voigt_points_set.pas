// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Pseudo-Voigt form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit pseudo_voigt_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    amplitude_curve_parameter, argument_axis, Classes, curve_points_set,
    curve_types_singleton, eta_curve_parameter, named_points_set, points_set, position_curve_parameter,
    sigma_curve_parameter, SimpMath, special_curve_parameter, SysUtils;

type
    { Function having Pseudo-Voigt form. }
    TPseudoVoigtPointsSet = class(TNamedPointsSet)
    protected
        { Relative weights of gaussian and lorentzian. }
        FEtaP: TEtaCurveParameter;

        function GetEta: double;

        { Performs recalculation of all points of function. }
        procedure DoCalc; override;

        property Eta: double read GetEta;

    public
        constructor Create(AOwner: TComponent; x0: double); overload;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        { A diffraction lineshape: its argument is the scattering angle. }
        class function CreatePreferredAxis(AWaveLength: double): TArgumentAxis;
            override;
        function GetCurveExpression: string; override;
    end;

implementation

uses
    int_curve_factory, checks;

{======================== TPseudoVoigtPointsSet ===============================}

procedure TPseudoVoigtPointsSet.DoCalc;
begin
    PseudoVoigt(FPoints, A, Sigma, Eta, x0);
end;

constructor TPseudoVoigtPointsSet.Create(AOwner: TComponent; x0: double);
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
    Parameter.Type_ := Shared;          //  common parameter for all instances
    AddParameter(Parameter);

    FEtaP := TEtaCurveParameter.Create;
    AddParameter(FEtaP);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 3, 'the pseudo-Voigt curve must have built exactly its three variable parameters');
end;

function TPseudoVoigtPointsSet.GetEta: double;
begin
    CheckAssigned(FEtaP, 'the pseudo-Voigt shape mixing parameter eta');
    Result := FEtaP.Value;
end;

class function TPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Pseudo-Voigt';
end;

class function TPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{9f27dc7c-970f-4dac-88cd-f5fb3400d38d}');
end;

class function TPseudoVoigtPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

class function TPseudoVoigtPointsSet.CreatePreferredAxis(
    AWaveLength: double): TArgumentAxis;
begin
    //  Coordinates of a diffraction pattern are stored in 2*Theta.
    Result := TDiffractionAngleAxis.Create(dmTwoTheta, AWaveLength);
end;

function TPseudoVoigtPointsSet.GetCurveExpression: string;
begin
    //  Mirrors PseudoVoigtPoint in SimpMath.pas: (1-eta) Gaussian + eta Lorentzian,
    //  each FWHM-normalized.
    Result :=
        'A*((1-eta)*(2*sqrt(log(2))/(sigma*sqrt(pi))*exp(-4*log(2)*(x0-x)**2/sigma**2))'
        + '+eta*((2/(pi*sigma))*(1/(1+(2*(x-x0)/sigma)**2))))';
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TPseudoVoigtPointsSet);
end.
