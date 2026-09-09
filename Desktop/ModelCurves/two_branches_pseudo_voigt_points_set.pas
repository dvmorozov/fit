// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class-container of Pseudo-Voigt curve having different form parameters for the right and left branches.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit two_branches_pseudo_voigt_points_set;

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
    { Pseudo-Voigt curve having different form parameters for
      the right and left branches. }
    T2BranchesPseudoVoigtPointsSet = class(TNamedPointsSet)
    protected
        FSigmaRightP: TSigmaCurveParameter;
        FEtaRightP: TEtaCurveParameter;
        FEtaP: TEtaCurveParameter;

        function GetSigmaRight: double;
        function GetEtaRight: double;
        function GetEta: double;

        { Performs recalculation of all points of function. }
        procedure DoCalc; override;

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

        property SigmaRight: double read GetSigmaRight;
        property EtaRight: double read GetEtaRight;
        property Eta: double read GetEta;
    end;

implementation

uses
    int_curve_factory, checks;

{=================== T2BranchesPseudoVoigtPointsSet ===========================}

procedure T2BranchesPseudoVoigtPointsSet.DoCalc;
begin
    TwoBranchesPseudoVoigt(FPoints, A, Sigma, Eta, SigmaRight, EtaRight, x0);
end;

constructor T2BranchesPseudoVoigtPointsSet.Create(AOwner: TComponent; x0: double);
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

    FEtaP := TEtaCurveParameter.Create;
    AddParameter(FEtaP);

    FSigmaRightP      := TSigmaCurveParameter.Create;
    FSigmaRightP.Name := 'sigmaright';
    AddParameter(FSigmaRightP);

    FEtaRightP      := TEtaCurveParameter.Create;
    FEtaRightP.Name := 'etaright';
    AddParameter(FEtaRightP);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 6, 'the two-branch pseudo-Voigt curve must have built exactly its six variable parameters');
end;

function T2BranchesPseudoVoigtPointsSet.GetEta: double;
begin
    CheckAssigned(FEtaP, 'the left branch shape mixing parameter eta');

    Result := FEtaP.Value;
end;

function T2BranchesPseudoVoigtPointsSet.GetEtaRight: double;
begin
    CheckAssigned(FEtaRightP, 'the right branch shape mixing parameter eta');

    Result := FEtaRightP.Value;
end;

function T2BranchesPseudoVoigtPointsSet.GetSigmaRight: double;
begin
    CheckAssigned(FSigmaRightP, 'the right branch width parameter sigma');

    Result := FSigmaRightP.Value;
end;

class function T2BranchesPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := '2 br. Pseudo-Voigt';
end;

class function T2BranchesPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{6de06c1b-e51a-48c6-b036-c81a841ec468}');
end;

class function T2BranchesPseudoVoigtPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

class function T2BranchesPseudoVoigtPointsSet.CreatePreferredAxis(
    AWaveLength: double): TArgumentAxis;
begin
    //  Coordinates of a diffraction pattern are stored in 2*Theta.
    Result := TDiffractionAngleAxis.Create(dmTwoTheta, AWaveLength);
end;

function T2BranchesPseudoVoigtPointsSet.GetCurveExpression: string;
begin
    //  Mirrors TwoBranchesPseudoVoigtPoint in SimpMath.pas: the right branch (x>=x0)
    //  uses sigmaright/etaright, the left branch uses sigma/eta.
    Result :=
        'where(x>=x0,'
        + 'A*((1-etaright)*exp(-4*log(2)*(x0-x)**2/sigmaright**2)'
        + '+etaright*(1/(1+(2*(x-x0)/sigmaright)**2))),'
        + 'A*((1-eta)*exp(-4*log(2)*(x0-x)**2/sigma**2)'
        + '+eta*(1/(1+(2*(x-x0)/sigma)**2))))';
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(T2BranchesPseudoVoigtPointsSet);
end.
