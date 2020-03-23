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
    Classes, SysUtils, points_set, named_points_set, curve_points_set,
    curve_types_singleton, special_curve_parameter, amplitude_curve_parameter,
    sigma_curve_parameter, position_curve_parameter, eta_curve_parameter,
    SimpMath;

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
        procedure DoCalc(const Intervals: TPointsSet); override;

    public
        constructor Create(AOwner: TComponent); override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;

        property SigmaRight: double read GetSigmaRight;
        property EtaRight: double read GetEtaRight;
        property Eta: double read GetEta;
    end;

implementation

uses int_curve_factory;

{=================== T2BranchesPseudoVoigtPointsSet ===========================}

procedure T2BranchesPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var
    i, j: longint;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                Points[j][2] :=
                    TwoBranchesPseudoVoigtPoint(A, Sigma, Eta,
                    SigmaRight, EtaRight, x0, Points[j][1]);
    end
    else
        TwoBranchesPseudoVoigt(Points, A, Sigma, Eta, SigmaRight, EtaRight, x0);
end;

constructor T2BranchesPseudoVoigtPointsSet.Create(AOwner: TComponent);
var
    Parameter: TSpecialCurveParameter;
    Count:     longint;
begin
    inherited;

    Parameter := TAmplitudeCurveParameter.Create;
    AddParameter(Parameter);

    Parameter := TPositionCurveParameter.Create(Self);
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
    Assert(Count = 6);
end;

function T2BranchesPseudoVoigtPointsSet.GetEta: double;
begin
    Assert(Assigned(FEtaP));
    Result := FEtaP.Value;
end;

function T2BranchesPseudoVoigtPointsSet.GetEtaRight: double;
begin
    Assert(Assigned(FEtaRightP));
    Result := FEtaRightP.Value;
end;

function T2BranchesPseudoVoigtPointsSet.GetSigmaRight: double;
begin
    Assert(Assigned(FSigmaRightP));
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

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(T2BranchesPseudoVoigtPointsSet);
end.
