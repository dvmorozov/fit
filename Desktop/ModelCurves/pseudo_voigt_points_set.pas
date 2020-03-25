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
    amplitude_curve_parameter, Classes, curve_points_set, curve_types_singleton,
    eta_curve_parameter, named_points_set, points_set, position_curve_parameter,
    sigma_curve_parameter, SimpMath, special_curve_parameter, SysUtils;

type
    { Function having Pseudo-Voigt form. }
    TPseudoVoigtPointsSet = class(TNamedPointsSet)
    protected
        { Relative weights of gaussian and lorentzian. }
        FEtaP: TEtaCurveParameter;

        function GetEta: double;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;

        property Eta: double read GetEta;

    public
        constructor Create(AOwner: TComponent); override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
    end;

implementation

uses
    int_curve_factory;

{======================== TPseudoVoigtPointsSet ===============================}

procedure TPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var
    i, j: longint;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                Points[j][2] := PseudoVoigtPoint(A, Sigma, Eta, x0, Points[j][1]);
    end
    else
        PseudoVoigt(Points, A, Sigma, Eta, x0);
end;

constructor TPseudoVoigtPointsSet.Create(AOwner: TComponent);
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
    Parameter.Type_ := Shared;          //  common parameter for all instances
    AddParameter(Parameter);

    FEtaP := TEtaCurveParameter.Create;
    AddParameter(FEtaP);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    Assert(Count = 3);
end;

function TPseudoVoigtPointsSet.GetEta: double;
begin
    Assert(Assigned(FEtaP));
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

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TPseudoVoigtPointsSet);
end.
