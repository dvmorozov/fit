{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having asymmetrical Pseudo-Voigt form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit asym_pseudo_voigt_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, points_set, pseudo_voigt_points_set, curve_points_set,
    curve_types_singleton, special_curve_parameter, delta_sigma_curve_parameter,
    named_points_set, SimpMath;

type
    { Curve having asymmetrical Pseudo-Voigt form. }
    TAsymPseudoVoigtPointsSet = class(TPseudoVoigtPointsSet)
    protected
        { Difference of half-widths of left and right sides of the curve. }
        FDeltaSigmaP: TDeltaSigmaCurveParameter;

        function GetDeltaSigma: double;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;

        property DeltaSigma: double read GetDeltaSigma;

    public
        constructor Create(AOwner: TComponent); override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
    end;

implementation

uses int_curve_factory;

{====================== TAsymPseudoVoigtPointsSet =============================}

procedure TAsymPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
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
                    AsymPseudoVoigtPoint(A, Sigma, Eta, x0,
                    Points[j][1], DeltaSigma);
    end
    else
        AsymPseudoVoigt(Points, A, Sigma, Eta, x0, DeltaSigma);
end;

function TAsymPseudoVoigtPointsSet.GetDeltaSigma: double;
begin
    Assert(Assigned(FDeltaSigmaP));
    Result := FDeltaSigmaP.Value;
end;

constructor TAsymPseudoVoigtPointsSet.Create(AOwner: TComponent);
var
    Count: longint;
begin
    inherited;

    FDeltaSigmaP := TDeltaSigmaCurveParameter.Create;
    AddParameter(FDeltaSigmaP);

    InitListOfVariableParameters;

    Count := FVariableParameters.Count;
    Assert(Count = 4);
end;

class function TAsymPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Asym. Pseudo-Voigt';
end;

class function TAsymPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{74a6ec30-a019-475d-99a3-b62c4ab03a6c}');
end;

class function TAsymPseudoVoigtPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TAsymPseudoVoigtPointsSet);
end.
