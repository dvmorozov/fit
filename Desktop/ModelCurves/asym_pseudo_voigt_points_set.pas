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

uses Classes, SysUtils, int_points_set, points_set, pseudo_voigt_points_set,
    curve_points_set, curve_types_singleton, special_curve_parameter,
    delta_sigma_curve_parameter, SimpMath;

type
    { Curve having asymmetrical Pseudo-Voigt form. }
    TAsymPseudoVoigtPointsSet = class(TPseudoVoigtPointsSet)
    protected
        { Difference of half-widths of left and right sides of the curve. }
        DeltaSigmaP: TDeltaSigmaCurveParameter;

        { Performs recalculation of all points of function. }
        procedure DoCalc(const Intervals: TPointsSet); override;
        
    public
        constructor Create(AOwner: TComponent); override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        function GetCurveTypeId: TCurveTypeId; override;
        class function GetCurveTypeId_: TCurveTypeId; override;
    end;

implementation

{====================== TAsymPseudoVoigtPointsSet =============================}

procedure TAsymPseudoVoigtPointsSet.DoCalc(const Intervals: TPointsSet);
var i, j: LongInt;
begin
    if Assigned(Intervals) then
    begin
        Assert((Intervals.PointsCount mod 2) = 0);
        for i := 0 to (Intervals.PointsCount shr 1) - 1 do
        begin
            for j := Trunc(Intervals.PointXCoord[i shl 1]) to
                Trunc(Intervals.PointXCoord[(i shl 1) + 1]) do
                    Points[j][2] := AsymPseudoVoigtPoint(
                        A, Sigma, Eta, x0, Points[j][1], DeltaSigmaP.Value);
        end;
    end
    else
    begin
        AsymPseudoVoigt(Points, A, Sigma, Eta, x0, DeltaSigmaP.Value);
    end;
end;

constructor TAsymPseudoVoigtPointsSet.Create(AOwner: TComponent);
begin
    inherited;

    DeltaSigmaP := TDeltaSigmaCurveParameter.Create;
    AddParameter(DeltaSigmaP);

    InitListOfVariableParameters;
end;

function TAsymPseudoVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Asym. Pseudo-Voigt';
end;

class function TAsymPseudoVoigtPointsSet.GetCurveTypeId_: TCurveTypeId;
begin
    Result := StringToGUID('{74a6ec30-a019-475d-99a3-b62c4ab03a6c}');
end;

function TAsymPseudoVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := GetCurveTypeId_;
end;

var CTS: TCurveTypesSingleton;

initialization
    CTS := TCurveTypesSingleton.Create;
    CTS.RegisterCurveType(TAsymPseudoVoigtPointsSet);
end.
