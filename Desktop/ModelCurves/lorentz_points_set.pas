{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Lorentz form.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit lorentz_points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    curve_types_singleton, gauss_points_set, named_points_set, points_set,
    SimpMath, SysUtils;

type
    { Curve class having Lorentz form. }
    TLorentzPointsSet = class(TGaussPointsSet)
    protected
        procedure DoCalc(const Bounds: TPointsSet); override;

    public
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
    end;

implementation

uses
    int_curve_factory;

{========================== TLorentzPointsSet =================================}

class function TLorentzPointsSet.GetCurveTypeName: string;
begin
    Result := 'Lorentzian';
end;

class function TLorentzPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{7ca6fdaf-95b7-4d84-bcba-130c828407cc}');
end;

class function TLorentzPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

procedure TLorentzPointsSet.DoCalc(const Bounds: TPointsSet);
var
    i, j: longint;
begin
    if Assigned(Bounds) then
    begin
        Assert((Bounds.PointsCount mod 2) = 0);
        for i := 0 to (Bounds.PointsCount shr 1) - 1 do
            for j := Trunc(Bounds.PointXCoord[i shl 1]) to
                Trunc(Bounds.PointXCoord[(i shl 1) + 1]) do
                FPoints[j][2] := LorentzPoint(A, Sigma, x0, FPoints[j][1])
    end
    else
    begin
        Lorentz(FPoints, A, Sigma, x0);
    end;
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TLorentzPointsSet);
end.
