// SPDX-License-Identifier: GPL-3.0-or-later
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
        procedure DoCalc; override;

    public
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        function GetCurveExpression: string; override;
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

function TLorentzPointsSet.GetCurveExpression: string;
begin
    //  Mirrors LorentzPoint in SimpMath.pas (FWHM = sigma, area-normalized).
    Result := 'A*(1/(pi*sigma/2))*(1/(1+((x-x0)/(sigma/2))**2))';
end;

procedure TLorentzPointsSet.DoCalc;
begin
    Lorentz(FPoints, A, Sigma, x0);
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TLorentzPointsSet);
end.
