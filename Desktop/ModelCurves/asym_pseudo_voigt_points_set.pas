// SPDX-License-Identifier: GPL-3.0-or-later
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

uses
    Classes, curve_points_set, curve_types_singleton, delta_sigma_curve_parameter,
    named_points_set, points_set, pseudo_voigt_points_set, SimpMath,
    special_curve_parameter, SysUtils;

type
    { Curve having asymmetrical Pseudo-Voigt form. }
    TAsymPseudoVoigtPointsSet = class(TPseudoVoigtPointsSet)
    protected
        { Difference of half-widths of left and right sides of the curve. }
        FDeltaSigmaP: TDeltaSigmaCurveParameter;

        function GetDeltaSigma: double;

        { Performs recalculation of all points of function. }
        procedure DoCalc; override;

        property DeltaSigma: double read GetDeltaSigma;

    public
        constructor Create(AOwner: TComponent; x0: double); overload;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
        function GetCurveExpression: string; override;
    end;

implementation

uses
    int_curve_factory, checks;

{====================== TAsymPseudoVoigtPointsSet =============================}

procedure TAsymPseudoVoigtPointsSet.DoCalc;
begin
    AsymPseudoVoigt(FPoints, A, Sigma, Eta, x0, DeltaSigma);
end;

function TAsymPseudoVoigtPointsSet.GetDeltaSigma: double;
begin
    CheckAssigned(FDeltaSigmaP, 'the asymmetric pseudo-Voigt width difference parameter');
    Result := FDeltaSigmaP.Value;
end;

constructor TAsymPseudoVoigtPointsSet.Create(AOwner: TComponent; x0: double);
var
    Count: longint;
begin
    inherited;

    FDeltaSigmaP := TDeltaSigmaCurveParameter.Create;
    AddParameter(FDeltaSigmaP);

    InitListOfVariableParameters;

    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the asymmetric pseudo-Voigt curve must have built exactly its four variable parameters');
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

function TAsymPseudoVoigtPointsSet.GetCurveExpression: string;
begin
    //  Mirrors AsymPseudoVoigtPoint in SimpMath.pas: right branch (x>=x0) widens by
    //  +deltasigma, left branch narrows by -deltasigma; not area-normalized.
    Result :=
        'where(x>=x0,'
        + 'A*((1-eta)*exp(-4*log(2)*(x0-x)**2/(sigma+deltasigma)**2)'
        + '+eta*(1/(1+(2*(x-x0)/(sigma+deltasigma))**2))),'
        + 'A*((1-eta)*exp(-4*log(2)*(x0-x)**2/(sigma-deltasigma)**2)'
        + '+eta*(1/(1+(2*(x-x0)/(sigma-deltasigma))**2))))';
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TAsymPseudoVoigtPointsSet);
end.
