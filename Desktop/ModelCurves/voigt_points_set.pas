// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having true Voigt form.)

Copyright (C) Dmitry Morozov
}
unit voigt_points_set;

{$mode delphi}

interface

uses
    explanation, amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, gamma_curve_parameter, named_points_set,
    position_curve_parameter, sigma_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Curve having true Voigt form - a Gaussian (std sigma) convolved with a
      Lorentzian (HWHM gamma), evaluated through the Faddeeva function. A is the
      area. Distinct from the existing Pseudo-Voigt (a weighted sum
      approximation): this is the exact convolution. Reduces to a Gaussian as
      gamma -> 0 and to a Lorentzian as sigma -> 0. }
    TVoigtPointsSet = class(TFormulaPointsSet)
    protected
        function GetNativeExpression: string; override;

    public
        { How wide the peak is at half maximum per unit of AParameterName
          (TCurvePointsSet.FullWidthPerUnit). }
        function FullWidthPerUnit(const AParameterName: string): double; override;
        constructor Create(AOwner: TComponent; x0: double); overload;
        class function GetCurveTypeName: string; override;
        { Overrides method defined in TNamedPointsSet. }
        class function Explanation: TExplanation; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function GetExtremumMode: TExtremumMode; override;
    end;

{ The widest a Voigt's Gaussian part may be at half maximum, given its
  Lorentzian part is ALorentzFull wide, for the profile to be no wider than
  AExtent (Olivero and Longbothum). 0 when the Lorentzian alone fills it. }
function VoigtLargestGaussFull(const AExtent, ALorentzFull: double): double;
{ The widest its Lorentzian part may be, given the Gaussian is AGaussFull wide. }
function VoigtLargestLorentzFull(const AExtent, AGaussFull: double): double;

implementation

uses
    int_curve_factory, checks, width_curve_parameter, Math, SimpMath;

{=========================== TVoigtPointsSet =================================}

constructor TVoigtPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TGammaCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the Voigt curve must have built exactly its four variable parameters');
end;

function TVoigtPointsSet.GetNativeExpression: string;
begin
    //  voigt(u, sigma, gamma) is the area-normalised Voigt profile (Faddeeva),
    //  provided by both engines (native_math_expr / scipy.special).
    Result := 'A*voigt(x-x0,sigma,gamma)';
end;

class function TVoigtPointsSet.GetCurveTypeName: string;
begin
    Result := 'Voigt';
end;

class function TVoigtPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{eeed2ec3-d036-473e-81e1-8e40943d8158}');
end;

class function TVoigtPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

const
    { J. J. Olivero and R. L. Longbothum, Empirical fits to the Voigt line
      width: a brief review, J. Quant. Spectrosc. Radiat. Transfer 17 (1977)
      233: fV = 0.5346 fL + sqrt(0.2166 fL^2 + fG^2), within 0.02 %. }
    OLIVERO_A = 0.5346;
    OLIVERO_B = 0.2166;

function VoigtLargestGaussFull(const AExtent, ALorentzFull: double): double;
var
    Room: double;
begin
    //  fV = E solved for fG: fG^2 = (E - a fL)^2 - b fL^2. None left when the
    //  Lorentzian alone already fills the extent - Room falls through zero at
    //  fL = E, by rounding either side of it - and none past it either: the
    //  quadratic turns positive again beyond fL = E / (a - sqrt b), about 14 E,
    //  which is not room but the other root.
    Room := Sqr(AExtent - OLIVERO_A * ALorentzFull) - OLIVERO_B * Sqr(ALorentzFull);
    if (Room > 0) and (AExtent > OLIVERO_A * ALorentzFull) then
        Result := Sqrt(Room)
    else
        Result := 0;
end;

function VoigtLargestLorentzFull(const AExtent, AGaussFull: double): double;
var
    C, Disc: double;
begin
    //  fV = E solved for fL: (a^2 - b) fL^2 - 2 a E fL + (E^2 - fG^2) = 0, the
    //  smaller root - the one that is fL = E for a pure Lorentzian, and 0 when
    //  the Gaussian alone fills E. sigma is told before gamma, so fG never
    //  exceeds E here; were it to, the root would be below 0 and the caller's
    //  floor at TINY would hold gamma all the same.
    C := Sqr(OLIVERO_A) - OLIVERO_B;
    Disc := Sqr(OLIVERO_A * AExtent) - C * (Sqr(AExtent) - Sqr(AGaussFull));
    Result := (OLIVERO_A * AExtent - Sqrt(Disc)) / C;
end;

function TVoigtPointsSet.FullWidthPerUnit(const AParameterName: string): double;
var
    Extent, Cap: double;
begin
    //  THE TWO WIDTHS COMBINE, so each is held by what the OTHER leaves room
    //  for: the cap on sigma is the largest that keeps the Voigt's full width
    //  at half maximum within the interval at the current gamma, and the cap on
    //  gamma likewise at the current sigma. The parameters are told one after
    //  the other before every calculation, so the second is held at what the
    //  first now is and the pair fits. Answered as the per-unit width the
    //  window divides its extent by - extent / cap.
    //
    //  Each on its own - the other at zero - this is the plain conversion:
    //  sigma a standard deviation (2.3548), gamma a half width (2).
    Extent := Abs(FParamWindow.LastX - FParamWindow.FirstX);
    if SameText(AParameterName, 'sigma') then
    begin
        Result := FULL_WIDTH_PER_STANDARD_DEVIATION;
        if FParamWindowSet and (Extent > 0) then
        begin
            Cap := VoigtLargestGaussFull(Extent, 2 * ValuesByName['gamma']) /
                FULL_WIDTH_PER_STANDARD_DEVIATION;
            //  No room left: the narrowest a width may be.
            Result := Extent / Max(Cap, TINY);
        end;
    end
    else if SameText(AParameterName, 'gamma') then
    begin
        Result := 2;
        if FParamWindowSet and (Extent > 0) then
        begin
            Cap := VoigtLargestLorentzFull(Extent,
                FULL_WIDTH_PER_STANDARD_DEVIATION * ValuesByName['sigma']) / 2;
            Result := Extent / Max(Cap, TINY);
        end;
    end
    else
        Result := inherited FullWidthPerUnit(AParameterName);
end;

var
    CTS: ICurveFactory;

class function TVoigtPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'The convolution of a Gaussian of width sigma with a Lorentzian ' +
        'of half-width gamma, scaled by A.',
        esCanonical);
    AddParagraph(Result,
        'The exact profile of a peak broadened by two independent ' +
        'mechanisms at once: a Gaussian one (instrument, strain) with ' +
        'standard deviation sigma and a Lorentzian one (lifetime, size) ' +
        'with half-width gamma.');
    AddParagraph(Result,
        'It is what Pseudo-Voigt approximates; use it when the two widths ' +
        'matter separately.');
    Result.Quote := 'U(x, t) + i V(x, t) = sqrt(pi / (4 t)) exp(z^2) erfc(z), z = (1 ' +
        '- i x) / (2 sqrt(t))';
    AddLimitation(Result,
        'It is more expensive to evaluate than Pseudo-Voigt.');
    AddLimitation(Result,
        'When one mechanism dominates, sigma and gamma become strongly ' +
        'correlated and the smaller one is poorly determined.');
    AddReference(Result,
        'NIST Digital Library of Mathematical Functions',
        'section 7.19, Voigt Functions',
        'https://dlmf.nist.gov/7.19');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TVoigtPointsSet);
end.
