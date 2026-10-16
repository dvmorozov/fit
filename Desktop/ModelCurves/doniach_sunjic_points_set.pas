// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of curve having Doniach-Sunjic form.)

Copyright (C) Dmitry Morozov
}
unit doniach_sunjic_points_set;

{$mode delphi}

interface

uses
    explanation, amplitude_curve_parameter, asymmetry_curve_parameter, Classes,
    curve_types_singleton, formula_points_set, named_points_set,
    position_curve_parameter, sigma_curve_parameter, special_curve_parameter,
    SysUtils;

type
    { Curve having Doniach-Sunjic form - the asymmetric core-level lineshape of
      X-ray photoelectron spectroscopy. sigma is the width, alpha the singularity
      (asymmetry) index; A is a linear scale. At alpha = 0 it is a Lorentzian with
      sigma as its half-width. }
    TDoniachSunjicPointsSet = class(TFormulaPointsSet)
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

{ The full width at half maximum of a Doniach-Sunjic line of unit sigma and
  asymmetry AAlpha. 2 at AAlpha = 0, where the line is a Lorentzian of half
  width sigma; wider as AAlpha grows (2.27 at 0.1, 3.35 at 0.3), with no closed
  form. Measured from the line: its maximum is at cot(pi / (2 - AAlpha)), and
  each half-height crossing is bracketed by doubling steps and bisected. 0 for
  an AAlpha at which the line has no maximum to measure (1 and above). }
function DoniachSunjicFullWidth(const AAlpha: double): double;

implementation

uses
    int_curve_factory, checks, width_curve_parameter, Math;

{======================= TDoniachSunjicPointsSet =============================}

constructor TDoniachSunjicPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TAsymmetryCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the Doniach-Sunjic curve must have built exactly its four variable parameters');
end;

function TDoniachSunjicPointsSet.GetNativeExpression: string;
begin
    Result := 'A*cos(pi*alpha/2+(1-alpha)*arctan((x-x0)/sigma))' +
        '/(sigma^2+(x-x0)^2)^((1-alpha)/2)';
end;

class function TDoniachSunjicPointsSet.GetCurveTypeName: string;
begin
    Result := 'Doniach-Sunjic';
end;

class function TDoniachSunjicPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{ec663a56-0e89-4bc3-91fd-f243aadb253e}');
end;

class function TDoniachSunjicPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

function DoniachSunjicFullWidth(const AAlpha: double): double;

    function Line(const T: double): double;
    begin
        Result := Cos(Pi * AAlpha / 2 + (1 - AAlpha) * ArcTan(T)) /
            Power(1 + T * T, (1 - AAlpha) / 2);
    end;

    { Where the line falls to AHalf, walking from the maximum at ATop in the
      direction ASign. }
    function Crossing(const ATop, AHalf, ASign: double): double;
    var
        Inside, Outside, Mid: double;
        Step: double;
        i: longint;
    begin
        Inside := ATop;
        Step := 0.5;
        Outside := ATop + ASign * Step;
        while Line(Outside) > AHalf do
        begin
            Inside := Outside;
            Step := Step * 2;
            Outside := ATop + ASign * Step;
        end;
        for i := 1 to 60 do
        begin
            Mid := (Inside + Outside) / 2;
            if Line(Mid) > AHalf then
                Inside := Mid
            else
                Outside := Mid;
        end;
        Result := (Inside + Outside) / 2;
    end;

var
    Top, Half: double;
begin
    if (AAlpha < 0) or (AAlpha >= 1) then
        Exit(0);
    Top := Cot(Pi / (2 - AAlpha));
    Half := Line(Top) / 2;
    Result := Crossing(Top, Half, 1) - Crossing(Top, Half, -1);
end;

function TDoniachSunjicPointsSet.FullWidthPerUnit(const AParameterName: string): double;
begin
    //  A Lorentzian of half width sigma at alpha = 0, and wider as the
    //  asymmetry grows - with no closed form, so measured from the line
    //  itself (DoniachSunjicFullWidth) at the alpha the fit has reached.
    if SameText(AParameterName, 'sigma') then
        Result := DoniachSunjicFullWidth(ValuesByName['alpha'])
    else
        Result := inherited FullWidthPerUnit(AParameterName);
end;

var
    CTS: ICurveFactory;

class function TDoniachSunjicPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'An asymmetric photoemission line of metals: a Lorentzian of ' +
        'half-width sigma skewed by the singularity index alpha.',
        esCanonical);
    AddParagraph(Result,
        'A cos(pi alpha / 2 + (1 - alpha) arctan((x - x0) / sigma)) / ' +
        '(sigma^2 + (x - x0)^2)^((1 - alpha) / 2). alpha = 0 gives a ' +
        'Lorentzian; larger alpha gives a heavier tail on one side.');
    AddParagraph(Result,
        'It describes core-level X-ray photoemission lines of metals, ' +
        'where screening by conduction electrons makes the line ' +
        'asymmetric.');
    Result.Quote := 'I(E) = cos(pi alpha / 2 + (1 - alpha) arctan(E / gamma)) / (E^2 ' +
        '+ gamma^2)^((1 - alpha) / 2)';
    AddLimitation(Result,
        'For alpha > 0 the integral of the line diverges, so a peak area ' +
        'is not defined without a cut-off.');
    AddLimitation(Result,
        'In practice the line is convolved with a Gaussian instrument ' +
        'function; that convolution is not part of this type.');
    AddReference(Result,
        'S. Doniach and M. Sunjic, Many-electron singularity in X-ray ' +
        'photoemission and X-ray line spectra from metals, J. Phys. C 3 ' +
        '(1970)',
        'pp. 285-291',
        '');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TDoniachSunjicPointsSet);
end.
