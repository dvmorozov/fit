// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of class of exponentially modified Gaussian curve.)

Copyright (C) Dmitry Morozov
}
unit emg_points_set;

{$mode delphi}

interface

uses
    explanation, amplitude_curve_parameter, Classes, curve_types_singleton,
    formula_points_set, named_points_set, position_curve_parameter,
    sigma_curve_parameter, special_curve_parameter, tau_curve_parameter,
    SysUtils;

type
    { Exponentially modified Gaussian - a Gaussian (sigma) convolved with a
      one-sided exponential (relaxation time tau), the standard skewed peak of
      chromatography. A is the area. Written with the scaled complementary error
      function erfcx so it stays finite as tau -> 0, where it becomes the plain
      Gaussian. }
    TEmgPointsSet = class(TFormulaPointsSet)
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

implementation

uses
    int_curve_factory, checks, width_curve_parameter, Math;

{============================ TEmgPointsSet ==================================}

constructor TEmgPointsSet.Create(AOwner: TComponent; x0: double);
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

    Parameter := TTauCurveParameter.Create;
    AddParameter(Parameter);

    InitListOfVariableParameters;
    Count := FVariableParameters.Count;
    CheckThat(Count = 4, 'the exponentially modified Gaussian curve must have built exactly its four variable parameters');
end;

function TEmgPointsSet.GetNativeExpression: string;
begin
    //  emg(u, sigma, tau) is the area-normalised EMG, provided by both engines
    //  (special_functions.EmgProfile / the sidecar's emg) via a numerically stable
    //  branch-wise evaluation. -> Gaussian as tau -> 0.
    Result := 'A*emg(x-x0,sigma,tau)';
end;

class function TEmgPointsSet.GetCurveTypeName: string;
begin
    Result := 'Exponentially Modified Gaussian';
end;

class function TEmgPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{4d320bbe-1000-4d70-a860-3ac0d7076ff5}');
end;

class function TEmgPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := OnlyMaximums;
end;

function TEmgPointsSet.FullWidthPerUnit(const AParameterName: string): double;
begin
    //  sigma is the Gaussian's standard deviation; the exponential tail falls
    //  to half its height ln 2 of tau from where it starts.
    if SameText(AParameterName, 'sigma') then
        Result := FULL_WIDTH_PER_STANDARD_DEVIATION
    else if SameText(AParameterName, 'tau') then
        Result := Ln(2)
    else
        Result := inherited FullWidthPerUnit(AParameterName);
end;

var
    CTS: ICurveFactory;

class function TEmgPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A Gaussian convolved with a one-sided exponential decay, giving ' +
        'a peak with a tail of time constant tau.',
        esCanonical);
    AddParagraph(Result,
        'A Gaussian of standard deviation sigma centred at x0, convolved ' +
        'with an exponential of time constant tau, and scaled by A.');
    AddParagraph(Result,
        'It is the classic model of chromatographic peak tailing, where ' +
        'the detector or column adds an exponential lag to a Gaussian ' +
        'band.');
    Result.Quote := 'f(t) = Gaussian(t; t_G, sigma) convolved with exp(-t / tau) / ' +
        'tau';
    AddLimitation(Result,
        'The tail is on one side only; a peak fronting on the other side ' +
        'needs a different model.');
    AddLimitation(Result,
        'x0 is the centre of the underlying Gaussian, not the position of ' +
        'the maximum, which moves toward the tail as tau grows.');
    AddReference(Result,
        'E. Grushka, Characterization of exponentially modified Gaussian ' +
        'peaks in chromatography, Anal. Chem. 44 (1972)',
        'pp. 1733-1738',
        '');
end;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TEmgPointsSet);
end.
