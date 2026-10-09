// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A power-law background: b0 (x / xr)^p.)

A background falling (p < 0) or rising (p > 0) as a power of x - the form small-
angle scattering takes, and a common description of a low-angle tail that
decays more slowly than an exponential.

DEFINED FOR x > 0 ONLY, and it says so (ArgumentMustBePositive): a fractional
power of a negative x is no number at all. fit_advice refuses the shape over
data that reaches zero, in words, rather than letting the fit meet a NaN.

MEASURED FROM A REFERENCE, like the other shapes: b0 is the background at xr,
the start of the interval, so it is a level the user can read off the chart
rather than the value the law would have at x = 1.
}
unit power_law_background_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, explanation, named_points_set, background_points_set;

type
    TPowerLawBackgroundPointsSet = class(TBackgroundPointsSet)
    protected
        function GetNativeExpression: string; override;
    public
        constructor Create(AOwner: TComponent); override;
        procedure SeedFromBaseline(const AX, AY: array of double); override;
        class function ArgumentMustBePositive: boolean; override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
    end;

implementation

uses
    curve_types_singleton, int_curve_factory;

constructor TPowerLawBackgroundPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    //  A LEVEL, and so never below zero: the shape's sign is b0's.
    AddLevelCoefficient('b0');
    AddCoefficient('p');
    //  A reference of 1 until seeded: 0 is a division by zero.
    AddReferenceParameter(1);
    InitListOfVariableParameters;
end;

function TPowerLawBackgroundPointsSet.GetNativeExpression: string;
begin
    Result := 'b0*(x/xr)^p';
end;

procedure TPowerLawBackgroundPointsSet.SeedFromBaseline(
    const AX, AY: array of double);
var
    LX, LY: array of double;
    C: array[0..1] of double;
    i, n: longint;
    Smallest: double;
    Found: boolean;
begin
    //  THE SMALLEST POSITIVE x, and only positive ones at all: there is no law
    //  to take the logarithm of below zero.
    Found := False;
    Smallest := 0;
    for i := 0 to High(AX) do
        if (AX[i] > 0) and ((not Found) or (AX[i] < Smallest)) then
        begin
            Smallest := AX[i];
            Found := True;
        end;
    if not Found then
    begin
        ValuesByName['b0'] := MeanOf(AY);
        Exit;
    end;
    Reference := Smallest;

    //  A STRAIGHT LINE ON LOG-LOG AXES: ln y = ln b0 + p ln(x / xr).
    SetLength(LX, Length(AX));
    SetLength(LY, Length(AX));
    n := 0;
    for i := 0 to High(AX) do
        if (AX[i] > 0) and (AY[i] > 0) then
        begin
            LX[n] := Ln(AX[i] / Smallest);
            LY[n] := Ln(AY[i]);
            Inc(n);
        end;
    SetLength(LX, n);
    SetLength(LY, n);

    if FitPolynomial(LX, LY, 0, 1, C) then
    begin
        ValuesByName['b0'] := Exp(C[0]);
        ValuesByName['p'] := C[1];
    end
    else
    begin
        ValuesByName['b0'] := MeanOf(AY);
        ValuesByName['p'] := 0;
    end;
end;

class function TPowerLawBackgroundPointsSet.ArgumentMustBePositive: boolean;
begin
    Result := True;
end;

class function TPowerLawBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Power-law background';
end;

class function TPowerLawBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{4a7f9793-f2e8-4d61-b3c1-155baa0c344b}');
end;

class function TPowerLawBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A background that changes as a power of x: level b0 at the start of ' +
        'the interval, exponent p.',
        esConvention);
    AddParagraph(Result,
        'b0 (x / xr)^p. With p below zero it falls, more slowly than an ' +
        'exponential over a long stretch; with p above zero it rises. The form ' +
        'small-angle scattering takes, and a common description of a slowly ' +
        'decaying low-angle tail.');
    AddParagraph(Result,
        'It is a background, not a peak: the model has one per fit interval, ' +
        'fitted with the peaks and never removed when the number of curves is ' +
        'reduced. It is added with Model > Background > Curve. xr is the start ' +
        'of the fit interval and is not fitted.');
    AddLimitation(Result,
        'It is defined only where x is greater than zero, so it is refused ' +
        'for data that reaches zero or below.');
    AddLimitation(Result,
        'It is not constrained to stay below the data. A background fitted ' +
        'together with broad peaks can trade intensity with them.');
    AddReference(Result,
        'NIST/SEMATECH e-Handbook of Statistical Methods',
        'section 4.1.4.2, Nonlinear Least Squares Regression',
        'https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd142.htm');
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TPowerLawBackgroundPointsSet);
end.
