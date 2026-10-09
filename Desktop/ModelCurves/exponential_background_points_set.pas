// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An exponential background: the falling low-angle tail of a pattern.)

b0 exp(-(x - xr) / tau). The shape of the tail that air scattering and the
direct beam leave at the low-angle end of a diffraction pattern - Data/1.dat
falls from 3377 counts at 2theta = 3 to about 800 by 13 - and of any baseline
that relaxes towards zero at a constant rate. A negative tau is a rising
exponential, and is allowed.
}
unit exponential_background_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, explanation, named_points_set, background_points_set;

type
    TExponentialBackgroundPointsSet = class(TBackgroundPointsSet)
    protected
        function GetNativeExpression: string; override;
    public
        constructor Create(AOwner: TComponent); override;
        procedure SeedFromBaseline(const AX, AY: array of double); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
    end;

implementation

uses
    curve_types_singleton, int_curve_factory;

constructor TExponentialBackgroundPointsSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    //  A LEVEL, and so never below zero: the shape's sign is b0's.
    AddLevelCoefficient('b0');
    AddCoefficient('tau');
    //  A decay length of 1 until seeded: 0 is a division by zero.
    ValuesByName['tau'] := 1;
    AddReferenceParameter(0);
    InitListOfVariableParameters;
end;

function TExponentialBackgroundPointsSet.GetNativeExpression: string;
begin
    Result := 'b0*exp(-(x-xr)/tau)';
end;

procedure TExponentialBackgroundPointsSet.SeedFromBaseline(
    const AX, AY: array of double);
var
    LX, LY: array of double;
    C: array[0..1] of double;
    i, n: longint;
    Span: double;
begin
    if Length(AX) = 0 then
        Exit;
    Reference := SmallestOf(AX);
    Span := AX[0] - Reference;
    for i := 1 to High(AX) do
        if AX[i] - Reference > Span then
            Span := AX[i] - Reference;
    if Span <= 0 then
        Span := 1;

    //  A STRAIGHT LINE THROUGH THE LOGARITHM: ln y = ln b0 - (x - xr) / tau.
    //  Only where there is a logarithm to take.
    SetLength(LX, Length(AX));
    SetLength(LY, Length(AX));
    n := 0;
    for i := 0 to High(AX) do
        if AY[i] > 0 then
        begin
            LX[n] := AX[i];
            LY[n] := Ln(AY[i]);
            Inc(n);
        end;
    SetLength(LX, n);
    SetLength(LY, n);

    if FitPolynomial(LX, LY, Reference, 1, C) then
    begin
        ValuesByName['b0'] := Exp(C[0]);
        //  FLAT DATA HAS NO DECAY LENGTH. A very long one is the same shape
        //  and still a number the fit can move from.
        if Abs(C[1]) * Span < 1e-9 then
            ValuesByName['tau'] := 1e6 * Span
        else
            ValuesByName['tau'] := -1 / C[1];
    end
    else
    begin
        //  No logarithm to fit: a flat start at the mean level.
        ValuesByName['b0'] := MeanOf(AY);
        ValuesByName['tau'] := 1e6 * Span;
    end;
end;

class function TExponentialBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Exponential background';
end;

class function TExponentialBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{1413e48f-a118-4b6f-99ac-7f708c3ce019}');
end;

class function TExponentialBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'An exponentially decaying background: level b0 at the start of the ' +
        'interval, falling by a factor e every tau.',
        esConvention);
    AddParagraph(Result,
        'b0 exp(-(x - xr) / tau). The shape of a low-angle tail - scattering ' +
        'from air and from the direct beam, strongest at the smallest angles - ' +
        'and of any baseline that relaxes at a constant rate. A negative tau ' +
        'makes it rise instead.');
    AddParagraph(Result,
        'It is a background, not a peak: the model has one per fit interval, ' +
        'fitted with the peaks and never removed when the number of curves is ' +
        'reduced. It is added with Model > Background > Curve. xr is the start ' +
        'of the fit interval and is not fitted.');
    AddLimitation(Result,
        'It decays towards zero, not towards a floor: a tail that levels off ' +
        'above zero is followed only over a stretch short enough that the ' +
        'floor does not show.');
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
    CTS.RegisterCurveType(TExponentialBackgroundPointsSet);
end.
