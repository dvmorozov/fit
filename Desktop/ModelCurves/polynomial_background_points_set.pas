// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Polynomial backgrounds: a constant, a straight line and a parabola.)

THREE TYPES, NOT ONE WITH AN ORDER SETTING, because the order is what the user
chooses and what a project records, and a curve type is the thing both of those
already carry. The three share everything but the formula and how many
coefficients they seed.

THE QUADRATIC IS WHAT THE AUTOMATIC RUN ADDS when a model has no background
(fit_service.EnsureAutomaticBackground). A flat or sloped baseline fits its
curvature to about zero, so it is never worse than the straight line there - and
a straight line under a curved baseline leaves a curved residue that the peak
search takes for peaks. Decided with the user; see the roadmap.
}
unit polynomial_background_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation, named_points_set, background_points_set;

type
    { b0 + b1 (x - xr) + ... up to the type's Order. }
    TPolynomialBackgroundPointsSet = class(TBackgroundPointsSet)
    protected
        function GetNativeExpression: string; override;
        { 0, 1 or 2. }
        class function Order: longint; virtual; abstract;
    public
        { b0 is the level, and a lift is a change of level: the coefficients
          then describe the curve that is drawn. }
        function LevelParameterName: string; override;
        constructor Create(AOwner: TComponent); override;
        procedure SeedFromBaseline(const AX, AY: array of double); override;
    end;

    TConstantBackgroundPointsSet = class(TPolynomialBackgroundPointsSet)
    protected
        class function Order: longint; override;
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
    end;

    TLinearBackgroundPointsSet = class(TPolynomialBackgroundPointsSet)
    protected
        class function Order: longint; override;
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
    end;

    TQuadraticBackgroundPointsSet = class(TPolynomialBackgroundPointsSet)
    protected
        class function Order: longint; override;
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
    end;

{ The background the automatic run puts under the peaks of a model that has
  none: the quadratic - see the unit header for why. }
function DefaultBackgroundCurveTypeId: TCurveTypeId;

implementation

uses
    curve_types_singleton, int_curve_factory;

function DefaultBackgroundCurveTypeId: TCurveTypeId;
begin
    Result := TQuadraticBackgroundPointsSet.GetCurveTypeId;
end;

const
    CoefficientNames: array[0..2] of string = ('b0', 'b1', 'b2');

{ ---- TPolynomialBackgroundPointsSet ---- }

constructor TPolynomialBackgroundPointsSet.Create(AOwner: TComponent);
var
    i: longint;
begin
    inherited Create(AOwner);
    for i := 0 to Order do
        AddCoefficient(CoefficientNames[i]);
    AddReferenceParameter(0);
    InitListOfVariableParameters;
end;

function TPolynomialBackgroundPointsSet.GetNativeExpression: string;
begin
    case Order of
        //  "+ 0*x" so the expression is a function of x on every engine: a
        //  bare constant evaluates to one number where a formula backend
        //  expects a value per sample.
        0: Result := 'b0+0*x';
        1: Result := 'b0+b1*(x-xr)';
    else
        Result := 'b0+b1*(x-xr)+b2*(x-xr)^2';
    end;
end;

function TPolynomialBackgroundPointsSet.LevelParameterName: string;
begin
    Result := CoefficientNames[0];
end;

procedure TPolynomialBackgroundPointsSet.SeedFromBaseline(
    const AX, AY: array of double);
var
    C: array[0..2] of double;
    N, i: longint;
begin
    if Length(AX) = 0 then
        Exit;
    Reference := SmallestOf(AX);
    for i := 0 to 2 do
        C[i] := 0;
    //  THE HIGHEST ORDER THE POINTS CAN FIX, down to the plain level: two points
    //  fix a line but not a parabola, and the level of one point is still the
    //  best start there is.
    N := Order;
    while (N >= 0) and not FitPolynomial(AX, AY, Reference, N, C) do
        Dec(N);
    if N < 0 then
        C[0] := MeanOf(AY);
    for i := 0 to Order do
        ValuesByName[CoefficientNames[i]] := C[i];
end;

{ ---- the three orders ---- }

class function TConstantBackgroundPointsSet.Order: longint;
begin
    Result := 0;
end;

class function TConstantBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Constant background';
end;

class function TConstantBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{d6553d98-f367-41d7-a717-6a4ea29605f2}');
end;

class function TLinearBackgroundPointsSet.Order: longint;
begin
    Result := 1;
end;

class function TLinearBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Linear background';
end;

class function TLinearBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{a7d78941-8a02-4af6-89e3-c14810a4ec6f}');
end;

class function TQuadraticBackgroundPointsSet.Order: longint;
begin
    Result := 2;
end;

class function TQuadraticBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Quadratic background';
end;

class function TQuadraticBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{85aeddba-26a1-4d1a-a95b-db730bd76ab8}');
end;

{ ---- what each says about itself ---- }

procedure AddPolynomialCommon(var E: TExplanation);
begin
    AddParagraph(E,
        'It is a background, not a peak: the model has one per fit interval, ' +
        'under the peaks, fitted with them and never removed when the number ' +
        'of curves is reduced. It is added with Model > Background > Curve, ' +
        'and the automatic run adds a quadratic one to a model that has none.');
    AddParagraph(E,
        'xr is the start of the fit interval, so b0 is the level of the ' +
        'background there. It is set when the curve is placed and is not ' +
        'fitted.');
    AddLimitation(E,
        'A polynomial follows a baseline only as far as its order allows; a ' +
        'background that bends more than once across the interval needs a ' +
        'narrower interval, or several.');
    AddLimitation(E,
        'It is not constrained to stay below the data. A background fitted ' +
        'together with broad peaks can trade intensity with them.');
    AddReference(E,
        'NIST/SEMATECH e-Handbook of Statistical Methods',
        'section 4.1.4.1, Linear Least Squares Regression',
        'https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd141.htm');
end;

class function TConstantBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A flat background of level b0 under the peaks of each fit interval.',
        esConvention);
    AddParagraph(Result,
        'b0 everywhere. The simplest background there is: right for data whose ' +
        'baseline does not change across the interval.');
    AddPolynomialCommon(Result);
end;

class function TLinearBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A straight-line background, level b0 at the start of the interval ' +
        'and slope b1.',
        esConvention);
    AddParagraph(Result,
        'b0 + b1 (x - xr). For a baseline that rises or falls steadily ' +
        'across the fit interval.');
    AddPolynomialCommon(Result);
end;

class function TQuadraticBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A parabolic background: level b0 at the start of the interval, ' +
        'slope b1 and curvature b2.',
        esConvention);
    AddParagraph(Result,
        'b0 + b1 (x - xr) + b2 (x - xr)^2. It follows a baseline that bends ' +
        'once across the fit interval, and fits a straight or flat one with ' +
        'b2 near zero - which is why it is the one the automatic run adds.');
    AddPolynomialCommon(Result);
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TConstantBackgroundPointsSet);
    CTS.RegisterCurveType(TLinearBackgroundPointsSet);
    CTS.RegisterCurveType(TQuadraticBackgroundPointsSet);
end.
