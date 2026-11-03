// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A Chebyshev background: a smooth curve under a whole pattern.)

c0 T0(t) + c1 T1(t) + ... + c[n-1] T[n-1](t), where t = 2 (x - xl) / (xh - xl)
- 1 runs from -1 at the first sample of the data to 1 at the last, and T_k is
the Chebyshev polynomial of the first kind of degree k.

WHY THIS SHAPE. The background is one curve under every fit interval, so it
must follow a baseline across the whole pattern, which bends more than once -
more than a quadratic can. A polynomial of higher degree in powers of x does
it badly: its coefficients are nearly dependent and the fit wanders among
them. In the Chebyshev basis each coefficient shapes the curve nearly
independently of the others over [-1, 1], which is why Rietveld programs use
it for the background.

HOW MANY TERMS, n, is a value of the curve - the parameter "terms" - and not a
curve type apiece. The curve holds room for MaxTermCount (64) coefficients and
uses the first n; the others are zero and held out of the fit, so the minimiser
varies n numbers, a model counts n parameters, and a project records the curve
as every other, c0 to c63 and the count. A project saved when there were six
terms opens with six: its coefficients are the same names, and the ones beyond
are zero.

SIX UNTIL SOMETHING CHANGES THEM. A curve starts with six terms: a polynomial of
degree five turns at most four times - enough for a baseline across a pattern,
few enough that the background does not model peaks. The search for the fit
intervals clips a curve of twelve terms, whoever asks (TFitService.
ClipDataInForce); the automatic run starts its background at twelve, fits it
to the floor of the data and scores more and fewer terms for as long as the
score against the floor falls (TFitService.FitBackgroundFloor). That is what lets it follow a hump twenty
degrees wide (an amorphous halo), which six terms spread over a pattern of a
hundred and fifty cannot: a feature of width w across a range L needs a degree
near pi L / w. Sixty-four is this software's ceiling, not a measured optimum
(it was thirty-two until 2026-11-02, when Tb2Fe17's sharp rise at the start of
the pattern still wanted more).

THE NATIVE ENGINE DOES NOT EVALUATE THE EXPRESSION. It sums the terms by
Clenshaw's recurrence (DoCalc), which is stable at any degree. The formula
given to a backend (GetNativeExpression) is the same polynomial written out in
powers of t, which has no variables to hold a recurrence in; its coefficients
grow as 2^(n-1), so past about twenty-five terms it loses digits to
cancellation (about 1e-7 of the coefficients at thirty-two, and nothing worth
having beyond fifty): a backend is given a curve it cannot evaluate well at
the highest degrees, and the native engine is the one that is right. The two
are compared in a test up to thirty-one terms.
}
unit chebyshev_background_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, explanation, named_points_set, background_points_set,
    special_curve_parameter;

type
    TChebyshevBackgroundPointsSet = class(TBackgroundPointsSet)
    private
        { The parameter "terms" and the parameter list it was found in. A copy
          of the curve replaces its list (SetParameters) and frees the old one,
          so the pointer is only trusted while the list is the one it came
          from; and it is asked for while the new list is being walked, which
          is why it is not found once, at a hook. }
        FTermsP: TSpecialCurveParameter;
        FTermsIn: TObject;
        function TermsParameter: TSpecialCurveParameter;
    protected
        procedure DoCalc; override;
        { The terms in use, which is what decides the variable parameters. }
        function VariableSetStamp: longint; override;
        function ParameterIsInUse(AParameter: TSpecialCurveParameter): boolean;
            override;
        function GetNativeExpression: string; override;
    public
        constructor Create(AOwner: TComponent); override;
        { c0 is the level: T0 is 1 everywhere, so a lift is a change of c0. }
        function LevelParameterName: string; override;
        { The range [xl, xh] is the curve's window - the data it is fitted
          over - and the coefficients the least-squares fit through AX, AY in
          as many terms as the points can fix. }
        procedure SeedFromBaseline(const AX, AY: array of double); override;
        { The terms in use, between 1 and CHEBYSHEV_MAX_TERMS: the value of the
          parameter "terms", read whatever way it was written. }
        function TermCount: longint; override;
        function MinTermCount: longint; override;
        function MaxTermCount: longint; override;
        procedure SetTermCount(ATermCount: longint); override;
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
        class function Explanation: TExplanation; override;
        { The background a model of peaks has unless another shape is chosen
          (decided with the user, 2026-10-31): six terms follow a curved
          floor a parabola cannot, and the Rietveld programs start there. }
        class function IsDefaultBackground: boolean; override;
    end;

const
    { The terms a curve starts with. }
    CHEBYSHEV_DEFAULT_TERMS = 6;
    { The terms it holds room for: degree 63 at most. }
    CHEBYSHEV_MAX_TERMS = 64;

{ T_AK(AT), the Chebyshev polynomial of the first kind, by its recurrence. }
function ChebyshevT(AK: longint; AT: double): double;

implementation

uses
    curve_types_singleton, int_curve_factory;

const
    TermsName = 'terms';

function CoefficientName(AK: longint): string;
begin
    Result := 'c' + IntToStr(AK);
end;

function ChebyshevT(AK: longint; AT: double): double;
var
    Previous, Next: double;
    k: longint;
begin
    if AK = 0 then
        Exit(1);
    Previous := 1;
    Result := AT;
    for k := 2 to AK do
    begin
        Next := 2 * AT * Result - Previous;
        Previous := Result;
        Result := Next;
    end;
end;

constructor TChebyshevBackgroundPointsSet.Create(AOwner: TComponent);
var
    i: longint;
begin
    inherited Create(AOwner);
    for i := 0 to CHEBYSHEV_MAX_TERMS - 1 do
        AddCoefficient(CoefficientName(i));
    //  [0, 1] until seeded: an empty range is a division by zero.
    AddCalculatedParameter('xl', 0);
    AddCalculatedParameter('xh', 1);
    //  A value, not a type apiece: see the unit header.
    AddCalculatedParameter(TermsName, CHEBYSHEV_DEFAULT_TERMS);
    InitListOfVariableParameters;
end;

function TChebyshevBackgroundPointsSet.TermsParameter: TSpecialCurveParameter;
begin
    if FTermsIn <> TObject(FParams) then
    begin
        FTermsP := FParams.FindByName(TermsName);
        //  Not remembered before it exists: the curve is being built.
        if FTermsP <> nil then
            FTermsIn := FParams
        else
            FTermsIn := nil;
    end;
    Result := FTermsP;
end;

function TChebyshevBackgroundPointsSet.TermCount: longint;
var
    P: TSpecialCurveParameter;
begin
    P := TermsParameter;
    if P = nil then
        Exit(CHEBYSHEV_DEFAULT_TERMS);
    //  A NaN or an absurd value is a count too: kept in what the curve holds.
    if IsNan(P.Value) then
        Exit(CHEBYSHEV_DEFAULT_TERMS);
    Result := Max(MinTermCount, Min(MaxTermCount,
        Round(Max(-1.0, Min(1000.0, P.Value)))));
end;

function TChebyshevBackgroundPointsSet.MinTermCount: longint;
begin
    Result := 1;
end;

function TChebyshevBackgroundPointsSet.MaxTermCount: longint;
begin
    Result := CHEBYSHEV_MAX_TERMS;
end;

procedure TChebyshevBackgroundPointsSet.SetTermCount(ATermCount: longint);
var
    i, N: longint;
begin
    N := Max(MinTermCount, Min(MaxTermCount, ATermCount));
    ValuesByName[TermsName] := N;
    //  What is no longer in use is zero, so a project records no coefficient
    //  nobody fits.
    for i := N to CHEBYSHEV_MAX_TERMS - 1 do
        ValuesByName[CoefficientName(i)] := 0;
end;

function TChebyshevBackgroundPointsSet.VariableSetStamp: longint;
begin
    Result := TermCount;
end;

function TChebyshevBackgroundPointsSet.ParameterIsInUse(
    AParameter: TSpecialCurveParameter): boolean;
var
    K: longint;
begin
    Result := True;
    if (Length(AParameter.Name) < 2) or (AParameter.Name[1] <> 'c') then
        Exit;
    K := StrToIntDef(Copy(AParameter.Name, 2, 3), -1);
    if K >= 0 then
        Result := K < TermCount;
end;

procedure TChebyshevBackgroundPointsSet.DoCalc;
var
    C: array[0..CHEBYSHEV_MAX_TERMS - 1] of double;
    N, k, j: longint;
    Lo, Hi, T, B0, B1, B2: double;
begin
    N := TermCount;
    for k := 0 to N - 1 do
        C[k] := ValuesByName[CoefficientName(k)];
    Lo := ValuesByName['xl'];
    Hi := ValuesByName['xh'];
    //  CLENSHAW'S RECURRENCE, over the terms in use only: stable at any degree,
    //  and linear in the terms where the written-out polynomial is not (see
    //  the unit header). An empty range has no t, and the curve is zero, as
    //  the formula engine gives for a division by zero.
    for j := 0 to PointsCount - 1 do
    begin
        if Hi = Lo then
        begin
            FPoints[j][2] := 0;
            Continue;
        end;
        T := 2 * (PointXCoord[j] - Lo) / (Hi - Lo) - 1;
        B1 := 0;
        B2 := 0;
        for k := N - 1 downto 1 do
        begin
            B0 := C[k] + 2 * T * B1 - B2;
            B2 := B1;
            B1 := B0;
        end;
        FPoints[j][2] := C[0] + T * B1 - B2;
    end;
end;

function TChebyshevBackgroundPointsSet.GetNativeExpression: string;
const
    //  t, written out where each level needs it.
    T = '(2*(x-xl)/(xh-xl)-1)';
var
    //  Row k holds the coefficients of T_k in powers of t, from t^0.
    //  Doubles: the coefficients of T_63 pass what an int64 holds.
    Coef: array[0..CHEBYSHEV_MAX_TERMS - 1, 0..CHEBYSHEV_MAX_TERMS - 1] of double;
    FS: TFormatSettings;
    N, k, m: longint;
    Sum: string;
    Any: boolean;
begin
    //  THE POLYNOMIALS WRITTEN OUT, not cos(k*arccos(t)): a sample a rounding
    //  error past either end of the range puts t a hair outside [-1, 1], where
    //  arccos has no value. They are collected by powers of t and written as
    //  a Horner chain, over the terms in USE: what a backend is given is the
    //  curve it is fitting, and the coefficients beyond are zero. "+ 0*x"
    //  keeps it a function of x on every engine whatever the coefficients are.
    N := TermCount;
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    FillChar(Coef, SizeOf(Coef), 0);
    Coef[0][0] := 1;
    if N > 1 then
        Coef[1][1] := 1;
    for k := 2 to N - 1 do
        for m := 0 to k do
        begin
            //  T_k = 2 t T_(k-1) - T_(k-2)
            if m > 0 then
                Coef[k][m] := 2 * Coef[k - 1][m - 1];
            Coef[k][m] := Coef[k][m] - Coef[k - 2][m];
        end;
    Result := '0';
    for m := N - 1 downto 0 do
    begin
        Sum := '';
        Any := False;
        for k := m to N - 1 do
        begin
            if Coef[k][m] = 0 then
                Continue;
            if Any then
                Sum := Sum + '+';
            Sum := Sum + Format('%.17g', [Coef[k][m]], FS) + '*' +
                CoefficientName(k);
            Any := True;
        end;
        if not Any then
            Sum := '0';
        if m = N - 1 then
            Result := '(' + Sum + ')'
        else
            Result := '(' + Sum + '+' + T + '*' + Result + ')';
    end;
    Result := Result + '+0*x';
end;

function TChebyshevBackgroundPointsSet.LevelParameterName: string;
begin
    Result := CoefficientName(0);
end;

function ChebyshevBasis(AK: longint; AU: double): double;
begin
    Result := ChebyshevT(AK, AU);
end;

procedure TChebyshevBackgroundPointsSet.SeedFromBaseline(
    const AX, AY: array of double);
var
    Lo, Hi: double;
    U: array of double;
    C: array[0..CHEBYSHEV_MAX_TERMS - 1] of double;
    N, i: longint;
begin
    if Length(AX) = 0 then
        Exit;
    //  THE WINDOW'S RANGE when the curve has one - the whole data, which the
    //  baseline points need not reach - and theirs otherwise.
    if PointsCount > 0 then
    begin
        Lo := PointXCoord[0];
        Hi := PointXCoord[PointsCount - 1];
    end
    else
    begin
        Lo := MinValue(AX);
        Hi := MaxValue(AX);
    end;
    if Hi <= Lo then
        Hi := Lo + 1;
    ValuesByName['xl'] := Lo;
    ValuesByName['xh'] := Hi;

    SetLength(U, Length(AX));
    for i := 0 to High(AX) do
        U[i] := 2 * (AX[i] - Lo) / (Hi - Lo) - 1;
    for i := 0 to CHEBYSHEV_MAX_TERMS - 1 do
        C[i] := 0;
    //  As many of the terms in use as the points can fix, down to the plain
    //  level.
    N := TermCount;
    while (N >= 1) and not FitLinearBasis(U, AY, N, @ChebyshevBasis, C) do
        Dec(N);
    if N < 1 then
        C[0] := MeanOf(AY);
    for i := 0 to CHEBYSHEV_MAX_TERMS - 1 do
        ValuesByName[CoefficientName(i)] := C[i];
end;

class function TChebyshevBackgroundPointsSet.IsDefaultBackground: boolean;
begin
    Result := True;
end;

class function TChebyshevBackgroundPointsSet.GetCurveTypeName: string;
begin
    Result := 'Chebyshev background';
end;

class function TChebyshevBackgroundPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := StringToGUID('{3f1c6b0e-8a52-4d07-9e2b-71c4d5a90e36}');
end;

class function TChebyshevBackgroundPointsSet.Explanation: TExplanation;
begin
    Result := NewExplanation('', '',
        'A smooth background under the whole pattern: a sum of Chebyshev ' +
        'polynomials over the range of the data - six to begin with, up to ' +
        'sixty-four.',
        esConvention);
    AddParagraph(Result,
        'c0 T0(t) + c1 T1(t) + ... + c[n-1] T[n-1](t), where t runs from -1 at the ' +
        'first sample of the data to 1 at the last and T_k is the Chebyshev ' +
        'polynomial of degree k. It follows a baseline that bends more than ' +
        'once, which a quadratic cannot, and each coefficient shapes the curve ' +
        'nearly independently of the others, so the fit does not wander among ' +
        'them as it does among the powers of x.');
    AddParagraph(Result,
        'Like every background it is one curve under the whole profile, ' +
        'fitted to the data with the peaks held, in turn with the peaks. xl ' +
        'and xh are the first and last samples; they are set when the curve ' +
        'is placed and are not fitted.');
    AddLimitation(Result,
        'The number of terms, n, is the parameter "terms": a curve starts ' +
        'with six, and turns at most four times. Fit > Automatically fits it ' +
        'to the floor of the data, starts it at twelve, scores 16, 24, 32, 48 ' +
        'and 64 terms and keeps the best, then tries two more and two fewer ' +
        'while the score falls; the score is a BIC against the floor in ' +
        'which each sample''s part is capped, so a peak is one miss however ' +
        'tall. Sixty-four is this software''s ceiling, not a measured ' +
        'optimum. A background with many terms begins to model broad peaks - ' +
        'a hump it can follow is taken for background - which is the cost ' +
        'the score weighs.');
    AddLimitation(Result,
        'The coefficients from c[n] on are zero and are not fitted. The ' +
        'formula given to the Python backend writes the polynomial out in ' +
        'powers, which loses digits to cancellation past about twenty-five ' +
        'terms; the native engine sums by a recurrence and does not.');
    AddLimitation(Result,
        'Outside the range of the data it was placed on the series is an ' +
        'ordinary polynomial and grows quickly; data loaded or extended ' +
        'afterwards should have the background placed again.');
    AddReference(Result,
        'A. C. Larson and R. B. Von Dreele, General Structure Analysis ' +
        'System (GSAS)',
        'Los Alamos National Laboratory Report LAUR 86-748, background ' +
        'function type 1, the shifted Chebyschev polynomial',
        '');
    AddReference(Result,
        'W. H. Press et al., Numerical Recipes',
        'section 5.8, Chebyshev Approximation',
        '');
end;

var
    CTS: ICurveFactory;

initialization
    CTS := TCurveTypesSingleton.CreateCurveFactory;
    CTS.RegisterCurveType(TChebyshevBackgroundPointsSet);
end.
