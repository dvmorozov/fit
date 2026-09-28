// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What every background shape has in common.)

A BACKGROUND SHAPE IS A CURVE WITH NO POSITION. The model puts one under the
peaks of each fit interval (TFitTask.RestoreBackgroundCurve), fitted with them,
drawn and reported like them, and never taken out by curve reduction. The
alternative - subtracting a background from the data before fitting - rewrites
the measurement and has to be remembered; see docs/internal/roadmap.md,
"Background element".

WHAT THE BASE SUPPLIES, so a shape states only its formula and how it seeds:

  * IsBackground, the capability everything else asks;
  * a REFERENCE abscissa xr, a Calculated parameter set when the curve is
    seeded, so a shape's coefficients describe the curve at the start of its
    interval rather than at x = 0 - which is where a diffraction pattern at
    2theta = 150 would otherwise put a polynomial's constant term, a hundred
    and fifty units away from any data;
  * coefficients whose variation step follows their value
    (TBackgroundCoefficientParameter), because the native simplex steps by an
    absolute amount and a level of 780 is not found from a step of 0.1;
  * the least-squares arithmetic every shape seeds with.

NAMES THAT ARE NOBODY'S ROLE. The engine reads a parameter called "A" as an
amplitude and one called "sigma" as a width, by name, whatever the class meant
(TCurvePointsSet.SetSpecParamPtr) - and an amplitude is what curve reduction
deletes a curve for having too little of. So the coefficients are b0, b1, b2,
the decay length tau, the exponent p, and nothing a peak would be recognised by.
}
unit background_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, coordinate_axis, named_points_set, formula_points_set,
    special_curve_parameter;

type
    { A fitted coefficient of a background shape. }
    TBackgroundCoefficientParameter = class(TSpecialCurveParameter)
    public
        constructor Create(const AName: string);
        function CreateCopy: TSpecialCurveParameter; override;
        { A tenth of the value, or 0.1 for a value of zero - see the unit
          header. Called after seeding, so it sees the seeded value. }
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
    end;

    TBackgroundPointsSet = class(TFormulaPointsSet)
    protected
        { Adds a fitted coefficient named AName, starting at zero. }
        procedure AddCoefficient(const AName: string);
        { Adds the reference abscissa xr. Call once, after the coefficients, so
          the parameters grid shows what is fitted first. }
        procedure AddReferenceParameter(AInitial: double);
        function GetReference: double;
        procedure SetReference(AValue: double);

    public
        class function IsBackground: boolean; override;
        { None: a background is about no field in particular, and the peaks'
          own preference - or the data's - decides the axes. }
        class function PreferredAxisMode(ADimension: TAxisDimension): string;
            override;
        { A background has no extremum to search for. }
        class function GetExtremumMode: TExtremumMode; override;
        property Reference: double read GetReference write SetReference;
    end;

{ Least squares for y = c0 + c1 u + ... + c[AOrder] u^AOrder, where u = x - ARef.
  Returns False, leaving AC untouched, when there are fewer points than
  coefficients or the points cannot fix them (all at one x). }
function FitPolynomial(const AX, AY: array of double; ARef: double;
    AOrder: longint; var AC: array of double): boolean;
{ The smallest of AX; 0 for none. }
function SmallestOf(const AX: array of double): double;
{ The mean of AY; 0 for none. }
function MeanOf(const AY: array of double): double;

implementation

uses
    calculated_curve_parameter;

{ ---- TBackgroundCoefficientParameter ---- }

constructor TBackgroundCoefficientParameter.Create(const AName: string);
begin
    inherited Create;
    FName := AName;
    FType := Variable;
end;

function TBackgroundCoefficientParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TBackgroundCoefficientParameter.Create(FName);
    CopyTo(Result);
end;

procedure TBackgroundCoefficientParameter.InitVariationStep;
begin
    if Value = 0 then
        FVariationStep := 0.1
    else
        FVariationStep := 0.1 * Abs(Value);
end;

procedure TBackgroundCoefficientParameter.InitValue;
begin
    FValue := 0.0;
end;

function TBackgroundCoefficientParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.0001;
end;

{ ---- TBackgroundPointsSet ---- }

procedure TBackgroundPointsSet.AddCoefficient(const AName: string);
begin
    AddParameter(TBackgroundCoefficientParameter.Create(AName));
end;

procedure TBackgroundPointsSet.AddReferenceParameter(AInitial: double);
var
    P: TSpecialCurveParameter;
begin
    //  CALCULATED: sent to a formula backend with the rest (it is in the
    //  expression) but held there, and never varied here - it is where the
    //  coefficients are measured from, not something to fit.
    P := TCalculatedCurveParameter.Create;
    P.Name := 'xr';
    P.Value := AInitial;
    AddParameter(P);
end;

function TBackgroundPointsSet.GetReference: double;
begin
    Result := ValuesByName['xr'];
end;

procedure TBackgroundPointsSet.SetReference(AValue: double);
begin
    ValuesByName['xr'] := AValue;
end;

class function TBackgroundPointsSet.IsBackground: boolean;
begin
    Result := True;
end;

{$hints off}
class function TBackgroundPointsSet.PreferredAxisMode(
    ADimension: TAxisDimension): string;
begin
    Result := '';
end;
{$hints on}

class function TBackgroundPointsSet.GetExtremumMode: TExtremumMode;
begin
    Result := MaximumsAndMinimums;
end;

{ ---- the arithmetic ---- }

function SmallestOf(const AX: array of double): double;
var
    i: longint;
begin
    Result := 0;
    if Length(AX) = 0 then
        Exit;
    Result := AX[0];
    for i := 1 to High(AX) do
        if AX[i] < Result then
            Result := AX[i];
end;

function MeanOf(const AY: array of double): double;
var
    i: longint;
begin
    Result := 0;
    if Length(AY) = 0 then
        Exit;
    for i := 0 to High(AY) do
        Result := Result + AY[i];
    Result := Result / Length(AY);
end;

function FitPolynomial(const AX, AY: array of double; ARef: double;
    AOrder: longint; var AC: array of double): boolean;
const
    MaxN = 3;
var
    N, i, j, k, Row, Pivot: longint;
    M: array[0..MaxN - 1, 0..MaxN] of double;
    Powers: array[0..2 * MaxN] of double;
    U, T, Factor: double;
    Sol: array[0..MaxN - 1] of double;
begin
    Result := False;
    N := AOrder + 1;
    if (N < 1) or (N > MaxN) or (Length(AX) < N) or (Length(AX) <> Length(AY)) then
        Exit;

    //  THE NORMAL EQUATIONS, sum over the points of u^(i+j) c_j = sum u^i y.
    //  Three unknowns at most, and u measured from the reference keeps the
    //  powers of a pattern at 2theta = 150 from swamping the constant term.
    for i := 0 to N - 1 do
        for j := 0 to N do
            M[i, j] := 0;
    for k := 0 to High(AX) do
    begin
        U := AX[k] - ARef;
        Powers[0] := 1;
        for i := 1 to 2 * N do
            Powers[i] := Powers[i - 1] * U;
        for i := 0 to N - 1 do
        begin
            for j := 0 to N - 1 do
                M[i, j] := M[i, j] + Powers[i + j];
            M[i, N] := M[i, N] + Powers[i] * AY[k];
        end;
    end;

    //  Gaussian elimination with partial pivoting.
    for Row := 0 to N - 1 do
    begin
        Pivot := Row;
        for i := Row + 1 to N - 1 do
            if Abs(M[i, Row]) > Abs(M[Pivot, Row]) then
                Pivot := i;
        if Abs(M[Pivot, Row]) < 1e-300 then
            Exit;
        if Pivot <> Row then
            for j := 0 to N do
            begin
                T := M[Row, j];
                M[Row, j] := M[Pivot, j];
                M[Pivot, j] := T;
            end;
        for i := Row + 1 to N - 1 do
        begin
            Factor := M[i, Row] / M[Row, Row];
            for j := Row to N do
                M[i, j] := M[i, j] - Factor * M[Row, j];
        end;
    end;
    for i := N - 1 downto 0 do
    begin
        T := M[i, N];
        for j := i + 1 to N - 1 do
            T := T - M[i, j] * Sol[j];
        //  A pivot that is merely tiny against the others is still singular
        //  for this purpose: every point at one x cannot fix a slope.
        if Abs(M[i, i]) <= 1e-12 * Max(1, Abs(M[0, 0])) then
            Exit;
        Sol[i] := T / M[i, i];
    end;
    for i := 0 to N - 1 do
    begin
        if IsNan(Sol[i]) or IsInfinite(Sol[i]) then
            Exit;
        AC[i] := Sol[i];
    end;
    Result := True;
end;

end.
