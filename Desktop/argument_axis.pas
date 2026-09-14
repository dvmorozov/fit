// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL.

@abstract(Argument (abscissa) axis abstraction.)

The stored profile argument is kept as the raw value as loaded. An argument axis is a
display-only transform: it converts the stored value to/from the value shown to the user
and supplies the axis name/unit. It never alters stored data or the fit.
}
unit argument_axis;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math;

type
    { Display-only transform between the stored (raw) argument and the shown value. }
    IArgumentAxis = interface
        function DisplayName: string;
        function UnitName: string;
        function ToDisplay(const RawValue: double): double;
        function FromDisplay(const DisplayValue: double): double;
    end;

    { Common base so callers can hold/free any axis by one type. }
    TArgumentAxis = class(TObject, IArgumentAxis)
    public
        function DisplayName: string; virtual; abstract;
        function UnitName: string; virtual; abstract;
        function ToDisplay(const RawValue: double): double; virtual; abstract;
        function FromDisplay(const DisplayValue: double): double; virtual; abstract;
    end;

    { General default: the stored value is the argument as loaded (no transform). }
    TIdentityAxis = class(TArgumentAxis)
    public
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
    end;

    { Selectable diffraction display unit. Raw value is 2*Theta (degrees). }
    TDiffractionMode = (dmTwoTheta, dmTheta, dmSinThetaOverLambda);

    { Preserves the legacy 2*Theta / Theta / Sin(Theta)/Lambda family as one axis. }
    TDiffractionAngleAxis = class(TArgumentAxis)
    private
        FMode: TDiffractionMode;
        FWaveLength: double;
    public
        constructor Create(AMode: TDiffractionMode; AWaveLength: double);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
    end;

    { User-defined axis: display and its inverse are arbitrary formulas of the
      single variable x (the stored raw value for the forward transform, the
      shown value for the inverse), evaluated by the native expression engine.
      Purely presentational - it never changes stored data or the fit (D5). }
    TExpressionAxis = class(TArgumentAxis)
    private
        FDisplayName: string;
        FUnitName: string;
        FForwardExpr: string;   //  display = f(x),   x = raw value
        FInverseExpr: string;   //  raw = g(x),       x = shown value
    public
        constructor Create(const ADisplayName, AUnitName,
            AForwardExpr, AInverseExpr: string);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
    end;

{ WHETHER A TYPED WAVELENGTH CAN BE USED, and what to say if it cannot.

  The sin(theta)/lambda axis divides by the wavelength, so a wavelength of zero
  is not an axis - and zero is what "not set" already means to the client, which
  is why accepting it would silently do nothing at all rather than failing.

  TWO REFUSALS, ONE PLACE. Both were written in an LCL menu handler, each with its
  own MessageDlg, where the only way to reach either was to type into the box.
  False with a message to show; True with an empty one and the value out. }
function WavelengthFromText(const AText: string; out AValue: double;
    out ARefusal: string): boolean;

implementation

uses
    typed_number,
    native_math_expr, checks;

{ TIdentityAxis }

function TIdentityAxis.DisplayName: string; begin Result := 'Position'; end;
function TIdentityAxis.UnitName: string; begin Result := ''; end;
function TIdentityAxis.ToDisplay(const RawValue: double): double; begin Result := RawValue; end;
function TIdentityAxis.FromDisplay(const DisplayValue: double): double; begin Result := DisplayValue; end;

{ TDiffractionAngleAxis }

constructor TDiffractionAngleAxis.Create(AMode: TDiffractionMode; AWaveLength: double);
begin
    inherited Create;
    FMode := AMode;
    FWaveLength := AWaveLength;
end;

function TDiffractionAngleAxis.DisplayName: string;
begin
    case FMode of
        dmTwoTheta:            Result := '2*Theta';
        dmTheta:               Result := 'Theta';
        dmSinThetaOverLambda:  Result := 'Sin(Theta)/Lambda';
    end;
end;

function TDiffractionAngleAxis.UnitName: string;
begin
    case FMode of
        dmTwoTheta, dmTheta:   Result := 'deg';
        dmSinThetaOverLambda:  Result := '1/A';
    end;
end;

function TDiffractionAngleAxis.ToDisplay(const RawValue: double): double;
begin
    case FMode of
        dmTwoTheta: Result := RawValue;
        dmTheta:    Result := RawValue / 2;
        dmSinThetaOverLambda:
        begin
            CheckThat(FWaveLength <> 0, 'a sin(theta)/lambda axis needs a nonzero wavelength');
            Result := Sin((RawValue * pi) / (2 * 180)) / FWaveLength;
        end;
    end;
end;

function TDiffractionAngleAxis.FromDisplay(const DisplayValue: double): double;
begin
    case FMode of
        dmTwoTheta: Result := DisplayValue;
        dmTheta:    Result := DisplayValue * 2;
        dmSinThetaOverLambda:
        begin
            CheckThat(FWaveLength <> 0, 'a sin(theta)/lambda axis needs a nonzero wavelength');
            Result := 2 * (180 / pi) * ArcSin(DisplayValue * FWaveLength);
        end;
    end;
end;

{ TExpressionAxis }

{ Evaluates Expr with the single variable x set to XValue. Raises on an invalid
  or non-evaluable formula so the caller can report it. }
function EvalWithX(const Expr: string; const XValue: double): double;
var
    Fmt: TFormatSettings;
    Params: string;
begin
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    Params := 'x=' + FloatToStr(XValue, Fmt) + #0;
    Result := 0;
    if ParseAndCalcExpression(PChar(Expr), PChar(Params), @Result) <> 1 then
        raise Exception.CreateFmt(
            'The axis formula could not be evaluated: "%s". Use x as the ' +
            'variable and standard functions, e.g. ln(x).', [Expr]);
end;

constructor TExpressionAxis.Create(const ADisplayName, AUnitName,
    AForwardExpr, AInverseExpr: string);
begin
    inherited Create;
    FDisplayName := ADisplayName;
    FUnitName    := AUnitName;
    FForwardExpr := AForwardExpr;
    FInverseExpr := AInverseExpr;
end;

function TExpressionAxis.DisplayName: string;
begin
    Result := FDisplayName;
end;

function TExpressionAxis.UnitName: string;
begin
    Result := FUnitName;
end;

function TExpressionAxis.ToDisplay(const RawValue: double): double;
begin
    Result := EvalWithX(FForwardExpr, RawValue);
end;

function TExpressionAxis.FromDisplay(const DisplayValue: double): double;
begin
    Result := EvalWithX(FInverseExpr, DisplayValue);
end;


function WavelengthFromText(const AText: string; out AValue: double;
    out ARefusal: string): boolean;
begin
    ARefusal := '';
    AValue := 0;
    //  REFUSED, NOT RAISED. The code this came from swapped the process-wide
    //  decimal separator around a StrToFloat that raises on a typo, so the
    //  separator was never put back and the exception reached the top-level
    //  handler - which logs at Fatal and STOPS THE SERVER POLL. A typo in a text
    //  box disconnected the user from the compute server. See findings.md.
    if not TryTypedNumber(AText, AValue) then
    begin
        //  Names the separator, because a comma is what a great many keyboards
        //  and locales produce and the refusal is otherwise a mystery.
        ARefusal := 'The wavelength must be a number, written with a full ' +
            'stop - for example 1.5406.';
        AValue := 0;
        Result := False;
        Exit;
    end;
    if AValue <= 0 then
    begin
        ARefusal := 'The wavelength must be greater than zero.';
        AValue := 0;
        Result := False;
        Exit;
    end;
    Result := True;
end;

end.
