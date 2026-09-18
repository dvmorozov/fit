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

    { Common base so callers can hold/free any axis by one type.

      THE MARKS ARE THE AXIS'S TO CHOOSE, when it has a reason to. An axis whose
      values are a plain number leaves the chart to place its marks and print
      them as numbers; an axis whose values are a quantity with units of its
      own - a duration, say - picks steps that suit those units and writes each
      mark in them. The defaults below are the first kind. }
    TArgumentAxis = class(TObject, IArgumentAxis)
    public
        function DisplayName: string; virtual; abstract;
        function UnitName: string; virtual; abstract;
        function ToDisplay(const RawValue: double): double; virtual; abstract;
        function FromDisplay(const DisplayValue: double): double; virtual; abstract;
        { The first mark at or after AMin and the distance between marks, for
          shown values from AMin to AMax. False leaves the chart's own. }
        function ChooseMarks(AMin, AMax: double;
            out AStart, AStep: double): boolean; virtual;
        { Whether MarkText writes the marks, rather than the chart. }
        function FormatsMarks: boolean; virtual;
        { A mark at AValue on an axis marked every AStep. }
        function MarkText(AValue, AStep: double): string; virtual;
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

    { Elapsed time, stored and shown in seconds and marked in the units that
      read at its length: seconds, minutes, hours, days. The loss chart's axis. }
    TDurationAxis = class(TArgumentAxis)
    public
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
        function ChooseMarks(AMin, AMax: double;
            out AStart, AStep: double): boolean; override;
        function FormatsMarks: boolean; override;
        function MarkText(AValue, AStep: double): string; override;
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

{ TArgumentAxis }

{$hints off}
function TArgumentAxis.ChooseMarks(AMin, AMax: double;
    out AStart, AStep: double): boolean;
begin
    AStart := AMin;
    AStep := 0;
    Result := False;
end;

function TArgumentAxis.FormatsMarks: boolean;
begin
    Result := False;
end;

function TArgumentAxis.MarkText(AValue, AStep: double): string;
begin
    Result := '';
end;
{$hints on}

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

{ TDurationAxis }

const
    { The steps a duration is marked in, each a round number in its own unit. }
    DURATION_STEPS: array[0..24] of double = (
        0.1, 0.2, 0.5, 1, 2, 5, 10, 15, 30,
        60, 120, 300, 600, 900, 1800,
        3600, 7200, 10800, 21600, 43200,
        86400, 172800, 432000, 864000, 2592000);
    { At most this many intervals across the axis, which is about what the
      chart's own marks come to on the numeric axes beside it. }
    DURATION_MAX_INTERVALS = 8;

function TDurationAxis.DisplayName: string;
begin
    Result := 'Elapsed time';
end;

{ Written into every mark, so the axis caption carries none. }
function TDurationAxis.UnitName: string;
begin
    Result := '';
end;

function TDurationAxis.ToDisplay(const RawValue: double): double;
begin
    Result := RawValue;
end;

function TDurationAxis.FromDisplay(const DisplayValue: double): double;
begin
    Result := DisplayValue;
end;

function TDurationAxis.ChooseMarks(AMin, AMax: double;
    out AStart, AStep: double): boolean;
var
    i: longint;
begin
    AStep := DURATION_STEPS[High(DURATION_STEPS)];
    for i := 0 to High(DURATION_STEPS) do
        if (AMax - AMin) / DURATION_STEPS[i] <= DURATION_MAX_INTERVALS then
        begin
            AStep := DURATION_STEPS[i];
            Break;
        end;
    AStart := Ceil(AMin / AStep - 1e-9) * AStep;
    Result := True;
end;

function TDurationAxis.FormatsMarks: boolean;
begin
    Result := True;
end;

function TDurationAxis.MarkText(AValue, AStep: double): string;
var
    Total, Days, Hours, Minutes: int64;
    Seconds: double;
    Fmt: TFormatSettings;

    procedure Append(const APart: string);
    begin
        if Result <> '' then
            Result := Result + ' ';
        Result := Result + APart;
    end;

begin
    Result := '';
    //  Rounded to the step's own precision, so 0.30000000000000004 is 0.3.
    if AStep < 1 then
        AValue := Round(AValue * 10) / 10
    else
        AValue := Round(AValue);
    if AValue <= 0 then
    begin
        Result := '0';
        Exit;
    end;
    Total := Trunc(AValue);
    Days := Total div 86400;
    Hours := (Total mod 86400) div 3600;
    Minutes := (Total mod 3600) div 60;
    Seconds := AValue - (Total - Total mod 60);
    if Days > 0 then
        Append(IntToStr(Days) + ' d');
    if Hours > 0 then
        Append(IntToStr(Hours) + ' h');
    if Minutes > 0 then
        Append(IntToStr(Minutes) + ' min');
    if Seconds > 0 then
    begin
        Fmt := DefaultFormatSettings;
        Fmt.DecimalSeparator := '.';
        if AStep < 1 then
            Append(FormatFloat('0.0', Seconds, Fmt) + ' s')
        else
            Append(IntToStr(Round(Seconds)) + ' s');
    end;
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
