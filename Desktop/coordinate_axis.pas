// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL.

@abstract(Coordinate axis abstraction - for the argument and the value alike.)

Every point is stored as it was loaded. A coordinate axis is a display-only
transform: it converts a stored coordinate to and from the value shown to the
user and supplies the axis name and unit. It never alters stored data or the fit.

ONE ABSTRACTION FOR BOTH COORDINATES. The horizontal axis was the only one that
could be transformed, and the vertical one was captioned by a fixed word written
into the form - "Intensity" over a price series. An axis does not care which
coordinate it transforms, so the same class serves both, and which modes are
offered for which coordinate is declared by the mode (axis_mode_registry).

A VALUE WITH NO PLACE ON THE AXIS IS NaN. A logarithm of zero or of a negative
number does not exist; ToDisplay answers NaN, and the chart leaves a gap there.
The alternatives were each worse: a floor or a clamp draws a point that is not
in the data, and raising stops the whole plot over one sample.
}
unit coordinate_axis;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math;

type
    { Which coordinate of a point an axis shows: the argument (horizontal) or
      the value (vertical). }
    TAxisDimension = (adArgument, adValue);
    TAxisDimensions = set of TAxisDimension;
    { A calendar date per point of the data, by the point's index. }
    TAxisDates = array of TDateTime;

    { Display-only transform between a stored (raw) coordinate and the shown value. }
    ICoordinateAxis = interface
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
    TCoordinateAxis = class(TObject, ICoordinateAxis)
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
        { What the chart writes along the axis: 'Name [unit]', or the name alone
          when the axis has no unit. }
        function Title: string; virtual;
        { The caption beside the pointer's coordinate on this axis. }
        function ReadoutCaption: string;
        { The pointer's coordinate, given as the SHOWN value, in the words of
          this axis. Plain numbers by default; an axis whose marks are its own
          reads out the same way its marks are written. }
        function ReadoutText(ADisplayValue: double): string; virtual;
    end;

    { The stored coordinate as loaded, under a name saying what it is -
      Position, Intensity, Price. No transform at all. }
    TNamedAxis = class(TCoordinateAxis)
    private
        FName: string;
        FUnit: string;
    public
        constructor Create(const AName, AUnit: string);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
    end;

    { The decimal logarithm of a quantity, READ IN THE QUANTITY: the spacing is
      logarithmic, and the marks and the readout are the prices or counts
      themselves, as on any log chart. A value at or below zero has no
      logarithm and is left undrawn (NaN). }
    TLogarithmicAxis = class(TCoordinateAxis)
    private
        FName: string;
        FUnit: string;
    public
        { The quantity whose logarithm is drawn, by its own name and unit. }
        constructor Create(const AQuantityName, AQuantityUnit: string);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
        function FormatsMarks: boolean; override;
        function MarkText(AValue, AStep: double): string; override;
        function Title: string; override;
        function ReadoutText(ADisplayValue: double): string; override;
    end;

    { A calendar date, stored as the day count TDateTime uses, and marked and
      read out as a date. }
    TDateAxis = class(TCoordinateAxis)
    public
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
        function ChooseMarks(AMin, AMax: double;
            out AStart, AStep: double): boolean; override;
        function FormatsMarks: boolean; override;
        function MarkText(AValue, AStep: double): string; override;
        function ReadoutText(ADisplayValue: double): string; override;
    end;

    { Selectable diffraction display unit. Raw value is 2*Theta (degrees). }
    TDiffractionMode = (dmTwoTheta, dmTheta, dmSinThetaOverLambda);

    { Preserves the legacy 2*Theta / Theta / Sin(Theta)/Lambda family as one axis. }
    TDiffractionAngleAxis = class(TCoordinateAxis)
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
    TExpressionAxis = class(TCoordinateAxis)
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
    TDurationAxis = class(TCoordinateAxis)
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

type
    { BARS LABELLED BY THEIR DATES. The argument is the bar's number - so the
      bars stay evenly spaced, which is what a count is read in - and each mark
      and each reading names the day that bar was recorded on, from a table of
      one date per bar. A position between bars names the nearest one; one
      beyond the series names nothing, rather than the day of the last bar. }
    TBarDateAxis = class(TCoordinateAxis)
    private
        FDates: TAxisDates;
        function DateAt(AValue: double): string;
    public
        constructor Create(const ADates: TAxisDates);
        function DisplayName: string; override;
        function UnitName: string; override;
        function ToDisplay(const RawValue: double): double; override;
        function FromDisplay(const DisplayValue: double): double; override;
        function ChooseMarks(AMin, AMax: double;
            out AStart, AStep: double): boolean; override;
        function FormatsMarks: boolean; override;
        function MarkText(AValue, AStep: double): string; override;
        function ReadoutText(ADisplayValue: double): string; override;
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
    typed_number, status_readout,
    native_math_expr, checks;

{ TCoordinateAxis }

{$hints off}
function TCoordinateAxis.ChooseMarks(AMin, AMax: double;
    out AStart, AStep: double): boolean;
begin
    AStart := AMin;
    AStep := 0;
    Result := False;
end;

function TCoordinateAxis.FormatsMarks: boolean;
begin
    Result := False;
end;

function TCoordinateAxis.MarkText(AValue, AStep: double): string;
begin
    Result := '';
end;
{$hints on}

function TCoordinateAxis.Title: string;
begin
    Result := DisplayName;
    if UnitName <> '' then
        Result := Result + ' [' + UnitName + ']';
end;

function TCoordinateAxis.ReadoutCaption: string;
begin
    Result := DisplayName + ':';
end;

function TCoordinateAxis.ReadoutText(ADisplayValue: double): string;
begin
    Result := CoordinateReadout(ADisplayValue);
end;

{ TNamedAxis }

constructor TNamedAxis.Create(const AName, AUnit: string);
begin
    inherited Create;
    FName := AName;
    FUnit := AUnit;
end;

function TNamedAxis.DisplayName: string; begin Result := FName; end;
function TNamedAxis.UnitName: string; begin Result := FUnit; end;
function TNamedAxis.ToDisplay(const RawValue: double): double; begin Result := RawValue; end;
function TNamedAxis.FromDisplay(const DisplayValue: double): double; begin Result := DisplayValue; end;

{ TLogarithmicAxis }

constructor TLogarithmicAxis.Create(const AQuantityName, AQuantityUnit: string);
begin
    inherited Create;
    FName := AQuantityName;
    FUnit := AQuantityUnit;
end;

function TLogarithmicAxis.DisplayName: string; begin Result := FName; end;
function TLogarithmicAxis.UnitName: string; begin Result := FUnit; end;

function TLogarithmicAxis.ToDisplay(const RawValue: double): double;
begin
    //  Tested BEFORE Log10 is called: the logarithm of zero raises on some
    //  targets and answers -Inf on others, and neither is a point.
    if RawValue > 0 then
        Result := Log10(RawValue)
    else
        Result := NaN;
end;

function TLogarithmicAxis.FromDisplay(const DisplayValue: double): double;
begin
    Result := Power(10, DisplayValue);
end;

function TLogarithmicAxis.FormatsMarks: boolean;
begin
    Result := True;
end;

{$hints off}
function TLogarithmicAxis.MarkText(AValue, AStep: double): string;
var
    Fmt: TFormatSettings;
begin
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    //  Four significant figures: a price series spans well under one decade,
    //  so its marks are close together in the quantity and need the digits.
    Result := Trim(Format('%6.4g', [Power(10, AValue)], Fmt));
end;
{$hints on}

function TLogarithmicAxis.Title: string;
begin
    Result := inherited Title + ', log scale';
end;

function TLogarithmicAxis.ReadoutText(ADisplayValue: double): string;
begin
    Result := Trim(CoordinateReadout(Power(10, ADisplayValue)));
end;

{ TDateAxis }

const
    { The steps a date axis is marked in, in days: whole days, weeks, and
      roughly months, quarters, years and multiples of them. }
    DATE_STEPS: array[0..12] of double = (
        1, 2, 7, 14, 28, 56, 91, 182, 364, 728, 1820, 3640, 7280);
    DATE_MAX_INTERVALS = 8;

function TDateAxis.DisplayName: string; begin Result := 'Date'; end;
{ Written into every mark, so the axis caption carries none. }
function TDateAxis.UnitName: string; begin Result := ''; end;
function TDateAxis.ToDisplay(const RawValue: double): double; begin Result := RawValue; end;
function TDateAxis.FromDisplay(const DisplayValue: double): double; begin Result := DisplayValue; end;

function TDateAxis.ChooseMarks(AMin, AMax: double;
    out AStart, AStep: double): boolean;
var
    i: longint;
begin
    AStep := DATE_STEPS[High(DATE_STEPS)];
    for i := 0 to High(DATE_STEPS) do
        if (AMax - AMin) / DATE_STEPS[i] <= DATE_MAX_INTERVALS then
        begin
            AStep := DATE_STEPS[i];
            Break;
        end;
    //  On whole days, so every mark names the day it stands on rather than a
    //  moment part-way through it.
    AStart := Ceil(AMin / AStep - 1e-9) * AStep;
    Result := True;
end;

function TDateAxis.FormatsMarks: boolean;
begin
    Result := True;
end;

{$hints off}
function TDateAxis.MarkText(AValue, AStep: double): string;
begin
    Result := FormatDateTime('yyyy-mm-dd', Floor(AValue + 1e-9));
end;
{$hints on}

function TDateAxis.ReadoutText(ADisplayValue: double): string;
begin
    Result := FormatDateTime('yyyy-mm-dd', Floor(ADisplayValue + 1e-9));
end;

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

{ TBarDateAxis }

constructor TBarDateAxis.Create(const ADates: TAxisDates);
begin
    inherited Create;
    FDates := Copy(ADates);
end;

function TBarDateAxis.DateAt(AValue: double): string;
var
    Index: int64;
begin
    Result := '';
    if IsNan(AValue) then
        Exit;
    Index := Round(AValue);
    if (Index < 0) or (Index > High(FDates)) then
        Exit;
    Result := FormatDateTime('yyyy-mm-dd', FDates[Index]);
end;

function TBarDateAxis.DisplayName: string; begin Result := 'Date'; end;
function TBarDateAxis.UnitName: string; begin Result := ''; end;
function TBarDateAxis.ToDisplay(const RawValue: double): double; begin Result := RawValue; end;
function TBarDateAxis.FromDisplay(const DisplayValue: double): double; begin Result := DisplayValue; end;

const
    { The steps bars are marked in: whole bars, in round numbers of them. }
    BAR_STEPS: array[0..11] of double = (
        1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000);

function TBarDateAxis.ChooseMarks(AMin, AMax: double;
    out AStart, AStep: double): boolean;
var
    i: longint;
begin
    //  ON WHOLE BARS, so every mark stands on a bar and names its day.
    AStep := BAR_STEPS[High(BAR_STEPS)];
    for i := 0 to High(BAR_STEPS) do
        if (AMax - AMin) / BAR_STEPS[i] <= DATE_MAX_INTERVALS then
        begin
            AStep := BAR_STEPS[i];
            Break;
        end;
    AStart := Ceil(AMin / AStep - 1e-9) * AStep;
    Result := True;
end;

function TBarDateAxis.FormatsMarks: boolean;
begin
    Result := True;
end;

{$hints off}
function TBarDateAxis.MarkText(AValue, AStep: double): string;
begin
    Result := DateAt(AValue);
end;
{$hints on}

function TBarDateAxis.ReadoutText(ADisplayValue: double): string;
begin
    Result := DateAt(ADisplayValue);
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
