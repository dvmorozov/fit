// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A number as a user typed it.)

EVERY BOX IN THIS PROGRAM THAT TAKES A NUMBER faces the same two questions: what
does a full stop mean on a keyboard laid out for a comma, and what happens when
what was typed is not a number at all. Both were answered separately at each box,
and one of the answers was to swap the process-wide decimal separator around a
call to `StrToFloat` - which raises, so on a typo the separator was never put
back and the whole application went on reading numbers with a full stop it had
not asked for.

READ WITH A FULL STOP, ALWAYS. Every formula, every data file and every wire
format this program touches writes numbers that way, so a box that followed the
user's locale would accept a value the rest of the program then reads as a
different one. That is not a limitation to work around: it is the only reading
that is consistent with everything the number is compared against.

REFUSING IS AN ANSWER. A function that raises makes every caller wrap it, and the
one that did not wrapped it in a menu handler - where the exception reached the
top-level handler, was logged as fatal, and stopped the server poll. A typo in a
text box should not disconnect the user from the compute server.
}
unit typed_number;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

{ The number the text stands for, read with a full stop whatever the locale.
  False when the text is not a number; AValue is then zero. }
function TryTypedNumber(const AText: string; out AValue: double): boolean;

{ The same, with a value to fall back on. For the boxes where an empty or
  unreadable entry has a sensible meaning - and only those. }
function TypedNumberOr(const AText: string; ADefault: double): double;

{ True when the text is a positive number. Wavelengths, widths, counts: the
  boxes where zero and negative are as wrong as a letter, and saying so once is
  better than each box remembering to. }
function TypedNumberIsPositive(const AText: string): boolean;

implementation

{ The settings every typed number is read with: a full stop, and a thousands
  separator that cannot be confused with it. }
function PointSettings: TFormatSettings;
begin
    Result := DefaultFormatSettings;
    Result.DecimalSeparator := '.';
    Result.ThousandSeparator := ',';
end;

function TryTypedNumber(const AText: string; out AValue: double): boolean;
begin
    AValue := 0;
    //  A LOCAL SETTINGS RECORD, never the process-wide one. Swapping the global
    //  separator around a call that can raise is how it stopped being swapped
    //  back.
    Result := TryStrToFloat(Trim(AText), AValue, PointSettings);
    if not Result then
        AValue := 0;
end;

function TypedNumberOr(const AText: string; ADefault: double): double;
begin
    if not TryTypedNumber(AText, Result) then
        Result := ADefault;
end;

function TypedNumberIsPositive(const AText: string): boolean;
var
    Value: double;
begin
    Result := TryTypedNumber(AText, Value) and (Value > 0);
end;

end.
