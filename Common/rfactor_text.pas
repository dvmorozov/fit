// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How an R-factor reads, the one way it is written everywhere.)

A GOOD FIT DRIVES THE R-FACTOR TOWARDS ZERO. The fixed eight-decimal format this
replaces showed 5.68e-6 as 0.00000568 - a row of zeros with three digits of
information at the end - and a better fit than 1e-8 as nothing at all.

FOUR SIGNIFICANT DIGITS, always. A decimal while that stays short (from 0.001
up to below a million), scientific notation outside it. The switch is taken on
the magnitude AFTER rounding, so 0.00099999 reads as 0.001000 and not as a
decimal with five digits or 1.000E-3 beside its own neighbours.

The status bar, the progress header while a fit runs, and the stats the server
reports all use this, so the live number ends exactly where the final one begins.
The text still parses with StrToFloat, which the project file relies on.
}
unit rfactor_text;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

function RFactorText(AValue: double): string;

implementation

function RFactorText(AValue: double): string;
var
    Scientific: string;
    Exponent, Decimals: longint;
begin
    if AValue = 0 then
        Exit(FloatToStrF(0, ffFixed, 15, 3));
    //  "d.dddE+x": rounded to four digits, so its exponent is the magnitude
    //  of what will actually be shown.
    Scientific := FloatToStrF(AValue, ffExponent, 4, 1);
    Exponent := StrToInt(Copy(Scientific, Pos('E', Scientific) + 1, MaxInt));
    if (Exponent < -3) or (Exponent >= 6) then
        Exit(StringReplace(Scientific, 'E+', 'E', []));
    Decimals := 3 - Exponent;
    if Decimals < 0 then
        Decimals := 0;
    Result := FloatToStrF(AValue, ffFixed, 15, Decimals);
end;

end.
