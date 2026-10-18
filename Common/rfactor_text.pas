// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How an R-factor reads, the one way it is written everywhere.)

A GOOD FIT DRIVES THE R-FACTOR TOWARDS ZERO. The fixed eight-decimal format this
replaces showed 5.68e-6 as 0.00000568 - a row of zeros with three digits of
information at the end - and a better fit than 1e-8 as nothing at all.

SIX SIGNIFICANT DIGITS, always. A decimal while that stays short (from 0.001
up to below a million), scientific notation outside it. The switch is taken on
the magnitude AFTER rounding, so 0.0009999999 reads as 0.00100000 and not as a
decimal with seven digits or 1.00000E-3 beside its own neighbours.

SIX, NOT THE FOUR IT WAS. A long simplex run improves by parts in ten thousand
a second, and at four digits the R-factor over a running fit read 1.323E-6 for
minutes: a fit still working looked stopped. Six is what a double's last
digits can still be trusted for after a sum over thousands of points, and the
status-bar panel holds it. A project saved by a four-digit build is compared at
the digits it carries (fit_project_provenance), not at six.

The status bar, the progress header while a fit runs, and the stats the server
reports all use this, so the live number ends exactly where the final one begins.
The text still parses with StrToFloat, which the project file relies on.
}
unit rfactor_text;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

const
    { How many significant digits an R-factor is written with. }
    RFactorDigits = 6;

{ AValue in ADigits significant digits; RFactorDigits unless a caller has to
  read a figure written with fewer. }
function RFactorText(AValue: double;
    ADigits: longint = RFactorDigits): string;

implementation

function RFactorText(AValue: double; ADigits: longint): string;
var
    Scientific: string;
    Exponent, Decimals: longint;
begin
    if AValue = 0 then
        Exit(FloatToStrF(0, ffFixed, 15, ADigits - 1));
    //  "d.dddddE+x": rounded to the digits shown, so its exponent is the
    //  magnitude of what will actually be shown.
    Scientific := FloatToStrF(AValue, ffExponent, ADigits, 1);
    Exponent := StrToInt(Copy(Scientific, Pos('E', Scientific) + 1, MaxInt));
    if (Exponent < -3) or (Exponent >= 6) then
        Exit(StringReplace(Scientific, 'E+', 'E', []));
    Decimals := ADigits - 1 - Exponent;
    if Decimals < 0 then
        Decimals := 0;
    Result := FloatToStrF(AValue, ffFixed, 15, Decimals);
end;

end.
