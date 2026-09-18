// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A table as rows of text for a file, and a number as that file writes it.)

WHY NOT THE SCREEN'S TEXT. The tables show four decimals, which is right for
reading and wrong for a file: 0.000012 is 0.0000 on screen, and a fitted width
exported that way has lost what a publication needs. So a table offers its rows
for export separately, and a number in them is written so that reading it back
gives the same double.

SHORTEST THAT READS BACK, not always seventeen digits: 0.1 read back from '0.1'
is the same double, and 0.10000000000000001 is only noise to a reader.

A FULL STOP, whatever the machine's locale. A file travels, and a comma written
on one machine is a column separator on the next.
}
unit table_text;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { A table, row by row, each row its cells left to right. }
    TTextRows = array of TStringArray;

{ AValue as an exported table writes it. }
function ExportedNumber(AValue: double): string;

implementation

function ExportedNumber(AValue: double): string;
var
    F: TFormatSettings;
    Back: double;
begin
    F := DefaultFormatSettings;
    F.DecimalSeparator := '.';
    F.ThousandSeparator := #0;
    //  FIFTEEN DIGITS FIRST, which is what most values need and reads well.
    Result := FloatToStrF(AValue, ffGeneral, 15, 0, F);
    if TryStrToFloat(Result, Back, F) and (Back = AValue) then
        Exit;
    //  SEVENTEEN WHEN FIFTEEN DO NOT READ BACK. FloatToStrF will not give a
    //  double more than fifteen whatever it is asked for; Str gives seventeen,
    //  in exponent form and always with a full stop.
    Str(AValue, Result);
    Result := Trim(Result);
end;

end.
