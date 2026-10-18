// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Reading the numbers out of one line of a text data file.)

WHY THIS IS ITS OWN UNIT. It was the DAT loader's private scanner, and it is not
about DAT files at all: it answers "which numbers does this line hold, and was
every number-like run on it well formed?" - which is what any line-based numeric
format has to ask. The second caller is the JCAMP-DX loader's AFFN lines, in a
module repository; a copy there would be a second implementation of the comma
rule, and the two would disagree the first time either was corrected.

Nothing here reads a file, so every decision below is reachable from a test with
a string literal.
}
unit text_numbers;

{$mode delphi}

interface

uses
    SysUtils;

type
    TLineNumbers = array of double;

{ The numbers on one line, left to right, and whether every number-like run on
  it was a well-formed number.

  A NUMBER is an optional sign, digits with at most one decimal mark, and an
  optional exponent: e or E, an optional sign, digits. A sign starts a number
  only where a number may start - at the beginning or after a separator - so
  "10-20" is two numbers and "-10" is one. An e is an exponent only when digits
  follow it, so "3eV" is the number 3 and a unit.

  ACommaIsDecimal says whether a comma is a decimal mark or a separator. A run of
  marks with no digit - a full stop ending a sentence - is punctuation, not a
  number. A run with two decimal marks, "1.2.3", is malformed: AValid is False
  and the caller skips the line, as it does a header. }
function LineNumbers(const ALine: string; ACommaIsDecimal: boolean;
    out AValid: boolean): TLineNumbers;

implementation

function IsDigit(C: char): boolean;
begin
    Result := (C >= '0') and (C <= '9');
end;

function LineNumbers(const ALine: string; ACommaIsDecimal: boolean;
    out AValid: boolean): TLineNumbers;
var
    i, Len, Start, Marks, Digits: longint;
    Text: string;
    Value: double;
    Settings: TFormatSettings;

    function IsMark(C: char): boolean;
    begin
        Result := (C = '.') or (ACommaIsDecimal and (C = ','));
    end;

begin
    Result := nil;
    AValid := True;
    Settings := DefaultFormatSettings;
    Settings.DecimalSeparator := '.';
    Len := Length(ALine);
    i := 1;
    while i <= Len do
    begin
        if not (IsDigit(ALine[i]) or IsMark(ALine[i]) or
            (((ALine[i] = '-') or (ALine[i] = '+')) and (i < Len) and
            (IsDigit(ALine[i + 1]) or IsMark(ALine[i + 1])) and
            ((i = 1) or not (IsDigit(ALine[i - 1]) or IsMark(ALine[i - 1]))))) then
        begin
            Inc(i);
            Continue;
        end;
        Start := i;
        if (ALine[i] = '-') or (ALine[i] = '+') then
            Inc(i);
        Marks := 0;
        Digits := 0;
        while (i <= Len) and (IsDigit(ALine[i]) or IsMark(ALine[i])) do
        begin
            if IsDigit(ALine[i]) then
                Inc(Digits)
            else
                Inc(Marks);
            Inc(i);
        end;
        if (Digits > 0) and (i < Len) and ((ALine[i] = 'e') or (ALine[i] = 'E')) and
            (IsDigit(ALine[i + 1]) or ((i + 1 < Len) and
            ((ALine[i + 1] = '-') or (ALine[i + 1] = '+')) and
            IsDigit(ALine[i + 2]))) then
        begin
            Inc(i, 2);
            while (i <= Len) and IsDigit(ALine[i]) do
                Inc(i);
        end;
        if Digits = 0 then
            //  Punctuation, not a number.
            Continue;
        if Marks > 1 then
        begin
            AValid := False;
            Exit;
        end;
        Text := StringReplace(Copy(ALine, Start, i - Start), ',', '.', []);
        if not TryStrToFloat(Text, Value, Settings) then
        begin
            AValid := False;
            Exit;
        end;
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Value;
    end;
end;

end.
