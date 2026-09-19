// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of class loading data from DAT-files.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit dat_file_loader;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, data_loader, SysUtils;

type
    { Loads data from ordinary DAT-file consisting from lines having pairs
      of position and values. }
    TDATFileLoader = class(TDataLoader)
    protected
        procedure ParseLines(ALines: TStrings); override;
    end;

implementation


uses
    checks;

{============================== TDATFileLoader ================================}

type
    TLineNumbers = array of double;

function IsDigit(C: char): boolean;
begin
    Result := (C >= '0') and (C <= '9');
end;

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

procedure TDATFileLoader.ParseLines(ALines: TStrings);
var
    Numbers: TLineNumbers;
    Str: string;
    i: longint;
    Valid: boolean;
begin
    CheckAssigned(ALines, 'the lines of the DAT file being parsed');
    CheckAssigned(FPointsSet, 'the points set the DAT file is parsed into');

    FPointsSet.Clear;
    for i := 0 to ALines.Count - 1 do
    begin
        { The first column - X (argument), the second - Y (value); any further
          columns are ignored. Columns are separated by anything that is not
          part of a number. }
        Str := ALines.Strings[i];
        //  A COMMA IS A DECIMAL MARK only on a line that writes no decimal
        //  point, and only while that still leaves two columns: "1,5;2,5" is
        //  one and a half and two and a half, "12,40" and "1.5,2.5" are pairs.
        Numbers := LineNumbers(Str, Pos('.', Str) = 0, Valid);
        if (not Valid or (Length(Numbers) < 2)) and (Pos(',', Str) > 0) and
            (Pos('.', Str) = 0) then
            Numbers := LineNumbers(Str, False, Valid);
        //  A line with fewer than two numbers, or with a malformed one, is not
        //  a data line: a header, a comment, a blank.
        if not Valid or (Length(Numbers) < 2) then
            Continue;
        //  Duplicates by argument value are ignored: the first one wins.
        if FPointsSet.IndexOfValueX(Numbers[0]) = -1 then
            FPointsSet.AddNewPoint(Numbers[0], Numbers[1]);
    end;
end;

end.
