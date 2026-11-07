// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains implementation of class loading data from DAT-files.)

Copyright (C) Dmitry Morozov
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
      of position and values.

      NOTHING HERE IS ABOUT DIFFRACTION, and the registry registers this class
      for '.XY' and '.TXT' as well as for '.DAT' for that reason: two numbers
      per line and anything else skipped is the commonest interchange shape
      there is. The line scanner it reads them with is text_numbers. }
    TDATFileLoader = class(TDataLoader)
    protected
        procedure ParseLines(ALines: TStrings); override;
    end;

implementation


uses
    checks, text_numbers;

{============================== TDATFileLoader ================================}

{ Whether ALine starts, after its indentation, with what can start a number.

  A DATA LINE BEGINS WITH ITS ARGUMENT. A line that begins with a word or a
  comment mark is a header however many numbers it holds: FullProf's
  "! Monitor/counts, ... 65000.000 65000.000" was read as the point
  (65000, 65000) and drew a whole powder pattern as a straight line. Trailing
  text after the numbers ("3eV 7", "3.0 3377.0 58.1 1 extra") stays data -
  units and notes are written after values, never before them. }
function StartsWithNumber(const ALine: string): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 1 to Length(ALine) do
        if not (ALine[i] in [' ', #9]) then
            Exit(ALine[i] in ['0'..'9', '+', '-', '.', ',']);
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
        if not StartsWithNumber(Str) then
            Continue;
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
