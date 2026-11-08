// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A table of numbers read out of lines of text: the columns, their names
and their values.)

WHY A TABLE AND NOT TWO NUMBERS PER LINE. The two-column reader (text_numbers,
dat_file_loader) takes the first two numbers on each line and nothing else,
which is right for a profile written as a profile. What public repositories
publish is mostly a TABLE - a header naming the columns, a comma or a tab or a
semicolon between the cells, often more than two columns, sometimes quoted -
and reading it as "the first two numbers" loses the names and mistakes a date
or an index for the argument. Zenodo alone holds some 94,000 datasets with a
CSV in them (counted 2026-10-20). So this reads the table as a table, and the
loaders over it - comma-separated, tab-separated, spreadsheets, whose cells are
handed over as tab-separated lines - choose the columns from it.

NOTHING HERE READS A FILE, so every decision - which delimiter, which line is
the header, which columns are numeric, whether a comma is a decimal mark - is
reachable from a test with string literals.

THE RULES, each one a choice a real file forced:

  * the DELIMITER is the one of tab, semicolon and comma that splits the most
    lines into the same number of cells (two or more); a file none of them
    splits is cut at runs of spaces, which is how instruments write columns;
  * a COMMA IS A DECIMAL MARK only when the delimiter is not a comma and the
    table writes no full stop in a number - 1,5;2,5 is one and a half;
  * the HEADER is the FIRST line of two or more cells before the first data
    line: names come first, and a units line under them ("deg", "counts") is
    not the names;
  * a DATA LINE has a number in at least two cells; a NUMERIC COLUMN holds a
    number in at least half of the data lines, so a stray "n/a" does not cost
    a column and a column of labels is not mistaken for one.
}
unit text_table;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    TTableValues = array of double;

    { The numeric columns of a table, in file order. A cell that held no
      number is NaN, so every column has one value per data line. }
    TTextTable = record
        Names: TStringArray;
        Columns: array of TTableValues;
        { How many lines held data. }
        RowCount: longint;
        { How many columns the data lines had, numeric or not - what a refusal
          names when too few of them are numbers. }
        CellCount: longint;
    end;

{ The cells of ALine split at ADelimiter; #0 means at runs of spaces and tabs.
  A cell in double quotes may hold the delimiter, and a doubled quote inside it
  is one quote. Cells are trimmed and unquoted. }
function SplitTableLine(const ALine: string; ADelimiter: char): TStringArray;

{ The delimiter ALines are written with, by the rule in the header: #9, ';',
  ',' or #0 for runs of spaces. }
function DetectTableDelimiter(ALines: TStrings): char;

{ The number in ACell, read with a full stop as the decimal mark - or a comma
  when ACommaIsDecimal - and False when the cell is not one number. }
function TryTableNumber(const ACell: string; ACommaIsDecimal: boolean;
    out AValue: double): boolean;

{ The table in ALines. A table with fewer than two numeric columns comes back
  with fewer than two, and the reader says so in its own words. }
function ParseTextTable(ALines: TStrings): TTextTable;

{ A name for numeric column AIndex (from 0) when the table had no header line:
  'Column 1', 'Column 2'... counted from one, as a user counts. }
function DefaultColumnName(AIndex: longint): string;

implementation

uses
    Math;

const
    { How many lines are looked at to decide the delimiter: enough to get past
      a short preamble, few enough that a large file is not split twice. }
    DelimiterSampleLines = 50;

function SplitTableLine(const ALine: string; ADelimiter: char): TStringArray;
var
    i, Len: longint;
    Cell: string;
    Quoted, InQuotes: boolean;

    procedure Push;
    begin
        SetLength(Result, Length(Result) + 1);
        if Quoted then
            Result[High(Result)] := Cell
        else
            Result[High(Result)] := Trim(Cell);
        Cell := '';
        Quoted := False;
    end;

begin
    Result := nil;
    Cell := '';
    Quoted := False;
    InQuotes := False;
    Len := Length(ALine);
    if ADelimiter = #0 then
    begin
        //  Runs of white space: what an instrument's columns are.
        i := 1;
        while i <= Len do
        begin
            while (i <= Len) and (ALine[i] in [' ', #9]) do
                Inc(i);
            if i > Len then
                Break;
            Cell := '';
            while (i <= Len) and not (ALine[i] in [' ', #9]) do
            begin
                Cell := Cell + ALine[i];
                Inc(i);
            end;
            Push;
        end;
        Exit;
    end;
    i := 1;
    while i <= Len do
    begin
        if InQuotes then
        begin
            if ALine[i] = '"' then
            begin
                if (i < Len) and (ALine[i + 1] = '"') then
                begin
                    Cell := Cell + '"';
                    Inc(i);
                end
                else
                    InQuotes := False;
            end
            else
                Cell := Cell + ALine[i];
        end
        else if (ALine[i] = '"') and (Trim(Cell) = '') then
        begin
            InQuotes := True;
            Quoted := True;
            Cell := '';
        end
        else if ALine[i] = ADelimiter then
            Push
        else
            Cell := Cell + ALine[i];
        Inc(i);
    end;
    Push;
end;

{ Whether ALine is worth looking at: not blank, not a comment. }
function IsContentLine(const ALine: string): boolean;
var
    Text: string;
begin
    Text := Trim(ALine);
    Result := (Text <> '') and (Text[1] <> '#') and (Text[1] <> '%');
end;

function DetectTableDelimiter(ALines: TStrings): char;
const
    Candidates: array[0..2] of char = (#9, ';', ',');
var
    c, i, Seen, Count, Mode, Best, Score: longint;
    Counts: array of longint;
    Tally: TStringList;
begin
    Result := #0;
    Best := 0;
    for c := 0 to High(Candidates) do
    begin
        Counts := nil;
        Seen := 0;
        for i := 0 to ALines.Count - 1 do
        begin
            if Seen >= DelimiterSampleLines then
                Break;
            if not IsContentLine(ALines[i]) then
                Continue;
            Inc(Seen);
            SetLength(Counts, Length(Counts) + 1);
            Counts[High(Counts)] := Length(SplitTableLine(ALines[i], Candidates[c]));
        end;
        //  THE MOST LINES SPLIT ALIKE: a delimiter is the one a table is
        //  consistently written with, not the one that happens to occur.
        Tally := TStringList.Create;
        try
            Mode := 0;
            Score := 0;
            for i := 0 to High(Counts) do
            begin
                if Counts[i] < 2 then
                    Continue;
                Count := 0;
                if Tally.IndexOfName(IntToStr(Counts[i])) >= 0 then
                    Count := StrToInt(Tally.Values[IntToStr(Counts[i])]);
                Inc(Count);
                Tally.Values[IntToStr(Counts[i])] := IntToStr(Count);
                if Count > Score then
                begin
                    Score := Count;
                    Mode := Counts[i];
                end;
            end;
        finally
            Tally.Free;
        end;
        //  Ties go to the earlier candidate: a tab is never a decimal mark, a
        //  semicolon rarely, a comma often.
        if (Mode >= 2) and (Score > Best) then
        begin
            Best := Score;
            Result := Candidates[c];
        end;
    end;
end;

function TryTableNumber(const ACell: string; ACommaIsDecimal: boolean;
    out AValue: double): boolean;
var
    Text: string;
    Settings: TFormatSettings;
    i: longint;
begin
    AValue := NaN;
    Text := Trim(ACell);
    if Text = '' then
        Exit(False);
    if ACommaIsDecimal then
        Text := StringReplace(Text, ',', '.', [rfReplaceAll]);
    //  Only a number, whole: "12 kV" and "2024-03-01" are cells of text, not
    //  numbers with something after them - unlike a line of the two-column
    //  reader, a cell has nothing else in it to separate.
    for i := 1 to Length(Text) do
        if not (Text[i] in ['0'..'9', '.', '-', '+', 'e', 'E']) then
            Exit(False);
    Settings := DefaultFormatSettings;
    Settings.DecimalSeparator := '.';
    Settings.ThousandSeparator := #0;
    Result := TryStrToFloat(Text, AValue, Settings);
    if not Result then
        AValue := NaN;
end;

function DefaultColumnName(AIndex: longint): string;
begin
    Result := 'Column ' + IntToStr(AIndex + 1);
end;

function ParseTextTable(ALines: TStrings): TTextTable;
var
    Delimiter: char;
    CommaIsDecimal: boolean;
    Cells, Header: TStringArray;
    Rows: array of TStringArray;
    i, j, Numbers, Width, NumericCount: longint;
    Value: double;
    Counts: array of longint;
    Numeric: array of boolean;
    SeenFullStop, SeenComma: boolean;
begin
    Result := Default(TTextTable);
    Delimiter := DetectTableDelimiter(ALines);

    //  A COMMA IS A DECIMAL MARK only where it cannot be the delimiter and no
    //  number in the table is written with a full stop.
    SeenFullStop := False;
    SeenComma := False;
    if Delimiter <> ',' then
        for i := 0 to ALines.Count - 1 do
        begin
            if not IsContentLine(ALines[i]) then
                Continue;
            Cells := SplitTableLine(ALines[i], Delimiter);
            for j := 0 to High(Cells) do
            begin
                if TryTableNumber(Cells[j], False, Value) and (Pos('.', Cells[j]) > 0) then
                    SeenFullStop := True;
                if (Pos(',', Cells[j]) > 0) and
                    TryTableNumber(Cells[j], True, Value) then
                    SeenComma := True;
            end;
        end;
    CommaIsDecimal := (Delimiter <> ',') and SeenComma and not SeenFullStop;

    //  The data lines, and the header: the first line of two or more cells
    //  before the first data line.
    Rows := nil;
    Header := nil;
    for i := 0 to ALines.Count - 1 do
    begin
        if not IsContentLine(ALines[i]) then
            Continue;
        Cells := SplitTableLine(ALines[i], Delimiter);
        Numbers := 0;
        for j := 0 to High(Cells) do
            if TryTableNumber(Cells[j], CommaIsDecimal, Value) then
                Inc(Numbers);
        if Numbers >= 2 then
        begin
            SetLength(Rows, Length(Rows) + 1);
            Rows[High(Rows)] := Cells;
        end
        else if (Length(Rows) = 0) and (Header = nil) and (Length(Cells) >= 2) then
            Header := Cells;
    end;
    Result.RowCount := Length(Rows);
    if Length(Rows) = 0 then
        Exit;

    //  The width the data has: its commonest cell count, so one ragged line
    //  does not decide it.
    Width := 0;
    for i := 0 to High(Rows) do
        if Length(Rows[i]) > Width then
            Width := Length(Rows[i]);
    Result.CellCount := Width;

    Counts := nil;
    SetLength(Counts, Width);
    for i := 0 to High(Rows) do
        for j := 0 to High(Rows[i]) do
            if TryTableNumber(Rows[i][j], CommaIsDecimal, Value) then
                Inc(Counts[j]);
    Numeric := nil;
    SetLength(Numeric, Width);
    NumericCount := 0;
    for j := 0 to Width - 1 do
    begin
        Numeric[j] := 2 * Counts[j] >= Length(Rows);
        if Numeric[j] then
            Inc(NumericCount);
    end;

    SetLength(Result.Names, NumericCount);
    SetLength(Result.Columns, NumericCount);
    NumericCount := 0;
    for j := 0 to Width - 1 do
    begin
        if not Numeric[j] then
            Continue;
        if (j <= High(Header)) and (Trim(Header[j]) <> '') then
            Result.Names[NumericCount] := Trim(Header[j])
        else
            Result.Names[NumericCount] := DefaultColumnName(j);
        SetLength(Result.Columns[NumericCount], Length(Rows));
        for i := 0 to High(Rows) do
            if (j > High(Rows[i])) or
                not TryTableNumber(Rows[i][j], CommaIsDecimal, Value) then
                Result.Columns[NumericCount][i] := NaN
            else
                Result.Columns[NumericCount][i] := Value;
        Inc(NumericCount);
    end;
end;

end.
