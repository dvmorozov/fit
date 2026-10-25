// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Reads a table of numbers - comma- or tab-separated text, or a
spreadsheet's cells - as a profile.)

Copyright (C) Dmitry Morozov

A STUB UNTIL 2026-10-20, raising "not implemented" from the one method it had.
Public repositories publish tables - Zenodo alone holds some 94,000 datasets
with a CSV in them - and a search that cannot open one shows almost nothing.

THE TABLE IS text_table's; this unit only decides what of it is the profile:

  * the FIRST numeric column is the argument and the SECOND is the value - the
    order a table of a measurement is written in, and the order the two-column
    reader already reads;
  * every FURTHER numeric column is kept beside the profile, by its header's
    name (sample_columns), so nothing the file held is lost and another column
    can be chosen as the value (the data source wizard offers that);
  * a row without both numbers is skipped, and a repeated argument keeps its
    first row - the two-column reader's rules, so a table and a .dat of the
    same numbers read the same.

A spreadsheet reader descends from this one and hands its cells over as
tab-separated lines (LoadDataSetActually), so there is one answer to "which
columns of this table are the profile".
}
unit csv_file_loader;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, data_loader, SysUtils, sample_columns, text_table;

type
    { Loads a table: the first two numeric columns are the profile, the rest
      are kept beside it by name. }
    TCSVFileLoader = class(TDataLoader)
    private
        FValueName: string;
        FColumns: TSampleColumns;
    protected
        procedure ParseLines(ALines: TStrings); override;
    public
        { The header of the column read as the value - 'intensity' - or
          'Column 2' when the table names none. }
        function ValueName: string; override;
        { Every further numeric column, by name, one value per point kept. }
        function SampleColumns: TSampleColumns; override;
    end;

implementation

uses
    Math, checks;

//  `uses app` REMOVED, not tidied. app.pas uses Forms and its
//  initialization constructs a desktop client application object plus an
//  HTTP client aimed at the default server URL. This unit referenced
//  neither identifier app.pas exports, so the clause bought nothing and
//  cost the LCL - and, in the compute server, a client of itself built on
//  every start-up. See docs/contributing/findings.md.

{============================== TCSVFileLoader ================================}

procedure TCSVFileLoader.ParseLines(ALines: TStrings);
var
    Table: TTextTable;
    Kept: array of longint;
    i, j: longint;
begin
    CheckAssigned(ALines, 'the lines of the table being parsed');
    CheckAssigned(FPointsSet, 'the points set the table is parsed into');
    FPointsSet.Clear;
    FColumns := nil;
    FValueName := '';

    Table := ParseTextTable(ALines);
    if Table.RowCount = 0 then
        raise EInvalidFileFormat.Create(FSourceName + ' is not a table of ' +
            'numbers: Fit needs at least two columns of numbers, and found no ' +
            'line holding two.');
    if Length(Table.Columns) < 2 then
        raise EInvalidFileFormat.Create(FSourceName + ' has ' +
            IntToStr(Length(Table.Columns)) + ' column(s) of numbers among ' +
            IntToStr(Table.CellCount) + ', and Fit needs at least two columns ' +
            'of numbers: an argument and a value.');

    FValueName := Table.Names[1];
    Kept := nil;
    for i := 0 to Table.RowCount - 1 do
    begin
        if IsNan(Table.Columns[0][i]) or IsNan(Table.Columns[1][i]) then
            Continue;
        //  Duplicates by argument value are ignored: the first one wins, as the
        //  two-column reader does.
        if FPointsSet.IndexOfValueX(Table.Columns[0][i]) <> -1 then
            Continue;
        FPointsSet.AddNewPoint(Table.Columns[0][i], Table.Columns[1][i]);
        SetLength(Kept, Length(Kept) + 1);
        Kept[High(Kept)] := i;
    end;

    //  THE OTHER COLUMNS, aligned with the points that were kept: a sample's
    //  index is what a column is read by.
    SetLength(FColumns, Length(Table.Columns) - 2);
    for j := 2 to High(Table.Columns) do
    begin
        FColumns[j - 2].Name := Table.Names[j];
        SetLength(FColumns[j - 2].Values, Length(Kept));
        for i := 0 to High(Kept) do
            FColumns[j - 2].Values[i] := Table.Columns[j][Kept[i]];
    end;
end;

function TCSVFileLoader.ValueName: string;
begin
    Result := FValueName;
end;

function TCSVFileLoader.SampleColumns: TSampleColumns;
begin
    Result := CopySampleColumns(FColumns);
end;

end.
