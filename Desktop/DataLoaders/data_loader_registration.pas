// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one place that says which file formats this build ships.)

Second application of the shape curve_type_registration established, and the
reason that shape is a mechanism rather than a one-off: a loader unit is linked
only if something references it, and nothing connected "the file dialog offers
.CSV" to "this binary can read .CSV".

So: this unit references every loader, which makes linking a stated dependency in
a file whose whole purpose is to state it; and RegisterAllDataLoaders VERIFIES
the outcome, naming what is missing. Deleting a line here breaks the build at the
call site instead of quietly removing a format.

A MODULE'S loaders are not listed here - they arrive through the module's own
registration unit, which calls RegisterDataLoader for its formats. This file is
the framework's own set, and it is what the public build ships.
}
unit data_loader_registration;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_loader_registry;

{ Registers every loader this build ships and checks the result. Idempotent, so
  a second call - a test, a re-initialised client - is harmless rather than a
  duplicate-extension failure.

  Called at start-up by whatever opens files. Not from an initialization
  section: that runs only if the unit is linked, which is the property being
  checked, so it cannot be the thing that does the checking. }
procedure RegisterAllDataLoaders;

{ The formats RegisterAllDataLoaders registers, by the name a dialog shows. No
  field's - a test holds them to that. }
function CoreDataLoaderFormats: TStringArray;

{ The extensions the framework's two-column reading claims: '.XY' and '.TXT',
  and '.DAT' too unless a module has already claimed it (ADatClaimed) - the
  scientific module claims it as a diffraction profile. '.dat' is what every
  two-column series is saved as, so a build in which nothing claims it still
  reads one. }
function CoreDataLoaderExtensions(ADatClaimed: boolean): string;

{ The extensions the framework's table reader claims, as a DEFAULT: a module
  that reads one more specifically - a price-analysis module reads a .csv as
  price bars - takes it over (RegisterDefaultDataLoader). }
function CoreTableExtensions: string;

{ The extensions the framework's spreadsheet reader claims, as a default. }
function CoreSpreadsheetExtensions: string;

{ The formats registered, as a dialog filter:
  'All supported|*.dat;*.xy|Diffraction profile|*.dat|...'. Built from the
  registry rather than written out in the form, so what the dialog offers and
  what the build can open cannot drift apart. }
function DataLoaderDialogFilter: string;

implementation

uses
    dat_file_loader, csv_file_loader, spreadsheet_file_loader;

var
    Registered: boolean = False;

procedure RegisterAllDataLoaders;
begin
    if Registered then
        Exit;

    //  '.DAT' AS A DIFFRACTION PROFILE IS THE SCIENTIFIC MODULE'S, registered
    //  from its front door (scientific_module) before this runs, so the dialog
    //  still lists it first where the module is linked. The reader is this
    //  one; only the claim moved, because a format belongs to the field that
    //  names it and the framework claims no field's format.
    //
    //  THE SAME READER, REGISTERED HERE, for the shape it has always actually
    //  read: two numbers per line, everything else skipped. That is what '.xy'
    //  and a two-column '.txt' are, and what a repository or a spectrometer
    //  hands over. A second parser for the same shape would be a second set of
    //  answers to the comma-and-exponent questions text_numbers already settles.
    //  AND '.DAT' WITH THEM WHEN NO MODULE CLAIMED IT: every two-column series
    //  is saved as one - a price series among them - and a build without the
    //  scientific module must still read it. Where that module is linked it
    //  has claimed '.DAT' as a diffraction profile, named so in the dialog.
    RegisterDataLoader(TDATFileLoader,
        CoreDataLoaderExtensions(Assigned(FindDataLoaderClass('profile.dat'))),
        CoreDataLoaderFormats[0]);
    //  A TABLE OF NUMBERS, comma- or tab-separated (2026-10-20): what public
    //  repositories publish most. A DEFAULT, not a claim: a price-analysis
    //  module reads '.CSV' as price bars, and its claim must win whichever of
    //  the two registers first - RegisterDefaultDataLoader says why the '.DAT'
    //  rule above ("unless claimed") is not enough for that.
    RegisterDefaultDataLoader(TCSVFileLoader, CoreTableExtensions,
        CoreDataLoaderFormats[1]);
    //  A WORKBOOK'S FIRST SHEET, as the same table (2026-10-20): the third
    //  commonest kind of file in published datasets. No .xls - the legacy
    //  binary format needs a spreadsheet library (spreadsheet_sheet says why
    //  there is none).
    RegisterDefaultDataLoader(TSpreadsheetFileLoader, CoreSpreadsheetExtensions,
        CoreDataLoaderFormats[2]);
    DescribeDataFormat(CoreDataLoaderFormats[0], 'plain text, the first two ' +
        'numbers of each line read as the argument and the value');
    DescribeDataFormat(CoreDataLoaderFormats[1], 'a table with named columns, ' +
        'the first two columns of numbers read as the argument and the value');
    DescribeDataFormat(CoreDataLoaderFormats[2], 'a workbook whose first sheet ' +
        'is read as a table of numbers');

    Registered := True;
end;

function CoreDataLoaderFormats: TStringArray;
begin
    Result := nil;
    SetLength(Result, 3);
    Result[0] := 'Two-column numeric';
    Result[1] := 'Table of numbers';
    Result[2] := 'Spreadsheet, first sheet';
end;

function CoreTableExtensions: string;
begin
    Result := '.CSV;.TSV';
end;

function CoreSpreadsheetExtensions: string;
begin
    Result := '.XLSX;.ODS';
end;

function CoreDataLoaderExtensions(ADatClaimed: boolean): string;
begin
    if ADatClaimed then
        Result := '.XY;.TXT'
    else
        Result := '.DAT;.XY;.TXT';
end;

function DataLoaderDialogFilter: string;
var
    All, PerFormat: string;
    Loaders: TDataLoaderInfoArray;
    Masks: TStringList;
    i, j: longint;
begin
    Result := '';
    All := '';
    PerFormat := '';
    Loaders := RegisteredDataLoaders;
    Masks := TStringList.Create;
    try
        for i := 0 to High(Loaders) do
        begin
            Masks.Delimiter := ';';
            Masks.StrictDelimiter := True;
            Masks.DelimitedText := Loaders[i].Extensions;
            for j := 0 to Masks.Count - 1 do
            begin
                if All <> '' then
                    All := All + ';';
                All := All + '*' + LowerCase(Masks[j]);
            end;

            if PerFormat <> '' then
                PerFormat := PerFormat + '|';
            PerFormat := PerFormat + Loaders[i].FormatName + '|';
            for j := 0 to Masks.Count - 1 do
            begin
                if j > 0 then
                    PerFormat := PerFormat + ';';
                PerFormat := PerFormat + '*' + LowerCase(Masks[j]);
            end;
        end;

        if All = '' then
            Exit;
        Result := 'All supported|' + All;
        if PerFormat <> '' then
            Result := Result + '|' + PerFormat;
        Result := Result + '|All files|*.*';
    finally
        Masks.Free;
    end;
end;

end.
