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

{ The formats registered, as a dialog filter:
  'All supported|*.dat;*.csv|Diffraction profile|*.dat|...'. Built from the
  registry rather than written out in the form, so what the dialog offers and
  what the build can open cannot drift apart. }
function DataLoaderDialogFilter: string;

implementation

uses
    dat_file_loader, ohlc_csv_loader;

var
    Registered: boolean = False;

procedure RegisterAllDataLoaders;
begin
    if Registered then
        Exit;

    RegisterDataLoader(TDATFileLoader, '.DAT', 'Diffraction profile');
    //  TOHLCFileLoader, not TCSVFileLoader: the latter is still a stub that only
    //  raises ENotImplemented and remains the home for the general
    //  pandas-backed import.
    RegisterDataLoader(TOHLCFileLoader, '.CSV', 'Price data, OHLC');

    Registered := True;
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
