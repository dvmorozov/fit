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
  'All supported|*.dat;*.xy|Diffraction profile|*.dat|...'. Built from the
  registry rather than written out in the form, so what the dialog offers and
  what the build can open cannot drift apart. }
function DataLoaderDialogFilter: string;

implementation

uses
    dat_file_loader;

var
    Registered: boolean = False;

procedure RegisterAllDataLoaders;
begin
    if Registered then
        Exit;

    //  NO COORDINATES DECLARED, although the format is named for diffraction:
    //  '.dat' is what every two-column series is saved as - the price samples
    //  of the wave-count pack among them - and declaring 2 Theta here captioned
    //  a price series in scattering angle. A diffraction model says so itself
    //  (the peak shapes' PreferredAxisMode), as it always did.
    RegisterDataLoader(TDATFileLoader, '.DAT', 'Diffraction profile');
    //  THE SAME READER, REGISTERED AGAIN, for the shape it has always actually
    //  read: two numbers per line, everything else skipped. That is what '.xy'
    //  and a two-column '.txt' are, and what a repository or a spectrometer
    //  hands over. A second parser for the same shape would be a second set of
    //  answers to the comma-and-exponent questions text_numbers already settles.
    //  Registered separately from '.DAT' rather than as one claim, because the
    //  two are named differently in the dialog and because Stage 4 moves the
    //  diffraction one alone into a module.
    RegisterDataLoader(TDATFileLoader, '.XY;.TXT', 'Two-column numeric');
    //  NO .CSV. Price data is a module's - the one that analyses price
    //  series - which registers its reader
    //  from its own front door; a general comma-separated table has no reader
    //  yet (csv_file_loader is still a stub).

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
