// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Values a loader read beside the one it plots, kept per sample for
whichever module draws with them.)

WHY THE FRAMEWORK CARRIES THEM, AND KNOWS NOTHING ABOUT THEM. A loader turns a
file into a profile - one value per sample - and some files hold more than one:
a price bar has an open, a high and a low beside the close it plots. What those
mean is the field's business, not the framework's, so they cross it as named
columns of numbers, by the sample's index, exactly as the bar dates already do
(ArgumentDates): the loader hands them over, the client keeps them with the
profile, the project saves them, and a module asks for them by name.

SAVED WITH THE PROJECT for the reason the dates are: a project stores the
profile, not the file, and nothing reads the file again - without them a
reopened price series would have lost its bars.
}
unit sample_columns;

{$mode objfpc}{$H+}

interface

type
    TDoubleArray = array of double;

    { One column: a name the loader gave it, and a value per sample. }
    TSampleColumn = record
        Name: string;
        Values: TDoubleArray;
    end;

    TSampleColumns = array of TSampleColumn;

{ The column called AName, when there is one. }
function SampleColumnNamed(const AColumns: TSampleColumns; const AName: string;
    out AValues: TDoubleArray): boolean;

{ A copy that shares nothing with AColumns. }
function CopySampleColumns(const AColumns: TSampleColumns): TSampleColumns;

{ Whether A and B hold the same columns, in the same order, under the same
  names, with the same values exactly. }
function SameSampleColumns(const A, B: TSampleColumns): boolean;

implementation

uses
    SysUtils;

function SampleColumnNamed(const AColumns: TSampleColumns; const AName: string;
    out AValues: TDoubleArray): boolean;
var
    i: integer;
begin
    AValues := nil;
    for i := 0 to High(AColumns) do
        if SameText(AColumns[i].Name, AName) then
        begin
            AValues := AColumns[i].Values;
            Exit(True);
        end;
    Result := False;
end;

function CopySampleColumns(const AColumns: TSampleColumns): TSampleColumns;
var
    i: integer;
begin
    SetLength(Result, Length(AColumns));
    for i := 0 to High(AColumns) do
    begin
        Result[i].Name := AColumns[i].Name;
        Result[i].Values := Copy(AColumns[i].Values);
    end;
end;

function SameSampleColumns(const A, B: TSampleColumns): boolean;
var
    i, j: integer;
begin
    Result := False;
    if Length(A) <> Length(B) then
        Exit;
    for i := 0 to High(A) do
    begin
        if (A[i].Name <> B[i].Name) or
            (Length(A[i].Values) <> Length(B[i].Values)) then
            Exit;
        for j := 0 to High(A[i].Values) do
            //  EXACTLY: a value read from a file and put back from a project is
            //  the same double, and one that is not is a different column.
            if A[i].Values[j] <> B[i].Values[j] then
                Exit;
    end;
    Result := True;
end;

end.
