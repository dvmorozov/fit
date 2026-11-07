// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which legend rows a script names.)

A SCRIPT NAMES AN ENTRY BY THE WORDS THE LEGEND SHOWS, and a legend can show the
same words on many rows - a module draws one series per pattern under one title
- so a name stands for every row that carries it. A person ticks one box; a
script that wanted every pattern's markers gone would otherwise have to know how
many patterns there were.
}
unit testcase_legend_entries;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, legend_entries;

type
    TLegendEntriesTest = class(TTestCase)
    published
        procedure ANameStandsForEveryRowThatCarriesIt;
        procedure CaseAndSurroundingSpacesDoNotMatter;
        procedure ANameNoRowCarriesIsNoRows;
        procedure ARowWhoseCaptionOnlyContainsTheNameIsNotTheEntry;
        procedure AnEmptyNameNamesNothing;
    end;

implementation

const
    Captions: array[0..4] of string = ('Profile', 'Module markers',
        'Impulse 1', 'Module markers', 'Difference not noise');

procedure TLegendEntriesTest.ANameStandsForEveryRowThatCarriesIt;
var
    Rows: TLegendRowList;
begin
    Rows := LegendRowsCaptioned(Captions, 'Module markers');
    AssertEquals(2, Length(Rows));
    AssertEquals(1, Rows[0]);
    AssertEquals(3, Rows[1]);
end;

procedure TLegendEntriesTest.CaseAndSurroundingSpacesDoNotMatter;
begin
    AssertEquals(1, Length(LegendRowsCaptioned(Captions, '  profile ')));
end;

procedure TLegendEntriesTest.ANameNoRowCarriesIsNoRows;
begin
    AssertEquals(0, Length(LegendRowsCaptioned(Captions, 'Fit intervals')));
end;

procedure TLegendEntriesTest.ARowWhoseCaptionOnlyContainsTheNameIsNotTheEntry;
begin
    //  'Difference' must not take 'Difference not noise' with it: they are two
    //  entries, and a script that wants both says both.
    AssertEquals(0, Length(LegendRowsCaptioned(Captions, 'Difference')));
end;

procedure TLegendEntriesTest.AnEmptyNameNamesNothing;
begin
    AssertEquals(0, Length(LegendRowsCaptioned(['', 'x'], '')));
end;

initialization
    RegisterTest('unit', TLegendEntriesTest);
end.
