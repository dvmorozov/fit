// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which rows of the legend a script's "show this" or "hide this" means.)

A SCRIPT NAMES AN ENTRY BY THE WORDS THE LEGEND SHOWS, which is what a person
reads before ticking its box. Captions rather than a kind of series: a module's
series have no kind the framework knows, and the one thing every row has is the
words it is listed under. A name stands for EVERY row carrying it, because a
module draws one series per pattern under one title, and a script that wanted
every pattern's markers gone should not have to know how many patterns there are.
Whole captions, not parts of them: 'Difference' and 'Difference not noise' are
two entries.
}
unit legend_entries;

{$mode objfpc}{$H+}

interface

type
    TLegendRowList = array of longint;

{ The rows whose caption is AName. }
function LegendRowsCaptioned(const ACaptions: array of string;
    const AName: string): TLegendRowList;

implementation

uses
    SysUtils;

function LegendRowsCaptioned(const ACaptions: array of string;
    const AName: string): TLegendRowList;
var
    i: longint;
    Wanted: string;
begin
    Result := nil;
    Wanted := LowerCase(Trim(AName));
    if Wanted = '' then
        Exit;
    for i := 0 to High(ACaptions) do
        if LowerCase(Trim(ACaptions[i])) = Wanted then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := i;
        end;
end;

end.
