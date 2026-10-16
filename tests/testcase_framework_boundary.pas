// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(That the framework's own sources name no field's data.)

THE RULE. A module covers one class of fitting tasks and brings everything that
class needs; the framework hosts. Price data - the OHLC reader, the Bar, Date and
Price axis modes, and the choice of which column is read - is the price-series
module's: the public build showed "Price" under Value Transformation and a Price
Data menu to users with no price series and no module to analyse one.

WHY A SOURCE SCAN. What is wrong is that a name is PRESENT in the framework's
tree: no registry query can see it, because in a build that also links the module
the registries hold the same things either way. So the absence itself is what is
asserted, over the directories the framework ships - Modules/ is excluded, since
a module in this tree is a module.

Diffraction is the other field still here, and is not listed: it is being moved
out in stages (docs/internal/diffraction-extraction.md), and its names join this
list when its last unit leaves.
}
unit testcase_framework_boundary;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, StrUtils, fpcunit, testregistry, source_scan;

type
    TFrameworkBoundaryTest = class(TTestCase)
    published
        procedure TheFrameworkNamesNoPriceData;
    end;

implementation

const
    { What the price-data feature is made of, by the names that would have to
      appear for the framework to be using it. }
    PriceDataNames: array[0..5] of string = (
        'ohlc_csv_loader', 'price_axis_modes', 'TOHLCFileLoader',
        'RegisterPriceAxisModes', 'PriceDataTopic', 'BuildPriceDataMenu');

    FrameworkDirs: array[0..3] of string = ('Desktop', 'Common', 'Server', 'Worker');

function NamesWord(const AText, AWord: string): boolean;
var
    Text, Word_: string;
    k, After: integer;
begin
    //  As a whole identifier: 'price_axis_modes_test' is not the unit.
    Text := LowerCase(AText);
    Word_ := LowerCase(AWord);
    k := PosEx(Word_, Text, 1);
    while k > 0 do
    begin
        After := k + Length(Word_);
        if ((k = 1) or not (Text[k - 1] in ['a'..'z', '0'..'9', '_'])) and
            ((After > Length(Text)) or
            not (Text[After] in ['a'..'z', '0'..'9', '_'])) then
            Exit(True);
        k := PosEx(Word_, Text, k + 1);
    end;
    Result := False;
end;

procedure TFrameworkBoundaryTest.TheFrameworkNamesNoPriceData;
var
    Root: string;
    Files, Offenders, Source: TStringList;
    i, d, n: integer;
begin
    Root := RepoRoot;
    AssertTrue('the repository root was found - a scan of nothing is not a pass',
        Root <> '');
    Files := TStringList.Create;
    Offenders := TStringList.Create;
    Source := TStringList.Create;
    try
        for d := 0 to High(FrameworkDirs) do
            CollectPascalSources(Root + FrameworkDirs[d], Files);
        AssertTrue('the framework''s sources were found', Files.Count > 0);
        for i := 0 to Files.Count - 1 do
        begin
            Source.LoadFromFile(Files[i]);
            Source.Text := StripCommentsAndStrings(Source.Text);
            for n := 0 to High(PriceDataNames) do
                if NamesWord(Source.Text, PriceDataNames[n]) then
                    Offenders.Add(ExtractRelativePath(Root, Files[i]) + ' names ' +
                        PriceDataNames[n]);
        end;
        AssertEquals('price data is its module''s, not the framework''s: ' +
            Offenders.Text, 0, Offenders.Count);
    finally
        Source.Free;
        Offenders.Free;
        Files.Free;
    end;
end;

initialization
    //  A unit test, as testcase_no_pascal_labels is: it reads sources already
    //  on disk beside it and writes nothing.
    RegisterTest('unit', TFrameworkBoundaryTest);
end.
