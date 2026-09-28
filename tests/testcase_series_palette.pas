// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the chart's colours a curve is drawn in.)

ONE LINE, AND IT WAS WRONG FOR EVERY THIRTY-SECOND CURVE. The palette is indexed
one to sixteen and the old rule wrapped with `Index mod 16`, which gives zero at
32, 48, 64 - outside the array, read without complaint in a build with range
checking off, and drawn in whatever integer sat before the palette in memory.

Thirty-two curves is ordinary: selecting every sample of a coarse profile as a
curve position seeds one curve per sample, which is what that command exists for.

The test that matters here is the sweep. A wrapping rule is exactly the kind of
arithmetic that is right for the cases anyone thinks to try by hand and wrong at
the boundary nobody reaches until a user does.
}
unit testcase_series_palette;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, series_palette;

type
    TSeriesPaletteTest = class(TTestCase)
    published
        procedure TheFirstCurvesTakeTheColoursInOrder;
        procedure TheLastColourIsReached;
        procedure ThePaletteWrapsAfterItsLastColour;
        procedure TheWrapIsNeverZero;
        procedure EveryIndexIsInRange;
        procedure EveryColourIsUsedBeforeAnyIsRepeated;
        procedure CurvesOneApartNeverShareAColour;
        procedure NonsenseIndicesAreStillInRange;
    end;

implementation

procedure TSeriesPaletteTest.TheFirstCurvesTakeTheColoursInOrder;
begin
    //  Curves are numbered from one, and so is the palette.
    AssertEquals('the first', 1, SeriesColorIndex(1));
    AssertEquals('the second', 2, SeriesColorIndex(2));
    AssertEquals('the third', 3, SeriesColorIndex(3));
end;

procedure TSeriesPaletteTest.TheLastColourIsReached;
begin
    //  The sixteenth curve gets the sixteenth colour, not the first: a wrap one
    //  step early wastes a colour and makes two curves match sooner.
    AssertEquals('the last', SeriesColorCount,
        SeriesColorIndex(SeriesColorCount));
end;

procedure TSeriesPaletteTest.ThePaletteWrapsAfterItsLastColour;
begin
    AssertEquals('back to the first', 1,
        SeriesColorIndex(SeriesColorCount + 1));
    AssertEquals('and on to the second', 2,
        SeriesColorIndex(SeriesColorCount + 2));
end;

procedure TSeriesPaletteTest.TheWrapIsNeverZero;
begin
    //  THE DEFECT THIS REPLACED, named. Twice the palette size gave zero under
    //  the old rule, and there is no colour zero.
    AssertEquals('twice round', SeriesColorCount,
        SeriesColorIndex(2 * SeriesColorCount));
    AssertEquals('three times round', SeriesColorCount,
        SeriesColorIndex(3 * SeriesColorCount));
end;

procedure TSeriesPaletteTest.EveryIndexIsInRange;
var
    i, Index: longint;
begin
    //  THE SWEEP. A hundred curves is more than any model should have and less
    //  than a coarse profile with a position on every sample produces.
    for i := 1 to 200 do
    begin
        Index := SeriesColorIndex(i);
        AssertTrue(Format('curve %d gets colour %d, which is in the palette',
            [i, Index]),
            (Index >= 1) and (Index <= SeriesColorCount));
    end;
end;

procedure TSeriesPaletteTest.EveryColourIsUsedBeforeAnyIsRepeated;
var
    Seen: array of boolean;
    i, Index: longint;
begin
    //  Sixteen curves must use sixteen different colours. A rule that repeated
    //  one early would make two curves indistinguishable on the chart while
    //  another colour went unused.
    SetLength(Seen, SeriesColorCount + 1);
    for i := 1 to SeriesColorCount do
    begin
        Index := SeriesColorIndex(i);
        AssertFalse(Format('colour %d is not already taken', [Index]),
            Seen[Index]);
        Seen[Index] := True;
    end;
    for i := 1 to SeriesColorCount do
        AssertTrue(Format('colour %d was used', [i]), Seen[i]);
end;

procedure TSeriesPaletteTest.CurvesOneApartNeverShareAColour;
var
    i: longint;
begin
    //  Adjacent curves are the ones most likely to overlap on the chart, so they
    //  are the pair it matters most to tell apart.
    for i := 1 to 200 do
        AssertTrue(Format('curves %d and %d differ', [i, i + 1]),
            SeriesColorIndex(i) <> SeriesColorIndex(i + 1));
end;

procedure TSeriesPaletteTest.NonsenseIndicesAreStillInRange;
var
    Index: longint;
begin
    //  Zero and negatives have no meaning as curve numbers, and neither has an
    //  answer outside the palette. Pascal's `mod` keeps the sign of its
    //  dividend, so a negative index is exactly how a wrapping rule produces a
    //  negative subscript.
    Index := SeriesColorIndex(0);
    AssertTrue(Format('zero gives %d', [Index]),
        (Index >= 1) and (Index <= SeriesColorCount));
    Index := SeriesColorIndex(-1);
    AssertTrue(Format('minus one gives %d', [Index]),
        (Index >= 1) and (Index <= SeriesColorCount));
    Index := SeriesColorIndex(-SeriesColorCount);
    AssertTrue(Format('minus the count gives %d', [Index]),
        (Index >= 1) and (Index <= SeriesColorCount));
end;

initialization
    //  A unit test: a number in, a number out. No chart.
    RegisterTest('unit', TSeriesPaletteTest);
end.
