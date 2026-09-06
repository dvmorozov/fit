// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the chart's colours a curve is drawn in.)

ONE LINE, AND IT WAS WRONG. The chart holds sixteen colours, indexed one to
sixteen, and each curve took the one at its own position:

    if Index <= 16 then Palette[Index] else Palette[Index mod 16]

`Index` counts from one, so curves 1 to 16 are fine and 17 to 31 wrap onto 1 to
15. Curve 32 gives `32 mod 16` = 0 - and there is no colour zero. The read is
outside the array, and so is every thirty-second curve after it. Nothing catches
it in a build without range checking: the series is drawn in whatever integer
happened to sit before the palette in memory.

Thirty-two curves is not a hypothetical. Selecting every sample of a coarse
profile as a curve position seeds one curve per sample, which is what that
command is for.

The rule is one function now because that is what makes it assertable at all: it
was a conditional expression inside a nested procedure inside a method that takes
a chart.
}
unit series_palette;

{$mode objfpc}{$H+}

interface

const
    { How many colours the chart cycles through. The palette itself stays with
      the chart - a colour is a widget-set value - and this is its size, which is
      the only part of it the arithmetic needs. }
    SeriesColorCount = 16;

{ Which colour, numbered from one, a curve at ACurveIndex is drawn in.

  ALWAYS IN RANGE, for any input including zero and negatives: an index this
  cannot answer for is an index that would be read out of the palette, and the
  caller has no way to tell that it happened. }
function SeriesColorIndex(ACurveIndex: longint): longint;

implementation

function SeriesColorIndex(ACurveIndex: longint): longint;
begin
    //  Shifted down to a zero-based position, wrapped, and shifted back. Doing
    //  it the other way round - wrapping the one-based number directly - is what
    //  produced a zero, because the multiples of the count map onto it.
    Result := ACurveIndex - 1;
    //  A negative index has no meaning, but it must not produce a negative
    //  remainder either: Pascal's mod keeps the sign of the dividend.
    Result := Result mod SeriesColorCount;
    if Result < 0 then
        Result := Result + SeriesColorCount;
    Result := Result + 1;
end;

end.
