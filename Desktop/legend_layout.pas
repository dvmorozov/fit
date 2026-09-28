// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where the pieces of a legend row sit.)

THE LEGEND IS OWNER-DRAWN, which means the application paints the whole row: a
check box on the left where the widget set draws none, the series colour on the
right, and the title between them. Every position in it is arithmetic over the
row's rectangle, and all of it was inside a draw handler that takes a canvas.

WHAT IT COSTS TO GET WRONG. A check box drawn at a size the widget set does not
agree with is a box the user can see and only partly click - the hit area is a
theme metric, not the row. A title placed without room for the box overlaps it. A
swatch that runs to the row's edge has no border and reads as part of the next
column. None of that raises anything; the legend simply looks slightly wrong, on
one widget set, on one theme.

WHAT STAYS IN THE WINDOW. Asking the theme for a metric, scaling for the display,
and painting. The numbers are here.
}
unit legend_layout;

{$mode objfpc}{$H+}

interface

const
    { The check box size quoted at 96 dpi, used when the theme answers nothing.
      Scaled by the caller before it gets here. }
    DefaultCheckSize96 = 13;

{ How big the drawn check box should be.

  ASKED OF THE THEME, not taken from the row: the hit area both widget sets
  derive from the same metric, so a box drawn any other size is one the user can
  see and only partly click. Clamped to the row, because a theme on a display
  the row was not sized for can answer with more than fits; and falling back to
  the quoted default when the theme answers nothing at all, which is what a
  missing theme returns. }
function LegendCheckSizeFor(AThemeSize, AFallbackSize,
    ARowHeight: longint): longint;

{ Where a box of ABoxSize sits vertically in a row that starts at ARowTop and is
  ARowHeight tall.

  CENTRED, because the row is taller than a check box - it is sized for the text
  and the colour swatch - and a box pinned to the top of it reads as belonging to
  the row above. }
function CenteredBoxTop(ARowTop, ARowHeight, ABoxSize: longint): longint;

{ Where the title starts.

  Past the check box only where the box was drawn: where the widget set drew its
  own, the row's rectangle already begins after it, and reserving a second box's
  width would be white space the title is pushed out by. }
function LegendTextLeft(ARowLeft, ACheckSize, ATextGap: longint;
    ACheckWasDrawn: boolean): longint;

{ Where the colour swatch sits horizontally, given the row's right edge. It is
  square, sized from the row, and inset by one so its border is inside the row
  rather than on it. }
function LegendSwatchLeft(ARowRight, ASize: longint): longint;

{ Whether adding a series to the chart also adds a legend row.

  NOT ALWAYS, and that is deliberate: the redraws during a running fit set the
  flag false so the legend does not churn once per iteration. }
function LegendRowIsAdded(AUpdatingLegends: boolean): boolean;

{ Whether emptying the chart also empties the legend. ALWAYS, whatever the flag
  says.

  THE ASYMMETRY IS THE POINT, and getting it wrong is what made the legend lie.
  Rows are added conditionally and must be removed unconditionally: a row added
  while the flag was true has to go when the chart is cleared, or it survives as
  a row whose series no longer exists. Clear() tested the flag on the way out as
  well as on the way in, so the rows that outlived their series were exactly the
  ones added before a fit and cleared during one.

  And because rows are added conditionally, THE LEGEND IS NOT INDEX-PARALLEL TO
  THE CHART - so no row may ever be found by its position. Both the drawing
  handler and Hide() did that, and drew or deleted another series' row. }
function LegendIsClearedWith(AUpdatingLegends: boolean): boolean;

{ Whether the legend can be addressed by position at all, given that rows are
  added conditionally. Stated as a function so that a test can say no. }
function LegendRowsMatchChartPositions: boolean;

implementation

function LegendRowIsAdded(AUpdatingLegends: boolean): boolean;
begin
    Result := AUpdatingLegends;
end;

function LegendIsClearedWith(AUpdatingLegends: boolean): boolean;
begin
    //  The argument is accepted and ignored, deliberately: the caller HAS the
    //  flag in hand at that moment and the honest answer is that it does not
    //  matter. Taking no argument would let a reader assume it was never
    //  relevant, which is the assumption that broke this.
    Result := True;
end;

function LegendRowsMatchChartPositions: boolean;
begin
    Result := False;
end;

function LegendCheckSizeFor(AThemeSize, AFallbackSize,
    ARowHeight: longint): longint;
begin
    Result := AThemeSize;
    if Result <= 0 then
        Result := AFallbackSize;
    if Result > ARowHeight then
        Result := ARowHeight;
    //  A row of no height gives a box of no size rather than a negative one,
    //  which a rectangle call draws inside out.
    if Result < 0 then
        Result := 0;
end;

function CenteredBoxTop(ARowTop, ARowHeight, ABoxSize: longint): longint;
begin
    Result := ARowTop + (ARowHeight - ABoxSize) div 2;
    //  A box larger than its row is centred to a negative offset, which would
    //  put it above the row it belongs to.
    if Result < ARowTop then
        Result := ARowTop;
end;

function LegendTextLeft(ARowLeft, ACheckSize, ATextGap: longint;
    ACheckWasDrawn: boolean): longint;
begin
    Result := ARowLeft + ATextGap;
    if ACheckWasDrawn then
        Result := Result + ACheckSize;
end;

function LegendSwatchLeft(ARowRight, ASize: longint): longint;
begin
    Result := ARowRight - 1 - ASize;
end;

end.
