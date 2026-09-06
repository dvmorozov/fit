// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where the pieces of a legend row sit.)

THE LEGEND IS OWNER-DRAWN - the application paints the whole row - so every
position in it is arithmetic, and all of it was inside a handler that takes a
canvas.

None of the failures raise anything. A check box drawn at a size the widget set
does not agree with is one the user can see and only partly click, because the
hit area comes from a theme metric and not from what was painted. A title placed
without room overlaps the box. A swatch flush with the row's edge loses its
border. The legend just looks slightly wrong, on one widget set, on one theme -
which is to say, on somebody else's machine.
}
unit testcase_legend_layout;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, legend_layout;

type
    TLegendLayoutTest = class(TTestCase)
    published
        //  When a legend row appears and when it goes - the asymmetry that made
        //  the legend draw one series' colour against another's name.
        procedure ARowIsAddedOnlyWhileTheLegendIsBeingUpdated;
        procedure ButTheLegendIsClearedWhateverTheFlagSays;
        procedure SoTheLegendIsNotIndexParallelToTheChart;
        //  How big the check box is.
        procedure TheThemesAnswerIsUsed;
        procedure NoAnswerFromTheThemeFallsBackToTheQuotedSize;
        procedure ABoxLargerThanItsRowIsClampedToTheRow;
        procedure ABoxThatFitsIsNotClamped;
        procedure ARowOfNoHeightGivesNoBox;

        //  Where it sits.
        procedure ABoxIsCentredInItsRow;
        procedure ABoxTheHeightOfItsRowSitsAtTheTop;
        procedure ABoxIsNeverPlacedAboveItsRow;
        procedure TheCentringFollowsTheRow;

        //  Where the title starts.
        procedure ATitleLeavesRoomForABoxThatWasDrawn;
        procedure ATitleLeavesNoRoomForABoxTheWidgetSetDrew;
        procedure TheGapIsAlwaysThere;

        //  Where the colour swatch sits.
        procedure TheSwatchSitsInsideTheRowsRightEdge;
        procedure TheSwatchLeavesRoomForItsOwnBorder;
    end;

implementation

{ ---- how big the check box is ---------------------------------------------- }

procedure TLegendLayoutTest.TheThemesAnswerIsUsed;
begin
    //  ASKED OF THE THEME, because that is the metric both widget sets derive
    //  their hit area from - so what is drawn is what can be clicked.
    AssertEquals('the theme decides', 16, LegendCheckSizeFor(16, 13, 20));
end;

procedure TLegendLayoutTest.NoAnswerFromTheThemeFallsBackToTheQuotedSize;
begin
    //  A missing theme answers zero. Drawing a box of no size leaves the row
    //  with a title and a colour and no check box at all.
    AssertEquals('the fallback', 13, LegendCheckSizeFor(0, 13, 20));
    AssertEquals('and for a negative answer too',
        13, LegendCheckSizeFor(-1, 13, 20));
end;

procedure TLegendLayoutTest.ABoxLargerThanItsRowIsClampedToTheRow;
begin
    //  A theme on a display the row was not sized for. An unclamped box paints
    //  over the row above and below it.
    AssertEquals('clamped', 12, LegendCheckSizeFor(30, 13, 12));
end;

procedure TLegendLayoutTest.ABoxThatFitsIsNotClamped;
begin
    AssertEquals('exactly the row', 20, LegendCheckSizeFor(20, 13, 20));
    AssertEquals('smaller than the row', 14, LegendCheckSizeFor(14, 13, 20));
end;

procedure TLegendLayoutTest.ARowOfNoHeightGivesNoBox;
begin
    //  A row measured before the font was applied. A negative size draws a
    //  rectangle inside out, which some widget sets fill across the whole row.
    AssertEquals('nothing', 0, LegendCheckSizeFor(16, 13, 0));
    AssertEquals('and not a negative one', 0, LegendCheckSizeFor(16, 13, -5));
end;

{ ---- where it sits --------------------------------------------------------- }

procedure TLegendLayoutTest.ABoxIsCentredInItsRow;
begin
    //  The row is taller than a check box - it is sized for the text and the
    //  colour swatch - so a box pinned to the top reads as belonging to the row
    //  above it.
    AssertEquals('centred', 100 + 4, CenteredBoxTop(100, 20, 12));
end;

procedure TLegendLayoutTest.ABoxTheHeightOfItsRowSitsAtTheTop;
begin
    AssertEquals('flush', 100, CenteredBoxTop(100, 20, 20));
end;

procedure TLegendLayoutTest.ABoxIsNeverPlacedAboveItsRow;
begin
    //  A box larger than its row centres to a negative offset, and the row above
    //  is another legend entry - so the box would appear against the wrong
    //  series.
    AssertEquals('kept in its row', 100, CenteredBoxTop(100, 10, 30));
end;

procedure TLegendLayoutTest.TheCentringFollowsTheRow;
begin
    //  Every row is drawn by the same handler with a different rectangle, so an
    //  offset computed from anything but ARowTop puts every box on row zero.
    AssertEquals('the first row', 4, CenteredBoxTop(0, 20, 12));
    AssertEquals('the second', 24, CenteredBoxTop(20, 20, 12));
end;

{ ---- where the title starts ------------------------------------------------ }

procedure TLegendLayoutTest.ATitleLeavesRoomForABoxThatWasDrawn;
begin
    AssertEquals('past the box and the gap', 10 + 13 + 6,
        LegendTextLeft(10, 13, 6, True));
end;

procedure TLegendLayoutTest.ATitleLeavesNoRoomForABoxTheWidgetSetDrew;
begin
    //  WHERE THE WIDGET SET DREW ITS OWN, the row's rectangle already begins
    //  after it - so reserving a second box's width is white space the title is
    //  pushed out by, and on a narrow legend the title is then clipped.
    AssertEquals('only the gap', 10 + 6, LegendTextLeft(10, 13, 6, False));
end;

procedure TLegendLayoutTest.TheGapIsAlwaysThere;
begin
    //  A title flush against the box or the row edge reads as part of it.
    AssertTrue('with a box', LegendTextLeft(0, 13, 6, True) > 13);
    AssertTrue('and without', LegendTextLeft(0, 13, 6, False) > 0);
end;

{ ---- where the colour swatch sits ------------------------------------------ }

procedure TLegendLayoutTest.TheSwatchSitsInsideTheRowsRightEdge;
begin
    //  Right-aligned: it is the one thing in the row whose position must not
    //  move with the length of the title.
    AssertEquals('inside the edge', 200 - 1 - 20,
        LegendSwatchLeft(200, 20));
end;

procedure TLegendLayoutTest.TheSwatchLeavesRoomForItsOwnBorder;
begin
    //  One pixel, so the border is drawn inside the row rather than on its
    //  edge - where it merges with the control's own frame and the swatch reads
    //  as part of the next column.
    AssertEquals('a pixel of room', 199 - 20, LegendSwatchLeft(200, 20));
    AssertTrue('and it is inside', LegendSwatchLeft(200, 20) + 20 < 200);
end;

procedure TLegendLayoutTest.ARowIsAddedOnlyWhileTheLegendIsBeingUpdated;
begin
    //  The redraws during a running fit clear the flag, so the legend does not
    //  churn once per iteration.
    AssertTrue('added while updating', LegendRowIsAdded(True));
    AssertFalse('not while a fit redraws', LegendRowIsAdded(False));
end;

procedure TLegendLayoutTest.ButTheLegendIsClearedWhateverTheFlagSays;
begin
    //  THE ASYMMETRY. Rows are added conditionally and must be removed
    //  unconditionally: a row added while the flag was true has to go when the
    //  chart is cleared, or it outlives the series it names. Clear() tested the
    //  flag on the way out too, so the rows that survived their series were
    //  exactly the ones added before a fit and cleared during one.
    AssertTrue('cleared while updating', LegendIsClearedWith(True));
    AssertTrue('and cleared when not', LegendIsClearedWith(False));
end;

procedure TLegendLayoutTest.SoTheLegendIsNotIndexParallelToTheChart;
begin
    //  Which follows from the two above, and is the reason both the drawing
    //  handler and Hide() were wrong: they addressed a row by its position.
    //  A row's own object is the series it names, and that is the only sound
    //  way to pair them.
    AssertFalse('never address a row by position',
        LegendRowsMatchChartPositions);
end;

initialization
    //  A unit test: numbers in, numbers out. No canvas.
    RegisterTest('unit', TLegendLayoutTest);
end.
