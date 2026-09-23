// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The small tables beside the chart: how many rows, and what is in them.)

THE INTERVALS TABLE IS THE ONE THAT IS NOT OBVIOUS. Fitting intervals are picked
in pairs, and a user is halfway through picking one every single time they mark
one - so an odd number of bounds is the ordinary state, not an edge case. The
table needs a row for the half-made interval, with its second cell left blank.

Off by one either way and the user is misinformed about their own data: too few
rows hides the interval they are in the middle of marking, too many leaves a
blank row at the bottom that reads as one they have not started.

All of it was an expression assigned to a grid's RowCount and a loop writing
cells, in a unit that reaches into the main form by name.
}
unit testcase_points_tables;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    points_tables, summary_table, points_set, title_points_set;

type
    TPointsTablesTest = class(TTestCase)
    private
        FPoints: TTitlePointsSet;
        procedure AddPoints(ACount: longint);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Moving a pick by typing its new position
        procedure APositionsRowIsThePickOfTheSameNumber;
        procedure AnIntervalCellIsOneBoundOfItsPair;
        procedure ATypedPositionMovesThePickToTheNearestSample;
        procedure TheSameSampleIsNoMove;
        procedure ANonNumberIsRefusedInWords;
        procedure AnotherPicksSampleIsRefused;
        procedure APositionOutsideTheProfileIsRefused;
        procedure ARowWithNoPickIsNoMove;
        procedure ABoundMayNotCrossTheNextBound;
        //  Which positions the table shows
        procedure PicksAreShownWhenThereAreAny;
        procedure WithNoPicksTheModelsOwnPositionsAreShown;
        procedure WithNeitherThereIsNothingToShow;
        procedure PicksWinEvenWhenTheModelHasMore;
        //  A plain (x, y) table.
        procedure AnEmptySetIsAHeadingAndNothingElse;
        procedure EveryPointGetsARow;
        procedure ThereIsNoSpareRow;
        procedure TheFirstPointIsUnderTheHeading;
        procedure BothCoordinatesAreShown;
        procedure APointBeyondTheSetIsBlank;
        procedure TheHeadingRowIsNotAPoint;
        procedure AColumnThatDoesNotExistIsBlank;
        procedure ANilSetIsBlankRatherThanAFault;

        //  The one table that can be typed into.
        procedure TheEditableTableHasARowToTypeInto;
        procedure TheSpareRowIsPastTheLastPoint;

        //  The intervals table.
        procedure NoBoundsIsAHeadingAndNothingElse;
        procedure APairOfBoundsIsOneRow;
        procedure TwoPairsAreTwoRows;
        procedure AHalfPickedIntervalStillGetsARow;
        procedure TheOpenEndOfAHalfPickedIntervalIsBlank;
        procedure TheClosedEndOfAHalfPickedIntervalIsShown;
        procedure BothColumnsAreAbscissae;
        procedure EachPairIsOnItsOwnRow;
        procedure ARowBeyondTheBoundsIsBlank;
        procedure ANegativeCountIsNoRows;
    end;

implementation

procedure TPointsTablesTest.PicksAreShownWhenThereAreAny;
begin
    //  What the user put there, and what they can edit.
    AssertEquals(Ord(psPicked), Ord(PositionsForTable(3, 0)));
end;

procedure TPointsTablesTest.WithNoPicksTheModelsOwnPositionsAreShown;
begin
    //  AN ANALYSIS PACK PICKS NOTHING. Every pattern such a pack draws is built
    //  from its own markup, so the picked set is empty while the model holds curves -
    //  and the table showed nothing while the chart showed them all. The table
    //  is about the model's positions, not about the gesture that made them.
    AssertEquals(Ord(psAchieved), Ord(PositionsForTable(0, 2)));
end;

procedure TPointsTablesTest.WithNeitherThereIsNothingToShow;
begin
    AssertEquals(Ord(psNone), Ord(PositionsForTable(0, 0)));
end;

procedure TPointsTablesTest.PicksWinEvenWhenTheModelHasMore;
begin
    //  Never the other way round: the achieved positions are the model's
    //  ANSWER, and showing them in place of the input would replace what the
    //  user typed with what the fit made of it.
    AssertEquals(Ord(psPicked), Ord(PositionsForTable(2, 5)));
end;

procedure TPointsTablesTest.SetUp;
begin
    FPoints := TTitlePointsSet.Create(nil);
end;

procedure TPointsTablesTest.TearDown;
begin
    FreeAndNil(FPoints);
end;

procedure TPointsTablesTest.AddPoints(ACount: longint);
var
    i: longint;
begin
    //  x and y differ, so a column filled from the wrong coordinate shows.
    for i := 0 to ACount - 1 do
        FPoints.AddNewPoint(i + 1, 100 * (i + 1));
end;

{ ---- a plain (x, y) table -------------------------------------------------- }

procedure TPointsTablesTest.AnEmptySetIsAHeadingAndNothingElse;
begin
    //  The state before anything has been picked, which is what the table shows
    //  for most of a session.
    AssertEquals('one row', 1, PointsTableRowCount(0));
end;

procedure TPointsTablesTest.EveryPointGetsARow;
begin
    AssertEquals('three points and a heading', 4, PointsTableRowCount(3));
end;

procedure TPointsTablesTest.ThereIsNoSpareRow;
begin
    //  Entering a point by hand is not supported here, and a blank row at the
    //  bottom of a table invites someone to try.
    AssertEquals('no spare', 2, PointsTableRowCount(1));
end;

procedure TPointsTablesTest.TheFirstPointIsUnderTheHeading;
begin
    AddPoints(2);
    AssertEquals('row 1 is the first point', CurveValueText(1),
        PointsCellText(FPoints, 0, 1));
end;

procedure TPointsTablesTest.BothCoordinatesAreShown;
begin
    //  Column 0 is where, column 1 is how much. Swapped, the table is a
    //  plausible-looking lie.
    AddPoints(2);
    AssertEquals('the abscissa', CurveValueText(2),
        PointsCellText(FPoints, 0, 2));
    AssertEquals('the ordinate', CurveValueText(200),
        PointsCellText(FPoints, 1, 2));
end;

procedure TPointsTablesTest.APointBeyondTheSetIsBlank;
begin
    //  A grid outlives the set it is showing and may still be the larger of the
    //  two while it is being resized.
    AddPoints(1);
    AssertEquals('past the end', '', PointsCellText(FPoints, 0, 5));
end;

procedure TPointsTablesTest.TheHeadingRowIsNotAPoint;
begin
    //  Row 0 holds the column names, which the caller writes itself. Answering
    //  here would overwrite them with the first point.
    AddPoints(1);
    AssertEquals('the heading is not filled', '',
        PointsCellText(FPoints, 0, 0));
end;

procedure TPointsTablesTest.AColumnThatDoesNotExistIsBlank;
begin
    AddPoints(1);
    AssertEquals('a third column', '', PointsCellText(FPoints, 2, 1));
    AssertEquals('and a negative one', '', PointsCellText(FPoints, -1, 1));
end;

procedure TPointsTablesTest.ANilSetIsBlankRatherThanAFault;
begin
    //  The grid is filled from whatever the client currently holds, and that can
    //  be nothing at all.
    AssertEquals('nil', '', PointsCellText(nil, 0, 1));
end;

{ ---- the one table that can be typed into ---------------------------------- }

procedure TPointsTablesTest.TheEditableTableHasARowToTypeInto;
begin
    //  THE AFFORDANCE. The profile grid is the one table a value can be typed
    //  into, and a grid with no empty row at the bottom offers nowhere to type
    //  it - the feature exists and is unreachable.
    AssertEquals('one more than the read-only table',
        PointsTableRowCount(3) + 1, EditablePointsTableRowCount(3));
    AssertEquals('and an empty set still has one', 2,
        EditablePointsTableRowCount(0));
end;

procedure TPointsTablesTest.TheSpareRowIsPastTheLastPoint;
begin
    //  The spare row must not be a point, or the last measurement is shown
    //  twice - once as data and once as something half typed.
    AddPoints(3);
    AssertEquals('the last point', CurveValueText(3),
        PointsCellText(FPoints, 0, 3));
    AssertEquals('and the spare row is blank', '',
        PointsCellText(FPoints, 0, EditablePointsTableRowCount(3) - 1));
end;

{ ---- the intervals table --------------------------------------------------- }

procedure TPointsTablesTest.NoBoundsIsAHeadingAndNothingElse;
begin
    AssertEquals('one row', 1, IntervalTableRowCount(0));
end;

procedure TPointsTablesTest.APairOfBoundsIsOneRow;
begin
    AssertEquals('one interval and a heading', 2, IntervalTableRowCount(2));
end;

procedure TPointsTablesTest.TwoPairsAreTwoRows;
begin
    AssertEquals('two intervals and a heading', 3, IntervalTableRowCount(4));
end;

procedure TPointsTablesTest.AHalfPickedIntervalStillGetsARow;
begin
    //  THE STATE THE USER IS IN EVERY TIME THEY MARK AN INTERVAL. One row short
    //  and the bound they just picked does not appear at all, so the table says
    //  their click did nothing.
    AssertEquals('one bound needs a row', 2, IntervalTableRowCount(1));
    AssertEquals('and so does a third', 3, IntervalTableRowCount(3));
end;

procedure TPointsTablesTest.TheOpenEndOfAHalfPickedIntervalIsBlank;
begin
    //  BLANK, not a repeat of the first bound and not a zero: the interval has
    //  no second end yet, and showing one would say it is finished.
    AddPoints(1);
    AssertEquals('nothing yet', '', IntervalCellText(FPoints, 1, 1));
end;

procedure TPointsTablesTest.TheClosedEndOfAHalfPickedIntervalIsShown;
begin
    AddPoints(1);
    AssertEquals('the bound that was picked', CurveValueText(1),
        IntervalCellText(FPoints, 0, 1));
end;

procedure TPointsTablesTest.BothColumnsAreAbscissae;
begin
    //  An interval is a stretch of the axis, so its ordinates mean nothing -
    //  taking the second column from the y coordinate would show the height of
    //  the profile where the interval ends, labelled as a position.
    AddPoints(2);
    AssertEquals('the start', CurveValueText(1),
        IntervalCellText(FPoints, 0, 1));
    AssertEquals('and the finish, also an abscissa', CurveValueText(2),
        IntervalCellText(FPoints, 1, 1));
end;

procedure TPointsTablesTest.EachPairIsOnItsOwnRow;
begin
    //  Four bounds are two intervals, and the second row must start at the
    //  third bound - not the second.
    AddPoints(4);
    AssertEquals('the first interval starts', CurveValueText(1),
        IntervalCellText(FPoints, 0, 1));
    AssertEquals('and ends', CurveValueText(2),
        IntervalCellText(FPoints, 1, 1));
    AssertEquals('the second starts', CurveValueText(3),
        IntervalCellText(FPoints, 0, 2));
    AssertEquals('and ends', CurveValueText(4),
        IntervalCellText(FPoints, 1, 2));
end;

procedure TPointsTablesTest.ARowBeyondTheBoundsIsBlank;
begin
    AddPoints(2);
    AssertEquals('past the end', '', IntervalCellText(FPoints, 0, 9));
    AssertEquals('and the heading row', '', IntervalCellText(FPoints, 0, 0));
end;

procedure TPointsTablesTest.ANegativeCountIsNoRows;
begin
    //  Nonsense in must not produce a negative RowCount, which a grid rejects
    //  with an exception in the middle of a refresh.
    AssertEquals('still just the heading', 1, IntervalTableRowCount(-4));
    AssertEquals('and for the plain table', 1, PointsTableRowCount(-4));
end;

{ ---- moving a pick ---------------------------------------------------------

  THE REPORT: a pick could be added and removed but not moved - the engine could
  move one (the curve keeps its fitted shape and is re-seeded), and nothing in
  the window asked it to. Typing a new position into the Curve Positions or the
  Fit Intervals table now does. A pick is a real sample of the profile, so the
  typed value is taken to the nearest sample. }

function Profile0To10: TTitlePointsSet;
var
    i: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    for i := 0 to 10 do
        Result.AddNewPoint(i, 100 + i);
end;

function Picks(const AXs: array of double): TTitlePointsSet;
var
    i: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    for i := 0 to High(AXs) do
        Result.AddNewPoint(AXs[i], 100 + AXs[i]);
end;

procedure TPointsTablesTest.APositionsRowIsThePickOfTheSameNumber;
begin
    AssertEquals('under the heading', 0, PositionsCellPick(1));
    AssertEquals('the third', 2, PositionsCellPick(3));
    AssertEquals('the heading is no pick', -1, PositionsCellPick(0));
end;

procedure TPointsTablesTest.AnIntervalCellIsOneBoundOfItsPair;
begin
    AssertEquals('first interval, start', 0, IntervalCellPick(0, 1));
    AssertEquals('first interval, end', 1, IntervalCellPick(1, 1));
    AssertEquals('second interval, start', 2, IntervalCellPick(0, 2));
    AssertEquals('the heading is no pick', -1, IntervalCellPick(0, 0));
end;

procedure TPointsTablesTest.ATypedPositionMovesThePickToTheNearestSample;
var
    P, K: TTitlePointsSet;
    M: TPickMove;
    Why: string;
begin
    P := Profile0To10;
    K := Picks([2, 6]);
    try
        AssertTrue('a move', PlanPickMove(P, K, 0, '4.4', M, Why));
        AssertEquals('from where it was', 2.0, M.PrevX, 1e-12);
        AssertEquals('with its value', 102.0, M.PrevY, 1e-12);
        AssertEquals('to the nearest sample', 4.0, M.NewX, 1e-12);
        AssertEquals('and that sample''s value', 104.0, M.NewY, 1e-12);
    finally
        K.Free;
        P.Free;
    end;
end;

procedure TPointsTablesTest.TheSameSampleIsNoMove;
var
    P, K: TTitlePointsSet;
    M: TPickMove;
    Why: string;
begin
    P := Profile0To10;
    K := Picks([2, 6]);
    try
        AssertFalse('nothing to do', PlanPickMove(P, K, 0, '2.1', M, Why));
        AssertEquals('and nothing wrong', '', Why);
    finally
        K.Free;
        P.Free;
    end;
end;

procedure TPointsTablesTest.ANonNumberIsRefusedInWords;
var
    P, K: TTitlePointsSet;
    M: TPickMove;
    Why: string;
begin
    P := Profile0To10;
    K := Picks([2]);
    try
        AssertFalse(PlanPickMove(P, K, 0, '4 deg', M, Why));
        AssertTrue('names what was typed: ' + Why, Pos('4 deg', Why) > 0);
    finally
        K.Free;
        P.Free;
    end;
end;

procedure TPointsTablesTest.AnotherPicksSampleIsRefused;
var
    P, K: TTitlePointsSet;
    M: TPickMove;
    Why: string;
begin
    //  One pick per sample: two picks on one sample would be one pick, and the
    //  curve the moved one seeds would silently disappear.
    P := Profile0To10;
    K := Picks([2, 6]);
    try
        AssertFalse(PlanPickMove(P, K, 0, '5.8', M, Why));
        AssertTrue('says why: ' + Why, Why <> '');
    finally
        K.Free;
        P.Free;
    end;
end;

procedure TPointsTablesTest.APositionOutsideTheProfileIsRefused;
var
    P, K: TTitlePointsSet;
    M: TPickMove;
    Why: string;
begin
    //  Not taken to the last sample: 100 is not "near" 10, it is a typo or a
    //  unit, and moving the pick to the end would hide that.
    P := Profile0To10;
    K := Picks([2]);
    try
        AssertFalse(PlanPickMove(P, K, 0, '100', M, Why));
        AssertTrue('says why: ' + Why, Why <> '');
    finally
        K.Free;
        P.Free;
    end;
end;

procedure TPointsTablesTest.ABoundMayNotCrossTheNextBound;
var
    K: TTitlePointsSet;
begin
    //  Bounds pair in order: 0-4 is one interval, 6-9 the next. An end moved
    //  past the next start would re-pair them - 0-6 and 4-9 - and every curve
    //  would be fitted over a stretch nobody chose.
    K := Picks([0, 4, 6, 9]);
    try
        AssertTrue('inside its room', BoundMoveKeepsOrder(K, 1, 5));
        AssertFalse('onto the next bound', BoundMoveKeepsOrder(K, 1, 6));
        AssertFalse('past it', BoundMoveKeepsOrder(K, 1, 7));
        AssertFalse('behind the previous one', BoundMoveKeepsOrder(K, 2, 3));
        AssertTrue('the first has no bound before it', BoundMoveKeepsOrder(K, 0, -5));
    finally
        K.Free;
    end;
end;

procedure TPointsTablesTest.ARowWithNoPickIsNoMove;
var
    P, K: TTitlePointsSet;
    M: TPickMove;
    Why: string;
begin
    P := Profile0To10;
    K := Picks([2]);
    try
        AssertFalse('beyond the picks', PlanPickMove(P, K, 5, '4', M, Why));
        AssertFalse('the heading', PlanPickMove(P, K, -1, '4', M, Why));
    finally
        K.Free;
        P.Free;
    end;
end;

initialization
    //  A unit test: a point set in memory and some strings. No grid.
    RegisterTest('unit', TPointsTablesTest);
end.
