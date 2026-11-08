// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Editing a set of points: replacing one, deleting one, and finding the
one nearest a value.)

THESE THREE ARE WHAT EVERY PICK AND EVERY TYPED CELL GOES THROUGH. A click on the
chart replaces or deletes; a number typed into the table replaces; the crosshair
finds the nearest. They are small and they are ordered - the set is kept sorted
by abscissa, because everything downstream walks it in order - and each of them
has an edge that only shows on the first or the last point.

REPLACING IS NOT SIMPLY WRITING. It matches the point to change by VALUE, not by
index, because the table is sorted and the caller's index does not correspond. It
has three outcomes, and which one applies is decided in a single loop: the point
is found and moved, a point already sits where it would move TO, or neither and
it is a new point. Getting the middle case wrong is how a set ends up with two
points at one abscissa, which every consumer of the set assumes cannot happen.

DELETING HAS A DOCUMENTED SCAR. An earlier version broke out of its loop as soon
as the array it was filling was full, which made the LAST point impossible to
delete - the user clicked their final pick again and nothing happened. The
comment saying why there is no Break is in the code; this is the test that would
have caught it.

AND ABSENCE IS NOT AN ERROR in either of them. A pick can be removed twice - the
chart and the model both send one - so deleting something that is not there has
to be a quiet no-op rather than a fault.
}
unit testcase_points_set_edit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    points_set;

type
    TPointsSetEditTest = class(TTestCase)
    private
        FPoints: TPointsSet;
        { Five points at x = 10..14, y = 100..104. }
        procedure GivenFivePoints;
        { The abscissae in order, joined - so a wrong order fails with a message
          that shows the order rather than an index. }
        function Abscissae: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Replacing.
        procedure ReplacingMovesTheMatchedPoint;
        procedure ThePointIsMatchedByValueNotByIndex;
        procedure ReplacingKeepsTheSetSorted;
        procedure ReplacingAPointThatIsNotThereAddsIt;
        procedure MovingOntoAnExistingAbscissaOverwritesItsValue;
        procedure MovingAPointOntoItselfChangesNothing;
        procedure MovingAPointOntoAnotherLeavesTwoAtOneAbscissa;

        //  Deleting.
        procedure DeletingTakesThePointOut;
        procedure TheOthersSurvive;
        procedure TheFirstPointCanBeDeleted;
        procedure TheLastPointCanBeDeleted;
        procedure DeletingSomethingAbsentIsQuiet;
        procedure DeletingTheOnlyPointLeavesNothing;

        //  Finding.
        procedure TheNearestPointToAValueIsFound;
        procedure AValueBelowEverythingFindsTheFirst;
        procedure AValueAboveEverythingFindsTheLast;
        procedure AnExactMatchIsFoundByValue;
        procedure AValueThatIsNotThereIsNotAMatch;
    end;

implementation

procedure TPointsSetEditTest.SetUp;
begin
    FPoints := TPointsSet.Create(nil);
end;

procedure TPointsSetEditTest.TearDown;
begin
    FreeAndNil(FPoints);
end;

procedure TPointsSetEditTest.GivenFivePoints;
var
    i: longint;
begin
    for i := 0 to 4 do
        FPoints.AddNewPoint(10 + i, 100 + i);
end;

function TPointsSetEditTest.Abscissae: string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to FPoints.PointsCount - 1 do
        Result := Result + Format('%g ', [FPoints.PointXCoord[i]]);
end;

{ ---- replacing ------------------------------------------------------------- }

procedure TPointsSetEditTest.ReplacingMovesTheMatchedPoint;
begin
    GivenFivePoints;
    FPoints.ReplacePoint(12, 102, 12.5, 999);
    AssertEquals('the point moved', '10 11 12.5 13 14 ', Abscissae);
end;

procedure TPointsSetEditTest.ThePointIsMatchedByValueNotByIndex;
begin
    //  BY VALUE, because the set is sorted and the caller's index does not
    //  correspond - the table the user typed into is ordered by abscissa, and
    //  the model reorders as points move.
    GivenFivePoints;
    FPoints.ReplacePoint(13, 103, 13, 555);
    AssertEquals('the right point took the new value', 555.0,
        FPoints.PointYCoord[3], 1E-9);
    AssertEquals('and its neighbour did not', 102.0,
        FPoints.PointYCoord[2], 1E-9);
end;

procedure TPointsSetEditTest.ReplacingKeepsTheSetSorted;
begin
    //  EVERYTHING DOWNSTREAM WALKS THIS SET IN ORDER - the chart draws it as a
    //  line, the fit integrates over it. A point moved past its neighbour and
    //  left where it was would draw as a spike back and forth.
    GivenFivePoints;
    FPoints.ReplacePoint(10, 100, 13.5, 100);
    AssertEquals('still in order', '11 12 13 13.5 14 ', Abscissae);
end;

procedure TPointsSetEditTest.ReplacingAPointThatIsNotThereAddsIt;
begin
    //  How a pick is made: the caller passes zeros for the previous coordinates
    //  when there is nothing to replace, and the point is added instead.
    GivenFivePoints;
    FPoints.ReplacePoint(0, 0, 12.5, 500);
    AssertEquals('six now', 6, FPoints.PointsCount);
    AssertEquals('and in order', '10 11 12 12.5 13 14 ', Abscissae);
end;

procedure TPointsSetEditTest.MovingOntoAnExistingAbscissaOverwritesItsValue;
begin
    //  THE CASE THAT KEEPS THE SET SINGLE-VALUED. Moving a point onto an
    //  abscissa that already has one cannot leave both: every consumer assumes
    //  one y per x, and two would make the curve ambiguous exactly where the
    //  user was working.
    GivenFivePoints;
    FPoints.ReplacePoint(0, 0, 12, 777);
    AssertEquals('no point was added', 5, FPoints.PointsCount);
    AssertEquals('and the value was taken', 777.0,
        FPoints.PointYCoord[2], 1E-9);
end;

procedure TPointsSetEditTest.MovingAPointOntoItselfChangesNothing;
begin
    //  A drag that ended where it started, which is what a click on an existing
    //  point looks like to this method.
    GivenFivePoints;
    FPoints.ReplacePoint(0, 0, 12, 102);
    AssertEquals('still five', 5, FPoints.PointsCount);
    AssertEquals('unchanged', 102.0, FPoints.PointYCoord[2], 1E-9);
end;

procedure TPointsSetEditTest.MovingAPointOntoAnotherLeavesTwoAtOneAbscissa;
var
    i, Repeats: longint;
begin
    //  A DEFECT, pinned as it behaves. The single-valued rule the case above
    //  keeps holds only on the path that ADDS a point: when the previous
    //  coordinates match an existing one, it is moved without checking where it
    //  is being moved TO - so typing a new abscissa into the table that another
    //  row already has leaves the set with two y values at one x.
    //
    //  What that does downstream: the chart draws a vertical segment and the
    //  fit integrates over both. Nothing raises, because Sort was made correct
    //  for repeated abscissae after an earlier defect - see its header.
    //
    //  Not fixed here because the right answer is a product decision: merge the
    //  two, refuse the edit, or move the point and drop the other. See
    //  findings.md.
    GivenFivePoints;
    FPoints.ReplacePoint(10, 100, 12, 100);
    Repeats := 0;
    for i := 1 to FPoints.PointsCount - 1 do
        if FPoints.PointXCoord[i] = FPoints.PointXCoord[i - 1] then
            Inc(Repeats);
    AssertEquals('two points share an abscissa', 1, Repeats);
end;

{ ---- deleting -------------------------------------------------------------- }

procedure TPointsSetEditTest.DeletingTakesThePointOut;
begin
    GivenFivePoints;
    FPoints.DeletePoint(12);
    AssertEquals('four left', 4, FPoints.PointsCount);
    AssertEquals('and it is gone', '10 11 13 14 ', Abscissae);
end;

procedure TPointsSetEditTest.TheOthersSurvive;
begin
    //  With their values, not just their abscissae - a rebuild that copied the
    //  x and dropped the y would leave a flat line the user did not measure.
    GivenFivePoints;
    FPoints.DeletePoint(12);
    AssertEquals('the first', 100.0, FPoints.PointYCoord[0], 1E-9);
    AssertEquals('and the last', 104.0, FPoints.PointYCoord[3], 1E-9);
end;

procedure TPointsSetEditTest.TheFirstPointCanBeDeleted;
begin
    GivenFivePoints;
    FPoints.DeletePoint(10);
    AssertEquals('11 12 13 14 ', Abscissae);
end;

procedure TPointsSetEditTest.TheLastPointCanBeDeleted;
begin
    //  THE DEFECT THIS FILE EXISTS FOR. An earlier version broke out of its
    //  loop as soon as the array it was filling was full, which made the last
    //  point impossible to delete: the user clicked their final pick again and
    //  nothing happened. The comment saying why there is no Break is in the
    //  code; this is the assertion that holds it there.
    GivenFivePoints;
    FPoints.DeletePoint(14);
    AssertEquals('four left', 4, FPoints.PointsCount);
    AssertEquals('10 11 12 13 ', Abscissae);
end;

procedure TPointsSetEditTest.DeletingSomethingAbsentIsQuiet;
begin
    //  A pick can be removed twice - the chart and the model both send one - so
    //  this is an ordinary event rather than a caller in the wrong.
    GivenFivePoints;
    FPoints.DeletePoint(99);
    AssertEquals('nothing happened', 5, FPoints.PointsCount);
end;

procedure TPointsSetEditTest.DeletingTheOnlyPointLeavesNothing;
begin
    //  The array being rebuilt has length zero here, which is the size at which
    //  an off-by-one in the rebuild would raise rather than misbehave quietly.
    FPoints.AddNewPoint(10, 100);
    FPoints.DeletePoint(10);
    AssertEquals('empty', 0, FPoints.PointsCount);
end;

{ ---- finding --------------------------------------------------------------- }

procedure TPointsSetEditTest.TheNearestPointToAValueIsFound;
begin
    //  What the crosshair does with a pointer position: the user aims between
    //  two samples and means the closer one.
    GivenFivePoints;
    AssertEquals('nearest to 12.4', 2, FPoints.IndexOfNearestToX(12.4));
    AssertEquals('and to 12.6', 3, FPoints.IndexOfNearestToX(12.6));
end;

procedure TPointsSetEditTest.AValueBelowEverythingFindsTheFirst;
begin
    //  A click to the left of the data still has to name a point: the caller
    //  indexes the set with whatever comes back.
    GivenFivePoints;
    AssertEquals(0, FPoints.IndexOfNearestToX(-100));
end;

procedure TPointsSetEditTest.AValueAboveEverythingFindsTheLast;
begin
    GivenFivePoints;
    AssertEquals(4, FPoints.IndexOfNearestToX(1000));
end;

procedure TPointsSetEditTest.AnExactMatchIsFoundByValue;
begin
    //  A different question from "nearest": this one answers only for an
    //  abscissa the set actually holds, which is how a caller tells an existing
    //  point from a new one.
    GivenFivePoints;
    AssertEquals(2, FPoints.IndexOfValueX(12));
end;

procedure TPointsSetEditTest.AValueThatIsNotThereIsNotAMatch;
begin
    //  Answering the nearest here instead would make every click land on an
    //  existing point, and no pick could ever be added.
    GivenFivePoints;
    AssertTrue('no match', FPoints.IndexOfValueX(12.5) < 0);
end;

initialization
    //  A unit test: one point set in memory.
    RegisterTest('unit', TPointsSetEditTest);
end.
