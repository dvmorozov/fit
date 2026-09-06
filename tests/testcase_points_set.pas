// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(TPointsSet.Sort: correct for any input, including a repeated x.)

Sort used to be a selection sort that consumed one DISTINCT x per output slot,
so it was only defined for a set whose x values were pairwise distinct. A
repeated x left its search index at -1: an assertion in a debug build, and in a
release build FPoints[-1] - an out-of-bounds read that wrote a garbage point into
the result and reported nothing.

That is the shape these tests defend against, and the reason they are worth
having is that the duplicate did not arrive by mistake. A fit legitimately
produces two instances at the same x0, and the sets the chart sorts are built
from whatever the model turned out to be. The sort has to survive it.
}
unit testcase_points_set;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, points_set;

type
    TPointsSetSortTest = class(TTestCase)
    private
        FPoints: TPointsSet;
        function XsInOrder: boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure SortOrdersDuplicateXWithoutFailing;
        procedure SortKeepsEveryPoint;
        procedure SortIsStableForARepeatedX;
        procedure SortOfAnEmptySetIsNotAnError;
        procedure SortOfOnePointIsNotAnError;
        procedure SortOfAnAlreadySortedSetChangesNothing;

        //  THE TWO EXTENTS, neither of which any test had ever called. They are
        //  what a chart scales its axes by - see the group comment.
        procedure TheExtentsAreTheLargestOfEachCoordinate;
        procedure TheyDoNotAssumeTheSetIsSorted;
        procedure NorThatXAndYRiseTogether;
        procedure OnOnePointBothExtentsAreThatPoint;
        procedure NegativeCoordinatesAreNotTreatedAsAbsent;
        procedure AnEmptySetHasNoExtentsAndSaysSoByFaulting;
    end;

implementation

procedure TPointsSetSortTest.SetUp;
begin
    FPoints := TPointsSet.Create(nil);
end;

procedure TPointsSetSortTest.TearDown;
begin
    FPoints.Free;
    FPoints := nil;
end;

function TPointsSetSortTest.XsInOrder: boolean;
var
    i: longint;
begin
    Result := True;
    for i := 1 to FPoints.PointsCount - 1 do
        if FPoints.PointXCoord[i - 1] > FPoints.PointXCoord[i] then
        begin
            Result := False;
            Exit;
        end;
end;

{ The defect itself: this call is what used to assert. }
procedure TPointsSetSortTest.SortOrdersDuplicateXWithoutFailing;
begin
    FPoints.AddNewPoint(5, 50);
    FPoints.AddNewPoint(2, 20);
    FPoints.AddNewPoint(5, 51);
    FPoints.AddNewPoint(1, 10);
    FPoints.AddNewPoint(2, 21);

    FPoints.Sort;

    AssertEquals('point count', 5, FPoints.PointsCount);
    AssertTrue('x values ascending', XsInOrder);
    AssertEquals('smallest x first', 1, FPoints.PointXCoord[0], 1e-12);
    AssertEquals('largest x last', 5, FPoints.PointXCoord[4], 1e-12);
end;

{ Nothing invented and nothing dropped - the old failure mode wrote a point that
  had never been added, which a count alone would not have caught. }
procedure TPointsSetSortTest.SortKeepsEveryPoint;
var
    i:   longint;
    Sum: double;
begin
    FPoints.AddNewPoint(3, 1);
    FPoints.AddNewPoint(3, 2);
    FPoints.AddNewPoint(3, 4);
    FPoints.AddNewPoint(1, 8);

    FPoints.Sort;

    AssertEquals('point count', 4, FPoints.PointsCount);
    Sum := 0;
    for i := 0 to FPoints.PointsCount - 1 do
        Sum := Sum + FPoints.PointYCoord[i];
    //  Distinct powers of two, so the sum identifies the multiset exactly.
    AssertEquals('every y still present exactly once', 15, Sum, 1e-12);
end;

{ Stability is what makes a second Sort of the same set a no-op, and what keeps
  two instances that converged on one x0 in curve order on the chart. }
procedure TPointsSetSortTest.SortIsStableForARepeatedX;
begin
    FPoints.AddNewPoint(2, 100);
    FPoints.AddNewPoint(1, 1);
    FPoints.AddNewPoint(2, 200);
    FPoints.AddNewPoint(2, 300);

    FPoints.Sort;

    AssertEquals('x=1 first', 1, FPoints.PointXCoord[0], 1e-12);
    AssertEquals('first x=2 kept its order', 100, FPoints.PointYCoord[1], 1e-12);
    AssertEquals('second x=2 kept its order', 200, FPoints.PointYCoord[2], 1e-12);
    AssertEquals('third x=2 kept its order', 300, FPoints.PointYCoord[3], 1e-12);
end;

procedure TPointsSetSortTest.SortOfAnEmptySetIsNotAnError;
begin
    FPoints.Sort;
    AssertEquals('still empty', 0, FPoints.PointsCount);
end;

procedure TPointsSetSortTest.SortOfOnePointIsNotAnError;
begin
    FPoints.AddNewPoint(7, 70);
    FPoints.Sort;
    AssertEquals('point count', 1, FPoints.PointsCount);
    AssertEquals('x', 7, FPoints.PointXCoord[0], 1e-12);
    AssertEquals('y', 70, FPoints.PointYCoord[0], 1e-12);
end;

{ The already-ordered path short-circuits the merge, so it needs its own case. }
procedure TPointsSetSortTest.SortOfAnAlreadySortedSetChangesNothing;
var
    i: longint;
begin
    for i := 0 to 9 do
        FPoints.AddNewPoint(i, i * 10);

    FPoints.Sort;

    AssertEquals('point count', 10, FPoints.PointsCount);
    for i := 0 to 9 do
    begin
        AssertEquals('x at ' + IntToStr(i), i, FPoints.PointXCoord[i], 1e-12);
        AssertEquals('y at ' + IntToStr(i), i * 10, FPoints.PointYCoord[i], 1e-12);
    end;
end;

{ ------------------------------- the extents -------------------------------- }

{ TWO ROUTINES, NEITHER EVER CALLED BY ANY TEST. MaxXCoord and MaxYCoord are what
  a chart scales its axes by, and there are no matching minima - the callers take
  the lower edge from elsewhere, which is worth knowing before writing a test
  that assumes a symmetric pair.

  A maximum that answered the last point rather than the largest crops the top of
  the data on any set that is not monotonic - which is every fitted profile - and
  the picture still looks like a picture. Nothing about that reaches an assertion
  or a log, which is why it is worth four small tests rather than one.

  Read through the properties, which is how every caller reaches them: the
  getters themselves are protected. }

procedure TPointsSetSortTest.TheExtentsAreTheLargestOfEachCoordinate;
begin
    FPoints.AddNewPoint(3, 30);
    FPoints.AddNewPoint(1, 50);
    FPoints.AddNewPoint(2, 10);
    AssertEquals('largest x', 3.0, FPoints.MaxXCoord, 1e-12);
    AssertEquals('largest y', 50.0, FPoints.MaxYCoord, 1e-12);
end;

procedure TPointsSetSortTest.TheyDoNotAssumeTheSetIsSorted;
begin
    //  THE SET IS OFTEN NOT SORTED when this is asked. Curves are added in the
    //  order the user placed them, and the chart wants their extent before
    //  anything has ordered them - so an extent that answered the last element
    //  would be right on sorted data and wrong on the real thing.
    FPoints.AddNewPoint(5, 1);
    FPoints.AddNewPoint(9, 3);
    FPoints.AddNewPoint(0, 4);
    AssertEquals('largest x is not the last element', 9.0,
        FPoints.MaxXCoord, 1e-12);
    AssertEquals('largest y is not the largest x''s point', 4.0,
        FPoints.MaxYCoord, 1e-12);
end;

procedure TPointsSetSortTest.NorThatXAndYRiseTogether;
begin
    //  THE TWO AXES ARE INDEPENDENT, and the point carrying the largest x is
    //  deliberately not the one carrying the largest y. An implementation that
    //  found the extreme point and read both coordinates off it would pass every
    //  test whose data happens to rise, and this is what such data does not.
    FPoints.AddNewPoint(1, 100);
    FPoints.AddNewPoint(2, 5);
    FPoints.AddNewPoint(3, 50);
    AssertEquals('largest x', 3.0, FPoints.MaxXCoord, 1e-12);
    AssertEquals('largest y is on another point', 100.0,
        FPoints.MaxYCoord, 1e-12);
end;

procedure TPointsSetSortTest.OnOnePointBothExtentsAreThatPoint;
begin
    //  A degenerate range, not an error: one point on a chart has a width of
    //  zero, and whatever draws it has to be told so rather than refused.
    FPoints.AddNewPoint(7, -4);
    AssertEquals('max x', 7.0, FPoints.MaxXCoord, 1e-12);
    AssertEquals('max y', -4.0, FPoints.MaxYCoord, 1e-12);
end;

procedure TPointsSetSortTest.NegativeCoordinatesAreNotTreatedAsAbsent;
begin
    //  SEEDED FROM THE FIRST POINT, not from zero. Seeded from zero a maximum
    //  could never be negative, which on a profile sitting entirely below the
    //  axis - a subtracted background overshooting, which happens - would report
    //  an extent the data never reaches and scale the chart to empty space.
    FPoints.AddNewPoint(-30, -300);
    FPoints.AddNewPoint(-10, -100);
    AssertEquals('max x is still negative', -10.0, FPoints.MaxXCoord, 1e-12);
    AssertEquals('max y too', -100.0, FPoints.MaxYCoord, 1e-12);
end;

procedure TPointsSetSortTest.AnEmptySetHasNoExtentsAndSaysSoByFaulting;
var
    Raised: boolean;
begin
    //  CHARACTERISED, NOT ENDORSED. Both extents read point zero before looking
    //  at the count, so an empty set is an out-of-range read rather than an
    //  answer. There is no right answer to give - zero is a coordinate, not an
    //  absence - so the caller has to ask the count first, and every caller in
    //  the tree does.
    //
    //  Pinned so that it is a known state rather than a surprise, and so that
    //  giving it a defined answer later is a deliberate change.
    Raised := False;
    try
        FPoints.MaxXCoord;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('an empty set has no maximum to give', Raised);
end;

initialization
    RegisterTest('unit', TPointsSetSortTest);
end.
