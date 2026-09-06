// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a click on the chart is a pick of.)

THE DEFECT THIS DEFENDS AGAINST. The chart's crosshair snaps to the nearest
point of any visible series. The window took that series as the one the user had
clicked and dropped the click when it was not the series being marked up - so as
soon as a model was drawn over the data, every further pick was silently lost
and a second bounded pattern could not be marked at all.

The rules below are the fix expressed over plain numbers, where they can be
asserted exhaustively; the window keeps only the pixel-reading. Both directions
matter and are asserted: a click that must be re-aimed, and a click that must be
LEFT ALONE - re-aiming a click on the collected set would turn taking a pick
back into adding one.
}
unit testcase_pick_target;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, pick_target;

type
    TPickTargetTest = class(TTestCase)
    published
        //  Which button places a pick. Nothing checked before this existed.
        procedure ALeftClickPlacesAPick;
        procedure AndNoOtherButtonDoes;

        procedure AClickOnTheActiveSerieIsPickable;
        procedure AClickOnTheCollectedSetIsPickable;
        procedure AClickOnACurveDrawnOverTheDataIsNot;
        procedure AnEmptySerieHasNoNearestPoint;
        procedure TheNearestPointIsTheOneClicked;
        procedure DistanceCountsBothCoordinates;
        procedure TiesGoToTheEarlierPoint;
        procedure APointFarOffCanvasDoesNotOverflow;
        procedure CoordinatesMustComeInPairs;
    end;

implementation

procedure TPickTargetTest.ALeftClickPlacesAPick;
begin
    //  Picking is a left-click gesture and always has been.
    AssertTrue('the left button picks', ClickPlacesAPick(True));
end;

procedure TPickTargetTest.AndNoOtherButtonDoes;
begin
    //  WHAT NOTHING CHECKED. TTAChart's MouseDown and MouseUp both take the
    //  button and never read it, the window's handlers ignored it, and
    //  OnChartClick's gate tested the crosshair - which MouseMove draws, so it
    //  is button-independent. A right-click that did not move between press and
    //  release therefore added a point, and that is why the chart could offer no
    //  context menu: opening one would have edited the model.
    AssertFalse('any other button picks nothing', ClickPlacesAPick(False));
end;

procedure TPickTargetTest.AClickOnTheActiveSerieIsPickable;
begin
    AssertTrue('the series being marked up is where picks come from',
        IsPickableSerie(3, 3, False));
end;

procedure TPickTargetTest.AClickOnTheCollectedSetIsPickable;
begin
    //  A click there REMOVES a pick, which is a pick gesture of its own and
    //  must not be re-aimed at the data.
    AssertTrue('a click on the collected picks is one of them',
        IsPickableSerie(7, 3, True));
end;

procedure TPickTargetTest.AClickOnACurveDrawnOverTheDataIsNot;
begin
    AssertFalse('a model curve is not where a pick comes from',
        IsPickableSerie(7, 3, False));
end;

procedure TPickTargetTest.AnEmptySerieHasNoNearestPoint;
var
    X, Y: array of longint;
begin
    X := nil;
    Y := nil;
    AssertEquals('an empty series answers "no point", not point 0',
        NO_POINT, NearestPointIndex(X, Y, 10, 10));
end;

procedure TPickTargetTest.TheNearestPointIsTheOneClicked;
begin
    AssertEquals(2, NearestPointIndex([0, 50, 100], [0, 0, 0], 98, 0));
    AssertEquals(0, NearestPointIndex([0, 50, 100], [0, 0, 0], 2, 0));
end;

procedure TPickTargetTest.DistanceCountsBothCoordinates;
begin
    //  Point 0 is nearer in x alone; point 1 is nearer to the pointer. The
    //  pointer wins - the user aimed at pixels, and a chart whose axes are
    //  differently scaled makes "nearest in x" a different point entirely.
    AssertEquals(1, NearestPointIndex([10, 20], [500, 0], 12, 0));
end;

procedure TPickTargetTest.TiesGoToTheEarlierPoint;
begin
    //  Equally right, so the choice is arbitrary - but it must be the SAME
    //  arbitrary choice every time, or the same click marks different points.
    AssertEquals(0, NearestPointIndex([0, 20], [0, 0], 10, 0));
end;

procedure TPickTargetTest.APointFarOffCanvasDoesNotOverflow;
var
    Far: longint;
begin
    //  Points outside the visible range keep their image coordinates, which run
    //  far past the canvas. Squared in a longint they wrap round and can come
    //  back as the NEAREST point - so the arithmetic is done wider.
    Far := 2000 * 1000 * 1000;
    AssertEquals('the point under the pointer, not the one that wrapped round',
        1, NearestPointIndex([Far, 30], [Far, 0], 25, 0));
end;

procedure TPickTargetTest.CoordinatesMustComeInPairs;
var
    Raised: boolean;
begin
    Raised := False;
    try
        NearestPointIndex([1, 2, 3], [1, 2], 0, 0);
    except
        //  Loudly: a series whose coordinates disagree is a caller's mistake,
        //  and taking the shorter of the two would pick some other point and
        //  look like the user missed.
        on E: Exception do
            Raised := True;
    end;
    AssertTrue('mismatched coordinate arrays must raise', Raised);
end;

initialization
    RegisterTest('unit', TPickTargetTest);
end.
