// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A point with no place on the chart is kept and left undrawn.)

A logarithmic value axis has nowhere to put zero or a negative number, so the
axis answers NaN and the chart leaves a gap there. The point is KEPT - picks and
refreshes address points by index, so dropping it would shift every later one -
and left out of everything that draws or measures: its line, its marker, its
caption, the series' extents and the pointer's snapping.

What can be asserted without a chart on screen is asserted here: which points
are drawn, and that the extents - which decide the chart's scale - come from the
drawn points only, including when the extreme point itself becomes a gap.
}
unit testcase_chart_gaps;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, Graphics, TAGraph;

type
    TChartGapsTest = class(TTestCase)
    private
        FChart: TTAChart;
        FSerie: TTASerie;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure APointWithANumberInEachCoordinateIsDrawn;
        procedure APointWithNoValueIsKeptAndLeftUndrawn;
        procedure APointWithNoArgumentIsLeftUndrawnToo;
        procedure TheExtentsAreTakenFromTheDrawnPointsOnly;
        procedure TheHighestPointBecomingAGapLowersTheTop;
        procedure TheLowestPointBecomingAGapRaisesTheBottom;
        procedure AGapFilledAgainCountsAgain;
    end;

implementation

procedure TChartGapsTest.SetUp;
begin
    FChart := TTAChart.Create(nil);
    FSerie := TTASerie.Create(nil);
    FChart.AddSerie(FSerie);
    FSerie.AddXY(0, 10, clRed);
    FSerie.AddXY(1, NaN, clRed);
    FSerie.AddXY(2, 30, clRed);
    FSerie.AddXY(3, 20, clRed);
end;

procedure TChartGapsTest.TearDown;
begin
    FreeAndNil(FChart);
    FSerie := nil;
end;

procedure TChartGapsTest.APointWithANumberInEachCoordinateIsDrawn;
begin
    AssertTrue(FSerie.PointIsDrawn(0));
    AssertTrue(FSerie.PointIsDrawn(2));
end;

procedure TChartGapsTest.APointWithNoValueIsKeptAndLeftUndrawn;
begin
    AssertEquals('kept, so every later index still names its point', 4,
        FSerie.Count);
    AssertFalse('and not drawn', FSerie.PointIsDrawn(1));
end;

procedure TChartGapsTest.APointWithNoArgumentIsLeftUndrawnToo;
begin
    FSerie.AddXY(NaN, 5, clRed);
    AssertFalse(FSerie.PointIsDrawn(4));
end;

procedure TChartGapsTest.TheExtentsAreTakenFromTheDrawnPointsOnly;
begin
    AssertEquals('bottom', 10, FSerie.GetYMin, 0);
    AssertEquals('top', 30, FSerie.GetYMax, 0);
    AssertEquals('left', 0, FSerie.GetXMin, 0);
    AssertEquals('right', 3, FSerie.GetXMax, 0);
end;

procedure TChartGapsTest.TheHighestPointBecomingAGapLowersTheTop;
begin
    //  The incremental bookkeeping kept the old top: nothing compares with NaN,
    //  so the point was written and the extent never moved. The chart then kept
    //  room for a value that is no longer drawn.
    FSerie.SetYValue(2, NaN);
    AssertEquals(20, FSerie.GetYMax, 0);
end;

procedure TChartGapsTest.TheLowestPointBecomingAGapRaisesTheBottom;
begin
    FSerie.SetYValue(0, NaN);
    AssertEquals(20, FSerie.GetYMin, 0);
    AssertEquals('and its argument no longer widens the chart', 2,
        FSerie.GetXMin, 0);
end;

procedure TChartGapsTest.AGapFilledAgainCountsAgain;
begin
    FSerie.SetYValue(1, 50);
    AssertTrue(FSerie.PointIsDrawn(1));
    AssertEquals(50, FSerie.GetYMax, 0);
end;

initialization
    RegisterTest('unit', TChartGapsTest);
end.
