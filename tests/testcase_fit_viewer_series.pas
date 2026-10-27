// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The chart's series, as the model is rebuilt again and again: each one
freed when its curve goes, and nothing left behind.)

WHY THIS IS MEASURED. Every model edit and every animated frame takes the old
curves off the chart and draws the new ones (TFitClient.ReplaceCurves, then
PlotCurves), so a series that is unlinked and not freed leaks once per curve per
rebuild - which over a long fit, frame after frame, is the "memory grows during
long computations, and in animation mode too" that was reported. It happened
here once already: DeleteSeries only unlinks a series, and the viewer used to
stop there.

THE SAME CALLS THE CLIENT MAKES, in its order: Hide for every old curve, the old
point sets freed, PlotCurves with the new ones - on a real chart and a real
legend, which need no window (TFitViewer.Chart and .Legend). The grids are off,
as they are for an animated frame, because they are the window's.

THE HEAP IS ASKED, not the chart: a series taken off the chart and left
allocated is invisible to the chart, which is exactly what a leak is. The first
rebuilds are not counted - lists grow to their working size there - and every
later one must leave the heap where it found it, give or take less than one
series.
}
unit testcase_fit_viewer_series;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, CheckLst,
    fit_viewer, fit_chart, self_copied_component, test_curve_types;

type
    TFitViewerSeriesTest = class(TTestCase)
    private
        FViewer: TFitViewer;
        FChart: TFitChart;
        FLegend: TCheckListBox;
        FCurves: TSelfCopiedCompList;
        { What the client does when the model is rebuilt with ACount curves. }
        procedure RebuildModel(ACount: longint);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ARebuiltModelLeavesOneSeriesAndOneLegendRowPerCurve;
        procedure RebuildingTheModelFreesEverySeriesItTookOff;
    end;

implementation

const
    { Curves per model, rebuilds before counting, and rebuilds counted. }
    CurvesPerModel = 3;
    WarmUpRebuilds = 3;
    CountedRebuilds = 20;
    { Less than any series costs: a series is an object with a point source
      and a style, and leaking one per curve per rebuild would leave sixty of
      them behind here. }
    HeapSlack = 256;

procedure TFitViewerSeriesTest.SetUp;
begin
    FChart := TFitChart.Create(nil);
    FLegend := TCheckListBox.Create(nil);
    FViewer := TFitViewer.Create(nil);
    FViewer.Chart := FChart;
    FViewer.Legend := FLegend;
    //  As for an animated frame: the tables are the window's, and the legend
    //  rows are what this test counts, so they stay on.
    FViewer.SetUpdateGrids(False);
    FCurves := nil;
end;

procedure TFitViewerSeriesTest.TearDown;
begin
    if Assigned(FCurves) then
        FViewer.Clear(nil);
    FreeAndNil(FCurves);
    FreeAndNil(FViewer);
    FreeAndNil(FLegend);
    FreeAndNil(FChart);
end;

procedure TFitViewerSeriesTest.RebuildModel(ACount: longint);
var
    Fresh: TSelfCopiedCompList;
    Curve: TTestGaussPointsSet;
    i, j: longint;
begin
    //  TFitClient.ReplaceCurves: every old curve hidden, then the list freed.
    if Assigned(FCurves) then
    begin
        for i := 0 to FCurves.Count - 1 do
            FViewer.Hide(nil, TTestGaussPointsSet(FCurves.Items[i]));
        FreeAndNil(FCurves);
    end;
    Fresh := TSelfCopiedCompList.Create;
    for i := 0 to ACount - 1 do
    begin
        Curve := TTestGaussPointsSet.Create(nil, 10 * i);
        Curve.FTitle := 'curve ' + IntToStr(i + 1);
        for j := 0 to 49 do
            Curve.AddNewPoint(j, j * (i + 1));
        Fresh.Add(Curve);
    end;
    FCurves := Fresh;
    //  TFitClient.PlotCurves, with no parameters: the grids are off.
    FViewer.PlotCurves(nil, FCurves, nil);
end;

procedure TFitViewerSeriesTest.ARebuiltModelLeavesOneSeriesAndOneLegendRowPerCurve;
var
    i: longint;
begin
    for i := 1 to 5 do
        RebuildModel(CurvesPerModel);
    AssertEquals('one series per curve on the chart', CurvesPerModel,
        FChart.SeriesCount);
    AssertEquals('one legend row per curve', CurvesPerModel,
        FLegend.Items.Count);
    AssertEquals('one recorded series per curve', CurvesPerModel,
        FViewer.Series.Count);
end;

procedure TFitViewerSeriesTest.RebuildingTheModelFreesEverySeriesItTookOff;
var
    i: longint;
    Before, After: PtrUInt;
begin
    for i := 1 to WarmUpRebuilds do
        RebuildModel(CurvesPerModel);
    Before := GetFPCHeapStatus.CurrHeapUsed;
    for i := 1 to CountedRebuilds do
        RebuildModel(CurvesPerModel);
    After := GetFPCHeapStatus.CurrHeapUsed;
    AssertTrue(Format('%d rebuilds of %d curves left %d bytes behind',
        [CountedRebuilds, CurvesPerModel, Int64(After) - Int64(Before)]),
        Int64(After) - Int64(Before) < HeapSlack);
end;

initialization
    //  In-process: a chart and a list box, no window and no server.
    RegisterTest('unit', TFitViewerSeriesTest);
end.
