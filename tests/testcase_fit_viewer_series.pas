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
    fit_viewer, fit_chart, self_copied_component, test_curve_types,
    module_view_types, title_points_set, legend_entries;

type
    TFitViewerSeriesTest = class(TTestCase)
    private
        FViewer: TFitViewer;
        FChart: TFitChart;
        FLegend: TCheckListBox;
        FCurves: TSelfCopiedCompList;
        { What the client does when the model is rebuilt with ACount curves. }
        procedure RebuildModel(ACount: longint);
        { A module's series of ACount points with a caption each, the way a
          module's redraw hands it over. }
        function CaptionedPoints(ACount: longint; const ATitle: string;
            ALabels: TStringList): TTitlePointsSet;
        function RowsNamed(const ATitle: string): longint;
        function SerieOfRow(const ATitle: string; AIndex: longint): TFitSerie;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ARebuiltModelLeavesOneSeriesAndOneLegendRowPerCurve;
        procedure RebuildingTheModelFreesEverySeriesItTookOff;
        procedure ACaptionedSeriesGivesTwoLegendRows;
        procedure HidingTheMarkersLeavesTheCaptionsDrawn;
        procedure HidingTheCaptionsLeavesTheMarkersDrawn;
        procedure TheModulesWordsNameTheCaptionRow;
        procedure EveryReplotRefillsBothAndLeavesTwo;
        procedure DeletingTheOwnerRemovesBothRows;
        procedure ASeriesAfterACaptionedOneIsFilledIntoItsOwnSeries;
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
    Xs, Ys: array of double;
begin
    Xs := nil;
    Ys := nil;
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
        SetLength(Xs, 50);
        SetLength(Ys, 50);
        for j := 0 to 49 do
        begin
            Xs[j] := j;
            Ys[j] := j * (i + 1);
        end;
        Curve.TakeSamples(Xs, Ys);
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

function TFitViewerSeriesTest.CaptionedPoints(ACount: longint;
    const ATitle: string; ALabels: TStringList): TTitlePointsSet;
var
    i: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    Result.FTitle := ATitle;
    for i := 0 to ACount - 1 do
    begin
        Result.AddNewPoint(i, 10 + i);
        ALabels.Add(IntToStr(i + 1));
    end;
end;

function TFitViewerSeriesTest.RowsNamed(const ATitle: string): longint;
var
    Captions: array of string;
    i: longint;
begin
    SetLength(Captions, FLegend.Items.Count);
    for i := 0 to High(Captions) do
        Captions[i] := FLegend.Items[i];
    Result := Length(LegendRowsCaptioned(Captions, ATitle));
end;

function TFitViewerSeriesTest.SerieOfRow(const ATitle: string;
    AIndex: longint): TFitSerie;
var
    Captions: array of string;
    Rows: TLegendRowList;
    i: longint;
begin
    SetLength(Captions, FLegend.Items.Count);
    for i := 0 to High(Captions) do
        Captions[i] := FLegend.Items[i];
    Rows := LegendRowsCaptioned(Captions, ATitle);
    Result := TFitSerie(FLegend.Items.Objects[Rows[AIndex]]);
end;

procedure TFitViewerSeriesTest.ACaptionedSeriesGivesTwoLegendRows;
var
    Labels: TStringList;
    Points: TTitlePointsSet;
begin
    Labels := TStringList.Create;
    Points := CaptionedPoints(3, 'Pivots', Labels);
    try
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels,
            DefaultModuleSeriesStyle, 'curve-1');
        AssertEquals('the markers and the captions are two series',
            2, FChart.SeriesCount);
        AssertEquals('the markers have a row', 1, RowsNamed('Pivots'));
        AssertEquals('the captions have a row of their own', 2,
            FLegend.Items.Count);
        AssertEquals('both are recorded against the one curve', 2,
            Length(FViewer.Series.OwnedBy('curve-1')));
    finally
        FViewer.Clear(nil);
        Points.Free;
        Labels.Free;
    end;
end;

procedure TFitViewerSeriesTest.HidingTheMarkersLeavesTheCaptionsDrawn;
var
    Labels: TStringList;
    Points: TTitlePointsSet;
begin
    Labels := TStringList.Create;
    Points := CaptionedPoints(3, 'Pivots', Labels);
    try
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels,
            DefaultModuleSeriesStyle, 'curve-1');
        AssertEquals('two series to look at', 2, FChart.SeriesCount);
        SerieOfRow('Pivots', 0).ShowAsTicked(False);
        AssertFalse('the markers are off', SerieOfRow('Pivots', 0).IsShown);
        AssertTrue('the captions are still drawn',
            TFitSerie(FChart.Series[1]).IsShown);
        AssertEquals('the captions are named for what they are',
            'Pivots labels', TFitSerie(FChart.Series[1]).Title);
        AssertEquals('on the same points', 3, TFitSerie(FChart.Series[1]).Count);
    finally
        FViewer.Clear(nil);
        Points.Free;
        Labels.Free;
    end;
end;

procedure TFitViewerSeriesTest.HidingTheCaptionsLeavesTheMarkersDrawn;
var
    Labels: TStringList;
    Points: TTitlePointsSet;
begin
    Labels := TStringList.Create;
    Points := CaptionedPoints(3, 'Pivots', Labels);
    try
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels,
            DefaultModuleSeriesStyle, 'curve-1');
        AssertEquals('two series to look at', 2, FChart.SeriesCount);
        TFitSerie(FChart.Series[1]).ShowAsTicked(False);
        AssertFalse('the captions are off', TFitSerie(FChart.Series[1]).IsShown);
        AssertTrue('the markers are still drawn', SerieOfRow('Pivots', 0).IsShown);
        AssertEquals('and they hold no text', '',
            SerieOfRow('Pivots', 0).PointCaption(0));
    finally
        FViewer.Clear(nil);
        Points.Free;
        Labels.Free;
    end;
end;

procedure TFitViewerSeriesTest.TheModulesWordsNameTheCaptionRow;
var
    Labels: TStringList;
    Points: TTitlePointsSet;
    Style: TModuleSeriesStyle;
begin
    Labels := TStringList.Create;
    Points := CaptionedPoints(2, 'Pivots', Labels);
    try
        Style := DefaultModuleSeriesStyle;
        Style.CaptionsTitle := 'Wave numbers';
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels, Style, 'curve-1');
        AssertEquals(1, RowsNamed('Wave numbers'));
        AssertEquals(1, RowsNamed('Pivots'));
    finally
        FViewer.Clear(nil);
        Points.Free;
        Labels.Free;
    end;
end;

procedure TFitViewerSeriesTest.EveryReplotRefillsBothAndLeavesTwo;
var
    Labels, Fresh: TStringList;
    Points: TTitlePointsSet;
begin
    Labels := TStringList.Create;
    Fresh := TStringList.Create;
    Points := CaptionedPoints(3, 'Pivots', Labels);
    try
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels,
            DefaultModuleSeriesStyle, 'curve-1');
        Points.AddNewPoint(7, 17);
        Fresh.Add('a');
        Fresh.Add('b');
        Fresh.Add('c');
        Fresh.Add('d');
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Fresh,
            DefaultModuleSeriesStyle, 'curve-1');
        AssertEquals('still two series', 2, FChart.SeriesCount);
        AssertEquals('still two rows', 2, FLegend.Items.Count);
        AssertEquals('markers refilled', 4, TFitSerie(FChart.Series[0]).Count);
        AssertEquals('captions refilled', 4, TFitSerie(FChart.Series[1]).Count);
        AssertEquals('new text', 'd', TFitSerie(FChart.Series[1]).PointCaption(3));
    finally
        FViewer.Clear(nil);
        Points.Free;
        Labels.Free;
        Fresh.Free;
    end;
end;

procedure TFitViewerSeriesTest.DeletingTheOwnerRemovesBothRows;
var
    Labels: TStringList;
    Points: TTitlePointsSet;
begin
    Labels := TStringList.Create;
    Points := CaptionedPoints(3, 'Pivots', Labels);
    try
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels,
            DefaultModuleSeriesStyle, 'curve-1');
        FViewer.HideSeriesOwnedBy('curve-1');
        AssertEquals(0, FChart.SeriesCount);
        AssertEquals(0, FLegend.Items.Count);
        AssertEquals(0, FViewer.Series.Count);
    finally
        FViewer.Clear(nil);
        Points.Free;
        Labels.Free;
    end;
end;

procedure TFitViewerSeriesTest.ASeriesAfterACaptionedOneIsFilledIntoItsOwnSeries;
var
    Labels: TStringList;
    Points, Other: TTitlePointsSet;
begin
    Labels := TStringList.Create;
    Points := CaptionedPoints(3, 'Pivots', Labels);
    Other := TTitlePointsSet.Create(nil);
    Other.FTitle := 'Other';
    Other.AddNewPoint(1, 1);
    Other.AddNewPoint(2, 2);
    try
        FViewer.PlotModuleSeries(nil, 'Pivots', Points, Labels,
            DefaultModuleSeriesStyle, 'curve-1');
        FViewer.PlotModuleSeries(nil, 'Other', Other, nil,
            DefaultModuleSeriesStyle, 'curve-2');
        AssertEquals('three series', 3, FChart.SeriesCount);
        AssertEquals('the third is the other one', 2,
            TFitSerie(FChart.Series[2]).Count);
        Other.AddNewPoint(3, 3);
        FViewer.PlotModuleSeries(nil, 'Other', Other, nil,
            DefaultModuleSeriesStyle, 'curve-2');
        AssertEquals('filled into its own series', 3,
            TFitSerie(FChart.Series[2]).Count);
        AssertEquals('and the pivots untouched', 3,
            TFitSerie(FChart.Series[0]).Count);
    finally
        FViewer.Clear(nil);
        Points.Free;
        Other.Free;
        Labels.Free;
    end;
end;

initialization
    //  In-process: a chart and a list box, no window and no server.
    RegisterTest('unit', TFitViewerSeriesTest);
end.
