// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A picture of the chart is scaled to the series that are drawn.)

WHAT A PICTURE SHOWS IS WHAT THE LEGEND SHOWS. A series whose legend box is
unticked draws nothing (TFitSerie.IsShown), but it is still an active series of
the chart, and TAChart's scale is made of the active ones - so a picture drawn
as it stands would keep room for a difference or a set of interval bounds that
is not on it. The chart sets such series aside for the drawing and puts them
back, and what is asked here is that both halves hold.
}
unit testcase_chart_unshown_series;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_chart;

type
    TChartUnshownSeriesTest = class(TTestCase)
    private
        FChart: TFitChart;
        FData, FHidden: TFitSerie;
        FAside: TList;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnUntickedSeriesStillWidensTheScaleUntilItIsSetAside;
        procedure SettingItAsideLetsTheScaleFollowWhatIsDrawn;
        procedure PuttingItBackRestoresTheScaleAndTheSeries;
        procedure ASeriesThatWasNotActiveIsNotSwitchedOnByPuttingBack;
        procedure ASeriesThatIsDrawnIsLeftAlone;
    end;

implementation

procedure TChartUnshownSeriesTest.SetUp;
begin
    FChart := TFitChart.Create(nil);
    FAside := TList.Create;
    FData := TFitSerie.Create(nil);
    FChart.AddSeries(FData);
    FData.AddXY(0, 100);
    FData.AddXY(1, 110);
    //  The difference, hidden by its legend box: both lines and markers off,
    //  as a tick leaves a series (SeriesVisibilityForTick).
    FHidden := TFitSerie.Create(nil);
    FChart.AddSeries(FHidden);
    FHidden.AddXY(0, -50);
    FHidden.AddXY(1, 5);
    FHidden.ShowLines := False;
    FHidden.ShowPoints := False;
end;

procedure TChartUnshownSeriesTest.TearDown;
begin
    FAside.Free;
    FreeAndNil(FChart);
end;

procedure TChartUnshownSeriesTest.AnUntickedSeriesStillWidensTheScaleUntilItIsSetAside;
begin
    AssertEquals('the hidden series is still counted', -50, FChart.FullYGraphMin,
        0);
end;

procedure TChartUnshownSeriesTest.SettingItAsideLetsTheScaleFollowWhatIsDrawn;
begin
    FChart.SetAsideSeriesThatDrawNothing(FAside);
    AssertEquals('only the hidden one', 1, FAside.Count);
    AssertEquals('the scale starts at the data', 100, FChart.FullYGraphMin, 0);
    AssertEquals(110, FChart.FullYGraphMax, 0);
    AssertFalse(FHidden.Active);
end;

procedure TChartUnshownSeriesTest.PuttingItBackRestoresTheScaleAndTheSeries;
begin
    FChart.SetAsideSeriesThatDrawNothing(FAside);
    FChart.PutBackSeries(FAside);
    AssertTrue('active again', FHidden.Active);
    AssertEquals(-50, FChart.FullYGraphMin, 0);
    AssertEquals('and still drawing nothing', False, FHidden.IsShown);
end;

procedure TChartUnshownSeriesTest.ASeriesThatWasNotActiveIsNotSwitchedOnByPuttingBack;
begin
    //  Something else took it off the chart for its own reasons; the picture
    //  is not what puts it back.
    FHidden.Active := False;
    FChart.SetAsideSeriesThatDrawNothing(FAside);
    AssertEquals('not ours to put back', 0, FAside.Count);
    FChart.PutBackSeries(FAside);
    AssertFalse(FHidden.Active);
end;

procedure TChartUnshownSeriesTest.ASeriesThatIsDrawnIsLeftAlone;
begin
    FChart.SetAsideSeriesThatDrawNothing(FAside);
    AssertTrue(FData.Active);
    AssertTrue(FData.IsShown);
end;

initialization
    RegisterTest('unit', TChartUnshownSeriesTest);
end.
