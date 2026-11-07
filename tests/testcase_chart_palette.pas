// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A chart already drawn is repainted in the palette View > Theme puts in force.)

A series keeps WHERE its colour comes from (series_style.TSeriesColorSource),
not the colour it resolved to, so a change of palette recolours what is on the
chart by resolving each source again (TFitChart.ApplyPalette) - without
rebuilding a series, and without a second mapping kept beside the first. The
rule itself is series_style's and is tested there; this is the chart doing it:
the series, the plot area, the axes and the dashed grid.
}
unit testcase_chart_palette;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, Graphics, fit_chart,
    series_style, app_theme, module_view_types;

type
    TChartPaletteTest = class(TTestCase)
    private
        FChart: TFitChart;
        function NewSerie(const ASource: TSeriesColorSource): TFitSerie;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ASeriesIsBornInThePaletteInForce;
        procedure ANewPaletteRecoloursEverySeriesByItsRole;
        procedure AModulesColourIsMadeReadableOnTheDarkChart;
        procedure AFixedColourIsLeftAsItWasGiven;
        procedure ThePlotAreaTheAxesAndTheGridFollowThePalette;
    end;

implementation

function TChartPaletteTest.NewSerie(const ASource: TSeriesColorSource): TFitSerie;
begin
    Result := TFitSerie.Create(nil);
    FChart.AddSeries(Result);
    Result.ColorSource := ASource;
end;

procedure TChartPaletteTest.SetUp;
begin
    UseDarkPalette(False);
    FChart := TFitChart.Create(nil);
end;

procedure TChartPaletteTest.TearDown;
begin
    FreeAndNil(FChart);
    UseDarkPalette(False);
end;

procedure TChartPaletteTest.ASeriesIsBornInThePaletteInForce;
var
    S: TFitSerie;
begin
    UseDarkPalette(True);
    S := NewSerie(RoleColorSource(crExperiment, 0));
    AssertEquals(PaletteFor(True).Experiment, S.SeriesColor);
end;

procedure TChartPaletteTest.ANewPaletteRecoloursEverySeriesByItsRole;
var
    Data, Curve: TFitSerie;
begin
    Data := NewSerie(RoleColorSource(crExperiment, 0));
    Curve := NewSerie(RoleColorSource(crModelCurve, 3));
    AssertEquals('light data', PaletteFor(False).Experiment, Data.SeriesColor);
    FChart.ApplyPalette(PaletteFor(True));
    AssertEquals('dark data', PaletteFor(True).Experiment, Data.SeriesColor);
    AssertEquals('the third curve, dark', PaletteFor(True).Curves[3],
        Curve.SeriesColor);
end;

procedure TChartPaletteTest.AModulesColourIsMadeReadableOnTheDarkChart;
var
    S: TFitSerie;
begin
    S := NewSerie(ModuleColorSource(mcNavy));
    AssertEquals('kept on white', mcNavy, S.SeriesColor);
    FChart.ApplyPalette(PaletteFor(True));
    AssertTrue('readable on the dark chart',
        ContrastRatio(S.SeriesColor, PaletteFor(True).Background) >=
        MinGraphicContrast);
end;

procedure TChartPaletteTest.AFixedColourIsLeftAsItWasGiven;
var
    S: TFitSerie;
begin
    S := TFitSerie.Create(nil);
    FChart.AddSeries(S);
    S.SeriesColor := clFuchsia;
    FChart.ApplyPalette(PaletteFor(True));
    AssertEquals(clFuchsia, S.SeriesColor);
end;

procedure TChartPaletteTest.ThePlotAreaTheAxesAndTheGridFollowThePalette;
var
    P: TThemePalette;
begin
    P := PaletteFor(True);
    FChart.ApplyPalette(P);
    AssertEquals('the plot area', P.Background, FChart.BackColor);
    AssertEquals('the axes', P.Axis, FChart.AxisColor);
    AssertEquals('the axis marks', P.Axis, FChart.BottomAxis.Marks.LabelFont.Color);
    //  The grid stayed the component's black, which vanished on the dark chart.
    AssertEquals('the dashed grid', P.Axis, FChart.BottomAxis.Grid.Color);
    AssertEquals('both grids', P.Axis, FChart.LeftAxis.Grid.Color);
end;

initialization
    RegisterTest('unit', TChartPaletteTest);
end.
