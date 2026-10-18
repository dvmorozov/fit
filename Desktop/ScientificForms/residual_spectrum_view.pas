// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The Residual Spectrum tab's chart: the wavelet scalogram as a colour
map, its key and its two scroll bars.)

MOVED FROM THE WINDOW, UNCHANGED (2026-10-17): the construction, the colours
and the drawing of each cell are the code the window ran, so a product
without the scientific module carries none of it. The window still decides
everything else - see spectrum_view - and reaches these controls through
the view's Chart, ScrollBarX and ScrollBarY.
}
unit residual_spectrum_view;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Controls, StdCtrls, Forms, Graphics, Math, fit_chart,
    fit_window, residual_randomness, difference_scalogram, spectrum_view,
    TAFuncSeries, TASources, TALegend;

type
    TResidualSpectrumView = class(TObject, ISpectrumView)
    private
        FSpectrumChart: TFitChart;
        { THE RESIDUAL SPECTRUM TAB (Stage I4): the wavelet scalogram, a colour
          map asked cell by cell (ScalogramCalculate) from the grid last taken,
          and the cone of influence as a line. }
        FScalogramSerie: TColorMapSeries;
        FScalogramColors: TListChartSource;
        FScalogram: TScalogramGrid;
        { The Residual Spectrum tab's own horizontal scroll bar, showing the same
          view as the model tab's ScrollBarX. }
        FSpectrumScrollBarX: TScrollBar;
        { And its vertical one, along the map's wave lengths. }
        FSpectrumScrollBarY: TScrollBar;
        procedure ScalogramCalculate(const AX, AY: double; out AZ: double);
        procedure ScalogramYMarks(AMin, AMax: double; var AStart, AStep: double;
            var AHandled: boolean);
        function ScalogramYMarkText(AValue, AStep: double): string;
    public
        procedure Build(AOwner: TComponent; ASheet: TWinControl;
            AXBarHeight, AYBarWidth: integer; const AXAxisLabel: string);
        function Chart: TFitChart;
        function ScrollBarX: TScrollBar;
        function ScrollBarY: TScrollBar;
        procedure Draw(const AWindow: TFitWindow; AHeld: boolean);
        function Periods: TDoubleArray;
        function Columns: longint;
        function ColumnsFor(const AWindow: TFitWindow): longint;
        function KeyNamed: longint;
        function KeyKinds: longint;
        { The colour map's stops from the palette in force and the level. }
        procedure ApplyColors;
    end;

implementation

uses
    app_theme, chart_tabs;

procedure TResidualSpectrumView.Build(AOwner: TComponent; ASheet: TWinControl;
    AXBarHeight, AYBarWidth: integer; const AXAxisLabel: string);
begin
    //  WHERE, AND AT WHICH RHYTHM (Stage I4): the scalogram on the model's x,
    //  on the same view as the strip (the window's ApplyChartViews), so a
    //  patch sits over the bars it is about; the period upwards as log2, an
    //  octave a unit (difference_scalogram).
    //  ITS OWN SCROLL BAR, where the model's tab has one, moving the same
    //  view (ApplyChartViews keeps the two thumbs together). Bottom-aligned
    //  first, so it runs under its vertical bar as the model's does.
    FSpectrumScrollBarX := TScrollBar.Create(AOwner);
    FSpectrumScrollBarX.Name := 'ScrollBarSpectrumX';
    FSpectrumScrollBarX.Kind := sbHorizontal;
    FSpectrumScrollBarX.Parent := ASheet;
    FSpectrumScrollBarX.Align := alBottom;
    FSpectrumScrollBarX.Height := AXBarHeight;
    //  AND ONE UP ITS SIDE, along its wave lengths (the user, 2026-10-15),
    //  where a blank spacer as wide kept the plot under the model's.
    FSpectrumScrollBarY := TScrollBar.Create(AOwner);
    FSpectrumScrollBarY.Name := 'ScrollBarSpectrumY';
    FSpectrumScrollBarY.Kind := sbVertical;
    FSpectrumScrollBarY.Parent := ASheet;
    FSpectrumScrollBarY.Align := alRight;
    FSpectrumScrollBarY.Width := AYBarWidth;
    FSpectrumChart := TFitChart.Create(AOwner);
    FSpectrumChart.Name := 'ChartResidualSpectrum';
    FSpectrumChart.Parent := ASheet;
    FSpectrumChart.Align := alClient;
    FSpectrumChart.ShowAxisLabel := True;
    FSpectrumChart.XAxisLabel := AXAxisLabel;
    FSpectrumChart.YAxisLabel := SpectrumYAxisLabel;
    //  A PLAIN LINE UNDER THE TITLE saying how to read the map (the user,
    //  2026-10-14: "totally unclear what it means").
    FSpectrumChart.Title.Text.Text := SpectrumChartTitle + LineEnding +
        SpectrumChartCaption;
    FSpectrumChart.Title.Visible := True;
    FSpectrumChart.Hint := SpectrumChartHint;
    FSpectrumChart.ShowHint := True;
    FSpectrumChart.OnYMarks := @ScalogramYMarks;
    FSpectrumChart.OnYMarkText := @ScalogramYMarkText;
    FScalogramColors := TListChartSource.Create(AOwner);
    FScalogramSerie := TColorMapSeries.Create(nil);
    FScalogramSerie.ColorSource := FScalogramColors;
    //  KINDS, NOT A SCALE: each cell takes its kind's colour exactly.
    FScalogramSerie.Interpolate := False;
    FScalogramSerie.Legend.Multiplicity := lmPoint;
    FScalogramSerie.StepX := 2;
    FScalogramSerie.StepY := 2;
    FScalogramSerie.OnCalculate := @ScalogramCalculate;
    FSpectrumChart.AddSeries(FScalogramSerie);
    //  THE KEY UNDER THE MAP, not beside it: beside it would narrow the plot,
    //  and the three charts on the model's x stand over each other.
    FSpectrumChart.Legend.Visible := True;
    FSpectrumChart.Legend.Alignment := laBottomCenter;
    FSpectrumChart.Legend.ColumnCount := 3;
    FSpectrumChart.ApplyPalette(CurrentPalette);
    ApplyColors;
end;

function TResidualSpectrumView.Chart: TFitChart;
begin
    Result := FSpectrumChart;
end;

function TResidualSpectrumView.ScrollBarX: TScrollBar;
begin
    Result := FSpectrumScrollBarX;
end;

function TResidualSpectrumView.ScrollBarY: TScrollBar;
begin
    Result := FSpectrumScrollBarY;
end;

procedure TResidualSpectrumView.Draw(const AWindow: TFitWindow; AHeld: boolean);
begin
    //  THE WAVELET SCALOGRAM OF THE DIFFERENCE, over the samples the fit counts
    //  (fit_window) and standardised as the server's test is - built from the
    //  client's window, as the client's ResidualScalogram built it, empty with
    //  no model or no interval.
    FScalogram := Default(TScalogramGrid);
    if AHeld then
        FScalogram := ScalogramGridOf(AWindow);
    if Length(FScalogram.Periods) > 0 then
    begin
        FScalogramSerie.Extent.UseXMin := True;
        FScalogramSerie.Extent.UseXMax := True;
        FScalogramSerie.Extent.UseYMin := True;
        FScalogramSerie.Extent.UseYMax := True;
        FScalogramSerie.Extent.XMin := FScalogram.X[0];
        FScalogramSerie.Extent.XMax := FScalogram.X[High(FScalogram.X)];
        FScalogramSerie.Extent.YMin := Log2(FScalogram.Periods[0]);
        FScalogramSerie.Extent.YMax :=
            Log2(FScalogram.Periods[High(FScalogram.Periods)]);
    end;
end;

function TResidualSpectrumView.Periods: TDoubleArray;
begin
    Result := FScalogram.Periods;
end;

function TResidualSpectrumView.Columns: longint;
begin
    Result := Length(FScalogram.X);
end;

function TResidualSpectrumView.ColumnsFor(const AWindow: TFitWindow): longint;
begin
    Result := Length(ScalogramGridOf(AWindow).X);
end;

function TResidualSpectrumView.KeyNamed: longint;
var
    Items: TChartLegendItems;
    Key: TScalogramKey;
    i, k: longint;
begin
    //  THE KEY AS DRAWN: the legend's own items, against the words of every
    //  kind of cell.
    Result := 0;
    Items := FSpectrumChart.GetLegendItems;
    try
        Key := ScalogramKey(CurrentPalette);
        for i := 0 to Items.Count - 1 do
            for k := 0 to High(Key) do
                if Items[i].Text = Key[k].Text then
                    Inc(Result);
    finally
        Items.Free;
    end;
end;

function TResidualSpectrumView.KeyKinds: longint;
begin
    Result := Ord(High(TScalogramCell)) + 1;
end;

procedure TResidualSpectrumView.ScalogramCalculate(const AX, AY: double;
    out AZ: double);
begin
    //  WHAT THE CELL SAYS, as the key's position: the colour source holds one
    //  item per kind (ApplyColors).
    AZ := Ord(ScalogramCellAt(FScalogram, AX, ScalogramPeriodAt(AY)));
end;

procedure TResidualSpectrumView.ScalogramYMarks(AMin, AMax: double;
    var AStart, AStep: double; var AHandled: boolean);
begin
    ScalogramPeriodMarks(AMin, AMax, AStart, AStep);
    AHandled := True;
end;

function TResidualSpectrumView.ScalogramYMarkText(AValue, AStep: double): string;
begin
    Result := ScalogramPeriodMarkText(AValue);
end;

procedure TResidualSpectrumView.ApplyColors;
var
    Key: TScalogramKey;
    i: longint;
begin
    if not Assigned(FScalogramColors) then
        Exit;
    //  ONE ITEM PER KIND OF CELL, worded: the colour source is also the key
    //  the chart's legend lists.
    Key := ScalogramKey(CurrentPalette);
    FScalogramColors.Clear;
    for i := 0 to High(Key) do
        FScalogramColors.Add(Ord(Key[i].Cell), 0, Key[i].Text,
            TColor(Key[i].Color));
    if Assigned(FSpectrumChart) then
    begin
        FSpectrumChart.Legend.BackgroundBrush.Color :=
            TColor(CurrentPalette.Background);
        FSpectrumChart.Legend.Font.Color := TColor(CurrentPalette.Text);
        FSpectrumChart.Legend.Frame.Color := TColor(CurrentPalette.Axis);
        FSpectrumChart.Invalidate;
    end;
end;

end.
