// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A curve is drawn edge to edge however many points share a pixel.)

On a 200% desktop the Qt6 build showed a diffraction profile as its peaks and
nothing else: every stretch of background between them was missing, and no zoom
brought it back. The series was stroked one sample pair at a time with
MoveTo/LineTo, and the LCL's Qt6 LineTo takes the last pixel off every line - the
GDI convention - before handing it to Qt, which draws a line of no length as
nothing. A profile has more samples than the plot has pixels (1692 over some
650 logical pixels there), so a flat stretch is made entirely of steps of zero
or one pixel, and each of them was shortened to nothing.

The canvas here records what it is asked to draw and renders it the way that
widget set does (recording_canvas), so what these tests assert is what reaches
the screen: a LineTo covers its pixels less the last one, and a stroke of no
length covers nothing. The chart is drawn through TAChart's own drawer - what
Paint runs - so the series is laid out and drawn exactly as on screen, with only
the canvas replaced.

The fork this was first written against joined samples into polylines by hand;
TAChart's TLineSeries does the same by itself, and these tests are what say so.
}
unit testcase_chart_strokes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, Math, fpcunit, testregistry, Graphics,
    TADrawerCanvas, fit_chart, recording_canvas;

type
    TChartStrokesTest = class(TTestCase)
    private
        FChart: TFitChart;
        FSerie: TFitSerie;
        FRecorder: TRecordingCanvas;
        procedure Draw;
        procedure AssertDrawnEdgeToEdge(const AWhat: string; AColor: TColor;
            AFirst, ALast: longint);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFlatStretchDenserThanThePixelsIsDrawnEdgeToEdge;
        procedure ANoisyStretchDenserThanThePixelsIsDrawnEdgeToEdge;
        procedure NoStrokeCrossesAnUndrawnPoint;
    end;

implementation

const
    { More samples than the plot has columns, as a real profile has. }
    DenseCount = 1000;

procedure TChartStrokesTest.SetUp;
begin
    FRecorder := TRecordingCanvas.Create;
    FChart := TFitChart.Create(nil);
    FSerie := TFitSerie.Create(nil);
    FSerie.SeriesColor := clRed;
    FChart.AddSeries(FSerie);
end;

procedure TChartStrokesTest.TearDown;
begin
    FreeAndNil(FChart);
    FSerie := nil;
    FreeAndNil(FRecorder);
end;

{ A plot some 270 columns wide: fewer than the samples, as on screen. }
procedure TChartStrokesTest.Draw;
begin
    FChart.Draw(TRecordingDrawer.Create(FRecorder), Rect(0, 0, 320, 200));
end;

{ Every column from point AFirst's to point ALast's is covered in AColor. }
procedure TChartStrokesTest.AssertDrawnEdgeToEdge(const AWhat: string;
    AColor: TColor; AFirst, ALast: longint);
var
    c, FromColumn, ToColumn, Missing: longint;
begin
    FromColumn := Min(FSerie.GetXImgValue(AFirst), FSerie.GetXImgValue(ALast));
    ToColumn := Max(FSerie.GetXImgValue(AFirst), FSerie.GetXImgValue(ALast));
    AssertTrue(AWhat + ': the stretch spans many columns', ToColumn - FromColumn > 20);
    Missing := 0;
    for c := FromColumn to ToColumn do
        if not FRecorder.Covers(AColor, c) then
            Inc(Missing);
    AssertEquals(AWhat + ': columns of ' + IntToStr(FromColumn) + '..' +
        IntToStr(ToColumn) + ' left blank', 0, Missing);
end;

procedure TChartStrokesTest.AFlatStretchDenserThanThePixelsIsDrawnEdgeToEdge;
var
    i: longint;
begin
    for i := 0 to DenseCount - 1 do
        FSerie.AddXY(i, 100);
    Draw;
    AssertDrawnEdgeToEdge('flat', clRed, 0, DenseCount - 1);
end;

{ A background is not flat: counting noise moves it a pixel up or down from one
  sample to the next, which the shortening turned into no line either. }
procedure TChartStrokesTest.ANoisyStretchDenserThanThePixelsIsDrawnEdgeToEdge;
var
    i: longint;
begin
    for i := 0 to DenseCount - 1 do
        FSerie.AddXY(i, 100 + (i mod 3));
    FSerie.AddXY(DenseCount, 400);
    Draw;
    AssertDrawnEdgeToEdge('noisy', clRed, 0, DenseCount - 1);
end;

{ A point with no place on the chart is a gap (testcase_chart_gaps); a longer
  stroke must not bridge it any more than a single line did. }
procedure TChartStrokesTest.NoStrokeCrossesAnUndrawnPoint;
var
    i, Gap, c, Before, After: longint;
begin
    Gap := 400;
    for i := 0 to DenseCount - 1 do
        if (i >= Gap) and (i < Gap + 100) then
            FSerie.AddXY(i, NaN)
        else
            FSerie.AddXY(i, 100);
    Draw;
    Before := FSerie.GetXImgValue(Gap - 1);
    After := FSerie.GetXImgValue(Gap + 100);
    for c := Before + 1 to After - 1 do
        AssertFalse('column ' + IntToStr(c) + ' is inside the gap',
            FRecorder.Covers(clRed, c));
    AssertDrawnEdgeToEdge('before the gap', clRed, 0, Gap - 1);
    AssertDrawnEdgeToEdge('after the gap', clRed, Gap + 100, DenseCount - 1);
end;

initialization
    RegisterTest('unit', TChartStrokesTest);
end.
