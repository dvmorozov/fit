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
widget set does, so what these tests assert is what reaches the screen: a
LineTo covers its pixels less the last one, and a stroke of no length covers
nothing. The chart is driven through Refresh - what Paint runs - so the series
is laid out and drawn exactly as on screen, with only the canvas replaced.
}
unit testcase_chart_strokes;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, Math, fpcunit, testregistry, Graphics, TAGraph;

type
    { A canvas that draws nothing and remembers, per pen colour, which pixel
      columns the strokes it was given would have covered. }
    TRecordingCanvas = class(TCanvas)
    private
        FColumns: array of record
            Color: TColor;
            Column: longint;
        end;
        procedure Cover(AColor: TColor; AFrom, ATo: longint);
    protected
        procedure DoMoveTo(X, Y: integer); override;
        procedure DoLineTo(X, Y: integer); override;
    public
        { Nothing here has a handle, and nothing needs one. }
        procedure RequiredState(ReqState: TCanvasState); override;
        procedure Polyline(Points: PPoint; NumPts: integer); override;
        function TextExtent(const Text: string): TSize; override;
        function Covers(AColor: TColor; AColumn: longint): boolean;
    end;

    { The chart with its bitmap replaced by the recorder - canvas and size, the
      seam TTAChart.GetCanvas, GetWidth and GetHeight exist for. }
    TRecordingChart = class(TTAChart)
    private
        FRecorder: TRecordingCanvas;
    protected
        function GetCanvas: TCanvas; override;
        function GetWidth: longint; override;
        function GetHeight: longint; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        property Recorder: TRecordingCanvas read FRecorder;
    end;

    TChartStrokesTest = class(TTestCase)
    private
        FChart: TRecordingChart;
        FSerie: TTASerie;
        procedure AssertDrawnEdgeToEdge(const AWhat: string; AColor: TColor;
            AFirst, ALast: longint);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFlatStretchDenserThanThePixelsIsDrawnEdgeToEdge;
        procedure ANoisyStretchDenserThanThePixelsIsDrawnEdgeToEdge;
        procedure EachColourIsDrawnOverItsOwnStretch;
        procedure NoStrokeCrossesAnUndrawnPoint;
    end;

implementation

const
    { More samples than the plot has columns, as a real profile has. }
    DenseCount = 1000;

procedure TRecordingCanvas.RequiredState(ReqState: TCanvasState);
begin
end;

procedure TRecordingCanvas.Cover(AColor: TColor; AFrom, ATo: longint);
var
    c, n: longint;
begin
    if AFrom > ATo then
    begin
        c := AFrom; AFrom := ATo; ATo := c;
    end;
    for c := AFrom to ATo do
    begin
        n := Length(FColumns);
        SetLength(FColumns, n + 1);
        FColumns[n].Color := AColor;
        FColumns[n].Column := c;
    end;
end;

procedure TRecordingCanvas.DoMoveTo(X, Y: integer);
begin
end;

{ The LCL's Qt6 LineTo, as far as a column can tell: the end is pulled back one
  pixel towards the start on each axis (TQtDeviceContext.GetLineLastPixelPos),
  and a line left with no length is not drawn at all. }
procedure TRecordingCanvas.DoLineTo(X, Y: integer);
var
    EndX, EndY: longint;
begin
    EndX := X - Sign(X - PenPos.X);
    EndY := Y - Sign(Y - PenPos.Y);
    if (EndX = PenPos.X) and (EndY = PenPos.Y) then
        Exit;
    Cover(Pen.Color, PenPos.X, EndX);
end;

{ A polyline is drawn whole, through every vertex it is given - unless none of
  its segments has any length, which draws nothing, as a line would not. }
procedure TRecordingCanvas.Polyline(Points: PPoint; NumPts: integer);
var
    i: longint;
    HasLength: boolean;
begin
    HasLength := False;
    for i := 1 to NumPts - 1 do
        if (Points[i].X <> Points[0].X) or (Points[i].Y <> Points[0].Y) then
            HasLength := True;
    if not HasLength then
        Exit;
    for i := 1 to NumPts - 1 do
        Cover(Pen.Color, Points[i - 1].X, Points[i].X);
end;

function TRecordingCanvas.TextExtent(const Text: string): TSize;
begin
    Result.cx := 6 * Length(Text);
    Result.cy := 12;
end;

function TRecordingCanvas.Covers(AColor: TColor; AColumn: longint): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to High(FColumns) do
        if (FColumns[i].Color = AColor) and (FColumns[i].Column = AColumn) then
            Exit(True);
end;

constructor TRecordingChart.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FRecorder := TRecordingCanvas.Create;
end;

destructor TRecordingChart.Destroy;
begin
    inherited Destroy;
    FRecorder.Free;
end;

function TRecordingChart.GetCanvas: TCanvas;
begin
    Result := FRecorder;
end;

{ A plot some 270 columns wide: fewer than the samples, as on screen. }
function TRecordingChart.GetWidth: longint;
begin
    Result := 320;
end;

function TRecordingChart.GetHeight: longint;
begin
    Result := 200;
end;

procedure TChartStrokesTest.SetUp;
begin
    FChart := TRecordingChart.Create(nil);
    FChart.AxisColor := clBlack;
    FSerie := TTASerie.Create(nil);
    FChart.AddSerie(FSerie);
end;

procedure TChartStrokesTest.TearDown;
begin
    FreeAndNil(FChart);
    FSerie := nil;
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
        if not FChart.Recorder.Covers(AColor, c) then
            Inc(Missing);
    AssertEquals(AWhat + ': columns of ' + IntToStr(FromColumn) + '..' +
        IntToStr(ToColumn) + ' left blank', 0, Missing);
end;

procedure TChartStrokesTest.AFlatStretchDenserThanThePixelsIsDrawnEdgeToEdge;
var
    i: longint;
begin
    for i := 0 to DenseCount - 1 do
        FSerie.AddXY(i, 100, clRed);
    FChart.Refresh;
    AssertDrawnEdgeToEdge('flat', clRed, 0, DenseCount - 1);
end;

{ A background is not flat: counting noise moves it a pixel up or down from one
  sample to the next, which the shortening turned into no line either. }
procedure TChartStrokesTest.ANoisyStretchDenserThanThePixelsIsDrawnEdgeToEdge;
var
    i: longint;
begin
    for i := 0 to DenseCount - 1 do
        FSerie.AddXY(i, 100 + (i mod 3), clRed);
    FSerie.AddXY(DenseCount, 400, clRed);
    FChart.Refresh;
    AssertDrawnEdgeToEdge('noisy', clRed, 0, DenseCount - 1);
end;

{ A series may colour its points one by one; each stretch keeps its own colour,
  so joining the samples into longer strokes must not join across a change. }
procedure TChartStrokesTest.EachColourIsDrawnOverItsOwnStretch;
var
    i, Half: longint;
begin
    Half := DenseCount div 2;
    for i := 0 to DenseCount - 1 do
        if i < Half then
            FSerie.AddXY(i, 100, clRed)
        else
            FSerie.AddXY(i, 100, clBlue);
    FChart.Refresh;
    AssertDrawnEdgeToEdge('red half', clRed, 0, Half - 1);
    AssertDrawnEdgeToEdge('blue half', clBlue, Half, DenseCount - 1);
    AssertFalse('and red stops where blue begins',
        FChart.Recorder.Covers(clRed, FSerie.GetXImgValue(DenseCount - 1)));
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
            FSerie.AddXY(i, NaN, clRed)
        else
            FSerie.AddXY(i, 100, clRed);
    FChart.Refresh;
    Before := FSerie.GetXImgValue(Gap - 1);
    After := FSerie.GetXImgValue(Gap + 100);
    for c := Before + 1 to After - 1 do
        AssertFalse('column ' + IntToStr(c) + ' is inside the gap',
            FChart.Recorder.Covers(clRed, c));
    AssertDrawnEdgeToEdge('before the gap', clRed, 0, Gap - 1);
    AssertDrawnEdgeToEdge('after the gap', clRed, Gap + 100, DenseCount - 1);
end;

initialization
    RegisterTest('unit', TChartStrokesTest);
end.
