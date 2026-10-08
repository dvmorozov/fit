// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The chart the window draws on: Lazarus's TAChart, and the few things
Fit asks of a chart that TAChart does not do by itself.)

WHAT THIS REPLACED. Until 2026-09 the window drew on Packages/TAGraph, a private
fork of Philippe Martinole's 2005 TAChart that had grown by eighteen years of
local changes: interval bands, per-point captions, highlighted curves, axis marks
supplied by the axis modes, high-DPI scaling, a polyline stroke for Qt6, a
repaint timer. Upstream TAChart - the same component, kept up by the Lazarus
project since - already does most of that, and it carries the LCL's modified
LGPL, whose linking exception the fork's plain LGPL v2 did not have. So the fork
was deleted and TAChart used instead, and what is here is the difference.

EVERYTHING HERE GOES THROUGH TACHART'S OWN EXTENSION POINTS - a series subclass,
a marks source, the chart's mouse methods and its after-draw event - and nothing
edits TAChart itself. An edit to TAChart would have to be published as a
modified copy of it under its licence; the rule, recorded in the roadmap, is to
write Fit-side code instead wherever that is possible, and here it always was.

WHAT FIT ADDS, and why TAChart's own does not serve:

  * The fit-interval bands (TFitSerie.DrawBands). Pairs of points are the ends
    of an interval, drawn as vertical lines through the plot and a diagonal
    hatch between them. TAChart has no series that draws that.

  * The drawing order (TFitSerie.UpdateZOrder). Bands under the data, a
    highlighted curve over it, everything else in the order it was added.
    TAChart orders by ZPosition with an unstable sort, so the add order has to
    be written into ZPosition too, or curves of equal rank swap places.

  * The extents of a series come from the points it DRAWS (TFitSerie.Extent). A
    point with no place on the chart - a coordinate that is NaN - takes no part
    in either axis; TAChart leaves out the NaN coordinate alone.

  * The axis marks (TFitAxisMarks). The window's axis modes say where marks go
    and how they read - a date, a duration, a logarithmic value - through two
    events per axis, which a marks source turns into TAChart's marks.

  * The zoom gesture's mouse events. TAChart's zoom tool draws its rectangle
    inside the chart's own painting (DrawingMode tdmNormal), which is what the
    fork could not do: it drew the rubber band straight onto the control with
    an XOR pen, which Qt and double-buffered canvases never show, so zooming
    worked with no rectangle at all. But a tool that takes a mouse event keeps
    it from the control's OnMouseDown, and the window picks points on a click -
    so the chart raises OnPlotMouseDown and OnPlotMouseUp itself, for every
    press and release, before any tool sees them.

  * The crosshair (MouseMove, DrawReticule). It snaps to the nearest point of
    any series on show, by pixel distance, however far away, and says which
    point that is - the window's picks are taken from it. TAChart's crosshair
    tool has a grab radius and cannot know which of Fit's series are hidden,
    so it would have answered a different question.

  * High-DPI (Sc). TAChart 4.8 draws its margins, ticks and pointer sizes in
    literal pixels. Every size set here goes through Sc, which scales a size
    quoted at 96 dpi to the font's resolution and never to zero.

  * The repaint timing (Paint). How long each repaint took is logged, and the
    window recorder takes its frames on it.
}
unit fit_chart;

interface

uses
    Classes, SysUtils, Types, Math, Graphics, Controls,
    TAGraph, TASeries, TACustomSeries, TATypes, TAChartUtils, TADrawUtils,
    TACustomSource, TAIntervalSources, TATools,
    series_style, app_theme;

type
    { How long one repaint took. ADetail names the series that took a
      measurable part of it. }
    TChartPaintTimingEvent = procedure(ADurationMs: Int64; const ADetail: string)
        of object;
    { Where an axis's marks go, for values with units of their own. Setting
      AHandled places them from AStart every AStep; left False, the chart places
      its own. }
    TChartMarksEvent = procedure(AMin, AMax: double; var AStart, AStep: double;
        var AHandled: boolean) of object;
    { The text of the mark at AValue. Left unassigned, a mark reads as a number. }
    TChartMarkTextEvent = function(AValue, AStep: double): string of object;
    { The crosshair has snapped to point Index of series IndexSerie, which is at
      pixel (Xi, Yi) and has the values (Xg, Yg). }
    TDrawReticule = procedure(Sender: TComponent; IndexSerie, Index, Xi, Yi: integer;
        Xg, Yg: double) of object;

    { Point AIndex of series ASerie was dragged and let go at (AX, AY), in the
      values of the axes. The chart has put the point back where it was: the
      model decides whether it moves, and its replot is what shows it. }
    TPointDraggedEvent = procedure(Sender: TObject; ASerie, AIndex: integer;
        AX, AY: double) of object;

    { The marks of one axis, from the two events the axis modes answer. }
    TFitAxisMarks = class(TIntervalChartSource)
    private
        FOnMarks: TChartMarksEvent;
        FOnMarkText: TChartMarkTextEvent;
    public
        procedure ValuesInRange(AParams: TValuesInRangeParams;
            var AValues: TChartValueTextArray); override;
        property OnMarks: TChartMarksEvent read FOnMarks write FOnMarks;
        property OnMarkText: TChartMarkTextEvent read FOnMarkText write FOnMarkText;
    end;

    { One series of the window's chart. }
    TFitSerie = class(TLineSeries)
    private
        FMarker: TSeriesMarker;
        FImageSize: integer;
        FHollow: boolean;
        FLineWidth: integer;
        FDrawOnTop: boolean;
        FInitShowLines: boolean;
        FInitShowPoints: boolean;
        FCaptions: TStringList;
        FDragKind: string;
        FColorSource: TSeriesColorSource;
        { The opening, highest and lowest value at each point, when the series
          is drawn as candles; the point's own y is where it closed. }
        FOpen, FHigh, FLow: array of double;
        { Where this series came in the order series were added to its chart. }
        FOrdinal: integer;
        FExtent: TDoubleRect;
        FExtentValid: boolean;
        function Sc(ADesignPixels: integer): integer;
        procedure ApplySizes;
        procedure UpdateZOrder;
        procedure SetMarker(AValue: TSeriesMarker);
        procedure SetImageSize(AValue: integer);
        procedure SetHollow(AValue: boolean);
        procedure SetLineWidth(AValue: integer);
        procedure SetDrawOnTop(AValue: boolean);
        function GetFitSeriesColor: TColor;
        procedure SetFitSeriesColor(AValue: TColor);
        procedure SetColorSource(const AValue: TSeriesColorSource);
        procedure CaptionOf(ASeries: TChartSeries; APointIndex, AXIndex,
            AYIndex: integer; var AFormattedMark: string);
        procedure DrawBands(ADrawer: IChartDrawer);
        procedure DrawCandles(ADrawer: IChartDrawer);
        function HasCandle(AIndex: integer): boolean;
        procedure DrawHatch(ADrawer: IChartDrawer; AX1, AY1, AX2, AY2: integer;
            ADescending: boolean);
    protected
        procedure AfterAdd; override;
        procedure SourceChanged(ASender: TObject); override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Draw(ADrawer: IChartDrawer); override;
        { The rectangle the points that are DRAWN occupy: a point with a NaN in
          either coordinate takes no part in either axis. }
        function Extent: TDoubleRect; override;

        { Every point replaced by AX, AY - one change to the chart, however
          many points: a frame of an animated fit refills every curve, and a
          change per point was a repaint per point (testcase_chart_refill). }
        procedure ReplacePoints(const AX, AY: array of double);
        { True for a series that paints an area behind the data rather than a
          curve - the fit-interval and selected-point bands. }
        function IsBackgroundBand: boolean;
        { False for a point with no place on the chart. Such a point is kept, so
          indices still match the data, and left undrawn. }
        function PointIsDrawn(AIndex: integer): boolean;
        { Whether the series is drawn at all - the legend hides one by turning
          both off. }
        function IsShown: boolean;
        { Where point AIndex is on the canvas. An undrawn point is parked far off
          it: a NaN cannot be rounded to a pixel, and a pick aimed at the
          nearest point must never find one there. }
        function GetXImgValue(AIndex: integer): integer;
        function GetYImgValue(AIndex: integer): integer;

        { A caption for each point, in point order. Copied; nil takes them all
          away. They follow the points by position. }
        procedure SetCaptions(ACaptions: TStrings);
        { The caption of point AIndex, or '' for one that has none. }
        function PointCaption(AIndex: integer): string;

        { DRAWN AS CANDLES: for each point, where it opened, its highest and its
          lowest value - the point's y is where it closed. Empty arrays draw the
          series as it was again. Copied, and matched to the points by position. }
        procedure SetCandles(const AOpen, AHigh, ALow: array of double);
        function IsCandles: boolean;

        { The shape drawn at each point, in the framework's own vocabulary. }
        property Marker: TSeriesMarker read FMarker write SetMarker;
        { The marker's size, in pixels at 96 dpi. }
        property ImageSize: integer read FImageSize write SetImageSize;
        { Markers drawn as outlines. }
        property Hollow: boolean read FHollow write SetHollow;
        { The pen the whole series is drawn with, lines and marker outlines, in
          pixels at 96 dpi. }
        property LineWidth: integer read FLineWidth write SetLineWidth;
        { Drawn after every series that is not. The chart's list of series keeps
          its order - the viewer's lookups by index rely on it. }
        property DrawOnTop: boolean read FDrawOnTop write SetDrawOnTop;
        { How the series was created to look, which the "View markers" toggle
          asks - ShowLines and ShowPoints are what the legend has made of it. }
        property InitShowLines: boolean read FInitShowLines write FInitShowLines;
        property InitShowPoints: boolean read FInitShowPoints write FInitShowPoints;
        { The colour of the line, the markers and the captions alike. }
        property SeriesColor: TColor read GetFitSeriesColor write SetFitSeriesColor;
        { Where SeriesColor comes from. A role or a module's colour is resolved
          against the palette in force at once, and again by ApplyPalette when
          View > Theme changes it; a fixed source leaves SeriesColor alone. }
        property ColorSource: TSeriesColorSource read FColorSource
            write SetColorSource;
        { Resolves the colour source again, in APalette. }
        procedure ApplyPalette(const APalette: TThemePalette);
        { The set this series' points belong to when they can be dragged on
          the chart (pick_target names them), or '' when they cannot. }
        property DragKind: string read FDragKind write FDragKind;
    end;

    { The window's chart. }
    TFitChart = class(TChart)
    private
        FToolset: TChartToolset;
        FZoomTool: TZoomDragTool;
        FXMarks: TFitAxisMarks;
        FYMarks: TFitAxisMarks;
        FNextOrdinal: integer;
        FAxisColor: TColor;
        FShowAxisLabel: boolean;
        FXAxisLabel: string;
        FYAxisLabel: string;
        FShowReticule: boolean;
        FHasReticule: boolean;
        FReticule: TPoint;
        FOnDrawReticule: TDrawReticule;
        FOnPlotMouseDown: TMouseEvent;
        FOnPlotMouseUp: TMouseEvent;
        FOnPaintTiming: TChartPaintTimingEvent;
        FPaintDetail: string;
        FScaledAt: integer;
        FOnPointDragged: TPointDraggedEvent;
        { The point being dragged: its series, index and pixel, and whether one
          is. FDragMoved says the pointer has left it since the press. }
        FDragging: boolean;
        FDragMoved: boolean;
        FDragSerie: integer;
        FDragIndex: integer;
        FDragFrom: TPoint;
        FDragAt: TPoint;
        function GrabbedPoint(X, Y: integer; out ASerie, AIndex: integer): boolean;
        function GetOnXMarks: TChartMarksEvent;
        function GetOnXMarkText: TChartMarkTextEvent;
        function GetOnYMarks: TChartMarksEvent;
        function GetOnYMarkText: TChartMarkTextEvent;
        procedure SetOnXMarks(AValue: TChartMarksEvent);
        procedure SetOnXMarkText(AValue: TChartMarkTextEvent);
        procedure SetOnYMarks(AValue: TChartMarksEvent);
        procedure SetOnYMarkText(AValue: TChartMarkTextEvent);
        procedure SetAxisColor(AValue: TColor);
        procedure SetShowAxisLabel(AValue: boolean);
        procedure SetXAxisLabel(const AValue: string);
        procedure SetYAxisLabel(const AValue: string);
        procedure SetShowReticule(AValue: boolean);
        procedure ApplyAxisTitles;
        procedure ApplyScale;
        function Window: TDoubleRect;
        procedure SetWindow(const AWindow: TDoubleRect);
        function GetXGraphMin: double;
        function GetXGraphMax: double;
        function GetYGraphMin: double;
        function GetYGraphMax: double;
        function GetFullXGraphMin: double;
        function GetFullXGraphMax: double;
        function GetFullYGraphMin: double;
        function GetFullYGraphMax: double;
        procedure SetXGraphMin(AValue: double);
        procedure SetXGraphMax(AValue: double);
        procedure SetYGraphMin(AValue: double);
        procedure SetYGraphMax(AValue: double);
        procedure Rescale(AFactor: double);
        procedure DrawReticule(ASender: TChart; ADrawer: IChartDrawer);
        procedure FollowPointer(X, Y: integer);
    protected
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
            X, Y: integer); override;
        procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
            X, Y: integer); override;
    public
        constructor Create(AOwner: TComponent); override;
        procedure Paint; override;
        { One design pixel, at 96 dpi, in the pixels of the display: scaled to
          the font's resolution, and never to zero for a positive size. }
        function Sc(ADesignPixels: integer): integer;
        { The next place in the order series are added in. }
        function TakeOrdinal: integer;
        { Narrows the window shown by a tenth on each side. }
        procedure ZoomIn;
        { Widens it by a tenth on each side. }
        procedure ZoomOut;
        { Notes that a series took AMs of the repaint in progress. }
        procedure NoteSeriesTime(const ATitle: string; AMs: Int64);

        { The window shown, in the values of the axes. Unzoomed, the whole data. }
        property XGraphMin: double read GetXGraphMin write SetXGraphMin;
        property XGraphMax: double read GetXGraphMax write SetXGraphMax;
        property YGraphMin: double read GetYGraphMin write SetYGraphMin;
        property YGraphMax: double read GetYGraphMax write SetYGraphMax;
        { The whole data, whatever the window: what the scroll bars measure the
          window against. }
        property FullXGraphMin: double read GetFullXGraphMin;
        property FullXGraphMax: double read GetFullXGraphMax;
        property FullYGraphMin: double read GetFullYGraphMin;
        property FullYGraphMax: double read GetFullYGraphMax;
        { The colour of the axes, their marks and their titles. }
        property AxisColor: TColor read FAxisColor write SetAxisColor;
        { Paints the plot area, the axes and every series that follows the
          palette in APalette - what View > Theme does to a chart. }
        procedure ApplyPalette(const APalette: TThemePalette);
    published
        property ShowAxisLabel: boolean read FShowAxisLabel write SetShowAxisLabel
            default True;
        property XAxisLabel: string read FXAxisLabel write SetXAxisLabel;
        property YAxisLabel: string read FYAxisLabel write SetYAxisLabel;
        { Follow the pointer with a crosshair snapped to the nearest point. }
        property ShowReticule: boolean read FShowReticule write SetShowReticule
            default False;
        property OnDrawReticule: TDrawReticule read FOnDrawReticule
            write FOnDrawReticule;
        { Every press and release on the chart, whatever a tool made of it. }
        property OnPlotMouseDown: TMouseEvent read FOnPlotMouseDown
            write FOnPlotMouseDown;
        property OnPlotMouseUp: TMouseEvent read FOnPlotMouseUp write FOnPlotMouseUp;
        property OnPaintTiming: TChartPaintTimingEvent read FOnPaintTiming
            write FOnPaintTiming;
        { A point of a draggable series was dragged and let go. }
        property OnPointDragged: TPointDraggedEvent read FOnPointDragged
            write FOnPointDragged;
        property OnXMarks: TChartMarksEvent read GetOnXMarks write SetOnXMarks;
        property OnXMarkText: TChartMarkTextEvent read GetOnXMarkText
            write SetOnXMarkText;
        property OnYMarks: TChartMarksEvent read GetOnYMarks write SetOnYMarks;
        property OnYMarkText: TChartMarkTextEvent read GetOnYMarkText
            write SetOnYMarkText;
    end;

{ The index, among APoints, of the one nearest to AAt - the last of equals -
  or -1 when there are none. Points at NaN are skipped. }
function NearestPoint(const APoints: array of TPoint; const AAt: TPoint): integer;

implementation

const
    { The spacing of the hatch lines filling a band, in pixels. A fit interval
      defaults to the whole profile, so the hatch routinely covers the whole
      plot and is read through, not looked at; platform hatch brushes sit at
      eight, and for a fill this large sparser is better. }
    HatchStep = 16;
    { The most marks - and so grid lines - an axis draws. TAChart places its
      own a few dozen pixels apart, which on a wide window was fifty lines
      across a few degrees: a grid that dense is read as texture, not as
      coordinates. Ten is what the eye can still count across. }
    MaxAxisMarks = 10;
    { The three layers series are drawn in, each LayerSpan wide so that the add
      order fits inside one. TChartDistance cannot be negative. }
    LayerSpan = 100000000;
    BandLayer = 0;
    { Where an undrawn point is on the canvas - see GetXImgValue. }
    UndrawnImage = -(MaxInt div 4);
    DataLayer = 1;
    TopLayer = 2;

function NearestPoint(const APoints: array of TPoint; const AAt: TPoint): integer;
var
    i: integer;
    Best, D, DX, DY: Int64;
begin
    //  In whole pixels, squared, in 64 bits: exact, and no floating point for a
    //  parked point (UndrawnImage) to overflow.
    Result := -1;
    Best := High(Int64);
    for i := 0 to High(APoints) do
    begin
        DX := APoints[i].X - AAt.X;
        DY := APoints[i].Y - AAt.Y;
        D := DX * DX + DY * DY;
        if D <= Best then
        begin
            Best := D;
            Result := i;
        end;
    end;
end;

{ ------------------------------ TFitAxisMarks ------------------------------- }

{ Every k-th of AValues, marked every AStep from AAnchor, for the smallest k
  of 1, 2, 5, 10, 20, 50... that leaves no more than MaxAxisMarks; AStep
  becomes the step between the ones kept.

  BY VALUE, NOT BY POSITION: a mark is kept when it is a whole number of kept
  steps from the anchor, so scrolling the window moves the grid with the data
  instead of re-counting it from whichever mark is first on screen. And by
  multiples 2, 5, 10 of the step asked for, which keep a step that was a round
  number a round number - 0.1 becomes 0.2, 0.5 or 1, never 0.3. }
procedure KeepFewMarks(var AValues: TChartValueTextArray; var AStep: double;
    AAnchor: double);
const
    Factors: array[0..2] of integer = (2, 5, 10);
var
    Kept: TChartValueTextArray;
    k, Decade, i, n, Idx: Int64;
    f: integer;
begin
    if (Length(AValues) <= MaxAxisMarks) or (AStep <= 0) then
        Exit;
    Decade := 1;
    repeat
        for f := 0 to High(Factors) do
        begin
            k := Factors[f] * Decade;
            SetLength(Kept, Length(AValues));
            n := 0;
            for i := 0 to High(AValues) do
            begin
                Idx := Round((AValues[i].FValue - AAnchor) / AStep);
                if Idx mod k = 0 then
                begin
                    Kept[n] := AValues[i];
                    Inc(n);
                end;
            end;
            if n <= MaxAxisMarks then
            begin
                SetLength(Kept, n);
                AValues := Kept;
                AStep := AStep * k;
                Exit;
            end;
        end;
        Decade := Decade * 10;
    until Decade > High(Int64) div 100;
end;

procedure TFitAxisMarks.ValuesInRange(AParams: TValuesInRangeParams;
    var AValues: TChartValueTextArray);
var
    Start, Step, V: double;
    Handled: boolean;
    i, n: integer;
begin
    Handled := False;
    Start := AParams.FMin;
    Step := 0;
    if Assigned(FOnMarks) then
        FOnMarks(AParams.FMin, AParams.FMax, Start, Step, Handled);
    if Handled and (Step > 0) then
    begin
        SetLength(AValues, 0);
        n := 0;
        //  Counted rather than accumulated, so a hundred steps do not drift.
        i := Ceil((AParams.FMin - Start) / Step - 1e-9);
        V := Start + i * Step;
        while V <= AParams.FMax + Step * 1e-9 do
        begin
            SetLength(AValues, n + 1);
            AValues[n].FValue := V;
            AValues[n].FText := '';
            Inc(n);
            Inc(i);
            V := Start + i * Step;
        end;
    end
    else
    begin
        inherited ValuesInRange(AParams, AValues);
        if Length(AValues) > 1 then
            Step := AValues[1].FValue - AValues[0].FValue;
        //  TAChart's own marks are whole multiples of their step.
        Start := 0;
    end;
    //  Before the text, which is written for the step between marks shown: a
    //  date axis marked every month writes months, every year writes years.
    KeepFewMarks(AValues, Step, Start);
    if Assigned(FOnMarkText) then
        for i := 0 to High(AValues) do
            AValues[i].FText := FOnMarkText(AValues[i].FValue, Step);
end;

{ -------------------------------- TFitSerie --------------------------------- }

constructor TFitSerie.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCaptions := TStringList.Create;
    FMarker := smRectangle;
    FImageSize := 2;
    FLineWidth := 1;
    ShowLines := True;
    ShowPoints := False;
    FInitShowLines := True;
    //  Captions are the marks' text, and only a point with one shows any.
    OnGetMarkText := CaptionOf;
    Marks.Frame.Visible := False;
    Marks.LinkPen.Visible := False;
    Marks.LabelBrush.Style := bsClear;
    Marks.Clipped := True;
    Pointer.Style := psRectangle;
    ApplySizes;
    UpdateZOrder;
end;

destructor TFitSerie.Destroy;
begin
    FreeAndNil(FCaptions);
    inherited Destroy;
end;

function TFitSerie.Sc(ADesignPixels: integer): integer;
begin
    if ParentChart is TFitChart then
        Result := TFitChart(ParentChart).Sc(ADesignPixels)
    else
        Result := ADesignPixels;
end;

procedure TFitSerie.ApplySizes;
begin
    Pointer.HorizSize := Sc(FImageSize);
    Pointer.VertSize := Sc(FImageSize);
    LinePen.Width := Sc(FLineWidth);
    Pointer.Pen.Width := Sc(FLineWidth);
    Marks.Distance := Sc(FImageSize + 2);
end;

{ The layer says what is drawn over what; the ordinal keeps equals in the order
  they were added, which TAChart's unstable sort would not. }
procedure TFitSerie.UpdateZOrder;
var
    Layer: integer;
begin
    if IsBackgroundBand then
        Layer := BandLayer
    else if FDrawOnTop then
        Layer := TopLayer
    else
        Layer := DataLayer;
    ZPosition := Layer * LayerSpan + FOrdinal;
end;

procedure TFitSerie.AfterAdd;
begin
    inherited AfterAdd;
    if ParentChart is TFitChart then
        FOrdinal := TFitChart(ParentChart).TakeOrdinal;
    //  Sizes quoted at 96 dpi can be scaled only once there is a chart to ask.
    ApplySizes;
    UpdateZOrder;
end;

procedure TFitSerie.ReplacePoints(const AX, AY: array of double);
var
    i: longint;
begin
    //  The source tells its listeners once, at EndUpdate - the clear and every
    //  point added inside are one change.
    ListSource.BeginUpdate;
    try
        Clear;
        for i := 0 to High(AX) do
            AddXY(AX[i], AY[i]);
    finally
        ListSource.EndUpdate;
    end;
end;

procedure TFitSerie.SourceChanged(ASender: TObject);
begin
    FExtentValid := False;
    inherited SourceChanged(ASender);
end;

procedure TFitSerie.SetMarker(AValue: TSeriesMarker);
begin
    FMarker := AValue;
    case AValue of
        smCircle:    Pointer.Style := psCircle;
        smDiagCross: Pointer.Style := psDiagCross;
        smRectangle: Pointer.Style := psRectangle;
    end;
    UpdateZOrder;
end;

procedure TFitSerie.SetImageSize(AValue: integer);
begin
    FImageSize := AValue;
    ApplySizes;
end;

procedure TFitSerie.SetHollow(AValue: boolean);
begin
    FHollow := AValue;
    if AValue then
        Pointer.Brush.Style := bsClear
    else
        Pointer.Brush.Style := bsSolid;
end;

procedure TFitSerie.SetLineWidth(AValue: integer);
begin
    FLineWidth := AValue;
    ApplySizes;
end;

procedure TFitSerie.SetDrawOnTop(AValue: boolean);
begin
    FDrawOnTop := AValue;
    UpdateZOrder;
end;

function TFitSerie.GetFitSeriesColor: TColor;
begin
    Result := LinePen.Color;
end;

procedure TFitSerie.SetFitSeriesColor(AValue: TColor);
begin
    LinePen.Color := AValue;
    Pointer.Pen.Color := AValue;
    Pointer.Brush.Color := AValue;
    Marks.LabelFont.Color := AValue;
end;

procedure TFitSerie.SetColorSource(const AValue: TSeriesColorSource);
begin
    FColorSource := AValue;
    ApplyPalette(CurrentPalette);
end;

procedure TFitSerie.ApplyPalette(const APalette: TThemePalette);
begin
    if SeriesColorFollowsPalette(FColorSource) then
        SeriesColor := SeriesColorIn(APalette, FColorSource);
end;

procedure TFitSerie.CaptionOf(ASeries: TChartSeries; APointIndex, AXIndex,
    AYIndex: integer; var AFormattedMark: string);
begin
    AFormattedMark := PointCaption(APointIndex);
end;

function TFitSerie.IsBackgroundBand: boolean;
begin
    Result := FMarker in [smVertLineBT, smVertLineTB];
end;

function TFitSerie.PointIsDrawn(AIndex: integer): boolean;
begin
    Result := (AIndex >= 0) and (AIndex < Count) and
        not IsNaN(XValue[AIndex]) and not IsNaN(YValue[AIndex]);
end;

function TFitSerie.IsShown: boolean;
begin
    Result := Active and (ShowLines or ShowPoints);
end;

function TFitSerie.GetXImgValue(AIndex: integer): integer;
begin
    if PointIsDrawn(AIndex) then
        Result := ParentChart.XGraphToImage(AxisToGraphX(XValue[AIndex]))
    else
        Result := UndrawnImage;
end;

function TFitSerie.GetYImgValue(AIndex: integer): integer;
begin
    if PointIsDrawn(AIndex) then
        Result := ParentChart.YGraphToImage(AxisToGraphY(YValue[AIndex]))
    else
        Result := UndrawnImage;
end;

procedure TFitSerie.SetCaptions(ACaptions: TStrings);
begin
    if ACaptions = nil then
        FCaptions.Clear
    else
        FCaptions.Assign(ACaptions);
    if FCaptions.Count > 0 then
        Marks.Style := smsLabel
    else
        Marks.Style := smsNone;
end;

function TFitSerie.PointCaption(AIndex: integer): string;
begin
    if (AIndex >= 0) and (AIndex < FCaptions.Count) then
        Result := FCaptions[AIndex]
    else
        Result := '';
end;

procedure TFitSerie.SetCandles(const AOpen, AHigh, ALow: array of double);
var
    i: integer;
begin
    SetLength(FOpen, Length(AOpen));
    SetLength(FHigh, Length(AHigh));
    SetLength(FLow, Length(ALow));
    for i := 0 to High(AOpen) do
        FOpen[i] := AOpen[i];
    for i := 0 to High(AHigh) do
        FHigh[i] := AHigh[i];
    for i := 0 to High(ALow) do
        FLow[i] := ALow[i];
    FExtentValid := False;
    if Assigned(ParentChart) then
        ParentChart.Invalidate;
end;

function TFitSerie.IsCandles: boolean;
begin
    Result := Length(FOpen) > 0;
end;

{ Point AIndex has all four values: a candle to draw and to measure. }
function TFitSerie.HasCandle(AIndex: integer): boolean;
begin
    Result := PointIsDrawn(AIndex) and (AIndex < Length(FOpen)) and
        (AIndex < Length(FHigh)) and (AIndex < Length(FLow)) and
        not IsNaN(FOpen[AIndex]) and not IsNaN(FHigh[AIndex]) and
        not IsNaN(FLow[AIndex]);
end;

function TFitSerie.Extent: TDoubleRect;
var
    i: integer;
begin
    if not FExtentValid then
    begin
        FExtent := EmptyExtent;
        for i := 0 to Count - 1 do
            if PointIsDrawn(i) then
            begin
                UpdateMinMax(XValue[i], FExtent.a.X, FExtent.b.X);
                UpdateMinMax(YValue[i], FExtent.a.Y, FExtent.b.Y);
                //  A candle reaches its high and its low, not only its close.
                if HasCandle(i) then
                begin
                    UpdateMinMax(FHigh[i], FExtent.a.Y, FExtent.b.Y);
                    UpdateMinMax(FLow[i], FExtent.a.Y, FExtent.b.Y);
                end;
            end;
        FExtentValid := True;
    end;
    Result := FExtent;
end;

procedure TFitSerie.Draw(ADrawer: IChartDrawer);
var
    Started: QWord;
begin
    if not IsShown then
        Exit;
    Started := GetTickCount64;
    if IsBackgroundBand then
        DrawBands(ADrawer)
    else if IsCandles then
        DrawCandles(ADrawer)
    else
        inherited Draw(ADrawer);
    if ParentChart is TFitChart then
        TFitChart(ParentChart).NoteSeriesTime(Title, GetTickCount64 - Started);
end;

{ Each point is a bound, drawn as a vertical line through the plot; each pair of
  them - the first and second, the third and fourth - is an interval, hatched
  between its bounds. Drawn with line primitives only: the fork once decided
  where to hatch by reading pixels back off the canvas, one X round trip each,
  and every repaint took seconds. }
procedure TFitSerie.DrawBands(ADrawer: IChartDrawer);
var
    Plot: TRect;
    i, X, XNext: integer;
begin
    Plot := ParentChart.ClipRect;
    ADrawer.SetPenParams(psSolid, SeriesColor, Sc(FLineWidth));
    for i := 0 to Count - 1 do
    begin
        if not PointIsDrawn(i) then
            Continue;
        X := ParentChart.XGraphToImage(AxisToGraphX(XValue[i]));
        if (X >= Plot.Left) and (X <= Plot.Right) then
            ADrawer.Line(X, Plot.Top, X, Plot.Bottom);
    end;
    i := 0;
    while i + 1 < Count do
    begin
        if PointIsDrawn(i) and PointIsDrawn(i + 1) then
        begin
            X := ParentChart.XGraphToImage(AxisToGraphX(XValue[i]));
            XNext := ParentChart.XGraphToImage(AxisToGraphX(XValue[i + 1]));
            DrawHatch(ADrawer, Min(X, XNext), Plot.Top, Max(X, XNext) - 1,
                Plot.Bottom - 1, FMarker = smVertLineTB);
        end;
        Inc(i, 2);
    end;
end;

{ Each point as a candle: a wick from its low to its high, and a body from where
  it opened to where it closed - hollow when it rose, filled when it fell, the
  convention a black-and-white chart has always used, so a candle reads the same
  in any colour. Nothing is joined: a price series drawn as candles is a row of
  bars, not a path. The body is a little over half the space to the nearest
  neighbour, so bars never touch however far the chart is zoomed. }
procedure TFitSerie.DrawCandles(ADrawer: IChartDrawer);
var
    i, X, Gap, HalfWidth, YOpen, YClose: integer;
    Pixels: array of integer;
begin
    SetLength(Pixels, Count);
    for i := 0 to Count - 1 do
        if HasCandle(i) then
            Pixels[i] := ParentChart.XGraphToImage(AxisToGraphX(XValue[i]));
    Gap := MaxInt;
    for i := 1 to Count - 1 do
        if HasCandle(i) and HasCandle(i - 1) and (Pixels[i] <> Pixels[i - 1]) then
            Gap := Min(Gap, Abs(Pixels[i] - Pixels[i - 1]));
    if Gap = MaxInt then
        Gap := Sc(8);
    HalfWidth := Max(1, Round(0.3 * Gap));
    for i := 0 to Count - 1 do
    begin
        if not HasCandle(i) then
            Continue;
        X := Pixels[i];
        ADrawer.SetPenParams(psSolid, SeriesColor, Sc(FLineWidth));
        ADrawer.Line(X, ParentChart.YGraphToImage(AxisToGraphY(FLow[i])),
            X, ParentChart.YGraphToImage(AxisToGraphY(FHigh[i])));
        YOpen := ParentChart.YGraphToImage(AxisToGraphY(FOpen[i]));
        YClose := ParentChart.YGraphToImage(AxisToGraphY(YValue[i]));
        if YValue[i] >= FOpen[i] then
            ADrawer.SetBrushParams(bsClear, SeriesColor)
        else
            ADrawer.SetBrushParams(bsSolid, SeriesColor);
        ADrawer.Rectangle(X - HalfWidth, Min(YOpen, YClose),
            X + HalfWidth, Max(YOpen, YClose));
    end;
end;

{ The diagonal hatch over the inclusive rectangle (AX1,AY1)-(AX2,AY2): down to
  the right when ADescending, up to it otherwise.

  Anchored to the canvas origin - a line is where x+y (or x-y) is a multiple of
  HatchStep in absolute pixels - so the pattern stays put while a bound is
  dragged or the chart scrolls, instead of crawling with the band. Clipped to
  the plot first: a bound dragged far off-screen puts it millions of pixels
  away, and the work is proportional to the rectangle asked for. }
procedure TFitSerie.DrawHatch(ADrawer: IChartDrawer; AX1, AY1, AX2, AY2: integer;
    ADescending: boolean);
var
    Plot: TRect;
    k, kMin, kMax, xa, xb: integer;
begin
    Plot := ParentChart.ClipRect;
    AX1 := Max(AX1, Plot.Left);
    AY1 := Max(AY1, Plot.Top);
    AX2 := Min(AX2, Plot.Right - 1);
    AY2 := Min(AY2, Plot.Bottom - 1);
    if (AX2 < AX1) or (AY2 < AY1) then
        Exit;
    //  k is the invariant of one diagonal: x-y going down-right, x+y going up.
    if ADescending then
    begin
        kMin := AX1 - AY2;
        kMax := AX2 - AY1;
    end
    else
    begin
        kMin := AX1 + AY1;
        kMax := AX2 + AY2;
    end;
    //  The first multiple of HatchStep at or after kMin; div truncates towards
    //  zero, which is already at or above kMin for a negative kMin.
    k := (kMin div HatchStep) * HatchStep;
    if k < kMin then
        Inc(k, HatchStep);
    while k <= kMax do
    begin
        if ADescending then
        begin
            xa := Max(k + AY1, AX1);
            xb := Min(k + AY2, AX2);
            if xa < xb then
                ADrawer.Line(xa, xa - k, xb, xb - k);
        end
        else
        begin
            xa := Max(k - AY2, AX1);
            xb := Min(k - AY1, AX2);
            if xa < xb then
                ADrawer.Line(xa, k - xa, xb, k - xb);
        end;
        Inc(k, HatchStep);
    end;
end;

{ -------------------------------- TFitChart --------------------------------- }

constructor TFitChart.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FAxisColor := clBlack;
    FShowAxisLabel := True;
    FShowReticule := False;
    BackColor := clWindow;
    Legend.Visible := False;
    Title.Visible := False;

    FXMarks := TFitAxisMarks.Create(Self);
    FYMarks := TFitAxisMarks.Create(Self);
    BottomAxis.Marks.Source := FXMarks;
    LeftAxis.Marks.Source := FYMarks;

    FToolset := TChartToolset.Create(Self);
    FZoomTool := TZoomDragTool.Create(FToolset);
    FZoomTool.Toolset := FToolset;
    FZoomTool.Shift := [ssLeft];
    //  INSIDE THE CHART'S OWN PAINTING, not XOR onto the control - see the
    //  unit header for what that cost. Stated, not left to tdmDefault, which
    //  TAChart resolves to XOR on GTK2 and Win32.
    FZoomTool.DrawingMode := tdmNormal;
    FZoomTool.Brush.Style := bsClear;
    //  Down and to the right zooms; any other drag shows everything again;
    //  a click leaves the window as it is.
    FZoomTool.RestoreExtentOn := [zreDragTopLeft, zreDragTopRight,
        zreDragBottomLeft];
    Toolset := FToolset;

    OnAfterDraw := DrawReticule;
    SetAxisColor(FAxisColor);
    ApplyScale;
end;

function TFitChart.Sc(ADesignPixels: integer): integer;
begin
    Result := Scale96ToFont(ADesignPixels);
    if (Result < 1) and (ADesignPixels > 0) then
        Result := 1;
end;

{ The chart's own sizes at the display's resolution: margins, ticks, the gap
  beside a mark. Asked again whenever the resolution may have changed - which
  is on a repaint, since that is when the font the scale is read from is the
  one the chart is drawn with. }
procedure TFitChart.ApplyScale;
var
    Scale: integer;
begin
    Scale := Sc(96);
    if Scale = FScaledAt then
        Exit;
    FScaledAt := Scale;
    Margins.Left := Sc(4);
    Margins.Right := Sc(4);
    Margins.Top := Sc(4);
    Margins.Bottom := Sc(4);
    BottomAxis.TickLength := Sc(4);
    LeftAxis.TickLength := Sc(4);
    BottomAxis.Marks.Distance := Sc(1);
    LeftAxis.Marks.Distance := Sc(1);
end;

function TFitChart.TakeOrdinal: integer;
begin
    Result := FNextOrdinal;
    Inc(FNextOrdinal);
end;

procedure TFitChart.NoteSeriesTime(const ATitle: string; AMs: Int64);
begin
    if AMs > 0 then
        FPaintDetail := FPaintDetail + Format('; %s %d ms', [ATitle, AMs]);
end;

procedure TFitChart.Paint;
var
    Started: QWord;
begin
    ApplyScale;
    FPaintDetail := '';
    Started := GetTickCount64;
    inherited Paint;
    if Assigned(FOnPaintTiming) then
        FOnPaintTiming(GetTickCount64 - Started, FPaintDetail);
end;

function TFitChart.GetOnXMarks: TChartMarksEvent;
begin
    Result := FXMarks.OnMarks;
end;

function TFitChart.GetOnXMarkText: TChartMarkTextEvent;
begin
    Result := FXMarks.OnMarkText;
end;

function TFitChart.GetOnYMarks: TChartMarksEvent;
begin
    Result := FYMarks.OnMarks;
end;

function TFitChart.GetOnYMarkText: TChartMarkTextEvent;
begin
    Result := FYMarks.OnMarkText;
end;

procedure TFitChart.SetOnXMarks(AValue: TChartMarksEvent);
begin
    FXMarks.OnMarks := AValue;
    Invalidate;
end;

procedure TFitChart.SetOnXMarkText(AValue: TChartMarkTextEvent);
begin
    FXMarks.OnMarkText := AValue;
    Invalidate;
end;

procedure TFitChart.SetOnYMarks(AValue: TChartMarksEvent);
begin
    FYMarks.OnMarks := AValue;
    Invalidate;
end;

procedure TFitChart.SetOnYMarkText(AValue: TChartMarkTextEvent);
begin
    FYMarks.OnMarkText := AValue;
    Invalidate;
end;

procedure TFitChart.SetAxisColor(AValue: TColor);
begin
    FAxisColor := AValue;
    BottomAxis.AxisPen.Color := AValue;
    LeftAxis.AxisPen.Color := AValue;
    BottomAxis.TickColor := AValue;
    LeftAxis.TickColor := AValue;
    BottomAxis.Marks.LabelFont.Color := AValue;
    LeftAxis.Marks.LabelFont.Color := AValue;
    BottomAxis.Title.LabelFont.Color := AValue;
    LeftAxis.Title.LabelFont.Color := AValue;
    //  The dashed grid too: left at the component's black, it vanished on the
    //  dark palette.
    BottomAxis.Grid.Color := AValue;
    LeftAxis.Grid.Color := AValue;
    Frame.Color := AValue;
end;

procedure TFitChart.ApplyPalette(const APalette: TThemePalette);
var
    i: longint;
begin
    BackColor := APalette.Background;
    AxisColor := APalette.Axis;
    for i := 0 to Series.Count - 1 do
        if Series[i] is TFitSerie then
            TFitSerie(Series[i]).ApplyPalette(APalette);
    Invalidate;
end;

procedure TFitChart.ApplyAxisTitles;
begin
    BottomAxis.Title.Caption := FXAxisLabel;
    LeftAxis.Title.Caption := FYAxisLabel;
    BottomAxis.Title.Visible := FShowAxisLabel and (FXAxisLabel <> '');
    LeftAxis.Title.Visible := FShowAxisLabel and (FYAxisLabel <> '');
    LeftAxis.Title.LabelFont.Orientation := 900;
end;

procedure TFitChart.SetShowAxisLabel(AValue: boolean);
begin
    FShowAxisLabel := AValue;
    ApplyAxisTitles;
end;

procedure TFitChart.SetXAxisLabel(const AValue: string);
begin
    FXAxisLabel := AValue;
    ApplyAxisTitles;
end;

procedure TFitChart.SetYAxisLabel(const AValue: string);
begin
    FYAxisLabel := AValue;
    ApplyAxisTitles;
end;

procedure TFitChart.SetShowReticule(AValue: boolean);
begin
    FShowReticule := AValue;
    if not AValue then
        FHasReticule := False;
    Invalidate;
end;

{ The window shown. Zoomed, the one the user chose; otherwise the whole data,
  asked of the series rather than of the last drawing, so it is right before
  the first repaint too. }
function TFitChart.Window: TDoubleRect;
begin
    if IsZoomed then
        Result := LogicalExtent
    else
        Result := GetFullExtent;
end;

procedure TFitChart.SetWindow(const AWindow: TDoubleRect);
begin
    LogicalExtent := AWindow;
end;

function TFitChart.GetXGraphMin: double;
begin
    Result := Window.a.X;
end;

function TFitChart.GetXGraphMax: double;
begin
    Result := Window.b.X;
end;

function TFitChart.GetYGraphMin: double;
begin
    Result := Window.a.Y;
end;

function TFitChart.GetYGraphMax: double;
begin
    Result := Window.b.Y;
end;

function TFitChart.GetFullXGraphMin: double;
begin
    Result := GetFullExtent.a.X;
end;

function TFitChart.GetFullXGraphMax: double;
begin
    Result := GetFullExtent.b.X;
end;

function TFitChart.GetFullYGraphMin: double;
begin
    Result := GetFullExtent.a.Y;
end;

function TFitChart.GetFullYGraphMax: double;
begin
    Result := GetFullExtent.b.Y;
end;

procedure TFitChart.SetXGraphMin(AValue: double);
var
    W: TDoubleRect;
begin
    W := Window;
    W.a.X := AValue;
    SetWindow(W);
end;

procedure TFitChart.SetXGraphMax(AValue: double);
var
    W: TDoubleRect;
begin
    W := Window;
    W.b.X := AValue;
    SetWindow(W);
end;

procedure TFitChart.SetYGraphMin(AValue: double);
var
    W: TDoubleRect;
begin
    W := Window;
    W.a.Y := AValue;
    SetWindow(W);
end;

procedure TFitChart.SetYGraphMax(AValue: double);
var
    W: TDoubleRect;
begin
    W := Window;
    W.b.Y := AValue;
    SetWindow(W);
end;

{ The window grown by AFactor of its size on each side - negative to shrink. }
procedure TFitChart.Rescale(AFactor: double);
var
    W: TDoubleRect;
    DX, DY: double;
begin
    if SeriesCount = 0 then
        Exit;
    W := Window;
    DX := (W.b.X - W.a.X) * AFactor;
    DY := (W.b.Y - W.a.Y) * AFactor;
    W.a.X := W.a.X - DX;
    W.b.X := W.b.X + DX;
    W.a.Y := W.a.Y - DY;
    W.b.Y := W.b.Y + DY;
    SetWindow(W);
end;

procedure TFitChart.ZoomIn;
begin
    Rescale(-0.1);
end;

procedure TFitChart.ZoomOut;
begin
    Rescale(0.1);
end;

{ SNAPPED TO THE NEAREST DRAWN POINT OF ANY SERIES ON SHOW, by pixel distance and
  however far away: the window takes its picks from what the crosshair reports,
  so a click anywhere over the data must name a point. The last of equals wins,
  as it did. }
procedure TFitChart.FollowPointer(X, Y: integer);
var
    Candidates: array of TPoint;
    Owners, Indices: array of integer;
    i, j, n, Best: integer;
    S: TFitSerie;
begin
    n := 0;
    for i := 0 to SeriesCount - 1 do
    begin
        if not (Series[i] is TFitSerie) then
            Continue;
        S := TFitSerie(Series[i]);
        if not S.IsShown then
            Continue;
        for j := 0 to S.Count - 1 do
            if S.PointIsDrawn(j) then
            begin
                SetLength(Candidates, n + 1);
                SetLength(Owners, n + 1);
                SetLength(Indices, n + 1);
                Candidates[n] := Point(XGraphToImage(S.AxisToGraphX(S.XValue[j])),
                    YGraphToImage(S.AxisToGraphY(S.YValue[j])));
                Owners[n] := i;
                Indices[n] := j;
                Inc(n);
            end;
    end;
    Best := NearestPoint(Candidates, Point(X, Y));
    if Best < 0 then
        Exit;
    //  Only a point inside the plot: one scrolled out of view is not there.
    with Candidates[Best] do
        if (X < ClipRect.Left) or (X > ClipRect.Right) or
            (Y < ClipRect.Top) or (Y > ClipRect.Bottom) then
            Exit;
    if FHasReticule and (FReticule = Candidates[Best]) then
        Exit;
    FHasReticule := True;
    FReticule := Candidates[Best];
    S := TFitSerie(Series[Owners[Best]]);
    if Assigned(FOnDrawReticule) then
        FOnDrawReticule(Self, Owners[Best], Indices[Best], FReticule.X,
            FReticule.Y, S.XValue[Indices[Best]], S.YValue[Indices[Best]]);
    Invalidate;
end;

{ The draggable point under a press, within a few pixels: by both coordinates
  for an ordinary point, by x alone for a bound, which is a line through the
  whole plot. The nearest of those wins; a series not on show, a gap and a
  series with no DragKind are never taken. }
function TFitChart.GrabbedPoint(X, Y: integer; out ASerie, AIndex: integer): boolean;
var
    i, j, D, Best, Radius: integer;
    S: TFitSerie;
begin
    Result := False;
    ASerie := -1;
    AIndex := -1;
    Radius := Sc(6);
    Best := MaxInt;
    for i := 0 to SeriesCount - 1 do
    begin
        if not (Series[i] is TFitSerie) then
            Continue;
        S := TFitSerie(Series[i]);
        if (S.DragKind = '') or not S.IsShown then
            Continue;
        for j := 0 to S.Count - 1 do
        begin
            if not S.PointIsDrawn(j) then
                Continue;
            if S.IsBackgroundBand then
                D := Abs(S.GetXImgValue(j) - X)
            else
                D := Max(Abs(S.GetXImgValue(j) - X), Abs(S.GetYImgValue(j) - Y));
            if (D <= Radius) and (D < Best) then
            begin
                Best := D;
                ASerie := i;
                AIndex := j;
                Result := True;
            end;
        end;
    end;
end;

{ Drawn with the rest of the chart, after the series: where a dragged point
  would go, as a line through the plot in its series' colour, and the
  crosshair, in the colour of the axes. }
procedure TFitChart.DrawReticule(ASender: TChart; ADrawer: IChartDrawer);
begin
    if FDragging and FDragMoved then
    begin
        ADrawer.SetPenParams(psDash, TFitSerie(Series[FDragSerie]).SeriesColor, 1);
        ADrawer.Line(FDragAt.X, ClipRect.Top, FDragAt.X, ClipRect.Bottom);
    end;
    if not (FShowReticule and FHasReticule) then
        Exit;
    ADrawer.SetPenParams(psSolid, FAxisColor, 1);
    ADrawer.Line(FReticule.X, ClipRect.Top, FReticule.X, ClipRect.Bottom);
    ADrawer.Line(ClipRect.Left, FReticule.Y, ClipRect.Right, FReticule.Y);
end;

procedure TFitChart.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: integer);
begin
    //  FIRST, and whatever a tool makes of it: the zoom tool takes the left
    //  button, and a taken event never reaches OnMouseDown.
    if Assigned(FOnPlotMouseDown) then
        FOnPlotMouseDown(Self, Button, Shift, X, Y);
    //  A PRESS ON A POINT THAT MAY BE DRAGGED IS A DRAG, and no tool sees it -
    //  otherwise the zoom tool would start a rectangle from it. Anywhere else,
    //  the tools as usual.
    FDragging := (Button = mbLeft) and GrabbedPoint(X, Y, FDragSerie, FDragIndex);
    if FDragging then
    begin
        FDragMoved := False;
        FDragFrom := Point(X, Y);
        FDragAt := FDragFrom;
        Exit;
    end;
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TFitChart.MouseMove(Shift: TShiftState; X, Y: integer);
begin
    if FDragging then
    begin
        FDragAt := Point(X, Y);
        FDragMoved := FDragMoved or (FDragAt <> FDragFrom);
        Invalidate;
        Exit;
    end;
    //  Not while a button is held: that is a drag, and the rectangle is what
    //  it shows.
    if FShowReticule and ([ssLeft, ssRight, ssMiddle] * Shift = []) then
        FollowPointer(X, Y);
    inherited MouseMove(Shift, X, Y);
end;

procedure TFitChart.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: integer);
var
    At: TDoublePoint;
    Moved: boolean;
begin
    if FDragging then
    begin
        FDragging := False;
        Moved := FDragMoved or (Point(X, Y) <> FDragFrom);
        Invalidate;
        //  The window hears the release as it hears every other one - a
        //  press and release in one place is still a click on a pick.
        if Assigned(FOnPlotMouseUp) then
            FOnPlotMouseUp(Self, Button, Shift, X, Y);
        //  NOT MOVED HERE: the model decides, and a refused move must leave
        //  the point where it was. Its replot shows it moved.
        if Moved and Assigned(FOnPointDragged) then
        begin
            At := ImageToGraph(Point(X, Y));
            FOnPointDragged(Self, FDragSerie, FDragIndex,
                Series[FDragSerie].GraphToAxisX(At.X),
                Series[FDragSerie].GraphToAxisY(At.Y));
        end;
        Exit;
    end;
    //  The tool first, so a handler reading the window reads the new one.
    inherited MouseUp(Button, Shift, X, Y);
    if Assigned(FOnPlotMouseUp) then
        FOnPlotMouseUp(Self, Button, Shift, X, Y);
end;

end.
