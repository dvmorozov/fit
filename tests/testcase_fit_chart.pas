// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the window's chart does beyond Lazarus's TAChart: the fit-interval
bands, the drawing order, the captions, the zoom gesture, the crosshair, the axis
marks and the window it shows.)

Until 2026-09 the window drew on a private fork of the 2005 TAChart. Everything it
added on top of the original is asked for here of fit_chart, which gets it from
upstream TAChart where TAChart has it and draws it itself where it has not. Each
test says which user-visible behaviour it keeps.

THE CHART IS DRAWN, NOT INSPECTED. Every assertion on appearance is made on what
TAChart's own drawer handed a recording canvas (recording_canvas): the strokes,
rectangles and text a screen would have received, in the order it would have
received them. The mouse is the chart's own MouseDown, MouseMove and MouseUp -
the methods the widget set calls - so the zoom tool and the crosshair are driven
the way a user drives them.
}
unit testcase_fit_chart;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, Math, fpcunit, testregistry, Graphics, Controls,
    TADrawerCanvas, TATypes, fit_chart, series_style, recording_canvas;

type
    { The chart, with the three calls the widget set makes on a mouse gesture
      made public - the way a user's gesture enters.

      And WITHOUT A WINDOW: TAChart's zoom tool captures the mouse when a drag
      starts, which asks the control for its window handle, and the nogui
      widget set cannot create one. Capture then goes to handle 0, which the
      widget set ignores - standing in for the window the application has. }
    TMouseChart = class(TFitChart)
    protected
        procedure CreateHandle; override;
    public
        procedure Press(X, Y: integer; AButton: TMouseButton = mbLeft);
        procedure Hover(X, Y: integer);
        procedure DragTo(X, Y: integer);
        procedure Release(X, Y: integer; AButton: TMouseButton = mbLeft);
    end;

    TFitChartTest = class(TTestCase)
    private
        FChart: TMouseChart;
        FData: TFitSerie;
        FRecorder: TRecordingCanvas;
        FDowns, FUps: integer;
        FReticules: integer;
        FReticuleSerie, FReticuleIndex: integer;
        FReticuleX, FReticuleY: double;
        FDrops, FDropSerie, FDropIndex: integer;
        FDropX, FDropY: double;
        procedure Dropped(Sender: TObject; ASerie, AIndex: integer; AX, AY: double);
        function AddMovable(const AKind: string): TFitSerie;
        function AddCandles: TFitSerie;
        procedure Draw;
        function AddSeries(AColor: TColor; const AXs, AYs: array of double): TFitSerie;
        function AddBand(AMarker: TSeriesMarker; AFrom, ATo: double): TFitSerie;
        function PX(AX: double): integer;
        function PY(AY: double): integer;
        procedure CountDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: integer);
        procedure CountUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: integer);
        procedure Reticule(Sender: TComponent; IndexSerie, Index, Xi, Yi: integer;
            Xg, Yg: double);
        procedure MarksEvery25(AMin, AMax: double; var AStart, AStep: double;
            var AHandled: boolean);
        procedure MarksEvery1(AMin, AMax: double; var AStart, AStep: double;
            var AHandled: boolean);
        function XMarksWritten: integer;
        procedure DrawWide;
        procedure MarksLeftToTheChart(AMin, AMax: double; var AStart, AStep: double;
            var AHandled: boolean);
        function MarkTextX(AValue, AStep: double): string;
        function MarkTextY(AValue, AStep: double): string;
        function DiagonalStrokesIn(AColor: TColor): integer;
        procedure AssertEdgeAt(const AWhat: string; AX: integer);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The fit-interval bands.
        procedure BothBoundsOfABandAreVerticalLinesThroughThePlot;
        procedure ABandIsHatchedBetweenItsBoundsAndNowhereElse;
        procedure ARisingBandIsHatchedUpToTheRight;
        procedure AFallingBandIsHatchedDownToTheRight;
        procedure TheHatchStaysPutAsTheBandMoves;
        procedure BandsAreDrawnUnderTheDataWhateverTheOrderAdded;
        procedure AHiddenBandDrawsNothing;
        procedure ABandScrolledOutOfViewIsNotHatched;

        //  Which series is drawn over which.
        procedure SeriesAreDrawnInTheOrderTheyWereAdded;
        procedure ASeriesDrawnOnTopComesAfterEveryOther;
        procedure TheLineWidthIsThePenTheSeriesIsDrawnWith;

        //  Captions.
        procedure EachCaptionIsWrittenInTheSeriesColour;
        procedure ACaptionIsWrittenBesideItsPoint;
        procedure ACaptionOfAPointOutOfViewIsNotWritten;
        procedure ASeriesWithoutCaptionsWritesNone;

        //  The zoom gesture.
        procedure DraggingDownAndRightZoomsToTheRectangle;
        procedure TheRectangleIsDrawnWhileDragging;
        procedure TheRectangleIsGoneOnceReleased;
        procedure DraggingUpAndLeftShowsEverythingAgain;
        procedure AClickKeepsTheWindow;
        procedure TheWindowHearsEveryPressAndReleaseEvenWhenTheZoomTakesThem;
        procedure ARightClickIsHeardTooAndZoomsNothing;

        //  The crosshair.
        procedure ThePointerSnapsToTheNearestPoint;
        procedure ItReportsThePointsOwnValues;
        procedure AHiddenSeriesIsNotSnappedTo;
        procedure AGapIsNotSnappedTo;
        procedure TheCrosshairIsDrawnThroughThePointItSnappedTo;
        procedure WithoutTheCrosshairNothingIsReported;
        procedure DraggingIsNotFollowedByTheCrosshair;

        //  Candles.
        procedure ACandleIsAWickFromLowToHighAndABodyFromOpenToClose;
        procedure ARisingCandleIsHollowAndAFallingOneFilled;
        procedure CandlesJoinNothing;
        procedure TheCandlesHighsAndLowsAreInTheExtents;
        procedure CandlesTakenAwayLeaveALineAgain;

        //  Dragging a point.
        procedure DroppingADraggedPointSaysWhereItWent;
        procedure TheChartDoesNotMoveThePointItself;
        procedure APressAwayFromAnyDraggablePointStillZooms;
        procedure APointOfASeriesThatIsNotDraggableIsNotGrabbed;
        procedure ABoundIsGrabbedAnywhereAlongItsLine;
        procedure AClickOnADraggablePointIsNotADrop;
        procedure WhileDraggingALineShowsWhereItWillGo;
        procedure AHiddenSeriesIsNotGrabbed;

        //  The axis marks.
        procedure TheXMarksAreWhereTheAxisPutsThem;
        procedure TheYMarksAreWhereTheAxisPutsThem;
        procedure MarksTheAxisDeclinesToPlaceAreTheChartsAndStillItsText;
        procedure WithoutAnEventTheMarksAreNumbers;
        procedure AnAxisMarkedFinelyDrawsNoMoreThanTenLines;
        procedure TheChartsOwnMarksAreNoMoreThanTenOnAWideChart;
        procedure TheMarksKeptStayPutWhenTheWindowMoves;
        procedure MarksAlreadyFewAreAllKept;

        //  The window it shows.
        procedure UnzoomedTheWindowIsTheWholeData;
        procedure ASetWindowIsTheWindowShown;
        procedure ZoomInTakesATenthOffEachSide;
        procedure ZoomOutAddsATenthToEachSide;
        procedure ZoomingAnEmptyChartChangesNothing;
        procedure TheFullRangeIsTheWholeDataEvenWhenZoomed;

        //  Axes and series.
        procedure TheAxisTitlesAreWrittenWhenShown;
        procedure TheAxisTitlesAreNotWrittenWhenHidden;
        procedure TheAxisColourIsTheColourOfTheMarks;
        procedure EachMarkerIsTheShapeItNames;
        procedure TheTwoBandMarkersAreBands;
        procedure TheMarkerSizeIsScaledLikeEverythingElse;
        procedure AHollowMarkerIsNotFilled;
        procedure TheSeriesColourIsTheLineTheMarkersAndTheCaptions;
        procedure TheInitialLookIsRemembered;
        procedure ScalingNeverRoundsAPenToNothing;
    end;

implementation

const
    Red = clRed;
    Blue = clBlue;
    Green = clGreen;

procedure TMouseChart.CreateHandle;
begin
end;

procedure TMouseChart.Press(X, Y: integer; AButton: TMouseButton);
begin
    if AButton = mbLeft then
        MouseDown(AButton, [ssLeft], X, Y)
    else
        MouseDown(AButton, [ssRight], X, Y);
end;

procedure TMouseChart.Hover(X, Y: integer);
begin
    MouseMove([], X, Y);
end;

procedure TMouseChart.DragTo(X, Y: integer);
begin
    MouseMove([ssLeft], X, Y);
end;

procedure TMouseChart.Release(X, Y: integer; AButton: TMouseButton);
begin
    MouseUp(AButton, [], X, Y);
end;

procedure TFitChartTest.SetUp;
begin
    FRecorder := TRecordingCanvas.Create;
    FChart := TMouseChart.Create(nil);
    FDowns := 0;
    FUps := 0;
    FReticules := 0;
    FReticuleSerie := -1;
    FReticuleIndex := -1;
    FDrops := 0;
    //  The data every case draws over: 0..100 on both axes.
    FData := AddSeries(Red, [0, 25, 50, 75, 100], [0, 100, 50, 25, 75]);
end;

procedure TFitChartTest.TearDown;
begin
    FreeAndNil(FChart);
    FData := nil;
    FreeAndNil(FRecorder);
end;

procedure TFitChartTest.Draw;
begin
    FRecorder.Reset;
    FChart.Draw(TRecordingDrawer.Create(FRecorder), Rect(0, 0, 400, 300));
end;

function TFitChartTest.AddSeries(AColor: TColor;
    const AXs, AYs: array of double): TFitSerie;
var
    i: integer;
begin
    Result := TFitSerie.Create(nil);
    Result.SeriesColor := AColor;
    Result.ShowLines := True;
    Result.ShowPoints := False;
    for i := 0 to High(AXs) do
        Result.AddXY(AXs[i], AYs[i]);
    FChart.AddSeries(Result);
end;

function TFitChartTest.AddBand(AMarker: TSeriesMarker; AFrom, ATo: double): TFitSerie;
begin
    Result := TFitSerie.Create(nil);
    Result.Marker := AMarker;
    Result.SeriesColor := Blue;
    Result.ShowLines := False;
    Result.ShowPoints := True;
    Result.AddXY(AFrom, 50);
    Result.AddXY(ATo, 50);
    FChart.AddSeries(Result);
end;

function TFitChartTest.PX(AX: double): integer;
begin
    Result := FChart.XGraphToImage(AX);
end;

function TFitChartTest.PY(AY: double): integer;
begin
    Result := FChart.YGraphToImage(AY);
end;

procedure TFitChartTest.CountDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
begin
    Inc(FDowns);
end;

procedure TFitChartTest.CountUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
begin
    Inc(FUps);
end;

procedure TFitChartTest.Reticule(Sender: TComponent; IndexSerie, Index,
    Xi, Yi: integer; Xg, Yg: double);
begin
    Inc(FReticules);
    FReticuleSerie := IndexSerie;
    FReticuleIndex := Index;
    FReticuleX := Xg;
    FReticuleY := Yg;
end;

procedure TFitChartTest.MarksEvery25(AMin, AMax: double;
    var AStart, AStep: double; var AHandled: boolean);
begin
    AStart := 0;
    AStep := 25;
    AHandled := True;
end;

procedure TFitChartTest.MarksEvery1(AMin, AMax: double;
    var AStart, AStep: double; var AHandled: boolean);
begin
    AStart := 0;
    AStep := 1;
    AHandled := True;
end;

function TFitChartTest.XMarksWritten: integer;
var
    i: integer;
begin
    Result := 0;
    for i := 0 to FRecorder.TextCount - 1 do
        if Copy(FRecorder.Text(i).Text, 1, 1) = 'x' then
            Inc(Result);
end;

{ As wide as the window in the report: 2560 pixels, where the chart's own
  marks came every tenth of a degree. }
procedure TFitChartTest.DrawWide;
begin
    FRecorder.Reset;
    FChart.Draw(TRecordingDrawer.Create(FRecorder), Rect(0, 0, 2560, 1400));
end;

procedure TFitChartTest.MarksLeftToTheChart(AMin, AMax: double;
    var AStart, AStep: double; var AHandled: boolean);
begin
    AHandled := False;
end;

function TFitChartTest.MarkTextX(AValue, AStep: double): string;
begin
    Result := 'x' + FloatToStr(AValue);
end;

function TFitChartTest.MarkTextY(AValue, AStep: double): string;
begin
    Result := 'y' + FloatToStr(AValue);
end;

function TFitChartTest.DiagonalStrokesIn(AColor: TColor): integer;
var
    i: integer;
    S: TRecordedStroke;
begin
    Result := 0;
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color = AColor) and (S.X1 <> S.X2) and (S.Y1 <> S.Y2) then
            Inc(Result);
    end;
end;

procedure TFitChartTest.AssertEdgeAt(const AWhat: string; AX: integer);
var
    i: integer;
    S: TRecordedStroke;
begin
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color = Blue) and (S.X1 = AX) and (S.X2 = AX) and
            (Min(S.Y1, S.Y2) <= FChart.ClipRect.Top) and
            (Max(S.Y1, S.Y2) >= FChart.ClipRect.Bottom) then
            Exit;
    end;
    Fail(AWhat + ': no vertical line through the plot at column ' + IntToStr(AX));
end;

{ ------------------------------ the bands ---------------------------------- }

procedure TFitChartTest.BothBoundsOfABandAreVerticalLinesThroughThePlot;
begin
    AddBand(smVertLineBT, 20, 60);
    Draw;
    AssertEdgeAt('the left bound', PX(20));
    AssertEdgeAt('the right bound', PX(60));
end;

procedure TFitChartTest.ABandIsHatchedBetweenItsBoundsAndNowhereElse;
var
    i: integer;
    S: TRecordedStroke;
begin
    AddBand(smVertLineBT, 20, 60);
    Draw;
    AssertTrue('the band is hatched', DiagonalStrokesIn(Blue) > 3);
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color <> Blue) or (S.X1 = S.X2) then
            Continue;
        AssertTrue('a hatch line starts inside the band',
            (Min(S.X1, S.X2) >= PX(20)) and (Max(S.X1, S.X2) < PX(60)));
        AssertTrue('and stays inside the plot',
            (Min(S.Y1, S.Y2) >= FChart.ClipRect.Top) and
            (Max(S.Y1, S.Y2) <= FChart.ClipRect.Bottom));
    end;
end;

procedure TFitChartTest.ARisingBandIsHatchedUpToTheRight;
var
    i: integer;
    S: TRecordedStroke;
begin
    AddBand(smVertLineBT, 20, 60);
    Draw;
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color = Blue) and (S.X1 <> S.X2) then
            AssertEquals('screen y falls as x grows', -1,
                Sign(S.Y2 - S.Y1) * Sign(S.X2 - S.X1));
    end;
end;

procedure TFitChartTest.AFallingBandIsHatchedDownToTheRight;
var
    i: integer;
    S: TRecordedStroke;
begin
    AddBand(smVertLineTB, 20, 60);
    Draw;
    AssertTrue(DiagonalStrokesIn(Blue) > 3);
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color = Blue) and (S.X1 <> S.X2) then
            AssertEquals('screen y grows with x', 1,
                Sign(S.Y2 - S.Y1) * Sign(S.X2 - S.X1));
    end;
end;

{ Anchored to the canvas, sixteen pixels apart: the pattern does not crawl while
  a bound is dragged or the chart scrolled. }
procedure TFitChartTest.TheHatchStaysPutAsTheBandMoves;
var
    i: integer;
    S: TRecordedStroke;
begin
    AddBand(smVertLineBT, 23, 61);
    Draw;
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color = Blue) and (S.X1 <> S.X2) then
            AssertEquals('on a diagonal of the canvas', 0, (S.X1 + S.Y1) mod 16);
    end;
end;

procedure TFitChartTest.BandsAreDrawnUnderTheDataWhateverTheOrderAdded;
begin
    //  The band is added AFTER the data, which the fork drew in a pass of its
    //  own before every curve.
    AddBand(smVertLineBT, 20, 60);
    Draw;
    AssertTrue('both are drawn',
        (FRecorder.FirstStrokeIn(Blue) >= 0) and (FRecorder.FirstStrokeIn(Red) >= 0));
    AssertTrue('the band first', FRecorder.FirstStrokeIn(Blue) <
        FRecorder.FirstStrokeIn(Red));
end;

procedure TFitChartTest.AHiddenBandDrawsNothing;
var
    Band: TFitSerie;
begin
    Band := AddBand(smVertLineBT, 20, 60);
    Band.ShowLines := False;
    Band.ShowPoints := False;
    Draw;
    AssertEquals(-1, FRecorder.FirstStrokeIn(Blue));
end;

procedure TFitChartTest.ABandScrolledOutOfViewIsNotHatched;
begin
    AddBand(smVertLineBT, 20, 60);
    Draw;
    FChart.XGraphMin := 70;
    FChart.XGraphMax := 100;
    Draw;
    AssertEquals(0, DiagonalStrokesIn(Blue));
end;

{ ----------------------------- drawing order -------------------------------- }

procedure TFitChartTest.SeriesAreDrawnInTheOrderTheyWereAdded;
var
    i: integer;
begin
    AddSeries(Green, [0, 100], [10, 10]);
    AddSeries(Blue, [0, 100], [20, 20]);
    //  More than two, so an unstable sort would show.
    for i := 1 to 20 do
        AddSeries(RGBToColor(i, 0, 0), [0, 100], [i, i]);
    Draw;
    AssertTrue(FRecorder.FirstStrokeIn(Red) < FRecorder.FirstStrokeIn(Green));
    AssertTrue(FRecorder.FirstStrokeIn(Green) < FRecorder.FirstStrokeIn(Blue));
    for i := 2 to 20 do
        AssertTrue('series ' + IntToStr(i),
            FRecorder.FirstStrokeIn(RGBToColor(i - 1, 0, 0)) <
            FRecorder.FirstStrokeIn(RGBToColor(i, 0, 0)));
end;

procedure TFitChartTest.ASeriesDrawnOnTopComesAfterEveryOther;
var
    Top: TFitSerie;
begin
    Top := AddSeries(Green, [0, 100], [10, 10]);
    AddSeries(Blue, [0, 100], [20, 20]);
    Top.DrawOnTop := True;
    Draw;
    AssertTrue(FRecorder.FirstStrokeIn(Blue) < FRecorder.FirstStrokeIn(Green));
    AssertTrue(FRecorder.FirstStrokeIn(Red) < FRecorder.FirstStrokeIn(Green));
    Top.DrawOnTop := False;
    Draw;
    AssertTrue('and back in its place', FRecorder.FirstStrokeIn(Green) <
        FRecorder.FirstStrokeIn(Blue));
end;

procedure TFitChartTest.TheLineWidthIsThePenTheSeriesIsDrawnWith;
var
    Wide: TFitSerie;
begin
    Wide := AddSeries(Green, [0, 100], [10, 90]);
    Wide.LineWidth := 3;
    Draw;
    AssertEquals(FChart.Sc(3),
        FRecorder.Stroke(FRecorder.FirstStrokeIn(Green)).Width);
    AssertEquals(3, Wide.LineWidth);
end;

{ ------------------------------- captions ---------------------------------- }

function Strings(const AItems: array of string): TStringList;
var
    i: integer;
begin
    Result := TStringList.Create;
    for i := 0 to High(AItems) do
        Result.Add(AItems[i]);
end;

procedure TFitChartTest.EachCaptionIsWrittenInTheSeriesColour;
var
    Wave: TFitSerie;
    L: TStringList;
    i: integer;
begin
    Wave := AddSeries(Green, [10, 50, 90], [20, 80, 40]);
    L := Strings(['A', 'B', 'C']);
    try
        Wave.SetCaptions(L);
    finally
        L.Free;
    end;
    Draw;
    AssertTrue(FRecorder.Wrote('A') and FRecorder.Wrote('B') and FRecorder.Wrote('C'));
    for i := 0 to FRecorder.TextCount - 1 do
        if FRecorder.Text(i).Text = 'B' then
            AssertEquals(Green, FRecorder.Text(i).Color);
end;

procedure TFitChartTest.ACaptionIsWrittenBesideItsPoint;
var
    Wave: TFitSerie;
    L: TStringList;
    At: TPoint;
begin
    Wave := AddSeries(Green, [10, 50, 90], [20, 80, 40]);
    L := Strings(['A', 'B', 'C']);
    try
        Wave.SetCaptions(L);
    finally
        L.Free;
    end;
    Draw;
    AssertTrue(FRecorder.WhereWritten('B', At));
    AssertTrue('near the point across', Abs(At.X - PX(50)) < 30);
    AssertTrue('near the point up and down', Abs(At.Y - PY(80)) < 40);
end;

procedure TFitChartTest.ACaptionOfAPointOutOfViewIsNotWritten;
var
    Wave: TFitSerie;
    L: TStringList;
begin
    Wave := AddSeries(Green, [10, 50, 90], [20, 80, 40]);
    L := Strings(['A', 'B', 'C']);
    try
        Wave.SetCaptions(L);
    finally
        L.Free;
    end;
    Draw;
    FChart.XGraphMin := 0;
    FChart.XGraphMax := 60;
    Draw;
    AssertTrue(FRecorder.Wrote('B'));
    AssertFalse('it would be written over the axes', FRecorder.Wrote('C'));
end;

procedure TFitChartTest.ASeriesWithoutCaptionsWritesNone;
var
    Wave: TFitSerie;
    L: TStringList;
begin
    Wave := AddSeries(Green, [10, 50, 90], [20, 80, 40]);
    L := Strings(['A', 'B', 'C']);
    try
        Wave.SetCaptions(L);
    finally
        L.Free;
    end;
    Wave.SetCaptions(nil);
    Draw;
    AssertFalse(FRecorder.Wrote('A'));
end;

{ ----------------------------- the zoom gesture ----------------------------- }

procedure TFitChartTest.DraggingDownAndRightZoomsToTheRectangle;
var
    Tolerance: double;
begin
    Draw;
    //  A pixel's worth of data either way, from rounding to pixels.
    Tolerance := 2 * 100 / (FChart.ClipRect.Right - FChart.ClipRect.Left);
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(40), PY(50));
    FChart.DragTo(PX(60), PY(30));
    FChart.Release(PX(60), PY(30));
    AssertEquals('left', 20, FChart.XGraphMin, Tolerance);
    AssertEquals('right', 60, FChart.XGraphMax, Tolerance);
    AssertEquals('bottom', 30, FChart.YGraphMin, 2 * Tolerance);
    AssertEquals('top', 80, FChart.YGraphMax, 2 * Tolerance);
end;

{ The rectangle is how the user sees what they are about to zoom to. The fork
  drew it straight onto the control with an XOR pen while the mouse moved, which
  a double-buffered or Qt canvas never shows - so zooming worked and nothing
  showed where. It is drawn inside the chart's own drawing now. }
procedure TFitChartTest.TheRectangleIsDrawnWhileDragging;
var
    i: integer;
    Found: boolean;
    R: TRect;
begin
    Draw;
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(60), PY(30));
    Draw;
    Found := False;
    for i := 0 to FRecorder.RectCount - 1 do
    begin
        R := FRecorder.Rect(i).Rect;
        if (Abs(Min(R.Left, R.Right) - PX(20)) <= 1) and
            (Abs(Max(R.Left, R.Right) - PX(60)) <= 1) and
            (Abs(Min(R.Top, R.Bottom) - PY(80)) <= 1) and
            (Abs(Max(R.Top, R.Bottom) - PY(30)) <= 1) then
        begin
            Found := True;
            AssertTrue('with a pen every canvas draws - Qt has no XOR mode',
                FRecorder.Rect(i).PenMode <> pmXor);
        end;
    end;
    AssertTrue('the rectangle being dragged is drawn', Found);
end;

procedure TFitChartTest.TheRectangleIsGoneOnceReleased;
var
    Before: integer;
begin
    Draw;
    Before := FRecorder.RectCount;
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(60), PY(30));
    FChart.Release(PX(60), PY(30));
    Draw;
    AssertEquals('only what an idle chart draws', Before, FRecorder.RectCount);
end;

procedure TFitChartTest.DraggingUpAndLeftShowsEverythingAgain;
begin
    Draw;
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(60), PY(30));
    FChart.Release(PX(60), PY(30));
    Draw;
    FChart.Press(PX(50), PY(40));
    FChart.DragTo(PX(30), PY(70));
    FChart.Release(PX(30), PY(70));
    Draw;
    AssertEquals(0, FChart.XGraphMin, 1e-9);
    AssertEquals(100, FChart.XGraphMax, 1e-9);
end;

procedure TFitChartTest.AClickKeepsTheWindow;
var
    Left, Right: double;
begin
    Draw;
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(60), PY(30));
    FChart.Release(PX(60), PY(30));
    Draw;
    Left := FChart.XGraphMin;
    Right := FChart.XGraphMax;
    FChart.Press(PX(40), PY(50));
    FChart.Release(PX(40), PY(50));
    Draw;
    AssertEquals(Left, FChart.XGraphMin, 1e-9);
    AssertEquals(Right, FChart.XGraphMax, 1e-9);
end;

{ The window picks on a click and updates its scroll bars on a release. TAChart's
  zoom tool takes the left button for itself, and a chart tool that takes an
  event keeps it from the control's own OnMouseDown - so the window would never
  have heard of a press again, and a click would have picked nothing. }
procedure TFitChartTest.TheWindowHearsEveryPressAndReleaseEvenWhenTheZoomTakesThem;
begin
    FChart.OnPlotMouseDown := @CountDown;
    FChart.OnPlotMouseUp := @CountUp;
    Draw;
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(60), PY(30));
    FChart.Release(PX(60), PY(30));
    AssertEquals('a drag', 1, FDowns);
    AssertEquals('a drag', 1, FUps);
    FChart.Press(PX(40), PY(50));
    FChart.Release(PX(40), PY(50));
    AssertEquals('and a click', 2, FDowns);
    AssertEquals('and a click', 2, FUps);
end;

procedure TFitChartTest.ARightClickIsHeardTooAndZoomsNothing;
begin
    FChart.OnPlotMouseDown := @CountDown;
    FChart.OnPlotMouseUp := @CountUp;
    Draw;
    FChart.Press(PX(20), PY(80), mbRight);
    FChart.Release(PX(60), PY(30), mbRight);
    AssertEquals(1, FDowns);
    AssertEquals(1, FUps);
    AssertEquals(0, FChart.XGraphMin, 1e-9);
end;

{ ------------------------------ the crosshair ------------------------------- }

procedure TFitChartTest.ThePointerSnapsToTheNearestPoint;
begin
    AddSeries(Green, [10, 30, 70], [10, 10, 10]);
    FChart.ShowReticule := True;
    FChart.OnDrawReticule := @Reticule;
    Draw;
    FChart.Hover(PX(68), PY(14));
    AssertEquals('which series', 1, FReticuleSerie);
    AssertEquals('which point', 2, FReticuleIndex);
    FChart.Hover(PX(51), PY(52));
    AssertEquals(0, FReticuleSerie);
    AssertEquals(2, FReticuleIndex);
end;

procedure TFitChartTest.ItReportsThePointsOwnValues;
begin
    FChart.ShowReticule := True;
    FChart.OnDrawReticule := @Reticule;
    Draw;
    FChart.Hover(PX(26), PY(97));
    AssertEquals(25, FReticuleX, 0);
    AssertEquals(100, FReticuleY, 0);
end;

procedure TFitChartTest.AHiddenSeriesIsNotSnappedTo;
var
    Hidden: TFitSerie;
begin
    Hidden := AddSeries(Green, [70], [10]);
    Hidden.ShowLines := False;
    Hidden.ShowPoints := False;
    FChart.ShowReticule := True;
    FChart.OnDrawReticule := @Reticule;
    Draw;
    FChart.Hover(PX(70), PY(10));
    AssertEquals(0, FReticuleSerie);
end;

procedure TFitChartTest.AGapIsNotSnappedTo;
begin
    AddSeries(Green, [70, 90], [NaN, 90]);
    FChart.ShowReticule := True;
    FChart.OnDrawReticule := @Reticule;
    Draw;
    FChart.Hover(PX(70), PY(30));
    AssertFalse('not the point with no value',
        (FReticuleSerie = 1) and (FReticuleIndex = 0));
end;

procedure TFitChartTest.TheCrosshairIsDrawnThroughThePointItSnappedTo;
var
    i: integer;
    S: TRecordedStroke;
    Vertical, Horizontal: boolean;
begin
    FChart.ShowReticule := True;
    Draw;
    FChart.Hover(PX(51), PY(52));
    Draw;
    Vertical := False;
    Horizontal := False;
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.X1 = PX(50)) and (S.X2 = PX(50)) and
            (Min(S.Y1, S.Y2) <= FChart.ClipRect.Top + 1) then
            Vertical := True;
        if (S.Y1 = PY(50)) and (S.Y2 = PY(50)) and
            (Max(S.X1, S.X2) >= FChart.ClipRect.Right - 1) then
            Horizontal := True;
    end;
    AssertTrue('a vertical line through the point', Vertical);
    AssertTrue('a horizontal line through the point', Horizontal);
end;

procedure TFitChartTest.WithoutTheCrosshairNothingIsReported;
begin
    FChart.ShowReticule := False;
    FChart.OnDrawReticule := @Reticule;
    Draw;
    FChart.Hover(PX(51), PY(52));
    AssertEquals(0, FReticules);
end;

procedure TFitChartTest.DraggingIsNotFollowedByTheCrosshair;
begin
    FChart.ShowReticule := True;
    FChart.OnDrawReticule := @Reticule;
    Draw;
    FChart.Press(PX(20), PY(80));
    FChart.DragTo(PX(51), PY(52));
    AssertEquals(0, FReticules);
end;

{ -------------------------------- candles ----------------------------------- }

function TFitChartTest.AddCandles: TFitSerie;
begin
    //  Bar 20 rises 40 -> 60 between 30 and 70; bar 60 falls 60 -> 45 between
    //  40 and 65.
    Result := AddSeries(Blue, [20, 60], [60, 45]);
    Result.SetCandles([40, 60], [70, 65], [30, 40]);
end;

function WickAt(ARecorder: TRecordingCanvas; AColor: TColor; AX, AFrom, ATo: integer): boolean;
var
    i: integer;
    S: TRecordedStroke;
begin
    Result := False;
    for i := 0 to ARecorder.StrokeCount - 1 do
    begin
        S := ARecorder.Stroke(i);
        if (S.Color = AColor) and (S.X1 = AX) and (S.X2 = AX) and
            (Min(S.Y1, S.Y2) = Min(AFrom, ATo)) and
            (Max(S.Y1, S.Y2) = Max(AFrom, ATo)) then
            Exit(True);
    end;
end;

function BodyAt(ARecorder: TRecordingCanvas; AX, ATop, ABottom: integer;
    out ARect: TRecordedRect): boolean;
var
    i: integer;
    R: TRect;
begin
    Result := False;
    for i := 0 to ARecorder.RectCount - 1 do
    begin
        R := ARecorder.Rect(i).Rect;
        if (Min(R.Left, R.Right) < AX) and (Max(R.Left, R.Right) > AX) and
            (Abs(Min(R.Top, R.Bottom) - ATop) <= 1) and
            (Abs(Max(R.Top, R.Bottom) - ABottom) <= 1) then
        begin
            ARect := ARecorder.Rect(i);
            Exit(True);
        end;
    end;
end;

procedure TFitChartTest.ACandleIsAWickFromLowToHighAndABodyFromOpenToClose;
var
    R: TRecordedRect;
begin
    AddCandles;
    Draw;
    AssertTrue('the wick of the rising bar',
        WickAt(FRecorder, Blue, PX(20), PY(30), PY(70)));
    AssertTrue('its body, open to close', BodyAt(FRecorder, PX(20), PY(60), PY(40), R));
    AssertTrue('the wick of the falling bar',
        WickAt(FRecorder, Blue, PX(60), PY(40), PY(65)));
    AssertTrue('its body', BodyAt(FRecorder, PX(60), PY(60), PY(45), R));
end;

{ The convention a black-and-white chart has always used, so a candle reads the
  same whatever colour the series is: an open body rose, a filled one fell. }
procedure TFitChartTest.ARisingCandleIsHollowAndAFallingOneFilled;
var
    Rising, Falling: TRecordedRect;
begin
    AddCandles;
    Draw;
    AssertTrue(BodyAt(FRecorder, PX(20), PY(60), PY(40), Rising));
    AssertTrue(BodyAt(FRecorder, PX(60), PY(60), PY(45), Falling));
    AssertTrue('rising: hollow', Rising.BrushStyle = bsClear);
    AssertTrue('falling: filled', Falling.BrushStyle = bsSolid);
    AssertEquals('in the series colour', Blue, Falling.BrushColor);
end;

procedure TFitChartTest.CandlesJoinNothing;
var
    i: integer;
    S: TRecordedStroke;
begin
    AddCandles;
    Draw;
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if S.Color = Blue then
            AssertTrue('only vertical strokes: no line from bar to bar',
                S.X1 = S.X2);
    end;
end;

procedure TFitChartTest.TheCandlesHighsAndLowsAreInTheExtents;
var
    S: TFitSerie;
begin
    S := AddCandles;
    AssertEquals('the lowest low', 30, S.Extent.a.Y, 0);
    AssertEquals('the highest high', 70, S.Extent.b.Y, 0);
end;

procedure TFitChartTest.CandlesTakenAwayLeaveALineAgain;
var
    S: TFitSerie;
begin
    S := AddCandles;
    S.SetCandles([], [], []);
    AssertFalse(S.IsCandles);
    Draw;
    AssertTrue('joined again', DiagonalStrokesIn(Blue) > 0);
end;

{ ---------------------------- dragging a point ------------------------------ }

procedure TFitChartTest.Dropped(Sender: TObject; ASerie, AIndex: integer;
    AX, AY: double);
begin
    Inc(FDrops);
    FDropSerie := ASerie;
    FDropIndex := AIndex;
    FDropX := AX;
    FDropY := AY;
end;

{ Two picks, at 30 and 70, of a set the chart may move. }
function TFitChartTest.AddMovable(const AKind: string): TFitSerie;
begin
    Result := AddSeries(Green, [30, 70], [40, 60]);
    Result.ShowLines := False;
    Result.ShowPoints := True;
    Result.Marker := smCircle;
    Result.DragKind := AKind;
    FChart.OnPointDragged := @Dropped;
end;

procedure TFitChartTest.DroppingADraggedPointSaysWhereItWent;
var
    Tolerance: double;
begin
    AddMovable('curve-positions');
    Draw;
    Tolerance := 2 * 100 / (FChart.ClipRect.Right - FChart.ClipRect.Left);
    FChart.Press(PX(70), PY(60));
    FChart.DragTo(PX(80), PY(55));
    FChart.DragTo(PX(85), PY(50));
    FChart.Release(PX(85), PY(50));
    AssertEquals('one drop', 1, FDrops);
    AssertEquals('of that series', 1, FDropSerie);
    AssertEquals('that point', 1, FDropIndex);
    AssertEquals('where it was let go', 85, FDropX, Tolerance);
    AssertEquals(50, FDropY, 2 * Tolerance);
end;

{ The model decides whether a point moves - a move can be refused - and a
  refused move must not leave the point where it was dropped. So the chart puts
  it back, and the model's own replot is what shows it moved. }
procedure TFitChartTest.TheChartDoesNotMoveThePointItself;
var
    S: TFitSerie;
begin
    S := AddMovable('curve-positions');
    Draw;
    FChart.Press(PX(70), PY(60));
    FChart.DragTo(PX(85), PY(50));
    FChart.Release(PX(85), PY(50));
    AssertEquals(70, S.XValue[1], 0);
    AssertEquals(60, S.YValue[1], 0);
end;

procedure TFitChartTest.APressAwayFromAnyDraggablePointStillZooms;
begin
    AddMovable('curve-positions');
    Draw;
    FChart.Press(PX(5), PY(95));
    FChart.DragTo(PX(25), PY(80));
    FChart.Release(PX(25), PY(80));
    AssertEquals('no drop', 0, FDrops);
    AssertTrue('a zoom', FChart.IsZoomed);
end;

procedure TFitChartTest.APointOfASeriesThatIsNotDraggableIsNotGrabbed;
begin
    AddMovable('');
    Draw;
    FChart.Press(PX(70), PY(60));
    FChart.DragTo(PX(85), PY(50));
    FChart.Release(PX(85), PY(50));
    AssertEquals(0, FDrops);
end;

{ A fit-interval bound is a line through the plot, and is taken wherever along
  it the pointer is - its y is no value of the data. }
procedure TFitChartTest.ABoundIsGrabbedAnywhereAlongItsLine;
var
    Band: TFitSerie;
begin
    Band := AddBand(smVertLineBT, 20, 60);
    Band.DragKind := 'fit-bounds';
    FChart.OnPointDragged := @Dropped;
    Draw;
    FChart.Press(PX(60), PY(95));
    FChart.DragTo(PX(75), PY(95));
    FChart.Release(PX(75), PY(95));
    AssertEquals(1, FDrops);
    AssertEquals('the second bound', 1, FDropIndex);
end;

{ A click on a pick is how a pick is taken back; it must stay a click. }
procedure TFitChartTest.AClickOnADraggablePointIsNotADrop;
begin
    AddMovable('curve-positions');
    FChart.OnPlotMouseDown := @CountDown;
    FChart.OnPlotMouseUp := @CountUp;
    Draw;
    FChart.Press(PX(70), PY(60));
    FChart.Release(PX(70), PY(60));
    AssertEquals('no drop', 0, FDrops);
    AssertEquals('but the window heard the click', 1, FUps);
    AssertEquals(1, FDowns);
end;

procedure TFitChartTest.WhileDraggingALineShowsWhereItWillGo;
var
    i: integer;
    S: TRecordedStroke;
    Seen: boolean;
begin
    AddMovable('curve-positions');
    Draw;
    FChart.Press(PX(70), PY(60));
    FChart.DragTo(PX(85), PY(50));
    Draw;
    Seen := False;
    for i := 0 to FRecorder.StrokeCount - 1 do
    begin
        S := FRecorder.Stroke(i);
        if (S.Color = Green) and (S.X1 = PX(85)) and (S.X2 = PX(85)) and
            (Min(S.Y1, S.Y2) <= FChart.ClipRect.Top) then
            Seen := True;
    end;
    AssertTrue('a line where the point would go', Seen);
end;

procedure TFitChartTest.AHiddenSeriesIsNotGrabbed;
var
    S: TFitSerie;
begin
    S := AddMovable('curve-positions');
    S.ShowPoints := False;
    Draw;
    FChart.Press(PX(70), PY(60));
    FChart.DragTo(PX(85), PY(50));
    FChart.Release(PX(85), PY(50));
    AssertEquals(0, FDrops);
end;

{ ------------------------------- axis marks --------------------------------- }

procedure TFitChartTest.TheXMarksAreWhereTheAxisPutsThem;
begin
    FChart.OnXMarks := @MarksEvery25;
    FChart.OnXMarkText := @MarkTextX;
    Draw;
    AssertTrue(FRecorder.Wrote('x0'));
    AssertTrue(FRecorder.Wrote('x25'));
    AssertTrue(FRecorder.Wrote('x100'));
    AssertFalse(FRecorder.Wrote('x20'));
end;

procedure TFitChartTest.TheYMarksAreWhereTheAxisPutsThem;
begin
    FChart.OnYMarks := @MarksEvery25;
    FChart.OnYMarkText := @MarkTextY;
    Draw;
    AssertTrue(FRecorder.Wrote('y50'));
    AssertTrue(FRecorder.Wrote('y75'));
end;

procedure TFitChartTest.MarksTheAxisDeclinesToPlaceAreTheChartsAndStillItsText;
var
    i, Written: integer;
begin
    FChart.OnXMarks := @MarksLeftToTheChart;
    FChart.OnXMarkText := @MarkTextX;
    Draw;
    Written := 0;
    for i := 0 to FRecorder.TextCount - 1 do
        if Copy(FRecorder.Text(i).Text, 1, 1) = 'x' then
            Inc(Written);
    AssertTrue(Written >= 2);
end;

procedure TFitChartTest.WithoutAnEventTheMarksAreNumbers;
begin
    Draw;
    AssertTrue(FRecorder.Wrote('0'));
    AssertTrue(FRecorder.Wrote('100'));
end;

{ A GRID EVERY FEW PIXELS IS NOISE. Every mark draws a grid line, and an axis
  asked for a hundred marks - one a unit over a hundred - drew a hundred of
  them. No more than ten are drawn: every second, fifth, tenth, twentieth... of
  the ones asked for, whichever first brings them within ten. }
procedure TFitChartTest.AnAxisMarkedFinelyDrawsNoMoreThanTenLines;
begin
    FChart.OnXMarks := @MarksEvery1;
    FChart.OnXMarkText := @MarkTextX;
    Draw;
    AssertTrue('no more than ten: ' + IntToStr(XMarksWritten),
        XMarksWritten <= 10);
    AssertTrue('and not none', XMarksWritten >= 2);
    AssertTrue('every twentieth', FRecorder.Wrote('x20'));
    AssertFalse(FRecorder.Wrote('x7'));
end;

procedure TFitChartTest.TheChartsOwnMarksAreNoMoreThanTenOnAWideChart;
begin
    FChart.OnXMarks := @MarksLeftToTheChart;
    FChart.OnXMarkText := @MarkTextX;
    DrawWide;
    AssertTrue('no more than ten: ' + IntToStr(XMarksWritten),
        XMarksWritten <= 10);
    AssertTrue('and not none', XMarksWritten >= 2);
end;

{ Kept by their value, not by their place in the window: scrolled a little,
  the grid moves with the data rather than jumping to other values. }
procedure TFitChartTest.TheMarksKeptStayPutWhenTheWindowMoves;
begin
    FChart.OnXMarks := @MarksEvery1;
    FChart.OnXMarkText := @MarkTextX;
    AddSeries(Green, [0, 200], [0, 100]);
    FChart.XGraphMax := 103;
    FChart.XGraphMin := 3;
    Draw;
    AssertTrue(FRecorder.Wrote('x20'));
    AssertFalse('not counted from the window''s edge', FRecorder.Wrote('x3'));
    AssertFalse(FRecorder.Wrote('x23'));
end;

procedure TFitChartTest.MarksAlreadyFewAreAllKept;
begin
    FChart.OnXMarks := @MarksEvery25;
    FChart.OnXMarkText := @MarkTextX;
    Draw;
    AssertTrue(FRecorder.Wrote('x25'));
    AssertTrue(FRecorder.Wrote('x50'));
    AssertTrue(FRecorder.Wrote('x75'));
end;

{ ------------------------------ the window ---------------------------------- }

procedure TFitChartTest.UnzoomedTheWindowIsTheWholeData;
begin
    AddSeries(Green, [-20, 140], [-5, 130]);
    Draw;
    AssertEquals(-20, FChart.XGraphMin, 1e-9);
    AssertEquals(140, FChart.XGraphMax, 1e-9);
    AssertEquals(-5, FChart.YGraphMin, 1e-9);
    AssertEquals(130, FChart.YGraphMax, 1e-9);
end;

procedure TFitChartTest.ASetWindowIsTheWindowShown;
begin
    Draw;
    FChart.XGraphMin := 20;
    FChart.XGraphMax := 40;
    FChart.YGraphMin := 10;
    FChart.YGraphMax := 30;
    Draw;
    AssertEquals(20, FChart.XGraphMin, 1e-9);
    AssertEquals(40, FChart.XGraphMax, 1e-9);
    AssertEquals(10, FChart.YGraphMin, 1e-9);
    AssertEquals(30, FChart.YGraphMax, 1e-9);
end;

procedure TFitChartTest.ZoomInTakesATenthOffEachSide;
begin
    Draw;
    FChart.ZoomIn;
    AssertEquals(10, FChart.XGraphMin, 1e-9);
    AssertEquals(90, FChart.XGraphMax, 1e-9);
    AssertEquals(10, FChart.YGraphMin, 1e-9);
    AssertEquals(90, FChart.YGraphMax, 1e-9);
end;

procedure TFitChartTest.ZoomOutAddsATenthToEachSide;
begin
    Draw;
    FChart.ZoomOut;
    AssertEquals(-10, FChart.XGraphMin, 1e-9);
    AssertEquals(110, FChart.XGraphMax, 1e-9);
end;

procedure TFitChartTest.ZoomingAnEmptyChartChangesNothing;
begin
    FreeAndNil(FChart);
    FChart := TMouseChart.Create(nil);
    FChart.ZoomIn;
    FChart.ZoomOut;
    AssertFalse(FChart.IsZoomed);
end;

{ The scroll bars measure the window against the whole data. They kept the whole
  range in fields of their own, captured at the moment data was loaded; a window
  restored from a project is set before that moment, so they would have taken
  the window for the whole. }
procedure TFitChartTest.TheFullRangeIsTheWholeDataEvenWhenZoomed;
begin
    Draw;
    FChart.XGraphMin := 20;
    FChart.XGraphMax := 40;
    AssertEquals(0, FChart.FullXGraphMin, 1e-9);
    AssertEquals(100, FChart.FullXGraphMax, 1e-9);
    AssertEquals(0, FChart.FullYGraphMin, 1e-9);
    AssertEquals(100, FChart.FullYGraphMax, 1e-9);
end;

{ ------------------------------ axes and series ----------------------------- }

procedure TFitChartTest.TheAxisTitlesAreWrittenWhenShown;
begin
    FChart.XAxisLabel := 'Position';
    FChart.YAxisLabel := 'Amplitude';
    FChart.ShowAxisLabel := True;
    Draw;
    AssertTrue(FRecorder.Wrote('Position'));
    AssertTrue(FRecorder.Wrote('Amplitude'));
    AssertEquals('Position', FChart.XAxisLabel);
    AssertEquals('Amplitude', FChart.YAxisLabel);
end;

procedure TFitChartTest.TheAxisTitlesAreNotWrittenWhenHidden;
begin
    FChart.XAxisLabel := 'Position';
    FChart.ShowAxisLabel := False;
    Draw;
    AssertFalse(FRecorder.Wrote('Position'));
    AssertFalse(FChart.ShowAxisLabel);
end;

procedure TFitChartTest.TheAxisColourIsTheColourOfTheMarks;
var
    i: integer;
    Seen: boolean;
begin
    FChart.AxisColor := clMaroon;
    Draw;
    Seen := False;
    for i := 0 to FRecorder.TextCount - 1 do
        if FRecorder.Text(i).Text = '100' then
        begin
            Seen := True;
            AssertEquals(clMaroon, FRecorder.Text(i).Color);
        end;
    AssertTrue(Seen);
    AssertEquals(clMaroon, FChart.AxisColor);
end;

procedure TFitChartTest.EachMarkerIsTheShapeItNames;
var
    S: TFitSerie;
begin
    S := TFitSerie.Create(nil);
    try
        S.Marker := smCircle;
        AssertTrue(S.Pointer.Style = psCircle);
        S.Marker := smDiagCross;
        AssertTrue(S.Pointer.Style = psDiagCross);
        S.Marker := smRectangle;
        AssertTrue(S.Pointer.Style = psRectangle);
        AssertFalse(S.IsBackgroundBand);
        AssertTrue(S.Marker = smRectangle);
    finally
        S.Free;
    end;
end;

procedure TFitChartTest.TheTwoBandMarkersAreBands;
var
    S: TFitSerie;
begin
    S := TFitSerie.Create(nil);
    try
        S.Marker := smVertLineTB;
        AssertTrue(S.IsBackgroundBand);
        S.Marker := smVertLineBT;
        AssertTrue(S.IsBackgroundBand);
    finally
        S.Free;
    end;
end;

procedure TFitChartTest.TheMarkerSizeIsScaledLikeEverythingElse;
var
    S: TFitSerie;
begin
    S := AddSeries(Green, [10], [10]);
    S.ImageSize := 4;
    AssertEquals(4, S.ImageSize);
    AssertEquals(FChart.Sc(4), S.Pointer.HorizSize);
    AssertEquals(FChart.Sc(4), S.Pointer.VertSize);
end;

procedure TFitChartTest.AHollowMarkerIsNotFilled;
var
    S: TFitSerie;
begin
    S := TFitSerie.Create(nil);
    try
        S.Hollow := True;
        AssertTrue(S.Pointer.Brush.Style = bsClear);
        AssertTrue(S.Hollow);
    finally
        S.Free;
    end;
end;

procedure TFitChartTest.TheSeriesColourIsTheLineTheMarkersAndTheCaptions;
var
    S: TFitSerie;
begin
    S := TFitSerie.Create(nil);
    try
        S.SeriesColor := clTeal;
        AssertEquals(clTeal, S.SeriesColor);
        AssertEquals(clTeal, S.LinePen.Color);
        AssertEquals(clTeal, S.Pointer.Pen.Color);
        AssertEquals(clTeal, S.Pointer.Brush.Color);
        AssertEquals(clTeal, S.Marks.LabelFont.Color);
    finally
        S.Free;
    end;
end;

{ Kept apart from ShowLines and ShowPoints, which the legend switches: the
  "View markers" toggle asks how a series was MEANT to look. }
procedure TFitChartTest.TheInitialLookIsRemembered;
var
    S: TFitSerie;
begin
    S := TFitSerie.Create(nil);
    try
        S.InitShowLines := True;
        S.InitShowPoints := False;
        S.ShowLines := False;
        S.ShowPoints := True;
        AssertTrue(S.InitShowLines);
        AssertFalse(S.InitShowPoints);
    finally
        S.Free;
    end;
end;

{ However low the resolution - the headless one these tests run at is below 96
  dpi - a one-pixel pen stays a pen. }
procedure TFitChartTest.ScalingNeverRoundsAPenToNothing;
begin
    AssertEquals(0, FChart.Sc(0));
    AssertTrue(FChart.Sc(1) >= 1);
    AssertTrue(FChart.Sc(16) >= FChart.Sc(1));
end;

initialization
    RegisterTest('unit', TFitChartTest);
end.
