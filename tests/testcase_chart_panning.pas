// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The view a chart shows along one axis: zoomed, kept inside the
data, and drawn on a scroll bar whose thumb is the share on show.)
}
unit testcase_chart_panning;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    chart_panning;

type
    { THE VIEW SHARED BY THE CHARTS ON THE MODEL'S X, and the scroll bars that
      show it (the user, 2026-10-14: "zooming and scrolling of both charts
      should work consistently"). A view never leaves the data: zooming out
      stops at the whole of it, and a thumb is as long as the share on show. }
    TChartViewTest = class(TTestCase)
    private
        { Data over 0..100, showing ALo..AHi. }
        function View(ALo, AHi: double): TAxisWindow;
        procedure AssertView(const AWhat: string; ALo, AHi: double;
            const AView: TAxisWindow);
    published
        procedure TheWholeIsWholeAndAZoomIsNot;
        procedure ZoomingInKeepsTheCentre;
        procedure ZoomingOutStopsAtTheWholeData;
        procedure AZoomOutNearAnEdgeStaysInsideTheData;
        procedure AZoomedRangeIsOrderedAndKeptInsideTheData;
        procedure ARangeWithNoWidthChangesNothing;
        procedure NewDataKeepsAWholeViewWholeAndAZoomInside;
        procedure TheThumbIsAsLongAsTheShareOnShow;
        procedure AWholeViewHasNothingToScroll;
        procedure TheThumbAtTheEndShowsTheEnd;
        procedure TheVerticalThumbIsUpsideDown;
        procedure EveryThumbPositionSurvivesTheRoundTrip;
    end;

implementation

function TChartViewTest.View(ALo, AHi: double): TAxisWindow;
begin
    Result.FullMin := 0;
    Result.FullMax := 100;
    Result.ViewMin := ALo;
    Result.ViewMax := AHi;
end;

procedure TChartViewTest.AssertView(const AWhat: string; ALo, AHi: double;
    const AView: TAxisWindow);
begin
    AssertEquals(AWhat + ': from', ALo, AView.ViewMin, 1e-9);
    AssertEquals(AWhat + ': to', AHi, AView.ViewMax, 1e-9);
end;

procedure TChartViewTest.TheWholeIsWholeAndAZoomIsNot;
begin
    AssertTrue('whole', WindowIsWhole(View(0, 100)));
    AssertFalse('zoomed', WindowIsWhole(View(40, 60)));
    AssertTrue('made whole', WindowIsWhole(WholeWindow(View(40, 60))));
end;

procedure TChartViewTest.ZoomingInKeepsTheCentre;
begin
    AssertView('a fifth in', 42, 58, WindowScaled(View(40, 60), 0.8));
end;

procedure TChartViewTest.ZoomingOutStopsAtTheWholeData;
var
    V: TAxisWindow;
    i: longint;
begin
    V := View(40, 60);
    for i := 1 to 30 do
    begin
        V := WindowScaled(V, 1.25);
        AssertTrue('never past the data', (V.ViewMin >= V.FullMin - 1e-9) and
            (V.ViewMax <= V.FullMax + 1e-9));
    end;
    AssertTrue('ends whole', WindowIsWhole(V));
end;

procedure TChartViewTest.AZoomOutNearAnEdgeStaysInsideTheData;
begin
    AssertView('moved in, as wide as asked', 0, 25,
        WindowScaled(View(0, 20), 1.25));
end;

procedure TChartViewTest.AZoomedRangeIsOrderedAndKeptInsideTheData;
begin
    AssertView('ordered', 40, 60, WindowZoomedTo(View(0, 100), 60, 40));
    AssertView('cut at the start', 0, 30, WindowZoomedTo(View(0, 100), -10, 30));
    AssertTrue('past both ends is the whole',
        WindowIsWhole(WindowZoomedTo(View(40, 60), -10, 130)));
end;

procedure TChartViewTest.ARangeWithNoWidthChangesNothing;
begin
    AssertView('a click is not a zoom', 40, 60,
        WindowZoomedTo(View(40, 60), 50, 50));
end;

procedure TChartViewTest.NewDataKeepsAWholeViewWholeAndAZoomInside;
begin
    AssertView('whole stays whole', 0, 200,
        WindowWithFull(View(0, 100), 0, 200));
    AssertView('a zoom inside is kept', 40, 60,
        WindowWithFull(View(40, 60), 0, 200));
    AssertView('a zoom past the new end is moved in', 30, 50,
        WindowWithFull(View(40, 60), 0, 50));
end;

procedure TChartViewTest.TheThumbIsAsLongAsTheShareOnShow;
var
    T: TBarThumb;
begin
    T := ThumbForWindow(View(40, 60), False);
    AssertEquals('a fifth of the track', BAR_STEPS div 5, T.PageSize);
    AssertEquals('two fifths along', 2 * BAR_STEPS div 5, T.Position);
    AssertEquals('the track', BAR_STEPS - 1, T.Max);
    AssertTrue('something to scroll', T.Enabled);
end;

procedure TChartViewTest.AWholeViewHasNothingToScroll;
var
    T: TBarThumb;
begin
    T := ThumbForWindow(View(0, 100), False);
    AssertEquals('the whole track', BAR_STEPS, T.PageSize);
    AssertEquals('at the start', 0, T.Position);
    AssertFalse('nothing to scroll', T.Enabled);
end;

procedure TChartViewTest.TheThumbAtTheEndShowsTheEnd;
begin
    //  The farthest a thumb goes is the track less its own length.
    AssertView('the last fifth', 80, 100,
        WindowForThumb(View(40, 60), BAR_STEPS - BAR_STEPS div 5, False));
    AssertView('the first fifth', 0, 20, WindowForThumb(View(40, 60), 0, False));
end;

procedure TChartViewTest.TheVerticalThumbIsUpsideDown;
begin
    AssertEquals('the top of the data at the top', 0,
        ThumbForWindow(View(80, 100), True).Position);
    AssertView('the top asks for the top', 80, 100,
        WindowForThumb(View(40, 60), 0, True));
end;

procedure TChartViewTest.EveryThumbPositionSurvivesTheRoundTrip;
var
    p: longint;
    Inverted: boolean;
begin
    for Inverted := False to True do
    begin
        p := 0;
        while p <= BAR_STEPS - BAR_STEPS div 5 do
        begin
            AssertEquals(Format('position %d', [p]), p, ThumbForWindow(
                WindowForThumb(View(40, 60), p, Inverted), Inverted).Position);
            Inc(p, 37);
        end;
    end;
end;

initialization
    //  A unit test: two records of numbers. No chart and no scroll bar.
    RegisterTest('unit', TChartViewTest);
end.
