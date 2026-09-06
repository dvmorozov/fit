// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Scrolling a zoomed chart: where the thumb belongs, and which part of
the data a thumb position asks for.)

TWO DIRECTIONS THAT HAVE TO AGREE. The user drags a scroll bar, the chart moves
to the window that position asks for, and the chart then computes where the
thumb belongs for the window it now shows. Those are two separate formulas, and
if they are not exact inverses the thumb slides out from under the pointer on
every drag - which reads as a chart fighting the mouse rather than as arithmetic
disagreeing with itself. THE ROUND TRIP IS THE POINT OF THIS FILE.

IT USED TO BE FOUR COPIES, two per axis, inside LCL event handlers, each with a
commented-out alternative left over from the time somebody got the direction
wrong and swapped the ends by trial. Nothing could state which way round either
bar went.

THE VERTICAL BAR IS GENUINELY UPSIDE DOWN. A scroll bar's minimum is at the top
of the screen; the chart's maximum is. So the thumb at the vertical minimum
means the window at the TOP of the data, where the horizontal thumb at its
minimum means the window at the LEFT. That one difference is a flag here, not a
second copy of the formula - which is what stops the two drifting apart while
each still looks right on its own.

AND THE DEGENERATE CASES MATTER MORE THAN THEY LOOK. An unzoomed chart has
nothing off-screen, so every thumb position means the same window and the
formula divides by zero. An infinity written back as the chart's extent is a
chart that draws nothing at all, from a scroll of a chart that was fine.
}
unit testcase_chart_panning;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    chart_panning;

type
    TChartPanningTest = class(TTestCase)
    private
        { Data over 0..100, showing a window of 20 starting at AAt, with a bar
          of 0..1000. The one arrangement every test below varies. }
        function WindowAt(AAt: double): TAxisWindow;
        function Bar(APosition: longint): TBarRange;
    published
        //  How much there is to scroll.
        procedure AZoomedChartHasSomethingHidden;
        procedure AChartShowingEverythingHasNothingHidden;
        procedure AChartShowingEverythingCannotBePanned;
        procedure ABarWithNoRangeCannotPanEither;
        procedure AWindowWiderThanTheDataCannotBePanned;

        //  Where the thumb belongs.
        procedure AtTheStartTheThumbIsAtTheMinimum;
        procedure AtTheEndTheThumbIsAtTheMaximum;
        procedure HalfwayThroughTheThumbIsHalfway;
        procedure TheVerticalBarRunsTheOtherWay;
        procedure AThumbNeverLeavesItsOwnRange;
        procedure WithNothingToScrollTheThumbIsLeftWhereItIs;

        //  Which window a position asks for.
        procedure TheMinimumAsksForTheStartOfTheData;
        procedure TheMaximumAsksForTheEndOfTheData;
        procedure ThePannedWindowKeepsItsWidth;
        procedure TheVerticalBarAsksTheOtherWayRound;
        procedure WithNothingToScrollTheWindowIsLeftAlone;

        //  The two directions agree - the reason this unit exists.
        procedure AThumbPositionSurvivesTheRoundTrip;
        procedure AWindowSurvivesTheRoundTrip;
        procedure AndSoDoesTheVerticalOne;
        procedure TheRoundTripHoldsAcrossTheWholeTravel;
    end;

implementation

const
    //  The bar the form actually uses is finer than this; a thousand steps is
    //  enough to show rounding without hiding it.
    BarMin = 0;
    BarMax = 1000;

function TChartPanningTest.WindowAt(AAt: double): TAxisWindow;
begin
    Result := AxisWindow(0, 100, AAt, AAt + 20);
end;

function TChartPanningTest.Bar(APosition: longint): TBarRange;
begin
    Result := BarRange(BarMin, BarMax, APosition);
end;

{ ---- how much there is to scroll ------------------------------------------- }

procedure TChartPanningTest.AZoomedChartHasSomethingHidden;
begin
    //  Eighty of the hundred units are off-screen, and that span is the travel
    //  the whole length of the bar stands for.
    AssertEquals('eighty hidden', 80.0, HiddenSpan(WindowAt(0)), 1E-9);
end;

procedure TChartPanningTest.AChartShowingEverythingHasNothingHidden;
begin
    AssertEquals('nothing hidden', 0.0,
        HiddenSpan(AxisWindow(0, 100, 0, 100)), 1E-9);
end;

procedure TChartPanningTest.AChartShowingEverythingCannotBePanned;
begin
    //  THE DIVISION BY ZERO, guarded at its source. Every thumb position means
    //  the same window, so the fraction is 0/0; an infinity written back as the
    //  chart's extent is a chart that draws nothing, reached by scrolling one
    //  that was perfectly fine.
    AssertFalse('nothing to scroll',
        CanPan(AxisWindow(0, 100, 0, 100), Bar(0)));
end;

procedure TChartPanningTest.ABarWithNoRangeCannotPanEither;
begin
    //  The same division from the other side: a bar whose ends coincide cannot
    //  express a position, and dividing by its length divides by zero.
    AssertFalse('no range', CanPan(WindowAt(0), BarRange(5, 5, 5)));
end;

procedure TChartPanningTest.AWindowWiderThanTheDataCannotBePanned;
begin
    //  What a zoom out past the extent leaves behind. There is less than
    //  nothing off-screen, so scrolling has no meaning - and the fraction would
    //  come out negative, putting the thumb outside its bar.
    AssertFalse('over-zoomed',
        CanPan(AxisWindow(0, 100, -50, 150), Bar(0)));
end;

{ ---- where the thumb belongs ----------------------------------------------- }

procedure TChartPanningTest.AtTheStartTheThumbIsAtTheMinimum;
begin
    AssertEquals('hard left', BarMin,
        BarPositionForWindow(WindowAt(0), Bar(500), False));
end;

procedure TChartPanningTest.AtTheEndTheThumbIsAtTheMaximum;
begin
    //  The window's right edge against the data's: 80..100 of 0..100.
    AssertEquals('hard right', BarMax,
        BarPositionForWindow(WindowAt(80), Bar(500), False));
end;

procedure TChartPanningTest.HalfwayThroughTheThumbIsHalfway;
begin
    //  Halfway through the TRAVEL, which is 80 units, not halfway through the
    //  data - a thumb placed by the window's position in the data instead would
    //  never reach either end.
    AssertEquals('mid travel', 500,
        BarPositionForWindow(WindowAt(40), Bar(0), False));
end;

procedure TChartPanningTest.TheVerticalBarRunsTheOtherWay;
begin
    //  UPSIDE DOWN, and deliberately: a scroll bar's minimum is at the top of
    //  the screen while the chart's maximum is. A vertical bar wired like a
    //  horizontal one scrolls the chart the wrong way, which is the single most
    //  noticeable thing a scroll bar can do wrong.
    AssertEquals('the bottom of the data is the bar''s maximum', BarMax,
        BarPositionForWindow(WindowAt(0), Bar(0), True));
    AssertEquals('and the top of it is the minimum', BarMin,
        BarPositionForWindow(WindowAt(80), Bar(0), True));
end;

procedure TChartPanningTest.AThumbNeverLeavesItsOwnRange;
var
    At: double;
    Position: longint;
begin
    //  A window can sit a little outside the data after a zoom centred near an
    //  edge. The widget pins an out-of-range position to an end anyway, so the
    //  position handed back has to be the one that will actually be shown -
    //  otherwise the next round trip reads back a thumb nobody put there.
    At := -30;
    while At <= 130 do
    begin
        Position := BarPositionForWindow(WindowAt(At), Bar(0), False);
        AssertTrue(Format('at %g: not below the bar', [At]),
            Position >= BarMin);
        AssertTrue(Format('at %g: not above it', [At]), Position <= BarMax);
        At := At + 10;
    end;
end;

procedure TChartPanningTest.WithNothingToScrollTheThumbIsLeftWhereItIs;
begin
    //  NOT MOVED TO AN END. Zooming out to the full extent would otherwise
    //  yank the thumb across the bar, which looks like the chart scrolled when
    //  it did not.
    AssertEquals('left alone', 371,
        BarPositionForWindow(AxisWindow(0, 100, 0, 100), Bar(371), False));
end;

{ ---- which window a position asks for -------------------------------------- }

procedure TChartPanningTest.TheMinimumAsksForTheStartOfTheData;
var
    W: TAxisWindow;
begin
    W := WindowForBarPosition(WindowAt(40), Bar(BarMin), False);
    AssertEquals('against the left edge', 0.0, W.ViewMin, 1E-9);
    AssertEquals('and twenty wide', 20.0, W.ViewMax, 1E-9);
end;

procedure TChartPanningTest.TheMaximumAsksForTheEndOfTheData;
var
    W: TAxisWindow;
begin
    W := WindowForBarPosition(WindowAt(40), Bar(BarMax), False);
    AssertEquals('against the right edge', 80.0, W.ViewMin, 1E-9);
    AssertEquals('ending at the data''s end', 100.0, W.ViewMax, 1E-9);
end;

procedure TChartPanningTest.ThePannedWindowKeepsItsWidth;
var
    At: longint;
    W: TAxisWindow;
begin
    //  SCROLLING MOVES THE WINDOW AND NEVER RESIZES IT. A pan that also zoomed
    //  would change the scale under the user while they were dragging, and the
    //  thumb would then belong somewhere else again - the two directions would
    //  chase each other.
    for At := 0 to 10 do
    begin
        W := WindowForBarPosition(WindowAt(40), Bar(At * 100), False);
        AssertEquals(Format('at %d: still twenty wide', [At * 100]),
            20.0, W.ViewMax - W.ViewMin, 1E-9);
    end;
end;

procedure TChartPanningTest.TheVerticalBarAsksTheOtherWayRound;
var
    W: TAxisWindow;
begin
    W := WindowForBarPosition(WindowAt(40), Bar(BarMin), True);
    AssertEquals('the minimum means the top of the data', 80.0,
        W.ViewMin, 1E-9);
    W := WindowForBarPosition(WindowAt(40), Bar(BarMax), True);
    AssertEquals('and the maximum the bottom', 0.0, W.ViewMin, 1E-9);
end;

procedure TChartPanningTest.WithNothingToScrollTheWindowIsLeftAlone;
var
    W: TAxisWindow;
begin
    W := WindowForBarPosition(AxisWindow(0, 100, 0, 100), Bar(BarMax), False);
    AssertEquals('unchanged', 0.0, W.ViewMin, 1E-9);
    AssertEquals('at both ends', 100.0, W.ViewMax, 1E-9);
end;

{ ---- the two directions agree ---------------------------------------------- }

procedure TChartPanningTest.AThumbPositionSurvivesTheRoundTrip;
var
    W: TAxisWindow;
begin
    //  DRAG, THEN REDRAW. The chart moves to the window a thumb position asks
    //  for and then puts the thumb where it computes it belongs. If that is not
    //  the position the user dragged it to, the thumb slides out from under the
    //  pointer.
    W := WindowForBarPosition(WindowAt(40), Bar(250), False);
    AssertEquals('back where the user put it', 250,
        BarPositionForWindow(W, Bar(250), False));
end;

procedure TChartPanningTest.AWindowSurvivesTheRoundTrip;
var
    W: TAxisWindow;
begin
    //  The other direction: the chart is redrawn, the thumb is placed, and
    //  nothing about the window may change as a result.
    W := WindowForBarPosition(WindowAt(37.5),
        Bar(BarPositionForWindow(WindowAt(37.5), Bar(0), False)), False);
    AssertEquals('the window did not move', 37.5, W.ViewMin, 0.1);
end;

procedure TChartPanningTest.AndSoDoesTheVerticalOne;
var
    W: TAxisWindow;
begin
    //  Asserted separately rather than assumed from the horizontal case: the
    //  inversion is exactly where a sign gets dropped, and a bar that inverts
    //  in one direction only round-trips to the opposite end of the data.
    W := WindowForBarPosition(WindowAt(40), Bar(250), True);
    AssertEquals('back where the user put it', 250,
        BarPositionForWindow(W, Bar(250), True));
end;

procedure TChartPanningTest.TheRoundTripHoldsAcrossTheWholeTravel;
var
    At, Back: longint;
begin
    //  ACROSS THE WHOLE BAR, because a formula that is right at the ends and
    //  wrong in the middle is exactly what a wrong sign produces - and the ends
    //  are what anybody checks by hand.
    for At := 0 to 20 do
    begin
        Back := BarPositionForWindow(
            WindowForBarPosition(WindowAt(40), Bar(At * 50), False),
            Bar(At * 50), False);
        AssertEquals(Format('horizontal at %d', [At * 50]), At * 50, Back);

        Back := BarPositionForWindow(
            WindowForBarPosition(WindowAt(40), Bar(At * 50), True),
            Bar(At * 50), True);
        AssertEquals(Format('vertical at %d', [At * 50]), At * 50, Back);
    end;
end;

initialization
    //  A unit test: two records of numbers. No chart and no scroll bar.
    RegisterTest('unit', TChartPanningTest);
end.
