// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where each heading and button of the Tools pane sits.)

WHY THESE TESTS EXIST. The pane is generated from a table a module can add rows
to, so how many rows and how many groups there are is unknown until run time.
The two ways that goes wrong are a button drawn on top of another and a row
placed below the visible area, and both look to the user like the pane simply
not offering that command - there is nothing to see and nothing in a log.

The case that would cost the most is a group that ends mid-row, leaving the
next heading to be drawn over it - inline arithmetic in a method that needs a
window, until this unit existed.

AND THE PANE'S WIDTH IS ASKED THE OTHER WAY ROUND. It was a constant, and the
button width was whatever two columns of it left; the caller measures its widest
caption now and asks how wide the pane has to be. The two directions have to
agree exactly, because a disagreement of one pixel is a caption clipped by one
pixel.
}
unit testcase_tool_pane_layout;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, tool_pane_layout,
    //  The rule the pane's width has to satisfy, asserted against the unit that
    //  owns it rather than restated here - the layout check measures with it.
    ui_scaling;

type
    TToolPaneLayoutTest = class(TTestCase)
    private
        FMetrics: TPaneMetrics;
        FLayout: TToolPaneLayout;
        procedure GivenAPane(AWidth: longint = 178);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  The width two columns leave.
        procedure TwoColumnsShareTheWidthAndThreeGaps;
        procedure ANarrowPaneStillGivesAUsableWidth;

        //  Headings.
        procedure AHeadingSpansThePane;
        procedure TheFirstHeadingIsAtTheTop;
        procedure ASecondHeadingClearsTheFirstGroup;

        //  Buttons, two to a row.
        procedure TheFirstButtonSitsUnderItsHeading;
        procedure TheSecondSharesItsRow;
        procedure TheThirdStartsANewRow;
        procedure ButtonsNeverOverlap;

        //  One width, and a pane sized to it.
        procedure EveryButtonIsTheSameWidth;
        procedure ThePaneWidthIsTheInverseOfTheButtonWidth;
        procedure APaneSizedForAButtonHoldsTwoOfThem;
        procedure AZeroWidthButtonStillAsksForAUsablePane;

        //  The width the pane is actually built from.
        procedure ThePaneFitsItsWidestCaption;
        procedure AndIsNeverNarrowerThanTheDesignedWidth;
        procedure ACaptionTheDesignedPaneHoldsFits;
        procedure ALongerOneDoesNot;
        procedure ARowThatRendersNowhereHasNothingToFit;
        procedure NoCaptionsAtAllGivesTheDesignedWidth;
        procedure EveryCaptionFitsTheButtonItIsMeasuredFor;

        //  Groups that end untidily.
        procedure AGroupEndingMidRowStillOccupiesIt;
        procedure ContentGrowsWithEveryGroup;
        procedure ContentHeightIsHonestWithAGroupStillOpen;
    end;

implementation

procedure TToolPaneLayoutTest.SetUp;
begin
    FLayout := nil;
end;

procedure TToolPaneLayoutTest.TearDown;
begin
    FLayout.Free;
    FLayout := nil;
end;

procedure TToolPaneLayoutTest.GivenAPane(AWidth: longint);
begin
    FMetrics.PaneWidth := AWidth;
    FMetrics.Gap := 4;
    FMetrics.ButtonHeight := 23;
    FMetrics.HeadingHeight := 17;
    FLayout.Free;
    FLayout := TToolPaneLayout.Create(FMetrics);
end;

{ ---- the width ---- }

procedure TToolPaneLayoutTest.TwoColumnsShareTheWidthAndThreeGaps;
begin
    GivenAPane(178);
    //  One gap at each edge and one between: (178 - 12) / 2.
    AssertEquals('two columns', 83, TwoColumnButtonWidth(FMetrics));
end;

procedure TToolPaneLayoutTest.ANarrowPaneStillGivesAUsableWidth;
begin
    GivenAPane(6);
    //  A pane can be dragged narrower than its content, and a negative width is
    //  a widget the widget set draws in surprising ways.
    AssertTrue('never negative', TwoColumnButtonWidth(FMetrics) >= 1);
end;

{ ---- headings ---- }

procedure TToolPaneLayoutTest.AHeadingSpansThePane;
var
    R: TPaneRect;
begin
    GivenAPane;
    R := FLayout.StartGroup;
    AssertEquals('inset by one gap', 4, R.Left);
    AssertEquals('and both edges', 178 - 8, R.Width);
end;

procedure TToolPaneLayoutTest.TheFirstHeadingIsAtTheTop;
var
    R: TPaneRect;
begin
    GivenAPane;
    R := FLayout.StartGroup;
    AssertEquals('one gap down', 4, R.Top);
end;

procedure TToolPaneLayoutTest.ASecondHeadingClearsTheFirstGroup;
var
    First, Second: TPaneRect;
    Btn: TPaneRect;
begin
    GivenAPane;
    First := FLayout.StartGroup;
    Btn := FLayout.NextButton;
    FLayout.EndGroup;
    Second := FLayout.StartGroup;
    AssertTrue('below the heading above it', Second.Top > First.Top);
    AssertTrue('and below that group''s button',
        Second.Top >= Btn.Top + Btn.Height);
end;

{ ---- buttons ---- }

procedure TToolPaneLayoutTest.TheFirstButtonSitsUnderItsHeading;
var
    H, B: TPaneRect;
begin
    GivenAPane;
    H := FLayout.StartGroup;
    B := FLayout.NextButton;
    AssertTrue('below it', B.Top >= H.Top + H.Height);
    AssertEquals('in the first column', 4, B.Left);
end;

procedure TToolPaneLayoutTest.TheSecondSharesItsRow;
var
    A, B: TPaneRect;
begin
    GivenAPane;
    FLayout.StartGroup;
    A := FLayout.NextButton;
    B := FLayout.NextButton;
    AssertEquals('the same row', A.Top, B.Top);
    AssertTrue('to its right', B.Left > A.Left);
    //  AND CLEAR OF IT. Two buttons on one row that overlap is the failure this
    //  arithmetic exists to avoid, and it looks like one command missing.
    AssertTrue('not overlapping', B.Left >= A.Left + A.Width);
end;

procedure TToolPaneLayoutTest.TheThirdStartsANewRow;
var
    A, C: TPaneRect;
begin
    GivenAPane;
    FLayout.StartGroup;
    A := FLayout.NextButton;
    FLayout.NextButton;
    C := FLayout.NextButton;
    AssertTrue('a row down', C.Top >= A.Top + A.Height);
    AssertEquals('back in the first column', A.Left, C.Left);
end;

procedure TToolPaneLayoutTest.ButtonsNeverOverlap;
var
    Rects: array[0..8] of TPaneRect;
    i, j: longint;

    function Overlaps(const A, B: TPaneRect): boolean;
    begin
        Result := (A.Left < B.Left + B.Width) and (B.Left < A.Left + A.Width) and
            (A.Top < B.Top + B.Height) and (B.Top < A.Top + A.Height);
    end;

begin
    //  Nine buttons over three groups - the shape the framework's own table
    //  actually produces.
    GivenAPane;
    FLayout.StartGroup;
    for i := 0 to 2 do
        Rects[i] := FLayout.NextButton;
    FLayout.EndGroup;
    FLayout.StartGroup;
    Rects[3] := FLayout.NextButton;
    Rects[4] := FLayout.NextButton;
    Rects[5] := FLayout.NextButton;
    FLayout.EndGroup;
    FLayout.StartGroup;
    for i := 6 to 8 do
        Rects[i] := FLayout.NextButton;
    FLayout.EndGroup;

    for i := 0 to 8 do
        for j := i + 1 to 8 do
            AssertFalse(Format('button %d overlaps %d', [i, j]),
                Overlaps(Rects[i], Rects[j]));
end;

{ ---- one width, and a pane sized to it ---- }

procedure TToolPaneLayoutTest.EveryButtonIsTheSameWidth;
var
    First, Last: TPaneRect;
    i: longint;
begin
    //  ONE WIDTH FOR ALL OF THEM. One button used to be drawn double width to
    //  mark it as the obvious thing to press, and a row of buttons in two sizes
    //  reads as two kinds of control - so the pane now says which is which by
    //  its group headings and its order, not by size.
    GivenAPane;
    FLayout.StartGroup;
    First := FLayout.NextButton;
    for i := 1 to 6 do
        Last := FLayout.NextButton;
    AssertEquals('the same width throughout', First.Width, Last.Width);
    AssertEquals('and it is the two-column width',
        TwoColumnButtonWidth(FMetrics), Last.Width);
end;

procedure TToolPaneLayoutTest.ThePaneWidthIsTheInverseOfTheButtonWidth;
begin
    GivenAPane(178);
    //  THE TWO DIRECTIONS HAVE TO AGREE, because the caller now measures its
    //  widest caption and asks how wide the pane must be - and then the pane
    //  divides that back into columns. A disagreement of one pixel here is a
    //  caption clipped by one pixel, which is exactly how this started.
    AssertEquals('round trip', 178,
        PaneWidthForButton(TwoColumnButtonWidth(FMetrics), FMetrics.Gap));
end;

procedure TToolPaneLayoutTest.APaneSizedForAButtonHoldsTwoOfThem;
var
    A, B: TPaneRect;
begin
    //  A caption needing 120 px: the pane is asked for the width that holds two
    //  of them, and both must actually be that wide and not overlap.
    FMetrics.PaneWidth := PaneWidthForButton(120, 4);
    FMetrics.Gap := 4;
    FMetrics.ButtonHeight := 23;
    FMetrics.HeadingHeight := 17;
    FLayout.Free;
    FLayout := TToolPaneLayout.Create(FMetrics);
    FLayout.StartGroup;
    A := FLayout.NextButton;
    B := FLayout.NextButton;
    AssertEquals('the width asked for', 120, A.Width);
    AssertEquals('for both', 120, B.Width);
    AssertTrue('side by side', B.Left >= A.Left + A.Width);
    AssertTrue('and inside the pane',
        B.Left + B.Width <= FMetrics.PaneWidth);
end;

procedure TToolPaneLayoutTest.AZeroWidthButtonStillAsksForAUsablePane;
begin
    //  Nothing measurable - no rows in the table, or a font that measured
    //  everything as empty. A pane of three gaps is useless but not negative,
    //  and the caller floors it at the designed width anyway.
    AssertTrue('positive', PaneWidthForButton(0, 4) > 0);
    AssertTrue('and no smaller than its gaps', PaneWidthForButton(-5, 4) > 0);
end;

{ ---- the width the pane is built from ---- }

procedure TToolPaneLayoutTest.ThePaneFitsItsWidestCaption;
var
    Wide: longint;
begin
    //  ONE LONG CAPTION DECIDES IT, which is the case a constant pane width got
    //  wrong: a module contributing a longer word than anything the framework
    //  declares had its button clipped, and nothing reported it.
    Wide := PaneWidthForCaptions([30, 200, 45], 4, 178);
    AssertTrue('wider than the designed pane', Wide > 178);
    FMetrics.PaneWidth := Wide;
    FMetrics.Gap := 4;
    AssertTrue('and its button holds the longest caption',
        TwoColumnButtonWidth(FMetrics) >= 200 + 4);
end;

procedure TToolPaneLayoutTest.ACaptionTheDesignedPaneHoldsFits;
begin
    //  THE BUDGET IS PINNED HERE, because it is a number two repositories obey
    //  and neither of them can see it move. The framework's own longest pane
    //  caption is "Subtract"; a module's may be longer, up to this.
    AssertTrue('the framework''s own longest', PaneCaptionFits('Subtract'));
    AssertTrue('and a module''s, at the limit', PaneCaptionFits('Mark Bounds'));
end;

procedure TToolPaneLayoutTest.ALongerOneDoesNot;
begin
    //  One caption over the budget widens every button in the pane, and the
    //  pane takes the width from the chart. Refused with the pane at its
    //  designed width rather than absorbed into it.
    AssertFalse('past the budget', PaneCaptionFits('Undo Wave Detection'));
end;

procedure TToolPaneLayoutTest.ARowThatRendersNowhereHasNothingToFit;
begin
    //  A framework row that drives a menu entry and no button carries no pane
    //  caption at all, and asking whether nothing fits must not refuse it.
    AssertTrue('empty fits', PaneCaptionFits(''));
end;

procedure TToolPaneLayoutTest.AndIsNeverNarrowerThanTheDesignedWidth;
begin
    //  Short captions must not shrink the pane: the rest of the window was laid
    //  out beside it, and a pane that shrank would move the splitter and the
    //  chart for no reason the user asked for.
    AssertEquals('the floor holds', 178,
        PaneWidthForCaptions([10, 12, 8], 4, 178));
end;

procedure TToolPaneLayoutTest.NoCaptionsAtAllGivesTheDesignedWidth;
begin
    //  A table with no pane rows, or a build with every module left out. Not an
    //  error - the floor is the answer.
    AssertEquals('the floor again', 178, PaneWidthForCaptions([], 4, 178));
end;

procedure TToolPaneLayoutTest.EveryCaptionFitsTheButtonItIsMeasuredFor;
var
    Widths: TCaptionWidths;
    i: longint;
begin
    //  THE PROPERTY THAT MATTERS, asserted against ui_scaling's own rule rather
    //  than against this unit's arithmetic - the two agreeing is what keeps the
    //  layout check quiet. Swept over widths that straddle the designed floor,
    //  because the rounding in TwoColumnButtonWidth is where a pixel goes
    //  missing.
    for i := 1 to 200 do
    begin
        Widths := [i];
        FMetrics.PaneWidth := PaneWidthForCaptions(Widths, 4, 178);
        FMetrics.Gap := 4;
        AssertTrue(Format('a caption of %d px fits its button', [i]),
            CaptionFits(i, TwoColumnButtonWidth(FMetrics), 4));
    end;
end;

{ ---- groups that end untidily ---- }

procedure TToolPaneLayoutTest.AGroupEndingMidRowStillOccupiesIt;
var
    A, H: TPaneRect;
begin
    GivenAPane;
    FLayout.StartGroup;
    A := FLayout.NextButton;
    //  One button, so the row is half empty. The next heading must still clear
    //  it.
    FLayout.EndGroup;
    H := FLayout.StartGroup;
    AssertTrue('the heading clears the half row', H.Top >= A.Top + A.Height);
end;

procedure TToolPaneLayoutTest.ContentGrowsWithEveryGroup;
var
    First, Second: longint;
begin
    GivenAPane;
    FLayout.StartGroup;
    FLayout.NextButton;
    FLayout.EndGroup;
    First := FLayout.ContentHeight;
    FLayout.StartGroup;
    FLayout.NextButton;
    FLayout.EndGroup;
    Second := FLayout.ContentHeight;
    //  What a scrolling container needs: a content height that stops growing
    //  would put the last group beyond the scrollable area, which reads as the
    //  pane not having those commands.
    AssertTrue('taller', Second > First);
end;

procedure TToolPaneLayoutTest.ContentHeightIsHonestWithAGroupStillOpen;
var
    B: TPaneRect;
begin
    GivenAPane;
    FLayout.StartGroup;
    B := FLayout.NextButton;
    //  Asked before EndGroup, which a caller may well do: the row the cursor is
    //  standing in counts, or the last button would sit below the reported
    //  height.
    AssertTrue('includes the open row',
        FLayout.ContentHeight >= B.Top + B.Height);
end;

initialization
    //  A unit test: numbers in, rectangles out. No widget, no window, and no
    //  display - which is why none of this could be checked while it was
    //  arithmetic inside a method that needs one.
    RegisterTest('unit', TToolPaneLayoutTest);
end.
