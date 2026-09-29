// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where each heading and button of the Tools pane sits.)

WHY THIS IS NOT IN THE WINDOW. It is arithmetic - a running y, two columns of
one width, a gap after each group - and arithmetic in a method that needs a
window is arithmetic no test can read. What went wrong
without it is not hypothetical either: the pane is generated from a table a
module can add rows to, so the number of rows and the number of groups are both
unknown until run time, and the failure modes are a button drawn on top of
another and a row placed below the visible area. Both look like the pane simply
not having that command.

RECTANGLES, NOT WIDGETS. Nothing here knows what a button is; it answers where
the n-th thing goes. The window creates widgets and calls SetBounds with what
this returns.
}
unit tool_pane_layout;

{$mode objfpc}{$H+}

interface

type
    { A rectangle, in the pane's own client coordinates. Its own type rather
      than the widget set's TRect, so this unit names nothing from the LCL and
      is reachable from the suite that compiles without one. }
    TPaneRect = record
        Left, Top, Width, Height: longint;
    end;

    { The measured width of each caption the pane will draw, in the font it
      will draw them in. Measuring needs a font and a canvas, which is the
      window's business; deciding how wide the pane must be from them is not. }
    TCaptionWidths = array of longint;

    { The sizes the pane is laid out with, all already scaled for the display.
      Passed in rather than computed here: what a scaled pixel is belongs to
      ui_scaling, and this unit must not have an opinion about fonts. }
    TPaneMetrics = record
        { The pane's usable width. }
        PaneWidth: longint;
        { Between things, and half of it between the rows of one group. }
        Gap: longint;
        ButtonHeight: longint;
        HeadingHeight: longint;
    end;

    { Walks the rows of the pane, in order, and says where each goes.

      A CURSOR RATHER THAN A FUNCTION OF AN INDEX, because the position of the
      n-th button depends on every row before it: how many groups have opened
      and how many buttons each held. A function of the index alone would have
      to re-walk that, which is the same loop with a chance to disagree with
      itself. }
    TToolPaneLayout = class
    private
        FMetrics: TPaneMetrics;
        FY: longint;
        FColumn: longint;
        FButtonWidth: longint;
    public
        constructor Create(const AMetrics: TPaneMetrics);

        { Opens a group and answers where its heading goes. }
        function StartGroup: TPaneRect;
        { Where the next button of the open group goes.

          EVERY BUTTON IS THE SAME WIDTH. One of them used to take the full
          width and a row of its own, to mark the obvious thing to press; a row
          of buttons in two sizes reads as two kinds of control, and which one
          is obvious is the group heading's job. }
        function NextButton: TPaneRect;
        { Closes the open group, so the next heading clears its last row. }
        procedure EndGroup;

        { How tall the pane's content has grown - what a scrolling container
          needs to know. }
        function ContentHeight: longint;
        { What every button in this pane is. Two columns, so a caption like
          "Subtract" has room a third would not leave it - and the pane is sized
          from this rather than the other way about (PaneWidthForButton). }
        property ButtonWidth: longint read FButtonWidth;
    end;

{ The button width two columns leave in a pane of this width. Exposed because it
  is the one number a caller may want before laying anything out. }
function TwoColumnButtonWidth(const AMetrics: TPaneMetrics): longint;

{ The pane width two columns of AButtonWidth need - the inverse of the above.

  WHICH WAY ROUND THIS GOES IS THE POINT. The pane width was a constant and the
  button width was whatever it left, so a caption longer than that constant
  allowed was clipped and the only remedy was to guess a bigger constant. The
  caller measures its widest caption instead and asks how wide the pane has to
  be to hold it, which is a question with one answer. }
function PaneWidthForButton(AButtonWidth, AGap: longint): longint;

{ How wide the pane has to be to draw all of these captions, never narrower than
  AMinPaneWidth.

  THE PADDING IS TWICE THE GAP, and that is the whole subtlety. ui_scaling's
  CaptionFits allows a button one gap of padding, so a button sized to exactly
  one gap more than its text sits on that limit - and TwoColumnButtonWidth
  divides the pane width by two, so a pane rounding down by one pixel puts the
  caption one pixel over and the layout check reports the button this arithmetic
  exists to size. Two gaps leave it room to round.

  AMinPaneWidth is the width the rest of the window was laid out beside: a pane
  that shrank to fit short captions would move the splitter and the chart for no
  reason the user asked for. }
function PaneWidthForCaptions(const AWidths: TCaptionWidths;
    AGap, AMinPaneWidth: longint): longint;

const
    { The longest a caption may be before the button it sits on widens the pane
      past the width the window was designed around.

      WHY A BUDGET AT ALL, when the pane widens to fit. Because widening is not
      free: every button in the pane is one width, so the longest caption in it
      sets all of them, the pane grows, and the splitter and the chart move to
      pay for it. The pane fitting its captions is what stops a button being
      clipped; this is what stops one caption rearranging the window.

      WHERE TWELVE COMES FROM. The designed pane is 178 px at 96 ppi, which
      PaneWidthForCaptions inverts to a caption of at most 75 px
      (2 * (w + 2 * gap) + 3 * gap <= 178 with gap 4), and the designed font
      draws about six pixels to the character. Characters rather than pixels
      because this has to be answerable with no widget set: the pixel truth is
      measured in the window, where ui_dpi reports a caption that does not fit.

      STATED HERE, in the framework, because a module spends the same budget and
      cannot be expected to rediscover it - see docs/contributing/writing-a-module. }
    PaneCaptionBudget = 12;

{ Whether a caption is short enough to leave the pane the width it was designed
  at. Empty fits: a row that renders nowhere has nothing to draw. }
function PaneCaptionFits(const ACaption: string): boolean;

implementation

function TwoColumnButtonWidth(const AMetrics: TPaneMetrics): longint;
begin
    //  Two columns and three gaps: one at each edge and one between. Never
    //  negative, because a pane can be dragged narrower than its content and a
    //  negative width is a widget the widget set draws in surprising ways.
    Result := (AMetrics.PaneWidth - 3 * AMetrics.Gap) div 2;
    if Result < 1 then
        Result := 1;
end;

function PaneWidthForButton(AButtonWidth, AGap: longint): longint;
begin
    //  Two columns and three gaps, the same three the width above subtracts:
    //  one at each edge and one between.
    if AButtonWidth < 1 then
        AButtonWidth := 1;
    Result := 2 * AButtonWidth + 3 * AGap;
end;

function PaneWidthForCaptions(const AWidths: TCaptionWidths;
    AGap, AMinPaneWidth: longint): longint;
var
    i, Widest: longint;
begin
    Widest := 0;
    for i := 0 to High(AWidths) do
        if AWidths[i] > Widest then
            Widest := AWidths[i];
    //  NO CAPTIONS AT ALL is not an error: a table with no pane rows, or a
    //  build where every module was left out. The floor answers it.
    Result := PaneWidthForButton(Widest + 2 * AGap, AGap);
    if Result < AMinPaneWidth then
        Result := AMinPaneWidth;
end;

function PaneCaptionFits(const ACaption: string): boolean;
begin
    Result := Length(ACaption) <= PaneCaptionBudget;
end;

constructor TToolPaneLayout.Create(const AMetrics: TPaneMetrics);
begin
    inherited Create;
    FMetrics := AMetrics;
    FButtonWidth := TwoColumnButtonWidth(AMetrics);
    FY := AMetrics.Gap;
    FColumn := 0;
end;

function TToolPaneLayout.StartGroup: TPaneRect;
begin
    Result.Left := FMetrics.Gap;
    Result.Top := FY;
    Result.Width := FMetrics.PaneWidth - 2 * FMetrics.Gap;
    if Result.Width < 1 then
        Result.Width := 1;
    Result.Height := FMetrics.HeadingHeight;

    FY := FY + FMetrics.HeadingHeight + FMetrics.Gap div 2;
    //  A new group starts a new row whatever the last one left behind.
    FColumn := 0;
end;

function TToolPaneLayout.NextButton: TPaneRect;
begin
    Result.Left := FMetrics.Gap + FColumn * (FButtonWidth + FMetrics.Gap);
    Result.Top := FY;
    Result.Width := FButtonWidth;
    Result.Height := FMetrics.ButtonHeight;

    Inc(FColumn);
    if FColumn >= 2 then
    begin
        FColumn := 0;
        FY := FY + FMetrics.ButtonHeight + FMetrics.Gap div 2;
    end;
end;

procedure TToolPaneLayout.EndGroup;
begin
    //  A group that ended mid-row still occupies that row.
    if FColumn <> 0 then
    begin
        FY := FY + FMetrics.ButtonHeight + FMetrics.Gap div 2;
        FColumn := 0;
    end;
    FY := FY + FMetrics.Gap;
end;

function TToolPaneLayout.ContentHeight: longint;
begin
    //  Whatever the cursor has reached, plus the row it is standing in when a
    //  group was left open - a caller may ask before calling EndGroup.
    Result := FY;
    if FColumn <> 0 then
        Result := Result + FMetrics.ButtonHeight + FMetrics.Gap div 2;
end;

end.
