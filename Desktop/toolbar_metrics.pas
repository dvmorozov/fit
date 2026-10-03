// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How much room a toolbar button gets around its icon.)

WHY THIS IS NOT IN THE WINDOW. Left to itself a toolbar draws a button barely
larger than the icon in it and starts the first one on the bar's edge, so the
icons run together in one unbroken block and the bar has no boundary: nothing
tells the eye where one command ends and the next begins, and a pointer aimed
between two of them still hits one. Every widely followed desktop guideline says
the same two things about that - an icon needs white space around it to read as
a separate control, and a pointer target has a minimum size below which it is
missed - and neither is a number a widget set supplies.

QUOTED AT 96 dpi, SCALED BY THE CALLER. The same division tool_pane_layout
follows: what a scaled pixel is belongs to ui_scaling, and this unit must not
have an opinion about displays. Everything here is arithmetic over sizes it is
handed, so the rules can be read and tested with no display at all.
}
unit toolbar_metrics;

{$mode objfpc}{$H+}

interface

const
    { The image lists the toolbars draw from are 16 px square at the design
      density. Stated rather than measured so a test can assert the default
      button size is big enough without a widget set. }
    ToolBarIconSize96 = 16;

    { White space on each side of the icon, at the design density. Two of these
      sit between the icons of any two adjacent buttons, which is what makes
      them read as separate commands. }
    ToolBarButtonPadding96 = 6;

    { Between the outermost buttons and the bar's edge, on every side, at the
      design density. Without it a toolbar reads as continuous with whatever it
      is docked against. }
    ToolBarMargin96 = 4;

    { The smallest a pointer target may be, at the design density. Not a
      preference: below roughly this size the miss rate on a click rises
      sharply, and the accessibility guidance that the rest of the desktop is
      written against draws its line here. The default padding above is chosen
      so the button clears it, and a test asserts that rather than trusting the
      arithmetic to stay true if either constant is edited. }
    ToolBarMinimumTarget96 = 24;

type
    { The sizes one toolbar is laid out with, all already scaled for the
      display. A record rather than four calls, because a bar set to a button
      size from one call and a margin from another is the disagreement this
      unit exists to prevent. }
    TToolBarMetrics = record
        { One button. Square: the icons are square and a wider button would put
          more space to the sides of an icon than above it. }
        ButtonWidth, ButtonHeight: longint;
        { Around the whole row or column of buttons. }
        Margin: longint;
        { Across a bar that holds a single file of buttons - a button and the
          margin on both sides of it. What the narrow vertical strips are
          sized to. }
        Thickness: longint;
    end;

{ How big a button holding an AIconSize icon with APadding around it must be.
  Padding below zero is no padding: a negative one would be a button smaller
  than the icon it draws. }
function ToolBarButtonSize(AIconSize, APadding: longint): longint;

{ Every size one bar needs, from the icon it draws and the two spacings. }
function ToolBarMetricsFor(AIconSize, APadding, AMargin: longint):
    TToolBarMetrics;

{ Whether a button of AButtonSize is a target a pointer can be expected to hit.
  Asked of a measured button in the running window, which is the only place the
  answer is about what the user sees. }
function PointerTargetMet(AButtonSize, AMinimum: longint): boolean;

implementation

function ToolBarButtonSize(AIconSize, APadding: longint): longint;
begin
    Result := AIconSize;
    if APadding > 0 then
        Inc(Result, 2 * APadding);
end;

function ToolBarMetricsFor(AIconSize, APadding, AMargin: longint):
    TToolBarMetrics;
begin
    Result.ButtonWidth := ToolBarButtonSize(AIconSize, APadding);
    Result.ButtonHeight := Result.ButtonWidth;
    Result.Margin := AMargin;
    if Result.Margin < 0 then
        Result.Margin := 0;
    Result.Thickness := Result.ButtonWidth + 2 * Result.Margin;
end;

function PointerTargetMet(AButtonSize, AMinimum: longint): boolean;
begin
    Result := AButtonSize >= AMinimum;
end;

end.
