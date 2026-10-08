// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How much room a toolbar button gets around its icon.)

WHY THESE TESTS EXIST. A toolbar left at the widget set's own metrics draws a
button barely bigger than the icon in it and starts the first one on the bar's
edge. Nothing raises, nothing is logged, and no headless suite can see it - the
window simply shows a block of icons with no boundary between one command and
the next, and a pointer aimed between two of them still hits one.

So the arithmetic that decides the spacing is here, where it can be asserted
without a display, and the window is left applying what it returns. The two
rules worth defending are that the padding really does appear on both sides of
the icon, and that the size this unit hands out clears the smallest target a
pointer can be expected to hit - the second one written against the constants
themselves, so editing either of them into a button the user would miss fails
by name.
}
unit testcase_toolbar_metrics;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, toolbar_metrics;

type
    TToolBarMetricsTest = class(TTestCase)
    published
        //  The button.
        procedure AButtonIsTheIconPlusPaddingOnBothSides;
        procedure AButtonIsSquare;
        procedure NoPaddingIsTheIconItself;
        procedure NegativePaddingIsNoPadding;
        procedure AButtonAlwaysHoldsItsIcon;

        //  The gap the user actually sees, which is what the padding is for.
        procedure TwoAdjacentIconsAreTwoPaddingsApart;

        //  The bar around them.
        procedure TheBarKeepsItsButtonsOffItsEdge;
        procedure NegativeMarginIsNoMargin;
        procedure ASingleFileBarIsAButtonAndBothMargins;

        //  The rule the running window is measured against.
        procedure TheDefaultSpacingClearsThePointerTarget;
        procedure ATightButtonFailsThePointerTarget;
        procedure AButtonAtExactlyTheMinimumPasses;
    end;

implementation

procedure TToolBarMetricsTest.AButtonIsTheIconPlusPaddingOnBothSides;
begin
    AssertEquals(16 + 2 * 6, ToolBarButtonSize(16, 6));
end;

procedure TToolBarMetricsTest.AButtonIsSquare;
var
    M: TToolBarMetrics;
begin
    M := ToolBarMetricsFor(16, 6, 4);
    AssertEquals(M.ButtonWidth, M.ButtonHeight);
end;

procedure TToolBarMetricsTest.NoPaddingIsTheIconItself;
begin
    AssertEquals(16, ToolBarButtonSize(16, 0));
end;

procedure TToolBarMetricsTest.NegativePaddingIsNoPadding;
begin
    //  A negative padding would be a button smaller than the icon it draws.
    AssertEquals(16, ToolBarButtonSize(16, -5));
end;

procedure TToolBarMetricsTest.AButtonAlwaysHoldsItsIcon;
var
    Padding: longint;
begin
    for Padding := -8 to 24 do
        AssertTrue(Format('padding %d leaves no room for the icon', [Padding]),
            ToolBarButtonSize(16, Padding) >= 16);
end;

procedure TToolBarMetricsTest.TwoAdjacentIconsAreTwoPaddingsApart;
begin
    //  What the eye reads as the separation between two commands: the padding
    //  on the right of one icon and on the left of the next.
    AssertEquals(2 * 6, ToolBarButtonSize(16, 6) - 16);
end;

procedure TToolBarMetricsTest.TheBarKeepsItsButtonsOffItsEdge;
var
    M: TToolBarMetrics;
begin
    M := ToolBarMetricsFor(16, 6, 4);
    AssertEquals(4, M.Margin);
end;

procedure TToolBarMetricsTest.NegativeMarginIsNoMargin;
var
    M: TToolBarMetrics;
begin
    M := ToolBarMetricsFor(16, 6, -3);
    AssertEquals(0, M.Margin);
end;

procedure TToolBarMetricsTest.ASingleFileBarIsAButtonAndBothMargins;
var
    M: TToolBarMetrics;
begin
    M := ToolBarMetricsFor(16, 6, 4);
    AssertEquals(M.ButtonWidth + 2 * 4, M.Thickness);
end;

procedure TToolBarMetricsTest.TheDefaultSpacingClearsThePointerTarget;
begin
    //  AGAINST THE CONSTANTS, not against a number repeated here: this is the
    //  assertion that fails if either the icon size or the padding is ever
    //  edited into a button the user would miss.
    AssertTrue('the default button is under the smallest usable target',
        PointerTargetMet(
            ToolBarButtonSize(ToolBarIconSize96, ToolBarButtonPadding96),
            ToolBarMinimumTarget96));
end;

procedure TToolBarMetricsTest.ATightButtonFailsThePointerTarget;
begin
    AssertFalse(PointerTargetMet(22, ToolBarMinimumTarget96));
end;

procedure TToolBarMetricsTest.AButtonAtExactlyTheMinimumPasses;
begin
    AssertTrue(PointerTargetMet(ToolBarMinimumTarget96, ToolBarMinimumTarget96));
end;

initialization
    RegisterTest('unit', TToolBarMetricsTest);
end.
