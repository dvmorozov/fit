// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A tick that arrives before the row it belongs to.)

WHAT IS BEING PINNED. The user-curve properties dialog lets a parameter be fixed
by ticking it in a checked list. The list box reports the tick and the change of
selected row through two different handlers, and the tick comes first - so on the
very first interaction there is no row to apply it to at all.

THE HANDLERS CANNOT BE DRIVEN, so the rule was taken out of them. An LCL list box
needs a parent and a canvas before it will report anything, and the sequence that
produces the defect is precisely the one nobody reproduces by hand: it happens
once, on the first tick after the dialog opens, and it either faults on an index
of -1 or fixes the wrong parameter without saying so.

THE STATE IS ONE BOOLEAN AND THE RULES ARE TWO, which is why this file is short.
What makes it worth having is that both rules are about ORDER, and order is what
a handler pair is worst at making visible.
}
unit testcase_deferred_tick;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, deferred_tick;

type
    TDeferredTickTest = class(TTestCase)
    private
        FTick: TDeferredTick;
    protected
        procedure SetUp; override;
    published
        procedure NothingIsOwedToBeginWith;

        //  A tick with a row under it.
        procedure ATickOnASelectedRowIsAppliedAtOnce;
        procedure AndLeavesNothingOwing;
        procedure ThePlainClickThatFollowsAppliesNothing;

        //  A tick with no row under it.
        procedure ATickWithNoSelectionIsNotApplied;
        procedure ItIsRememberedInstead;
        procedure TheClickThatFollowsCarriesItOut;
        procedure AndOnlyOnce;
        procedure ASecondClickAppliesNothing;

        //  Starting again.
        procedure AFreshStateOwesNothingEvenAfterATick;

        //  The one case the dialog does not produce, characterised.
        procedure ATickWithASelectionDoesNotCancelARememberedOne;
    end;

implementation

procedure TDeferredTickTest.SetUp;
begin
    FTick := NoDeferredTick;
end;

procedure TDeferredTickTest.NothingIsOwedToBeginWith;
begin
    AssertFalse('nothing remembered', FTick.Pending);
end;

{ ---------------------------- a tick with a row ----------------------------- }

procedure TDeferredTickTest.ATickOnASelectedRowIsAppliedAtOnce;
begin
    //  The ordinary case: the row is already selected, so the tick means what
    //  it appears to mean and is carried out where the user is looking.
    AssertTrue('applied now', FTick.Ticked(True));
end;

procedure TDeferredTickTest.AndLeavesNothingOwing;
begin
    FTick.Ticked(True);
    AssertFalse('nothing remembered', FTick.Pending);
end;

procedure TDeferredTickTest.ThePlainClickThatFollowsAppliesNothing;
begin
    //  Selecting a row is not by itself a request to fix its parameter. A
    //  click that applied one would fix a parameter every time the user looked
    //  at a different row - which is the same damage as the deferred tick,
    //  arrived at from the other direction.
    FTick.Ticked(True);
    AssertFalse('a click alone changes nothing', FTick.Clicked);
end;

{ --------------------------- a tick with no row ----------------------------- }

procedure TDeferredTickTest.ATickWithNoSelectionIsNotApplied;
begin
    //  THE DEFECT THIS PREVENTS. Applying here reads the item at ItemIndex,
    //  which is -1: either a fault, or - if the selection has merely not caught
    //  up - the wrong parameter fixed silently.
    AssertFalse('not applied', FTick.Ticked(False));
end;

procedure TDeferredTickTest.ItIsRememberedInstead;
begin
    FTick.Ticked(False);
    AssertTrue('owing', FTick.Pending);
end;

procedure TDeferredTickTest.TheClickThatFollowsCarriesItOut;
begin
    //  The selection catching up is what makes the tick meaningful, so that is
    //  when it is applied - to the row the user actually ticked.
    FTick.Ticked(False);
    AssertTrue('applied with the row', FTick.Clicked);
end;

procedure TDeferredTickTest.AndOnlyOnce;
begin
    FTick.Ticked(False);
    FTick.Clicked;
    AssertFalse('nothing left owing', FTick.Pending);
end;

procedure TDeferredTickTest.ASecondClickAppliesNothing;
begin
    //  CLEARING IS PART OF THE RULE. A remembered tick that survived its own
    //  click would be re-applied by every later click on any row, fixing a
    //  parameter each time the user looked at another one.
    FTick.Ticked(False);
    FTick.Clicked;
    AssertFalse('and the next click does nothing', FTick.Clicked);
end;

{ ------------------------------ starting again ------------------------------ }

procedure TDeferredTickTest.AFreshStateOwesNothingEvenAfterATick;
begin
    //  The dialog is reused between visits. A tick left owing by a previous
    //  one would be carried out against a list of different parameters.
    FTick.Ticked(False);
    FTick := NoDeferredTick;
    AssertFalse('nothing carried over', FTick.Pending);
    AssertFalse('and nothing to apply', FTick.Clicked);
end;

{ ------------------------- characterised, not endorsed ---------------------- }

procedure TDeferredTickTest.ATickWithASelectionDoesNotCancelARememberedOne;
begin
    //  WHAT THE DIALOG HAS ALWAYS DONE. Only a click clears the flag, so a tick
    //  applied immediately leaves an earlier remembered one still owing. It
    //  takes two ticks with no row change between them to reach, which the list
    //  box does not produce - so it is pinned rather than changed, because
    //  changing it would be a behaviour change decided without being able to
    //  drive the widget.
    FTick.Ticked(False);
    FTick.Ticked(True);
    AssertTrue('the earlier one is still owing', FTick.Pending);
end;

initialization
    //  Unit tests: one boolean and two rules, no widget.
    RegisterTest('unit', TDeferredTickTest);
end.
