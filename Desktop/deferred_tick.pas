// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A tick that arrives before the row it belongs to.)

THE WIDGET TELLS YOU IN THE WRONG ORDER. A checked list box reports a click on
its checkbox through one handler and a change of the selected row through
another, and clicking the box of a row that is not the selected one fires the
first BEFORE the selection has moved. So at the moment the tick is reported there
is no row to apply it to: ItemIndex is still -1, or still the previous row.

APPLYING IT ANYWAY IS THE BUG THIS EXISTS TO PREVENT. Reading the item at
ItemIndex when the selection has not caught up either faults on -1 or - far worse
- fixes the parameter the user was previously looking at, silently, while the one
they actually ticked stays free. Both symptoms appear one interaction later than
their cause.

SO A TICK WITH NO ROW IS REMEMBERED, not applied, and the click that follows
carries it out. That is a two-state machine with one rule in each direction, and
it is the whole of the logic left in the properties dialog once the roles moved
to parameter_roles - which is why it is here rather than in a handler where no
test can reach it.

CLEARING THE FLAG IS PART OF THE RULE, not bookkeeping around it. A remembered
tick that survived the click it was waiting for would be re-applied by every
later click on any row, turning one tick into a fixed parameter each time the
user looked at another one.
}
unit deferred_tick;

{$MODE Delphi}

interface

type
    { Remembers a tick that arrived with no row selected, and hands it to the
      click that follows. One instance per list; a plain record, because the
      state is one boolean and nothing owns it. }
    TDeferredTick = record
    private
        FPending: boolean;
    public
        { A checkbox was clicked. AHasSelection is whether the list has a row
          selected at that moment. Returns True when the tick can be applied
          now; when it returns False the tick is remembered instead. }
        function Ticked(AHasSelection: boolean): boolean;
        { The selected row changed. Returns True when a remembered tick is due
          with it - and forgets it, so it is applied once and not again. }
        function Clicked: boolean;
        { Whether a tick is waiting for a row. Nothing in the dialog needs this;
          it is here so that "remembered" can be asserted directly rather than
          inferred from the next call's answer. }
        property Pending: boolean read FPending;
    end;

{ A fresh state: nothing remembered. }
function NoDeferredTick: TDeferredTick;

implementation

function NoDeferredTick: TDeferredTick;
begin
    Result.FPending := False;
end;

function TDeferredTick.Ticked(AHasSelection: boolean): boolean;
begin
    Result := AHasSelection;
    //  SET, NEVER CLEARED HERE, which is what the dialog has always done. Only
    //  the click that carries a remembered tick out clears it, so a tick that
    //  is applied immediately leaves an earlier remembered one still owing.
    //  That is reachable only by ticking twice with no row change in between,
    //  which the list box does not produce; it is kept rather than tidied
    //  because tidying it would be a behaviour change made blind, and it is
    //  pinned in the tests so the choice is visible.
    if not AHasSelection then
        FPending := True;
end;

function TDeferredTick.Clicked: boolean;
begin
    Result := FPending;
    FPending := False;
end;

end.
