// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the window has been asked to do later, and when it may.)

TWO PIECES OF WORK CANNOT BE DONE WHEN THEY ARE ASKED FOR, and both of them
learned that from a freeze:

  A MESSAGE RAISED FROM A TIMER OR THE CALCULATION THREAD. Opening a dialog from
  there runs a message loop inside whatever call faulted - which under X11 may
  hold a pointer and keyboard grab, and then the grab is never released and every
  click in the session goes to a process that is not answering.

  A MENU REBUILD ASKED FOR BY A MENU ITEM'S OWN CLICK. The rebuild destroys the
  items, Sender among them, while the widget set is still dispatching that click.
  What follows is an access violation with no frame of ours on the stack.

So both are recorded and done from the main loop instead. This unit is the
recording: which work is outstanding, whether it may run yet, and what running it
consumes. The window keeps the parts that are actually the widget set's - the
async call, the timer, the dialog, and asking whether a menu is open.

THE RULES, each with a failure behind it:

  * AT MOST ONE DIALOG IS OUTSTANDING, and a second request while one is pending
    is DROPPED, not queued. The state poll fires twice a second; a fault on its
    path would otherwise stack dialogs until nothing else on screen can be
    reached.

  * AT MOST ONE REBUILD, for a gentler reason: several clicks can arrive before
    the main loop turns, one rebuild covers them all, and a second would work
    over items the first had just created.

  * NOTHING RUNS WHILE A MENU IS OPEN. A dialog opened over a dropped-down menu
    takes the grab from it and leaves it painted on screen belonging to nothing;
    a rebuild destroys the submenu the user is reading. The work stays
    outstanding and is asked for again shortly.

  * THE REBUILD GOES FIRST. Every queued message about curve types was queued by
    something that also asked for the rebuild, and it describes the menu as it
    will be - "select one in Model \ Curve Type" is about entries the rebuild has
    yet to create.
}
unit deferred_ui;

{$mode objfpc}{$H+}

interface

type
    { How a queued message should be presented. The widget set's own
      TMsgDlgType is not named here: this unit is compiled by the light test
      suite, which has no widget set, and the window maps these onto it. }
    TNoticeKind = (nkInformation, nkWarning, nkError);

    { One piece of deferred work, in the order it must be done. }
    TDeferredWork = (dwNothing, dwMenuRebuild, dwDialog);

    { What the window owes the user, and whether now is the time.

      Plain TObject: it holds no widget and no interface, and it is owned by the
      form for the form's lifetime. }
    TDeferredUi = class(TObject)
    private
        FDialogPending: boolean;
        FRebuildPending: boolean;
        FMessage: string;
        FKind: TNoticeKind;
        FDroppedDialogs: longint;
    public
        { Records a message to be shown from the main loop.

          False when one is already outstanding, in which case THIS MESSAGE IS
          DROPPED and the caller queues nothing. Dropping is deliberate: the
          alternative is a stack of dialogs from a repeating fault. }
        function RequestDialog(const AMessage: string;
            AKind: TNoticeKind): boolean;
        { Records that the curve-type menu must be rebuilt. False when one is
          already outstanding - one rebuild covers every click that asked. }
        function RequestMenuRebuild: boolean;
        { What to do now, given whether a menu is open. dwNothing when there is
          nothing outstanding OR when a menu is open - the work keeps. }
        function WorkNow(AMenuIsOpen: boolean): TDeferredWork;
        { True while something is outstanding, whether or not it may run - which
          is what the window's retry timer runs on. }
        function AnythingOutstanding: boolean;
        { Takes the pending message, clearing it. The caller shows it; if the
          show raises, the message is gone all the same, which is the point -
          a message that could not be shown must not block every later one. }
        function TakeDialog(out AMessage: string; out AKind: TNoticeKind): boolean;
        { Marks the rebuild done. Called after it has run, and after a failed
          attempt too: leaving it pending would rebuild on every timer tick. }
        procedure RebuildDone;
        { How many messages were dropped because one was already outstanding.
          Not shown to the user - it is here so that a fault storm leaves a trace
          rather than looking like a single error. }
        property DroppedDialogs: longint read FDroppedDialogs;
    end;

implementation

function TDeferredUi.RequestDialog(const AMessage: string;
    AKind: TNoticeKind): boolean;
begin
    if FDialogPending then
    begin
        //  Dropped, and counted. See the unit header for why this is not a
        //  queue.
        Inc(FDroppedDialogs);
        Result := False;
        Exit;
    end;
    FDialogPending := True;
    FMessage := AMessage;
    FKind := AKind;
    Result := True;
end;

function TDeferredUi.RequestMenuRebuild: boolean;
begin
    Result := not FRebuildPending;
    FRebuildPending := True;
end;

function TDeferredUi.WorkNow(AMenuIsOpen: boolean): TDeferredWork;
begin
    Result := dwNothing;
    //  The work keeps. Both kinds destroy or cover what the user is reading.
    if AMenuIsOpen then
        Exit;
    //  THE REBUILD FIRST: a queued message about curve types describes the menu
    //  the rebuild has yet to produce.
    if FRebuildPending then
        Result := dwMenuRebuild
    else if FDialogPending then
        Result := dwDialog;
end;

function TDeferredUi.AnythingOutstanding: boolean;
begin
    Result := FRebuildPending or FDialogPending;
end;

function TDeferredUi.TakeDialog(out AMessage: string;
    out AKind: TNoticeKind): boolean;
begin
    Result := FDialogPending;
    AMessage := FMessage;
    AKind := FKind;
    //  Cleared before the caller shows anything, so that a show which raises
    //  cannot leave the flag set and every later message blocked behind it.
    FDialogPending := False;
    FMessage := '';
end;

procedure TDeferredUi.RebuildDone;
begin
    FRebuildPending := False;
end;

end.
