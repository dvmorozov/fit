// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the window has been asked to do later, and when it may.)

THE MACHINERY BEHIND TWO FREEZES, and until now reachable only by reproducing
them. A message raised from a timer, a menu rebuild asked for by the menu item
being clicked: both have to wait for the main loop, and while they wait there are
four rules, each of which was a bug once.

The one that reads like a mistake and is not: a second message arriving while one
is outstanding is DROPPED. The state poll fires twice a second, so a fault on its
path would otherwise stack dialogs until nothing else on screen can be reached.
It is counted rather than silently discarded, so a fault storm leaves a trace.
}
unit testcase_deferred_ui;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, deferred_ui;

type
    TDeferredUiTest = class(TTestCase)
    private
        FUi: TDeferredUi;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure WithNothingAskedForThereIsNothingToDo;
        procedure AMessageIsWorthQueueingFor;
        procedure ASecondMessageIsDroppedRatherThanStacked;
        procedure ADroppedMessageIsCounted;
        procedure ARebuildIsWorthQueueingFor;
        procedure ASecondRebuildRequestNeedsNoSecondQueueing;
        procedure NothingRunsWhileAMenuIsOpen;
        procedure AndTheWorkIsStillThereWhenItCloses;
        procedure TheRebuildGoesBeforeTheMessage;
        procedure TakingTheMessageClearsIt;
        procedure AMessageTakenIsNoLongerOutstanding;
        procedure AndTheNextOneIsAcceptedAgain;
        procedure TheRebuildStaysUntilItIsDeclaredDone;
        procedure TakingAMessageThatIsNotThereAnswersFalse;
    end;

implementation

procedure TDeferredUiTest.SetUp;
begin
    FUi := TDeferredUi.Create;
end;

procedure TDeferredUiTest.TearDown;
begin
    FreeAndNil(FUi);
end;

procedure TDeferredUiTest.WithNothingAskedForThereIsNothingToDo;
begin
    AssertTrue('nothing outstanding', not FUi.AnythingOutstanding);
    AssertTrue('and nothing to run', FUi.WorkNow(False) = dwNothing);
end;

procedure TDeferredUiTest.AMessageIsWorthQueueingFor;
begin
    //  True means "queue the async call": the window must not queue one for a
    //  request that was dropped, or the loop wakes for nothing.
    AssertTrue('queue it', FUi.RequestDialog('boom', nkError));
    AssertTrue('outstanding', FUi.AnythingOutstanding);
    AssertTrue('and ready to run', FUi.WorkNow(False) = dwDialog);
end;

procedure TDeferredUiTest.ASecondMessageIsDroppedRatherThanStacked;
begin
    //  THE RULE THAT LOOKS LIKE A MISTAKE. A repeating fault - the state poll is
    //  twice a second - would otherwise open a dialog per occurrence until
    //  nothing else can be reached.
    AssertTrue('the first', FUi.RequestDialog('first', nkError));
    AssertTrue('the second is dropped',
        not FUi.RequestDialog('second', nkError));
end;

procedure TDeferredUiTest.ADroppedMessageIsCounted;
begin
    AssertEquals('none yet', 0, FUi.DroppedDialogs);
    FUi.RequestDialog('first', nkError);
    FUi.RequestDialog('second', nkError);
    FUi.RequestDialog('third', nkError);
    //  Counted, so a storm leaves a trace rather than looking like one error.
    AssertEquals('two dropped', 2, FUi.DroppedDialogs);
end;

procedure TDeferredUiTest.ARebuildIsWorthQueueingFor;
begin
    AssertTrue('queue it', FUi.RequestMenuRebuild);
    AssertTrue('outstanding', FUi.AnythingOutstanding);
    AssertTrue('ready', FUi.WorkNow(False) = dwMenuRebuild);
end;

procedure TDeferredUiTest.ASecondRebuildRequestNeedsNoSecondQueueing;
begin
    //  Several clicks can arrive before the main loop turns. One rebuild covers
    //  them; a second would work over the items the first had just created.
    AssertTrue('the first', FUi.RequestMenuRebuild);
    AssertTrue('the second needs no queueing', not FUi.RequestMenuRebuild);
    AssertTrue('and one is still outstanding',
        FUi.WorkNow(False) = dwMenuRebuild);
end;

procedure TDeferredUiTest.NothingRunsWhileAMenuIsOpen;
begin
    //  A dialog over a dropped-down menu takes its grab and leaves it painted
    //  belonging to nothing; a rebuild destroys the submenu being read.
    FUi.RequestDialog('boom', nkError);
    FUi.RequestMenuRebuild;
    AssertTrue('not now', FUi.WorkNow(True) = dwNothing);
end;

procedure TDeferredUiTest.AndTheWorkIsStillThereWhenItCloses;
begin
    //  Asking while a menu is open must not consume anything - the whole point
    //  is that the work keeps.
    FUi.RequestDialog('boom', nkError);
    FUi.WorkNow(True);
    FUi.WorkNow(True);
    AssertTrue('still outstanding', FUi.AnythingOutstanding);
    AssertTrue('and runs once the menu closes',
        FUi.WorkNow(False) = dwDialog);
end;

procedure TDeferredUiTest.TheRebuildGoesBeforeTheMessage;
var
    Msg: string;
    Kind: TNoticeKind;
begin
    //  ORDER MATTERS AND IS NOT ARBITRARY: the queued message describes the menu
    //  the rebuild has yet to produce - "select one in Model \ Curve Type" is
    //  about entries that do not exist until it runs.
    FUi.RequestDialog('the type you were fitting is gone', nkInformation);
    FUi.RequestMenuRebuild;
    AssertTrue('rebuild first', FUi.WorkNow(False) = dwMenuRebuild);
    FUi.RebuildDone;
    AssertTrue('then the message', FUi.WorkNow(False) = dwDialog);
    AssertTrue('and it is the one that was queued',
        FUi.TakeDialog(Msg, Kind));
    AssertEquals('unchanged by waiting',
        'the type you were fitting is gone', Msg);
end;

procedure TDeferredUiTest.TakingTheMessageClearsIt;
var
    Msg: string;
    Kind: TNoticeKind;
begin
    FUi.RequestDialog('careful', nkWarning);
    AssertTrue('taken', FUi.TakeDialog(Msg, Kind));
    AssertEquals('the text', 'careful', Msg);
    AssertTrue('and its kind', Kind = nkWarning);
end;

procedure TDeferredUiTest.AMessageTakenIsNoLongerOutstanding;
var
    Msg: string;
    Kind: TNoticeKind;
begin
    //  CLEARED BEFORE IT IS SHOWN, deliberately: if showing it raises, the flag
    //  must not stay set with every later message blocked behind it.
    FUi.RequestDialog('boom', nkError);
    FUi.TakeDialog(Msg, Kind);
    AssertTrue('nothing outstanding', not FUi.AnythingOutstanding);
    AssertTrue('nothing to run', FUi.WorkNow(False) = dwNothing);
end;

procedure TDeferredUiTest.AndTheNextOneIsAcceptedAgain;
var
    Msg: string;
    Kind: TNoticeKind;
begin
    FUi.RequestDialog('first', nkError);
    FUi.TakeDialog(Msg, Kind);
    AssertTrue('the next is accepted', FUi.RequestDialog('second', nkError));
    FUi.TakeDialog(Msg, Kind);
    AssertEquals('and it is the second one', 'second', Msg);
end;

procedure TDeferredUiTest.TheRebuildStaysUntilItIsDeclaredDone;
begin
    FUi.RequestMenuRebuild;
    //  Asked twice, done once: asking is not doing.
    AssertTrue('still there', FUi.WorkNow(False) = dwMenuRebuild);
    AssertTrue('still there', FUi.WorkNow(False) = dwMenuRebuild);
    FUi.RebuildDone;
    AssertTrue('gone', FUi.WorkNow(False) = dwNothing);
    //  And a rebuild is wanted again afterwards, which is how the next click is
    //  served rather than being swallowed by the flag never having cleared.
    AssertTrue('and can be asked for again', FUi.RequestMenuRebuild);
end;

procedure TDeferredUiTest.TakingAMessageThatIsNotThereAnswersFalse;
var
    Msg: string;
    Kind: TNoticeKind;
begin
    //  The retry timer can reach the taking code after the async call already
    //  showed the message, and there must be nothing to show twice.
    AssertTrue('nothing to take', not FUi.TakeDialog(Msg, Kind));
    AssertEquals('and no text', '', Msg);
end;

initialization
    RegisterTest('unit', TDeferredUiTest);
end.
