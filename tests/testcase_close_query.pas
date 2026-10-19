// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What closing the window does about work the user has not saved.)

THE LAST THING THE PROGRAM DOES FOR THE USER, and it lived as two copies of the
same thirty lines inside an LCL close handler, where the only way to run it was to
close the window with unsaved changes in one of two particular tables.

The case worth the whole fixture is the failed save: the user chose "save", the
save did not happen, and closing anyway would destroy exactly the work they asked
to keep. The others are cheap to state and each of them is a way this could be
got wrong quietly - closing on Cancel, asking about a table nobody touched,
clearing the modified flag on a save that failed.
}
unit testcase_close_query;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, close_query;

type
    TCloseQueryTest = class(TTestCase)
    published
        procedure AnUnmodifiedDocumentIsNotAskedAbout;
        procedure AndItsAnswerIsIgnoredEvenIfOneIsSupplied;
        procedure SayingYesSavesFirst;
        procedure SayingNoClosesWithoutSaving;
        procedure SayingNoLeavesTheDocumentModified;
        procedure CancelKeepsTheWindowOpen;
        procedure AnUnknownAnswerIsTreatedAsCancelRatherThanConsent;
        procedure ASuccessfulSaveLetsTheCloseGoOn;
        procedure AFailedSaveStopsTheClose;
        procedure OnlyASuccessfulSaveClearsTheModifiedFlag;
        procedure TheQuestionNamesTheDocumentAndAsksOneThing;
        procedure EveryAnswerLeadsSomewhere;
    end;

implementation

procedure TCloseQueryTest.AnUnmodifiedDocumentIsNotAskedAbout;
begin
    //  Being asked to save something you did not change is how people learn to
    //  dismiss the question without reading it.
    AssertTrue('carry on', ActionForDocument(False, saYes) = caCarryOn);
end;

procedure TCloseQueryTest.AndItsAnswerIsIgnoredEvenIfOneIsSupplied;
begin
    //  There was no dialog, so whatever is in the answer variable is left over
    //  from somewhere else and must not be able to stop the close.
    AssertTrue('cancel from nowhere cannot block it',
        ActionForDocument(False, saCancel) = caCarryOn);
end;

procedure TCloseQueryTest.SayingYesSavesFirst;
begin
    AssertTrue('save first', ActionForDocument(True, saYes) = caSaveFirst);
end;

procedure TCloseQueryTest.SayingNoClosesWithoutSaving;
begin
    AssertTrue('carry on', ActionForDocument(True, saNo) = caCarryOn);
end;

procedure TCloseQueryTest.SayingNoLeavesTheDocumentModified;
begin
    //  Nothing was saved, so nothing about the document changed. The flag is a
    //  fact about the file, not a record of having been asked.
    AssertTrue('still modified', StillModifiedAfterSaving(False));
end;

procedure TCloseQueryTest.CancelKeepsTheWindowOpen;
begin
    //  Cancel means cancel the CLOSE, not cancel the save.
    AssertTrue('stay', ActionForDocument(True, saCancel) = caStayAndShow);
end;

procedure TCloseQueryTest.AnUnknownAnswerIsTreatedAsCancelRatherThanConsent;
var
    Bogus: TSaveAnswer;
begin
    //  Reached by casting past the end of the enum, which is what a dialog
    //  returning something new would amount to. The safe reading of an answer
    //  this cannot understand is not to throw the user's work away.
    Bogus := TSaveAnswer(Ord(High(TSaveAnswer)) + 1);
    AssertTrue('stay, not close',
        ActionForDocument(True, Bogus) = caStayAndShow);
end;

procedure TCloseQueryTest.ASuccessfulSaveLetsTheCloseGoOn;
begin
    AssertTrue('carry on', ActionAfterSaving(True) = caCarryOn);
end;

procedure TCloseQueryTest.AFailedSaveStopsTheClose;
begin
    //  THE CASE THE WHOLE UNIT IS FOR. The user asked for the work to be kept,
    //  it was not kept, and closing now would lose it.
    AssertTrue('stay and show which one',
        ActionAfterSaving(False) = caStayAndShow);
end;

procedure TCloseQueryTest.OnlyASuccessfulSaveClearsTheModifiedFlag;
begin
    AssertTrue('cleared', not StillModifiedAfterSaving(True));
    AssertTrue('not cleared', StillModifiedAfterSaving(False));
end;

procedure TCloseQueryTest.TheQuestionNamesTheDocumentAndAsksOneThing;
var
    Q: string;
begin
    Q := SaveQuestion('Model parameters');
    AssertTrue('it names what changed', Pos('Model parameters', Q) > 0);
    AssertTrue('it says what happened', Pos('modified', Q) > 0);
    AssertTrue('and asks', Pos('Save?', Q) > 0);
    //  One question, not two: a dialog that asks two things has no single
    //  answer, and this one is answered with three buttons.
    AssertEquals('one question mark', 1,
        Length(Q) - Length(StringReplace(Q, '?', '', [rfReplaceAll])));
end;

procedure TCloseQueryTest.EveryAnswerLeadsSomewhere;
var
    Answer: TSaveAnswer;
begin
    //  A sweep, so an answer added to the enum and forgotten in the case
    //  statement cannot end up with whatever the record happened to hold. Every
    //  answer must map to one of the three actions, and only "yes" may ask for a
    //  save.
    for Answer := Low(TSaveAnswer) to High(TSaveAnswer) do
        if ActionForDocument(True, Answer) = caSaveFirst then
            AssertTrue('only yes saves', Answer = saYes)
        else
            AssertTrue('and everything else either closes or stays',
                (ActionForDocument(True, Answer) = caCarryOn) or
                (ActionForDocument(True, Answer) = caStayAndShow));
end;

initialization
    RegisterTest('unit', TCloseQueryTest);
end.
