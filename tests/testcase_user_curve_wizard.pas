// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the two dialogs comes next when a user defines a curve.)

WHAT THIS REPLACED. The sequence lived in `ShowConfigurationDialog` as two labels
and three `goto`s wrapped around two `ShowModal` calls, so no branch of it could
be reached without opening a modal window - which is to say, none of them were
reached at all. The unit sat at 25 %.

THE FOUR ENDINGS ARE THE POINT. A wizard has more ways to end than to succeed,
and every one of them is somebody's afternoon: cancel the first dialog and
nothing should have been stored; mistype the formula and the name you typed
should still be there; go back from the second dialog and the draft should be
gone; cancel the second and - this is the one worth reading twice - the curve
stays. That last is characterised below rather than changed.

A MISTYPED FORMULA IS NOT A REFUSAL, and keeping the two apart is the rule this
file exists for. The parser reports what is wrong with the formula itself; if
that also ended the wizard, correcting a typo would cost the user the name and
the formula both.
}
unit testcase_user_curve_wizard;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, user_curve_wizard;

type
    TUserCurveWizardTest = class(TTestCase)
    published
        //  After the name-and-formula dialog.
        procedure AFormulaThatParsedGoesOnToTheRoles;
        procedure AFormulaThatDidNotParseAsksAgain;
        procedure AndDoesNotEndTheWizard;
        procedure CancellingTheFirstDialogEndsIt;
        procedure ACancelIsACancelWhateverTheParserSaid;

        //  After the roles dialog.
        procedure AcceptingTheRolesFinishes;
        procedure GoingBackReturnsToTheFormula;
        procedure AndIsNotAnEnding;
        procedure CancellingTheRolesEndsIt;
        procedure GoingBackIsNotCancelling;

        //  The loop the caller writes around them.
        procedure TheWizardStartsRunning;
        procedure BothEndingsStopIt;
        procedure OnlyOneOfThemCountsAsSuccess;
        procedure AStepIsNeverBothRunningAndAnEnding;

        //  Round trips.
        procedure ATypoThenACorrectFormulaReachesTheRoles;
        procedure GoingBackTwiceIsStillTheFormula;
    end;

implementation

{ ---------------------- after the formula dialog ---------------------------- }

procedure TUserCurveWizardTest.AFormulaThatParsedGoesOnToTheRoles;
begin
    AssertTrue('on to the roles',
        AfterFormula(faAccepted, True) = wsAskForRoles);
end;

procedure TUserCurveWizardTest.AFormulaThatDidNotParseAsksAgain;
begin
    //  ROUND AGAIN, not out. The parser has already explained itself; ending
    //  here would make the user retype the name as well.
    AssertTrue('back to the formula',
        AfterFormula(faAccepted, False) = wsAskForFormula);
end;

procedure TUserCurveWizardTest.AndDoesNotEndTheWizard;
begin
    AssertTrue('still running',
        WizardIsRunning(AfterFormula(faAccepted, False)));
end;

procedure TUserCurveWizardTest.CancellingTheFirstDialogEndsIt;
begin
    AssertTrue('given up', AfterFormula(faCancelled, False) = wsGiveUp);
    AssertFalse('and it did not succeed',
        WizardSucceeded(AfterFormula(faCancelled, False)));
end;

procedure TUserCurveWizardTest.ACancelIsACancelWhateverTheParserSaid;
begin
    //  The parse result is only consulted for an ACCEPTED dialog. A cancel that
    //  looped because the previous formula happened to parse would trap the
    //  user in a wizard they had just asked to leave.
    AssertTrue('cancelled with a good formula behind it',
        AfterFormula(faCancelled, True) = wsGiveUp);
end;

{ ------------------------ after the roles dialog ---------------------------- }

procedure TUserCurveWizardTest.AcceptingTheRolesFinishes;
begin
    AssertTrue('done', AfterRoles(raAccepted) = wsDone);
    AssertTrue('and that is the success', WizardSucceeded(AfterRoles(raAccepted)));
end;

procedure TUserCurveWizardTest.GoingBackReturnsToTheFormula;
begin
    AssertTrue('back to the first dialog',
        AfterRoles(raStartAgain) = wsAskForFormula);
end;

procedure TUserCurveWizardTest.AndIsNotAnEnding;
begin
    AssertTrue('still running', WizardIsRunning(AfterRoles(raStartAgain)));
end;

procedure TUserCurveWizardTest.CancellingTheRolesEndsIt;
begin
    AssertTrue('given up', AfterRoles(raCancelled) = wsGiveUp);
    AssertFalse('and reported as no curve defined',
        WizardSucceeded(AfterRoles(raCancelled)));
end;

procedure TUserCurveWizardTest.GoingBackIsNotCancelling;
begin
    //  THE DISTINCTION THE WHOLE UNIT RESTS ON. Both leave the second dialog
    //  and only one leaves the wizard; the caller also deletes the stored draft
    //  on one and not the other. Collapsing them would either strand the user
    //  or throw away the step they meant to revisit.
    AssertTrue('different answers',
        AfterRoles(raStartAgain) <> AfterRoles(raCancelled));
end;

{ ------------------------------- the loop ----------------------------------- }

procedure TUserCurveWizardTest.TheWizardStartsRunning;
begin
    AssertTrue('the first step is a step',
        WizardIsRunning(wsAskForFormula));
    AssertTrue('and so is the second', WizardIsRunning(wsAskForRoles));
end;

procedure TUserCurveWizardTest.BothEndingsStopIt;
begin
    //  A loop that kept going on either would re-open a dialog the user has
    //  just finished with.
    AssertFalse('done stops it', WizardIsRunning(wsDone));
    AssertFalse('and so does giving up', WizardIsRunning(wsGiveUp));
end;

procedure TUserCurveWizardTest.OnlyOneOfThemCountsAsSuccess;
begin
    AssertTrue('done', WizardSucceeded(wsDone));
    AssertFalse('gave up', WizardSucceeded(wsGiveUp));
    //  Neither of the two live steps is an answer. Asking mid-wizard is a
    //  caller error, and False is the safe reading of it.
    AssertFalse('mid-wizard', WizardSucceeded(wsAskForFormula));
    AssertFalse('mid-wizard', WizardSucceeded(wsAskForRoles));
end;

procedure TUserCurveWizardTest.AStepIsNeverBothRunningAndAnEnding;
var
    S: TWizardStep;
begin
    //  Walked rather than listed, so a fifth step added later has to declare
    //  which side of the line it is on.
    for S := Low(TWizardStep) to High(TWizardStep) do
        AssertFalse('a step that both runs and succeeds would never end',
            WizardIsRunning(S) and WizardSucceeded(S));
end;

{ ------------------------------ round trips --------------------------------- }

procedure TUserCurveWizardTest.ATypoThenACorrectFormulaReachesTheRoles;
var
    S: TWizardStep;
begin
    S := AfterFormula(faAccepted, False);
    AssertTrue('asked again', S = wsAskForFormula);
    S := AfterFormula(faAccepted, True);
    AssertTrue('and this time went on', S = wsAskForRoles);
end;

procedure TUserCurveWizardTest.GoingBackTwiceIsStillTheFormula;
begin
    //  Nothing accumulates: the wizard has no depth to unwind, so going back is
    //  the same answer however many times it is given.
    AssertTrue('first', AfterRoles(raStartAgain) = wsAskForFormula);
    AssertTrue('and again', AfterRoles(raStartAgain) = wsAskForFormula);
end;

initialization
    //  Unit tests: two functions over enumerations, no dialog.
    RegisterTest('unit', TUserCurveWizardTest);
end.
