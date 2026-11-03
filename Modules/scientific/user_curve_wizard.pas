// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the two dialogs comes next when a user defines a curve.)

DEFINING A CURVE IS TWO DIALOGS AND FOUR WAYS OUT. The first takes a name and a
formula; the second assigns roles to the parameters the formula turned out to
have. From either the user can go back, and from the first the formula can fail
to parse - which is not a refusal, because the user has to be able to correct a
typo without losing the name they typed.

THAT MAKES IT A LOOP, and it was written as two labels and three `goto`s. The
labels are the reason it had no test: the sequence is decided in the middle of a
routine that opens modal windows, so there was no way to ask what happens after
a cancelled second dialog without opening the first one by hand.

SO THE SEQUENCE IS HERE AND THE WINDOWS ARE NOT. Each function answers one
question - given what the user just did, what happens next - and the caller does
the opening, the parsing and the storing. The same split as table_export, and
for the same reason: every branch of a wizard is reachable in a test and none of
them is reachable through a window.

GOING BACK IS NOT CANCELLING, and the difference is the whole reason both exist.
Going back re-opens the first dialog; cancelling ends the wizard. A wizard that
confused the two would either trap the user in a loop they cannot leave, or
throw away the work of the step they only wanted to revisit.
}
unit user_curve_wizard;

{$MODE Delphi}

interface

type
    { Where the wizard is. wsDone and wsGiveUp are ends, not steps: the loop
      runs while it is on neither. }
    TWizardStep = (wsAskForFormula, wsAskForRoles, wsDone, wsGiveUp);

    { What the first dialog reported. }
    TFormulaAnswer = (
        faAccepted,     //  the user confirmed a name and a formula
        faCancelled);   //  anything else, which in a modal dialog is a refusal

    { What the second dialog reported. }
    TRolesAnswer = (
        raAccepted,     //  the roles are set
        raStartAgain,   //  "back": discard this curve and type another
        raCancelled);   //  the dialog was dismissed

{ After the name-and-formula dialog. AParsed is whether the formula yielded a
  parameter list - the parser reports its own reason to the user, so a formula
  that did not parse sends the wizard round again rather than ending it. }
function AfterFormula(AAnswer: TFormulaAnswer; AParsed: boolean): TWizardStep;

{ After the roles dialog. }
function AfterRoles(AAnswer: TRolesAnswer): TWizardStep;

{ Whether the caller should keep going. Spelled out so the loop condition is
  the same sentence in the dialog and in the tests. }
function WizardIsRunning(AStep: TWizardStep): boolean;

{ Whether a curve was defined. }
function WizardSucceeded(AStep: TWizardStep): boolean;

implementation

function AfterFormula(AAnswer: TFormulaAnswer; AParsed: boolean): TWizardStep;
begin
    if AAnswer = faCancelled then
        Exit(wsGiveUp);
    //  ROUND AGAIN, not out. The parser has already told the user what is wrong
    //  with the formula; ending here would make them retype the name as well,
    //  and a mistyped formula is the ordinary case rather than a refusal.
    if not AParsed then
        Exit(wsAskForFormula);
    Result := wsAskForRoles;
end;

function AfterRoles(AAnswer: TRolesAnswer): TWizardStep;
begin
    case AAnswer of
        raAccepted: Result := wsDone;
        //  BACK TO THE START. The caller deletes the curve type it stored on
        //  the way here first - going back means the curve was not the one the
        //  user wanted, so leaving it behind would fill the list with drafts.
        raStartAgain: Result := wsAskForFormula;
        else Result := wsGiveUp;
    end;
end;

function WizardIsRunning(AStep: TWizardStep): boolean;
begin
    Result := not (AStep in [wsDone, wsGiveUp]);
end;

function WizardSucceeded(AStep: TWizardStep): boolean;
begin
    Result := AStep = wsDone;
end;

end.
