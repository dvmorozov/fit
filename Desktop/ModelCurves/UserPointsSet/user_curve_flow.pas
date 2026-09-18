// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Defining a user's own curve type: the sequence, with no windows in it.)

WHAT THIS IS THE REST OF. `user_curve_wizard` decides which step comes next;
this does what each answer implies - parse, create, store, delete - and it is
the half that used to be untestable, because it was written inline in a class
method that opened two modal windows. Twenty-seven lines, none of them reachable
by any test, containing every way a half-made curve type gets cleaned up.

FIVE COLLABORATORS, ALL INTERFACES. Four of them already were; the fifth was a
global dialog object with a public field, set from the caller. That one field was
the whole reason this could not be tested, and it is `IUserCurveRolesDlg` now.

THE RULE MOST WORTH A TEST IS THE DELETION. Rejecting the roles has to remove the
draft type BEFORE going back to the formula, or a curve the user rejected stays
in their list looking like one they made. Nothing about that is visible from
outside: the wrong version leaves an extra entry in a settings file, discovered
later and blamed on the file.
}
unit user_curve_flow;

{$mode objfpc}{$H+}

interface

uses
    app_settings, int_curve_type_parameters_factory, int_curve_type_storage,
    int_expression_parser, int_user_curve_dialogs;

{ Runs the whole definition sequence; answers True when a curve type was defined
  and stored, False when the user abandoned it.

  EVERY COLLABORATOR IS A PARAMETER, so the production caller wires the real
  dialogs and a test wires recording ones. It owns nothing it is given, and the
  curve type it creates is owned by the storage it hands it to - which is also
  what makes the delete-before-going-back step the storage's business rather
  than a Free here. }
function RunUserCurveFlow(AFormula: IUserCurveFormulaDlg;
    ARoles: IUserCurveRolesDlg; AParser: IExpressionParser;
    AFactory: ICurveTypeParametersFactory;
    AStorage: ICurveTypeStorage): boolean;

implementation

uses
    persistent_curve_parameters, user_curve_wizard;

function RunUserCurveFlow(AFormula: IUserCurveFormulaDlg;
    ARoles: IUserCurveRolesDlg; AParser: IExpressionParser;
    AFactory: ICurveTypeParametersFactory;
    AStorage: ICurveTypeStorage): boolean;
var
    ct: Curve_type;
    params: Curve_parameters;
    Step: TWizardStep;
begin
    ct := nil;
    Step := wsAskForFormula;
    while WizardIsRunning(Step) do
    begin
        if Step = wsAskForFormula then
        begin
            //  CLEARED ON EVERY PASS. Coming back here means the previous
            //  draft was deleted, and a stale reference would be handed to the
            //  roles dialog or updated in the storage on the next time round.
            ct := nil;
            if AFormula.Ask <> daAccepted then
                Step := AfterFormula(faCancelled, False)
            else
            begin
                //  VALIDATED BEFORE ANYTHING IS STORED. The parser shows its own
                //  explanation and answers nil for a formula it cannot read,
                //  which sends the sequence round again rather than out - so a
                //  typo costs a correction, not the whole definition.
                params := AParser.ParseExpression(AFormula.GetExpression);
                Step := AfterFormula(faAccepted, params <> nil);
                if Step = wsAskForRoles then
                begin
                    ct := AFactory.CreateUserCurveType(AFormula.GetName,
                        AFormula.GetExpression, params);
                    AStorage.AddCurveType(ct);
                end;
            end;
        end
        else
            case ARoles.Ask(ct) of
                daAccepted:
                begin
                    //  Rewrites the roles the user chose onto the type that is
                    //  already stored.
                    AStorage.UpdateCurveType(ct);
                    Step := AfterRoles(raAccepted);
                end;
                daStartAgain:
                begin
                    //  DELETED BEFORE GOING BACK, so a curve the user rejected
                    //  does not stay in the list as a draft. Going back is not
                    //  cancelling: the type was added the moment the formula
                    //  parsed, and the user is still in the middle of defining
                    //  one.
                    AStorage.DeleteCurveType(ct);
                    Step := AfterRoles(raStartAgain);
                end;
                else
                    //  CANCELLED LEAVES THE DRAFT, which is the existing
                    //  behaviour and is characterised rather than changed: the
                    //  type was stored when the formula parsed, and whether
                    //  abandoning the roles should also remove it is a question
                    //  about what the user expects to find in their list, not a
                    //  question about this sequence.
                    Step := AfterRoles(raCancelled);
            end;
    end;

    Result := WizardSucceeded(Step);
end;

end.
