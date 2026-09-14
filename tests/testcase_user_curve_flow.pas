// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Defining a user's own curve type: every way through the two dialogs.)

TWENTY-SEVEN LINES THAT NO TEST COULD REACH. Defining a curve type takes two
modal windows - a name and a formula, then which parameter plays which role - and
the sequence between them was written inline in the class method that opened
them. So the whole of it was unreachable: what happens when a formula will not
parse, what happens when the roles are rejected, what happens to the draft type
either way.

THE ONE THAT MATTERS IS THE DELETION. Rejecting the roles has to remove the draft
BEFORE going back to the formula, because the type is stored the moment the
formula parses. Get it wrong and a curve the user rejected stays in their list
looking like one they made - an extra entry in a settings file, found later and
blamed on the file.

WHAT MADE IT TESTABLE was one field. Four of the five collaborators were already
interfaces; the fifth was a global dialog object whose curve-type field the caller
set before showing it. Behind IUserCurveRolesDlg the sequence names no window and
no widget-set constant, and this fixture wires five doubles to it.

STILL IN THE NOGUI HALF, though nothing here opens a window: the flow reaches
app_settings and the curve-type machinery, which the light plain-FPC suite does
not link. A unit test all the same - no dialog, no file, nothing leaves the
process.
}
unit testcase_user_curve_flow;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    app_settings, int_user_curve_dialogs, user_curve_flow,
    mock_user_curve_dialogs;

type
    TUserCurveFlowTest = class(TTestCase)
    private
        FFormula: TMockFormulaDlg;
        FRoles: TMockRolesDlg;
        FParser: TMockExpressionParser;
        FFactory: TMockCurveTypeFactory;
        FStorage: TMockCurveTypeStorage;
        { Runs the flow over the fixture's five doubles. Scripts are set first. }
        function Run: boolean;
        procedure Script(const AFormula, ARoles: TAnswerScript);
    protected
        procedure TearDown; override;
    published
        //  Straight through.
        procedure AConfirmedFormulaAndConfirmedRolesDefineACurveType;
        procedure TheTypeIsStoredUnderTheNameAndFormulaTheUserGave;
        procedure TheRolesDialogIsShownTheTypeThatWasJustCreated;
        procedure AndTheRolesAreWrittenBackOntoIt;

        //  Abandoning at the first dialog.
        procedure CancellingTheFormulaDefinesNothing;
        procedure AndStoresNothing;
        procedure AndNeverOpensTheSecondDialog;

        //  A formula that will not parse.
        procedure AnUnreadableFormulaAsksAgainRatherThanGivingUp;
        procedure AndStoresNothingOnTheWayRound;
        procedure ACorrectionOnTheSecondAttemptSucceeds;
        procedure TheSecondAttemptIsDefinedFromTheSecondFormula;

        //  Rejecting the roles.
        procedure RejectingTheRolesReturnsToTheFormula;
        procedure TheDraftIsDeletedBeforeGoingBack;
        procedure AndTheStorageIsLeftWithNothingIfTheUserThenCancels;
        procedure TheSecondAttemptGetsItsOwnTypeNotTheRejectedOne;

        //  Abandoning at the second dialog.
        procedure CancellingTheRolesDefinesNothing;
        procedure ButLeavesTheStoredTypeAsItIs;
    end;

implementation

function TUserCurveFlowTest.Run: boolean;
begin
    Result := RunUserCurveFlow(FFormula, FRoles, FParser, FFactory, FStorage);
end;

procedure TUserCurveFlowTest.Script(const AFormula, ARoles: TAnswerScript);
begin
    FFormula := TMockFormulaDlg.Create(AFormula);
    FRoles := TMockRolesDlg.Create(ARoles);
    FParser := TMockExpressionParser.Create;
    FFactory := TMockCurveTypeFactory.Create;
    FStorage := TMockCurveTypeStorage.Create;
end;

procedure TUserCurveFlowTest.TearDown;
begin
    //  -SIcorba: the interfaces above carry no refcount, so every double is
    //  freed as the object it is. The storage double holds references only; the
    //  factory owns what it made.
    FreeAndNil(FStorage);
    FreeAndNil(FFactory);
    FreeAndNil(FParser);
    FreeAndNil(FRoles);
    FreeAndNil(FFormula);
end;

{ ------------------------------- straight through --------------------------- }

procedure TUserCurveFlowTest.AConfirmedFormulaAndConfirmedRolesDefineACurveType;
begin
    Script(Answers(daAccepted), Answers(daAccepted));
    AssertTrue('the flow succeeded', Run);
    AssertEquals('one type was made', 1, FFactory.MadeCount);
    AssertEquals('and it is still stored', 1, FStorage.HeldCount);
end;

procedure TUserCurveFlowTest.TheTypeIsStoredUnderTheNameAndFormulaTheUserGave;
begin
    //  BOTH, and read off the dialog rather than restated: the name is what the
    //  user finds in their curve list and the formula is what gets fitted, and a
    //  flow that passed one twice would produce a type named after its own
    //  expression without failing anything.
    Script(Answers(daAccepted), Answers(daAccepted));
    FFormula.Name_ := 'skewed thing';
    FFormula.Expression := 'A*exp(-(x-x0)*(x-x0)/s)';
    AssertTrue(Run);
    AssertEquals('the name', 'skewed thing', FFactory.Made(0).Name);
    AssertEquals('the formula', 'A*exp(-(x-x0)*(x-x0)/s)',
        FFactory.Made(0).Expression);
end;

procedure TUserCurveFlowTest.TheRolesDialogIsShownTheTypeThatWasJustCreated;
begin
    //  THE SAME OBJECT, not one like it. The roles dialog writes onto what it is
    //  shown, so being handed a different instance - or nothing - means the
    //  user's choices land on something that is never stored.
    Script(Answers(daAccepted), Answers(daAccepted));
    AssertTrue(Run);
    AssertFalse('it was never shown nothing', FRoles.SawNil);
    AssertTrue('it was shown the type the factory made',
        FRoles.LastCurveType = FFactory.Made(0));
end;

procedure TUserCurveFlowTest.AndTheRolesAreWrittenBackOntoIt;
begin
    //  ADD THEN UPDATE, in that order. The type is stored when the formula
    //  parses, so what the roles dialog produces has to be written over it -
    //  a flow that added a second time would leave two entries, and one that
    //  never updated would store a type with no roles assigned.
    Script(Answers(daAccepted), Answers(daAccepted));
    AssertTrue(Run);
    //  THE NAMES ARE IN THE SEQUENCE, which is the better assertion: it pins
    //  that the update names the SAME type as the add, and not merely that both
    //  happened.
    AssertEquals('added once then updated once, on the same type',
        'Add(my curve);Update(my curve)', FStorage.Log.Sequence);
end;

{ -------------------------- abandoning the first dialog --------------------- }

procedure TUserCurveFlowTest.CancellingTheFormulaDefinesNothing;
begin
    Script(Answers(daCancelled), Answers(daAccepted));
    AssertFalse('the flow failed', Run);
    AssertEquals('nothing was made', 0, FFactory.MadeCount);
end;

procedure TUserCurveFlowTest.AndStoresNothing;
begin
    Script(Answers(daCancelled), Answers(daAccepted));
    Run;
    AssertEquals('the storage was never touched', '', FStorage.Log.Sequence);
end;

procedure TUserCurveFlowTest.AndNeverOpensTheSecondDialog;
begin
    //  A user who cancels the first window must not then be asked about the
    //  roles of a type that does not exist.
    Script(Answers(daCancelled), Answers(daAccepted));
    Run;
    AssertEquals('the roles dialog was not shown', 0, FRoles.Asked);
    AssertEquals('and the formula was not parsed', 0, FParser.Seen.Count);
end;

{ ------------------------ a formula that will not parse --------------------- }

procedure TUserCurveFlowTest.AnUnreadableFormulaAsksAgainRatherThanGivingUp;
begin
    //  A TYPO COSTS A CORRECTION, NOT THE DEFINITION. The parser shows its own
    //  explanation and answers nothing; the flow goes round rather than out, so
    //  the user's name and their other text are still in the window.
    Script(Answers(daAccepted, daCancelled), Answers(daAccepted));
    FParser.Refuse(FFormula.Expression);
    AssertFalse('cancelled on the second showing', Run);
    AssertEquals('the formula dialog was shown twice', 2, FFormula.Asked);
    AssertEquals('and never a third time', 0, FFormula.Overruns);
end;

procedure TUserCurveFlowTest.AndStoresNothingOnTheWayRound;
begin
    //  NOTHING IS STORED BEFORE THE FORMULA PARSES, which is what makes the
    //  retry harmless. A flow that created the type first would leave a draft
    //  with an unreadable formula in the user's list every time they mistyped.
    Script(Answers(daAccepted, daCancelled), Answers(daAccepted));
    FParser.Refuse(FFormula.Expression);
    Run;
    AssertEquals('nothing was made', 0, FFactory.MadeCount);
    AssertEquals('and nothing stored', '', FStorage.Log.Sequence);
    AssertEquals('the roles were never asked', 0, FRoles.Asked);
end;

procedure TUserCurveFlowTest.ACorrectionOnTheSecondAttemptSucceeds;
begin
    //  The user mistypes, is sent back, and corrects it - which is what the real
    //  dialog supports by keeping its text box.
    Script(Answers(daAccepted, daAccepted), Answers(daAccepted));
    FParser.Refuse('bad(');
    FFormula.Types('first try', 'bad(');
    FFormula.Types('second try', 'A*exp(-x*x)');
    AssertTrue('the corrected formula was accepted', Run);
    AssertEquals('one type, from the corrected attempt', 1,
        FFactory.MadeCount);
    AssertEquals('and it is stored', 1, FStorage.HeldCount);
end;

procedure TUserCurveFlowTest.TheSecondAttemptIsDefinedFromTheSecondFormula;
begin
    //  RE-READ AFTER EACH SHOWING. A flow that read the text once and reused it
    //  would define the corrected attempt from the formula that failed - and
    //  since it stores whatever parsed, the user would end up with a type they
    //  did not write, under a name they did not choose, with nothing reported.
    Script(Answers(daAccepted, daAccepted), Answers(daAccepted));
    FParser.Refuse('bad(');
    FFormula.Types('first try', 'bad(');
    FFormula.Types('second try', 'A*exp(-x*x)');
    AssertTrue(Run);
    AssertEquals('both formulas reached the parser, in order',
        'bad(' + LineEnding + 'A*exp(-x*x)', Trim(FParser.Seen.Text));
    AssertEquals('the type carries the second formula', 'A*exp(-x*x)',
        FFactory.Made(0).Expression);
    AssertEquals('and the second name', 'second try', FFactory.Made(0).Name);
end;

{ ---------------------------- rejecting the roles --------------------------- }

procedure TUserCurveFlowTest.RejectingTheRolesReturnsToTheFormula;
begin
    //  BACK A STEP, not out. The dialog offers this as the way to change the
    //  formula, so the answer is "start again" rather than "cancel" - two ways
    //  of saying no that a boolean would have made one.
    Script(Answers(daAccepted, daCancelled), Answers(daStartAgain));
    AssertFalse('cancelled at the second showing of the formula', Run);
    AssertEquals('the formula dialog was shown twice', 2, FFormula.Asked);
end;

procedure TUserCurveFlowTest.TheDraftIsDeletedBeforeGoingBack;
begin
    //  THE RULE THIS FIXTURE EXISTS FOR. The type was stored the moment the
    //  formula parsed, so rejecting the roles has to remove it - and remove it
    //  before the next attempt, or the second attempt's Add would sit behind a
    //  Delete that never came and the list would keep the rejected one.
    Script(Answers(daAccepted, daCancelled), Answers(daStartAgain));
    Run;
    AssertEquals('added, then deleted, and the same type both times',
        'Add(my curve);Delete(my curve)', FStorage.Log.Sequence);
end;

procedure TUserCurveFlowTest.AndTheStorageIsLeftWithNothingIfTheUserThenCancels;
begin
    //  THE USER'S LIST IS AS THEY FOUND IT. This is the observable form of the
    //  rule above: a call log can be satisfied by a Delete of the wrong object,
    //  and what the user sees is what is left.
    Script(Answers(daAccepted, daCancelled), Answers(daStartAgain));
    Run;
    AssertEquals('nothing is left stored', 0, FStorage.HeldCount);
end;

procedure TUserCurveFlowTest.TheSecondAttemptGetsItsOwnTypeNotTheRejectedOne;
begin
    //  A FRESH TYPE. The rejected one is deleted, so carrying its reference into
    //  the next attempt would have the roles dialog shown a type the storage no
    //  longer holds, and the final Update would write onto nothing.
    Script(Answers(daAccepted, daAccepted), Answers(daStartAgain, daAccepted));
    AssertTrue('the second attempt succeeded', Run);
    AssertEquals('two types were made', 2, FFactory.MadeCount);
    AssertTrue('the roles dialog last saw the second one',
        FRoles.LastCurveType = FFactory.Made(1));
    AssertFalse('the rejected one is not stored',
        FStorage.Holds(FFactory.Made(0)));
    AssertTrue('the accepted one is', FStorage.Holds(FFactory.Made(1)));
end;

{ ------------------------- abandoning the second dialog --------------------- }

procedure TUserCurveFlowTest.CancellingTheRolesDefinesNothing;
begin
    Script(Answers(daAccepted), Answers(daCancelled));
    AssertFalse('the flow failed', Run);
    AssertEquals('the formula dialog was shown once', 1, FFormula.Asked);
end;

procedure TUserCurveFlowTest.ButLeavesTheStoredTypeAsItIs;
begin
    //  CHARACTERISED, NOT ENDORSED. The type is stored when the formula parses,
    //  and cancelling the roles does not remove it - unlike rejecting them,
    //  which does. So a user who abandons the second window finds a type in
    //  their list with no roles assigned.
    //
    //  Whether that is right is a question about what a user expects to find,
    //  not about this sequence, and it is the behaviour the program has always
    //  had. Pinned here so that changing it is a decision rather than an
    //  accident.
    Script(Answers(daAccepted), Answers(daCancelled));
    Run;
    AssertEquals('added and not deleted', 'Add(my curve)',
        FStorage.Log.Sequence);
    AssertEquals('the type is still stored', 1, FStorage.HeldCount);
end;

initialization
    RegisterTest('unit', TUserCurveFlowTest);
end.
