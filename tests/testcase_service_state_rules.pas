// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the compute service's states admit which operation.)

FIVE RULES THAT WERE WRITTEN ABOUT THIRTY TIMES. The service decided all of this
inline at every entry point, and the profile rule alone appeared at twenty
separate raise sites spelled out in full. Thirty copies is not a tidiness
problem: it is why none of the rules could be reached without driving the whole
service, which needs the optimiser - so every one of them was covered, if at
all, only by an integration test, in the unit that carries the largest uncovered
block in the program.

EVERY RULE HERE IS A FUNCTION OF THE STATE AND NOTHING ELSE, so every one can be
stated over all six states at once. That is what these tests do: each walks the
whole enum rather than naming the interesting value, so a seventh state added
later has to be considered rather than falling into whichever answer the
compiler gives it.

THE FOUR ANSWERS ARE NOT INTERCHANGEABLE, and the tests are arranged around the
distinctions rather than around the functions:

  * ABORTING IS NOT REFUSING. A command that finds a calculation running may
    replace it - the caller is told the previous one was cancelled, and the
    command goes ahead. Another refuses instead. Which of the two a command does
    is the command's own choice and the service makes it differently for
    different commands on purpose.

  * A PICK IS STRICTER THAN AN ORDINARY OPERATION by exactly one state: a pick
    that arrived mid-calculation would land on a model being rebuilt underneath
    it, and the user did not ask for the fit to stop - they clicked on a chart.

  * AN ABORT REFUSES EVERY STATE BUT ONE, which is the inverse of every other
    rule here and the one most likely to be written back-to-front.

  * AND OFFERING IS NOT ACCEPTING. The window's rule for whether to offer a fit
    is here beside the service's for whether to accept one, because they differ
    at exactly one state on purpose. That difference is a workflow choice, so
    the tests below state it as a difference rather than asserting one side is
    the other's narrowing.

AND THE TEXT IS PART OF THE RULE. A refusal the user cannot act on is a refusal
that will be reported as a bug, so each one opens with the same recognisable
first line and then says which refusal it is.
}
unit testcase_service_state_rules;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    int_fit_service, service_state_rules;

type
    TServiceStateRulesTest = class(TTestCase)
    private
        { Every state, so a test can walk them without naming them. }
        function AllStates: TStringList;
    published
        //  Aborting a running operation.
        procedure OnlyTheRunningStateHasSomethingToAbort;
        procedure AbortingIsNotRefusing;

        //  Operations that need the profile.
        procedure OnlyTheWaitingStateLacksAProfile;
        procedure EveryOtherStateAdmitsAnOrdinaryOperation;
        procedure TheRefusalNamesWhatIsMissing;

        //  Operations that must not run alongside a calculation.
        procedure ARunningCalculationRefusesThem;
        procedure AndNothingElseDoes;

        //  Accepting picked points.
        procedure APickIsRefusedWithNoProfile;
        procedure APickIsAlsoRefusedWhileCalculating;
        procedure APickIsStricterThanAnOrdinaryOperationByOneState;
        procedure AndAgreesWithItEverywhereElse;

        //  Aborting.
        procedure AnAbortIsRefusedUnlessSomethingIsRunning;
        procedure WhichIsTheInverseOfEveryOtherRuleHere;

        //  Offering a fit, which is not the same as accepting one.
        procedure TheWindowOffersAFitInTheThreeReadyStates;
        procedure TheServiceWouldAcceptOneInEveryStateThatHasAProfile;
        procedure TheyDifferAtTwoStatesOfWhichOneIsIncidental;
        procedure AndThatStateIsTheOneWithAnUnsubtractedBackground;

        //  Reading a result.
        procedure AResultIsRefusedUntilAFitHasFinished;

        //  The text.
        procedure EveryRefusalHasTwoLines;
        procedure EveryRefusalOpensTheSameWay;
        procedure NoTwoRefusalsReadAlike;
        procedure NotBeingRefusedSaysNothingAtAll;

        //  Raising it.
        procedure RefusingRaisesAUserErrorRatherThanAFault;
        procedure NotBeingRefusedRaisesNothing;
        procedure TheRaisedMessageIsTheRefusalText;
    end;

implementation

const
    { Named for the tests that walk them. Every value of TFitServerState; the
      compiler checks the count in AllStatesCoversTheEnum below. }
    States: array[0..5] of TFitServerState = (
        ProfileWaiting, BackNotRemoved, AsyncOperation,
        ReadyForAutoFit, ReadyForFit, Finished);

    Refusals: array[0..3] of TServiceRefusal = (
        rfDataMustBeSet, rfNowCalculating, rfCalcNotStarted, rfFitNotDone);

function TServiceStateRulesTest.AllStates: TStringList;
var
    i: longint;
begin
    Result := TStringList.Create;
    for i := 0 to High(States) do
        Result.Add(FitServerStateName(States[i]));
end;

{ --------------- offering a fit, and accepting one, side by side ------------ }

procedure TServiceStateRulesTest.TheWindowOffersAFitInTheThreeReadyStates;
var
    i: longint;
begin
    //  Walked rather than named, so a seventh state has to be considered: one
    //  that fell into "offered" by accident would put a manual fit in front of
    //  the user in a state nobody had thought about.
    for i := 0 to High(States) do
        AssertEquals(FitServerStateName(States[i]),
            States[i] in [ReadyForFit, ReadyForAutoFit, Finished],
            FitIsOffered(States[i]));
end;

procedure TServiceStateRulesTest.TheServiceWouldAcceptOneInEveryStateThatHasAProfile;
var
    i: longint;
begin
    //  The other half of the pair. A fit entry point refuses on ProfileRefusal
    //  and nothing else - the state check that would have made it stricter is
    //  commented out in the service under the note "Instead of an error, the
    //  data that is needed is created".
    for i := 0 to High(States) do
        AssertEquals(FitServerStateName(States[i]),
            States[i] <> ProfileWaiting,
            ProfileRefusal(States[i]) = rfNone);
end;

procedure TServiceStateRulesTest.TheyDifferAtTwoStatesOfWhichOneIsIncidental;
var
    i, Differ: longint;
begin
    //  COUNTED, so that a change to either side has to account for the other.
    //  TWO, and only one of them is interesting. AsyncOperation is the
    //  incidental one: ProfileRefusal answers rfNone there because a fit
    //  entry point aborts what is running and goes ahead, while the window
    //  keeps the command dark for the length of the operation - that is the
    //  same intent expressed twice, not a disagreement. BackNotRemoved is the
    //  real difference, and the test below is about it alone.
    Differ := 0;
    for i := 0 to High(States) do
        if FitIsOffered(States[i]) <> (ProfileRefusal(States[i]) = rfNone) then
            Inc(Differ);
    AssertEquals('states where offering and accepting disagree', 2, Differ);
end;

procedure TServiceStateRulesTest.AndThatStateIsTheOneWithAnUnsubtractedBackground;
begin
    //  THE WORKFLOW, WRITTEN DOWN. Fitting curves to a profile that still
    //  carries its background is a thing to ask for deliberately, so the window
    //  does not offer the two manual fits here - it offers "do all
    //  automatically", which subtracts and then fits. A REST caller that asks
    //  outright is not stopped, because completing the missing data is what the
    //  service does everywhere else too.
    //
    //  Both halves asserted, because the finding is the DIFFERENCE. Either one
    //  alone would keep passing if the other side quietly changed.
    AssertFalse('the window does not offer a manual fit',
        FitIsOffered(BackNotRemoved));
    AssertTrue('the service would accept one',
        ProfileRefusal(BackNotRemoved) = rfNone);
end;

{ ------------------------- aborting what is running ------------------------- }

procedure TServiceStateRulesTest.OnlyTheRunningStateHasSomethingToAbort;
var
    i: longint;
begin
    for i := 0 to High(States) do
        AssertEquals(FitServerStateName(States[i]),
            States[i] = AsyncOperation,
            MustAbortRunningOperation(States[i]));
end;

procedure TServiceStateRulesTest.AbortingIsNotRefusing;
begin
    //  THE DISTINCTION THE SERVICE MAKES ON PURPOSE. A command that aborts goes
    //  ahead and tells the caller it cancelled the previous calculation; one
    //  that refuses does not run at all. Collapsing the two would either
    //  silently kill a running fit or refuse a command the user is entitled to.
    AssertTrue('a running calculation is aborted',
        MustAbortRunningOperation(AsyncOperation));
    AssertTrue('and the same state also refuses the operations that refuse',
        BusyRefusal(AsyncOperation) = rfNowCalculating);
end;

{ ------------------------ operations needing a profile ---------------------- }

procedure TServiceStateRulesTest.OnlyTheWaitingStateLacksAProfile;
var
    i: longint;
begin
    for i := 0 to High(States) do
        AssertEquals(FitServerStateName(States[i]),
            Ord(States[i] = ProfileWaiting),
            Ord(ProfileRefusal(States[i]) = rfDataMustBeSet));
end;

procedure TServiceStateRulesTest.EveryOtherStateAdmitsAnOrdinaryOperation;
var
    i: longint;
begin
    //  DELIBERATELY PERMISSIVE, and the service says so in a commented-out
    //  guard: rather than refusing when some of what a fit needs is missing, it
    //  completes the missing part itself. A stricter rule here would disable
    //  most of the program immediately after a file was loaded.
    for i := 0 to High(States) do
        if States[i] <> ProfileWaiting then
            AssertTrue(FitServerStateName(States[i]) + ' admits it',
                ProfileRefusal(States[i]) = rfNone);
end;

procedure TServiceStateRulesTest.TheRefusalNamesWhatIsMissing;
begin
    AssertTrue('it says to load data',
        Pos(DataMustBeSet, ServiceRefusalText(rfDataMustBeSet)) > 0);
end;

{ ----------------------- operations refused while busy ---------------------- }

procedure TServiceStateRulesTest.ARunningCalculationRefusesThem;
begin
    AssertTrue('refused', BusyRefusal(AsyncOperation) = rfNowCalculating);
end;

procedure TServiceStateRulesTest.AndNothingElseDoes;
var
    i: longint;
begin
    for i := 0 to High(States) do
        if States[i] <> AsyncOperation then
            AssertTrue(FitServerStateName(States[i]) + ' admits it',
                BusyRefusal(States[i]) = rfNone);
end;

{ ---------------------------- accepting a pick ------------------------------ }

procedure TServiceStateRulesTest.APickIsRefusedWithNoProfile;
begin
    AssertTrue('nothing to pick onto',
        PickRefusal(ProfileWaiting) = rfDataMustBeSet);
end;

procedure TServiceStateRulesTest.APickIsAlsoRefusedWhileCalculating;
begin
    //  The model is being rebuilt underneath it, and the user did not ask for
    //  the calculation to stop - they clicked on a chart. So this refuses where
    //  an ordinary command would abort.
    AssertTrue('refused rather than aborting the calculation',
        PickRefusal(AsyncOperation) = rfNowCalculating);
    AssertTrue('and an ordinary command would abort in the same state',
        MustAbortRunningOperation(AsyncOperation));
end;

procedure TServiceStateRulesTest.APickIsStricterThanAnOrdinaryOperationByOneState;
var
    i, Stricter: longint;
begin
    //  BY EXACTLY ONE, counted rather than asserted state by state: if the two
    //  rules ever diverge anywhere else, this fails without needing to know
    //  where.
    Stricter := 0;
    for i := 0 to High(States) do
        if (ProfileRefusal(States[i]) = rfNone) and
            (PickRefusal(States[i]) <> rfNone) then
            Inc(Stricter);
    AssertEquals('one state refuses a pick and admits an operation',
        1, Stricter);
end;

procedure TServiceStateRulesTest.AndAgreesWithItEverywhereElse;
var
    i: longint;
begin
    for i := 0 to High(States) do
        if States[i] <> AsyncOperation then
            AssertEquals(FitServerStateName(States[i]) + ': the same answer',
                Ord(ProfileRefusal(States[i])),
                Ord(PickRefusal(States[i])));
end;

{ -------------------------------- aborting ---------------------------------- }

procedure TServiceStateRulesTest.AnAbortIsRefusedUnlessSomethingIsRunning;
var
    i: longint;
begin
    for i := 0 to High(States) do
        AssertEquals(FitServerStateName(States[i]),
            Ord(States[i] <> AsyncOperation),
            Ord(AbortRefusal(States[i]) = rfCalcNotStarted));
end;

procedure TServiceStateRulesTest.WhichIsTheInverseOfEveryOtherRuleHere;
var
    i: longint;
begin
    //  THE ONE MOST LIKELY TO BE WRITTEN BACK-TO-FRONT. Every other rule
    //  refuses one state and admits the rest; this admits one and refuses the
    //  rest. Written as the inverse relation so the two cannot drift apart.
    for i := 0 to High(States) do
        AssertEquals(FitServerStateName(States[i]),
            MustAbortRunningOperation(States[i]),
            AbortRefusal(States[i]) = rfNone);
end;

{ ----------------------------- reading a result ----------------------------- }

procedure TServiceStateRulesTest.AResultIsRefusedUntilAFitHasFinished;
begin
    AssertTrue('no fit, no result', ResultRefusal(False) = rfFitNotDone);
    AssertTrue('once it is done, it may be read',
        ResultRefusal(True) = rfNone);
end;

{ --------------------------------- the text --------------------------------- }

procedure TServiceStateRulesTest.EveryRefusalHasTwoLines;
var
    i: longint;
    Text: string;
begin
    //  The first line makes it recognisable as a refusal rather than a fault;
    //  the second is the only part that tells the user what to do. One without
    //  the other is a message that either alarms or does not help.
    for i := 0 to High(Refusals) do
    begin
        Text := ServiceRefusalText(Refusals[i]);
        AssertTrue('refusal ' + IntToStr(i) + ' says something', Text <> '');
        AssertTrue('refusal ' + IntToStr(i) + ' has a second line',
            Pos(REFUSAL_CRLF, Text) > 0);
    end;
end;

procedure TServiceStateRulesTest.EveryRefusalOpensTheSameWay;
var
    i: longint;
begin
    for i := 0 to High(Refusals) do
        AssertEquals('refusal ' + IntToStr(i) + ' opens with the common line',
            1, Pos(InadmissibleServerState, ServiceRefusalText(Refusals[i])));
end;

procedure TServiceStateRulesTest.NoTwoRefusalsReadAlike;
var
    i, j: longint;
begin
    //  Four distinct reasons must produce four distinct messages, or the user
    //  is told "not now" without being told which "not now" it is.
    for i := 0 to High(Refusals) do
        for j := i + 1 to High(Refusals) do
            AssertTrue(Format('refusals %d and %d differ', [i, j]),
                ServiceRefusalText(Refusals[i]) <>
                ServiceRefusalText(Refusals[j]));
end;

procedure TServiceStateRulesTest.NotBeingRefusedSaysNothingAtAll;
begin
    //  Empty, not a cheerful sentence: nothing is shown when nothing is wrong.
    AssertEquals('', '', ServiceRefusalText(rfNone));
end;

{ -------------------------------- raising it -------------------------------- }

procedure TServiceStateRulesTest.RefusingRaisesAUserErrorRatherThanAFault;
var
    i: longint;
    Raised: boolean;
begin
    //  THE CLASS IS THE CONTRACT. The REST layer maps EUserException to 400 and
    //  everything else to 500, so this class is the difference between "your
    //  request was wrong for this state" and "the server broke". Every engine
    //  refusal came back as 500 once; see findings.md.
    for i := 0 to High(Refusals) do
    begin
        Raised := False;
        try
            RefuseIf(Refusals[i]);
        except
            on E: Exception do
                Raised := E.ClassName = 'EUserException';
        end;
        AssertTrue('refusal ' + IntToStr(i) + ' is a user error', Raised);
    end;
end;

procedure TServiceStateRulesTest.NotBeingRefusedRaisesNothing;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RefuseIf(rfNone);
    except
        on Exception do
            Raised := True;
    end;
    AssertFalse('nothing escaped', Raised);
end;

procedure TServiceStateRulesTest.TheRaisedMessageIsTheRefusalText;
var
    Msg: string;
begin
    //  So that the sentence the tests above pin is the sentence the user reads,
    //  rather than a second copy composed at the raise site.
    Msg := '';
    try
        RefuseIf(rfDataMustBeSet);
    except
        on E: Exception do
            Msg := E.Message;
    end;
    AssertEquals('the message is the text',
        ServiceRefusalText(rfDataMustBeSet), Msg);
end;

initialization
    //  Unit tests: five functions of an enumeration. No service, no profile, no
    //  optimiser - which is the entire point of taking them out of one.
    RegisterTest('unit', TServiceStateRulesTest);
end.
