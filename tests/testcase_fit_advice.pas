// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for what a fit will ACTUALLY do, and for how that is explained.

  These matter more than they look. The engine's corrections are all sound, but
  every one of them means the user selected one thing and a different thing ran.
  The only defence against that reading as a bug is an explanation that is
  present, correct, and specific - so the explanations are asserted here, not
  just the decisions.

  The decision table is walked EXHAUSTIVELY (four booleans x every loss), because
  the interesting failures are combinations nobody thought about: two reasons to
  fall back at once, or an objective substituted into one the selected engine
  then cannot minimise. }
unit testcase_fit_advice;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry,
  fit_advice, fit_loss, loss_compatibility, sample_coverage, fit_statistics;
type
  { WHAT FIT > AUTOMATICALLY DID TO THE FIT INTERVALS. It replaces them with
    its own, and a sample the user's covered and its own do not is no longer
    fitted - which made one sample no curve covered vanish from the figure
    without a word, and the warning about it with it. }
  TReplacedIntervalsAdviceTest = class(TTestCase)
  published
    procedure WithNoIntervalsBeforeTheRunThereIsNothingToSay;
    procedure TheSameIntervalsAreNothingToSay;
    procedure ReplacedIntervalsAreNamedBeforeAndAfter;
    procedure ASampleNoLongerFittedIsNamed;
    procedure SeveralAreCountedAndPlaced;
    procedure WhenNothingIsLeftOutItDoesNotSaySo;
    procedure ItSaysHowToFitThemAgain;
  end;

  { WHAT THE WINDOW SAYS IN A DIALOG WHEN A RUN ENDS: the run's own note and
    the samples no curve covers, each once for what it says. }
  TEndOfRunNoticeTest = class(TTestCase)
  published
    procedure TheRunsNoteIsShown;
    procedure AndNotAgainForTheSameNote;
    procedure TheNoteAndTheUncoveredSamplesAreShownTogether;
    procedure AnUnattendedRunShowsNothing;
  end;

  { ASKED BEFORE THE FIT, while the user can still correct it: after it, the
    run has been spent on a figure one sample dominates. }
  TBeforeFitQuestionTest = class(TTestCase)
  published
    procedure AModelLeavingSamplesUncoveredIsAskedAbout;
    procedure TheQuestionSaysWhatNoDoes;
    procedure ACoveredModelIsNotAskedAbout;
    procedure TheSameStretchesAreNotAskedTwice;
    procedure AndTheEndOfTheFitDoesNotSayItAgain;
    procedure AnUnattendedRunIsNotAsked;
  end;

  { THE SAMPLES NO CURVE COVERS. Not an override - the fit does what was asked
    - but a figure the user can notice, a model scoring 70 times worse over an
    interval one sample longer, so it explains itself (AGENTS, 7). }
  TUncoveredSamplesAdviceTest = class(TTestCase)
  published
    procedure WithNoneThereIsNothingToSay;
    procedure OneSampleIsNamedWithWhereItIs;
    procedure ItSaysTheModelIsZeroThereAndStillCounted;
    procedure ItSaysHowMuchOfTheDifferenceTheyMake;
    procedure ATinyShareIsNotQuoted;
    procedure ItOffersBothWaysForward;
    procedure AStretchIsNamedByItsEnds;
    procedure ALongListIsCutAndSaysHowMuchWasLeftOut;
    procedure TheStatusLineIsEmptyWithNone;
    procedure TheStatusLineSaysHowManyAndWhereToRead;
    //  THE DIALOG WHEN A RUN ENDS: said once for the same stretches, not after
    //  every fit, and to nobody when nobody is there.
    procedure AFitLeavingSamplesUncoveredSaysSoInADialog;
    procedure TheSameStretchesAreNotSaidAgainWhenOnlyTheShareMoves;
    procedure OtherStretchesAreSaidAfresh;
    procedure CoveredAndThenUncoveredAgainIsSaidAgain;
    procedure AnUnattendedRunSaysNothingAndForgetsNothing;
  end;

  TFitAdviceTest = class(TTestCase)
  published
    procedure APlainPeakFitOnTheNativeEngineHasNothingToExplain;
    procedure ASelfNormalisingLossIsSubstitutedAndExplained;
    procedure AFormulaLessCurveFallsBackAndSaysWhy;
    procedure ANonLeastSquaresLossFallsBackAndNamesTheAlternatives;
    procedure BothReasonsToFallBackAreReportedNotJustTheFirst;
    procedure TheSubstitutedLossIsItselfAlwaysUsable;
    procedure TheEffectiveLossIsNeverLeftUnknown;
    procedure CurveScalingIsReportedButNeverRaisedAsAnAlert;
    procedure TheSummaryAlwaysDescribesWhatWillHappen;
    procedure AnythingOverriddenIsAlwaysExplained;
    procedure NothingOverriddenMeansNothingToExplain;

    //  MOVING A MARKUP POINT AFTER A FIT. A separate decision in this unit, and
    //  the only one here that REFUSES rather than substituting - see the group.
    procedure BeforeAnyFitAMarkupPointMovesFreely;
    procedure AfterAFitTheMoveIsRefused;
    procedure ARefusedMoveSaysTheMoveDidNotHappen;
    procedure AndWhyTheWholeModelWouldBeRebuilt;
    procedure AndOffersBothWaysForward;
    procedure AnAllowedMoveCarriesNoText;
    procedure TheRefusalAvoidsTheWordSeed;

    //  WHETHER TO SAY IT OUT LOUD THIS TIME. The advice is recomputed on every
    //  change of loss, minimizer, curve type and scaling flag, so the question
    //  of when to put a dialog in front of the user is its own rule - and it was
    //  three conditions and an else-if in a form method.
    procedure AdviceWithNothingToExplainIsNotAnnounced;
    procedure AdviceThatNeedsAttentionIsAnnouncedOnce;
    procedure AndNotAgainForTheSameSelection;
    procedure ButAgainAfterLeavingAndComingBack;
    procedure TheWindowsOneMemoryIsNotSaidTwice;
    procedure StartUpDoesNotAnnounceAnything;
    procedure AndStartUpDoesNotDisturbWhatWasRemembered;
  end;

  { THE BACKGROUND, TWO WAYS, AND NEVER BOTH. A background curve in the model
    and Enable Variation both put a background under the peaks; together they
    count it twice. These rules say which may be added while the other is
    there, what the automatic run does about a model that has neither, and
    which curve types may play which part. Walked exhaustively: two booleans. }
  TBackgroundAdviceTest = class(TTestCase)
  published
    procedure WithNeitherBothMayBeAddedAndTheRunAddsACurve;
    procedure ACurveInTheModelRefusesVariationAndSaysWhy;
    procedure VariationOnRefusesACurveAndSaysWhy;
    procedure WithVariationOnTheRunAddsNoCurveAndSaysSo;
    procedure WithACurveTheRunAddsNothingAndSaysNothing;
    procedure BothAtOnceKeepsTheCurveAndDropsTheVariation;
    procedure EveryRefusalIsExplained;
    procedure OnlyABackgroundShapeMayBeTheBackground;
    procedure APeakShapeIsRefusedAsTheBackgroundByName;
    procedure AShapeNeedingPositiveXIsRefusedOnDataThatReachesZero;
    procedure AndAllowedOnDataThatStaysPositive;
    procedure ABackgroundShapeIsRefusedAsThePeakTypeByName;
    procedure APeakShapeIsAllowedAsThePeakType;
    procedure VariationIsOfferedWhereItMayBeSwitchedOn;
    procedure AndAlwaysWhereItIsOnSoItCanBeSwitchedOff;

    //  What Model > Background > Vary Background says when hovered.
    procedure AnAllowedVariationKeepsItsDesignedHint;
    procedure ARefusedOneSaysWhy;
  end;

  { ONE MODEL, ONE MODULE (fit-performance.md, stage 6). A module covers one
    class of fitting tasks and is fully responsible for it, so curves of two
    modules in one model fit nothing either understands. Which types may join a
    model, and whether its type takes a background, walked over every input. }
  TModelModuleAdviceTest = class(TTestCase)
  published
    procedure AnEmptyModelTakesATypeOfAnyModule;
    procedure AModelWithContentTakesTypesOfItsOwnModule;
    procedure AndRefusesAnotherModulesNamingBothAndTheWayOut;
    procedure OwnersAreComparedWithoutRegardToCase;
    procedure AModelWhoseTypeTakesNoBackgroundRefusesOneAndSaysWhy;
    procedure AnyOtherTakesOne;
    procedure AMixedProjectIsToldWhyAndHowToMakeItOneModules;
    //  Fit intervals > Auto (stage 7).
    procedure AModulesProposalIsTakenWhateverTheType;
    procedure WithoutOneTheFrameworksTypesAreSearchedForPeaks;
    procedure AndAModulesTypesAreRefusedSayingWhatToDo;
    procedure AnEmptyModelIsSearchedForPeaksWhateverTheType;
  end;

  { A VALUE TYPED INTO THE TABLE THAT THE MODEL CANNOT HOLD AS TYPED. Every
    parameter keeps to its own range - a width no wider than its fit interval,
    a mixing fraction in [0, 1], an amplitude or a background level not below
    zero, a position between its neighbouring samples - and a background is
    lifted until it is not below zero. The value is then held at the nearest
    one allowed, which the user sees in the redrawn table; this says why. }
  THeldParameterAdviceTest = class(TTestCase)
  published
    procedure AValueHeldAsTypedNeedsNoWord;
    procedure ARoundingDifferenceIsNotAHold;
    procedure AHeldValueNamesTheParameterAndTheLimits;
    procedure AndPointsToWhereTheLimitsAreExplained;
    procedure AValueHeldAtZeroFromAboveIsAHoldToo;
    procedure AProjectHeldAsSavedNeedsNoWord;
    procedure AProjectHeldOtherwiseNamesTheValuesAndTheLimits;
  end;

implementation

function Advise(ALoss: longint; AFormula, AAnalytic, AUnbounded,
  AScaling: boolean): TFitAdvice;
begin
  Result := AdviseFit(ALoss, AFormula, AAnalytic, AUnbounded, AScaling);
end;

{ The overwhelmingly common case: a peak, the native engine, the default
  objective. Nothing is corrected, so nothing must be said - an app that
  explains itself when there is nothing to explain teaches people to ignore it. }
procedure TFitAdviceTest.APlainPeakFitOnTheNativeEngineHasNothingToExplain;
var A: TFitAdvice;
begin
  A := Advise(LOSS_KIND_RFACTOR, False, True, False, True);
  AssertEquals('the objective is honoured', LOSS_KIND_RFACTOR, A.LossKind);
  AssertFalse('no fallback', A.FallsBackToNativeEngine);
  AssertFalse('no substitution', A.LossOverridden);
  AssertFalse('scaling untouched', A.CurveScalingDisabled);
  AssertFalse('nothing to draw attention to', AdviceNeedsAttention(A));
  AssertEquals('and so nothing to justify', '', A.Detail);
  AssertTrue('but the status line still says what runs', A.Summary <> '');
end;

procedure TFitAdviceTest.ASelfNormalisingLossIsSubstitutedAndExplained;
var A: TFitAdvice;
begin
  //  Model-normalised objective + a model whose amplitude is free.
  A := Advise(LOSS_KIND_RFACTOR_LEGACY, False, True, True, False);
  AssertTrue('it must be substituted', A.LossOverridden);
  AssertEquals('for the corrected R-factor', LOSS_KIND_RFACTOR, A.LossKind);
  AssertTrue('the user must be told', AdviceNeedsAttention(A));
  AssertTrue('the explanation names what was refused',
    Pos(LossName(LOSS_KIND_RFACTOR_LEGACY), A.Detail) > 0);
  AssertTrue('and what replaced it',
    Pos(LossName(LOSS_KIND_RFACTOR), A.Detail) > 0);
end;

procedure TFitAdviceTest.AFormulaLessCurveFallsBackAndSaysWhy;
var A: TFitAdvice;
begin
  A := Advise(LOSS_KIND_RFACTOR, True, False, False, True);
  AssertTrue('a formula engine cannot evaluate a formula-less curve',
    A.FallsBackToNativeEngine);
  AssertTrue('the user must be told', AdviceNeedsAttention(A));
  AssertTrue('the explanation gives the reason, not just the fact',
    Pos('formula', LowerCase(A.Detail)) > 0);
  //  The honest cost of the fallback, so nobody hunts for missing error bars.
  AssertTrue('and states what is lost',
    Pos('uncertaint', LowerCase(A.Detail)) > 0);
end;

procedure TFitAdviceTest.ANonLeastSquaresLossFallsBackAndNamesTheAlternatives;
var A: TFitAdvice;
begin
  A := Advise(LOSS_KIND_RELATIVE, True, True, False, True);
  AssertTrue('the sidecar cannot minimise an L1 objective',
    A.FallsBackToNativeEngine);
  AssertEquals('but the objective itself is still honoured',
    LOSS_KIND_RELATIVE, A.LossKind);
  //  An explanation the user can act on beats one they can only accept.
  AssertTrue('it must name a loss that would keep the selected engine',
    (Pos(LossName(LOSS_KIND_RFACTOR), A.Detail) > 0) and
    (Pos(LossName(LOSS_KIND_SUMSQ), A.Detail) > 0));
end;

{ Reporting only the first reason would be actively misleading: the user fixes
  it, expects their engine back, and is refused again for a reason nobody
  mentioned. }
procedure TFitAdviceTest.BothReasonsToFallBackAreReportedNotJustTheFirst;
var A: TFitAdvice;
begin
  //  A formula-less curve AND an objective the sidecar cannot express.
  A := Advise(LOSS_KIND_RELATIVE, True, False, False, True);
  AssertTrue('falls back', A.FallsBackToNativeEngine);
  AssertTrue('the formula reason is present',
    Pos('formula', LowerCase(A.Detail)) > 0);
  AssertTrue('the objective reason is present too',
    Pos('squared residuals', LowerCase(A.Detail)) > 0);
end;

{ Substituting one unusable objective for another would be a silent trap. }
procedure TFitAdviceTest.TheSubstitutedLossIsItselfAlwaysUsable;
var
  K: longint;
  Unbounded, Formula, Analytic, Scaling: boolean;
  A: TFitAdvice;
begin
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    for Unbounded := False to True do
      for Formula := False to True do
        for Analytic := False to True do
          for Scaling := False to True do
          begin
            A := Advise(K, Formula, Analytic, Unbounded, Scaling);
            AssertTrue(Format('loss %d/unb=%s: the effective objective must be '
              + 'usable with this model', [K, BoolToStr(Unbounded, True)]),
              LossAllowedForCapability(A.LossKind, Unbounded));
          end;
end;

procedure TFitAdviceTest.TheEffectiveLossIsNeverLeftUnknown;
var
  A: TFitAdvice;
begin
  //  A nonsense value must resolve to something real rather than propagate:
  //  the engine would otherwise raise mid-fit on an unknown kind.
  A := Advise(LOSS_KIND_LAST + 99, False, True, False, False);
  AssertTrue('an unknown objective resolves to a known one',
    IsKnownLoss(A.LossKind));
  A := Advise(-5, False, True, False, False);
  AssertTrue('including a negative one', IsKnownLoss(A.LossKind));
end;

{ Curve scaling is an internal convergence aid, not something the user chose for
  its own sake. Alerting on it would fire on every such selection and train
  people to dismiss these messages - which would cost us the two that matter. }
procedure TFitAdviceTest.CurveScalingIsReportedButNeverRaisedAsAnAlert;
var A: TFitAdvice;
begin
  A := Advise(LOSS_KIND_RFACTOR, False, True, True, True);
  AssertTrue('it is switched off for a self-scaling model',
    A.CurveScalingDisabled);
  AssertTrue('and explained if the user looks', A.Detail <> '');
  AssertFalse('but never on its own raises a dialog', AdviceNeedsAttention(A));

  //  Not switched off when it was not asked for, and not for ordinary peaks.
  AssertFalse('not disabled when it was never on',
    Advise(LOSS_KIND_RFACTOR, False, True, True, False).CurveScalingDisabled);
  AssertFalse('not disabled for a peak',
    Advise(LOSS_KIND_RFACTOR, False, True, False, True).CurveScalingDisabled);
end;

{ A status line echoing the selection is worse than useless when the selection
  is not what runs - that is precisely the case it exists for. }
procedure TFitAdviceTest.TheSummaryAlwaysDescribesWhatWillHappen;
var
  K: longint;
  Unbounded, Formula, Analytic: boolean;
  A: TFitAdvice;
begin
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    for Unbounded := False to True do
      for Formula := False to True do
        for Analytic := False to True do
        begin
          A := Advise(K, Formula, Analytic, Unbounded, True);
          AssertTrue('the summary is never empty', A.Summary <> '');
          //  It must name the objective that will actually be minimised.
          AssertTrue(Format('summary must name the effective objective (%s): %s',
            [LossName(A.LossKind), A.Summary]),
            Pos(LossName(A.LossKind), A.Summary) > 0);
          if A.FallsBackToNativeEngine then
            AssertTrue('and must say the engine changed: ' + A.Summary,
              Pos('built-in', LowerCase(A.Summary)) > 0);
        end;
end;

procedure TFitAdviceTest.AnythingOverriddenIsAlwaysExplained;
var
  K: longint;
  Unbounded, Formula, Analytic, Scaling: boolean;
  A: TFitAdvice;
begin
  //  The invariant that makes the feature trustworthy: there is no combination
  //  in which something is silently changed.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    for Unbounded := False to True do
      for Formula := False to True do
        for Analytic := False to True do
          for Scaling := False to True do
          begin
            A := Advise(K, Formula, Analytic, Unbounded, Scaling);
            if A.LossOverridden or A.FallsBackToNativeEngine or
               A.CurveScalingDisabled then
              AssertTrue(Format('loss=%d formula=%s analytic=%s unbounded=%s: '
                + 'something changed and nothing was said',
                [K, BoolToStr(Formula, True), BoolToStr(Analytic, True),
                 BoolToStr(Unbounded, True)]), A.Detail <> '');
          end;
end;

procedure TFitAdviceTest.NothingOverriddenMeansNothingToExplain;
var
  K: longint;
  Unbounded, Formula, Analytic, Scaling: boolean;
  A: TFitAdvice;
begin
  //  The converse, and the reason the message stays credible: no text is
  //  produced when nothing was corrected.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    for Unbounded := False to True do
      for Formula := False to True do
        for Analytic := False to True do
          for Scaling := False to True do
          begin
            A := Advise(K, Formula, Analytic, Unbounded, Scaling);
            if not (A.LossOverridden or A.FallsBackToNativeEngine or
                    A.CurveScalingDisabled) then
              AssertEquals('nothing was corrected, so nothing may be claimed',
                '', A.Detail);
          end;
end;

{ --------------------- moving a markup point after a fit -------------------- }

{ THE ONLY DECISION IN THIS UNIT THAT REFUSES. Everything else here substitutes
  something workable and explains what it did; this one stops the user, because
  there is nothing to substitute: the curves are placed by the whole markup
  rather than one by one, so moving any of its points rebuilds all of them and
  discards whatever the last fit found.

  IT HAD NO TEST, and neither did the service wrapper that carries it to the
  user - twelve lines that log the refusal and raise it. The wrapper needs a
  service to reach; the rule and its wording do not.

  THE WORDING IS THE DELIVERABLE HERE. A refusal that only says no costs the user
  the work they were about to do and tells them nothing; this one has to say what
  did not happen, why, and the two ways to get what they wanted. Those are three
  separate claims and they are asserted separately, because a message can lose one
  of them in an edit and still read like a sentence. }

procedure TFitAdviceTest.BeforeAnyFitAMarkupPointMovesFreely;
var
  Reason: string;
begin
  //  NOTHING TO LOSE YET. Refusing here would make the markup uneditable from
  //  the moment it is drawn, which is the opposite of the intent.
  AssertTrue('allowed', AdviseMoveMarkupPoint(False, Reason));
end;

procedure TFitAdviceTest.AfterAFitTheMoveIsRefused;
var
  Reason: string;
begin
  AssertFalse('refused', AdviseMoveMarkupPoint(True, Reason));
end;

procedure TFitAdviceTest.ARefusedMoveSaysTheMoveDidNotHappen;
var
  Reason: string;
begin
  //  FIRST, AND PLAINLY. The user has just dragged something; the one thing they
  //  need to know before any explanation is whether it moved. A message that
  //  opened with the reasoning would leave them looking at the chart trying to
  //  work out whether it had.
  AdviseMoveMarkupPoint(True, Reason);
  AssertTrue('it says the point was not moved: ' + Reason,
    Pos('was not moved', Reason) > 0);
end;

procedure TFitAdviceTest.AndWhyTheWholeModelWouldBeRebuilt;
var
  Reason: string;
begin
  //  THE REASON IS NOT OBVIOUS FROM THE SCREEN. One point looks like one point;
  //  that all the curves depend on all of it is a property of how this kind of
  //  markup places them, and the user has no way to know it.
  AdviseMoveMarkupPoint(True, Reason);
  AssertTrue('it explains the rebuild: ' + Reason,
    (Pos('rebuild', Reason) > 0) or (Pos('rebuilds', Reason) > 0));
  AssertTrue('and that the fit would be lost: ' + Reason,
    Pos('lost', Reason) > 0);
end;

procedure TFitAdviceTest.AndOffersBothWaysForward;
var
  Reason: string;
begin
  //  TWO WAYS, because which one the user wants depends on something the program
  //  cannot know: whether the fit or the markup is the thing they care about.
  //  Offering only "fit again" reads as "your fit was worthless"; offering only
  //  "undo" reads as "you cannot change the markup".
  AdviseMoveMarkupPoint(True, Reason);
  AssertTrue('fit again: ' + Reason, Pos('fit again', Reason) > 0);
  AssertTrue('or undo first: ' + Reason, Pos('undo', Reason) > 0);
end;

procedure TFitAdviceTest.AnAllowedMoveCarriesNoText;
var
  Reason: string;
begin
  //  EMPTY, because the caller raises on non-empty. Any text here - even
  //  "allowed" - would refuse every markup move made before a fit.
  Reason := 'left over from somewhere';
  AdviseMoveMarkupPoint(False, Reason);
  AssertEquals('no reason when there is nothing to refuse', '', Reason);
end;

procedure TFitAdviceTest.TheRefusalAvoidsTheWordSeed;
var
  Reason: string;
begin
  //  THE UNIT'S OWN RULE, stated in its comments and applied to every message in
  //  it: phrased for someone who has not read the documentation. "Seed" is the
  //  internal name for a curve's starting position and appears nowhere the user
  //  can learn it, so a message using it explains nothing to the person reading.
  AdviseMoveMarkupPoint(True, Reason);
  AssertTrue('no jargon: ' + Reason,
    Pos('seed', LowerCase(Reason)) = 0);
end;

{ ---- whether to say it out loud ------------------------------------------- }

function AdviceThatNeedsAttention: TFitAdvice;
begin
    //  A formula backend asked for, with a curve that has no formula: the fit
    //  falls back to the built-in engine, and says so.
    Result := Advise(LOSS_KIND_RFACTOR, True, False, False, False);
end;

function AdviceWithNothingToSay: TFitAdvice;
begin
    Result := Advise(LOSS_KIND_RFACTOR, False, True, False, False);
end;

procedure TFitAdviceTest.AdviceWithNothingToExplainIsNotAnnounced;
var
    Kept: string;
begin
    Kept := '';
    AssertFalse('nothing to say',
        AdviceShouldBeAnnounced(True, AdviceWithNothingToSay, Kept));
end;

procedure TFitAdviceTest.AdviceThatNeedsAttentionIsAnnouncedOnce;
var
    Kept: string;
begin
    Kept := '';
    AssertTrue('said',
        AdviceShouldBeAnnounced(True, AdviceThatNeedsAttention, Kept));
    AssertTrue('and remembered', Kept <> '');
end;

procedure TFitAdviceTest.AndNotAgainForTheSameSelection;
var
    Advice: TFitAdvice;
    Kept, Again: string;
begin
    //  The user is adjusting settings; every adjustment recomputes the advice.
    //  Repeating the dialog would put it in front of someone in the middle of
    //  changing something.
    Advice := AdviceThatNeedsAttention;
    Kept := '';
    AdviceShouldBeAnnounced(True, Advice, Kept);
    Again := Kept;
    AssertFalse('not twice',
        AdviceShouldBeAnnounced(True, Advice, Again));
    AssertEquals('and still remembered', Kept, Again);
end;

procedure TFitAdviceTest.TheWindowsOneMemoryIsNotSaidTwice;
var
    Advice: TFitAdvice;
    Memory: string;
begin
    //  AS THE WINDOW CALLS IT: one field, read and written.
    Advice := AdviceThatNeedsAttention;
    Memory := '';
    AdviceShouldBeAnnounced(True, Advice, Memory);
    AssertFalse('not twice through one memory',
        AdviceShouldBeAnnounced(True, Advice, Memory));
end;

procedure TFitAdviceTest.ButAgainAfterLeavingAndComingBack;
var
    Advice: TFitAdvice;
    Kept, Cleared, Again: string;
begin
    //  THE PART THAT IS EASY TO GET WRONG. Leaving the problematic selection
    //  FORGETS the message, so coming back explains itself afresh rather than
    //  staying silent because it was mentioned once about something else.
    Advice := AdviceThatNeedsAttention;
    Kept := '';
    AdviceShouldBeAnnounced(True, Advice, Kept);
    Cleared := Kept;
    AdviceShouldBeAnnounced(True, AdviceWithNothingToSay, Cleared);
    AssertEquals('forgotten', '', Cleared);
    Again := Cleared;
    AssertTrue('and said again on return',
        AdviceShouldBeAnnounced(True, Advice, Again));
end;

procedure TFitAdviceTest.StartUpDoesNotAnnounceAnything;
var
    Kept: string;
begin
    //  Start-up recomputes the advice too, and a dialog on every launch for a
    //  setting chosen long ago is how people learn to dismiss these unread.
    Kept := '';
    AssertFalse('not on start-up',
        AdviceShouldBeAnnounced(False, AdviceThatNeedsAttention, Kept));
end;

procedure TFitAdviceTest.AndStartUpDoesNotDisturbWhatWasRemembered;
var
    Kept: string;
begin
    //  Clearing here would make the next user-driven change repeat a message
    //  already read; setting it would swallow one not yet read.
    Kept := 'said before';
    AdviceShouldBeAnnounced(False, AdviceThatNeedsAttention, Kept);
    AssertEquals('left alone', 'said before', Kept);
end;

{ ---- the background, two ways ---- }

procedure TBackgroundAdviceTest.WithNeitherBothMayBeAddedAndTheRunAddsACurve;
var A: TBackgroundModelAdvice;
begin
  A := AdviseBackgroundModel(False, False);
  AssertTrue('variation may be switched on', A.VariationAllowed);
  AssertTrue('a curve may be added', A.BackgroundCurveAllowed);
  AssertFalse('no variation is in force', A.VariationInForce);
  AssertTrue('the automatic run adds a background curve',
    A.AutomaticRunAddsBackground);
  AssertEquals('and there is nothing to say about it', '', A.AutomaticRunNote);
end;

procedure TBackgroundAdviceTest.ACurveInTheModelRefusesVariationAndSaysWhy;
var A: TBackgroundModelAdvice;
begin
  A := AdviseBackgroundModel(False, True);
  AssertFalse('variation may not be switched on', A.VariationAllowed);
  AssertTrue('the reason names the curve',
    Pos('background curve', A.VariationReason) > 0);
  AssertTrue('and what counting it twice would do',
    Pos('twice', A.VariationReason) > 0);
  AssertTrue('the curve itself stays allowed', A.BackgroundCurveAllowed);
end;

procedure TBackgroundAdviceTest.VariationOnRefusesACurveAndSaysWhy;
var A: TBackgroundModelAdvice;
begin
  A := AdviseBackgroundModel(True, False);
  AssertFalse('a curve may not be added', A.BackgroundCurveAllowed);
  AssertTrue('the reason names the option by its menu path',
    Pos('Enable Variation', A.BackgroundCurveReason) > 0);
  AssertTrue('and says how to get the curve instead',
    Pos('off', A.BackgroundCurveReason) > 0);
  AssertTrue('variation itself stays allowed', A.VariationAllowed);
  AssertTrue('and is in force', A.VariationInForce);
end;

procedure TBackgroundAdviceTest.WithVariationOnTheRunAddsNoCurveAndSaysSo;
var A: TBackgroundModelAdvice;
begin
  //  DECIDED BY THE USER: the run respects the option they switched on rather
  //  than overriding it, and says that it did.
  A := AdviseBackgroundModel(True, False);
  AssertFalse('no curve is added', A.AutomaticRunAddsBackground);
  AssertTrue('the note says the variation is the background',
    Pos('Enable Variation', A.AutomaticRunNote) > 0);
end;

procedure TBackgroundAdviceTest.WithACurveTheRunAddsNothingAndSaysNothing;
var A: TBackgroundModelAdvice;
begin
  A := AdviseBackgroundModel(False, True);
  AssertFalse('the model already has one', A.AutomaticRunAddsBackground);
  AssertEquals('nothing to say', '', A.AutomaticRunNote);
end;

procedure TBackgroundAdviceTest.BothAtOnceKeepsTheCurveAndDropsTheVariation;
var A: TBackgroundModelAdvice;
begin
  //  ONLY A HAND-EDITED PROJECT GETS HERE - the setters refuse the second of
  //  the two. The curve wins because it is the one the user can see, and the
  //  fit must not count the background twice either way.
  A := AdviseBackgroundModel(True, True);
  AssertFalse('the variation is not applied', A.VariationInForce);
  AssertFalse('no curve is added', A.AutomaticRunAddsBackground);
  AssertTrue('and that is explained', A.VariationReason <> '');
end;

procedure TBackgroundAdviceTest.EveryRefusalIsExplained;
var
  V, C: boolean;
  A: TBackgroundModelAdvice;
begin
  for V := False to True do
    for C := False to True do
    begin
      A := AdviseBackgroundModel(V, C);
      AssertEquals('variation refused iff explained',
        not A.VariationAllowed, A.VariationReason <> '');
      AssertEquals('a curve refused iff explained',
        not A.BackgroundCurveAllowed, A.BackgroundCurveReason <> '');
    end;
end;

procedure TBackgroundAdviceTest.OnlyABackgroundShapeMayBeTheBackground;
var Why: string;
begin
  AssertTrue('a background shape', AdviseBackgroundCurveType('Linear background',
    True, False, 0, Why));
  AssertEquals('carries no text', '', Why);
  AssertFalse('a peak shape', AdviseBackgroundCurveType('Gaussian',
    False, False, 0, Why));
end;

procedure TBackgroundAdviceTest.APeakShapeIsRefusedAsTheBackgroundByName;
var Why: string;
begin
  AdviseBackgroundCurveType('Gaussian', False, False, 0, Why);
  AssertTrue('names the shape', Pos('Gaussian', Why) > 0);
  AssertTrue('and says it is a peak', Pos('peak', Why) > 0);
end;

procedure TBackgroundAdviceTest.AShapeNeedingPositiveXIsRefusedOnDataThatReachesZero;
var Why: string;
begin
  AssertFalse('x reaches zero', AdviseBackgroundCurveType('Power-law background',
    True, True, 0, Why));
  AssertTrue('names the shape', Pos('Power-law background', Why) > 0);
  AssertTrue('and the smallest x', Pos('0', Why) > 0);
  AssertFalse('x negative', AdviseBackgroundCurveType('Power-law background',
    True, True, -3, Why));
end;

procedure TBackgroundAdviceTest.AndAllowedOnDataThatStaysPositive;
var Why: string;
begin
  AssertTrue(AdviseBackgroundCurveType('Power-law background',
    True, True, 0.5, Why));
  AssertEquals('', Why);
end;

procedure TBackgroundAdviceTest.ABackgroundShapeIsRefusedAsThePeakTypeByName;
var Why: string;
begin
  AssertFalse(AdvisePeakCurveType('Linear background', True, Why));
  AssertTrue('names the shape', Pos('Linear background', Why) > 0);
  AssertTrue('and where it belongs',
    Pos('Model > Background > Curve', Why) > 0);
end;

procedure TBackgroundAdviceTest.APeakShapeIsAllowedAsThePeakType;
var Why: string;
begin
  AssertTrue(AdvisePeakCurveType('Gaussian', False, Why));
  AssertEquals('', Why);
end;

procedure TBackgroundAdviceTest.VariationIsOfferedWhereItMayBeSwitchedOn;
begin
  AssertTrue('no background curve', VariationCommandEnabled(False, False));
  AssertFalse('greyed beside a background curve',
    VariationCommandEnabled(False, True));
end;

procedure TBackgroundAdviceTest.AndAlwaysWhereItIsOnSoItCanBeSwitchedOff;
begin
  //  A hand-edited project can hold both; the way out has to stay open.
  AssertTrue(VariationCommandEnabled(True, True));
  AssertTrue(VariationCommandEnabled(True, False));
end;

procedure TBackgroundAdviceTest.AnAllowedVariationKeepsItsDesignedHint;
begin
  AssertEquals('Vary the background', VariationHint(
    AdviseBackgroundModel(False, False), 'Vary the background'));
end;

procedure TBackgroundAdviceTest.ARefusedOneSaysWhy;
var
  Advice: TBackgroundModelAdvice;
begin
  //  NEVER BOTH BACKGROUNDS: the words the server would refuse with.
  Advice := AdviseBackgroundModel(False, True);
  AssertFalse(Advice.VariationAllowed);
  AssertEquals(Advice.VariationReason, VariationHint(Advice, 'Vary the background'));
end;

procedure TModelModuleAdviceTest.AnEmptyModelTakesATypeOfAnyModule;
var
  Reason: string;
begin
  //  Choosing the first type is how a model gets its module.
  AssertTrue(AdviseCurveTypeChoice('Impulse (5)', 'Waves', 'Standard', False,
    Reason));
  AssertEquals('nothing to explain', '', Reason);
end;

procedure TModelModuleAdviceTest.AModelWithContentTakesTypesOfItsOwnModule;
var
  Reason: string;
begin
  AssertTrue(AdviseCurveTypeChoice('Lorentzian', 'Standard', 'Standard', True,
    Reason));
  AssertEquals('', Reason);
end;

procedure TModelModuleAdviceTest.AndRefusesAnotherModulesNamingBothAndTheWayOut;
var
  Reason: string;
begin
  AssertFalse(AdviseCurveTypeChoice('Impulse (5)', 'Waves', 'Standard', True,
    Reason));
  AssertTrue('names the type: ' + Reason, Pos('Impulse (5)', Reason) > 0);
  AssertTrue('the module it belongs to', Pos('Waves', Reason) > 0);
  AssertTrue('and the model''s', Pos('Standard', Reason) > 0);
  //  What to do instead, by the paths a user follows.
  AssertTrue('a new project', Pos('File > New Project', Reason) > 0);
  AssertTrue('or emptying the model',
    Pos('Model > Clear Model', Reason) > 0);
end;

procedure TModelModuleAdviceTest.OwnersAreComparedWithoutRegardToCase;
var
  Reason: string;
begin
  AssertTrue(AdviseCurveTypeChoice('Flat', 'waves', 'Waves', True, Reason));
end;

procedure TModelModuleAdviceTest.AModelWhoseTypeTakesNoBackgroundRefusesOneAndSaysWhy;
var
  Reason: string;
begin
  AssertFalse(AdviseBackgroundForModel('Impulse (5)', False, Reason));
  AssertTrue('names the type: ' + Reason, Pos('Impulse (5)', Reason) > 0);
  AssertTrue('and says what to do', Pos('Model > Background > Curve > None',
    Reason) > 0);
end;

procedure TModelModuleAdviceTest.AnyOtherTakesOne;
var
  Reason: string;
begin
  AssertTrue(AdviseBackgroundForModel('Gaussian', True, Reason));
  AssertEquals('', Reason);
end;

procedure TModelModuleAdviceTest.AMixedProjectIsToldWhyAndHowToMakeItOneModules;
begin
  AssertTrue('that it still opens and fits',
    Pos('opens and fits as it was saved', MixedModelWarning) > 0);
  AssertTrue('and the ways to make it one module''s',
    (Pos('Model > Curve Positions > Clear', MixedModelWarning) > 0) and
    (Pos('Model > Clear Model', MixedModelWarning) > 0));
end;

procedure TModelModuleAdviceTest.AModulesProposalIsTakenWhateverTheType;
var
  Reason: string;
begin
  AssertTrue(AdviseIntervalSearch('Impulse (5)', 'Waves', False, True, True, Reason));
  AssertEquals('', Reason);
end;

procedure TModelModuleAdviceTest.WithoutOneTheFrameworksTypesAreSearchedForPeaks;
var
  Reason: string;
begin
  AssertTrue(AdviseIntervalSearch('Gaussian', 'Standard', True, False, True, Reason));
end;

procedure TModelModuleAdviceTest.AndAModulesTypesAreRefusedSayingWhatToDo;
var
  Reason: string;
begin
  AssertFalse(AdviseIntervalSearch('Impulse (5)', 'Waves', False, False, True, Reason));
  AssertTrue('names the type: ' + Reason, Pos('Impulse (5)', Reason) > 0);
  AssertTrue('and the manual way', Pos('Model > Fit Intervals > Start Manual ' +
    'Selection', Reason) > 0);
end;

{ An empty model belongs to no module, so the type selected over it says
  nothing yet about what the data is made of: Auto searches it for peaks, as it
  did before a module could split a model. Found in use on a diffraction
  project whose saved type was a module's. }
procedure TModelModuleAdviceTest.AnEmptyModelIsSearchedForPeaksWhateverTheType;
var
  Reason: string;
begin
  AssertTrue(AdviseIntervalSearch('Impulse (5)', 'Waves', False, False, False,
    Reason));
  AssertEquals('', Reason);
end;

{ ---- THeldParameterAdviceTest ---- }

procedure THeldParameterAdviceTest.AValueHeldAsTypedNeedsNoWord;
var
  Why: string;
begin
  AssertFalse(AdviseHeldParameterValue('sigma', 0.5, 0.5, Why));
  AssertEquals('', Why);
end;

procedure THeldParameterAdviceTest.ARoundingDifferenceIsNotAHold;
var
  Why: string;
begin
  //  A value typed in the table's units comes back through a conversion.
  AssertFalse(AdviseHeldParameterValue('x0', 42.1, 42.1 + 1e-12, Why));
  AssertEquals('', Why);
end;

procedure THeldParameterAdviceTest.AHeldValueNamesTheParameterAndTheLimits;
var
  Why: string;
begin
  AssertTrue(AdviseHeldParameterValue('sigma', 50, 10, Why));
  AssertTrue('names it: ' + Why, Pos('sigma', Why) > 0);
  AssertTrue('the width rule: ' + Why, Pos('fit interval', Why) > 0);
  AssertTrue('the background rule: ' + Why, Pos('below zero', Why) > 0);
end;

procedure THeldParameterAdviceTest.AndPointsToWhereTheLimitsAreExplained;
var
  Why: string;
begin
  AdviseHeldParameterValue('eta', 3, 1, Why);
  AssertTrue(Why, Pos('Limits on parameter values', Why) > 0);
end;

procedure THeldParameterAdviceTest.AValueHeldAtZeroFromAboveIsAHoldToo;
var
  Why: string;
begin
  //  A background level typed below zero and lifted: -5 typed, 8 held.
  AssertTrue(AdviseHeldParameterValue('b0', -5, 8, Why));
  AssertTrue(Why <> '');
end;

procedure THeldParameterAdviceTest.AProjectHeldAsSavedNeedsNoWord;
begin
  AssertEquals('', AdviseValuesHeldOnOpen([]));
end;

procedure THeldParameterAdviceTest.AProjectHeldOtherwiseNamesTheValuesAndTheLimits;
var
  Why: string;
begin
  Why := AdviseValuesHeldOnOpen(['sigma of curve 2', 'b0 of curve 5']);
  AssertTrue(Why, Pos('sigma of curve 2', Why) > 0);
  AssertTrue(Why, Pos('b0 of curve 5', Why) > 0);
  AssertTrue(Why, Pos('Limits on parameter values', Why) > 0);
end;

function OneAtZero: TSampleRanges;
begin
  Result := UncoveredRanges([0, 1, 2], [False, True, True]);
end;

procedure TUncoveredSamplesAdviceTest.WithNoneThereIsNothingToSay;
begin
  AssertEquals('', AdviseUncoveredSamples(nil, 0));
end;

procedure TUncoveredSamplesAdviceTest.OneSampleIsNamedWithWhereItIs;
var
  Why: string;
begin
  Why := AdviseUncoveredSamples(OneAtZero, 0.985);
  AssertTrue(Why, Pos('1 sample ', Why) > 0);
  AssertTrue(Why, Pos('x = 0', Why) > 0);
end;

procedure TUncoveredSamplesAdviceTest.ItSaysTheModelIsZeroThereAndStillCounted;
var
  Why: string;
begin
  Why := AdviseUncoveredSamples(OneAtZero, 0.985);
  AssertTrue(Why, Pos('model is 0 there', Why) > 0);
  AssertTrue(Why, Pos('R-factor', Why) > 0);
end;

procedure TUncoveredSamplesAdviceTest.ItSaysHowMuchOfTheDifferenceTheyMake;
var
  Why: string;
begin
  Why := AdviseUncoveredSamples(OneAtZero, 0.985);
  AssertTrue(Why, Pos('98.5 %', Why) > 0);
end;

procedure TUncoveredSamplesAdviceTest.ATinyShareIsNotQuoted;
var
  Why: string;
begin
  //  Under half a percent the share says nothing a reader can use, and
  //  "0.0 %" reads as "none at all".
  Why := AdviseUncoveredSamples(OneAtZero, 0.001);
  AssertTrue(Why <> '');
  AssertEquals(Why, 0, Pos('%', Why));
end;

procedure TUncoveredSamplesAdviceTest.ItOffersBothWaysForward;
var
  Why: string;
begin
  Why := AdviseUncoveredSamples(OneAtZero, 0.985);
  AssertTrue('narrow the interval: ' + Why, Pos('narrow the fit interval', Why) > 0);
  AssertTrue('or extend the model: ' + Why, Pos('extend the model', Why) > 0);
end;

procedure TUncoveredSamplesAdviceTest.AStretchIsNamedByItsEnds;
var
  Why: string;
begin
  Why := AdviseUncoveredSamples(
    UncoveredRanges([1, 2, 3, 4, 5, 6], [True, False, False, False, False, True]),
    0.5);
  AssertTrue(Why, Pos('4 samples ', Why) > 0);
  AssertTrue(Why, Pos('x = 2 to 5', Why) > 0);
end;

procedure TUncoveredSamplesAdviceTest.ALongListIsCutAndSaysHowMuchWasLeftOut;
var
  Why: string;
begin
  //  Six stretches, every other sample.
  Why := AdviseUncoveredSamples(UncoveredRanges(
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
    [False, True, False, True, False, True, False, True, False, True, False,
     True]), 0.5);
  AssertTrue(Why, Pos('x = 6', Why) > 0);
  AssertEquals('the fifth is not listed: ' + Why, 0, Pos('x = 8', Why));
  AssertTrue(Why, Pos('and 2 more stretches', Why) > 0);
end;

procedure TUncoveredSamplesAdviceTest.TheStatusLineIsEmptyWithNone;
begin
  AssertEquals('', UncoveredSamplesStatusText(0));
end;

procedure TUncoveredSamplesAdviceTest.TheStatusLineSaysHowManyAndWhereToRead;
var
  Line: string;
begin
  Line := UncoveredSamplesStatusText(3);
  AssertTrue(Line, Pos('3 samples', Line) > 0);
  AssertTrue(Line, Pos('History', Line) > 0);
  AssertTrue(Line, Pos('1 sample ', UncoveredSamplesStatusText(1)) > 0);
end;

function OneAtOne: TSampleRanges;
begin
  Result := UncoveredRanges([0, 1, 2], [True, False, True]);
end;

procedure TUncoveredSamplesAdviceTest.AFitLeavingSamplesUncoveredSaysSoInADialog;
var
  Remembered: string;
begin
  Remembered := '';
  AssertEquals('the History text, in full',
    AdviseUncoveredSamples(OneAtZero, 0.985),
    UncoveredSamplesToAnnounce(OneAtZero, 0.985, True, Remembered));
  AssertTrue('and remembered', Remembered <> '');
end;

procedure TUncoveredSamplesAdviceTest.TheSameStretchesAreNotSaidAgainWhenOnlyTheShareMoves;
var
  First, Second: string;
begin
  //  EVERY FIT MOVES THE SHARE, so remembering the words would say it after
  //  every fit; what is remembered is where the samples are.
  First := '';
  UncoveredSamplesToAnnounce(OneAtZero, 0.985, True, First);
  Second := First;
  AssertEquals('', UncoveredSamplesToAnnounce(OneAtZero, 0.969, True, Second));
  AssertEquals('still remembered', First, Second);
end;

procedure TUncoveredSamplesAdviceTest.OtherStretchesAreSaidAfresh;
var
  First, Second: string;
begin
  First := '';
  UncoveredSamplesToAnnounce(OneAtZero, 0.985, True, First);
  Second := First;
  AssertTrue(UncoveredSamplesToAnnounce(OneAtOne, 0.5, True, Second) <> '');
end;

procedure TUncoveredSamplesAdviceTest.CoveredAndThenUncoveredAgainIsSaidAgain;
var
  First, Cleared, Again: string;
begin
  First := '';
  UncoveredSamplesToAnnounce(OneAtZero, 0.985, True, First);
  Cleared := First;
  AssertEquals('nothing to say once covered', '',
    UncoveredSamplesToAnnounce(nil, 0, True, Cleared));
  AssertEquals('and forgotten', '', Cleared);
  Again := Cleared;
  AssertTrue('so the same gap later is said again',
    UncoveredSamplesToAnnounce(OneAtZero, 0.985, True, Again) <> '');
end;

procedure TUncoveredSamplesAdviceTest.AnUnattendedRunSaysNothingAndForgetsNothing;
var
  Remembered: string;
begin
  //  The window checking itself, or a recording: a dialog would stop it.
  Remembered := 'before';
  AssertEquals('', UncoveredSamplesToAnnounce(OneAtZero, 0.985, False,
    Remembered));
  AssertEquals('what was remembered is kept', 'before', Remembered);
end;

const
  Bars: array[0..9] of double = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

procedure TReplacedIntervalsAdviceTest.WithNoIntervalsBeforeTheRunThereIsNothingToSay;
begin
  AssertEquals('', AdviseReplacedIntervals([], [1, 9], Bars));
end;

procedure TReplacedIntervalsAdviceTest.TheSameIntervalsAreNothingToSay;
begin
  AssertEquals('', AdviseReplacedIntervals([0, 9], [0, 9], Bars));
end;

procedure TReplacedIntervalsAdviceTest.ReplacedIntervalsAreNamedBeforeAndAfter;
var
  Why: string;
begin
  Why := AdviseReplacedIntervals([0, 9], [1, 4, 5, 9], Bars);
  AssertTrue(Why, Pos('Fit > Automatically replaced the fit intervals', Why) > 0);
  AssertTrue('before: ' + Why, Pos('x = 0 to 9', Why) > 0);
  AssertTrue('after: ' + Why, Pos('x = 1 to 4; x = 5 to 9', Why) > 0);
end;

procedure TReplacedIntervalsAdviceTest.ASampleNoLongerFittedIsNamed;
var
  Why: string;
begin
  Why := AdviseReplacedIntervals([0, 9], [1, 9], Bars);
  AssertTrue(Why, Pos('The sample at x = 0 is no longer fitted', Why) > 0);
end;

procedure TReplacedIntervalsAdviceTest.SeveralAreCountedAndPlaced;
var
  Why: string;
begin
  Why := AdviseReplacedIntervals([0, 9], [2, 7], Bars);
  AssertTrue(Why, Pos('4 samples', Why) > 0);
  AssertTrue(Why, Pos('x = 0 to 1; x = 8 to 9', Why) > 0);
  AssertTrue(Why, Pos('are no longer fitted', Why) > 0);
end;

procedure TReplacedIntervalsAdviceTest.WhenNothingIsLeftOutItDoesNotSaySo;
var
  Why: string;
begin
  //  Wider than before: everything the user fitted still is.
  Why := AdviseReplacedIntervals([2, 7], [0, 9], Bars);
  AssertTrue(Why <> '');
  AssertEquals(Why, 0, Pos('no longer fitted', Why));
end;

procedure TReplacedIntervalsAdviceTest.ItSaysHowToFitThemAgain;
var
  Why: string;
begin
  Why := AdviseReplacedIntervals([0, 9], [1, 9], Bars);
  AssertTrue(Why, Pos('Model > Fit Intervals', Why) > 0);
  AssertTrue(Why, Pos('Fit > Minimize Difference', Why) > 0);
end;

procedure TEndOfRunNoticeTest.TheRunsNoteIsShown;
var
  NoteMemory, UncoveredMemory: string;
begin
  NoteMemory := '';
  UncoveredMemory := '';
  AssertEquals('The note.', EndOfRunNotice('The note.', EmptyFitStatistics,
    True, NoteMemory, UncoveredMemory));
end;

procedure TEndOfRunNoticeTest.AndNotAgainForTheSameNote;
var
  NoteMemory, UncoveredMemory: string;
begin
  NoteMemory := '';
  UncoveredMemory := '';
  EndOfRunNotice('The note.', EmptyFitStatistics, True, NoteMemory,
    UncoveredMemory);
  AssertEquals('', EndOfRunNotice('The note.', EmptyFitStatistics, True,
    NoteMemory, UncoveredMemory));
end;

procedure TEndOfRunNoticeTest.TheNoteAndTheUncoveredSamplesAreShownTogether;
var
  NoteMemory, UncoveredMemory, Shown: string;
  S: TFitStatistics;
begin
  NoteMemory := '';
  UncoveredMemory := '';
  S := EmptyFitStatistics;
  S.UncoveredRanges := OneAtZero;
  Shown := EndOfRunNotice('The note.', S, True, NoteMemory, UncoveredMemory);
  AssertTrue(Shown, Pos('The note.', Shown) = 1);
  AssertTrue(Shown, Pos('covered by no curve', Shown) > 0);
end;

procedure TEndOfRunNoticeTest.AnUnattendedRunShowsNothing;
var
  NoteMemory, UncoveredMemory: string;
  S: TFitStatistics;
begin
  NoteMemory := '';
  UncoveredMemory := '';
  S := EmptyFitStatistics;
  S.UncoveredRanges := OneAtZero;
  AssertEquals('', EndOfRunNotice('The note.', S, False, NoteMemory,
    UncoveredMemory));
end;

function UncoveredAtZero: TFitStatistics;
begin
  Result := EmptyFitStatistics;
  Result.UncoveredRanges := OneAtZero;
  Result.UncoveredResidualShare := 0.8;
end;

procedure TBeforeFitQuestionTest.AModelLeavingSamplesUncoveredIsAskedAbout;
var
  Memory, Q: string;
begin
  Memory := '';
  Q := UncoveredSamplesBeforeFit(UncoveredAtZero, True, Memory);
  AssertTrue(Q, Pos('covered by no curve', Q) > 0);
  AssertTrue(Q, Pos('Fit anyway?', Q) > 0);
end;

procedure TBeforeFitQuestionTest.TheQuestionSaysWhatNoDoes;
var
  Memory, Q: string;
begin
  Memory := '';
  Q := UncoveredSamplesBeforeFit(UncoveredAtZero, True, Memory);
  AssertTrue(Q, Pos('No leaves the model as it is', Q) > 0);
end;

procedure TBeforeFitQuestionTest.ACoveredModelIsNotAskedAbout;
var
  Memory: string;
begin
  Memory := '';
  AssertEquals('', UncoveredSamplesBeforeFit(EmptyFitStatistics, True, Memory));
end;

procedure TBeforeFitQuestionTest.TheSameStretchesAreNotAskedTwice;
var
  Memory: string;
begin
  //  ANSWERED: Yes was a decision to fit as it is, and No was read - asking
  //  again before the next fit of the same model is asking twice.
  Memory := '';
  UncoveredSamplesBeforeFit(UncoveredAtZero, True, Memory);
  AssertEquals('', UncoveredSamplesBeforeFit(UncoveredAtZero, True, Memory));
end;

procedure TBeforeFitQuestionTest.AndTheEndOfTheFitDoesNotSayItAgain;
var
  Memory, NoteMemory: string;
begin
  //  ONE MEMORY for the question and the end-of-run dialog.
  Memory := '';
  NoteMemory := '';
  UncoveredSamplesBeforeFit(UncoveredAtZero, True, Memory);
  AssertEquals('', EndOfRunNotice('', UncoveredAtZero, True, NoteMemory,
    Memory));
end;

procedure TBeforeFitQuestionTest.AnUnattendedRunIsNotAsked;
var
  Memory: string;
begin
  Memory := '';
  AssertEquals('', UncoveredSamplesBeforeFit(UncoveredAtZero, False, Memory));
end;

initialization
  RegisterTest('unit', TBeforeFitQuestionTest);
  RegisterTest('unit', TReplacedIntervalsAdviceTest);
  RegisterTest('unit', TEndOfRunNoticeTest);
  RegisterTest('unit', TUncoveredSamplesAdviceTest);
  RegisterTest('unit', TFitAdviceTest);
  RegisterTest('unit', TModelModuleAdviceTest);
  RegisterTest('unit', TBackgroundAdviceTest);
  RegisterTest('unit', THeldParameterAdviceTest);
end.
