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
  fit_advice, fit_loss, loss_compatibility;
type
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
    procedure StartUpDoesNotAnnounceAnything;
    procedure AndStartUpDoesNotDisturbWhatWasRemembered;
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
    AssertFalse('nothing to say',
        AdviceShouldBeAnnounced(True, AdviceWithNothingToSay, '', Kept));
end;

procedure TFitAdviceTest.AdviceThatNeedsAttentionIsAnnouncedOnce;
var
    Kept: string;
begin
    AssertTrue('said',
        AdviceShouldBeAnnounced(True, AdviceThatNeedsAttention, '', Kept));
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
    AdviceShouldBeAnnounced(True, Advice, '', Kept);
    AssertFalse('not twice',
        AdviceShouldBeAnnounced(True, Advice, Kept, Again));
    AssertEquals('and still remembered', Kept, Again);
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
    AdviceShouldBeAnnounced(True, Advice, '', Kept);
    AdviceShouldBeAnnounced(True, AdviceWithNothingToSay, Kept, Cleared);
    AssertEquals('forgotten', '', Cleared);
    AssertTrue('and said again on return',
        AdviceShouldBeAnnounced(True, Advice, Cleared, Again));
end;

procedure TFitAdviceTest.StartUpDoesNotAnnounceAnything;
var
    Kept: string;
begin
    //  Start-up recomputes the advice too, and a dialog on every launch for a
    //  setting chosen long ago is how people learn to dismiss these unread.
    AssertFalse('not on start-up',
        AdviceShouldBeAnnounced(False, AdviceThatNeedsAttention, '', Kept));
end;

procedure TFitAdviceTest.AndStartUpDoesNotDisturbWhatWasRemembered;
var
    Kept: string;
begin
    //  Clearing here would make the next user-driven change repeat a message
    //  already read; setting it would swallow one not yet read.
    AdviceShouldBeAnnounced(False, AdviceThatNeedsAttention, 'said before',
        Kept);
    AssertEquals('left alone', 'said before', Kept);
end;

initialization
  RegisterTest('unit', TFitAdviceTest);
end.
