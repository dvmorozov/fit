// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the selectable objective functions and their compatibility rule.

  Two things are pinned here, and the first matters most:

  1. THE DEFECT IS DEMONSTRATED, not asserted in prose: under the model-
     normalised form, scaling a model up lowers the figure while the actual
     agreement with the data is untouched, and the corrected form does not move.
     That single pair of assertions is the whole argument for the change, and it
     keeps being true after anyone rewrites the surrounding code.

  2. EACH FORMULA IS PINNED TO HAND-COMPUTED NUMBERS, including the original's,
     so neither can drift silently - the original stays available for comparison
     and is only useful if it is still genuinely the original.

  Ordering-independent throughout: the constants were renumbered once already
  (to put the default at 0), and loops written over a sub-range would have
  quietly stopped testing anything. }
unit testcase_fit_loss;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Math, fpcunit, testregistry,
  fit_loss, loss_compatibility;
type
  TFitLossTest = class(TTestCase)
  private
    { obs and a calc that differs from it, for the arithmetic assertions. }
    function Obs: TLossDoubleArray;
    function Calc: TLossDoubleArray;
  published
    procedure LegacyRFactorMatchesTheOriginalFormula;
    procedure CorrectedRFactorNormalisesByTheObservedIntegral;
    procedure SumOfSquaresIsUnnormalised;
    procedure RelativeIsDeviationOverObservedMagnitude;

    procedure LegacyRewardsInflatingTheModel;
    procedure CorrectedIsUnmovedByInflatingTheModel;

    procedure APerfectModelScoresZeroUnderEveryLoss;
    procedure AZeroModelDoesNotDivideByZero;
    procedure AZeroObservationDoesNotDivideByZero;
    procedure AnUnknownLossRaisesRatherThanScoringZero;
    procedure EveryKnownKindIsNamedAndDescribed;

    procedure OnlyTheLegacyFormIsSelfNormalising;
    procedure ASelfNormalisingLossIsRefusedForAFreeAmplitude;
    procedure EveryOtherPairingIsAllowed;
    procedure TheFallbackKeepsTheUsersIntent;
    procedure TheRefusalExplainsItselfAndNamesTheAlternative;
    procedure OnlyConstantMultiplesOfSumOfSquaresSuitALeastSquaresSolver;

    //  THE POOLED FORMS, which combine several fit intervals into one number.
    //  Their divide-by-zero guards had never been reached: the per-point ones
    //  are tested above and the pooled ones are separate code.
    procedure OnlyTheSelfNormalisingFormRefusesToBePooled;
    procedure EveryPoolableFormSurvivesAnAllZeroObservation;
    procedure APooledTotalOfOneIntervalIsThatIntervalScore;
    procedure AnUnknownKindCannotBePooledEither;

    //  Registering an objective.
    procedure AnObjectiveWithNoNameIsRefused;
    procedure AnObjectiveWithNoWayToEvaluateItIsRefused;
    procedure AndTheTwoRefusalsSayWhichIsWrong;
  end;

implementation

function TFitLossTest.Obs: TLossDoubleArray;
begin
  Result := TLossDoubleArray.Create(1, 2, 3, 4);   //  sum 10
end;

function TFitLossTest.Calc: TLossDoubleArray;
begin
  Result := TLossDoubleArray.Create(2, 2, 2, 2);   //  sum 8
end;

{ ------------------------------------------------------------------ arithmetic }

procedure TFitLossTest.LegacyRFactorMatchesTheOriginalFormula;
var Expected: double;
begin
  //  At scale 1: deviations 1, 0, -1, -2 -> sum of squares 6; divided by 8^2.
  Expected := 6 / 64;
  AssertEquals('legacy R-factor arithmetic must not drift', Expected,
    EvaluateLoss(LOSS_KIND_RFACTOR_LEGACY, Calc, Obs, 1), 1e-12);
end;

procedure TFitLossTest.CorrectedRFactorNormalisesByTheObservedIntegral;
begin
  //  Same numerator, denominator 10^2 (the OBSERVED sum) instead of 8^2.
  AssertEquals('corrected R-factor divides by the observed integral', 6 / 100,
    EvaluateLoss(LOSS_KIND_RFACTOR, Calc, Obs, 1), 1e-12);
end;

procedure TFitLossTest.SumOfSquaresIsUnnormalised;
begin
  AssertEquals('sum of squares is the bare numerator', 6,
    EvaluateLoss(LOSS_KIND_SUMSQ, Calc, Obs, 1), 1e-12);
end;

procedure TFitLossTest.RelativeIsDeviationOverObservedMagnitude;
begin
  //  |1| + |0| + |-1| + |-2| = 4, over sum |obs| = 10.
  AssertEquals('relative deviation is |dev| over |obs|', 0.4,
    EvaluateLoss(LOSS_KIND_RELATIVE, Calc, Obs, 1), 1e-12);
end;

{ ------------------------------------------------ the defect, and the correction }

procedure TFitLossTest.LegacyRewardsInflatingTheModel;
var
  Big: TLossDoubleArray;
  i: integer;
  Before, After: double;
begin
  //  The SAME model, ten times larger, compared under the scaling the engine
  //  applies when curve scaling is on: s = sum(obs)/sum(calc). The scaled model
  //  is point-for-point identical in both cases, so agreement with the data is
  //  EXACTLY the same - only the denominator changed.
  Big := TLossDoubleArray.Create(20, 20, 20, 20);
  for i := 0 to High(Big) do
    AssertEquals('the scaled models must be identical, or this proves nothing',
      Calc[i] * (10 / 8), Big[i] * (10 / 80), 1e-12);

  Before := EvaluateLoss(LOSS_KIND_RFACTOR_LEGACY, Calc, Obs, 10 / 8);
  After := EvaluateLoss(LOSS_KIND_RFACTOR_LEGACY, Big, Obs, 10 / 80);

  AssertTrue('inflating the model must lower the legacy figure - this is the ' +
    'defect the corrected form exists to remove', After < Before);
end;

procedure TFitLossTest.CorrectedIsUnmovedByInflatingTheModel;
var Before, After: double;
begin
  Before := EvaluateLoss(LOSS_KIND_RFACTOR, Calc, Obs, 10 / 8);
  After := EvaluateLoss(LOSS_KIND_RFACTOR,
    TLossDoubleArray.Create(20, 20, 20, 20), Obs, 10 / 80);
  AssertEquals('a pure change of model scale must not change the objective',
    Before, After, 1e-12);
end;

{ ----------------------------------------------------------- degenerate inputs }

procedure TFitLossTest.APerfectModelScoresZeroUnderEveryLoss;
var K: longint;
begin
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    AssertEquals(LossName(K) + ': an exact model scores zero', 0,
      EvaluateLoss(K, Obs, Obs, 1), 1e-12);
end;

procedure TFitLossTest.AZeroModelDoesNotDivideByZero;
var V: double;
begin
  //  Only the legacy form divides by the model, so only it is at risk here.
  V := EvaluateLoss(LOSS_KIND_RFACTOR_LEGACY,
    TLossDoubleArray.Create(0, 0, 0, 0), Obs, 1);
  AssertTrue('a zero model must not produce NaN or infinity',
    not IsNan(V) and not IsInfinite(V));
end;

procedure TFitLossTest.AZeroObservationDoesNotDivideByZero;
var
  K: longint;
  V: double;
  Zero: TLossDoubleArray;
begin
  Zero := TLossDoubleArray.Create(0, 0, 0, 0);
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
  begin
    V := EvaluateLoss(K, Calc, Zero, 1);
    AssertTrue(LossName(K) + ': all-zero data must not produce NaN or infinity',
      not IsNan(V) and not IsInfinite(V));
  end;
end;

procedure TFitLossTest.AnUnknownLossRaisesRatherThanScoringZero;
var Raised: boolean;
begin
  //  Returning 0 would read as a perfect fit, which is the worst possible way to
  //  fail: the fit would stop immediately and report success.
  Raised := False;
  try
    EvaluateLoss(LOSS_KIND_LAST + 1, Calc, Obs, 1);
  except
    on E: Exception do Raised := True;
  end;
  AssertTrue('an unknown loss kind must raise, never score zero', Raised);
end;

procedure TFitLossTest.EveryKnownKindIsNamedAndDescribed;
var K: longint;
begin
  //  Self-enforcing: a new loss kind without a name or description reaches the
  //  menu as "Unknown" or as a blank tooltip, and this fails first.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
  begin
    AssertTrue(Format('kind %d must be known', [K]), IsKnownLoss(K));
    AssertTrue(Format('kind %d needs a name', [K]),
      (LossName(K) <> '') and (LossName(K) <> 'Unknown'));
    AssertTrue(Format('kind %d needs a description', [K]),
      Length(LossDescription(K)) > 20);
  end;
  AssertFalse('the range must end where it says it does',
    IsKnownLoss(LOSS_KIND_LAST + 1));
end;

{ ------------------------------------------------------------- compatibility }

procedure TFitLossTest.OnlyTheLegacyFormIsSelfNormalising;
var K: longint;
begin
  AssertTrue('the legacy form divides by the model',
    LossIsSelfNormalising(LOSS_KIND_RFACTOR_LEGACY));
  //  Written over the whole range rather than a sub-range, so it does not
  //  silently stop testing anything if the constants are ever renumbered - which
  //  is exactly what happened when the corrected form was moved to 0.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    if K <> LOSS_KIND_RFACTOR_LEGACY then
      AssertFalse(LossName(K) + ' must not divide by the model',
        LossIsSelfNormalising(K));
end;

procedure TFitLossTest.ASelfNormalisingLossIsRefusedForAFreeAmplitude;
begin
  AssertFalse('the one refused pairing',
    LossAllowedForCapability(LOSS_KIND_RFACTOR_LEGACY, True));
end;

procedure TFitLossTest.EveryOtherPairingIsAllowed;
var K: longint;
begin
  //  Exhaustive over the whole matrix, so the rule cannot quietly grow a second
  //  clause without this failing.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
  begin
    AssertTrue(LossName(K) + ' must stay available to a bounded amplitude',
      LossAllowedForCapability(K, False));
    if K <> LOSS_KIND_RFACTOR_LEGACY then
      AssertTrue(LossName(K) + ' must stay available to a free amplitude',
        LossAllowedForCapability(K, True));
  end;
end;

procedure TFitLossTest.TheFallbackKeepsTheUsersIntent;
begin
  //  A user who chose an R-factor should land on the other R-factor, not on a
  //  differently-scaled measure they never asked for.
  AssertEquals('a free amplitude falls back to the corrected R-factor',
    LOSS_KIND_RFACTOR, DefaultLossFor(True));
  AssertEquals('everything else keeps the historical default',
    LOSS_KIND_RFACTOR_LEGACY, DefaultLossFor(False));
  AssertTrue('the fallback must itself be allowed',
    LossAllowedForCapability(DefaultLossFor(True), True));
end;

procedure TFitLossTest.TheRefusalExplainsItselfAndNamesTheAlternative;
var S: string;
begin
  //  A refusal the user cannot act on is not much better than a silent one.
  S := LossRefusalReason(LOSS_KIND_RFACTOR_LEGACY);
  AssertTrue('the reason must name the refused loss',
    Pos(LossName(LOSS_KIND_RFACTOR_LEGACY), S) > 0);
  AssertTrue('the reason must name what to use instead',
    Pos(LossName(LOSS_KIND_RFACTOR), S) > 0);
end;

{ WHICH OBJECTIVES A FORMULA BACKEND MAY BE GIVEN.

  The Python sidecar drives scipy's least_squares, which is handed a residual
  VECTOR and squares it - so it can only honour objectives that are a positive
  constant multiple of a sum of squares. Getting this classification wrong does
  not fail loudly: the sidecar would minimise something other than what was
  asked, and report success. Mirrored by
  test_every_loss_code_is_named_and_classified on the Python side. }
procedure TFitLossTest.OnlyConstantMultiplesOfSumOfSquaresSuitALeastSquaresSolver;
var K: longint;
begin
  //  Differs from the sum of squares only by the constant sum(obs)^2.
  AssertTrue('the corrected R-factor is a scaled sum of squares',
    LossIsLeastSquares(LOSS_KIND_RFACTOR));
  AssertTrue('sum of squares is the solver''s native form',
    LossIsLeastSquares(LOSS_KIND_SUMSQ));

  //  Denominator moves with the parameters - a ratio, not a sum of squares.
  AssertFalse('the model-normalised form is not a least-squares problem',
    LossIsLeastSquares(LOSS_KIND_RFACTOR_LEGACY));
  //  Absolute deviations are L1; a least-squares solver squares what it is given.
  AssertFalse('absolute deviation is not a least-squares problem',
    LossIsLeastSquares(LOSS_KIND_RELATIVE));

  //  Every kind must be classified one way or the other - a new loss that nobody
  //  classified would otherwise inherit whatever False happens to mean.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    AssertTrue(Format('kind %d must be classified', [K]), IsKnownLoss(K));

  //  A self-normalising loss can never be a least-squares one: that is the same
  //  fact stated twice, and the two must not be able to disagree.
  for K := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    if LossIsSelfNormalising(K) then
      AssertFalse(LossName(K) + ': self-normalising implies not least-squares',
        LossIsLeastSquares(K));
end;

{ ------------------------------- the pooled forms --------------------------- }

{ Whether AKind can be combined across intervals at all. Asked by TRYING it,
  because the property is declared by the presence of a pooler and there is no
  predicate to ask - which is itself worth knowing: a caller that wants to offer
  only the poolable objectives has to catch. }
function CanBePooled(AKind: longint; const AParts: TLossParts): boolean;
begin
  Result := True;
  try
    LossFromParts(AKind, AParts);
  except
    on Exception do
      Result := False;
  end;
end;

procedure TFitLossTest.OnlyTheSelfNormalisingFormRefusesToBePooled;
var
  Parts: TLossParts;
  k, Refused: longint;
begin
  //  A DECLARED PROPERTY, NOT A GAP. An objective normalised by the MODEL
  //  cannot be pooled, because the quantity it divides by is not a property of
  //  the data - so summing parts across intervals would divide by a total that
  //  means nothing. Exactly one of the four is like that, and it is the same
  //  one OnlyTheLegacyFormIsSelfNormalising names.
  //
  //  Counted over the whole range so that a fifth objective added later has to
  //  say which it is, rather than inheriting whichever answer it happens to get.
  Parts := LossPartsOf(Calc, Obs, 1);
  Refused := 0;
  for k := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    if not CanBePooled(k, Parts) then
    begin
      Inc(Refused);
      AssertTrue(LossName(k) + ' refuses pooling because it is self-normalising',
        LossIsSelfNormalising(k));
    end;
  AssertEquals('exactly one form cannot be pooled', 1, Refused);
end;

procedure TFitLossTest.EveryPoolableFormSurvivesAnAllZeroObservation;
var
  Parts: TLossParts;
  k: longint;
  v: double;
begin
  //  AN ALL-ZERO OBSERVED INTERVAL IS REAL: a stretch of a profile with no
  //  counts in it, which a user may perfectly well include. Every pooled form
  //  divides by an observed total, so each carries its own guard - and they are
  //  separate code from the per-point guards already tested here.
  //
  //  WALKED OVER EVERY KIND, so a fifth objective added later has to carry the
  //  guard too. A missing one is a division by zero inside the objective, which
  //  with the exception mask the fit runs under is an Inf that propagates into
  //  the R-factor the user is shown.
  Parts := LossPartsOf(TLossDoubleArray.Create(0, 0), TLossDoubleArray.Create(0, 0), 1);
  for k := LOSS_KIND_FIRST to LOSS_KIND_LAST do
  begin
    //  The self-normalising form is not poolable at all; the test above says
    //  which one and why.
    if not CanBePooled(k, Parts) then
      Continue;
    v := LossFromParts(k, Parts);
    AssertTrue(Format('%s pooled a zero interval to a finite number (%g)',
      [LossName(k), v]), not IsInfinite(v));
    AssertFalse(Format('%s pooled it to a number at all', [LossName(k)]),
      IsNan(v));
  end;
end;

procedure TFitLossTest.APooledTotalOfOneIntervalIsThatIntervalScore;
var
  Parts: TLossParts;
  k: longint;
begin
  //  THE IDENTITY THAT SAYS THE POOLED FORM IS THE SAME OBJECTIVE. With one
  //  interval there is nothing to combine, so pooling must answer exactly what
  //  evaluating it does - and a pooled form normalising differently would make
  //  a fit over one interval score differently from the same fit "pooled",
  //  which is how a pooled total comes to be compared against a per-interval
  //  one and read as an improvement.
  Parts := LossPartsOf(Calc, Obs, 1);
  for k := LOSS_KIND_FIRST to LOSS_KIND_LAST do
    if CanBePooled(k, Parts) then
      AssertEquals(LossName(k) + ' pooled over one interval',
        EvaluateLoss(k, Calc, Obs, 1), LossFromParts(k, Parts), 1e-12);
end;

procedure TFitLossTest.AnUnknownKindCannotBePooledEither;
var
  Parts: TLossParts;
  Raised: boolean;
begin
  //  THE OTHER ENTRY POINT. The per-point one is checked above; this one is
  //  reached by the multi-interval path, and answering zero here would report a
  //  perfect fit for an objective that does not exist.
  Parts := LossPartsOf(Calc, Obs, 1);
  Raised := False;
  try
    LossFromParts(LOSS_KIND_LAST + 99, Parts);
  except
    on Exception do
      Raised := True;
  end;
  AssertTrue('an unknown kind raises rather than pooling to a number', Raised);
end;

{ --------------------------- registering an objective ----------------------- }

procedure TFitLossTest.AnObjectiveWithNoNameIsRefused;
var
  Info: TLossInfo;
  Raised: boolean;
begin
  //  NOTHING COULD OFFER IT. The name is what the settings store, what the menu
  //  shows and what a REST caller sends, so an objective without one is
  //  registered and unreachable - present in the count and absent from every
  //  list.
  Info := Default(TLossInfo);
  Info.Kind := LOSS_KIND_LAST + 50;
  Info.Evaluate := nil;
  Raised := False;
  try
    RegisterLoss(Info);
  except
    on E: ELossRegistration do
      Raised := True;
  end;
  AssertTrue('refused', Raised);
end;

procedure TFitLossTest.AnObjectiveWithNoWayToEvaluateItIsRefused;
var
  Info: TLossInfo;
  Raised: boolean;
begin
  //  WORSE THAN NO NAME, because it IS offered: the user selects it, the fit
  //  runs, and the objective call goes through a nil pointer.
  Info := Default(TLossInfo);
  Info.Kind := LOSS_KIND_LAST + 51;
  Info.Name := 'nameless-evaluator';
  Info.Evaluate := nil;
  Raised := False;
  try
    RegisterLoss(Info);
  except
    on E: ELossRegistration do
      Raised := True;
  end;
  AssertTrue('refused', Raised);
end;

procedure TFitLossTest.AndTheTwoRefusalsSayWhichIsWrong;
var
  Info: TLossInfo;
  NoName, NoEvaluator: string;
begin
  //  TWO FAULTS, TWO MESSAGES. Both are programming errors rather than user
  //  errors - they fire while a build registers what it offers - so the only
  //  reader is whoever added the objective, and a shared message would send
  //  them to check the wrong field.
  NoName := '';
  Info := Default(TLossInfo);
  Info.Kind := LOSS_KIND_LAST + 52;
  try
    RegisterLoss(Info);
  except
    on E: ELossRegistration do
      NoName := E.Message;
  end;

  NoEvaluator := '';
  Info := Default(TLossInfo);
  Info.Kind := LOSS_KIND_LAST + 53;
  Info.Name := 'has-a-name';
  try
    RegisterLoss(Info);
  except
    on E: ELossRegistration do
      NoEvaluator := E.Message;
  end;

  AssertTrue('the nameless one says so: ' + NoName,
    Pos('no name', NoName) > 0);
  AssertTrue('the other names the objective: ' + NoEvaluator,
    Pos('has-a-name', NoEvaluator) > 0);
  AssertTrue('and says what it lacks',
    Pos('evaluate', NoEvaluator) > 0);
  AssertTrue('they are not the same message', NoName <> NoEvaluator);
end;

initialization
  RegisterTest('unit', TFitLossTest);
end.
