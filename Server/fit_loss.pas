// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The objective functions a fit can minimise.)

The app had ONE hard-wired objective for 25 years. That is fine for a single
domain and wrong for a framework meant to carry several, which is what this
codebase is becoming - so the objective becomes a choice, and which choices are
legitimate for a given model is derived from what that model IS (D18).

Kept as plain functions over arrays, with no engine types, so the arithmetic can
be tested directly and cheaply.

THE FORMS, and why each exists:

  R-FACTOR          sum( (calc*s - obs)^2 ) / (sum obs)^2
      THE DEFAULT. Dividing by an integral is what makes an R-factor a
      dimensionless RELATIVE measure - the same sample measured with ten times
      the counting time must score the same - and that intent is correct and
      worth keeping. This form divides by the OBSERVED integral, as the standard
      diffraction R-factors (Rp, Rwp) do, which serves that intent with a
      denominator that is CONSTANT during the fit.

  LEGACY R-FACTOR   sum( (calc*s - obs)^2 ) / (sum calc)^2
      The original, kept so the two can be compared. Same intent, normalised by
      the MODEL instead - and for a good fit sum(calc) ~ sum(obs), which is why
      the choice looks immaterial and why this stood for 25 years.

      It is not immaterial once CURVE SCALING is on. The engine then sets
      s = (sum obs)/(sum calc), which makes calc*s - and hence the whole
      numerator - invariant under a change of model amplitude, while the
      denominator still grows with it. The objective is then lowered by
      INFLATING the model, along a direction that changes nothing about the
      agreement with the data. The defect is the INTERACTION of the two, not the
      normalisation on its own.

      A peak's amplitude is seeded from the data and stays near it, so the
      degenerate direction was never explored. A model with a free amplitude
      finds it immediately.

  SUM OF SQUARES    sum( (calc*s - obs)^2 )
      Unnormalised least squares. Minimised by the same parameters as the
      corrected R-factor - they differ only by the constant sum(obs)^2 - so it is
      here for interpretability, not for a different answer.

  RELATIVE          sum( |calc*s - obs| ) / sum( |obs| )
      The Rp form: absolute deviations over observed magnitude. Less sensitive to
      a few large misfits than a squared measure, and directly readable as "the
      model is off by this fraction".

SELF-NORMALISING is the property that matters for compatibility: a loss whose
denominator depends on the MODEL can be improved by changing the model's scale
rather than its shape. Only the legacy form has it, and D18's rule refuses that
combination for any curve type whose amplitude is unbounded.

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_loss;

{$mode objfpc}{$H+}

interface

uses
    Math, SysUtils;

const
    { THE DEFAULT, and deliberately 0: a TFitTask can be built through the
      inherited TComponent constructor, which leaves the field zero-initialised,
      so whatever 0 means is what unconfigured code silently gets. That must be
      the objective we would have chosen, not merely the oldest one. }
    LOSS_KIND_RFACTOR = 0;
    { The original form, normalised by the model. Kept for comparison. }
    LOSS_KIND_RFACTOR_LEGACY = 1;
    { Plain least squares. }
    LOSS_KIND_SUMSQ = 2;
    { Absolute deviations over observed magnitude (the Rp form). }
    LOSS_KIND_RELATIVE = 3;

    LOSS_KIND_FIRST = LOSS_KIND_RFACTOR;
    LOSS_KIND_LAST = LOSS_KIND_RELATIVE;

type
    TLossDoubleArray = array of double;

{ Human-readable name, for menus and logs. }
function LossName(AKind: longint): string;
{ A one-line explanation, shown where the user chooses. }
function LossDescription(AKind: longint): string;
{ True when the loss is normalised by a quantity derived from the MODEL, so its
  value can be changed by rescaling the model rather than by fitting it better.
  This is the fact D18's compatibility rule is derived from. }
function LossIsSelfNormalising(AKind: longint): boolean;
{ True when AKind names a loss this unit implements. }
function IsKnownLoss(AKind: longint): boolean;
{ True when minimising this objective is the same problem as minimising a sum of
  squared residuals - i.e. it can be handed to a least-squares solver as a
  residual VECTOR rather than a scalar.

  This is what the formula-based backends can accept: the Python sidecar drives
  scipy's least_squares (Trust Region Reflective), which is given residuals, not
  an objective value. A loss that is a positive CONSTANT multiple of the sum of
  squares has the same minimiser, so it qualifies even though the numbers differ;
  one whose denominator varies with the parameters, or which sums absolute rather
  than squared deviations, does not.

  Another capability in D18's sense: the engine derives from this whether a given
  loss can run on a given backend, instead of listing engine/loss pairs. }
function LossIsLeastSquares(AKind: longint): boolean;

{ Evaluates the objective. ACalc and AObs must be the same length; AScale is the
  factor the engine applies to the model before comparing (1 when curve scaling
  is off). }
function EvaluateLoss(AKind: longint;
    const ACalc, AObs: TLossDoubleArray; const AScale: double): double;

{ THE PARTS EVERY LOSS IS BUILT FROM, so several stretches of profile can be
  POOLED into one figure.

  Fitting intervals are separate sub-problems, so each is measured on its own -
  but the number the user reads is about the model as a whole, and a ratio is not
  additive. Adding two intervals each reading 0.01 gave 0.02, so marking a third,
  well-fitted interval made the reported fit look worse. Pooling the parts and
  dividing once is the same arithmetic one interval already does, which is what
  makes the figure mean the same thing however the profile is divided up. }
type
    TLossParts = record
        SumSq:     double;
        SumAbs:    double;
        SumObs:    double;
        SumAbsObs: double;
    end;

function LossPartsOf(const ACalc, AObs: TLossDoubleArray;
    const AScale: double): TLossParts;
procedure AddLossParts(var ATotal: TLossParts; const APart: TLossParts);
function LossFromParts(AKind: longint; const AParts: TLossParts): double;

{ ---------------------------------------------------------------------------
  The objectives this build offers.

  WHAT THIS REPLACES: four case statements over the kind - name, description,
  arithmetic over one stretch, arithmetic pooled across intervals - plus a range
  check standing in for "is this a real objective". Five places to edit to add
  one, and the range check would happily accept a gap between two of them.

  An objective now declares itself once. Its two pieces of arithmetic are
  function pointers rather than branches, so a module can contribute one without
  touching this unit (D18).
  --------------------------------------------------------------------------- }
type
    { Everything an objective can be computed from over one stretch of profile.

      TLossParts plus the MODEL integral - which only the legacy form needs, and
      which is precisely why that form cannot be pooled across intervals: it is
      the one quantity that is not a property of the data. }
    TLossTerms = record
        SumSq:     double;
        SumAbs:    double;
        SumObs:    double;
        SumAbsObs: double;
        SumCalc:   double;
    end;

    { The objective over one stretch. }
    TLossEvaluator = function(const ATerms: TLossTerms): double;
    { The objective over parts pooled from several intervals, or nil when the
      objective cannot be pooled - nil is the declaration, not an oversight, and
      it is what LossFromParts refuses on. }
    TLossPooler = function(const AParts: TLossParts): double;

    TLossInfo = record
        { Persisted in settings and sent over REST; never change a shipped one. }
        Kind: longint;
        Name: string;
        Description: string;
        { Normalised by a quantity derived from the MODEL, so its value can be
          changed by rescaling the model instead of fitting it better. The fact
          D18's compatibility rule is derived from. }
        IsSelfNormalising: boolean;
        { Minimising it is the same problem as minimising a sum of squared
          residuals, so a least-squares solver can be given a residual VECTOR. }
        IsLeastSquares: boolean;
        Evaluate: TLossEvaluator;
        Pool: TLossPooler;
    end;

    TLossInfoArray = array of TLossInfo;

    ELossRegistration = class(Exception);

{ Registers an objective. Raises on a duplicate kind or a missing evaluator. }
procedure RegisterLoss(const AInfo: TLossInfo);
{ Everything registered, in registration order - which is menu order, so the
  default registers first. }
function RegisteredLosses: TLossInfoArray;
function FindLoss(AKind: longint; out AInfo: TLossInfo): boolean;
{ Registers the objectives this unit implements. Idempotent; called by anything
  that needs the list, so no host has to remember to do it at start-up. }
procedure RegisterBuiltInLosses;

{ The registered objectives as "0 (R-factor), 2 (Sum of squares), ...", for an
  error message that tells the user what to use instead. }
function KnownLossNames: string;

implementation

var
    Registry: TLossInfoArray;

{ ------------------------- the built-in objectives ------------------------- }

{ The original form, normalised by the MODEL's integral. Its guard is reproduced
  exactly: a zero model integral becomes 1 rather than dividing by zero. }
function EvaluateRFactorLegacy(const ATerms: TLossTerms): double;
var
    SumCalc: double;
begin
    SumCalc := ATerms.SumCalc;
    if SumCalc = 0 then
        SumCalc := 1;
    Result := ATerms.SumSq / Sqr(SumCalc);
end;

function EvaluateRFactor(const ATerms: TLossTerms): double;
var
    SumObs: double;
begin
    SumObs := ATerms.SumObs;
    if SumObs = 0 then
        SumObs := 1;
    Result := ATerms.SumSq / Sqr(SumObs);
end;

function PoolRFactor(const AParts: TLossParts): double;
var
    SumObs: double;
begin
    SumObs := AParts.SumObs;
    if SumObs = 0 then
        SumObs := 1;
    Result := AParts.SumSq / Sqr(SumObs);
end;

function EvaluateSumSq(const ATerms: TLossTerms): double;
begin
    Result := ATerms.SumSq;
end;

function PoolSumSq(const AParts: TLossParts): double;
begin
    Result := AParts.SumSq;
end;

function EvaluateRelative(const ATerms: TLossTerms): double;
var
    SumAbsObs: double;
begin
    SumAbsObs := ATerms.SumAbsObs;
    if SumAbsObs = 0 then
        SumAbsObs := 1;
    Result := ATerms.SumAbs / SumAbsObs;
end;

function PoolRelative(const AParts: TLossParts): double;
var
    SumAbsObs: double;
begin
    SumAbsObs := AParts.SumAbsObs;
    if SumAbsObs = 0 then
        SumAbsObs := 1;
    Result := AParts.SumAbs / SumAbsObs;
end;

{ ------------------------------ the registry ------------------------------- }

function RegisteredLosses: TLossInfoArray;
begin
    RegisterBuiltInLosses;
    Result := Registry;
end;

function FindLoss(AKind: longint; out AInfo: TLossInfo): boolean;
var
    i: longint;
begin
    RegisterBuiltInLosses;
    Result := False;
    AInfo := Default(TLossInfo);
    for i := 0 to High(Registry) do
        if Registry[i].Kind = AKind then
        begin
            AInfo := Registry[i];
            Exit(True);
        end;
end;

procedure RegisterLoss(const AInfo: TLossInfo);
var
    Existing: TLossInfo;
    i: longint;
begin
    if AInfo.Name = '' then
        raise ELossRegistration.Create(
            'an objective was registered with no name, so nothing could offer it');
    if not Assigned(AInfo.Evaluate) then
        raise ELossRegistration.Create(AInfo.Name +
            ' was registered without a way to evaluate it');
    //  Deliberately NOT through FindLoss: that registers the built-ins first,
    //  which would recurse while they are being registered.
    for i := 0 to High(Registry) do
        if Registry[i].Kind = AInfo.Kind then
        begin
            Existing := Registry[i];
            raise ELossRegistration.CreateFmt(
                'loss kind %d is claimed by both "%s" and "%s"',
                [AInfo.Kind, Existing.Name, AInfo.Name]);
        end;

    SetLength(Registry, Length(Registry) + 1);
    Registry[High(Registry)] := AInfo;
end;

var
    BuiltInsRegistered: boolean = False;

procedure RegisterBuiltInLosses;
var
    Info: TLossInfo;
begin
    if BuiltInsRegistered then
        Exit;
    //  Before the first registration, so a reentrant call from RegisterLoss
    //  cannot start the list again.
    BuiltInsRegistered := True;

    Info := Default(TLossInfo);
    Info.Kind := LOSS_KIND_RFACTOR;
    Info.Name := 'R-factor';
    Info.Description := 'Squared deviations normalised by the data, so values ' +
        'are comparable between datasets.';
    Info.IsSelfNormalising := False;
    //  Differs from SUMSQ only by the constant sum(obs)^2, which cannot move the
    //  minimum, so it is ordinary least squares.
    Info.IsLeastSquares := True;
    Info.Evaluate := @EvaluateRFactor;
    Info.Pool := @PoolRFactor;
    RegisterLoss(Info);

    Info := Default(TLossInfo);
    Info.Kind := LOSS_KIND_RFACTOR_LEGACY;
    Info.Name := 'R-factor (legacy)';
    Info.Description := 'The original objective. Normalised by the model rather '
        + 'than by the data, so with curve scaling on it can be lowered by '
        + 'inflating the model. Kept for comparison.';
    //  The only form that divides by a model quantity.
    Info.IsSelfNormalising := True;
    //  Its denominator depends on the model, so it is a ratio being minimised
    //  rather than a sum of squares - the very property that makes it gameable.
    Info.IsLeastSquares := False;
    Info.Evaluate := @EvaluateRFactorLegacy;
    //  No pooling: the model integral is not a property of the data, so parts
    //  from different intervals cannot be added and divided once.
    Info.Pool := nil;
    RegisterLoss(Info);

    Info := Default(TLossInfo);
    Info.Kind := LOSS_KIND_SUMSQ;
    Info.Name := 'Sum of squares';
    Info.Description := 'Plain least squares, with no normalisation at all - '
        + 'the value scales with the data, so compare it only within one '
        + 'dataset.';
    Info.IsSelfNormalising := False;
    Info.IsLeastSquares := True;
    Info.Evaluate := @EvaluateSumSq;
    Info.Pool := @PoolSumSq;
    RegisterLoss(Info);

    Info := Default(TLossInfo);
    Info.Kind := LOSS_KIND_RELATIVE;
    Info.Name := 'Relative deviation';
    Info.Description := 'Absolute deviations as a fraction of the data - reads '
        + 'directly as "the model is off by this much".';
    Info.IsSelfNormalising := False;
    //  Absolute deviations are an L1 problem: a least-squares solver squares
    //  whatever vector it is given, so no residual vector expresses this.
    Info.IsLeastSquares := False;
    Info.Evaluate := @EvaluateRelative;
    Info.Pool := @PoolRelative;
    RegisterLoss(Info);
end;

{ --------------------------- the derived answers --------------------------- }

function KnownLossNames: string;
var
    Losses: TLossInfoArray;
    i: longint;
begin
    Result := '';
    Losses := RegisteredLosses;
    for i := 0 to High(Losses) do
    begin
        if Result <> '' then
            Result := Result + ', ';
        Result := Result + Format('%d (%s)', [Losses[i].Kind, Losses[i].Name]);
    end;
end;

function IsKnownLoss(AKind: longint): boolean;
var
    Info: TLossInfo;
begin
    //  Asked of the registry rather than of a numeric range: a range accepts a
    //  gap between two real objectives, and an objective a module contributed
    //  would sit outside it entirely.
    Result := FindLoss(AKind, Info);
end;

function LossName(AKind: longint): string;
var
    Info: TLossInfo;
begin
    if FindLoss(AKind, Info) then
        Result := Info.Name
    else
        Result := 'Unknown';
end;

function LossDescription(AKind: longint): string;
var
    Info: TLossInfo;
begin
    Result := '';
    if FindLoss(AKind, Info) then
        Result := Info.Description;
end;

function LossIsSelfNormalising(AKind: longint): boolean;
var
    Info: TLossInfo;
begin
    Result := FindLoss(AKind, Info) and Info.IsSelfNormalising;
end;

function LossIsLeastSquares(AKind: longint): boolean;
var
    Info: TLossInfo;
begin
    Result := FindLoss(AKind, Info) and Info.IsLeastSquares;
end;

function LossPartsOf(const ACalc, AObs: TLossDoubleArray;
    const AScale: double): TLossParts;
var
    i, N: integer;
    D: double;
begin
    Result := Default(TLossParts);
    N := Min(Length(ACalc), Length(AObs));
    for i := 0 to N - 1 do
    begin
        D := ACalc[i] * AScale - AObs[i];
        Result.SumSq := Result.SumSq + D * D;
        Result.SumAbs := Result.SumAbs + Abs(D);
        Result.SumObs := Result.SumObs + AObs[i];
        Result.SumAbsObs := Result.SumAbsObs + Abs(AObs[i]);
    end;
end;

procedure AddLossParts(var ATotal: TLossParts; const APart: TLossParts);
begin
    ATotal.SumSq := ATotal.SumSq + APart.SumSq;
    ATotal.SumAbs := ATotal.SumAbs + APart.SumAbs;
    ATotal.SumObs := ATotal.SumObs + APart.SumObs;
    ATotal.SumAbsObs := ATotal.SumAbsObs + APart.SumAbsObs;
end;

{ The legacy kind is absent on purpose: it normalises by the MODEL's integral,
  which these parts do not carry. It is a comparison baseline for a single task
  and is not offered as a pooled total. }
function LossFromParts(AKind: longint; const AParts: TLossParts): double;
var
    Info: TLossInfo;
begin
    if not FindLoss(AKind, Info) then
        raise Exception.CreateFmt('Unknown loss function kind %d.', [AKind]);
    if not Assigned(Info.Pool) then
        //  A declared property of the objective, not a gap in a case statement:
        //  an objective normalised by the model cannot be pooled, because the
        //  quantity it divides by is not a property of the data.
        raise Exception.CreateFmt(
            'Loss function kind %d cannot be pooled across intervals.',
            [AKind]);
    Result := Info.Pool(AParts);
end;

function EvaluateLoss(AKind: longint;
    const ACalc, AObs: TLossDoubleArray; const AScale: double): double;
var
    i, N: integer;
    D: double;
    Terms: TLossTerms;
    Info: TLossInfo;
begin
    //  Looked up BEFORE the sums are accumulated: an unknown kind must not
    //  silently become "zero error", which would look like a perfect fit, and
    //  there is no reason to do the work first.
    if not FindLoss(AKind, Info) then
        raise Exception.CreateFmt('Unknown loss function kind %d.', [AKind]);

    N := Min(Length(ACalc), Length(AObs));
    Terms := Default(TLossTerms);
    for i := 0 to N - 1 do
    begin
        D := ACalc[i] * AScale - AObs[i];
        Terms.SumSq := Terms.SumSq + D * D;
        Terms.SumAbs := Terms.SumAbs + Abs(D);
        Terms.SumCalc := Terms.SumCalc + ACalc[i];
        Terms.SumObs := Terms.SumObs + AObs[i];
        Terms.SumAbsObs := Terms.SumAbsObs + Abs(AObs[i]);
    end;

    Result := Info.Evaluate(Terms);
end;

end.
