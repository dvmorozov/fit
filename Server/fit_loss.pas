// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The objective functions a fit can minimise.)

The app had ONE hard-wired objective for 25 years. That is fine for a single
domain and wrong for a framework meant to carry several, which is what this
codebase is becoming - so the objective becomes a choice, and which choices are
legitimate for a given model is derived from what that model IS.

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
rather than its shape. Only the legacy form has it, and the compatibility rule refuses that
combination for any curve type whose amplitude is unbounded.

Copyright (C) Dmitry Morozov
}
unit fit_loss;

{$mode objfpc}{$H+}

interface

uses
    Math, SysUtils, explanation, explanation_registry;

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
  This is the fact the compatibility rule (capabilities, not type names) is derived from. }
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

  Another capability, not a type name: the engine derives from this whether a given
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
  touching this unit.
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
        { What the objective is, why it exists, how much authority stands
          behind it, what it does not cover and where to read more. Only the
          parts an objective alone knows are filled in: its body, standing,
          quote, limitations, references and related topics. The topic, the
          title (its Name) and the summary (its Description) are made from the
          fields above, so they cannot disagree with what the menu shows. An
          objective registered with none is reported by the explanation
          registry's completeness walk, by name. }
        Explanation: TExplanation;
        { Normalised by a quantity derived from the MODEL, so its value can be
          changed by rescaling the model instead of fitting it better. The fact
          the compatibility rule is derived from. }
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
{ 'losses/<kind>': what an objective is explained under. The kind is what
  settings and the wire already name an objective by, so the topic is as stable
  as they are. }
function LossTopic(AKind: longint): string;
{ The explanation an objective gives of itself: its declared Explanation with
  the topic, title and summary its other fields supply. }
function ExplainLoss(const AInfo: TLossInfo): TExplanation;
{ The one provider explaining every registered objective, registered with the
  explanation registry by the first RegisterLoss. }
function LossExplanationProvider: IExplanationProvider;
{ Registers the objectives this unit implements. Idempotent; called by anything
  that needs the list, so no host has to remember to do it at start-up. }
procedure RegisterBuiltInLosses;

{ The registered objectives as "0 (R-factor), 2 (Sum of squares), ...", for an
  error message that tells the user what to use instead. }
function KnownLossNames: string;

{ AKind when it names a registered objective, the R-factor otherwise - what a
  settings file written by another build may hold. }
function LossKindOrDefault(AKind: longint): longint;

implementation

type
    TLossExplanations = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

const
    LossesNamespace = 'losses';

var
    Registry: TLossInfoArray;
    Explanations: TLossExplanations = nil;

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
    //  REGISTERING IS EXPLAINING, as for an implementation: no second
    //  registration to forget. Idempotent by namespace.
    RegisterExplanationProvider(LossExplanationProvider);
end;

var
    BuiltInsRegistered: boolean = False;

const
    McCuskerWork = 'McCusker, L. B., Von Dreele, R. B., Cox, D. E., Louer, ' +
        'D. and Scardi, P., "Rietveld refinement guidelines", Journal of ' +
        'Applied Crystallography 32, 36-50 (1999)';
    McCuskerUrl = 'https://doi.org/10.1107/S0021889898009856';

{ What the default objective says of itself. THE STANDING IS THIS SOFTWARE'S
  CHOICE, not the field's: Rp and Rwp are the field's, and this is neither. }
function RFactorExplanation: TExplanation;
begin
    Result := Default(TExplanation);
    Result.Standing := esModelChoice;
    AddParagraph(Result, 'Over the points inside the fit intervals it is');
    AddParagraph(Result, 'R = sum( (s * calc - obs)^2 ) / ( sum obs )^2');
    AddParagraph(Result, 'where obs is a measured value, calc the model''s ' +
        'value at the same point and s the curve-scaling factor (1 when curve ' +
        'scaling is off).');
    AddParagraph(Result, 'Dividing by an integral makes it a relative ' +
        'measure: the same sample measured for ten times as long scores the ' +
        'same. It divides by the integral of the DATA, which does not change ' +
        'while the fit runs, as the profile R-factors of Rietveld refinement ' +
        'do; so the value cannot be lowered by changing the model''s scale ' +
        'instead of its shape.');
    AddParagraph(Result, 'It differs from Sum of squares only by the constant ' +
        'factor (sum obs)^2, so the two find the same parameters; this is ' +
        'ordinary least squares, which is why another engine that minimises ' +
        'squared residuals can run it.');
    AddParagraph(Result, 'It is the default, and kind 0 on purpose: what an ' +
        'unconfigured fit task gets should be the objective chosen, not merely ' +
        'the oldest.');
    AddLimitation(Result, 'It is unweighted, squared and has no square root, ' +
        'so it is neither Rp nor Rwp and is not directly comparable with one ' +
        'quoted elsewhere.');
    AddLimitation(Result, 'Only the points inside the fit intervals count.');
    AddReference(Result, McCuskerWork, '', McCuskerUrl);
    AddRelated(Result, LossTopic(LOSS_KIND_SUMSQ));
    AddRelated(Result, LossTopic(LOSS_KIND_RFACTOR_LEGACY));
    AddRelated(Result, LossTopic(LOSS_KIND_RELATIVE));
end;

function RFactorLegacyExplanation: TExplanation;
begin
    Result := Default(TExplanation);
    Result.Standing := esModelChoice;
    AddParagraph(Result, 'Over the points inside the fit intervals it is');
    AddParagraph(Result, 'R = sum( (s * calc - obs)^2 ) / ( sum calc )^2');
    AddParagraph(Result, 'It is this program''s original objective, kept so ' +
        'the two forms can be compared. Its intent is the same as the ' +
        'R-factor''s - a relative measure - but it divides by the integral of ' +
        'the MODEL.');
    AddParagraph(Result, 'For a good fit sum(calc) is close to sum(obs), so ' +
        'the choice looks immaterial, which is why it stood for years. With ' +
        'curve scaling on it is not: the engine then sets s = (sum obs) / ' +
        '(sum calc), which makes the numerator unchanged by the model''s ' +
        'amplitude while the denominator still grows with it. The value can ' +
        'then be lowered by inflating the model, along a direction that does ' +
        'nothing for the agreement with the data.');
    AddParagraph(Result, 'That is why it is greyed for a curve type whose ' +
        'amplitude is free to grow, and why it is replaced by the R-factor if ' +
        'it was selected.');
    AddLimitation(Result, 'It cannot be pooled across fit intervals: the model ' +
        'integral is not a property of the data, so only one interval at a ' +
        'time can be measured by it.');
    AddLimitation(Result, 'It minimises a ratio whose denominator moves with ' +
        'the model, not a sum of squares, so an engine that minimises squared ' +
        'residuals cannot run it.');
    AddRelated(Result, LossTopic(LOSS_KIND_RFACTOR));
end;

function SumOfSquaresExplanation: TExplanation;
begin
    Result := Default(TExplanation);
    Result.Standing := esConvention;
    AddParagraph(Result, 'Over the points inside the fit intervals it is');
    AddParagraph(Result, 'S = sum( (s * calc - obs)^2 )');
    AddParagraph(Result, 'Least squares is the usual objective for fitting a ' +
        'model to data whose errors are independent and of equal size; it is ' +
        'the maximum-likelihood choice when they are Gaussian. It is a ' +
        'convention of the field, not a requirement.');
    AddParagraph(Result, 'It is minimised by the same parameters as the ' +
        'R-factor, which differs from it only by the constant (sum obs)^2. It ' +
        'is offered for interpretability: the value is in the units of the ' +
        'data squared.');
    AddLimitation(Result, 'Nothing normalises it, so its value scales with ' +
        'the data; compare it only within one dataset.');
    AddLimitation(Result, 'It is unweighted: every point counts alike, ' +
        'whatever its uncertainty, so it is not a chi-squared.');
    AddReference(Result, 'Press, W. H., Teukolsky, S. A., Vetterling, W. T. ' +
        'and Flannery, B. P., Numerical Recipes: The Art of Scientific ' +
        'Computing, 3rd edition, Cambridge University Press (2007)',
        'Section 15.1, Least Squares as a Maximum Likelihood Estimator', '');
    AddRelated(Result, LossTopic(LOSS_KIND_RFACTOR));
end;

function RelativeExplanation: TExplanation;
begin
    Result := Default(TExplanation);
    Result.Standing := esConvention;
    AddParagraph(Result, 'Over the points inside the fit intervals it is');
    AddParagraph(Result, 'D = sum( |s * calc - obs| ) / sum( |obs| )');
    AddParagraph(Result, 'It is the form of the profile R-factor Rp of ' +
        'Rietveld refinement: absolute deviations over the observed ' +
        'magnitude. It reads directly as the fraction by which the model is ' +
        'off, and, since it does not square the deviations, a few large ' +
        'misfits count for less than they do in a squared measure.');
    AddParagraph(Result, 'Absolute deviations are not a sum of squares, so ' +
        'an engine that minimises squared residuals cannot run it; the fit ' +
        'runs on the built-in engine instead.');
    AddLimitation(Result, 'Its surface has a kink wherever a residual crosses ' +
        'zero, and a downhill simplex can stall on the flat facets between ' +
        'them. On the diffraction profile it was measured on, it reached ' +
        '0.0636 by its own measure where the squared form reached 0.0487. The ' +
        'cause is not established.');
    AddLimitation(Result, 'Only the points inside the fit intervals count.');
    AddReference(Result, McCuskerWork, '', McCuskerUrl);
    AddRelated(Result, LossTopic(LOSS_KIND_RFACTOR));
end;

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
    Info.Explanation := RFactorExplanation;
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
    Info.Explanation := RFactorLegacyExplanation;
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
    Info.Explanation := SumOfSquaresExplanation;
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
    Info.Explanation := RelativeExplanation;
    Info.IsSelfNormalising := False;
    //  Absolute deviations are an L1 problem: a least-squares solver squares
    //  whatever vector it is given, so no residual vector expresses this.
    Info.IsLeastSquares := False;
    Info.Evaluate := @EvaluateRelative;
    Info.Pool := @PoolRelative;
    RegisterLoss(Info);
end;

{ ------------------------------ the explanations --------------------------- }

function LossTopic(AKind: longint): string;
begin
    Result := LossesNamespace + '/' + IntToStr(AKind);
end;

function ExplainLoss(const AInfo: TLossInfo): TExplanation;
var
    Summary: string;
begin
    Result := AInfo.Explanation;
    Result.Topic := LossTopic(AInfo.Kind);
    Result.Title := AInfo.Name;
    Summary := Trim(AInfo.Description);
    if (Summary <> '') and not (Summary[Length(Summary)] in ['.', '!', '?']) then
        Summary := Summary + '.';
    Result.Summary := Summary;
end;

function TLossExplanations.Namespace: string;
begin
    Result := LossesNamespace;
end;

function TLossExplanations.StaticTopics: TStringArray;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, Length(Registry));
    for i := 0 to High(Registry) do
        Result[i] := LossTopic(Registry[i].Kind);
end;

function TLossExplanations.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
var
    i: longint;
begin
    AExplanation := Default(TExplanation);
    for i := 0 to High(Registry) do
        if LossTopic(Registry[i].Kind) = ATopic then
        begin
            AExplanation := ExplainLoss(Registry[i]);
            Exit(True);
        end;
    Result := False;
end;

function LossExplanationProvider: IExplanationProvider;
begin
    if not Assigned(Explanations) then
        Explanations := TLossExplanations.Create;
    Result := Explanations;
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

function LossKindOrDefault(AKind: longint): longint;
begin
    if IsKnownLoss(AKind) then
        Result := AKind
    else
        Result := LOSS_KIND_RFACTOR;
end;

finalization
    Explanations.Free;
end.
