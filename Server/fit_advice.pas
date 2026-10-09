// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What a fit will ACTUALLY do, and how to say so in plain language.)

The engine quietly corrects a few choices that cannot be honoured: a formula
backend cannot evaluate a curve that has no formula, cannot minimise an
objective that is not a sum of squares, and a self-normalising objective is
meaningless for a model that sets its own amplitude. Each correction is right.
Each is also INVISIBLE - the user selects one thing, a different thing runs, and
the only trace is a line in a log nobody opens.

That is the gap this unit exists to close, and it closes it structurally: the
decisions are made HERE, once, and both the engine and the UI read the answer.
Not a UI copy of the engine's logic - a copy would drift, and a UI that
confidently explains something the engine no longer does is worse than no
explanation at all.

DELIBERATELY FREE OF ENGINE TYPES. It takes booleans and integers, not a
TFitTask, so the whole decision table can be tested exhaustively without
building a fit - and so the client can call it without dragging the engine in.

The advice is phrased for someone who has not read the documentation, because
that is everyone. Each message says WHAT will happen, WHY, and - where there is
one - what to do instead.

Copyright (C) Dmitry Morozov
}
unit fit_advice;

{$mode objfpc}{$H+}

interface

uses
    fit_loss, fit_statistics, loss_compatibility, Math, sample_coverage,
    SysUtils;

type
    TFitAdvice = record
        { What will actually be minimised (may differ from what was asked). }
        LossKind: longint;
        { True when a formula backend was asked for but cannot be used, so the
          native engine will run instead. }
        FallsBackToNativeEngine: boolean;
        { True when the requested objective could not be honoured. }
        LossOverridden: boolean;
        { True when the global curve-scaling factor is switched off because the
          model sets its own amplitude. }
        CurveScalingDisabled: boolean;
        { One line for a status bar: what will happen. Never empty. }
        Summary: string;
        { The full explanation, for a dialog or a tooltip. Empty when nothing
          was overridden - there is then nothing to justify. }
        Detail: string;
    end;

{ THE SAMPLES NO CURVE COVERS, said for the History panel: how many, where,
  that the model is 0 there and they still count, how much of the squared
  difference they make (AShare, 0..1), and the two ways forward. Empty when
  there are none. Not an override - the fit does what was asked - but a figure
  the user can notice: one sample at the edge of a price series made a model
  score 70 times worse over an interval one sample longer. }
function AdviseUncoveredSamples(const ARanges: TSampleRanges;
    AShare: double): string;

{ The same in one line, for the status bar when a run ends. Empty for none. }
function UncoveredSamplesStatusText(ACount: longint): string;

{ What the dialog at the end of a run says about the samples no curve covers -
  AdviseUncoveredSamples' text - or '' when it is not to be shown this time.
  Said ONCE for the same stretches: AMemory is what was last said, updated to
  what to keep. The STRETCHES are remembered, not the words,
  because every fit moves the share and the words with it. Forgotten once
  nothing is uncovered, so the same gap coming back is said again. AAttended is
  False for a run nobody is watching - the window checking itself, a recording
  - which says nothing and keeps the memory as it was. }
function UncoveredSamplesToAnnounce(const ARanges: TSampleRanges;
    AShare: double; AAttended: boolean; var AMemory: string): string;

{ WHAT FIT > AUTOMATICALLY DID TO THE FIT INTERVALS: it replaces them with its
  own. AKept are the bounds in force before the run and ANew after it, each as
  consecutive (start, end) x pairs; ADataX the x of every sample of the data.
  Names both, the samples the old intervals fitted and the new ones do not, and
  how to fit them again. Empty when there were none before or nothing changed. }
function AdviseReplacedIntervals(const AKept, ANew, ADataX: array of double): string;

{ ASKED BEFORE A FIT, while the user can still correct it: the samples no curve
  covers, and whether to fit anyway. Empty when there is nothing to ask - none
  uncovered, already said for the same stretches, or nobody watching. Shares
  AMemory with the end-of-run dialog, so what was asked is not said again when
  the fit ends. }
function UncoveredSamplesBeforeFit(const AStatistics: TFitStatistics;
    AAttended: boolean; var AMemory: string): string;

{ What the window says in a dialog when a run ends: the run's own note, then
  the samples no curve covers - each said once for what it says (see
  UncoveredSamplesToAnnounce), both kept in the memories passed in. Empty when
  there is nothing new to say, or when nobody is watching (AAttended False). }
function EndOfRunNotice(const ANote: string; const AStatistics: TFitStatistics;
    AAttended: boolean; var ANoteMemory, AUncoveredMemory: string): string;

{ Works out what a fit with these settings will really do.

  AFormulaBackendRequested covers both out-of-process engines (the Python
  sidecar and the standalone compute server), because both fit by evaluating a
  curve's expression and so share every limitation that matters here. }
function AdviseFit(ALossKind: longint;
    AFormulaBackendRequested, ACurveIsAnalytic, AAmplitudeIsUnbounded,
    ACurveScalingRequested: boolean): TFitAdvice;

{ True when the fit will not do literally what was selected, so the user should
  be told rather than left to notice. }
function AdviceNeedsAttention(const AAdvice: TFitAdvice): boolean;

{ WHETHER TO SAY IT OUT LOUD THIS TIME, and what to remember having said.

  A message the user has already been shown for the selection they are still in
  must not be repeated - the advice is recomputed on every change of loss,
  minimizer, curve type and scaling flag, so repeating it would put a dialog in
  front of someone who is adjusting settings. But it must be shown AGAIN if they
  leave the problematic selection and come back to it, which is what makes this a
  rule about remembering rather than a flag that is set once.

  AAnnounce is whether this recomputation came from something the user just did:
  start-up recomputes the advice too, and a dialog on every launch for a setting
  chosen long ago is exactly how people learn to dismiss these unread.

  AMemory is what was last announced, and is updated to what to keep. The
  memory is CLEARED when the advice no longer needs attention, so that returning
  to a problematic selection explains itself afresh. }
function AdviceShouldBeAnnounced(AAnnounce: boolean;
    const AAdvice: TFitAdvice; var AMemory: string): boolean;

{ MOVING A POINT OF A MODULE'S OWN MARKUP, WHEN THE MODEL HAS BEEN FITTED.

  NOT the same case as moving a picked curve position, which is allowed: a pick
  carries an identity that a move takes with it, so its curve keeps the shape
  the optimiser found and is simply re-seeded where the user put it (see
  curve_identity_registry.TakeSeedFrom).

  A module's markup is different in kind. Its points are not one-per-curve: the
  whole markup is what places the instances, so moving ONE point re-derives
  EVERY instance the markup produced, all of them with new seeds. There is no
  correspondence to carry - the model after the move is a different set of
  curves, not the same set moved - so the whole model's fit goes, not one
  curve's.

  So this move is refused instead of performed. That is the one honest option of
  the three: performing it loses work the user cannot see was lost and cannot get
  back, and performing it with a warning still loses the work.

  AAnyCurveIsFitted is false before any fit. The move is then ordinary and is
  allowed.

  Returns True when the move may proceed. AReason carries the refusal - what will
  happen, why, and what to do instead - and is empty when the answer is True. }
function AdviseMoveMarkupPoint(AAnyCurveIsFitted: boolean;
    out AReason: string): boolean;

{ A VALUE TYPED INTO THE CURVE ATTRIBUTES TABLE THAT THE MODEL DID NOT KEEP AS
  TYPED.

  Every parameter keeps to its own range, and the engine brings a value outside
  it to the nearest one allowed: a width is no wider than the fit interval it is
  fitted in, a mixing fraction lies in [0, 1], an amplitude or a background
  level is not below zero, a position stays between the samples either side of
  its pick - and a background is lifted until it is nowhere below zero, its
  level carrying the lift. Each of those is right; each is also a value the user
  typed and then sees replaced in the redrawn table (non-negotiable 7).

  DECIDED BY COMPARISON, not by re-deriving the range: the engine's parameter
  is the one authority on what it holds, so what it holds after the write is the
  answer, and a copy of its rules here would drift. A difference within the
  rounding of a unit conversion is no hold.

  Returns True, with AReason saying what happened and why, when AHeld is not
  ATyped; False and an empty reason otherwise. }
function AdviseHeldParameterValue(const AName: string; ATyped, AHeld: double;
    out AReason: string): boolean;

{ A SAVED MODEL WHOSE VALUES THE ENGINE DID NOT KEEP AS SAVED, when a project
  opens or a history entry is made current.

  A project saved before a limit existed - a width wider than its interval, a
  background below zero - opens with each such value held at the nearest one
  allowed (AdviseHeldParameterValue says why for one typed value). The user
  sees numbers that are not the ones they saved, and is told so.

  AHeld names each value held otherwise ('sigma of curve 2'); empty means the
  model holds what was saved, and the answer is ''. }
function AdviseValuesHeldOnOpen(const AHeld: array of string): string;

type
    { THE BACKGROUND, TWO WAYS, AND NEVER BOTH.

      A background can be put under the peaks as a CURVE in the model - fitted,
      drawn and reported like them, and saved with the project - or by Enable
      Variation, the older hidden quadratic the engine adds inside every fit
      interval. Both add to the calculated profile, so both at once count the
      background twice and the peaks come out short by exactly the amount the
      second one took.

      REFUSED IN BOTH DIRECTIONS rather than one switched off silently: each is
      something the user chose, and overriding either is an override they would
      notice (non-negotiable 7). The setters, the menu and the engine all ask
      this one function. }
    TBackgroundModelAdvice = record
        { May Enable Variation be switched ON? Switching it off always may. }
        VariationAllowed: boolean;
        VariationReason: string;
        { May a background curve be put into the model? }
        BackgroundCurveAllowed: boolean;
        BackgroundCurveReason: string;
        { Whether the engine applies the variation in a fit. False when a
          background curve is in the model as well - only a hand-edited project
          gets there, and the curve wins because it is the one the user sees. }
        VariationInForce: boolean;
        { Whether Do All Automatically puts a background curve into a model
          that has none. }
        AutomaticRunAddsBackground: boolean;
        { What the automatic run says when it deliberately adds no background,
          for the status line. Empty when there is nothing to say. }
        AutomaticRunNote: string;
    end;

{ What may be added, and what the automatic run does, given which of the two
  backgrounds the problem holds now. }
function AdviseBackgroundModel(AVariationEnabled,
    ABackgroundCurveInModel: boolean): TBackgroundModelAdvice;

{ WHETHER Model > Background > Enable Variation IS OFFERED: always while it is
  on, so it can be switched off; otherwise only where AdviseBackgroundModel
  allows switching it on. The window greys the entry with this, and shows the
  refusal's reason as its hint. }
function VariationCommandEnabled(AVariationEnabled,
    ABackgroundCurveInModel: boolean): boolean;

{ WHETHER A CURVE TYPE MAY BE THE MODEL'S BACKGROUND.

  Only a type that declares itself a background shape
  (TNamedPointsSet.IsBackground): a peak shape placed as "the background" would
  be seeded, fitted and reduced by rules written for something else. A shape
  that is defined for positive x only (a power law) is refused over data that
  reaches zero or below, where it has no value to give - saying so here rather
  than letting the fit fail on a NaN.

  ADataMinX is the smallest x of the data the problem holds. True when the type
  may be used; AReason is empty then. }
function AdviseBackgroundCurveType(const ATypeName: string;
    AIsBackgroundType, AArgumentMustBePositive: boolean; ADataMinX: double;
    out AReason: string): boolean;

{ WHETHER A CURVE TYPE MAY JOIN THE MODEL: a model holds the curves of ONE
  module. A module covers one class of fitting tasks and is fully responsible
  for it, so curves of two modules in one model fit nothing either module
  understands - a decision taken with the user (fit-performance.md, stage 6).

  ATypeOwner is the module the type belongs to, AModelOwner the module of the
  type the model is made of now (curve_type_registration.CurveTypeOwner), and
  AModelHasContent whether the model holds anything yet - picks, curves, a
  module's markup. An empty model takes any type: choosing the first one is how
  a model gets its module. Owners are compared without regard to case. }
function AdviseCurveTypeChoice(const ATypeName, ATypeOwner, AModelOwner: string;
    AModelHasContent: boolean; out AReason: string): boolean;

{ WHETHER FIT INTERVALS > AUTO CAN SPLIT THIS MODEL. A module splits its own
  model (IModuleSession.ProposeFitIntervals); the framework's peak search is
  for the framework's own curve types, which are peaks (ATypeIsTheFrameworks).
  A model of a module that proposes nothing is refused rather than searched for
  peaks it is not made of (docs/internal/fit-performance.md, stage 7).

  AModelHasContent: an EMPTY model belongs to no module - any type may be
  chosen for it (AdviseCurveTypeChoice) - so the type selected over it says
  nothing yet about what the data is made of, and it is searched for peaks as
  every model was before a module could split one. Refusing it refused Auto on
  a diffraction project whose saved type was a module's (findings.md). }
function AdviseIntervalSearch(const ATypeName, ATypeOwner: string;
    ATypeIsTheFrameworks, AModuleProposed, AModelHasContent: boolean;
    out AReason: string): boolean;

{ WHAT A PROJECT SAVED BEFORE THE ONE-MODULE RULE IS TOLD when its model mixes
  modules - the framework's curve positions beside a module's own markup. It
  opens and fits as it was saved (nothing a user made is changed under them);
  this says why nothing more of another module can be added, and how to make
  the model one module's. }
function MixedModelWarning: string;

{ WHETHER THE MODEL TAKES A SEPARATE BACKGROUND CURVE. A module whose curves
  describe the whole of the data - their level included - declares that its
  types take none (TNamedPointsSet.AcceptsBackground); a background beside
  them would be counted twice. APeakTypeName is the type the model is made of. }
function AdviseBackgroundForModel(const APeakTypeName: string;
    AModelAcceptsBackground: boolean; out AReason: string): boolean;

{ WHETHER A CURVE TYPE MAY BE THE PEAK TYPE - the one every pick is built as.
  A background shape may not: it has no position to place it by, so every pick
  would make another copy of the same baseline. It belongs in Model >
  Background > Curve instead, and the refusal says so. }
function AdvisePeakCurveType(const ATypeName: string; AIsBackgroundType: boolean;
    out AReason: string): boolean;

{ The hint the Vary Background entry shows: its designed one, ADesignedHint,
  while variation is allowed, and the reason it is not otherwise - in the words
  the server would refuse with. }
function VariationHint(const AAdvice: TBackgroundModelAdvice;
    const ADesignedHint: string): string;

implementation

const
    CRLF2 = LineEnding + LineEnding;

function AdviseBackgroundModel(AVariationEnabled,
    ABackgroundCurveInModel: boolean): TBackgroundModelAdvice;
begin
    Result := Default(TBackgroundModelAdvice);

    Result.VariationAllowed := not ABackgroundCurveInModel;
    if not Result.VariationAllowed then
        Result.VariationReason :=
            'Enable Variation adds a background of its own inside every fit ' +
            'interval, and the model already has a background curve. With ' +
            'both, the background would be counted twice and the peaks would ' +
            'come out too small.' + CRLF2 +
            'Delete the background curve (Model > Background > Curve > None) ' +
            'first if you would rather use the variation.';

    Result.BackgroundCurveAllowed := not AVariationEnabled;
    if not Result.BackgroundCurveAllowed then
        Result.BackgroundCurveReason :=
            'A background curve cannot be added while Model > Background > ' +
            'Enable Variation is on: that option already adds a background ' +
            'inside every fit interval, and with both the background would be ' +
            'counted twice.' + CRLF2 +
            'Switch Enable Variation off first if you would rather have the ' +
            'background as a curve you can see, delete and save.';

    Result.VariationInForce := AVariationEnabled and not ABackgroundCurveInModel;
    Result.AutomaticRunAddsBackground :=
        (not AVariationEnabled) and (not ABackgroundCurveInModel);

    //  DECIDED BY THE USER, not by the engine: with the variation on and no
    //  curve, the run respects the option rather than switching it off and
    //  adding a curve behind their back - and says what it did instead, because
    //  "no background curve" after an automatic run otherwise reads as a
    //  failure to find one.
    if AVariationEnabled and not ABackgroundCurveInModel then
        Result.AutomaticRunNote :=
            'Background handled by Enable Variation; no background curve added.';
end;

function VariationCommandEnabled(AVariationEnabled,
    ABackgroundCurveInModel: boolean): boolean;
begin
    Result := AVariationEnabled or
        AdviseBackgroundModel(AVariationEnabled,
            ABackgroundCurveInModel).VariationAllowed;
end;

function AdviseBackgroundCurveType(const ATypeName: string;
    AIsBackgroundType, AArgumentMustBePositive: boolean; ADataMinX: double;
    out AReason: string): boolean;
begin
    AReason := '';
    if not AIsBackgroundType then
    begin
        AReason := Format('"%s" is a peak shape, not a background shape, so it ' +
            'cannot be the background of the model. Choose one of the shapes ' +
            'under Model > Background > Curve.', [ATypeName]);
        Exit(False);
    end;
    if AArgumentMustBePositive and (ADataMinX <= 0) then
    begin
        AReason := Format('"%s" is defined only where x is greater than zero, ' +
            'and this data reaches x = %s. Choose another background shape, or ' +
            'select a data interval that stays above zero.',
            [ATypeName, FloatToStr(ADataMinX)]);
        Exit(False);
    end;
    Result := True;
end;

function AdviseCurveTypeChoice(const ATypeName, ATypeOwner, AModelOwner: string;
    AModelHasContent: boolean; out AReason: string): boolean;
begin
    AReason := '';
    Result := (not AModelHasContent) or SameText(ATypeOwner, AModelOwner);
    if Result then
        Exit;
    AReason := Format('"%s" belongs to %s, and this model is made of %s ' +
        'curves. A model holds the curves of one module only: each module ' +
        'covers its own kind of task, and curves of two of them fit nothing ' +
        'either understands. To use %s, start a new project with File > New ' +
        'Project, or empty this model first with Model > Clear Model.',
        [ATypeName, ATypeOwner, AModelOwner, ATypeOwner]);
end;

function AdviseIntervalSearch(const ATypeName, ATypeOwner: string;
    ATypeIsTheFrameworks, AModuleProposed, AModelHasContent: boolean;
    out AReason: string): boolean;
begin
    AReason := '';
    Result := AModuleProposed or ATypeIsTheFrameworks or not AModelHasContent;
    if Result then
        Exit;
    AReason := Format('Fit intervals cannot be computed for this model: "%s" ' +
        'belongs to %s, which marks nothing to split it by yet - and searching ' +
        'it for peaks would split it by something it is not made of. Mark what ' +
        'the model is made of first, or pick the bounds yourself with Model > ' +
        'Fit Intervals > Start Manual Selection.', [ATypeName, ATypeOwner]);
end;

function MixedModelWarning: string;
begin
    Result := 'This project''s model holds curves of more than one module: ' +
        'curve positions for the program''s own curve types, and a module''s ' +
        'own markup. A model now holds one module''s curves only - each module ' +
        'covers its own kind of task, and curves of two of them fit nothing ' +
        'either understands. The project opens and fits as it was saved, but ' +
        'nothing more of another module can be added to it. To make it one ' +
        'module''s, clear the picks with Model > Curve Positions > Clear, or ' +
        'empty the model with Model > Clear Model.';
end;

function AdviseBackgroundForModel(const APeakTypeName: string;
    AModelAcceptsBackground: boolean; out AReason: string): boolean;
begin
    AReason := '';
    Result := AModelAcceptsBackground;
    if Result then
        Exit;
    AReason := Format('A model of "%s" takes no separate background curve: its ' +
        'own curves describe the whole of the data, its level included, so a ' +
        'background beside them would be counted twice. Leave Model > ' +
        'Background > Curve > None selected.', [APeakTypeName]);
end;

function AdvisePeakCurveType(const ATypeName: string; AIsBackgroundType: boolean;
    out AReason: string): boolean;
begin
    AReason := '';
    Result := not AIsBackgroundType;
    if not Result then
        AReason := Format('"%s" is a background shape: it has no position, so ' +
            'every curve position would make another copy of the same baseline. ' +
            'Put it under the peaks with Model > Background > Curve instead.',
            [ATypeName]);
end;

function AdviseMoveMarkupPoint(AAnyCurveIsFitted: boolean;
    out AReason: string): boolean;
begin
    AReason := '';
    Result := not AAnyCurveIsFitted;
    if Result then
        Exit;

    //  Phrased for someone who has not read the documentation, like every other
    //  message in this unit: what happens, why, and the way to get what they
    //  wanted. No jargon - the user never sees the word "seed".
    AReason :=
        'This point was not moved.' + CRLF2 +
        'The curves here are placed by the whole markup rather than one by ' +
        'one, so moving any of its points rebuilds all of them from scratch. ' +
        'Everything the last fit found for this model would be lost, with ' +
        'nothing on the chart to say so.' + CRLF2 +
        'To change the markup: move the point and fit again, accepting that ' +
        'the model is fitted afresh - or undo the fit first if you would ' +
        'rather keep it.';
end;

function AdviseHeldParameterValue(const AName: string; ATyped, AHeld: double;
    out AReason: string): boolean;
begin
    AReason := '';
    Result := Abs(AHeld - ATyped) >
        1e-9 * Max(1.0, Max(Abs(ATyped), Abs(AHeld)));
    if not Result then
        Exit;
    //  No numbers: the table shows the value in its own units, which may not
    //  be the engine's, and the redrawn table already shows what is held.
    AReason :=
        'The model holds a different value of ' + AName + ' from the one ' +
        'typed: the nearest one this parameter allows.' + CRLF2 +
        'Every parameter keeps to its own range. A width is no wider than the ' +
        'fit interval its curve is fitted in, a mixing fraction lies between 0 ' +
        'and 1, an amplitude or a background level is not below zero, and a ' +
        'position stays between the samples either side of where it was ' +
        'picked. A background curve is lifted until it is nowhere below zero, ' +
        'and its level carries the lift.' + CRLF2 +
        'Help > Explain Everything, "Limits on parameter values", says why ' +
        'each limit is there.';
end;

function AdviseValuesHeldOnOpen(const AHeld: array of string): string;
var
    i: longint;
begin
    Result := '';
    if Length(AHeld) = 0 then
        Exit;
    Result := 'The model was put back, but holds different values from the ' +
        'ones saved for:';
    for i := 0 to High(AHeld) do
        Result := Result + LineEnding + '    ' + AHeld[i];
    Result := Result + CRLF2 +
        'Each is held at the nearest value its parameter now allows. A peak is ' +
        'no wider at half maximum than the fit interval it is fitted in, and a ' +
        'background is not below zero where the data are not - limits that may ' +
        'not have existed when the project was saved. Fit again to fit from ' +
        'here.' + CRLF2 +
        'Help > Explain Everything, "Limits on parameter values", says why ' +
        'each limit is there.';
end;

const
    { How many stretches are listed before the rest are only counted: past a
      handful, a list of x values is a table nobody reads in a sentence. }
    UNCOVERED_STRETCHES_LISTED = 4;
    { Below this share the figure tells the reader nothing, and "0.0 %" reads
      as "none at all". }
    UNCOVERED_SHARE_QUOTED: double = 0.005;

function SamplesWords(ACount: longint): string;
begin
    if ACount = 1 then
        Result := '1 sample'
    else
        Result := IntToStr(ACount) + ' samples';
end;

{ AOne or AMany: the string IfThen is in StrUtils, which nothing here needs. }
function Either(AIsOne: boolean; const AOne, AMany: string): string;
begin
    if AIsOne then
        Result := AOne
    else
        Result := AMany;
end;

function XText(AX: double): string;
begin
    Result := FloatToStrF(AX, ffGeneral, 6, 0);
end;

{ Where the stretches are, in words: "x = 0; x = 2 to 5", the first few and
  then how many more. One sample is named by its x alone. }
function StretchesText(const ARanges: TSampleRanges): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to Min(High(ARanges), UNCOVERED_STRETCHES_LISTED - 1) do
    begin
        if Result <> '' then
            Result := Result + '; ';
        if ARanges[i].Count = 1 then
            Result := Result + 'x = ' + XText(ARanges[i].FromX)
        else
            Result := Result + 'x = ' + XText(ARanges[i].FromX) + ' to ' +
                XText(ARanges[i].ToX);
    end;
    if Length(ARanges) > UNCOVERED_STRETCHES_LISTED then
        Result := Result + Format('; and %d more stretches',
            [Length(ARanges) - UNCOVERED_STRETCHES_LISTED]);
end;

function AdviseUncoveredSamples(const ARanges: TSampleRanges;
    AShare: double): string;
var
    Count, i: longint;
    Where: string;
    One: boolean;
begin
    Result := '';
    Count := UncoveredSampleCount(ARanges);
    if Count = 0 then
        Exit;
    One := Count = 1;

    Where := StretchesText(ARanges);

    if One then
        Result := SamplesWords(Count) + ' inside the fit intervals is covered ' +
            'by no curve of the model (' + Where + '). The model is 0 there, ' +
            'and the sample still counts in the R-factor'
    else
        Result := SamplesWords(Count) + ' inside the fit intervals are ' +
            'covered by no curve of the model (' + Where + '). The model is 0 ' +
            'there, and the samples still count in the R-factor';
    if AShare >= UNCOVERED_SHARE_QUOTED then
        Result := Result + Format(': %s %.1f %% of the squared difference ' +
            'between the data and the model.',
            [Either(One, 'it makes up', 'they make up'), AShare * 100])
    else
        Result := Result + '.';
    Result := Result + ' To measure only what the model describes, narrow ' +
        'the fit interval to the stretch the curves cover, or extend the ' +
        'model over ' + Either(One, 'it', 'them') + '.';
end;

function UncoveredSamplesStatusText(ACount: longint): string;
begin
    if ACount <= 0 then
        Result := ''
    else
        Result := SamplesWords(ACount) + ' in the fit intervals ' +
            Either(ACount = 1, 'is', 'are') + ' covered by no curve - ' +
            'History says what that does to the R-factor.';
end;

{ The pairs as words: "x = 0 to 239; x = 300 to 400". }
function IntervalsText(const ABounds: array of double): string;
var
    i: longint;
begin
    Result := '';
    i := 0;
    while i + 1 <= High(ABounds) do
    begin
        if Result <> '' then
            Result := Result + '; ';
        Result := Result + 'x = ' + XText(ABounds[i]) + ' to ' +
            XText(ABounds[i + 1]);
        Inc(i, 2);
    end;
end;

function InIntervals(const ABounds: array of double; AX: double): boolean;
var
    i: longint;
begin
    Result := False;
    i := 0;
    while i + 1 <= High(ABounds) do
    begin
        if (AX >= ABounds[i]) and (AX <= ABounds[i + 1]) then
            Exit(True);
        Inc(i, 2);
    end;
end;

function AdviseReplacedIntervals(const AKept, ANew, ADataX: array of double): string;
var
    Kept: array of boolean;
    LeftOut: TSampleRanges;
    Same: boolean;
    i, Count: longint;
begin
    Result := '';
    if Length(AKept) < 2 then
        Exit;
    Same := Length(AKept) = Length(ANew);
    for i := 0 to Min(High(AKept), High(ANew)) do
        Same := Same and (AKept[i] = ANew[i]);
    if Same then
        Exit;

    Result := 'Fit > Automatically replaced the fit intervals (' +
        IntervalsText(AKept) + ') with its own (' + IntervalsText(ANew) + ').';

    //  WHAT THE OLD INTERVALS FITTED AND THE NEW ONES DO NOT: "uncovered" by
    //  the new ones, read with the same stretch grouping. A sample outside the
    //  old intervals was never fitted, so it counts as kept.
    SetLength(Kept, Length(ADataX));
    for i := 0 to High(ADataX) do
        Kept[i] := (not InIntervals(AKept, ADataX[i])) or
            InIntervals(ANew, ADataX[i]);
    LeftOut := UncoveredRanges(ADataX, Kept);
    Count := UncoveredSampleCount(LeftOut);
    if Count = 1 then
        Result := Result + ' The sample at ' + StretchesText(LeftOut) +
            ' is no longer fitted.'
    else if Count > 1 then
        Result := Result + ' ' + SamplesWords(Count) + ' (' +
            StretchesText(LeftOut) + ') are no longer fitted.';
    Result := Result + ' To fit what you had chosen, set the intervals again ' +
        'under Model > Fit Intervals and run Fit > Minimize Difference.';
end;


function AdviceNeedsAttention(const AAdvice: TFitAdvice): boolean;
begin
    //  Curve scaling is deliberately NOT on this list. It is an internal
    //  convergence aid rather than something the user chose for its own sake,
    //  and warning about it on every such selection would train people to
    //  dismiss these messages - which would cost us the two that matter.
    Result := AAdvice.FallsBackToNativeEngine or AAdvice.LossOverridden;
end;

{ SAY IT ONCE: the one rule both dialogs here follow - the fit's advice and
  the samples no curve covers. AKey identifies what would be said; ANeedsSaying
  whether there is anything to say; AAnnounce whether this moment is one the
  user caused, or is there for. }
function ShouldAnnounceOnce(ANeedsSaying, AAnnounce: boolean;
    const AKey: string; var AMemory: string): boolean;
begin
    //  ONE MEMORY, READ AND WRITTEN: a remembered value passed as const beside
    //  an out parameter for the same field read as empty, because the out
    //  parameter is cleared on entry - so the window, which passes one field
    //  for both, said its advice again every time.
    Result := False;
    if not ANeedsSaying then
    begin
        //  FORGOTTEN, so that coming back to a problematic state is explained
        //  again rather than staying silent because it was mentioned once, an
        //  hour ago, about something else.
        AMemory := '';
        Exit;
    end;
    //  Worth saying, but not now - AND THE MEMORY IS LEFT ALONE: clearing it
    //  would make the next moment that counts repeat a message already read,
    //  setting it would swallow one not yet read. Nor when it was already said
    //  for this same thing.
    if (not AAnnounce) or (AKey = AMemory) then
        Exit;
    AMemory := AKey;
    Result := True;
end;

function AdviceShouldBeAnnounced(AAnnounce: boolean;
    const AAdvice: TFitAdvice; var AMemory: string): boolean;
begin
    Result := ShouldAnnounceOnce(AdviceNeedsAttention(AAdvice), AAnnounce,
        AAdvice.Detail, AMemory);
end;

{ Where the stretches are, as the key to remember: the share is left out on
  purpose, since every fit moves it. }
function StretchesKey(const ARanges: TSampleRanges): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(ARanges) do
        Result := Result + XText(ARanges[i].FromX) + '..' +
            XText(ARanges[i].ToX) + ';';
end;

function UncoveredSamplesToAnnounce(const ARanges: TSampleRanges;
    AShare: double; AAttended: boolean; var AMemory: string): string;
begin
    Result := '';
    if ShouldAnnounceOnce(Length(ARanges) > 0, AAttended,
            StretchesKey(ARanges), AMemory) then
        Result := AdviseUncoveredSamples(ARanges, AShare);
end;

function UncoveredSamplesBeforeFit(const AStatistics: TFitStatistics;
    AAttended: boolean; var AMemory: string): string;
begin
    Result := UncoveredSamplesToAnnounce(AStatistics.UncoveredRanges,
        AStatistics.UncoveredResidualShare, AAttended, AMemory);
    if Result <> '' then
        Result := Result + LineEnding + LineEnding + 'Fit anyway? No leaves ' +
            'the model as it is, so the intervals or the curves can be ' +
            'changed first.';
end;

function EndOfRunNotice(const ANote: string; const AStatistics: TFitStatistics;
    AAttended: boolean; var ANoteMemory, AUncoveredMemory: string): string;
var
    Uncovered: string;
begin
    Result := '';
    if ShouldAnnounceOnce(Trim(ANote) <> '', AAttended, ANote, ANoteMemory) then
        Result := ANote;
    Uncovered := UncoveredSamplesToAnnounce(AStatistics.UncoveredRanges,
        AStatistics.UncoveredResidualShare, AAttended, AUncoveredMemory);
    if Uncovered <> '' then
    begin
        if Result <> '' then
            Result := Result + LineEnding + LineEnding;
        Result := Result + Uncovered;
    end;
end;

function AdviseFit(ALossKind: longint;
    AFormulaBackendRequested, ACurveIsAnalytic, AAmplitudeIsUnbounded,
    ACurveScalingRequested: boolean): TFitAdvice;
var
    Requested: longint;
    Reasons: string;

    procedure AddReason(const AText: string);
    begin
        if Reasons <> '' then
            Reasons := Reasons + CRLF2;
        Reasons := Reasons + AText;
    end;

begin
    Requested := ALossKind;
    if not IsKnownLoss(Requested) then
        Requested := LOSS_KIND_RFACTOR;

    Result := Default(TFitAdvice);
    Result.LossKind := Requested;
    Reasons := '';

    //  1. THE OBJECTIVE. Mirrors TFitTask.EnforceLossCompatibility, which calls
    //     this same rule - see loss_compatibility.
    if not LossAllowedForCapability(Result.LossKind, AAmplitudeIsUnbounded) then
    begin
        Result.LossKind := DefaultLossFor(AAmplitudeIsUnbounded);
        Result.LossOverridden := True;
        AddReason(Format('The objective was changed from "%s" to "%s".',
            [LossName(Requested), LossName(Result.LossKind)]) + ' ' +
            LossRefusalReason(Requested));
    end;

    //  2. THE ENGINE. Two independent reasons a formula backend cannot be used;
    //     report BOTH when both apply, because fixing only one would still not
    //     get the user the engine they picked.
    if AFormulaBackendRequested then
    begin
        if not ACurveIsAnalytic then
        begin
            Result.FallsBackToNativeEngine := True;
            AddReason('The fit will run on the built-in engine, not the one '
                + 'you selected, because this curve type has no formula - it '
                + 'computes its points directly, and the other engines fit by '
                + 'evaluating a formula. The result is still a proper fit; you '
                + 'will not get per-parameter uncertainties.');
        end;

        if not LossIsLeastSquares(Result.LossKind) then
        begin
            Result.FallsBackToNativeEngine := True;
            AddReason(Format('The fit will run on the built-in engine, not the '
                + 'one you selected, because "%s" cannot be written as a sum of '
                + 'squared residuals, which is the only form those engines can '
                + 'minimise. Your choice of objective is honoured - the engine '
                + 'is what changes. Choose "%s" or "%s" if you would rather '
                + 'keep the selected engine.',
                [LossName(Result.LossKind), LossName(LOSS_KIND_RFACTOR),
                 LossName(LOSS_KIND_SUMSQ)]));
        end;
    end;

    //  3. CURVE SCALING. Reported for completeness - it explains a visible
    //     difference in how a fit behaves - but never raised as an alert.
    if ACurveScalingRequested and AAmplitudeIsUnbounded then
    begin
        Result.CurveScalingDisabled := True;
        AddReason('Curve scaling is switched off for this model. It fits one '
            + 'overall multiplier for the whole profile, which duplicates a '
            + 'model that already sets its own amplitude - and the duplicate '
            + 'lets the fit collapse the shape while the multiplier absorbs the '
            + 'difference.');
    end;

    Result.Detail := Reasons;

    //  The summary always states what WILL happen, never what was asked for.
    //  A status line that echoes the selection is worse than useless when the
    //  selection is not what runs.
    if Result.FallsBackToNativeEngine then
        Result.Summary := Format(
            'Fitting with the built-in engine, minimising %s.',
            [LossName(Result.LossKind)])
    else if AFormulaBackendRequested then
        Result.Summary := Format(
            'Fitting with the selected engine, minimising %s.',
            [LossName(Result.LossKind)])
    else
        Result.Summary := Format('Minimising %s.', [LossName(Result.LossKind)]);

    if Result.LossOverridden then
        Result.Summary := Result.Summary + Format(
            ' ("%s" is not usable with this curve type.)', [LossName(Requested)]);
end;

function VariationHint(const AAdvice: TBackgroundModelAdvice;
    const ADesignedHint: string): string;
begin
    if AAdvice.VariationAllowed then
        Result := ADesignedHint
    else
        Result := AAdvice.VariationReason;
end;

end.
