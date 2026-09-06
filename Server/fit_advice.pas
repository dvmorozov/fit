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

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit fit_advice;

{$mode objfpc}{$H+}

interface

uses
    fit_loss, loss_compatibility, SysUtils;

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

  ARemembered is what was last announced; ANowRemembered is what to keep. The
  memory is CLEARED when the advice no longer needs attention, so that returning
  to a problematic selection explains itself afresh. }
function AdviceShouldBeAnnounced(AAnnounce: boolean;
    const AAdvice: TFitAdvice; const ARemembered: string;
    out ANowRemembered: string): boolean;

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

implementation

const
    CRLF2 = LineEnding + LineEnding;

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

function AdviceNeedsAttention(const AAdvice: TFitAdvice): boolean;
begin
    //  Curve scaling is deliberately NOT on this list. It is an internal
    //  convergence aid rather than something the user chose for its own sake,
    //  and warning about it on every such selection would train people to
    //  dismiss these messages - which would cost us the two that matter.
    Result := AAdvice.FallsBackToNativeEngine or AAdvice.LossOverridden;
end;

function AdviceShouldBeAnnounced(AAnnounce: boolean;
    const AAdvice: TFitAdvice; const ARemembered: string;
    out ANowRemembered: string): boolean;
begin
    if not AdviceNeedsAttention(AAdvice) then
    begin
        //  FORGOTTEN, so that coming back to a problematic selection is
        //  explained again rather than staying silent because it was mentioned
        //  once, an hour ago, about something else.
        ANowRemembered := '';
        Result := False;
        Exit;
    end;

    //  Worth saying, but not on a recomputation the user did not cause.
    if not AAnnounce then
    begin
        //  AND THE MEMORY IS LEFT ALONE. Clearing it here would make the next
        //  user-driven change repeat a message they have already read; setting
        //  it would swallow the message they have not.
        ANowRemembered := ARemembered;
        Result := False;
        Exit;
    end;

    //  Already said, for this same advice.
    if AAdvice.Detail = ARemembered then
    begin
        ANowRemembered := ARemembered;
        Result := False;
        Exit;
    end;

    ANowRemembered := AAdvice.Detail;
    Result := True;
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

end.
