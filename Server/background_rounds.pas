// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(When the background and the peaks stop taking turns, and what the
Run Log says of it.)

The background is one curve over the whole profile, fitted apart from the fit
intervals: its own step fits it with the peaks held, then the intervals fit
their peaks with it held (TFitService.RunTasksOverProfileBackground). Each pair
of steps is a ROUND, and this decides whether another one is worth running.

Fit > Automatically fits the background to the floor of the data first
(TFitService.FitBackgroundFloor), before the reduction, and then takes these
turns like any fit: through the floor's samples alone the curve is free under a
peak, where the data has none, so the turns settle it with the peaks.

Decisions and words over plain values, so they are tested without a fit.

WHY A RELATIVE IMPROVEMENT AND A LIMIT. The two steps share every sample under
the intervals, so each moves what the other found and the model creeps towards
the joint optimum without ever reaching it exactly. A round that improves the
model by less than a thousandth of its figure changes nothing a user can see,
and the limit bounds a model that keeps creeping. Both numbers are this
software's choice, not measured optima: each round costs one fit of every
interval. Where the intervals leave stretches of bare background between them
the turns settle in a few rounds; where one interval covers peaks and
background alike they creep, closing a fixed fraction of the gap each round,
and five were not enough for a known parabola under such a profile.
}
unit background_rounds;

{$mode objfpc}{$H+}

interface

uses
    fit_progress_json;

const
    { A round must improve the model's figure by at least this fraction of it
      for another to follow. }
    BACKGROUND_ROUND_IMPROVEMENT = 1e-3;
    { The most rounds one fit runs. }
    BACKGROUND_ROUND_LIMIT = 20;
    { The guide topic the rounds are explained in (guide_fitting). }
    BackgroundRoundsTopic = 'fitting/background-in-turns';
    { The terms a run's background starts its turns with, when it is a series
      shape (TNamedPointsSet.MaxTermCount > 0): what follows a pattern's
      broad hump, which six terms spread over the pattern cannot. This
      software's choice, as the step and the sample count are. }
    BACKGROUND_START_TERMS = 12;
    { How many terms a turn tries to add or to drop. }
    BACKGROUND_TERMS_STEP = 2;
    { Where the search for the terms looks before it steps: the most each
      number of terms can follow is a width, so it widens by about half again.
      This software's choice. }
    BACKGROUND_TERMS_LADDER: array[0..5] of longint = (12, 16, 24, 32, 48, 64);
    { Samples a term needs to be fixed by. The window of fifty-one samples a
      test fits holds six; a pattern of three thousand holds more than any
      shape has. }
    BACKGROUND_SAMPLES_PER_TERM = 8;

type
    { What follows a round: another, or the end and why. }
    TBackgroundRoundVerdict = (
        brAnother,
        { The user pressed Stop. }
        brStopped,
        { BACKGROUND_ROUND_LIMIT rounds have run. }
        brLimit,
        { The round improved the figure by less than
          BACKGROUND_ROUND_IMPROVEMENT of it, or not at all. }
        brSettled);

type
    TBackgroundTermsList = array of longint;

{ The most terms the data can fix: ASamples div BACKGROUND_SAMPLES_PER_TERM,
  between the least the shape has and the most. 0 for a shape with no terms
  to choose (AMaxTerms 0). }
function BackgroundTermsCap(AMaxTerms, ASamples: longint): longint;

{ The terms the background starts the run's turns with: BACKGROUND_START_TERMS
  where the data hold that many, and never fewer than it already has - a run
  starts from the model the last one left. ATerms for a shape with none to
  choose. }
function BackgroundStartTerms(ATerms, AMinTerms, AMaxTerms,
    ASamples: longint): longint;

{ The numbers of terms worth trying from ATerms: BACKGROUND_TERMS_STEP more,
  then as many fewer, each only when it differs from ATerms and lies within
  the least and the cap. Empty for a shape with none to choose. }
function BackgroundTermsToTry(ATerms, AMinTerms, AMaxTerms,
    ASamples: longint): TBackgroundTermsList;

{ THE NUMBERS OF TERMS TO SCAN FIRST, above ATerms: BACKGROUND_TERMS_LADDER
  within the cap, ascending. The score against the floor does not fall by
  degrees as terms are added: a hump is clipped as a peak until the curve has
  the terms to follow it, and then it is not, so a search by steps of two stops
  at the first step that gains nothing, short of the cliff. Empty for a shape
  with none to choose. }
function BackgroundTermsLadder(ATerms, AMinTerms, AMaxTerms,
    ASamples: longint): TBackgroundTermsList;

{ Whether a change of terms is kept: the BIC with the tried number is strictly
  lower than with the present one. Equal is not better, for the reason a
  removal that changes nothing is not (fit_reduction.BicFavoursRemoval, which
  this is the same rule as). }
function BackgroundTermsPay(ABicNow, ABicTried: double): boolean;

{ What the Run Log says of the background fitted to the floor of the data: its
  terms at the start and the end (0 for a shape with none to choose), its score
  against the floor, and how many of ASamples lie on it. }
function BackgroundFloorDetail(AStartedAt, ATerms: longint; ABic: double;
    AOnTheCurve, ASamples: longint): string;

{ The Run Log's line for a background that went from AFrom terms to ATo, its
  score against the floor of the data (clipping.FloorFitBic) from ABicFrom to
  ABicTo. }
function BackgroundTermsEvent(AFrom, ATo: longint;
    ABicFrom, ABicTo: double): TRunEvent;

{ What follows round ARound (counted from 1), which took the model's figure
  from ABefore to AAfter - ABefore is +Inf before the first round. A figure
  that cannot be compared settles the rounds: it says nothing about the
  next. }
function BackgroundRoundVerdict(ARound: longint; ABefore, AAfter: double;
    AStopped: boolean): TBackgroundRoundVerdict;

{ The Run Log's line for rounds that ended after ARounds with AVerdict,
  leaving the model's figure at AFigure. }
function BackgroundRoundsEvent(ARounds: longint;
    AVerdict: TBackgroundRoundVerdict; AFigure: double): TRunEvent;

implementation

uses
    SysUtils, Math, run_event_text, fit_reduction;

function BackgroundRoundVerdict(ARound: longint; ABefore, AAfter: double;
    AStopped: boolean): TBackgroundRoundVerdict;
begin
    if AStopped then
        Exit(brStopped);
    if IsNan(AAfter) or IsInfinite(AAfter) then
        Exit(brSettled);
    if ARound >= BACKGROUND_ROUND_LIMIT then
        Exit(brLimit);
    //  The first round has nothing to be compared with.
    if IsInfinite(ABefore) then
        Exit(brAnother);
    if (AAfter >= ABefore) or
       (ABefore - AAfter < BACKGROUND_ROUND_IMPROVEMENT * ABefore) then
        Exit(brSettled);
    Result := brAnother;
end;

function BackgroundTermsCap(AMaxTerms, ASamples: longint): longint;
begin
    if AMaxTerms < 1 then
        Exit(0);
    Result := Max(1, Min(AMaxTerms, ASamples div BACKGROUND_SAMPLES_PER_TERM));
end;

function BackgroundStartTerms(ATerms, AMinTerms, AMaxTerms,
    ASamples: longint): longint;
begin
    if AMaxTerms < 1 then
        Exit(ATerms);
    Result := Max(ATerms, Min(BACKGROUND_START_TERMS,
        Max(AMinTerms, BackgroundTermsCap(AMaxTerms, ASamples))));
end;

function BackgroundTermsToTry(ATerms, AMinTerms, AMaxTerms,
    ASamples: longint): TBackgroundTermsList;
var
    More, Fewer: longint;
begin
    Result := nil;
    if AMaxTerms < 1 then
        Exit;
    More := Min(ATerms + BACKGROUND_TERMS_STEP,
        Max(AMinTerms, BackgroundTermsCap(AMaxTerms, ASamples)));
    if More > ATerms then
    begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := More;
    end;
    Fewer := Max(ATerms - BACKGROUND_TERMS_STEP, AMinTerms);
    if Fewer < ATerms then
    begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Fewer;
    end;
end;

function BackgroundTermsLadder(ATerms, AMinTerms, AMaxTerms,
    ASamples: longint): TBackgroundTermsList;
var
    Cap, k: longint;
begin
    Result := nil;
    if AMaxTerms < 1 then
        Exit;
    Cap := Max(AMinTerms, BackgroundTermsCap(AMaxTerms, ASamples));
    for k := 0 to High(BACKGROUND_TERMS_LADDER) do
        if (BACKGROUND_TERMS_LADDER[k] > ATerms) and
           (BACKGROUND_TERMS_LADDER[k] <= Cap) then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := BACKGROUND_TERMS_LADDER[k];
        end;
end;

function BackgroundTermsPay(ABicNow, ABicTried: double): boolean;
begin
    if IsNan(ABicNow) or IsNan(ABicTried) then
        Exit(False);
    Result := BicFavoursRemoval(ABicNow, ABicTried);
end;

function BackgroundFloorDetail(AStartedAt, ATerms: longint; ABic: double;
    AOnTheCurve, ASamples: longint): string;
begin
    Result := 'The background curve was fitted to the data itself, leaving out ' +
        'the samples standing more than a few noise units above it, so no peak ' +
        'curve pulls it up and it is not fitted to what the peak curves leave. ';
    if ATerms > 0 then
        if AStartedAt = ATerms then
            Result := Result + Format('It kept its %d terms. ', [ATerms])
        else
            Result := Result + Format('It started with %d terms and ended with ' +
                '%d. ', [AStartedAt, ATerms]);
    Result := Result + Format('%d of the %d samples lie on it; its score ' +
        'against the floor (a BIC with each sample capped) is %s.',
        [AOnTheCurve, ASamples, FloatToStrF(ABic, ffFixed, 15, 1)]);
end;

function BackgroundTermsEvent(AFrom, ATo: longint;
    ABicFrom, ABicTo: double): TRunEvent;
begin
    Result := Default(TRunEvent);
    Result.Source := RunEventFramework;
    Result.Section := RunSectionResult;
    Result.Topic := BackgroundRoundsTopic;
    Result.Title := Format('Background went from %d to %d terms', [AFrom, ATo]);
    Result.Detail := Format('Against the floor of the data the background ' +
        'scored BIC %s with %d terms and %s with %d: the extra terms were ' +
        'worth their parameters, or the dropped ones were not, so it keeps the ' +
        'shape with the lower figure.',
        [FloatToStrF(ABicFrom, ffFixed, 15, 1), AFrom,
         FloatToStrF(ABicTo, ffFixed, 15, 1), ATo]);
end;

function BackgroundRoundsEvent(ARounds: longint;
    AVerdict: TBackgroundRoundVerdict; AFigure: double): TRunEvent;
var
    Why: string;
begin
    case AVerdict of
        brStopped: Why := 'Stop ended the turns';
        brLimit: Why := Format('the turns ended at the limit of %d',
            [BACKGROUND_ROUND_LIMIT]);
    else
        Why := 'the last turn improved the model by less than a thousandth';
    end;
    Result := Default(TRunEvent);
    Result.Source := RunEventFramework;
    Result.Section := RunSectionResult;
    Result.Topic := BackgroundRoundsTopic;
    Result.Title := Format('Background fitted under every interval in %d ' +
        'turn(s)', [ARounds]);
    Result.Detail := Format('The background is one curve under the whole ' +
        'profile, so it was fitted to all of it with the peaks held, and the ' +
        'peaks in each interval with it held, in turn; %s. R-factor %s.',
        [Why, FloatToStrF(AFigure, ffGeneral, 4, 0)]);
end;

end.
