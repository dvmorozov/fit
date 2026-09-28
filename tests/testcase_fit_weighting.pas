// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a residual is weighted, and what an unrecognised value means.)

WEIGHTING DOES NOT FAIL WHEN IT IS WRONG - it answers a different question and
reports the answer with the same confidence. That is why the name is worth a
unit and a test file rather than a literal at each of the six places that used
to carry one: the desktop settings, the wire, the service, the task, the window
and its menu.

THE RULE BELONGS TO THE SIDECAR, and this file is where that is written down.
Worker/py/fitting.py reads the value as

    if kind == "none": unweighted
    else:              poisson

so the test is exact and case-sensitive, and the Pascal side matches it
deliberately rather than by accident. A Pascal side that was lenient where
Python is strict would read 'None' as unweighted while the backend performing
the fit read it as poisson - two halves of one fit minimising different things,
with nothing failing.

AND THE REAL PROTECTION IS NORMALISING AT THE BOUNDARY, which is why
WeightingOrDefault exists and is used everywhere a value is stored or sent.
Only the two canonical names reach a settings file or the wire, so the
strictness above can never be met by a value this program wrote - only by one
it did not.
}
unit testcase_fit_weighting;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_weighting;

type
    TFitWeightingTest = class(TTestCase)
    published
        //  The two names.
        procedure TheTwoNamesAreTheOnesTheSidecarReads;
        procedure BothAreCanonicalAndNothingElseIs;

        //  What counts as unweighted.
        procedure OnlyTheExactWordMeansUnweighted;
        procedure TheTestIsCaseSensitiveBecauseThePythonOneIs;
        procedure AnEmptyValueIsNotUnweighted;
        procedure NorIsAnythingUnrecognised;

        //  Normalising.
        procedure TheDefaultIsPoisson;
        procedure AnEmptySettingBecomesTheDefault;
        procedure AnUnrecognisedValueBecomesTheDefault;
        procedure AnUnweightedValueIsKeptAsItIs;
        procedure NormalisingIsIdempotent;
        procedure EveryNormalisedValueIsCanonical;

        //  The two together.
        procedure NormalisingDoesNotChangeWhatAValueMeans;
    end;

implementation

const
    { What a settings file, a wire message or a typo can carry. The empty string
      is the important one: it is what every settings file written before this
      setting existed holds. }
    Strange: array[0..6] of string = (
        '', 'None', 'POISSON', 'Poisson', ' none', 'none ', 'unweighted');

{ ------------------------------- the two names ------------------------------ }

procedure TFitWeightingTest.TheTwoNamesAreTheOnesTheSidecarReads;
begin
    //  Spelled out rather than compared to each other: these four and seven
    //  lower-case letters are what Worker/py/fitting.py tests against, and a
    //  rename on one side has to fail here.
    AssertEquals('the counting-statistics name', 'poisson', WEIGHTING_POISSON);
    AssertEquals('the unweighted name', 'none', WEIGHTING_NONE);
end;

procedure TFitWeightingTest.BothAreCanonicalAndNothingElseIs;
var
    i: longint;
begin
    AssertTrue('poisson', IsCanonicalWeighting(WEIGHTING_POISSON));
    AssertTrue('none', IsCanonicalWeighting(WEIGHTING_NONE));
    for i := 0 to High(Strange) do
        AssertFalse('"' + Strange[i] + '" is not canonical',
            IsCanonicalWeighting(Strange[i]));
end;

{ ---------------------------- what is unweighted ---------------------------- }

procedure TFitWeightingTest.OnlyTheExactWordMeansUnweighted;
begin
    AssertTrue('none', WeightingIsUnweighted(WEIGHTING_NONE));
    AssertFalse('poisson', WeightingIsUnweighted(WEIGHTING_POISSON));
end;

procedure TFitWeightingTest.TheTestIsCaseSensitiveBecauseThePythonOneIs;
begin
    //  CHARACTERISED AND SHARED. Python compares kind == "none" exactly, so
    //  'None' is poisson there. Being lenient here would make the client and
    //  the backend disagree about the same value - the client showing an
    //  unweighted tick over a fit that was weighted.
    AssertFalse('None', WeightingIsUnweighted('None'));
    AssertFalse('NONE', WeightingIsUnweighted('NONE'));
end;

procedure TFitWeightingTest.AnEmptyValueIsNotUnweighted;
begin
    //  Every settings file older than this setting carries an empty string, and
    //  reading it as unweighted would silently change the objective of every
    //  fit those users run after upgrading.
    AssertFalse('the empty string', WeightingIsUnweighted(''));
end;

procedure TFitWeightingTest.NorIsAnythingUnrecognised;
var
    i: longint;
begin
    for i := 0 to High(Strange) do
        AssertFalse('"' + Strange[i] + '" is not unweighted',
            WeightingIsUnweighted(Strange[i]));
end;

{ ------------------------------- normalising -------------------------------- }

procedure TFitWeightingTest.TheDefaultIsPoisson;
begin
    //  The right default for the counting data this program was written for.
    AssertEquals('poisson', WEIGHTING_POISSON, WeightingOrDefault(''));
end;

procedure TFitWeightingTest.AnEmptySettingBecomesTheDefault;
begin
    AssertEquals('an absent setting', WEIGHTING_POISSON, WeightingOrDefault(''));
end;

procedure TFitWeightingTest.AnUnrecognisedValueBecomesTheDefault;
var
    i: longint;
begin
    for i := 0 to High(Strange) do
        AssertEquals('"' + Strange[i] + '" normalises to the default',
            WEIGHTING_POISSON, WeightingOrDefault(Strange[i]));
end;

procedure TFitWeightingTest.AnUnweightedValueIsKeptAsItIs;
begin
    //  The one value that must survive normalising: a user who chose unweighted
    //  must not be quietly moved back to the default on the next start.
    AssertEquals('none', WEIGHTING_NONE, WeightingOrDefault(WEIGHTING_NONE));
end;

procedure TFitWeightingTest.NormalisingIsIdempotent;
var
    i: longint;
    Once: string;
begin
    //  A value passes through this on the way to the settings file and again on
    //  the way out; a second pass that changed it would flip the setting on
    //  every restart.
    for i := 0 to High(Strange) do
    begin
        Once := WeightingOrDefault(Strange[i]);
        AssertEquals('"' + Strange[i] + '" is stable', Once,
            WeightingOrDefault(Once));
    end;
    AssertEquals('and so is none', WEIGHTING_NONE,
        WeightingOrDefault(WeightingOrDefault(WEIGHTING_NONE)));
end;

procedure TFitWeightingTest.EveryNormalisedValueIsCanonical;
var
    i: longint;
begin
    //  THE BOUNDARY PROPERTY. Because everything stored or sent goes through
    //  here, only these two strings ever reach a settings file or the wire -
    //  so the case-sensitivity above can only ever be met by a value this
    //  program did not write.
    for i := 0 to High(Strange) do
        AssertTrue('"' + Strange[i] + '" normalises to a canonical name',
            IsCanonicalWeighting(WeightingOrDefault(Strange[i])));
    AssertTrue('none', IsCanonicalWeighting(WeightingOrDefault(WEIGHTING_NONE)));
    AssertTrue('poisson',
        IsCanonicalWeighting(WeightingOrDefault(WEIGHTING_POISSON)));
end;

{ ----------------------------- the two together ----------------------------- }

procedure TFitWeightingTest.NormalisingDoesNotChangeWhatAValueMeans;
var
    i: longint;
begin
    //  The two functions have to agree or normalising would be a silent change
    //  of objective rather than a tidying of the name.
    for i := 0 to High(Strange) do
        AssertEquals('"' + Strange[i] + '" means the same after normalising',
            WeightingIsUnweighted(Strange[i]),
            WeightingIsUnweighted(WeightingOrDefault(Strange[i])));
    AssertEquals('none', WeightingIsUnweighted(WEIGHTING_NONE),
        WeightingIsUnweighted(WeightingOrDefault(WEIGHTING_NONE)));
end;

initialization
    //  Unit tests: two functions over a string. No settings file, no wire.
    RegisterTest('unit', TFitWeightingTest);
end.
