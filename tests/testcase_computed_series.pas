// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which of the computed series is named, and which is drawn, when it
comes back empty.)

WHAT THIS PINS, AND WHY IT WAS WORTH TAKING OUT. Five series are refreshed after
every recompute, and each was handled by its own copy of the same two
conditions, written inline in a hundred-line routine that fetches from a service
and draws on a chart - so no test could reach any of them, and two of the five
copies differed from the other three.

THE DIFFERENCE IS DELIBERATE and this file is where that is stated. A series the
user PICKS INTO is named while still empty, because the title is what names the
series on the chart and a set that came back empty - after a reset, or before
anything has been picked - would otherwise be drawn later under a blank name.
That was a real defect once. A series that is an OUTPUT has nothing to name
until it has points.

THE TWO GROUPS ARE THE SAME THREE MEMBERS, which is the part most likely to rot:
the sets fetched only on request are exactly the sets the user picks into, and
that is not a coincidence but the same fact read twice. It is written as one
condition in the unit and asserted as an identity here, so a fourth series added
to one group cannot quietly miss the other.
}
unit testcase_computed_series;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, computed_series;

type
    TComputedSeriesTest = class(TTestCase)
    published
        //  Which are fetched only on request.
        procedure ThePickedSetsAreTheExtraData;
        procedure TheModelAndTheResidualAreNot;

        //  Naming.
        procedure AnEmptyPickedSetIsStillNamed;
        procedure AnEmptyOutputIsNotNamed;
        procedure EverythingWithPointsIsNamed;

        //  Drawing.
        procedure NothingEmptyIsEverDrawn;
        procedure EverythingWithPointsIsDrawn;
        procedure ANamedSeriesIsNotNecessarilyADrawnOne;

        //  The two groups are one fact.
        procedure NamedWhileEmptyIsExactlyTheExtraDataGroup;
        procedure EverySeriesIsAnsweredForBothQuestions;
    end;

implementation

{ ------------------------------ extra data ---------------------------------- }

procedure TComputedSeriesTest.ThePickedSetsAreTheExtraData;
begin
    //  Not fetched during the rapid refreshes of a running fit: the model is
    //  what changes there, and the pick markers would be redrawn on top of
    //  themselves.
    AssertTrue('curve positions', SeriesIsExtraData(csCurvePositions));
    AssertTrue('fitted positions', SeriesIsExtraData(csFittedPositions));
    AssertTrue('fit intervals', SeriesIsExtraData(csRFactorBounds));
end;

procedure TComputedSeriesTest.TheModelAndTheResidualAreNot;
begin
    //  These two ARE what changes during a fit, so they are refreshed every
    //  time. A series moved into the extra-data group by mistake would simply
    //  stop updating while a fit ran.
    AssertFalse('the computed profile', SeriesIsExtraData(csComputedProfile));
    AssertFalse('the difference', SeriesIsExtraData(csDeltaProfile));
end;

{ -------------------------------- naming ------------------------------------ }

procedure TComputedSeriesTest.AnEmptyPickedSetIsStillNamed;
begin
    //  THE DEFECT THIS RULE EXISTS FOR. The title names the series the user's
    //  next pick will appear in. Left blank because the set was empty, the
    //  series is drawn later under no name at all.
    AssertTrue('curve positions', SeriesIsNamed(csCurvePositions, 0));
    AssertTrue('fitted positions', SeriesIsNamed(csFittedPositions, 0));
    AssertTrue('fit intervals', SeriesIsNamed(csRFactorBounds, 0));
end;

procedure TComputedSeriesTest.AnEmptyOutputIsNotNamed;
begin
    //  Nobody picks into these, and before there is a model there is no series
    //  to name.
    AssertFalse('the computed profile', SeriesIsNamed(csComputedProfile, 0));
    AssertFalse('the difference', SeriesIsNamed(csDeltaProfile, 0));
end;

procedure TComputedSeriesTest.EverythingWithPointsIsNamed;
var
    S: TComputedSeries;
begin
    for S := Low(TComputedSeries) to High(TComputedSeries) do
        AssertTrue('a series with points is always named',
            SeriesIsNamed(S, 1));
end;

{ -------------------------------- drawing ----------------------------------- }

procedure TComputedSeriesTest.NothingEmptyIsEverDrawn;
var
    S: TComputedSeries;
begin
    //  Drawing an empty series puts an entry in the legend for a curve that is
    //  not on the chart - which reads as a series that failed to compute.
    for S := Low(TComputedSeries) to High(TComputedSeries) do
        AssertFalse('nothing empty is drawn', SeriesIsPlotted(S, 0));
end;

procedure TComputedSeriesTest.EverythingWithPointsIsDrawn;
var
    S: TComputedSeries;
begin
    for S := Low(TComputedSeries) to High(TComputedSeries) do
        AssertTrue('anything with points is drawn', SeriesIsPlotted(S, 1));
end;

procedure TComputedSeriesTest.ANamedSeriesIsNotNecessarilyADrawnOne;
begin
    //  The two questions are separate, and this is the case that separates
    //  them: an empty picked set is titled and not drawn. A single condition
    //  serving both would either lose the name or draw an empty series.
    AssertTrue('named', SeriesIsNamed(csCurvePositions, 0));
    AssertFalse('but not drawn', SeriesIsPlotted(csCurvePositions, 0));
end;

{ --------------------------- the two groups agree --------------------------- }

procedure TComputedSeriesTest.NamedWhileEmptyIsExactlyTheExtraDataGroup;
var
    S: TComputedSeries;
begin
    //  ONE FACT READ TWICE: a series is named while empty exactly when it is
    //  one the user picks into, and those are exactly the ones fetched on
    //  request. Asserted as an identity so a sixth series added to one group
    //  cannot quietly miss the other.
    for S := Low(TComputedSeries) to High(TComputedSeries) do
        AssertEquals('the two groups have the same members',
            SeriesIsExtraData(S), SeriesIsNamed(S, 0));
end;

procedure TComputedSeriesTest.EverySeriesIsAnsweredForBothQuestions;
var
    S: TComputedSeries;
    Named, Drawn: longint;
begin
    //  Walked rather than listed, so a series added later has to be considered
    //  rather than defaulting into whichever answer the compiler gives.
    Named := 0;
    Drawn := 0;
    for S := Low(TComputedSeries) to High(TComputedSeries) do
    begin
        if SeriesIsNamed(S, 1) then Inc(Named);
        if SeriesIsPlotted(S, 1) then Inc(Drawn);
    end;
    AssertEquals('every series is named when it has points',
        Ord(High(TComputedSeries)) + 1, Named);
    AssertEquals('and every one is drawn',
        Ord(High(TComputedSeries)) + 1, Drawn);
end;

initialization
    //  Unit tests: two predicates over an enumeration. No service, no chart.
    RegisterTest('unit', TComputedSeriesTest);
end.
