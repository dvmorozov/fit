// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(When the background's step and the intervals' stop taking turns,
and what the Run Log says of it.)
}
unit testcase_background_rounds;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fit_progress_json,
    run_event_text, background_rounds, explanation, guide_fitting;

type
    TBackgroundRoundsTest = class(TTestCase)
    published
        procedure TheFirstRoundIsAlwaysFollowedByASecond;
        procedure AClearImprovementEarnsAnotherRound;
        procedure AnImprovementBelowAThousandthSettlesTheRounds;
        procedure AFigureThatGotWorseSettlesTheRounds;
        procedure TheLimitEndsTheRoundsHoweverMuchTheyImprove;
        procedure AStopEndsTheRoundsAtOnce;
        procedure AFigureThatCannotBeComparedSettlesTheRounds;
        procedure TheRunLogSaysHowManyTurnsAndWhyTheyEnded;
        procedure TheTopicItNamesIsInTheGuide;
        procedure TheRunLogSaysHowTheBackgroundWasFittedToTheFloor;
        procedure ABackgroundWithNoTermsToChooseIsLeftAsItIs;
        procedure TheRunStartsTheBackgroundAtTwelveTermsAndNeverLowersIt;
        procedure TheDataCapsHowManyTermsMayBeTried;
        procedure TheTermsToTryAreTwoMoreAndTwoFewerWithinTheLimits;
        procedure TheLadderIsTheNumbersAboveTheTermsWithinWhatTheDataHold;
        procedure ATermChangeIsKeptOnlyWhenBicIsStrictlyLower;
        procedure TheRunLogSaysWhichTermsChangedAndWhatBicSaid;
    end;

implementation

procedure TBackgroundRoundsTest.TheFirstRoundIsAlwaysFollowedByASecond;
begin
    //  The first round fits the peaks on a background seeded from the data;
    //  only the second fits them on one fitted with them.
    AssertTrue(BackgroundRoundVerdict(1, Infinity, 0.5, False) = brAnother);
end;

procedure TBackgroundRoundsTest.AClearImprovementEarnsAnotherRound;
begin
    AssertTrue(BackgroundRoundVerdict(2, 0.010, 0.009, False) = brAnother);
end;

procedure TBackgroundRoundsTest.AnImprovementBelowAThousandthSettlesTheRounds;
begin
    AssertTrue(BackgroundRoundVerdict(2, 0.010, 0.010 * (1 - 0.5e-3),
        False) = brSettled);
end;

procedure TBackgroundRoundsTest.AFigureThatGotWorseSettlesTheRounds;
begin
    AssertTrue(BackgroundRoundVerdict(2, 0.010, 0.011, False) = brSettled);
    AssertTrue('and one that stayed',
        BackgroundRoundVerdict(2, 0.010, 0.010, False) = brSettled);
end;

procedure TBackgroundRoundsTest.TheLimitEndsTheRoundsHoweverMuchTheyImprove;
begin
    AssertTrue('below it', BackgroundRoundVerdict(BACKGROUND_ROUND_LIMIT - 1,
        1, 0.5, False) = brAnother);
    AssertTrue('at it', BackgroundRoundVerdict(BACKGROUND_ROUND_LIMIT,
        1, 0.5, False) = brLimit);
end;

procedure TBackgroundRoundsTest.AStopEndsTheRoundsAtOnce;
begin
    AssertTrue(BackgroundRoundVerdict(1, Infinity, 0.5, True) = brStopped);
end;

procedure TBackgroundRoundsTest.AFigureThatCannotBeComparedSettlesTheRounds;
begin
    AssertTrue(BackgroundRoundVerdict(1, Infinity, NaN, False) = brSettled);
    AssertTrue(BackgroundRoundVerdict(1, Infinity, Infinity, False) = brSettled);
end;

procedure TBackgroundRoundsTest.TheRunLogSaysHowManyTurnsAndWhyTheyEnded;
var
    E: TRunEvent;
begin
    E := BackgroundRoundsEvent(3, brSettled, 0.0125);
    AssertEquals('the framework''s', RunEventFramework, E.Source);
    AssertEquals(BackgroundRoundsTopic, E.Topic);
    AssertTrue('how many', Pos('3 turn', E.Title) > 0);
    AssertTrue('why they ended', Pos('thousandth', E.Detail) > 0);
    AssertTrue('the figure', Pos('0.0125', E.Detail) > 0);
    AssertTrue('a stop', Pos('Stop', BackgroundRoundsEvent(1, brStopped,
        1).Detail) > 0);
    AssertTrue('the limit', Pos('limit', BackgroundRoundsEvent(5, brLimit,
        1).Detail) > 0);
end;

procedure TBackgroundRoundsTest.TheTopicItNamesIsInTheGuide;
var
    All: TExplanations;
    i: longint;
begin
    //  Written twice - the engine names it, the guide explains it, and no
    //  unit of one may use the other's - so tied here.
    AssertEquals(BackgroundInTurnsTopic, BackgroundRoundsTopic);
    All := FittingExplanations;
    for i := 0 to High(All) do
        if All[i].Topic = BackgroundRoundsTopic then
            Exit;
    Fail('the fitting chapter explains ' + BackgroundRoundsTopic);
end;

procedure TBackgroundRoundsTest.TheRunLogSaysHowTheBackgroundWasFittedToTheFloor;
var
    D: string;
begin
    D := BackgroundFloorDetail(12, 30, 11460.1, 2800, 3160);
    AssertTrue('the peaks do not pull it', Pos('no peak', D) > 0);
    AssertTrue('the terms it started and ended with',
        (Pos('12 terms', D) > 0) and (Pos('30', D) > 0));
    AssertTrue('the samples on it', Pos('2800 of the 3160', D) > 0);
    AssertTrue('the score', Pos('11460.1', D) > 0);
    AssertTrue('kept terms',
        Pos('kept its 12 terms', BackgroundFloorDetail(12, 12, 1, 1, 2)) > 0);
    AssertEquals('a shape with no terms says nothing of them', 0,
        Pos('It ', BackgroundFloorDetail(0, 0, 1, 1, 2)));
end;

procedure TBackgroundRoundsTest.ABackgroundWithNoTermsToChooseIsLeftAsItIs;
begin
    //  A type answering zero for its most terms has none to choose: every
    //  type but a series shape.
    AssertEquals(0, BackgroundStartTerms(0, 0, 0, 3000));
    AssertEquals(0, Length(BackgroundTermsToTry(0, 0, 0, 3000)));
end;

procedure TBackgroundRoundsTest.TheRunStartsTheBackgroundAtTwelveTermsAndNeverLowersIt;
begin
    AssertEquals('from six', 12, BackgroundStartTerms(6, 1, 32, 3000));
    AssertEquals('a model the last run left at 18 starts there', 18,
        BackgroundStartTerms(18, 1, 32, 3000));
end;

procedure TBackgroundRoundsTest.TheDataCapsHowManyTermsMayBeTried;
begin
    //  A term needs samples to be fixed by: eight each. A window of 51
    //  samples holds six, and a run does not start it above what it holds.
    AssertEquals(6, BackgroundTermsCap(32, 51));
    AssertEquals(6, BackgroundStartTerms(6, 1, 32, 51));
    AssertEquals(32, BackgroundTermsCap(32, 3160));
    AssertEquals('never below the least the shape has', 1,
        BackgroundTermsCap(32, 3));
end;

procedure TBackgroundRoundsTest.TheTermsToTryAreTwoMoreAndTwoFewerWithinTheLimits;
var
    T: TBackgroundTermsList;
begin
    T := BackgroundTermsToTry(12, 1, 32, 3160);
    AssertEquals(2, Length(T));
    AssertEquals('more first', 14, T[0]);
    AssertEquals(10, T[1]);
    T := BackgroundTermsToTry(31, 1, 32, 3160);
    AssertEquals('one more is all there is room for', 32, T[0]);
    T := BackgroundTermsToTry(32, 1, 32, 3160);
    AssertEquals('at the ceiling only fewer', 1, Length(T));
    AssertEquals(30, T[0]);
    T := BackgroundTermsToTry(2, 1, 32, 3160);
    AssertEquals(4, T[0]);
    AssertEquals('down to the least', 1, T[1]);
    T := BackgroundTermsToTry(1, 1, 32, 3160);
    AssertEquals('at the least only more', 1, Length(T));
    T := BackgroundTermsToTry(6, 1, 32, 51);
    AssertEquals('what the data hold is the ceiling', 1, Length(T));
    AssertEquals(4, T[0]);
end;

procedure TBackgroundRoundsTest.TheLadderIsTheNumbersAboveTheTermsWithinWhatTheDataHold;
var
    T: TBackgroundTermsList;
begin
    T := BackgroundTermsLadder(12, 1, 64, 3160);
    AssertEquals(5, Length(T));
    AssertEquals(16, T[0]);
    AssertEquals(64, T[4]);
    T := BackgroundTermsLadder(30, 1, 64, 3160);
    AssertEquals('only above', 3, Length(T));
    AssertEquals(32, T[0]);
    T := BackgroundTermsLadder(12, 1, 64, 321);
    AssertEquals('what 321 samples hold is 40', 3, Length(T));
    AssertEquals(32, T[2]);
    AssertEquals('none to choose', 0, Length(BackgroundTermsLadder(0, 0, 0, 3000)));
end;

procedure TBackgroundRoundsTest.ATermChangeIsKeptOnlyWhenBicIsStrictlyLower;
begin
    AssertTrue(BackgroundTermsPay(100, 90));
    AssertFalse('equal is not better', BackgroundTermsPay(100, 100));
    AssertFalse(BackgroundTermsPay(100, 110));
    AssertFalse('a figure that cannot be compared', BackgroundTermsPay(100, NaN));
    AssertFalse(BackgroundTermsPay(NaN, 90));
end;

procedure TBackgroundRoundsTest.TheRunLogSaysWhichTermsChangedAndWhatBicSaid;
var
    E: TRunEvent;
begin
    E := BackgroundTermsEvent(12, 14, 5000.5, 4900.3);
    AssertEquals(RunEventFramework, E.Source);
    AssertEquals(BackgroundRoundsTopic, E.Topic);
    AssertTrue('from', Pos('12', E.Title) > 0);
    AssertTrue('to', Pos('14', E.Title) > 0);
    AssertTrue('the figures', Pos('5000.5', E.Detail) > 0);
    AssertTrue(Pos('4900.3', E.Detail) > 0);
end;

initialization
    RegisterTest('unit', TBackgroundRoundsTest);
end.
