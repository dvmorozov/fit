// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the user is told next while picking, and when a gesture ends.)

THE STATUS LINE IS THE ONLY INSTRUCTION. Picking is a multi-click gesture and
nothing else tells the user how far through it they are - so a prompt asking for
a third point in a two-point gesture, or one that never changes, is the whole of
what the user knows about the state they are in.

All of it lived in nested `case` statements inside a chart click handler,
reachable only by clicking on a chart with data loaded and a mode entered. What
follows is the same rules with the chart taken away.
}
unit testcase_pick_guidance;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, pick_guidance, fit_client;

type
    TPickGuidanceTest = class(TTestCase)
    published
        //  How long a gesture is.
        procedure AnAreaTakesTwoPicks;
        procedure ACurvesBoundsTakeTwo;
        procedure APeakTakesThree;
        procedure TheSetsAreOpenEnded;
        procedure NoModeIsNotAGesture;

        //  When it is over.
        procedure ABoundedGestureEndsAtItsCount;
        procedure ABoundedGestureIsNotOverBeforeThat;
        procedure AnOpenEndedGestureNeverEndsOfItsOwnAccord;
        procedure APastTheEndCountIsStillComplete;

        //  What the user is told.
        procedure EveryStepOfAnAreaHasItsOwnPrompt;
        procedure EveryStepOfAPeakHasItsOwnPrompt;
        procedure ACompletedGestureSaysNothingFurther;
        procedure TheOpenEndedSetsChangeTheirPromptOnce;
        procedure TheIntervalPromptAlternates;
        procedure NoModeSaysNothing;
        procedure EveryPromptAskedForIsDistinctWithinItsGesture;
        procedure NoPromptIsEmptyWhereOneIsExpected;
    end;

implementation

{ ---- how long a gesture is ------------------------------------------------- }

procedure TPickGuidanceTest.AnAreaTakesTwoPicks;
begin
    AssertEquals('two ends', 2, PicksPerGesture(ModeSelectIntervalBounds));
end;

procedure TPickGuidanceTest.ACurvesBoundsTakeTwo;
begin
    AssertEquals('two', 2, PicksPerGesture(ModeSelectCurveBounds));
end;

procedure TPickGuidanceTest.APeakTakesThree;
begin
    //  Two flanks and the peak between them.
    AssertEquals('three', 3, PicksPerGesture(ModeSelectCharacteristicPoints));
end;

procedure TPickGuidanceTest.TheSetsAreOpenEnded;
begin
    //  ZERO MEANS NO NATURAL END. These modes build a set the user keeps adding
    //  to, and stopping them after some fixed number of picks would cut the
    //  background short in the middle of marking it.
    AssertEquals('background', 0, PicksPerGesture(ModeSelectBackground));
    AssertEquals('positions', 0, PicksPerGesture(ModeSelectCurvePositions));
    AssertEquals('intervals', 0, PicksPerGesture(ModeSelectRFactorBounds));
end;

procedure TPickGuidanceTest.NoModeIsNotAGesture;
begin
    AssertEquals('nothing', 0, PicksPerGesture(ModeSelectNothing));
end;

{ ---- when it is over ------------------------------------------------------- }

procedure TPickGuidanceTest.ABoundedGestureEndsAtItsCount;
begin
    //  WHAT THIS CONTROLS is more than a prompt: the mode ends, its markers come
    //  off the chart, and the entry unticks. A mode that outlives what it was
    //  entered for turns the next stray click into a pick nobody meant to make.
    AssertTrue('two ends of an area',
        GestureIsComplete(ModeSelectIntervalBounds, 2));
    AssertTrue('three points of a peak',
        GestureIsComplete(ModeSelectCharacteristicPoints, 3));
end;

procedure TPickGuidanceTest.ABoundedGestureIsNotOverBeforeThat;
begin
    AssertFalse('nothing picked',
        GestureIsComplete(ModeSelectIntervalBounds, 0));
    AssertFalse('one end',
        GestureIsComplete(ModeSelectIntervalBounds, 1));
    AssertFalse('two points of a peak',
        GestureIsComplete(ModeSelectCharacteristicPoints, 2));
end;

procedure TPickGuidanceTest.AnOpenEndedGestureNeverEndsOfItsOwnAccord;
begin
    //  Not after one pick, not after a hundred. It ends when the user leaves it.
    AssertFalse('one background point',
        GestureIsComplete(ModeSelectBackground, 1));
    AssertFalse('a hundred', GestureIsComplete(ModeSelectBackground, 100));
    AssertFalse('a whole set of positions',
        GestureIsComplete(ModeSelectCurvePositions, 50));
end;

procedure TPickGuidanceTest.APastTheEndCountIsStillComplete;
begin
    //  A count above the total means the gesture is over, not that it wrapped.
    //  A `= Needed` test would let a set that somehow gained an extra point read
    //  as unfinished forever.
    AssertTrue('past the end',
        GestureIsComplete(ModeSelectIntervalBounds, 5));
end;

{ ---- what the user is told ------------------------------------------------- }

procedure TPickGuidanceTest.EveryStepOfAnAreaHasItsOwnPrompt;
begin
    //  Pick one end, be asked for the other; pick that, and be sent to the menu
    //  - because marking the area and selecting it are separate acts, and
    //  nothing else says so.
    AssertEquals('after none', HintSecondFinish,
        PickHint(ModeSelectIntervalBounds, 0));
    AssertEquals('after one', HintSelectProfileInterval,
        PickHint(ModeSelectIntervalBounds, 1));
end;

procedure TPickGuidanceTest.EveryStepOfAPeakHasItsOwnPrompt;
begin
    AssertEquals('after none', HintSecondPeak,
        PickHint(ModeSelectCharacteristicPoints, 0));
    AssertEquals('after one', HintThirdFinish,
        PickHint(ModeSelectCharacteristicPoints, 1));
    AssertEquals('after two', HintMovePeak,
        PickHint(ModeSelectCharacteristicPoints, 2));
end;

procedure TPickGuidanceTest.ACompletedGestureSaysNothingFurther;
begin
    //  The last prompt already told the user what to do next. Repeating a
    //  "pick another point" after the gesture is full would ask for something
    //  that will be ignored.
    AssertEquals('a full area', '', PickHint(ModeSelectIntervalBounds, 2));
    AssertEquals('a full peak', '',
        PickHint(ModeSelectCharacteristicPoints, 3));
    AssertEquals('full curve bounds', '', PickHint(ModeSelectCurveBounds, 2));
end;

procedure TPickGuidanceTest.TheOpenEndedSetsChangeTheirPromptOnce;
begin
    //  From "start" to "carry on, or finish through the menu" - and then it
    //  stays, because there is nothing further to say.
    AssertEquals('background, first', HintFirst,
        PickHint(ModeSelectBackground, 0));
    AssertEquals('background, after one', HintNextBackPoint,
        PickHint(ModeSelectBackground, 1));
    AssertEquals('background, after many', HintNextBackPoint,
        PickHint(ModeSelectBackground, 20));

    AssertEquals('positions, first', HintFirst,
        PickHint(ModeSelectCurvePositions, 0));
    AssertEquals('positions, after one', HintNextPoint,
        PickHint(ModeSelectCurvePositions, 1));
end;

procedure TPickGuidanceTest.TheIntervalPromptAlternates;
begin
    //  IN PAIRS. An interval half-marked looks exactly like one fully marked
    //  until the fit uses it, so the prompt is the only thing that says which
    //  end is outstanding.
    //
    //  The parity reads backwards - an even count asks for a RIGHT point - and
    //  is asserted as it behaves. See the note in pick_guidance.
    AssertEquals('none picked', HintNextPointEven,
        PickHint(ModeSelectRFactorBounds, 0));
    AssertEquals('one picked', HintNextPointOdd,
        PickHint(ModeSelectRFactorBounds, 1));
    AssertEquals('a pair picked', HintNextPointEven,
        PickHint(ModeSelectRFactorBounds, 2));
    AssertEquals('three picked', HintNextPointOdd,
        PickHint(ModeSelectRFactorBounds, 3));
end;

procedure TPickGuidanceTest.NoModeSaysNothing;
begin
    AssertEquals('nothing to say', '', PickHint(ModeSelectNothing, 0));
end;

procedure TPickGuidanceTest.EveryPromptAskedForIsDistinctWithinItsGesture;
begin
    //  TWO STEPS WITH THE SAME PROMPT is a gesture the user cannot tell they
    //  have advanced through - which is the failure mode of writing these by
    //  hand in a nested case statement.
    AssertTrue('an area''s two steps differ',
        PickHint(ModeSelectIntervalBounds, 0) <>
        PickHint(ModeSelectIntervalBounds, 1));
    AssertTrue('a peak''s first two differ',
        PickHint(ModeSelectCharacteristicPoints, 0) <>
        PickHint(ModeSelectCharacteristicPoints, 1));
    AssertTrue('and its last two',
        PickHint(ModeSelectCharacteristicPoints, 1) <>
        PickHint(ModeSelectCharacteristicPoints, 2));
    AssertTrue('the interval prompts differ',
        PickHint(ModeSelectRFactorBounds, 0) <>
        PickHint(ModeSelectRFactorBounds, 1));
end;

procedure TPickGuidanceTest.NoPromptIsEmptyWhereOneIsExpected;
var
    M: TSelMode;
    i: longint;
begin
    //  WALKS EVERY MODE, so a picking mode added later without a prompt fails
    //  here rather than leaving the user with a status line that never changes.
    //  Only the steps BEFORE a gesture completes are required to speak.
    for M := Low(TSelMode) to High(TSelMode) do
    begin
        if M = ModeSelectNothing then
            Continue;
        //  A module's own set is named and prompted by the module, not here.
        if M = ModeSelectModulePoints then
            Continue;
        for i := 0 to 3 do
            if not GestureIsComplete(M, i) then
                AssertTrue(Format(
                    'mode %d has a prompt after %d pick(s)', [Ord(M), i]),
                    PickHint(M, i) <> '');
    end;
end;

initialization
    //  A unit test: a mode and a count in, a sentence out. No chart, no click.
    RegisterTest('unit', TPickGuidanceTest);
end.
