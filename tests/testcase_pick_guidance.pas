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
    Classes, SysUtils, fpcunit, testregistry, pick_guidance, fit_client,
    menu_paths;

type
    TPickGuidanceTest = class(TTestCase)
    published
        //  Every menu item a prompt sends the user to is in the menu.
        procedure EveryMenuItemAPromptNamesIsInTheMenu;
        //  How long a gesture is.
        procedure AnAreaTakesTwoPicks;
        procedure TheSetsAreOpenEnded;
        procedure NoModeIsNotAGesture;

        //  When it is over.
        procedure ABoundedGestureEndsAtItsCount;
        procedure ABoundedGestureIsNotOverBeforeThat;
        procedure AnOpenEndedGestureNeverEndsOfItsOwnAccord;
        procedure APastTheEndCountIsStillComplete;

        //  What the user is told.
        procedure EveryStepOfAnAreaHasItsOwnPrompt;
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
end;

procedure TPickGuidanceTest.ABoundedGestureIsNotOverBeforeThat;
begin
    AssertFalse('nothing picked',
        GestureIsComplete(ModeSelectIntervalBounds, 0));
    AssertFalse('one end',
        GestureIsComplete(ModeSelectIntervalBounds, 1));
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

{ ---- where the user is sent ------------------------------------------------ }

procedure TPickGuidanceTest.EveryMenuItemAPromptNamesIsInTheMenu;
var
    Paths: TStringArray;
    Prompts: array of string;
    Joined, Named, Prompt: string;
    i, Open_, Close_: longint;
    Mode: TSelMode;
begin
    //  THE REPORT: after marking a stretch of the profile the status line said
    //  to pick the menu item "Select Area", and there is none - the entry is
    //  Select Data Interval, under Data > Range. A prompt is the only
    //  instruction the user gets in the middle of a gesture, so the item it
    //  names is named by its full path and must exist.
    //
    //  EVERY PROMPT ANY MODE CAN SHOW, not a list of the ones known to name an
    //  item: the characteristic-points gesture ended by sending the user to
    //  "Move Peak to Results", a command that has never existed.
    Paths := DesignedMenuPaths;
    Joined := '|';
    for i := 0 to High(Paths) do
        Joined := Joined + Paths[i] + '|';
    Prompts := nil;
    for Mode := Low(TSelMode) to High(TSelMode) do
        for i := 0 to 6 do
        begin
            Prompt := PickHint(Mode, i);
            if Pos('menu item', Prompt) > 0 then
            begin
                SetLength(Prompts, Length(Prompts) + 1);
                Prompts[High(Prompts)] := Prompt;
            end;
        end;
    AssertTrue('some prompt names a menu item', Length(Prompts) > 0);
    for i := 0 to High(Prompts) do
    begin
        Open_ := Pos('"', Prompts[i]);
        AssertTrue('names an item in quotes: ' + Prompts[i], Open_ > 0);
        Close_ := Pos('"', Copy(Prompts[i], Open_ + 1, MaxInt));
        Named := Copy(Prompts[i], Open_ + 1, Close_ - 1);
        AssertTrue('"' + Named + '" is a menu path', Pos('|' + Named + '|', Joined) > 0);
    end;
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

procedure TPickGuidanceTest.ACompletedGestureSaysNothingFurther;
begin
    //  The last prompt already told the user what to do next. Repeating a
    //  "pick another point" after the gesture is full would ask for something
    //  that will be ignored.
    AssertEquals('a full area', '', PickHint(ModeSelectIntervalBounds, 2));
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
