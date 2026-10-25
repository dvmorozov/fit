// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(When the window shows that a long step is going, how often it answers
the system meanwhile, and what it says about the time gone by.)

A UNIT test: the clock is handed every instant, so nothing here waits.
}
unit testcase_busy_clock;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, busy_clock;

type
    TBusyClockTest = class(TTestCase)
    published
        procedure AShortStepShowsNoWindow;
        procedure ALongOneShowsItOnceItHasLasted;
        procedure TheWindowAnswersAtMostEveryFewHundredthsOfASecond;
        procedure ButAlwaysOnTheFirstAsking;
        procedure TheSecondsGoneByAreSaidInWholeSeconds;
        procedure AMinuteOrMoreIsSaidInMinutesAndSeconds;
    end;

implementation

procedure TBusyClockTest.AShortStepShowsNoWindow;
var
    Clock: TBusyClock;
begin
    Clock := StartBusyClock(1000);
    AssertFalse('a flash of a window over a step the user never waited for',
        WindowDue(Clock, 1000 + BusyWindowDelayMs - 1));
end;

procedure TBusyClockTest.ALongOneShowsItOnceItHasLasted;
var
    Clock: TBusyClock;
begin
    Clock := StartBusyClock(1000);
    AssertTrue(WindowDue(Clock, 1000 + BusyWindowDelayMs));
end;

procedure TBusyClockTest.TheWindowAnswersAtMostEveryFewHundredthsOfASecond;
var
    Clock: TBusyClock;
begin
    Clock := StartBusyClock(1000);
    AssertTrue('the first', PumpDue(Clock, 1000));
    AssertFalse('too soon after', PumpDue(Clock, 1000 + BusyPumpIntervalMs - 1));
    AssertTrue('due again', PumpDue(Clock, 1000 + BusyPumpIntervalMs));
end;

procedure TBusyClockTest.ButAlwaysOnTheFirstAsking;
var
    Clock: TBusyClock;
begin
    //  A step that began a moment ago has not answered yet: the window does.
    Clock := StartBusyClock(5);
    AssertTrue(PumpDue(Clock, 5));
end;

procedure TBusyClockTest.TheSecondsGoneByAreSaidInWholeSeconds;
var
    Clock: TBusyClock;
begin
    Clock := StartBusyClock(1000);
    AssertEquals('0 s', ElapsedWords(Clock, 1999));
    AssertEquals('12 s', ElapsedWords(Clock, 13500));
end;

procedure TBusyClockTest.AMinuteOrMoreIsSaidInMinutesAndSeconds;
var
    Clock: TBusyClock;
begin
    Clock := StartBusyClock(0);
    AssertEquals('1 min 05 s', ElapsedWords(Clock, 65000));
end;

initialization
    RegisterTest('unit', TBusyClockTest);
end.
