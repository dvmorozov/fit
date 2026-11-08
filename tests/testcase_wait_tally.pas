// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How long threads waited for a lock, counted the way a log line says it.)

Written for the one question no log could answer: on Windows, fits of several
intervals side by side reported their first progress five to ten times later
than on Linux, and nothing recorded whether a lock was the cause.
}
unit testcase_wait_tally;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, wait_tally;

type
    TWaitTallyTest = class(TTestCase)
    published
        procedure AnEmptyTallySaysNothingWaited;
        procedure EveryWaitIsCountedAndTheLongestKept;
        procedure TheLineSaysCountTotalAndLongestInMilliseconds;
        procedure TheClockNeverRunsBackwards;
        procedure TheClockCountsMicrosecondsNotTicks;
        procedure EachMomentIsInMillisecondsAndZeroIsNever;
    end;

implementation

procedure TWaitTallyTest.AnEmptyTallySaysNothingWaited;
var
    T: TWaitTally;
begin
    T := Default(TWaitTally);
    AssertEquals('none', WaitTallyText(T));
end;

procedure TWaitTallyTest.EveryWaitIsCountedAndTheLongestKept;
var
    T: TWaitTally;
begin
    T := Default(TWaitTally);
    TallyWait(T, 300);
    TallyWait(T, 5000);
    TallyWait(T, 700);
    AssertEquals('every wait counted', 3, T.Count);
    AssertEquals('the waits summed', 6000, T.TotalUs);
    AssertEquals('the longest kept', 5000, T.MaxUs);
end;

procedure TWaitTallyTest.TheLineSaysCountTotalAndLongestInMilliseconds;
var
    T: TWaitTally;
begin
    T := Default(TWaitTally);
    TallyWait(T, 1500);
    TallyWait(T, 250000);
    AssertEquals('2 times, 251.5 ms in all, longest 250.0 ms', WaitTallyText(T));
end;

procedure TWaitTallyTest.TheClockNeverRunsBackwards;
var
    A, B: int64;
    i: longint;
begin
    A := MonotonicMicroseconds;
    for i := 1 to 1000 do
    begin
        B := MonotonicMicroseconds;
        AssertTrue('the clock ran backwards', B >= A);
        A := B;
    end;
end;

procedure TWaitTallyTest.TheClockCountsMicrosecondsNotTicks;
var
    A, B: int64;
begin
    //  WHY A CLOCK OF ITS OWN: GetTickCount64 moves in 15.6 ms steps on
    //  Windows, so a lock wait of a few milliseconds would read as zero there -
    //  the one platform this was written to measure. A 20 ms sleep must read as
    //  roughly that, not as 0 or 31.
    A := MonotonicMicroseconds;
    Sleep(20);
    B := MonotonicMicroseconds;
    AssertTrue(Format('20 ms read as %d us', [B - A]),
        (B - A >= 15000) and (B - A < 2000000));
end;

procedure TWaitTallyTest.EachMomentIsInMillisecondsAndZeroIsNever;
begin
    //  When each interval first reported, from the step's start: an interval
    //  that never did is the finding, so it says so rather than "0.0".
    AssertEquals('12.3, never, 1500.0',
        MillisecondsListText([12345, 0, 1500000]));
    AssertEquals('', MillisecondsListText([]));
end;

initialization
    RegisterTest('unit', TWaitTallyTest);
end.
