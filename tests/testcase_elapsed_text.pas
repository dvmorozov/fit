// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a duration reads, the one way the server and the window write it.)

TWO COPIES OF ONE FORMAT, and the server's could not be tested. GetCalcTimeStr
padded each field from the wall clock, so which of its branches ran depended on
how many seconds a test happened to take: the coverage of fit_service moved by a
line or two between runs of the same code, and the ratchet read it as a loss.
The window already had the same shape in FormatElapsed, tested with fixed values.
One function now, here, fed fixed values.
}
unit testcase_elapsed_text;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, elapsed_text;

type
    TElapsedTextTest = class(TTestCase)
    published
        procedure EveryFieldIsTwoDigits;
        procedure TwoDigitFieldsAreNotPadded;
        procedure DaysAreCountedPastTwentyFourHours;
        procedure APartSecondIsDropped;
        procedure ANegativeDurationReadsAsNone;
    end;

implementation

procedure TElapsedTextTest.EveryFieldIsTwoDigits;
begin
    AssertEquals('0 day(s) 01:02:05', ElapsedText(3725));
    AssertEquals('0 day(s) 00:00:00', ElapsedText(0));
end;

procedure TElapsedTextTest.TwoDigitFieldsAreNotPadded;
begin
    AssertEquals('0 day(s) 10:10:10', ElapsedText(36610));
    AssertEquals('0 day(s) 23:59:59', ElapsedText(86399));
end;

procedure TElapsedTextTest.DaysAreCountedPastTwentyFourHours;
begin
    AssertEquals('1 day(s) 00:00:01', ElapsedText(86401));
    AssertEquals('12 day(s) 00:00:00', ElapsedText(12 * 86400));
end;

procedure TElapsedTextTest.APartSecondIsDropped;
begin
    AssertEquals('0 day(s) 00:00:09', ElapsedText(9.99));
end;

procedure TElapsedTextTest.ANegativeDurationReadsAsNone;
begin
    //  Two clocks a moment apart - the server's start and its now.
    AssertEquals('0 day(s) 00:00:00', ElapsedText(-0.5));
end;

initialization
    RegisterTest('unit', TElapsedTextTest);
end.
