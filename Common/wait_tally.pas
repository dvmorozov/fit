// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How long threads waited for a lock, and a clock fine enough to say.)

WHY IT EXISTS. Fits of several intervals side by side reported their first
progress five to ten times later on Windows than on Linux (1.2.0.2040 and
2041), and no log could say whether a lock was the cause: the workers and the
progress reads share two, and nothing measured a wait for either. A tally is
kept by whoever takes the lock - updated once it holds it, so the tally needs no
lock of its own - and written as one line when the step ends.

A CLOCK OF ITS OWN, because GetTickCount64 moves in 15.6 ms steps on Windows: a
wait of a few milliseconds, repeated thousands of times, would read as zero on
the one platform this was written to measure. QueryPerformanceCounter there,
clock_gettime(CLOCK_MONOTONIC) elsewhere - declared here as job_pool declares
sysconf, rather than through a unit only one of the platforms has.

Copyright (C) Dmitry Morozov
}
unit wait_tally;

{$mode objfpc}{$H+}

interface

type
    { Waits for one lock: how many, how long in all, and the longest. }
    TWaitTally = record
        Count: int64;
        TotalUs: int64;
        MaxUs: int64;
    end;

{ Microseconds from an arbitrary origin; never decreasing. }
function MonotonicMicroseconds: int64;

{ Counts one wait of AUs microseconds. }
procedure TallyWait(var ATally: TWaitTally; AUs: int64);

{ "N times, X ms in all, longest Y ms" - or "none". }
function WaitTallyText(const ATally: TWaitTally): string;

{ Moments in microseconds as "12.3, never, 1500.0" milliseconds: 0 is a moment
  that never came - an interval that never reported is the finding. }
function MillisecondsListText(const AUs: array of int64): string;

implementation

uses
{$IFDEF WINDOWS}
    Windows,
{$ENDIF}
{$IFDEF UNIX}
    ctypes,
{$ENDIF}
    SysUtils;

{$IFDEF UNIX}
type
    TTimeSpec = record
        tv_sec: clong;
        tv_nsec: clong;
    end;

function clock_gettime(AClock: cint; out ATime: TTimeSpec): cint; cdecl;
    external 'c' name 'clock_gettime';

const
{$IFDEF DARWIN}
    CLOCK_MONOTONIC = 6;
{$ELSE}
    CLOCK_MONOTONIC = 1;
{$ENDIF}
{$ENDIF}

function MonotonicMicroseconds: int64;
{$IFDEF WINDOWS}
var
    Count, Frequency: int64;
begin
    QueryPerformanceCounter(Count);
    QueryPerformanceFrequency(Frequency);
    //  Split so the product cannot overflow: the counter runs at ~10 MHz.
    Result := (Count div Frequency) * 1000000 +
        ((Count mod Frequency) * 1000000) div Frequency;
end;
{$ELSE}
{$IFDEF UNIX}
var
    T: TTimeSpec;
begin
    clock_gettime(CLOCK_MONOTONIC, T);
    Result := int64(T.tv_sec) * 1000000 + T.tv_nsec div 1000;
end;
{$ELSE}
begin
    Result := int64(GetTickCount64) * 1000;
end;
{$ENDIF}
{$ENDIF}

procedure TallyWait(var ATally: TWaitTally; AUs: int64);
begin
    Inc(ATally.Count);
    Inc(ATally.TotalUs, AUs);
    if AUs > ATally.MaxUs then
        ATally.MaxUs := AUs;
end;

function WaitTallyText(const ATally: TWaitTally): string;
var
    Fmt: TFormatSettings;
begin
    if ATally.Count = 0 then
        Exit('none');
    //  A point whatever the machine's locale: a log is read across machines.
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    Result := Format('%d times, %.1f ms in all, longest %.1f ms',
        [ATally.Count, ATally.TotalUs / 1000, ATally.MaxUs / 1000], Fmt);
end;

function MillisecondsListText(const AUs: array of int64): string;
var
    Fmt: TFormatSettings;
    i: longint;
begin
    Fmt := DefaultFormatSettings;
    Fmt.DecimalSeparator := '.';
    Result := '';
    for i := 0 to High(AUs) do
    begin
        if i > 0 then
            Result := Result + ', ';
        if AUs[i] = 0 then
            Result := Result + 'never'
        else
            Result := Result + Format('%.1f', [AUs[i] / 1000], Fmt);
    end;
end;

end.
