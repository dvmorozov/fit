// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The clock a computation's duration is measured on.)

NOT THE WALL CLOCK. Now moves with the calendar, so the time a machine spends
asleep or hibernated counts as time the fit ran, and a change of the system time
moves it too. A fit whose computer was hibernated overnight reported a single
re-fit of eight and a half hours, and drew a flat line across the night on the
loss chart (2026-09-17).

THIS CLOCK STOPS WHILE THE MACHINE SLEEPS, and nothing else moves it:

- Windows: QueryUnbiasedInterruptTime, which leaves out sleep and hibernation.
  GetTickCount64 would not: it keeps counting through both.
- macOS: mach_absolute_time, which stops while the machine sleeps. Free
  Pascal's GetTickCount64 reads gettimeofday there - the wall clock.
- Linux and the BSDs: GetTickCount64, which Free Pascal reads from
  CLOCK_MONOTONIC. That stops while the machine is suspended.

The origin is arbitrary. Only differences between two readings mean anything.
}
unit run_clock;

{$mode objfpc}{$H+}

interface

type
    { Seconds on the run clock. }
    TRunTime = double;

{ The run clock now. }
function RunTime: TRunTime;

implementation

uses
{$IFDEF WINDOWS}
    Windows;
{$ELSE}
    SysUtils;
{$ENDIF}

{$IFDEF WINDOWS}
function QueryUnbiasedInterruptTime(out AUnbiasedTime: QWord): BOOL; stdcall;
    external 'kernel32' name 'QueryUnbiasedInterruptTime';

function RunTime: TRunTime;
var
    Hundreds: QWord;
begin
    //  In units of 100 ns. It cannot fail given a valid pointer.
    QueryUnbiasedInterruptTime(Hundreds);
    Result := Hundreds / 1e7;
end;
{$ELSE}
{$IFDEF DARWIN}
type
    TMachTimebaseInfo = record
        Numer: longword;
        Denom: longword;
    end;

function mach_absolute_time: QWord; cdecl; external 'c';
function mach_timebase_info(var AInfo: TMachTimebaseInfo): longint; cdecl;
    external 'c';

var
    NanosecondsPerTick: double = 0;

function RunTime: TRunTime;
var
    Info: TMachTimebaseInfo;
begin
    //  The ratio is fixed for the life of the process, so it is asked once.
    if NanosecondsPerTick = 0 then
    begin
        Info := Default(TMachTimebaseInfo);
        mach_timebase_info(Info);
        NanosecondsPerTick := Info.Numer / Info.Denom;
    end;
    Result := mach_absolute_time * NanosecondsPerTick / 1e9;
end;
{$ELSE}
function RunTime: TRunTime;
begin
    Result := GetTickCount64 / 1000;
end;
{$ENDIF}
{$ENDIF}

end.
