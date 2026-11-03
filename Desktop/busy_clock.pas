// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The timing of a long step's busy state: when the window shows it, how
often the window answers the system meanwhile, and how the seconds gone by are
said.)

Kept out of the window so a test can reach it: every instant is handed in, from
GetTickCount64, and nothing here reads a clock or touches a widget.
}
unit busy_clock;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

const
    { A STEP SHORTER THAN THIS SHOWS NO WINDOW: a window that flashes up and
      away over a save of a small project says nothing and draws the eye. The
      status bar and the cursor say it from the first instant. }
    BusyWindowDelayMs = 400;
    { HOW OFTEN THE WINDOW ANSWERS THE SYSTEM while it waits - often enough
      that a move or a repaint follows the pointer, rarely enough that a step
      made of thousands of small phases is not slowed by answering. }
    BusyPumpIntervalMs = 40;

type
    TBusyClock = record
        Started: QWord;
        LastPump: QWord;
        Pumped: boolean;
    end;

{ A clock for a step that began at ANow. }
function StartBusyClock(ANow: QWord): TBusyClock;
{ Whether the step has lasted long enough to show its window. }
function WindowDue(const AClock: TBusyClock; ANow: QWord): boolean;
{ Whether the window should answer the system now; True also notes that it
  did. The first asking always answers. }
function PumpDue(var AClock: TBusyClock; ANow: QWord): boolean;
{ The time gone by, as the window says it: "12 s", "1 min 05 s". }
function ElapsedWords(const AClock: TBusyClock; ANow: QWord): string;

implementation

function StartBusyClock(ANow: QWord): TBusyClock;
begin
    Result.Started := ANow;
    Result.LastPump := ANow;
    Result.Pumped := False;
end;

function WindowDue(const AClock: TBusyClock; ANow: QWord): boolean;
begin
    Result := ANow - AClock.Started >= BusyWindowDelayMs;
end;

function PumpDue(var AClock: TBusyClock; ANow: QWord): boolean;
begin
    Result := (not AClock.Pumped) or
        (ANow - AClock.LastPump >= BusyPumpIntervalMs);
    if Result then
    begin
        AClock.Pumped := True;
        AClock.LastPump := ANow;
    end;
end;

function ElapsedWords(const AClock: TBusyClock; ANow: QWord): string;
var
    Seconds: QWord;
begin
    Seconds := (ANow - AClock.Started) div 1000;
    if Seconds < 60 then
        Result := Format('%d s', [Seconds])
    else
        Result := Format('%d min %.2d s', [Seconds div 60, Seconds mod 60]);
end;

end.
