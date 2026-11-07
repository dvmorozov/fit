// SPDX-License-Identifier: GPL-3.0-or-later
{ The clock a computation is timed on. What it leaves out - the time a machine
  spends asleep - cannot be exercised by a test; see run_clock for why each
  platform's source does leave it out. What can be checked is that it is a
  clock at all: it moves forward, at the rate of seconds. }
unit testcase_run_clock;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, run_clock;
type
  TRunClockTest = class(TTestCase)
  published
    procedure ItNeverRunsBackwards;
    procedure ItCountsSeconds;
    procedure ItTellsMillisecondsApart;
  end;
implementation

procedure TRunClockTest.ItNeverRunsBackwards;
var
  i: integer;
  Prev, Cur: TRunTime;
begin
  Prev := RunTime;
  for i := 1 to 100000 do
  begin
    Cur := RunTime;
    AssertTrue(Format('%.9f after %.9f', [Cur, Prev]), Cur >= Prev);
    Prev := Cur;
  end;
end;

procedure TRunClockTest.ItCountsSeconds;
var
  Started, Taken: TRunTime;
begin
  Started := RunTime;
  Sleep(300);
  Taken := RunTime - Started;
  //  Loose on both sides: the Windows source ticks every 15.6 ms, and a busy
  //  machine may oversleep.
  AssertTrue(Format('%.3f s for a 0.3 s sleep', [Taken]),
    (Taken > 0.25) and (Taken < 2));
end;

procedure TRunClockTest.ItTellsMillisecondsApart;
var
  Readings: TStringList;
  Until_: TRunTime;
  Prev, Cur: TRunTime;
begin
  //  THE CLOCK READ IN SINGLE PRECISION: "Hundreds / 1e7" divided by a
  //  constant Free Pascal types as single, and after about a million seconds
  //  of uptime a single can only tell sixteenths of a second apart. A 300 ms
  //  wait then measured 250 or 313 ms, depending on where it fell - which
  //  ItCountsSeconds caught only now and then. Over a tenth of a second of
  //  readings, a clock that resolves milliseconds shows dozens of values.
  Readings := TStringList.Create;
  try
    Readings.Sorted := True;
    Readings.Duplicates := dupIgnore;
    Prev := RunTime;
    Until_ := Prev + 0.1;
    repeat
      Cur := RunTime;
      Readings.Add(FloatToStr(Cur));
    until (Cur >= Until_) or (Readings.Count > 50);
    AssertTrue(Format('%d distinct readings over 0.1 s', [Readings.Count]),
      Readings.Count > 20);
  finally
    Readings.Free;
  end;
end;

initialization
  RegisterTest('unit', TRunClockTest);
end.
