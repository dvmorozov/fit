// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where a scroll bar's thumb belongs for the part of the chart on show,
and which part a thumb position asks for.)

A ZOOMED CHART SHOWS A WINDOW ONTO A LARGER WHOLE, and two scroll bars say where
that window sits. The arithmetic runs in both directions and has to agree with
itself: the user drags the thumb, the chart moves, and the chart then puts the
thumb back where it computed it belongs. If the two directions are not exact
inverses, the thumb jumps away from under the pointer on every drag - which
reads as a chart fighting the mouse, not as a formula that disagrees with
itself.

WHY IT WAS WORTH TAKING OUT. It was four copies of one calculation, two per
axis, inside LCL event handlers - and each carried a commented-out alternative
from the time somebody got the direction wrong and swapped the ends by trial.
Nothing could state which way round each bar goes, let alone that the two
directions match.

THE TWO BARS DO GENUINELY DIFFER. The horizontal one runs the way its values do:
thumb at the minimum, window at the left of the chart. The vertical one is
upside down, because a scroll bar's minimum is at the TOP of the screen while
the chart's maximum is - so its thumb at the minimum means the window at the top
of the data. That is the only difference between them, and it is the `AInverted`
flag rather than a second copy of the formula.
}
unit chart_panning;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math;

type
    { A scroll bar: the range it spans and where its thumb sits. }
    TBarRange = record
        Min: longint;
        Max: longint;
        Position: longint;
    end;

    { One axis of the chart: the whole extent of the data, and the window
      currently drawn. }
    TAxisWindow = record
        FullMin: double;
        FullMax: double;
        ViewMin: double;
        ViewMax: double;
    end;

{ Builds the two records, so a caller reading widget properties writes one line
  rather than four assignments. }
function BarRange(AMin, AMax, APosition: longint): TBarRange;
function AxisWindow(AFullMin, AFullMax, AViewMin, AViewMax: double): TAxisWindow;

{ How much of the whole is off-screen: the travel the thumb represents. Zero
  when the chart is not zoomed, and NEGATIVE when the window is wider than the
  data - which happens after a zoom out past the extent. }
function HiddenSpan(const AWindow: TAxisWindow): double;

{ True when there is anything to scroll at all.

  A chart showing everything has nothing off-screen, so every thumb position
  means the same window and the arithmetic divides by zero. Both directions
  answer "leave it alone" rather than producing an infinity that reaches the
  chart as its new extent. }
function CanPan(const AWindow: TAxisWindow; const ABar: TBarRange): boolean;

{ How far through the travel the window sits, from 0 (against FullMin) to 1
  (against FullMax). Clamped, because a window can sit slightly outside the
  extent after a zoom and a thumb outside its own range is not a thing the
  widget can show. }
function WindowFraction(const AWindow: TAxisWindow): double;

{ Where the thumb belongs for the window now shown. }
function BarPositionForWindow(const AWindow: TAxisWindow;
    const ABar: TBarRange; AInverted: boolean): longint;

{ The window a thumb position asks for. THE WIDTH IS PRESERVED: scrolling moves
  the window and never resizes it - a pan that also zoomed would change the
  scale under the user as they dragged. }
function WindowForBarPosition(const AWindow: TAxisWindow;
    const ABar: TBarRange; AInverted: boolean): TAxisWindow;

implementation

function BarRange(AMin, AMax, APosition: longint): TBarRange;
begin
    Result.Min := AMin;
    Result.Max := AMax;
    Result.Position := APosition;
end;

function AxisWindow(AFullMin, AFullMax, AViewMin, AViewMax: double): TAxisWindow;
begin
    Result.FullMin := AFullMin;
    Result.FullMax := AFullMax;
    Result.ViewMin := AViewMin;
    Result.ViewMax := AViewMax;
end;

function HiddenSpan(const AWindow: TAxisWindow): double;
begin
    Result := (AWindow.FullMax - AWindow.FullMin) -
        (AWindow.ViewMax - AWindow.ViewMin);
end;

function CanPan(const AWindow: TAxisWindow; const ABar: TBarRange): boolean;
begin
    //  A bar with no range of its own cannot express a position either, and
    //  dividing by it is the same division by zero from the other side.
    Result := (HiddenSpan(AWindow) > 0) and (ABar.Max > ABar.Min);
end;

function WindowFraction(const AWindow: TAxisWindow): double;
var
    Hidden: double;
begin
    Hidden := HiddenSpan(AWindow);
    if Hidden <= 0 then
        Exit(0);
    Result := (AWindow.ViewMin - AWindow.FullMin) / Hidden;
    //  CLAMPED. A window may sit a little outside the extent - a zoom out
    //  centred near an edge does it - and a fraction outside [0, 1] becomes a
    //  thumb position outside the bar, which the widget silently pins to an end
    //  anyway. Doing it here means the position handed back is the one that
    //  will be shown.
    if Result < 0 then
        Result := 0;
    if Result > 1 then
        Result := 1;
end;

function BarPositionForWindow(const AWindow: TAxisWindow;
    const ABar: TBarRange; AInverted: boolean): longint;
var
    Fraction: double;
begin
    if not CanPan(AWindow, ABar) then
        //  Nothing off-screen: the thumb stays where it is rather than jumping
        //  to an end the user did not ask for.
        Exit(ABar.Position);
    Fraction := WindowFraction(AWindow);
    if AInverted then
        Result := ABar.Max - Round(Fraction * (ABar.Max - ABar.Min))
    else
        Result := ABar.Min + Round(Fraction * (ABar.Max - ABar.Min));
end;

function WindowForBarPosition(const AWindow: TAxisWindow;
    const ABar: TBarRange; AInverted: boolean): TAxisWindow;
var
    Fraction, Width: double;
begin
    Result := AWindow;
    if not CanPan(AWindow, ABar) then
        Exit;
    if AInverted then
        Fraction := (ABar.Max - ABar.Position) / (ABar.Max - ABar.Min)
    else
        Fraction := (ABar.Position - ABar.Min) / (ABar.Max - ABar.Min);
    Width := AWindow.ViewMax - AWindow.ViewMin;
    Result.ViewMin := AWindow.FullMin + Fraction * HiddenSpan(AWindow);
    Result.ViewMax := Result.ViewMin + Width;
end;

end.
