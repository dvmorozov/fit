// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The view a chart shows of its data along one axis: zoomed, scrolled,
and drawn on a scroll bar.)

ONE VIEW, MANY SURFACES. The charts on the model's x - the model's chart, the
strip under it, the Residual Spectrum - show one view, and every gesture on any
of them writes it: a drag, Zoom In and Out, either tab's scroll bar. Each was
once its own mechanism - a TAChart link for zooming, the chart's extent for the
bars - and they drifted apart (the user, 2026-10-14). The window keeps the view
and draws it everywhere; this unit is its arithmetic, where a test can reach it.

A VIEW NEVER LEAVES THE DATA. Zooming out stops at the whole of it, as an image
viewer's does at "fit"; a view past an end is moved in. A view onto nothing is
a chart that draws nothing, and a scroll bar has no place for it.

THE THUMB IS AS LONG AS THE SHARE ON SHOW, as every platform draws a document's,
and the bar is disabled, not hidden, when there is nothing to scroll. The two
directions - thumb to view, view to thumb - are exact inverses, or the thumb
slides from under the pointer on every drag. THE VERTICAL BAR IS UPSIDE DOWN: a
bar's minimum is at the top of the screen, where the chart's maximum is.
}
unit chart_panning;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Math;

type
    { One axis of the chart: the whole extent of the data, and the window
      currently drawn. }
    TAxisWindow = record
        FullMin: double;
        FullMax: double;
        ViewMin: double;
        ViewMax: double;
    end;

const
    { The steps of a scroll bar's track: fine enough that a thumb moved by one
      step moves the view by a hundredth of a percent of the data. }
    BAR_STEPS = 10000;

type
    { A scroll bar as it is set: its track 0..Max, a thumb PageSize long at
      Position, and whether there is anything to scroll. }
    TBarThumb = record
        Max, PageSize, Position: longint;
        Enabled: boolean;
    end;

function WindowIsWhole(const AWindow: TAxisWindow): boolean;
function WholeWindow(const AWindow: TAxisWindow): TAxisWindow;
function WindowScaled(const AWindow: TAxisWindow; AFactor: double): TAxisWindow;
function WindowZoomedTo(const AWindow: TAxisWindow; ALo, AHi: double): TAxisWindow;
function WindowWithFull(const AWindow: TAxisWindow;
    AFullMin, AFullMax: double): TAxisWindow;
function ThumbForWindow(const AWindow: TAxisWindow; AInverted: boolean): TBarThumb;
function WindowForThumb(const AWindow: TAxisWindow; APosition: longint;
    AInverted: boolean): TAxisWindow;

implementation

{ A hair of the data's width: what two ends equal to within are equal. }
function Hair(const AWindow: TAxisWindow): double;
begin
    Result := 1e-9 * Max(1.0, Abs(AWindow.FullMax - AWindow.FullMin));
end;

function WindowIsWhole(const AWindow: TAxisWindow): boolean;
begin
    Result := (AWindow.ViewMin <= AWindow.FullMin + Hair(AWindow)) and
        (AWindow.ViewMax >= AWindow.FullMax - Hair(AWindow));
end;

function WholeWindow(const AWindow: TAxisWindow): TAxisWindow;
begin
    Result := AWindow;
    Result.ViewMin := AWindow.FullMin;
    Result.ViewMax := AWindow.FullMax;
end;

{ KEPT INSIDE THE DATA: a view as wide as the data or wider is the whole of it,
  and a narrower one past an end is moved in, keeping its width. Zooming out
  stops at the whole data, as an image viewer's does at "fit": a view onto
  nothing is a chart that shows nothing, and the scroll bar has no place for
  it. }
function WindowClamped(const AWindow: TAxisWindow): TAxisWindow;
var
    Width: double;
begin
    Result := AWindow;
    Width := AWindow.ViewMax - AWindow.ViewMin;
    if Width >= (AWindow.FullMax - AWindow.FullMin) - Hair(AWindow) then
        Exit(WholeWindow(AWindow));
    if Result.ViewMin < Result.FullMin then
    begin
        Result.ViewMin := Result.FullMin;
        Result.ViewMax := Result.FullMin + Width;
    end;
    if Result.ViewMax > Result.FullMax then
    begin
        Result.ViewMax := Result.FullMax;
        Result.ViewMin := Result.FullMax - Width;
    end;
end;

{ AFactor times as wide about the same centre: below one zooms in. }
function WindowScaled(const AWindow: TAxisWindow; AFactor: double): TAxisWindow;
var
    Centre, Half: double;
begin
    Result := AWindow;
    Centre := (AWindow.ViewMin + AWindow.ViewMax) / 2;
    Half := (AWindow.ViewMax - AWindow.ViewMin) * AFactor / 2;
    Result.ViewMin := Centre - Half;
    Result.ViewMax := Centre + Half;
    Result := WindowClamped(Result);
end;

{ The view ALo..AHi, either way round and CUT at the data's ends - a drag that
  ran past an end asks for what is there. One with no width is a click, not a
  zoom, and changes nothing. }
function WindowZoomedTo(const AWindow: TAxisWindow; ALo, AHi: double): TAxisWindow;
begin
    Result := AWindow;
    if Abs(AHi - ALo) <= Hair(AWindow) then
        Exit;
    Result.ViewMin := Max(Min(ALo, AHi), AWindow.FullMin);
    Result.ViewMax := Min(Max(ALo, AHi), AWindow.FullMax);
    if Result.ViewMax - Result.ViewMin <= Hair(AWindow) then
        Exit(AWindow);
    Result := WindowClamped(Result);
end;

{ The data's range changed - other data, a model drawn wider. A view showing
  all of it shows all of the new; a zoomed one stays, moved inside. }
function WindowWithFull(const AWindow: TAxisWindow;
    AFullMin, AFullMax: double): TAxisWindow;
var
    WasWhole: boolean;
begin
    WasWhole := WindowIsWhole(AWindow);
    Result := AWindow;
    Result.FullMin := AFullMin;
    Result.FullMax := AFullMax;
    if WasWhole then
        Result := WholeWindow(Result)
    else
        Result := WindowClamped(Result);
end;

{ THE THUMB IS AS LONG AS THE SHARE ON SHOW, as every platform's scroll bar
  draws a document: the track is the data, the thumb the view. Nothing to
  scroll is a thumb filling the track, and the bar disabled rather than hidden,
  so the layout does not jump as a zoom comes and goes. }
function ThumbForWindow(const AWindow: TAxisWindow; AInverted: boolean): TBarThumb;
var
    Full, Width, Fraction: double;
    Travel: longint;
begin
    Result.Max := BAR_STEPS - 1;
    Result.PageSize := BAR_STEPS;
    Result.Position := 0;
    Result.Enabled := False;
    Full := AWindow.FullMax - AWindow.FullMin;
    if (Full <= 0) or WindowIsWhole(AWindow) then
        Exit;
    Width := AWindow.ViewMax - AWindow.ViewMin;
    Result.PageSize := EnsureRange(Round(BAR_STEPS * Width / Full), 1, BAR_STEPS - 1);
    Travel := BAR_STEPS - Result.PageSize;
    Fraction := EnsureRange((AWindow.ViewMin - AWindow.FullMin) / (Full - Width), 0, 1);
    Result.Position := Round(Fraction * Travel);
    if AInverted then
        Result.Position := Travel - Result.Position;
    Result.Enabled := True;
end;

{ The view a thumb at APosition asks for: as wide as AWindow, moved. The exact
  inverse of ThumbForWindow, so the thumb stays under the pointer. }
function WindowForThumb(const AWindow: TAxisWindow; APosition: longint;
    AInverted: boolean): TAxisWindow;
var
    Full, Width: double;
    Travel, p: longint;
begin
    Result := AWindow;
    Full := AWindow.FullMax - AWindow.FullMin;
    if (Full <= 0) or WindowIsWhole(AWindow) then
        Exit;
    Width := AWindow.ViewMax - AWindow.ViewMin;
    Travel := BAR_STEPS - EnsureRange(Round(BAR_STEPS * Width / Full), 1, BAR_STEPS - 1);
    p := EnsureRange(APosition, 0, Travel);
    if AInverted then
        p := Travel - p;
    Result.ViewMin := AWindow.FullMin + (p / Travel) * (Full - Width);
    Result.ViewMax := Result.ViewMin + Width;
end;

end.
