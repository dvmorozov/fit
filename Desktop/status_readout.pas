// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The numbers along the bottom of the window, and how the bar is
divided between them.)

WHAT THE USER WATCHES WHILE A FIT RUNS. The status bar carries the elapsed time,
the goodness of fit, a hint, and whatever advice the engine has - and during a
long fit it is the only thing on screen that changes. What it says and how much
room each part gets were both inside the window: three `Format` calls in three
different handlers, and a width calculation in a resize handler.

WHY THE WIDTHS ARE ARITHMETIC AND NOT A LAYOUT. Two of the panels hold numbers of
a known shape, so they are measured from a sample of that shape and given exactly
what they need; the rest of the bar is shared between the two that hold prose.
Getting the remainder wrong by a pixel is invisible; getting it NEGATIVE is a
widget set that either clamps silently or draws nothing, depending which one.

A number with no fit behind it shows as NOTHING, not as zero. Statistics from a
model that has not been fitted would be read as a fit that went badly.
}
unit status_readout;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_statistics;

const
    { The widest each measured panel ever has to be, as a sample of its own
      shape. Measured rather than guessed, so the panel fits its contents under
      any font the user has. }
    ElapsedSample = 'Elapsed time: 00:00:00.00';
    StatsSample = 'Reduced Chi2: 0.0000   R2: 0.00000';

{ The goodness of fit, as it appears in the bar. Empty when there is no fit -
  a reduced chi-squared shown for a model nobody fitted reads as a fit that went
  badly, which is a worse answer than none. }
function StatisticsSummary(const AStats: TFitStatistics): string;

{ The elapsed time, labelled. }
function ElapsedTimeText(const ATimeStr: string): string;

{ A chart coordinate under the pointer. Fixed width and two decimals, so the two
  readouts do not jitter sideways as the pointer moves - which is what makes a
  number beside a moving crosshair readable at all. }
function CoordinateReadout(AValue: double): string;

{ How wide the two panels that hold prose should be, given the bar's width, the
  space the two measured panels need, and whether the advice panel exists.

  Returns both through the out parameters; a bar too narrow to hold even the
  measured panels leaves them at nothing rather than handing out a negative
  width. }
procedure ProsePanelWidths(ABarWidth, AElapsedWidth, AStatsWidth: longint;
    AHasAdvicePanel: boolean; out AHintWidth, AAdviceWidth: longint);

implementation

function StatisticsSummary(const AStats: TFitStatistics): string;
begin
    if not AStats.Valid then
        Exit('');
    //  The two figures worth a glance; the full set is on the statistics
    //  dialog. %.4g for the chi-squared because its magnitude varies by orders
    //  between models, and a fixed format would show either nothing or noise.
    Result := Format('Reduced Chi2: %.4g   R2: %.5f',
        [AStats.ReducedChiSquare, AStats.RSquared]);
end;

function ElapsedTimeText(const ATimeStr: string): string;
begin
    Result := 'Elapsed time: ' + ATimeStr;
end;

function CoordinateReadout(AValue: double): string;
begin
    Result := Format('%6.2f', [AValue]);
end;

procedure ProsePanelWidths(ABarWidth, AElapsedWidth, AStatsWidth: longint;
    AHasAdvicePanel: boolean; out AHintWidth, AAdviceWidth: longint);
var
    Remaining: longint;
begin
    Remaining := ABarWidth - AElapsedWidth - AStatsWidth;
    //  A window too narrow to hold even the measured panels. Nothing, rather
    //  than a negative width, which one widget set clamps silently and another
    //  draws as an empty bar.
    if Remaining < 0 then
        Remaining := 0;

    if AHasAdvicePanel then
    begin
        //  Halved, with the ODD PIXEL GOING TO THE ADVICE: it holds the longer
        //  text of the two, and losing a character of advice is worse than
        //  losing one of a hint that repeats what the pointer is over.
        AHintWidth := Remaining div 2;
        AAdviceWidth := Remaining - Remaining div 2;
    end
    else
    begin
        AHintWidth := Remaining;
        AAdviceWidth := 0;
    end;
end;

end.
