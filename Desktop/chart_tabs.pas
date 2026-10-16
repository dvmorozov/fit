// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The tabs of the chart area, and what the spectrum tab draws.)

THREE CHARTS, ONE TAB EACH (the user's decision, 2026-10-12): the model over
the data, the R-factor a run reached against its time, and the spectrum of
the difference. The R-factor chart used to replace the model's while a fit ran
and vanish when it ended; in a tab of its own it keeps the last run's curve
until the next run starts, and View > Animation Mode only decides whether the
model's curves move during a fit.

THE WINDOW NEVER SWITCHES TABS BY ITSELF. A tab that moves under the user's
hand when a fit starts is worse than one they switch to; the line over the
charts and the status bar report a run whichever tab is in front. That is this
software's choice; switching to the R-factor at every fit was the rejected
alternative.

Kept out of the form so each decision is a function a test calls.
}
unit chart_tabs;

{$mode objfpc}{$H+}

interface

uses
    Math;

type
    TChartTab = (ctModel, ctRFactor, ctSpectrum);


const
    ChartTabCount = Ord(High(TChartTab)) + 1;
    { THE RESIDUAL SPECTRUM TAB (Stage I4): the wavelet scalogram of the
      difference, where and at which rhythm it holds more than noise
      (difference_scalogram). }
    SpectrumChartTitle = 'Where, and at which rhythm, the difference is not noise';
    SpectrumYAxisLabel = 'Rhythm (samples per cycle)';
    SpectrumChartCaption = 'Across: where. Upwards: how long a wave, in ' +
        'points. The key below says what each colour means.';
    SpectrumChartHint = 'The difference between the data and the model, ' +
        'point by point across and wave length by wave length upwards. A cell ' +
        'in the colour of the shaded stretches is a wave the model misses ' +
        'there; grey cells are noise, the darker the stronger; near the ends ' +
        'of a fit interval nothing is judged.';
    { THE STRIP UNDER THE MODEL'S CHART (Stage I3): where the difference holds
      a rhythm noise would not, at each point of the fit intervals. Its words
      are here so the guide and a test can name them. }
    { Short: the strip is some hundred pixels tall, and 'Rhythm over noise'
      did not fit beside it (the user, 2026-10-14). }
    StructureYAxisLabel = 'x noise';
    StructureDifferenceAxisLabel = 'Difference';
    StructureChartHint = 'How much rhythm the difference between the data ' +
        'and the model holds at each point, against what noise would hold ' +
        '(1 for noise). Where it reaches the dashed line the model has ' +
        'something left to describe - the shaded stretches of the chart above. ' +
        'The difference itself is drawn against the right-hand axis.';
    { Its height at 96 dpi, in pixels. This software's choice: enough for a
      spike to read, little enough to leave the model its chart. }
    StructureStripHeight96 = 110;

function ChartTabCaption(ATab: TChartTab): string;
{ The guide topic that explains what the tab shows. }
function ChartTabTopic(ATab: TChartTab): string;
{ The tab a project saved as AIndex opens on: the model's when that tab is not
  one this build has. }
function RestoredChartTab(AIndex: longint): TChartTab;

implementation

function ChartTabCaption(ATab: TChartTab): string;
begin
    case ATab of
        ctModel:    Result := 'Model';
        ctRFactor:  Result := 'R-factor';
        ctSpectrum: Result := 'Residual Spectrum';
    end;
end;

function ChartTabTopic(ATab: TChartTab): string;
begin
    case ATab of
        ctModel:    Result := 'the-window/chart';
        ctRFactor:  Result := 'fit-progress/live-progress';
        ctSpectrum: Result := 'fitting/is-the-difference-noise';
    end;
end;

function RestoredChartTab(AIndex: longint): TChartTab;
begin
    if (AIndex < Ord(Low(TChartTab))) or (AIndex > Ord(High(TChartTab))) then
        Result := ctModel
    else
        Result := TChartTab(AIndex);
end;

end.
