// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The Residual Spectrum tab's chart, as the window reaches it.)

WHY A SEAM. The Residual Spectrum is the scientific module's to offer
(chart_tabs.OfferChartTab), but its chart - a colour map of the wavelet
scalogram, with its key and its two scroll bars - was built and drawn by the
window, so every product carried it, one without the module included. The
chart is the Free and Pro client's (Desktop/ScientificForms), registered here
from that client's client_modules; the window asks this for it.

A RESTRUCTURING ONLY (the user, 2026-10-17). What the view does is exactly
what the window did with those controls - build them, colour them, draw the
cells of the grid of the client's residual window - and the grid itself is
the module's (difference_scalogram), so the window keeps everything it
decided by asking the view for the numbers it read off the grid:
the shared view of the model's x, the map's wave lengths, the zoom and the
scroll gestures, when to redraw and the self-check's probes, reaching the
chart and the bars through Chart, ScrollBarX and ScrollBarY.
}
unit spectrum_view;

{$mode objfpc}{$H+}

interface

uses
    Classes, Controls, StdCtrls, fit_chart, fit_window, residual_randomness;

type
    ISpectrumView = interface
        ['{8D2A5F17-3C9E-4B60-A1D4-7E6B0C29F853}']
        { Builds the chart and its two bars in ASheet, owned by AOwner: the
          horizontal bar AXBarHeight high under the vertical one AYBarWidth
          wide, the chart in the rest, its x labelled AXAxisLabel. }
        procedure Build(AOwner: TComponent; ASheet: TWinControl;
            AXBarHeight, AYBarWidth: integer; const AXAxisLabel: string);
        function Chart: TFitChart;
        function ScrollBarX: TScrollBar;
        function ScrollBarY: TScrollBar;
        { Takes the scalogram of the difference over AWindow - empty when the
          client holds none (AHeld False) - and draws it: its extent, and its
          cells as asked. }
        procedure Draw(const AWindow: TFitWindow; AHeld: boolean);
        { The periods of the grid last drawn, in samples, ascending; and its
          columns, one per sample. }
        function Periods: TDoubleArray;
        function Columns: longint;
        { The columns a scalogram over AWindow would have, now. }
        function ColumnsFor(const AWindow: TFitWindow): longint;
        { How many kinds of cell the key names, as the legend drew it, and how
          many there are. }
        function KeyNamed: longint;
        function KeyKinds: longint;
        { The colours, the key and its frame from the palette in force. }
        procedure ApplyColors;
    end;

{ Registers the view. The last registration wins: there is one client. }
procedure RegisterSpectrumView(AView: ISpectrumView);
{ The view registered, or nil in a build without one. }
function SpectrumView: ISpectrumView;

implementation

var
    View: ISpectrumView;

procedure RegisterSpectrumView(AView: ISpectrumView);
begin
    View := AView;
end;

function SpectrumView: ISpectrumView;
begin
    Result := View;
end;

end.
