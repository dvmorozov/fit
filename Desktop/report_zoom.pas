// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How large an explanation is drawn: the ladder the zoom buttons walk,
and the page that carries the chosen size.)

WHY A UNIT OF ITS OWN. A zoom is a number and two buttons, and every part of it
that can be wrong is arithmetic: which values count as a size, what the next one
up is, whether either end has been reached, and what a page has to say so the
HTML component draws it that large. None of that needs a screen, and the unit
that owns the component cannot be compiled without one - so the decisions are
here and the view only obeys them.

A LADDER, NOT A PERCENTAGE. Free arithmetic lets a click land on a size that
rounds to the one before it, which is a button that does nothing, and lets a
config file hold a size the buttons can never walk back to. A fixed list makes
every step a visible change and both ends definite.

WHY THE PAGE IS TOUCHED AND NOT JUST THE COMPONENT. TIpHtmlPanel.DefaultFontSize
scales body text alone: TIpHtmlNodeHeader.SetProps takes a heading's size from
FONTSIZESVALUESARRAY, a fixed table, so <h2> and <h3> ignore it entirely. Told
only the font size, the component draws the findings at 24 px and leaves the
section titles above them at 18 - headings SMALLER than the text they head,
which reads as a broken page rather than a zoomed one. That is not a
prediction: it is what the component drew when it was asked.

AND WHY THE SIZE IS WRITTEN ON EACH HEADING. The obvious remedy - a <style>
block giving h2 a font-size of its own - does nothing: the same measurement showed
the component ignoring a heading rule in a style sheet while honouring a
font-size in the tag's own style attribute. So the size is put where it is
read, on the tag, and the component is told the body size as well, because the
two must agree.

NOTHING HERE EDITS THE REPORT. The words, the links and the structure are
report_html's; this adds a size and returns the rest unchanged.
}
unit report_zoom;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, StrUtils;

const
    { The sizes a report can be drawn at, as a percentage of the HTML
      component's own. Ascending, and the default is one of them - the buttons
      walk this list and nothing else.

      SPACED SO EVERY CLICK SHOWS. Steps closer than about 15 % round to the
      same pixel size at the bottom of the range, and a button whose click
      changes nothing is indistinguishable from one that is broken. }
    ReportZoomSteps: array[0..6] of longint = (70, 85, 100, 120, 140, 170, 200);

    { The size a build nobody has zoomed draws at, which is the component's
      own: at this step the page is handed over untouched. }
    ReportZoomDefault = 100;

    { TIpHtmlPanel's own default font size (iphtml.pas, TIpHtmlCustomPanel.
      Create). NAMED HERE so the zoom's own arithmetic starts from what the
      component would have done, and 100 % is therefore exactly today. }
    ReportBaseFontSize = 12;

    { What that component gives <h2> and <h3> (FONTSIZESVALUESARRAY, indexes 4
      and 3). Keeping the ratios means a zoomed page looks like the default one
      enlarged rather than restyled. }
    ReportH2FontSize = 18;
    ReportH3FontSize = 14;

{ The zoom to use, given what was stored. A value that is not one of the steps -
  including the zero an older config.xml holds, because the property existed
  nowhere when it was written - is not a size somebody chose, so it becomes the
  default. Precedent: UsableViewMode. }
function UsableZoom(APercent: longint): longint;

{ Whether there is a step above / below this one. What greys a button. }
function CanZoomIn(APercent: longint): boolean;
function CanZoomOut(APercent: longint): boolean;

{ The next step up / down, or this one at the end of the ladder. A value off the
  ladder is repaired first, so a hand-edited config cannot leave the buttons
  dead. }
function ZoomedIn(APercent: longint): longint;
function ZoomedOut(APercent: longint): longint;

{ The font size the HTML component is told to draw body text at. }
function FontSizeAtZoom(APercent: longint): longint;

{ The page as the component should be given it at this zoom: every heading
  carrying the size it should be drawn at. The body text is not touched here -
  that is the size the component itself is told.

  UNTOUCHED AT THE DEFAULT, where every heading already has the size the
  component would give it anyway. }
function HtmlAtZoom(const AHtml: string; APercent: longint): string;

implementation

{ The index of APercent on the ladder, or -1. }
function StepIndex(APercent: longint): longint;
var
    i: longint;
begin
    Result := -1;
    for i := Low(ReportZoomSteps) to High(ReportZoomSteps) do
        if ReportZoomSteps[i] = APercent then
            Exit(i);
end;

{ The index the ladder is walked from: always a real one. }
function UsableStepIndex(APercent: longint): longint;
begin
    Result := StepIndex(APercent);
    if Result < 0 then
        Result := StepIndex(ReportZoomDefault);
end;

function UsableZoom(APercent: longint): longint;
begin
    Result := ReportZoomSteps[UsableStepIndex(APercent)];
end;

function CanZoomIn(APercent: longint): boolean;
begin
    Result := UsableStepIndex(APercent) < High(ReportZoomSteps);
end;

function CanZoomOut(APercent: longint): boolean;
begin
    Result := UsableStepIndex(APercent) > Low(ReportZoomSteps);
end;

function ZoomedIn(APercent: longint): longint;
var
    i: longint;
begin
    i := UsableStepIndex(APercent);
    if i < High(ReportZoomSteps) then
        Inc(i);
    Result := ReportZoomSteps[i];
end;

function ZoomedOut(APercent: longint): longint;
var
    i: longint;
begin
    i := UsableStepIndex(APercent);
    if i > Low(ReportZoomSteps) then
        Dec(i);
    Result := ReportZoomSteps[i];
end;

{ A default size scaled by the zoom, rounded half up.

  INTEGER ARITHMETIC, deliberately: Round() on a float rounds halves to even, so
  two neighbouring steps could land on the same pixel size for no reason a
  reader of this file could see. }
function ScaledSize(ABase, APercent: longint): longint;
begin
    Result := (ABase * UsableZoom(APercent) + 50) div 100;
end;

function FontSizeAtZoom(APercent: longint): longint;
begin
    Result := ScaledSize(ReportBaseFontSize, APercent);
end;

{ One heading tag with its size written into it. Only the BARE tag is rewritten
  - <h2>, nothing else - because a heading a module wrote with attributes of its
  own is a page this unit does not know the shape of, and a size forced onto it
  could contradict what it already says. Matched whatever case it was written
  in, since nothing makes a module write lower case. }
function SizedHeadings(const AHtml, ATag: string; ASize: longint): string;
var
    Bare, Sized: string;
begin
    Bare := '<' + ATag + '>';
    Sized := '<' + ATag + ' style="font-size:' + IntToStr(ASize) + 'px">';
    Result := StringReplace(AHtml, Bare, Sized, [rfReplaceAll, rfIgnoreCase]);
end;

function HtmlAtZoom(const AHtml: string; APercent: longint): string;
begin
    Result := AHtml;
    if (AHtml = '') or (UsableZoom(APercent) = ReportZoomDefault) then
        Exit;
    Result := SizedHeadings(Result, 'h2', ScaledSize(ReportH2FontSize, APercent));
    Result := SizedHeadings(Result, 'h3', ScaledSize(ReportH3FontSize, APercent));
end;

end.
