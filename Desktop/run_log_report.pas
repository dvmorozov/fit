// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The Run Log: a run's decisions, as a report the window draws.)

WHY THE CURRENT MODEL IS WHAT IT IS. Every decision a run makes - a curve
removed and the numbers that removed it, one kept because taking it away
would leave a pattern, what a module placed and refused - is an event on the
progress channel (fit_progress_log). The Run Log tab shows them, live while a
run goes and, after it, as kept with the model's History entry, so a reopened
project still says why. One tab in every build: the framework logs its own
decisions and a module adds its own through the same channel (the user's
decision, 2026-10-12; a tab of the module's own was rejected - it left the
framework's decisions unsaid).

DRAWN AS A REPORT (module_report_types): sections in the order they first
came, a finding per decision with its reason and the guide topic of the rule
it applied, so it reads and links like every other report.
}
unit run_log_report;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_progress_json, module_report_types;

const
    RunLogTabCaption = 'Run Log';
    RunLogEmptyText = 'No run yet. A fit says here what it decided and why: ' +
        'each curve it removed or kept, and whether what it left is only noise.';
    { The section whose last word is the run's verdict. }
    RunLogResultSection = 'Result';

    { HOW MANY ENTRIES A PAGE OF THE LOG HOLDS. The HTML view lays a document
      out in one call the window cannot answer during, about 4 ms an entry on
      the S&P 500 project's log: a page of this many is under half a second,
      where its 926 entries drawn whole held the window for 3.9 s. }
    RunLogPageSize = 100;
    { The page a log opens on: its last, where a run that goes on writes and
      where its result is. }
    RunLogLastPage = 0;
    { The scheme of a link to a page of the log; the page follows, from 1. }
    RunLogPageLinkPrefix = 'runlog-page:';

{ The log of AEvents; ARunning while the run is still going. APage is the page
  to draw, from 1; RunLogLastPage, or one past the end, draws the last. }
function RunLogReport(const AEvents: array of TRunEvent;
    ARunning: boolean; APage: longint = RunLogLastPage): TModuleReport;
{ True, with the page, when AHref is a link to a page of the log. }
function RunLogPageFromLink(const AHref: string; out APage: longint): boolean;
{ What the window says while it lays out APage of a log of AEvents entries. }
function RunLogLayoutStage(APage, AEvents: longint): string;

implementation

function PageCount(AEvents: longint): longint;
begin
    Result := (AEvents + RunLogPageSize - 1) div RunLogPageSize;
    if Result < 1 then
        Result := 1;
end;

{ APage as it is drawn: the last for RunLogLastPage or a page past the end. }
function PageShown(APage, AEvents: longint): longint;
begin
    Result := APage;
    if (Result < 1) or (Result > PageCount(AEvents)) then
        Result := PageCount(AEvents);
end;

function FirstEntry(APage: longint): longint;
begin
    Result := (APage - 1) * RunLogPageSize;
end;

function LastEntry(APage, AEvents: longint): longint;
begin
    Result := APage * RunLogPageSize;
    if Result > AEvents then
        Result := AEvents;
    Dec(Result);
end;

function RunLogPageFromLink(const AHref: string; out APage: longint): boolean;
begin
    APage := 0;
    Result := (Copy(AHref, 1, Length(RunLogPageLinkPrefix)) = RunLogPageLinkPrefix)
        and TryStrToInt(Copy(AHref, Length(RunLogPageLinkPrefix) + 1, MaxInt),
        APage);
end;

function RunLogLayoutStage(APage, AEvents: longint): string;
var
    Page: longint;
begin
    if AEvents <= RunLogPageSize then
        Exit(Format('Laying out the Run Log: %d entries', [AEvents]));
    Page := PageShown(APage, AEvents);
    Result := Format('Laying out the Run Log: page %d of %d, entries %d to %d of %d',
        [Page, PageCount(AEvents), FirstEntry(Page) + 1,
        LastEntry(Page, AEvents) + 1, AEvents]);
end;

procedure AddLink(var AReport: TModuleReport; const ACaption: string;
    APage: longint);
var
    n: longint;
begin
    n := Length(AReport.PageLinks);
    SetLength(AReport.PageLinks, n + 1);
    AReport.PageLinks[n].Caption := ACaption;
    AReport.PageLinks[n].Href := RunLogPageLinkPrefix + IntToStr(APage);
end;

const
    FrameworkSource = 'framework';

function SectionTitle(const AEvent: TRunEvent): string;
begin
    Result := AEvent.Section;
    if (AEvent.Source <> '') and (AEvent.Source <> FrameworkSource) then
        Result := Format('%s (%s)', [AEvent.Section, AEvent.Source]);
end;

function RunLogReport(const AEvents: array of TRunEvent;
    ARunning: boolean; APage: longint): TModuleReport;
var
    i, s, n, Page, Pages: longint;
    Title: string;
    F: TReportFinding;
begin
    Result := Default(TModuleReport);
    Result.Module := 'run-log';
    Result.Title := RunLogTabCaption;
    //  THE VERDICT IS THE WHOLE RUN'S, on every page: the result is written
    //  last, and a reader on page 1 is owed it as much as one on the last.
    for i := 0 to High(AEvents) do
        if AEvents[i].Section = RunLogResultSection then
            Result.VerdictText := AEvents[i].Title;
    Page := PageShown(APage, Length(AEvents));
    Pages := PageCount(Length(AEvents));
    if Pages > 1 then
    begin
        Result.PageNote := Format('Page %d of %d - entries %d to %d of %d',
            [Page, Pages, FirstEntry(Page) + 1,
            LastEntry(Page, Length(AEvents)) + 1, Length(AEvents)]);
        if Page > 1 then
        begin
            AddLink(Result, 'first', 1);
            AddLink(Result, 'previous', Page - 1);
        end;
        if Page < Pages then
        begin
            AddLink(Result, 'next', Page + 1);
            AddLink(Result, 'last', Pages);
        end;
    end;
    for i := FirstEntry(Page) to LastEntry(Page, Length(AEvents)) do
    begin
        Title := SectionTitle(AEvents[i]);
        s := 0;
        while (s <= High(Result.Sections)) and
              (Result.Sections[s].Title <> Title) do
            Inc(s);
        if s > High(Result.Sections) then
        begin
            SetLength(Result.Sections, s + 1);
            Result.Sections[s].Title := Title;
        end;
        F := NewReportFinding(rpInfo, AEvents[i].Source, AEvents[i].Title,
            AEvents[i].Detail, AEvents[i].Topic);
        F.Measured := Format('%s s into the run',
            [FloatToStrF(AEvents[i].Elapsed, ffFixed, 15, 1)]);
        n := Length(Result.Sections[s].Findings);
        SetLength(Result.Sections[s].Findings, n + 1);
        Result.Sections[s].Findings[n] := F;
    end;
    if ARunning then
        Result.VerdictText := 'Running...';
end;

end.
