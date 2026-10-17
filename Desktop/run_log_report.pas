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

{ The log of AEvents; ARunning while the run is still going. }
function RunLogReport(const AEvents: array of TRunEvent;
    ARunning: boolean): TModuleReport;

implementation

const
    FrameworkSource = 'framework';

function SectionTitle(const AEvent: TRunEvent): string;
begin
    Result := AEvent.Section;
    if (AEvent.Source <> '') and (AEvent.Source <> FrameworkSource) then
        Result := Format('%s (%s)', [AEvent.Section, AEvent.Source]);
end;

function RunLogReport(const AEvents: array of TRunEvent;
    ARunning: boolean): TModuleReport;
var
    i, s, n: longint;
    Title: string;
    F: TReportFinding;
begin
    Result := Default(TModuleReport);
    Result.Module := 'run-log';
    Result.Title := RunLogTabCaption;
    for i := 0 to High(AEvents) do
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
        if AEvents[i].Section = RunLogResultSection then
            Result.VerdictText := AEvents[i].Title;
    end;
    if ARunning then
        Result.VerdictText := 'Running...';
end;

end.
