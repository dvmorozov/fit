// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The Run Log: a run's decisions, as a report the window draws.)
}
unit testcase_run_log_report;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_progress_json, module_report_types,
    run_log_report;

type
    TRunLogReportTest = class(TTestCase)
    private
        function Event(const ASource, ASection, ATitle: string;
            AElapsed: double): TRunEvent;
    published
        procedure NoEventsIsAnEmptyLog;
        procedure EachSectionIsAHeadingInTheOrderItFirstCame;
        procedure ADecisionKeepsItsReasonItsTopicAndWhen;
        procedure AModulesDecisionsAreHeadedByTheModule;
        procedure TheVerdictIsTheResultsLastWord;
        procedure ARunningLogSaysSo;
        //  A long log, a page at a time: laid out whole, 926 entries held the
        //  window for 3.9 s (2026-10-25).
        procedure ALongLogIsDrawnAPageAtATime;
        procedure TheLastPageIsShownUnlessAnotherIsChosen;
        procedure APageBeyondTheEndShowsTheLast;
        procedure TheVerdictIsTheWholeRunsOnEveryPage;
        procedure EachPageLinksToTheOthersButNotToItself;
        procedure ALogThatFitsOnAPageHasNoPageLine;
        procedure APageLinkReadsBack;
        procedure TheStageSaysWhatIsLaidOut;
    end;

implementation

function Events(ACount: longint): TRunEvents;
var
    i: longint;
begin
    Result := nil;
    SetLength(Result, ACount);
    for i := 0 to ACount - 1 do
    begin
        Result[i] := Default(TRunEvent);
        Result[i].Source := 'framework';
        Result[i].Section := 'Reduction';
        Result[i].Title := 'decision ' + IntToStr(i + 1);
    end;
end;

function FindingCount(const AReport: TModuleReport): longint;
var
    s: longint;
begin
    Result := 0;
    for s := 0 to High(AReport.Sections) do
        Inc(Result, Length(AReport.Sections[s].Findings));
end;

function Hrefs(const AReport: TModuleReport): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AReport.PageLinks) do
        Result := Result + AReport.PageLinks[i].Caption + '=' +
            AReport.PageLinks[i].Href + ';';
end;

function TRunLogReportTest.Event(const ASource, ASection, ATitle: string;
    AElapsed: double): TRunEvent;
begin
    Result := Default(TRunEvent);
    Result.Source := ASource;
    Result.Section := ASection;
    Result.Title := ATitle;
    Result.Detail := 'why ' + ATitle;
    Result.Topic := 'fitting/how-many-curves';
    Result.Elapsed := AElapsed;
end;

procedure TRunLogReportTest.NoEventsIsAnEmptyLog;
begin
    AssertTrue(ReportIsEmpty(RunLogReport([], False)));
end;

procedure TRunLogReportTest.EachSectionIsAHeadingInTheOrderItFirstCame;
var
    R: TModuleReport;
begin
    R := RunLogReport([Event('framework', 'Reduction', 'a', 1),
        Event('framework', 'Result', 'b', 2),
        Event('framework', 'Reduction', 'c', 3)], False);
    AssertEquals(2, Length(R.Sections));
    AssertEquals('Reduction', R.Sections[0].Title);
    AssertEquals('both of its decisions', 2, Length(R.Sections[0].Findings));
    AssertEquals('c', R.Sections[0].Findings[1].Title);
    AssertEquals('Result', R.Sections[1].Title);
end;

procedure TRunLogReportTest.ADecisionKeepsItsReasonItsTopicAndWhen;
var
    F: TReportFinding;
begin
    F := RunLogReport([Event('framework', 'Reduction', 'a', 3.5)],
        False).Sections[0].Findings[0];
    AssertEquals('why a', F.Detail);
    AssertEquals('fitting/how-many-curves', F.Topic);
    AssertEquals('3.5 s into the run', F.Measured);
end;

procedure TRunLogReportTest.AModulesDecisionsAreHeadedByTheModule;
var
    R: TModuleReport;
begin
    R := RunLogReport([Event('amodule', 'Placement', 'a', 1)], False);
    AssertEquals('Placement (amodule)', R.Sections[0].Title);
end;

procedure TRunLogReportTest.TheVerdictIsTheResultsLastWord;
var
    R: TModuleReport;
begin
    R := RunLogReport([Event('framework', 'Reduction', 'a', 1),
        Event('framework', 'Result', 'Difference: random (p = 0.31)', 2)],
        False);
    AssertEquals('Difference: random (p = 0.31)', R.VerdictText);
end;

procedure TRunLogReportTest.ARunningLogSaysSo;
begin
    AssertTrue(Pos('running',
        LowerCase(RunLogReport([Event('framework', 'Reduction', 'a', 1)],
        True).VerdictText)) > 0);
end;

procedure TRunLogReportTest.ALongLogIsDrawnAPageAtATime;
var
    R: TModuleReport;
begin
    R := RunLogReport(Events(250), False, 1);
    AssertEquals('a page of entries', RunLogPageSize, FindingCount(R));
    AssertEquals('the first of them', 'decision 1', R.Sections[0].Findings[0].Title);
    AssertEquals('Page 1 of 3 - entries 1 to 100 of 250', R.PageNote);
end;

procedure TRunLogReportTest.TheLastPageIsShownUnlessAnotherIsChosen;
var
    R: TModuleReport;
begin
    //  The last page is where a run that goes on writes, and where its result is.
    R := RunLogReport(Events(250), False);
    AssertEquals(50, FindingCount(R));
    AssertEquals('decision 201', R.Sections[0].Findings[0].Title);
    AssertEquals('Page 3 of 3 - entries 201 to 250 of 250', R.PageNote);
end;

procedure TRunLogReportTest.APageBeyondTheEndShowsTheLast;
begin
    //  A page chosen while the log was another's, or longer.
    AssertEquals('Page 3 of 3 - entries 201 to 250 of 250',
        RunLogReport(Events(250), False, 9).PageNote);
end;

procedure TRunLogReportTest.TheVerdictIsTheWholeRunsOnEveryPage;
var
    E: TRunEvents;
begin
    E := Events(250);
    E[249].Section := RunLogResultSection;
    E[249].Title := 'Difference: random';
    AssertEquals('Difference: random', RunLogReport(E, False, 1).VerdictText);
end;

procedure TRunLogReportTest.EachPageLinksToTheOthersButNotToItself;
begin
    AssertEquals('first=' + RunLogPageLinkPrefix + '1;previous=' +
        RunLogPageLinkPrefix + '1;next=' + RunLogPageLinkPrefix + '3;last=' +
        RunLogPageLinkPrefix + '3;',
        Hrefs(RunLogReport(Events(250), False, 2)));
    AssertEquals('next=' + RunLogPageLinkPrefix + '2;last=' +
        RunLogPageLinkPrefix + '3;',
        Hrefs(RunLogReport(Events(250), False, 1)));
end;

procedure TRunLogReportTest.ALogThatFitsOnAPageHasNoPageLine;
var
    R: TModuleReport;
begin
    R := RunLogReport(Events(RunLogPageSize), False);
    AssertEquals('', R.PageNote);
    AssertEquals(0, Length(R.PageLinks));
end;

procedure TRunLogReportTest.APageLinkReadsBack;
var
    Page: longint;
begin
    AssertTrue(RunLogPageFromLink(RunLogPageLinkPrefix + '7', Page));
    AssertEquals(7, Page);
    AssertFalse('another link', RunLogPageFromLink('row:7', Page));
    AssertFalse('no number', RunLogPageFromLink(RunLogPageLinkPrefix + 'x', Page));
end;

procedure TRunLogReportTest.TheStageSaysWhatIsLaidOut;
begin
    AssertEquals('Laying out the Run Log: page 10 of 10, entries 901 to 926 of 926',
        RunLogLayoutStage(RunLogLastPage, 926));
    AssertEquals('Laying out the Run Log: 26 entries',
        RunLogLayoutStage(RunLogLastPage, 26));
end;

initialization
    RegisterTest('unit', TRunLogReportTest);
end.
