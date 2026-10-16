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
    end;

implementation

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

initialization
    RegisterTest('unit', TRunLogReportTest);
end.
