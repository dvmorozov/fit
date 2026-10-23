// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the explanation of how to read a module's report.

  WHY THESE EXIST. A report is a page of verdicts, links and standings, and a
  user meets it with nothing else to go on. Everything in this program explains
  itself, and the report is no exception: its page links here, so what this says
  must name the words the page actually uses - the status words, the "why" and
  "show" links, the "Rules cited" list. A description of a page that no longer
  looks like it is worse than none. }
unit testcase_report_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_registry,
    module_report_types, report_explanations;

type
    TReportExplanationsTest = class(TTestCase)
    published
        procedure TheTopicIsListedUnderItsOwnNamespace;
        procedure ItNamesTheWordsThePageUses;
        procedure ItIsThisSoftwaresChoice;
        procedure ItExplainsNothingElse;
        procedure ItIsWellFormed;
        procedure RegisteredItResolvesThroughTheRegistry;
    end;

implementation

function Explained: TExplanation;
begin
    if not ReportExplanationProvider.Explain(ReadingAReportTopic, Result) then
        raise Exception.Create('the reading topic did not resolve');
end;

function AllText(const E: TExplanation): string;
var
    i: integer;
begin
    Result := E.Summary;
    for i := 0 to High(E.Body) do
        Result := Result + ' ' + E.Body[i];
end;

procedure TReportExplanationsTest.TheTopicIsListedUnderItsOwnNamespace;
var
    Topics: TStringArray;
begin
    Topics := ReportExplanationProvider.StaticTopics;
    AssertEquals(1, Length(Topics));
    AssertEquals(ReadingAReportTopic, Topics[0]);
    AssertEquals(ReportExplanationProvider.Namespace,
        TopicNamespace(ReadingAReportTopic));
end;

procedure TReportExplanationsTest.ItNamesTheWordsThePageUses;
var
    Text: string;
    S: TReportStatus;
begin
    Text := AllText(Explained);
    for S := Low(TReportStatus) to High(TReportStatus) do
        AssertTrue(ReportStatusCaption(S) + ' - ' + Text,
            Pos(ReportStatusCaption(S), Text) > 0);
    AssertTrue('the why link', Pos('"why"', Text) > 0);
    AssertTrue('the show link', Pos('"show"', Text) > 0);
    AssertTrue('the cited rules', Pos('Rules cited', Text) > 0);
end;

procedure TReportExplanationsTest.ItIsThisSoftwaresChoice;
var
    E: TExplanation;
begin
    //  How a page is laid out is decided here; no source states it.
    E := Explained;
    AssertTrue(E.Standing = esModelChoice);
    AssertEquals('no quote to claim', '', E.Quote);
    AssertTrue('a title', E.Title <> '');
end;

procedure TReportExplanationsTest.ItExplainsNothingElse;
var
    E: TExplanation;
begin
    AssertFalse(ReportExplanationProvider.Explain('report/something-else', E));
    AssertFalse(ReportExplanationProvider.Explain('pack/rule/a', E));
end;

procedure TReportExplanationsTest.ItIsWellFormed;
var
    Providers: TExplanationProviders;
    Findings: TStringArray;
    i: integer;
begin
    Providers := nil;
    SetLength(Providers, 1);
    Providers[0] := ReportExplanationProvider;
    Findings := ExplanationFindings(Providers);
    for i := 0 to High(Findings) do
        Fail(Findings[i]);
end;

procedure TReportExplanationsTest.RegisteredItResolvesThroughTheRegistry;
var
    E: TExplanation;
begin
    //  The window registers it and the report's link resolves through the
    //  registry, not through the provider directly. Registering twice is what a
    //  second window would do, and must change nothing.
    RegisterReportExplanations;
    RegisterReportExplanations;
    AssertTrue(FindExplanation(ReadingAReportTopic, E));
    AssertEquals(ReadingAReportTopic, E.Topic);
end;

initialization
    RegisterTest('unit', TReportExplanationsTest);
end.
