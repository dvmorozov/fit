// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(How to read a module's report, explained through the one registry.)

A REPORT EXPLAINS ITS RULES, AND THIS EXPLAINS THE REPORT. Every verdict on the
page links to the rule it applies; what the page itself is - its status words,
its links, its list of cited rules - needs explaining once, and the page links
here for it (report_html). Framework words only: which rules a report judges is
its module's to say.

REGISTERED BY THE WINDOW, and only when some module declares a report tab, so a
build with no report lists no explanation of one.
}
unit report_explanations;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry;

const
    ReportNamespace = 'report';
    ReadingAReportTopic = 'report/reading-a-report';

function ReportExplanationProvider: IExplanationProvider;
procedure RegisterReportExplanations;

implementation

type
    TReportExplanationProvider = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

var
    Instance: TReportExplanationProvider = nil;

function TReportExplanationProvider.Namespace: string;
begin
    Result := ReportNamespace;
end;

function TReportExplanationProvider.StaticTopics: TStringArray;
begin
    Result := nil;
    SetLength(Result, 1);
    Result[0] := ReadingAReportTopic;
end;

function TReportExplanationProvider.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    AExplanation := Default(TExplanation);
    Result := ATopic = ReadingAReportTopic;
    if not Result then
        Exit;
    //  THIS SOFTWARE'S CHOICE: how the page is laid out is decided here, and no
    //  source states it.
    AExplanation := NewExplanation(ReadingAReportTopic, 'Reading a report',
        'A report judges the whole model at once. The verdict comes first, then ' +
        'one section per part of the model, then the rules the report cites.',
        esModelChoice);
    AddParagraph(AExplanation, 'Every finding starts with its status in words. ' +
        'Pass means the rule holds. Warning means a guideline is missed: unusual, ' +
        'not wrong. Fail means a rule is broken. Info is neither a pass nor a miss, ' +
        'and says why, for instance a rule that cannot be judged yet.');
    AddParagraph(AExplanation, 'Where a rule is judged by a number, the finding ' +
        'shows what was measured and the limit it is compared with, and the limit ' +
        'says how: below, above, at least, at most, within, or the textbook value ' +
        'a guideline aims at.');
    AddParagraph(AExplanation, 'Each finding says how much authority stands ' +
        'behind its rule - stated by a source, a convention, or this software''s ' +
        'choice - and links to it: "why" opens the rule''s explanation, and "show" ' +
        'selects the part of the model it is about in the Model panel.');
    AddParagraph(AExplanation, 'Rules cited, at the end, lists each rule the ' +
        'report used once, with what it says, its source''s own words where there ' +
        'is a source, and where to find it.');
    AddLimitation(AExplanation, 'A report is rebuilt whenever the model changes. ' +
        'One saved with a project is shown as a note of what it said then; the ' +
        'model is judged again when the project opens.');
end;

function ReportExplanationProvider: IExplanationProvider;
begin
    if not Assigned(Instance) then
        Instance := TReportExplanationProvider.Create;
    Result := Instance;
end;

procedure RegisterReportExplanations;
begin
    RegisterExplanationProvider(ReportExplanationProvider);
end;

finalization
    Instance.Free;
end.
