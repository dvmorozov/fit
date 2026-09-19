// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A module's report as the small HTML its tab draws.)

THE SAME SUBSET AS AN EXPLANATION (explanation_html) - h2, h3, p, b, i, ul/li,
blockquote, a - and drawn by the same view, so a report and the rule it links to
look like one application and a later change of HTML component has one thing to
support.

BUILT HERE AND NOT IN THE FORM, for the reason explanation_html gives: the text
is a module's, full of measured values and limits ("x < 1"), and a missing escape
found by a test is cheaper than one found by a user.

THE STANDING COMES FROM THE TOPIC. A finding shows how much authority stands
behind its rule by asking the rule's explanation, which may only claim canon by
quoting a source. See module_report_types for why a finding cannot say so itself.

A PURE FUNCTION OF THE REPORT. The view skips a redraw when the HTML has not
changed, which is what keeps a refresh during a fit from resetting the scroll
position - so nothing here may vary between two identical reports: no clock, no
counter, no address.
}
unit report_html;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_html, module_report_types,
    report_explanations;

const
    { The link to how a report is read, beside the verdict. }
    HowToReadCaption = 'How to read this report';
    { The scheme of a link to one of the host's model-panel rows. }
    RowLinkPrefix = 'row:';
    { Drawn before a section title once per level of nesting. }
    SectionIndentMark = '&#8627; ';
    { After the title of a section whose parent the model does not have. }
    DetachedSectionNote = '(detached - its parent is missing)';

type
    { The explanation for a topic, when there is one. Shaped like
      explanation_registry.FindExplanation, which is what a host passes. }
    TTopicExplanation = function(const ATopic: string;
        out AExplanation: TExplanation): boolean;

{ The whole page for AReport, or the empty page saying AEmptyText when the report
  holds nothing. AExplain may be nil: findings are then drawn without links,
  standings or citations. }
function ModuleReportHtml(const AReport: TModuleReport;
    AExplain: TTopicExplanation; const AEmptyText: string): string;

{ True, with the row id, when AHref is a link to a model-panel row. }
function RowFromLink(const AHref: string; out ARowId: string): boolean;

implementation

function Explained(AExplain: TTopicExplanation; const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    AExplanation := Default(TExplanation);
    Result := Assigned(AExplain) and (ATopic <> '') and AExplain(ATopic, AExplanation);
end;

function Link(const AHref, ACaption: string): string;
begin
    Result := '<a href="' + EscapeHtml(AHref) + '">' + EscapeHtml(ACaption) + '</a>';
end;

function FindingHtml(const F: TReportFinding; AExplain: TTopicExplanation): string;
var
    E: TExplanation;
begin
    Result := '<li><b>' + ReportStatusCaption(F.Status) + '</b> ' +
        EscapeHtml(F.Title);
    if F.Detail <> '' then
        Result := Result + ' - ' + EscapeHtml(F.Detail);
    if F.Measured <> '' then
        Result := Result + ' (measured ' + EscapeHtml(F.Measured);
    if F.Limit <> '' then
    begin
        if F.Measured <> '' then
            Result := Result + '; limit ' + EscapeHtml(F.Limit) + ')'
        else
            Result := Result + ' (limit ' + EscapeHtml(F.Limit) + ')';
    end
    else if F.Measured <> '' then
        Result := Result + ')';
    //  An unresolved topic is drawn without its link rather than as a link to
    //  a blank pane: ModuleReportFindings is where it is reported.
    if Explained(AExplain, F.Topic, E) then
        Result := Result + ' <i>' + EscapeHtml(StandingCaption(E.Standing)) +
            '</i> ' + Link(TopicLinkPrefix + F.Topic, 'why');
    if F.RowId <> '' then
        Result := Result + ' ' + Link(RowLinkPrefix + F.RowId, 'show');
    Result := Result + '</li>';
end;

function FindingsHtml(const AFindings: TReportFindings;
    AExplain: TTopicExplanation): string;
var
    i: integer;
begin
    Result := '';
    if Length(AFindings) = 0 then
        Exit;
    Result := '<ul>';
    for i := 0 to High(AFindings) do
        Result := Result + FindingHtml(AFindings[i], AExplain);
    Result := Result + '</ul>' + LineEnding;
end;

function CitedHtml(const AReport: TModuleReport;
    AExplain: TTopicExplanation): string;
var
    Topics: TStringArray;
    E: TExplanation;
    R: TExplanationReference;
    Line: string;
    i, j: integer;
begin
    Result := '';
    Topics := ReportTopics(AReport);
    for i := 0 to High(Topics) do
    begin
        if not Explained(AExplain, Topics[i], E) then
            Continue;
        //  WHY, NEXT TO THE VERDICT. The link opens the full explanation; this
        //  is the part a reader of the report should not have to click for:
        //  what the rule says, how much authority stands behind it, and the
        //  source's own words.
        Result := Result + '<p><b>' + EscapeHtml(E.Title) + '</b> - <i>' +
            EscapeHtml(StandingCaption(E.Standing)) + '</i> ' +
            Link(TopicLinkPrefix + Topics[i], 'more') + '</p>' + LineEnding;
        if E.Summary <> '' then
            Result := Result + '<p>' + EscapeHtml(E.Summary) + '</p>' + LineEnding;
        if (Length(E.Body) > 0) and (Trim(E.Body[0]) <> '') then
            Result := Result + '<p>' + EscapeHtml(E.Body[0]) + '</p>' + LineEnding;
        if Trim(E.Quote) <> '' then
            Result := Result + '<blockquote>' + EscapeHtml(E.Quote) +
                '</blockquote>' + LineEnding;
        for j := 0 to High(E.References) do
        begin
            R := E.References[j];
            Line := EscapeHtml(R.Work);
            if Trim(R.Locator) <> '' then
                Line := Line + ', ' + EscapeHtml(R.Locator);
            if Trim(R.Url) <> '' then
                Line := Line + ' ' + Link(R.Url, R.Url);
            Result := Result + '<p><i>Source:</i> ' + Line + '</p>' + LineEnding;
        end;
    end;
    if Result <> '' then
        Result := '<h3>Rules cited</h3>' + LineEnding + Result;
end;

function ModuleReportHtml(const AReport: TModuleReport;
    AExplain: TTopicExplanation; const AEmptyText: string): string;
var
    S, Title, Links: string;
    E: TExplanation;
    i, k: integer;
begin
    if ReportIsEmpty(AReport) then
        Exit(EmptyExplanationHtml(AEmptyText));

    S := '<html><body>';
    S := S + '<h2>' + EscapeHtml(AReport.Title) + '</h2>' + LineEnding;
    //  THE VERDICT IN WORDS, FIRST. A colour or an icon is what a report is
    //  most often read without.
    S := S + '<p><b>Verdict: ' + ReportStatusCaption(AReport.Verdict) + '</b> - ' +
        EscapeHtml(AReport.VerdictText) + '</p>' + LineEnding;
    if AReport.Summary <> '' then
        S := S + '<p>' + EscapeHtml(AReport.Summary) + '</p>' + LineEnding;
    if AReport.Provenance <> '' then
        S := S + '<p><i>' + EscapeHtml(AReport.Provenance) + '</i></p>' + LineEnding;
    //  For the reader who does not yet know what the rest of the page means -
    //  and only when it resolves, since a link to a blank pane is worse than none.
    if Explained(AExplain, ReadingAReportTopic, E) then
        S := S + '<p>' + Link(TopicLinkPrefix + ReadingAReportTopic,
            HowToReadCaption) + '</p>' + LineEnding;

    S := S + FindingsHtml(AReport.Header, AExplain);

    for i := 0 to High(AReport.Sections) do
    begin
        Title := '';
        for k := 1 to AReport.Sections[i].Indent do
            Title := Title + SectionIndentMark;
        Title := Title + EscapeHtml(AReport.Sections[i].Title);
        if AReport.Sections[i].IsDetached then
            Title := Title + ' ' + EscapeHtml(DetachedSectionNote);
        S := S + '<h3>' + Title + '</h3>' + LineEnding;

        Links := '';
        if Explained(AExplain, AReport.Sections[i].Topic, E) then
            Links := Link(TopicLinkPrefix + AReport.Sections[i].Topic, 'about');
        if AReport.Sections[i].RowId <> '' then
        begin
            if Links <> '' then
                Links := Links + ' ';
            Links := Links + Link(RowLinkPrefix + AReport.Sections[i].RowId, 'show');
        end;
        if Links <> '' then
            S := S + '<p>' + Links + '</p>' + LineEnding;

        S := S + FindingsHtml(AReport.Sections[i].Findings, AExplain);
    end;

    S := S + CitedHtml(AReport, AExplain);
    Result := S + '</body></html>';
end;

function RowFromLink(const AHref: string; out ARowId: string): boolean;
begin
    ARowId := '';
    Result := False;
    if Copy(AHref, 1, Length(RowLinkPrefix)) <> RowLinkPrefix then
        Exit;
    ARowId := Copy(AHref, Length(RowLinkPrefix) + 1, MaxInt);
    Result := ARowId <> '';
end;

end.
