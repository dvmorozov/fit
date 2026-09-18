// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for how a module's report is drawn as HTML.

  WHY THESE EXIST. A report's text is the module's - rule messages with measured
  values and limits in them, which contain '<' and '&'. Unescaped, a limit such as
  "x < 1" becomes broken markup. And a report is only teachable if every verdict
  says in WORDS whether it passed, links to the rule's explanation, and shows how
  much authority stands behind that rule - read from the explanation, never
  claimed by the finding. The rendering must also be a pure function of the
  report: the host redraws only when the HTML changes, and output that differed
  between identical reports would redraw on every refresh. }
unit testcase_report_html;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_html,
    module_report_types, report_html, report_explanations;

type
    TReportHtmlTest = class(TTestCase)
    published
        procedure AnEmptyReportShowsTheEmptyText;
        procedure TheTitleIsAHeadingAndTheVerdictIsInWords;
        procedure EveryStatusIsWrittenInWords;
        procedure MeasuredAndLimitAreShownEscaped;
        procedure AMeasurementOrALimitAloneIsStillClosed;
        procedure ModuleTextIsNeverInjectedAsMarkup;
        procedure AResolvedTopicIsALinkWithItsStanding;
        procedure AnUnresolvedTopicIsShownWithoutALink;
        procedure AFindingAboutARowLinksToIt;
        procedure ARowLinkRoundTrips;
        procedure AnotherLinkIsNotARow;
        procedure EachCitedTopicIsListedOnceWithStandingQuoteAndReference;
        procedure NoExplainerStillDrawsTheFindings;
        procedure ASectionIsAHeadingIndentedByDepth;
        procedure ADetachedSectionSaysSo;
        procedure ASectionTopicAndRowAreLinked;
        procedure ProvenanceAndSummaryAreShownOnlyWhenPresent;
        procedure HeaderFindingsComeBeforeTheSections;
        procedure TheSameReportDrawsTheSameHtml;
        procedure AReportLinksToHowToReadIt;
        procedure NoHowToReadLinkWithoutItsExplanation;
    end;

implementation

function Explain(const ATopic: string; out AExplanation: TExplanation): boolean;
begin
    AExplanation := Default(TExplanation);
    Result := False;
    if ATopic = 'pack/rule/a' then
    begin
        AExplanation := NewExplanation(ATopic, 'Rule A', 'Wave 2 stays above.',
            esCanonical);
        AddParagraph(AExplanation, 'Why rule A exists.');
        AExplanation.Quote := 'Wave 2 never retraces more than 100%.';
        AddReference(AExplanation, 'Frost & Prechter', 'Chapter 1', 'https://x.test/a');
        Result := True;
    end
    else if ATopic = 'pack/rule/b' then
    begin
        AExplanation := NewExplanation(ATopic, 'Rule B', 'A guideline.',
            esConvention);
        AddParagraph(AExplanation, 'Why rule B exists.');
        Result := True;
    end;
end;

{ Explain, and the report's own reading topic as the program answers it. }
function ExplainWithHelp(const ATopic: string; out AExplanation: TExplanation): boolean;
begin
    if ATopic = ReadingAReportTopic then
        Result := ReportExplanationProvider.Explain(ATopic, AExplanation)
    else
        Result := Explain(ATopic, AExplanation);
end;

function Contains(const AHaystack, ANeedle: string): boolean;
begin
    Result := Pos(ANeedle, AHaystack) > 0;
end;

function Count(const AHaystack, ANeedle: string): integer;
var
    P: integer;
    S: string;
begin
    Result := 0;
    S := AHaystack;
    P := Pos(ANeedle, S);
    while P > 0 do
    begin
        Inc(Result);
        S := Copy(S, P + Length(ANeedle), MaxInt);
        P := Pos(ANeedle, S);
    end;
end;

function Sample: TModuleReport;
var
    F: TReportFinding;
begin
    Result := Default(TModuleReport);
    Result.Module := 'pack';
    Result.Title := 'Model analysis';
    Result.Verdict := rpFail;
    Result.VerdictText := '1 rule broken';
    SetLength(Result.Sections, 1);
    Result.Sections[0].Title := 'Wave 3 - impulse';
    F := NewReportFinding(rpFail, 'rule', 'Wave 2 retrace', 'Wave 2 went too far.',
        'pack/rule/a');
    F.Measured := '1.20';
    F.Limit := 'below 1.00';
    F.RowId := 'row-7';
    SetLength(Result.Sections[0].Findings, 2);
    Result.Sections[0].Findings[0] := F;
    Result.Sections[0].Findings[1] := NewReportFinding(rpPass, 'guideline',
        'Wave 3 proportion', 'Close to textbook.', 'pack/rule/b');
end;

procedure TReportHtmlTest.AnEmptyReportShowsTheEmptyText;
begin
    AssertEquals(EmptyExplanationHtml('Nothing to report <yet>.'),
        ModuleReportHtml(Default(TModuleReport), @Explain,
        'Nothing to report <yet>.'));
end;

procedure TReportHtmlTest.TheTitleIsAHeadingAndTheVerdictIsInWords;
var
    Html: string;
begin
    Html := ModuleReportHtml(Sample, @Explain, '');
    AssertTrue(Html, Contains(Html, '<h2>Model analysis</h2>'));
    AssertTrue(Html, Contains(Html, '<b>Verdict: Fail</b>'));
    AssertTrue(Html, Contains(Html, '1 rule broken'));
end;

procedure TReportHtmlTest.EveryStatusIsWrittenInWords;
var
    S: TReportStatus;
    R: TModuleReport;
    Html: string;
begin
    for S := Low(TReportStatus) to High(TReportStatus) do
    begin
        R := Sample;
        R.Sections[0].Findings[0].Status := S;
        Html := ModuleReportHtml(R, @Explain, '');
        AssertTrue(Html, Contains(Html, '<b>' + ReportStatusCaption(S) +
            '</b> Wave 2 retrace'));
    end;
end;

procedure TReportHtmlTest.AMeasurementOrALimitAloneIsStillClosed;
var
    R: TModuleReport;
    Html: string;
begin
    //  A module may know what it measured and have no limit to state, or the
    //  reverse; either way the bracket opened around it is closed.
    R := Sample;
    R.Sections[0].Findings[0].Limit := '';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '(measured 1.20)'));

    R := Sample;
    R.Sections[0].Findings[0].Measured := '';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '(limit below 1.00)'));
end;

procedure TReportHtmlTest.MeasuredAndLimitAreShownEscaped;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    R.Sections[0].Findings[0].Measured := 'x < 1 & y';
    R.Sections[0].Findings[0].Limit := '> 2';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, 'measured x &lt; 1 &amp; y'));
    AssertTrue(Html, Contains(Html, 'limit &gt; 2'));
end;

procedure TReportHtmlTest.ModuleTextIsNeverInjectedAsMarkup;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    R.Title := '<script>t</script>';
    R.VerdictText := '<b>v</b>';
    R.Sections[0].Title := '<i>s</i>';
    R.Sections[0].Findings[0].Title := '<u>f</u>';
    R.Sections[0].Findings[0].Detail := 'a "quoted" <tag>';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertFalse(Html, Contains(Html, '<script>'));
    AssertFalse(Html, Contains(Html, '<b>v</b>'));
    AssertFalse(Html, Contains(Html, '<i>s</i>'));
    AssertFalse(Html, Contains(Html, '<u>'));
    AssertFalse(Html, Contains(Html, '<tag>'));
end;

procedure TReportHtmlTest.AResolvedTopicIsALinkWithItsStanding;
var
    Html: string;
begin
    Html := ModuleReportHtml(Sample, @Explain, '');
    AssertTrue(Html, Contains(Html,
        '<i>' + StandingCaption(esCanonical) + '</i> <a href="topic:pack/rule/a">why</a>'));
    AssertTrue(Html, Contains(Html,
        '<i>' + StandingCaption(esConvention) + '</i> <a href="topic:pack/rule/b">why</a>'));
end;

procedure TReportHtmlTest.AnUnresolvedTopicIsShownWithoutALink;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    R.Sections[0].Findings[0].Topic := 'pack/rule/gone';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertFalse(Html, Contains(Html, 'topic:pack/rule/gone'));
    AssertTrue(Html, Contains(Html, 'Wave 2 retrace'));
end;

procedure TReportHtmlTest.AFindingAboutARowLinksToIt;
var
    Html: string;
begin
    Html := ModuleReportHtml(Sample, @Explain, '');
    AssertTrue(Html, Contains(Html, '<a href="' + RowLinkPrefix + 'row-7">show</a>'));
end;

procedure TReportHtmlTest.ARowLinkRoundTrips;
var
    Id: string;
begin
    AssertTrue(RowFromLink(RowLinkPrefix + '{1-2}', Id));
    AssertEquals('{1-2}', Id);
    AssertFalse(RowFromLink(RowLinkPrefix, Id));
    AssertEquals('', Id);
end;

procedure TReportHtmlTest.AnotherLinkIsNotARow;
var
    Id: string;
begin
    AssertFalse(RowFromLink('topic:pack/rule/a', Id));
    AssertFalse(RowFromLink('https://x.test/a', Id));
    AssertEquals('', Id);
end;

procedure TReportHtmlTest.EachCitedTopicIsListedOnceWithStandingQuoteAndReference;
var
    R: TModuleReport;
    Html, Cited: string;
begin
    R := Sample;
    SetLength(R.Header, 1);
    R.Header[0] := NewReportFinding(rpWarning, 'rule', 'Again', 'Same rule.',
        'pack/rule/a');
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '<h3>Rules cited</h3>'));
    Cited := Copy(Html, Pos('<h3>Rules cited</h3>', Html), MaxInt);
    AssertEquals(Cited, 1, Count(Cited, '<b>Rule A</b>'));
    AssertEquals(Cited, 1, Count(Cited, '<b>Rule B</b>'));
    AssertTrue(Cited, Contains(Cited, 'Wave 2 stays above.'));
    AssertTrue(Cited, Contains(Cited, 'Why rule A exists.'));
    AssertTrue(Cited, Contains(Cited,
        '<blockquote>Wave 2 never retraces more than 100%.</blockquote>'));
    AssertTrue(Cited, Contains(Cited, 'Frost &amp; Prechter, Chapter 1'));
    AssertTrue(Cited, Contains(Cited, StandingCaption(esConvention)));
end;

procedure TReportHtmlTest.NoExplainerStillDrawsTheFindings;
var
    Html: string;
begin
    Html := ModuleReportHtml(Sample, nil, '');
    AssertTrue(Html, Contains(Html, 'Wave 2 retrace'));
    AssertFalse(Html, Contains(Html, TopicLinkPrefix));
    AssertFalse(Html, Contains(Html, 'Rules cited'));
end;

procedure TReportHtmlTest.ASectionIsAHeadingIndentedByDepth;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '<h3>Wave 3 - impulse</h3>'));
    R.Sections[0].Indent := 2;
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '<h3>' + SectionIndentMark + SectionIndentMark +
        'Wave 3 - impulse</h3>'));
end;

procedure TReportHtmlTest.ADetachedSectionSaysSo;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    R.Sections[0].IsDetached := True;
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, 'Wave 3 - impulse ' + DetachedSectionNote + '</h3>'));
end;

procedure TReportHtmlTest.ASectionTopicAndRowAreLinked;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    R.Sections[0].Topic := 'pack/rule/b';
    R.Sections[0].RowId := 'row-3';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '<p><a href="topic:pack/rule/b">about</a> ' +
        '<a href="' + RowLinkPrefix + 'row-3">show</a></p>'));
end;

procedure TReportHtmlTest.ProvenanceAndSummaryAreShownOnlyWhenPresent;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    Html := ModuleReportHtml(R, @Explain, '');
    //  Above the first section: the rules cited further down carry sources of
    //  their own in italics, which are not the report's provenance.
    AssertEquals(Html, 0, Count(Copy(Html, 1, Pos('<h3>', Html)), '<p><i>'));
    AssertEquals(Html, 1, Count(Copy(Html, 1, Pos('<h3>', Html)), '<p>'));
    R.Summary := 'Three patterns.';
    R.Provenance := 'As saved <then>.';
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Contains(Html, '<p>Three patterns.</p>'));
    AssertTrue(Html, Contains(Html, '<p><i>As saved &lt;then&gt;.</i></p>'));
end;

procedure TReportHtmlTest.HeaderFindingsComeBeforeTheSections;
var
    R: TModuleReport;
    Html: string;
begin
    R := Sample;
    SetLength(R.Header, 1);
    R.Header[0] := NewReportFinding(rpWarning, 'structure', 'Gap in coverage',
        'Uncovered stretch.', '');
    Html := ModuleReportHtml(R, @Explain, '');
    AssertTrue(Html, Pos('Gap in coverage', Html) > 0);
    AssertTrue(Html, Pos('Gap in coverage', Html) < Pos('<h3>Wave 3', Html));
end;

procedure TReportHtmlTest.TheSameReportDrawsTheSameHtml;
begin
    AssertEquals(ModuleReportHtml(Sample, @Explain, ''),
        ModuleReportHtml(Sample, @Explain, ''));
end;

procedure TReportHtmlTest.AReportLinksToHowToReadIt;
var
    Html: string;
begin
    //  NEXT TO THE VERDICT, before the first section: the reader who needs it
    //  is the one who does not yet know what the rest of the page means.
    Html := ModuleReportHtml(Sample, @ExplainWithHelp, '');
    AssertTrue(Html, Contains(Html, '<a href="' + TopicLinkPrefix +
        ReadingAReportTopic + '">'));
    AssertTrue(Html, Pos(ReadingAReportTopic, Html) < Pos('<h3>', Html));
end;

procedure TReportHtmlTest.NoHowToReadLinkWithoutItsExplanation;
var
    Html: string;
begin
    //  A link to a blank pane is worse than no link.
    Html := ModuleReportHtml(Sample, @Explain, '');
    AssertFalse(Html, Contains(Html, ReadingAReportTopic));
end;

initialization
    RegisterTest('unit', TReportHtmlTest);
end.
