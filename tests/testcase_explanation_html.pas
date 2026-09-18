// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for how an explanation is drawn as HTML in the Explain pane.

  WHY THESE EXIST. The pane renders HTML, and an explanation's text comes from
  modules and from the field's literature - it contains angle brackets
  (a < x0), ampersands and quotes. Unescaped, a formula becomes broken markup
  and a title becomes an injected tag. And the pane is where a user learns
  whether a statement is canonical, a convention or this software's choice: a
  rendering that dropped the standing, the quote or the limitations would show
  the words and lose exactly what makes them teachable. Each section is asserted
  present when it has content and absent when it has none. }
unit testcase_explanation_html;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_html;

type
    TExplanationHtmlTest = class(TTestCase)
    published
        procedure SpecialCharactersAreEscaped;
        procedure TheTitleIsAHeading;
        procedure TheSummaryIsShown;
        procedure TheStandingIsShownWithItsCaptionAndHint;
        procedure EachParagraphIsItsOwnBlock;
        procedure ConsecutiveBulletLinesBecomeOneList;
        procedure AListThatEndsTheBodyIsClosed;
        procedure AQuoteIsABlockquoteOnlyWhenPresent;
        procedure LimitationsAreListedOnlyWhenThereAreSome;
        procedure AReferenceWithAUrlIsALinkAndOneWithoutIsNot;
        procedure AReferenceShowsItsLocator;
        procedure ARelatedTopicIsAnInternalLinkCaptionedByItsTitle;
        procedure ARelatedTopicWithNoKnownTitleIsCaptionedByItsTopic;
        procedure NoTitleLookupStillLinksRelatedTopics;
        procedure AnInternalLinkParsesBackToItsTopic;
        procedure AnExternalLinkIsNotATopic;
        procedure AnAttributeValueIsEscaped;
        procedure TextIsNeverInjectedAsMarkup;
        procedure TheEmptyStateSaysWhatItIsGivenEscaped;
    end;

implementation

function TitleOfKnown(const ATopic: string): string;
begin
    if ATopic = 'pack/rule/P4' then
        Result := 'Triangles never appear alone as wave 2'
    else
        Result := '';
end;

function Sample: TExplanation;
begin
    Result := NewExplanation('pack/sub/1', 'Sharp as wave 2',
        'A sharp may refine wave 2 of an impulse.', esCanonical);
    AddParagraph(Result, 'Wave 2 is a three.');
    Result.Quote := 'Corrective waves are never 5''s.';
    AddReference(Result, 'Frost & Prechter', 'pp. 86-92', '');
end;

function Contains(const AHaystack, ANeedle: string): boolean;
begin
    Result := Pos(ANeedle, AHaystack) > 0;
end;

procedure TExplanationHtmlTest.SpecialCharactersAreEscaped;
begin
    AssertEquals('a &lt; b &amp;&amp; c &gt; d &quot;q&quot; &#39;s&#39;',
        EscapeHtml('a < b && c > d "q" ''s'''));
end;

procedure TExplanationHtmlTest.TheTitleIsAHeading;
begin
    AssertTrue(Contains(ExplanationHtml(Sample, nil), '<h2>Sharp as wave 2</h2>'));
end;

procedure TExplanationHtmlTest.TheSummaryIsShown;
begin
    AssertTrue(Contains(ExplanationHtml(Sample, nil),
        'A sharp may refine wave 2 of an impulse.'));
end;

procedure TExplanationHtmlTest.TheStandingIsShownWithItsCaptionAndHint;
var
    S: TExplanationStanding;
    E: TExplanation;
    Html: string;
begin
    for S := Low(TExplanationStanding) to High(TExplanationStanding) do
    begin
        E := Sample;
        E.Standing := S;
        Html := ExplanationHtml(E, nil);
        AssertTrue(StandingCaption(S), Contains(Html,
            EscapeHtml(StandingCaption(S))));
        AssertTrue(StandingHint(S), Contains(Html, EscapeHtml(StandingHint(S))));
    end;
end;

procedure TExplanationHtmlTest.EachParagraphIsItsOwnBlock;
var
    E: TExplanation;
    Html: string;
begin
    E := Sample;
    AddParagraph(E, 'Second paragraph.');
    Html := ExplanationHtml(E, nil);
    AssertTrue(Contains(Html, '<p>Wave 2 is a three.</p>'));
    AssertTrue(Contains(Html, '<p>Second paragraph.</p>'));
end;

procedure TExplanationHtmlTest.ConsecutiveBulletLinesBecomeOneList;
var
    E: TExplanation;
    Html: string;
begin
    E := Sample;
    AddParagraph(E, '• sharp');
    AddParagraph(E, '• flat');
    AddParagraph(E, 'After the list.');
    Html := ExplanationHtml(E, nil);
    AssertTrue(Html, Contains(Html, '<ul><li>sharp</li><li>flat</li></ul>'));
    AssertTrue(Contains(Html, '<p>After the list.</p>'));
end;

procedure TExplanationHtmlTest.AQuoteIsABlockquoteOnlyWhenPresent;
var
    E: TExplanation;
begin
    AssertTrue(Contains(ExplanationHtml(Sample, nil),
        '<blockquote>Corrective waves are never 5&#39;s.</blockquote>'));
    E := Sample;
    E.Quote := '';
    AssertFalse(Contains(ExplanationHtml(E, nil), '<blockquote>'));
end;

procedure TExplanationHtmlTest.LimitationsAreListedOnlyWhenThereAreSome;
var
    E: TExplanation;
    Html: string;
begin
    AssertFalse(Contains(ExplanationHtml(Sample, nil), 'Limitations'));
    E := Sample;
    AddLimitation(E, 'Combinations are not modelled.');
    Html := ExplanationHtml(E, nil);
    AssertTrue(Contains(Html, 'Limitations'));
    AssertTrue(Contains(Html, '<li>Combinations are not modelled.</li>'));
end;

procedure TExplanationHtmlTest.AReferenceWithAUrlIsALinkAndOneWithoutIsNot;
var
    E: TExplanation;
    Html: string;
begin
    E := Sample;
    AddReference(E, 'NIST DLMF', '7.19', 'https://dlmf.nist.gov/7.19');
    Html := ExplanationHtml(E, nil);
    AssertTrue(Contains(Html, '<a href="https://dlmf.nist.gov/7.19">'));
    AssertTrue(Contains(Html, 'Frost &amp; Prechter'));
    AssertEquals('only the reference with a url is a link', 1,
        Length(Html) - Length(StringReplace(Html, '<a href="http', 'X' +
        StringOfChar('#', Length('<a href="http') - 2), [rfReplaceAll])));
end;

procedure TExplanationHtmlTest.AReferenceShowsItsLocator;
begin
    AssertTrue(Contains(ExplanationHtml(Sample, nil), 'pp. 86-92'));
end;

procedure TExplanationHtmlTest.ARelatedTopicIsAnInternalLinkCaptionedByItsTitle;
var
    E: TExplanation;
    Html: string;
begin
    E := Sample;
    AddRelated(E, 'pack/rule/P4');
    Html := ExplanationHtml(E, @TitleOfKnown);
    AssertTrue(Html, Contains(Html, '<a href="topic:pack/rule/P4">' +
        'Triangles never appear alone as wave 2</a>'));
end;

procedure TExplanationHtmlTest.ARelatedTopicWithNoKnownTitleIsCaptionedByItsTopic;
var
    E: TExplanation;
    Html: string;
begin
    E := Sample;
    AddRelated(E, 'pack/rule/P9');
    Html := ExplanationHtml(E, @TitleOfKnown);
    AssertTrue(Html, Contains(Html,
        '<a href="topic:pack/rule/P9">pack/rule/P9</a>'));
end;

procedure TExplanationHtmlTest.NoTitleLookupStillLinksRelatedTopics;
var
    E: TExplanation;
begin
    E := Sample;
    AddRelated(E, 'pack/rule/P4');
    AssertTrue(Contains(ExplanationHtml(E, nil),
        '<a href="topic:pack/rule/P4">pack/rule/P4</a>'));
end;

procedure TExplanationHtmlTest.AnInternalLinkParsesBackToItsTopic;
var
    Topic: string;
begin
    AssertTrue(TopicFromLink('topic:pack/rule/P4', Topic));
    AssertEquals('pack/rule/P4', Topic);
end;

procedure TExplanationHtmlTest.AnExternalLinkIsNotATopic;
var
    Topic: string;
begin
    AssertFalse(TopicFromLink('https://dlmf.nist.gov/7.19', Topic));
    AssertEquals('', Topic);
    AssertFalse('an empty topic is no topic', TopicFromLink('topic:', Topic));
end;

procedure TExplanationHtmlTest.AnAttributeValueIsEscaped;
var
    E: TExplanation;
begin
    E := Sample;
    AddReference(E, 'Evil', '', 'https://x.org/?a="b"&c');
    AssertTrue(Contains(ExplanationHtml(E, nil),
        'href="https://x.org/?a=&quot;b&quot;&amp;c"'));
end;

procedure TExplanationHtmlTest.TextIsNeverInjectedAsMarkup;
var
    E: TExplanation;
    Html: string;
begin
    E := Sample;
    E.Title := '<script>alert(1)</script>';
    AddParagraph(E, 'x < x0 and <b>not bold</b>');
    AddLimitation(E, '<i>');
    Html := ExplanationHtml(E, nil);
    AssertFalse(Contains(Html, '<script>'));
    AssertFalse(Contains(Html, '<b>not bold</b>'));
    AssertTrue(Contains(Html, 'x &lt; x0'));
    AssertFalse(Contains(Html, '<li><i></li>'));
end;

procedure TExplanationHtmlTest.TheEmptyStateSaysWhatItIsGivenEscaped;
var
    Html: string;
begin
    Html := EmptyExplanationHtml('Select a curve & see it explained.');
    AssertTrue(Html, Contains(Html, 'Select a curve &amp; see it explained.'));
end;


procedure TExplanationHtmlTest.AListThatEndsTheBodyIsClosed;
var
    E: TExplanation;
    Html: string;
begin
    //  No paragraph follows to close it, so the end of the body must - an
    //  unclosed list swallows every section drawn after it.
    E := NewExplanation('pack/t', 'T', 'A topic ending in a list.', esConvention);
    AddParagraph(E, 'Before.');
    AddParagraph(E, BulletMark + 'first');
    AddParagraph(E, BulletMark + 'last');
    AddLimitation(E, 'After the list.');
    Html := ExplanationHtml(E, nil);
    AssertTrue(Html, Pos('<li>last</li></ul>', Html) > 0);
    AssertTrue('the next section is outside the list: ' + Html,
        Pos('</ul>', Html) < Pos('After the list.', Html));
end;

initialization
    RegisterTest('unit', TExplanationHtmlTest);
end.
