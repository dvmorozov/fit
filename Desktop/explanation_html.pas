// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(An explanation as the small HTML the Explain pane draws.)

WHY HTML AT ALL. An explanation is paragraphs, a list or two, a quotation,
references and links - which a word-wrapping HTML panel draws with no layout code
of ours, and which TurboPowerIPro (already a required package of the client)
provides on every widget set. A memo would lose the structure; owner drawing
would be layout code nobody can test.

WHY IT IS BUILT HERE AND NOT IN THE FORM. The text comes from modules and from
the literature, and it contains '<', '&' and quotes: a formula is 'x < x0', a
rule quotes "Corrective waves are never 5's". Built inside a form, a missing
escape is a broken page found by a user; built here, every section is a
unit-tested function of the explanation.

THE SUBSET IS DELIBERATELY SMALL - h2, h3, p, b, i, ul/li, blockquote, a - and
styled by the panel, not by CSS, so a later change of HTML component has
nothing exotic to support.

INTERNAL LINKS use the scheme 'topic:'. The pane intercepts a click on one and
shows that topic; every other link opens outside the application.
}
unit explanation_html;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    { The scheme of a link to another explanation. }
    TopicLinkPrefix = 'topic:';
    { A paragraph starting with this is a bullet. }
    BulletMark = '• ';

type
    { The title of a topic, or '' when it has none - so a related link reads as
      the thing it leads to rather than as its identifier. }
    TTopicTitle = function(const ATopic: string): string;

function EscapeHtml(const AText: string): string;
{ The whole page for one explanation. ATitleOf may be nil. }
function ExplanationHtml(const AExplanation: TExplanation;
    ATitleOf: TTopicTitle): string;
{ The page shown when nothing in focus can be explained: AText says what to do. }
function EmptyExplanationHtml(const AText: string): string;
{ True, with the topic, when AHref is a link to another explanation. }
function TopicFromLink(const AHref: string; out ATopic: string): boolean;

implementation

function EscapeHtml(const AText: string): string;
begin
    //  The ampersand first, or every entity made below would be escaped again.
    Result := StringReplace(AText, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
    Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

function IsBullet(const AParagraph: string): boolean;
begin
    Result := Copy(AParagraph, 1, Length(BulletMark)) = BulletMark;
end;

function ListItem(const AText: string): string;
begin
    Result := '<li>' + EscapeHtml(AText) + '</li>';
end;

function ExplanationHtml(const AExplanation: TExplanation;
    ATitleOf: TTopicTitle): string;
var
    S, Caption, Line: string;
    InList: boolean;
    i: longint;
    R: TExplanationReference;
begin
    S := '<html><body>';
    S := S + '<h2>' + EscapeHtml(AExplanation.Title) + '</h2>' + LineEnding;
    //  THE STANDING, FIRST AND IN WORDS. Whether this is the field's own rule,
    //  a convention or this software's choice is what a learner most needs and
    //  can least infer, so it is never left to a colour or an icon.
    S := S + '<p><b>' + EscapeHtml(StandingCaption(AExplanation.Standing)) +
        '</b> - <i>' + EscapeHtml(StandingHint(AExplanation.Standing)) +
        '</i></p>' + LineEnding;
    if Trim(AExplanation.Summary) <> '' then
        S := S + '<p><b>' + EscapeHtml(AExplanation.Summary) + '</b></p>' +
            LineEnding;

    InList := False;
    for i := 0 to High(AExplanation.Body) do
    begin
        Line := AExplanation.Body[i];
        if IsBullet(Line) then
        begin
            if not InList then
                S := S + '<ul>';
            InList := True;
            S := S + ListItem(Trim(Copy(Line, Length(BulletMark) + 1, MaxInt)));
        end
        else
        begin
            if InList then
                S := S + '</ul>' + LineEnding;
            InList := False;
            if Trim(Line) <> '' then
                S := S + '<p>' + EscapeHtml(Line) + '</p>' + LineEnding;
        end;
    end;
    if InList then
        S := S + '</ul>' + LineEnding;

    if Trim(AExplanation.Quote) <> '' then
        S := S + '<blockquote>' + EscapeHtml(AExplanation.Quote) +
            '</blockquote>' + LineEnding;

    if Length(AExplanation.Limitations) > 0 then
    begin
        S := S + '<h3>Limitations</h3><ul>';
        for i := 0 to High(AExplanation.Limitations) do
            S := S + ListItem(AExplanation.Limitations[i]);
        S := S + '</ul>' + LineEnding;
    end;

    if Length(AExplanation.References) > 0 then
    begin
        S := S + '<h3>References</h3><ul>';
        for i := 0 to High(AExplanation.References) do
        begin
            R := AExplanation.References[i];
            Line := EscapeHtml(R.Work);
            if Trim(R.Locator) <> '' then
                Line := Line + ', ' + EscapeHtml(R.Locator);
            if Trim(R.Url) <> '' then
                Line := Line + ' <a href="' + EscapeHtml(R.Url) + '">' +
                    EscapeHtml(R.Url) + '</a>';
            S := S + '<li>' + Line + '</li>';
        end;
        S := S + '</ul>' + LineEnding;
    end;

    if Length(AExplanation.Related) > 0 then
    begin
        S := S + '<h3>See also</h3><ul>';
        for i := 0 to High(AExplanation.Related) do
        begin
            Caption := '';
            if Assigned(ATitleOf) then
                Caption := ATitleOf(AExplanation.Related[i]);
            if Caption = '' then
                Caption := AExplanation.Related[i];
            S := S + '<li><a href="' + TopicLinkPrefix +
                EscapeHtml(AExplanation.Related[i]) + '">' +
                EscapeHtml(Caption) + '</a></li>';
        end;
        S := S + '</ul>' + LineEnding;
    end;

    Result := S + '</body></html>';
end;

function EmptyExplanationHtml(const AText: string): string;
begin
    Result := '<html><body><p><i>' + EscapeHtml(AText) +
        '</i></p></body></html>';
end;

function TopicFromLink(const AHref: string; out ATopic: string): boolean;
begin
    ATopic := '';
    Result := False;
    if Copy(AHref, 1, Length(TopicLinkPrefix)) <> TopicLinkPrefix then
        Exit;
    ATopic := Copy(AHref, Length(TopicLinkPrefix) + 1, MaxInt);
    Result := ATopic <> '';
end;

end.
