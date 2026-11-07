// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Reading the few things this module needs out of a web page.)

A SERVICE WITHOUT AN API IS READ FROM ITS PAGES, and that is a liability worth
stating: a page is written for a person, its markup changes without notice, and
a scraper that guesses produces a plausible wrong answer. So this unit does the
smallest possible thing - find links, find the title - and every source built on
it REFUSES IN WORDS when it finds nothing, rather than reporting an empty
result. An empty list reads as "there is nothing there"; a refusal naming what
was looked for reads as "this needs fixing", which is the truth.

WHY IT IS IN THIS MODULE AND NOT IN THE FRAMEWORK. Only this module scrapes.
The moment a second one needs to, this moves to the framework - and not before,
because a shared unit with one caller is a decision nobody has had to make yet.

NOT AN HTML PARSER. It matches attributes in a string. That is enough for a
list of links and refuses to pretend otherwise; anything needing structure needs
a parser, and that is a different decision.
}
unit html_scan;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { One link found on a page. }
    THtmlLink = record
        Href: string;
        Text: string;
    end;

    THtmlLinks = array of THtmlLink;

{ Every link whose href contains AContains, with the text between the tags.
  Entities that appear in a URL are decoded (&amp; above all), because a page
  writes them and a fetch needs them undone. }
function LinksContaining(const APage, AContains: string): THtmlLinks;

{ The page's title, or '' - what a search service says it found. }
function PageTitle(const APage: string): string;

{ The value of a query parameter inside a URL: ParameterOf('...ID=C71432&x=1',
  'ID') is 'C71432'. }
function ParameterOf(const AUrl, AName: string): string;

{ The markup entities a URL or a title can carry, undone. }
function Unescaped(const AText: string): string;

implementation

uses
    StrUtils;

function Unescaped(const AText: string): string;
begin
    Result := StringReplace(AText, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '&#39;', '''', [rfReplaceAll]);
    Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
    //  Last, or an entity written as '&amp;lt;' would be undone twice.
    Result := StringReplace(Result, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

function PageTitle(const APage: string): string;
var
    Lower: string;
    Start, Stop: longint;
begin
    Result := '';
    Lower := LowerCase(APage);
    Start := Pos('<title>', Lower);
    if Start = 0 then
        Exit;
    Inc(Start, Length('<title>'));
    Stop := PosEx('</title>', Lower, Start);
    if Stop = 0 then
        Exit;
    Result := Trim(Unescaped(Copy(APage, Start, Stop - Start)));
end;

function LinksContaining(const APage, AContains: string): THtmlLinks;
var
    Lower, Href, Text: string;
    At, HrefStart, HrefEnd, TagEnd, TextEnd: longint;
    Quote: char;
    Link: THtmlLink;
begin
    Result := nil;
    Lower := LowerCase(APage);
    At := 1;
    repeat
        At := PosEx('<a ', Lower, At);
        if At = 0 then
            Break;
        HrefStart := PosEx('href=', Lower, At);
        TagEnd := PosEx('>', Lower, At);
        if (HrefStart = 0) or (TagEnd = 0) or (HrefStart > TagEnd) then
        begin
            At := TagEnd + 1;
            Continue;
        end;
        Inc(HrefStart, Length('href='));
        Quote := APage[HrefStart];
        if (Quote = '"') or (Quote = '''') then
        begin
            Inc(HrefStart);
            HrefEnd := PosEx(Quote, APage, HrefStart);
        end
        else
            HrefEnd := PosEx(' ', APage, HrefStart);
        if (HrefEnd = 0) or (HrefEnd > TagEnd + 1) then
            HrefEnd := TagEnd;
        Href := Unescaped(Copy(APage, HrefStart, HrefEnd - HrefStart));

        TextEnd := PosEx('</a>', Lower, TagEnd);
        if TextEnd = 0 then
            TextEnd := TagEnd + 1;
        Text := Trim(Unescaped(Copy(APage, TagEnd + 1, TextEnd - TagEnd - 1)));

        if (AContains = '') or (Pos(LowerCase(AContains), LowerCase(Href)) > 0) then
        begin
            Link.Href := Href;
            Link.Text := Text;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Link;
        end;
        At := TagEnd + 1;
    until At > Length(APage);
end;

function ParameterOf(const AUrl, AName: string): string;
var
    At, Stop: longint;
    Rest: string;
begin
    Result := '';
    At := Pos(LowerCase(AName) + '=', LowerCase(AUrl));
    if At = 0 then
        Exit;
    Rest := Copy(AUrl, At + Length(AName) + 1, MaxInt);
    Stop := 1;
    while (Stop <= Length(Rest)) and not (Rest[Stop] in ['&', '#', '"', ' ']) do
        Inc(Stop);
    Result := Copy(Rest, 1, Stop - 1);
end;

end.
