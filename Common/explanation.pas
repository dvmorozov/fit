// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What an explanation is, and when one is worth showing.)

WHY THIS EXISTS. This application is meant to teach as well as to fit: whatever
a user meets - a curve type, a rule, a greyed menu entry, a refusal - should say
what it is, why it is allowed or not, whether that rests on the field's own
authorities or on a choice this software made, what it does not cover, and
where to read more. Before this unit the app explained itself through loose
strings: a hint here, a status line there, TFitAdvice's Summary and Detail for
one decision. Those strings could not say HOW MUCH authority stood behind them,
and nothing could check that they said anything at all.

WHAT IT GENERALISES. TFitAdvice already had the right shape for one decision: a
line for where space is short, and the full reasoning for where it is not. This
record is that shape for every explainable thing, plus the parts an educational
explanation needs and advice never did: a standing, a verbatim quote, references,
limitations and related topics.

THE BODY IS PLAIN TEXT, NOT HTML. An explanation crosses from a module to
whichever host draws it - a pane, a hint, a generated page - and a module must
not name a widget or a markup its host may not render (int_ui_host). Paragraphs
are array entries; a line beginning with a bullet character is a bullet. The
host decides how either looks.

AN UNSTATED STANDING IS UNSETTLED. esUnsettled is the enum's first value on
purpose: whatever zero means is what unconfigured code silently gets, and a
record that claims canonical authority by omission is the worst available
answer.

ExplanationIsComplete is the one judgement of whether an explanation is worth
showing. Every registry-walking completeness test - the framework's curve types,
a module's rules - leans on it, so a module cannot satisfy the letter of "explain
yourself" with a summary that repeats its title.
}
unit explanation;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    { How much authority stands behind what an explanation says. }
    TExplanationStanding = (
        //  The sources disagree or are silent. Also what an unstated standing
        //  is - see the header.
        esUnsettled,
        //  Stated by the field's authoritative sources, which are quoted.
        esCanonical,
        //  Common practice or a guideline in the field, not a requirement.
        esConvention,
        //  A decision this software made - a bound, a simplification.
        esModelChoice
        );

    TExplanationReference = record
        { The work cited: author and title, as a reader would look it up. }
        Work: string;
        { Where in it: a chapter, a page range, a section. May be empty. }
        Locator: string;
        { Where to read it, when it can be read online. May be empty. }
        Url: string;
    end;

    TExplanationReferences = array of TExplanationReference;

    TExplanation = record
        { Stable and namespaced - 'curve-type/<id>', 'pack/rule/P4'. What a
          surface, a link or a test addresses the explanation by. }
        Topic: string;
        Title: string;
        { One sentence, for where space is short: a hint, a status line, the
          caption of a greyed menu entry. }
        Summary: string;
        { Paragraphs, in order. }
        Body: TStringArray;
        Standing: TExplanationStanding;
        { What a canonical statement rests on, verbatim: the source's own words
          for a rule, or the defining formula for a mathematical form. }
        Quote: string;
        { What this does not do or cover. }
        Limitations: TStringArray;
        References: TExplanationReferences;
        { Topics worth reading next. }
        Related: TStringArray;
    end;

    TExplanations = array of TExplanation;

function NewExplanation(const ATopic, ATitle, ASummary: string;
    AStanding: TExplanationStanding): TExplanation;
procedure AddParagraph(var AExplanation: TExplanation; const AText: string);
procedure AddLimitation(var AExplanation: TExplanation; const AText: string);
procedure AddReference(var AExplanation: TExplanation;
    const AWork, ALocator, AUrl: string);
procedure AddRelated(var AExplanation: TExplanation; const ATopic: string);

{ Whether the explanation is worth showing. AMissing names EVERY shortfall,
  separated by '; ', and is empty when there is none - a test that only learns
  the first fixes one thing per run. }
function ExplanationIsComplete(const AExplanation: TExplanation;
    out AMissing: string): boolean;

{ How a standing is labelled, and what the label means. }
function StandingCaption(AStanding: TExplanationStanding): string;
function StandingHint(AStanding: TExplanationStanding): string;

implementation

procedure Append(var AItems: TStringArray; const AText: string);
begin
    SetLength(AItems, Length(AItems) + 1);
    AItems[High(AItems)] := AText;
end;

function NewExplanation(const ATopic, ATitle, ASummary: string;
    AStanding: TExplanationStanding): TExplanation;
begin
    Result := Default(TExplanation);
    Result.Topic := ATopic;
    Result.Title := ATitle;
    Result.Summary := ASummary;
    Result.Standing := AStanding;
end;

procedure AddParagraph(var AExplanation: TExplanation; const AText: string);
begin
    Append(AExplanation.Body, AText);
end;

procedure AddLimitation(var AExplanation: TExplanation; const AText: string);
begin
    Append(AExplanation.Limitations, AText);
end;

procedure AddReference(var AExplanation: TExplanation;
    const AWork, ALocator, AUrl: string);
var
    n: longint;
begin
    n := Length(AExplanation.References);
    SetLength(AExplanation.References, n + 1);
    AExplanation.References[n].Work := AWork;
    AExplanation.References[n].Locator := ALocator;
    AExplanation.References[n].Url := AUrl;
end;

procedure AddRelated(var AExplanation: TExplanation; const ATopic: string);
begin
    Append(AExplanation.Related, ATopic);
end;

const
    SentenceEnds = ['.', '!', '?'];

function EndsAsSentence(const AText: string): boolean;
var
    T: string;
begin
    T := TrimRight(AText);
    Result := (T <> '') and (T[Length(T)] in SentenceEnds);
end;

function WithoutSentenceEnd(const AText: string): string;
begin
    Result := TrimRight(AText);
    while (Result <> '') and (Result[Length(Result)] in SentenceEnds) do
        SetLength(Result, Length(Result) - 1);
    Result := Trim(Result);
end;

function ExplanationIsComplete(const AExplanation: TExplanation;
    out AMissing: string): boolean;
var
    i: longint;
    HasBody: boolean;

    procedure Missing(const AWhat: string);
    begin
        if AMissing <> '' then
            AMissing := AMissing + '; ';
        AMissing := AMissing + AWhat;
    end;

begin
    AMissing := '';

    if Trim(AExplanation.Topic) = '' then
        Missing('no topic');
    if Trim(AExplanation.Title) = '' then
        Missing('no title');

    if Trim(AExplanation.Summary) = '' then
        Missing('no summary')
    else
    begin
        if not EndsAsSentence(AExplanation.Summary) then
            Missing('the summary does not end as a sentence');
        //  A summary that only restates the title tells a reader nothing the
        //  title had not - which is what an explanation written to pass a
        //  check, rather than to explain, most often looks like.
        if (Trim(AExplanation.Title) <> '') and SameText(
            WithoutSentenceEnd(AExplanation.Summary),
            Trim(AExplanation.Title)) then
            Missing('the summary repeats the title');
    end;

    HasBody := False;
    for i := 0 to High(AExplanation.Body) do
        if Trim(AExplanation.Body[i]) <> '' then
            HasBody := True;
    if not HasBody then
        Missing('no body');

    //  CANONICAL IS A CLAIM, and a claim of authority names its authority. The
    //  other standings are honest without one: a convention may be folklore,
    //  and this software's own choice has no source but this software.
    if AExplanation.Standing = esCanonical then
    begin
        if Trim(AExplanation.Quote) = '' then
            Missing('a canonical explanation must quote its source');
        if Length(AExplanation.References) = 0 then
            Missing('a canonical explanation must name a reference');
    end;

    for i := 0 to High(AExplanation.References) do
        if Trim(AExplanation.References[i].Work) = '' then
            Missing(Format('reference %d names no work', [i + 1]));

    Result := AMissing = '';
end;

function StandingCaption(AStanding: TExplanationStanding): string;
begin
    case AStanding of
        esCanonical:   Result := 'Canonical';
        esConvention:  Result := 'Convention';
        esModelChoice: Result := 'This software''s choice';
        else           Result := 'Not settled by the sources';
    end;
end;

function StandingHint(AStanding: TExplanationStanding): string;
begin
    case AStanding of
        esCanonical:
            Result := 'Stated by the field''s authoritative sources, which ' +
                'the explanation quotes and cites.';
        esConvention:
            Result := 'Common practice or a guideline in the field, not a ' +
                'rule its sources require.';
        esModelChoice:
            Result := 'A decision made by this software rather than by the ' +
                'field''s sources; the explanation says why it was made.';
        else
            Result := 'The sources disagree about this or do not address ' +
                'it, so nothing here treats it as a rule.';
    end;
end;

end.
