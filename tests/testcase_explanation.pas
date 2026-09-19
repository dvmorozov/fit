// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for what makes an explanation complete, and for the words a standing is
  shown with.

  WHY THESE EXIST. The app is meant to teach as well as to fit, so whatever a
  user meets - a curve type, a rule, a greyed menu entry - explains itself. An
  explanation is only worth showing if it actually says something: a summary
  that repeats the title, or a rule called canonical that quotes nobody, LOOKS
  like teaching and teaches nothing. ExplanationIsComplete is the one place that
  judgement is made, and every registry-walking completeness test leans on it,
  so its rules are pinned here one at a time - each test breaks exactly one. }
unit testcase_explanation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation;

type
    TExplanationTest = class(TTestCase)
    published
        procedure ACompleteExplanationIsComplete;
        procedure ACompleteExplanationNamesNothingMissing;
        procedure AnExplanationWithoutATopicIsIncomplete;
        procedure AnExplanationWithoutATitleIsIncomplete;
        procedure AnExplanationWithoutASummaryIsIncomplete;
        procedure ASummaryMustEndAsASentence;
        procedure ASummaryThatRepeatsTheTitleIsIncomplete;
        procedure AnExplanationWithoutABodyIsIncomplete;
        procedure ABlankParagraphIsNotABody;
        procedure ACanonicalExplanationWithoutAQuoteIsIncomplete;
        procedure ACanonicalExplanationWithoutAReferenceIsIncomplete;
        procedure AConventionNeedsNeitherQuoteNorReference;
        procedure AReferenceWithoutAWorkIsIncomplete;
        procedure EveryMissingPartIsNamedNotJustTheFirst;
        procedure AnUnstatedStandingIsUnsettledNotCanonical;
        procedure EveryStandingHasItsOwnCaption;
        procedure EveryStandingHintIsASentenceThatSaysMoreThanItsCaption;
        procedure TheBuildersKeepWhatTheyAreGiven;
        procedure TheBuildersAppendInTheOrderTheyAreCalled;
    end;

implementation

function Complete(AStanding: TExplanationStanding = esCanonical): TExplanation;
begin
    Result := NewExplanation('test/gaussian', 'Gaussian',
        'A symmetric, bell-shaped peak.', AStanding);
    AddParagraph(Result, 'Its width is set by sigma.');
    Result.Quote := 'exp(-(x - x0)^2 / (2 sigma^2))';
    AddReference(Result, 'A textbook of line profiles', 'ch. 1', '');
end;

function MissingOf(const AExplanation: TExplanation): string;
begin
    ExplanationIsComplete(AExplanation, Result);
end;

function IsComplete(const AExplanation: TExplanation): boolean;
var
    Missing: string;
begin
    Result := ExplanationIsComplete(AExplanation, Missing);
end;

procedure TExplanationTest.ACompleteExplanationIsComplete;
begin
    AssertTrue(IsComplete(Complete));
end;

procedure TExplanationTest.ACompleteExplanationNamesNothingMissing;
begin
    AssertEquals('', MissingOf(Complete));
end;

procedure TExplanationTest.AnExplanationWithoutATopicIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.Topic := '';
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('topic', MissingOf(E)) > 0);
end;

procedure TExplanationTest.AnExplanationWithoutATitleIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.Title := '   ';
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('title', MissingOf(E)) > 0);
end;

procedure TExplanationTest.AnExplanationWithoutASummaryIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.Summary := '';
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('no summary', MissingOf(E)) > 0);
end;

procedure TExplanationTest.ASummaryMustEndAsASentence;
var
    E: TExplanation;
begin
    E := Complete;
    E.Summary := 'A symmetric, bell-shaped peak';
    AssertFalse('no full stop', IsComplete(E));
    AssertTrue(MissingOf(E), Pos('sentence', MissingOf(E)) > 0);

    E.Summary := 'Is this a peak?';
    AssertTrue('a question is a sentence', IsComplete(E));
    E.Summary := 'A peak.  ';
    AssertTrue('trailing blanks do not count against it', IsComplete(E));
end;

procedure TExplanationTest.ASummaryThatRepeatsTheTitleIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.Summary := 'gaussian.';
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('repeats the title', MissingOf(E)) > 0);
end;

procedure TExplanationTest.AnExplanationWithoutABodyIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.Body := nil;
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('body', MissingOf(E)) > 0);
end;

procedure TExplanationTest.ABlankParagraphIsNotABody;
var
    E: TExplanation;
begin
    E := Complete;
    E.Body := nil;
    AddParagraph(E, '  ');
    AssertFalse(IsComplete(E));
end;

procedure TExplanationTest.ACanonicalExplanationWithoutAQuoteIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.Quote := '';
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('quote', MissingOf(E)) > 0);
end;

procedure TExplanationTest.ACanonicalExplanationWithoutAReferenceIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete;
    E.References := nil;
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('reference', MissingOf(E)) > 0);
end;

procedure TExplanationTest.AConventionNeedsNeitherQuoteNorReference;
var
    S: TExplanationStanding;
    E: TExplanation;
begin
    for S := Low(TExplanationStanding) to High(TExplanationStanding) do
    begin
        if S = esCanonical then
            Continue;
        E := Complete(S);
        E.Quote := '';
        E.References := nil;
        AssertTrue(StandingCaption(S) + ': ' + MissingOf(E), IsComplete(E));
    end;
end;

procedure TExplanationTest.AReferenceWithoutAWorkIsIncomplete;
var
    E: TExplanation;
begin
    E := Complete(esConvention);
    AddReference(E, '', 'p. 12', 'https://example.org');
    AssertFalse(IsComplete(E));
    AssertTrue(MissingOf(E), Pos('work', MissingOf(E)) > 0);
end;

procedure TExplanationTest.EveryMissingPartIsNamedNotJustTheFirst;
var
    E: TExplanation;
    Missing: string;
begin
    E := Default(TExplanation);
    E.Standing := esCanonical;
    Missing := MissingOf(E);
    AssertTrue(Missing, Pos('topic', Missing) > 0);
    AssertTrue(Missing, Pos('title', Missing) > 0);
    AssertTrue(Missing, Pos('summary', Missing) > 0);
    AssertTrue(Missing, Pos('body', Missing) > 0);
    AssertTrue(Missing, Pos('quote', Missing) > 0);
    AssertTrue(Missing, Pos('reference', Missing) > 0);
end;

procedure TExplanationTest.AnUnstatedStandingIsUnsettledNotCanonical;
begin
    //  Whatever zero means is what unconfigured code silently gets, and
    //  claiming canonical authority by omission is the worst available answer.
    AssertTrue(Default(TExplanation).Standing = esUnsettled);
end;

procedure TExplanationTest.EveryStandingHasItsOwnCaption;
var
    S, T: TExplanationStanding;
begin
    for S := Low(TExplanationStanding) to High(TExplanationStanding) do
    begin
        AssertTrue('a caption for every standing', Trim(StandingCaption(S)) <> '');
        for T := Succ(S) to High(TExplanationStanding) do
            if S <> T then
                AssertFalse('two standings read alike: ' + StandingCaption(S),
                    StandingCaption(S) = StandingCaption(T));
    end;
end;

procedure TExplanationTest.EveryStandingHintIsASentenceThatSaysMoreThanItsCaption;
var
    S: TExplanationStanding;
    Hint: string;
begin
    for S := Low(TExplanationStanding) to High(TExplanationStanding) do
    begin
        Hint := Trim(StandingHint(S));
        AssertTrue(StandingCaption(S) + ' has a hint', Length(Hint) > Length(StandingCaption(S)));
        AssertTrue(Hint + ' ends as a sentence', Hint[Length(Hint)] = '.');
    end;
end;

procedure TExplanationTest.TheBuildersKeepWhatTheyAreGiven;
var
    E: TExplanation;
begin
    E := NewExplanation('a/b', 'Title', 'Summary.', esModelChoice);
    AssertEquals('a/b', E.Topic);
    AssertEquals('Title', E.Title);
    AssertEquals('Summary.', E.Summary);
    AssertTrue(E.Standing = esModelChoice);
    AddReference(E, 'Work', 'p. 3', 'https://example.org/w');
    AssertEquals('Work', E.References[0].Work);
    AssertEquals('p. 3', E.References[0].Locator);
    AssertEquals('https://example.org/w', E.References[0].Url);
end;

procedure TExplanationTest.TheBuildersAppendInTheOrderTheyAreCalled;
var
    E: TExplanation;
begin
    E := NewExplanation('a/b', 'T', 'S.', esConvention);
    AddParagraph(E, 'first');
    AddParagraph(E, 'second');
    AddLimitation(E, 'one');
    AddLimitation(E, 'two');
    AddRelated(E, 'a/c');
    AddRelated(E, 'a/d');
    AssertEquals(2, Length(E.Body));
    AssertEquals('first', E.Body[0]);
    AssertEquals('second', E.Body[1]);
    AssertEquals('two', E.Limitations[1]);
    AssertEquals('a/d', E.Related[1]);
end;

initialization
    RegisterTest('unit', TExplanationTest);
end.
