// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for what the progress view and Animation Mode say about themselves.

  WHY THESE EXIST. Everything a user meets explains itself, and a chart that
  suddenly shows a different chart while a fit runs is something a user meets
  without being asked. What it says must name what the user actually sees - the
  menu entry, the Stop command, the logarithmic scale - or it describes a screen
  that is not there. }
unit testcase_fit_progress_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation, explanation_registry,
    fit_progress_explanations;

type
    TFitProgressExplanationsTest = class(TTestCase)
    private
        function TextOf(const ATopic: string): string;
    published
        procedure BothTopicsAreListedUnderTheirOwnNamespace;
        procedure TheProgressViewNamesWhatItDraws;
        procedure AnimationNamesItsMenuEntryAndItsCost;
        procedure BothAreThisSoftwaresChoice;
        procedure NothingElseIsExplained;
        procedure TheyAreWellFormed;
        procedure RegisteredTheyResolveThroughTheRegistry;
    end;

implementation

function TFitProgressExplanationsTest.TextOf(const ATopic: string): string;
var
    E: TExplanation;
    i: integer;
begin
    AssertTrue(ATopic + ' resolves',
        FitProgressExplanationProvider.Explain(ATopic, E));
    Result := E.Summary;
    for i := 0 to High(E.Body) do
        Result := Result + ' ' + E.Body[i];
end;

procedure TFitProgressExplanationsTest.BothTopicsAreListedUnderTheirOwnNamespace;
var
    Topics: TStringArray;
    i: integer;
begin
    Topics := FitProgressExplanationProvider.StaticTopics;
    AssertEquals(2, Length(Topics));
    for i := 0 to High(Topics) do
        AssertEquals(FitProgressExplanationProvider.Namespace,
            TopicNamespace(Topics[i]));
end;

procedure TFitProgressExplanationsTest.TheProgressViewNamesWhatItDraws;
var
    Text: string;
begin
    Text := TextOf(LiveFitProgressTopic);
    AssertTrue('the R-factor: ' + Text, Pos('R-factor', Text) > 0);
    AssertTrue('on a logarithmic scale', Pos('logarithmic', Text) > 0);
    AssertTrue('how to stop', Pos('Stop', Text) > 0);
    AssertTrue('and the engine that reports nothing',
        Pos('no intermediate progress', Text) > 0);
end;

procedure TFitProgressExplanationsTest.AnimationNamesItsMenuEntryAndItsCost;
var
    Text: string;
begin
    Text := TextOf(AnimationModeTopic);
    AssertTrue('the menu entry: ' + Text, Pos('Animation Mode', Text) > 0);
    AssertTrue('where it is', Pos('View', Text) > 0);
    AssertTrue('what it costs', Pos('slower', Text) > 0);
end;

procedure TFitProgressExplanationsTest.BothAreThisSoftwaresChoice;
var
    E: TExplanation;
begin
    FitProgressExplanationProvider.Explain(LiveFitProgressTopic, E);
    AssertTrue('the view', E.Standing = esModelChoice);
    FitProgressExplanationProvider.Explain(AnimationModeTopic, E);
    AssertTrue('the animation', E.Standing = esModelChoice);
end;

procedure TFitProgressExplanationsTest.NothingElseIsExplained;
var
    E: TExplanation;
begin
    AssertFalse(FitProgressExplanationProvider.Explain('fit-progress/other', E));
    AssertFalse(FitProgressExplanationProvider.Explain('report/reading-a-report', E));
end;

procedure TFitProgressExplanationsTest.TheyAreWellFormed;
var
    Providers: TExplanationProviders;
    Findings: TStringArray;
    i: integer;
begin
    Providers := nil;
    SetLength(Providers, 1);
    Providers[0] := FitProgressExplanationProvider;
    Findings := ExplanationFindings(Providers);
    for i := 0 to High(Findings) do
        Fail(Findings[i]);
end;

procedure TFitProgressExplanationsTest.RegisteredTheyResolveThroughTheRegistry;
var
    E: TExplanation;
begin
    RegisterFitProgressExplanations;
    RegisterFitProgressExplanations;
    AssertTrue(FindExplanation(LiveFitProgressTopic, E));
    AssertTrue(FindExplanation(AnimationModeTopic, E));
end;

initialization
    RegisterTest('unit', TFitProgressExplanationsTest);
end.
