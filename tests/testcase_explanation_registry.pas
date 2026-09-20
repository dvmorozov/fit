// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the registry every surface finds an explanation through.

  WHY THESE EXIST. Explanations come from many places - curve types, a module's
  rules, a verdict computed for one menu entry - and a pane, a hint, the
  registry dump and the generated guide all have to show any of them without
  knowing where it came from. So a topic is routed by its namespace to the one
  provider that owns it, and the registry is also where a WRONG provider is
  caught: one listing a topic it cannot explain, answering a question it was not
  asked, or explaining badly. ExplanationFindings is what every module's
  completeness test calls; it is only as strict as the cases pinned here.

  Routing and findings are tested over arrays handed in, not the process-wide
  registry, so the deliberately broken providers below never leak into it. }
unit testcase_explanation_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation,
    explanation_registry, mock_explanation_provider;

type
    TExplanationRegistryTest = class(TTestCase)
    private
        FA, FB: TMockExplanationProvider;
        function Both: TExplanationProviders;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ATopicsNamespaceIsWhatPrecedesItsFirstSlash;
        procedure ATopicWithNoSlashHasNoNamespace;
        procedure TopicsRouteToTheProviderOwningTheirNamespace;
        procedure OnlyThatProviderIsAsked;
        procedure AComputedTopicIsFoundThoughNotListed;
        procedure AnUnknownNamespaceIsNotFoundRatherThanFatal;
        procedure ATopicWithoutANamespaceIsNotFound;
        procedure ATopicTheProviderCannotExplainIsNotFound;
        procedure AProviderThatAnswersADifferentTopicIsNotBelieved;
        procedure StaticTopicsAreListedProviderByProviderInOrder;
        procedure AWellFormedSetOfProvidersHasNoFindings;
        procedure AStaticTopicOutsideItsNamespaceIsAFinding;
        procedure AStaticTopicThatDoesNotResolveIsAFinding;
        procedure AnIncompleteStaticExplanationIsAFindingThatSaysWhatIsMissing;
        procedure ARelatedTopicThatResolvesNowhereIsAFinding;
        procedure ARelatedComputedTopicThatResolvesIsNotAFinding;
        procedure ATopicListedTwiceIsAFinding;
        procedure TwoProvidersClaimingOneNamespaceIsAFinding;
        procedure ANilProviderIsRefused;
        procedure AProviderWithoutANamespaceIsRefused;
        procedure ANamespaceContainingASlashIsRefused;
        procedure RegisteringANamespaceTwiceKeepsTheFirst;
        procedure ARegisteredTopicIsFoundThroughTheProcessWideRegistry;
        procedure ARegisteredTopicResolvesAndAnUnknownOneDoesNot;
        procedure ARegisteredTopicsTitleIsItsExplanationsTitle;
        procedure AnUnknownTopicHasNoTitle;
    end;

implementation

var
    //  Registered into the process-wide registry, which cannot remove them, so
    //  they live for the whole run and are freed only at finalization.
    GRegisteredFirst, GRegisteredSecond: TMockExplanationProvider;

function Explained(const ATopic: string;
    AStanding: TExplanationStanding = esConvention): TExplanation;
begin
    Result := NewExplanation(ATopic, 'Title of ' + ATopic,
        'Explains ' + ATopic + '.', AStanding);
    AddParagraph(Result, 'A paragraph about ' + ATopic + '.');
end;

function Joined(const AItems: TStringArray): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AItems) do
        Result := Result + AItems[i] + LineEnding;
end;

function AnyContains(const AItems: TStringArray; const AText: string): boolean;
var
    i: longint;
begin
    Result := False;
    for i := 0 to High(AItems) do
        if Pos(AText, AItems[i]) > 0 then
            Exit(True);
end;

procedure TExplanationRegistryTest.SetUp;
begin
    FA := TMockExplanationProvider.Create('alpha');
    FA.Add(Explained('alpha/one'));
    FA.Add(Explained('alpha/two'));
    FA.Add(Explained('alpha/computed/7'), False);
    FB := TMockExplanationProvider.Create('beta');
    FB.Add(Explained('beta/one'));
end;

procedure TExplanationRegistryTest.TearDown;
begin
    FA.Free;
    FB.Free;
end;

function TExplanationRegistryTest.Both: TExplanationProviders;
begin
    SetLength(Result, 2);
    Result[0] := FA;
    Result[1] := FB;
end;

procedure TExplanationRegistryTest.ATopicsNamespaceIsWhatPrecedesItsFirstSlash;
begin
    AssertEquals('pack', TopicNamespace('pack/rule/P4'));
end;

procedure TExplanationRegistryTest.ATopicWithNoSlashHasNoNamespace;
begin
    AssertEquals('', TopicNamespace('pack'));
    AssertEquals('', TopicNamespace('/leading-slash'));
end;

procedure TExplanationRegistryTest.TopicsRouteToTheProviderOwningTheirNamespace;
var
    E: TExplanation;
begin
    AssertTrue(FindExplanationIn(Both, 'beta/one', E));
    AssertEquals('beta/one', E.Topic);
end;

procedure TExplanationRegistryTest.OnlyThatProviderIsAsked;
var
    E: TExplanation;
begin
    FindExplanationIn(Both, 'beta/one', E);
    AssertEquals('the other namespace is not asked', 0, FA.ExplainCalls);
end;

procedure TExplanationRegistryTest.AComputedTopicIsFoundThoughNotListed;
var
    E: TExplanation;
begin
    AssertTrue(FindExplanationIn(Both, 'alpha/computed/7', E));
end;

procedure TExplanationRegistryTest.AnUnknownNamespaceIsNotFoundRatherThanFatal;
var
    E: TExplanation;
begin
    AssertFalse(FindExplanationIn(Both, 'gamma/one', E));
    AssertEquals('', E.Topic);
end;

procedure TExplanationRegistryTest.ATopicWithoutANamespaceIsNotFound;
var
    E: TExplanation;
begin
    AssertFalse(FindExplanationIn(Both, 'alpha', E));
end;

procedure TExplanationRegistryTest.ATopicTheProviderCannotExplainIsNotFound;
var
    E: TExplanation;
begin
    AssertFalse(FindExplanationIn(Both, 'alpha/unknown', E));
end;

procedure TExplanationRegistryTest.AProviderThatAnswersADifferentTopicIsNotBelieved;
var
    E: TExplanation;
begin
    //  Showing the explanation of one thing under another's name is worse than
    //  showing nothing: it is confidently wrong.
    FA.AnswerAs('alpha/asked', 'alpha/one');
    AssertFalse(FindExplanationIn(Both, 'alpha/asked', E));
    AssertEquals('', E.Topic);
end;

procedure TExplanationRegistryTest.StaticTopicsAreListedProviderByProviderInOrder;
var
    Topics: TStringArray;
begin
    Topics := StaticTopicsOf(Both);
    AssertEquals(3, Length(Topics));
    AssertEquals('alpha/one', Topics[0]);
    AssertEquals('alpha/two', Topics[1]);
    AssertEquals('beta/one', Topics[2]);
end;

procedure TExplanationRegistryTest.AWellFormedSetOfProvidersHasNoFindings;
begin
    AssertEquals('', Joined(ExplanationFindings(Both)));
end;

procedure TExplanationRegistryTest.AStaticTopicOutsideItsNamespaceIsAFinding;
var
    Findings: TStringArray;
begin
    FA.Add(Explained('beta/stray'));
    Findings := ExplanationFindings(Both);
    AssertTrue(Joined(Findings), AnyContains(Findings, 'beta/stray'));
    AssertTrue(Joined(Findings), AnyContains(Findings, 'namespace'));
end;

procedure TExplanationRegistryTest.AStaticTopicThatDoesNotResolveIsAFinding;
var
    Findings: TStringArray;
begin
    FB.ListWithoutExplaining('beta/listed-only');
    Findings := ExplanationFindings(Both);
    AssertTrue(Joined(Findings), AnyContains(Findings, 'beta/listed-only'));
end;

procedure TExplanationRegistryTest.AnIncompleteStaticExplanationIsAFindingThatSaysWhatIsMissing;
var
    E: TExplanation;
    Findings: TStringArray;
begin
    E := Explained('beta/thin', esCanonical);
    FB.Add(E);
    Findings := ExplanationFindings(Both);
    AssertTrue(Joined(Findings), AnyContains(Findings, 'beta/thin'));
    AssertTrue(Joined(Findings), AnyContains(Findings, 'quote'));
end;

procedure TExplanationRegistryTest.ARelatedTopicThatResolvesNowhereIsAFinding;
var
    E: TExplanation;
    Findings: TStringArray;
begin
    E := Explained('beta/links');
    AddRelated(E, 'alpha/missing');
    FB.Add(E);
    Findings := ExplanationFindings(Both);
    AssertTrue(Joined(Findings), AnyContains(Findings, 'alpha/missing'));
end;

procedure TExplanationRegistryTest.ARelatedComputedTopicThatResolvesIsNotAFinding;
var
    E: TExplanation;
begin
    E := Explained('beta/links');
    AddRelated(E, 'alpha/computed/7');
    FB.Add(E);
    AssertEquals('', Joined(ExplanationFindings(Both)));
end;

procedure TExplanationRegistryTest.ATopicListedTwiceIsAFinding;
var
    Findings: TStringArray;
begin
    FB.ListWithoutExplaining('beta/one');
    Findings := ExplanationFindings(Both);
    AssertTrue(Joined(Findings), AnyContains(Findings, 'more than once'));
end;

procedure TExplanationRegistryTest.TwoProvidersClaimingOneNamespaceIsAFinding;
var
    Twin: TMockExplanationProvider;
    Providers: TExplanationProviders;
    Findings: TStringArray;
begin
    Twin := TMockExplanationProvider.Create('alpha');
    try
        Providers := Both;
        SetLength(Providers, 3);
        Providers[2] := Twin;
        Findings := ExplanationFindings(Providers);
        AssertTrue(Joined(Findings), AnyContains(Findings, 'alpha'));
        AssertTrue(Joined(Findings), AnyContains(Findings, 'claimed'));
    finally
        Providers := nil;
        Twin.Free;
    end;
end;

procedure TExplanationRegistryTest.ANilProviderIsRefused;
var
    Raised: boolean;
begin
    Raised := False;
    try
        RegisterExplanationProvider(nil);
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue(Raised);
end;

procedure TExplanationRegistryTest.AProviderWithoutANamespaceIsRefused;
var
    Nameless: TMockExplanationProvider;
    Raised: boolean;
begin
    Nameless := TMockExplanationProvider.Create('');
    Raised := False;
    try
        try
            RegisterExplanationProvider(Nameless);
        except
            on Exception do
                Raised := True;
        end;
        AssertTrue(Raised);
    finally
        Nameless.Free;
    end;
end;

procedure TExplanationRegistryTest.ANamespaceContainingASlashIsRefused;
var
    Slashed: TMockExplanationProvider;
    Raised: boolean;
begin
    Slashed := TMockExplanationProvider.Create('a/b');
    Raised := False;
    try
        try
            RegisterExplanationProvider(Slashed);
        except
            on Exception do
                Raised := True;
        end;
        AssertTrue(Raised);
    finally
        Slashed.Free;
    end;
end;

procedure TExplanationRegistryTest.RegisteringANamespaceTwiceKeepsTheFirst;
var
    E: TExplanation;
    i, Count: longint;
    Providers: TExplanationProviders;
begin
    RegisterExplanationProvider(GRegisteredFirst);
    RegisterExplanationProvider(GRegisteredSecond);
    Providers := RegisteredExplanationProviders;
    Count := 0;
    for i := 0 to High(Providers) do
        if Providers[i].Namespace = 'test-registry-twice' then
            Inc(Count);
    AssertEquals('one provider per namespace', 1, Count);
    AssertTrue(FindExplanation('test-registry-twice/first', E));
    AssertFalse('the second was not taken', FindExplanation(
        'test-registry-twice/second', E));
end;

procedure TExplanationRegistryTest.ARegisteredTopicIsFoundThroughTheProcessWideRegistry;
var
    E: TExplanation;
    Topics: TStringArray;
    i: longint;
    Listed: boolean;
begin
    RegisterExplanationProvider(GRegisteredFirst);
    AssertTrue(FindExplanation('test-registry-twice/first', E));
    Topics := AllStaticTopics;
    Listed := False;
    for i := 0 to High(Topics) do
        if Topics[i] = 'test-registry-twice/first' then
            Listed := True;
    AssertTrue('listed among all static topics', Listed);
end;

procedure TExplanationRegistryTest.ARegisteredTopicResolvesAndAnUnknownOneDoesNot;
begin
    RegisterExplanationProvider(GRegisteredFirst);
    AssertTrue(RegisteredTopicResolves('test-registry-twice/first'));
    AssertFalse(RegisteredTopicResolves('test-registry-twice/nothing'));
    AssertFalse(RegisteredTopicResolves(''));
end;

procedure TExplanationRegistryTest.ARegisteredTopicsTitleIsItsExplanationsTitle;
begin
    RegisterExplanationProvider(GRegisteredFirst);
    AssertEquals('Title of test-registry-twice/first',
        RegisteredTopicTitle('test-registry-twice/first'));
end;

procedure TExplanationRegistryTest.AnUnknownTopicHasNoTitle;
begin
    AssertEquals('', RegisteredTopicTitle('nowhere/at-all'));
end;

initialization
    GRegisteredFirst := TMockExplanationProvider.Create('test-registry-twice');
    GRegisteredFirst.Add(Explained('test-registry-twice/first'));
    GRegisteredSecond := TMockExplanationProvider.Create('test-registry-twice');
    GRegisteredSecond.Add(Explained('test-registry-twice/second'));
    RegisterTest('unit', TExplanationRegistryTest);

finalization
    GRegisteredSecond.Free;
    GRegisteredFirst.Free;
end.
