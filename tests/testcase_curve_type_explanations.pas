// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests that every registered curve type explains itself.

  WHY THESE EXIST. A curve type is the first thing a user chooses, and until now
  each one said only its name. The application is meant to teach: what a
  Pearson VII is, what its m controls, where it is used and where it is not.
  These tests make that a property of the REGISTRY rather than of the types
  someone remembered: they walk whatever is registered in this binary, so the
  next curve type - the framework's or a module's - that arrives without an
  explanation fails here by name.

  They enter where the application enters. RegisterAllCurveTypes is the call
  the client, the server and this runner all make at start-up; the provider is
  registered there, and a topic is looked up through FindExplanation, the same
  function the Explain pane calls. }
unit testcase_curve_type_explanations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation,
    explanation_registry, named_points_set, curve_types_singleton,
    int_curve_type_iterator, curve_type_registration,
    curve_type_explanations, gauss_points_set, voigt_points_set,
    user_points_set;

type
    TCurveTypeExplanationsTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        procedure ATopicIsNamespacedAndNamedByTheTypesId;
        procedure RegisteringTheCurveTypesMakesThemExplainable;
        procedure TheProviderListsEveryRegisteredCurveType;
        procedure EveryRegisteredCurveTypeExplainsItselfCompletely;
        procedure TheProviderStampsTheTopicAndTheTypesName;
        procedure ATypeThatDoesNotOverrideIsNotExplained;
        procedure ATopicNamingNoRegisteredTypeIsNotFound;
        procedure AMalformedTopicIsNotFoundRatherThanFatal;
        procedure EveryCurveTypeStatesAtLeastOneLimitation;
        procedure ARegisteredTypesNameLeadsToItsTopic;
        procedure ANameNoTypeCarriesLeadsNowhere;
        procedure ATopicOfAnotherNamespaceIsNotItsToExplain;
        procedure AnEmptyNameLeadsNowhere;
    end;

implementation

function Joined(const AItems: TStringArray): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AItems) do
        Result := Result + AItems[i] + LineEnding;
end;

function RegisteredClassCount: longint;
var
    It: ICurveTypeIterator;
begin
    Result := 0;
    It := TCurveTypesSingleton.CreateCurveTypeIterator;
    It.FirstCurveType;
    repeat
        Inc(Result);
        if It.EndCurveType then
            Break;
        It.NextCurveType;
    until False;
end;

procedure TCurveTypeExplanationsTest.SetUp;
begin
    RegisterAllCurveTypes;
end;

procedure TCurveTypeExplanationsTest.ATopicIsNamespacedAndNamedByTheTypesId;
var
    Id: string;
begin
    Id := LowerCase(GUIDToString(TGaussPointsSet.GetCurveTypeId));
    Id := Copy(Id, 2, Length(Id) - 2);
    AssertEquals('curve-type/' + Id, CurveTypeTopic(TGaussPointsSet));
    AssertEquals(CurveTypeNamespace, TopicNamespace(
        CurveTypeTopic(TGaussPointsSet)));
end;

procedure TCurveTypeExplanationsTest.RegisteringTheCurveTypesMakesThemExplainable;
var
    E: TExplanation;
begin
    //  Through the process-wide registry: the path the pane takes.
    AssertTrue(FindExplanation(CurveTypeTopic(TGaussPointsSet), E));
    AssertTrue(FindExplanation(CurveTypeTopic(TUserPointsSet), E));
end;

procedure TCurveTypeExplanationsTest.TheProviderListsEveryRegisteredCurveType;
var
    Topics: TStringArray;
    Listed: TStringList;
    i: longint;
begin
    Topics := CurveTypeExplanationProvider.StaticTopics;
    AssertEquals('one topic per registered type', RegisteredClassCount,
        Length(Topics));
    Listed := TStringList.Create;
    try
        for i := 0 to High(Topics) do
            Listed.Add(Topics[i]);
        AssertTrue(Listed.IndexOf(CurveTypeTopic(TGaussPointsSet)) >= 0);
        AssertTrue(Listed.IndexOf(CurveTypeTopic(TVoigtPointsSet)) >= 0);
    finally
        Listed.Free;
    end;
end;

procedure TCurveTypeExplanationsTest.EveryRegisteredCurveTypeExplainsItselfCompletely;
var
    Providers: TExplanationProviders;
begin
    SetLength(Providers, 1);
    Providers[0] := CurveTypeExplanationProvider;
    AssertEquals('', Joined(ExplanationFindings(Providers)));
end;

procedure TCurveTypeExplanationsTest.TheProviderStampsTheTopicAndTheTypesName;
var
    E: TExplanation;
begin
    AssertTrue(CurveTypeExplanationProvider.Explain(
        CurveTypeTopic(TVoigtPointsSet), E));
    AssertEquals(CurveTypeTopic(TVoigtPointsSet), E.Topic);
    AssertEquals(TVoigtPointsSet.GetCurveTypeName, E.Title);
end;

procedure TCurveTypeExplanationsTest.ATypeThatDoesNotOverrideIsNotExplained;
var
    Missing: string;
begin
    AssertFalse(ExplanationIsComplete(TNamedPointsSet.Explanation, Missing));
end;

procedure TCurveTypeExplanationsTest.ATopicNamingNoRegisteredTypeIsNotFound;
var
    E: TExplanation;
begin
    AssertFalse(CurveTypeExplanationProvider.Explain(
        'curve-type/00000000-0000-0000-0000-000000000000', E));
end;

procedure TCurveTypeExplanationsTest.AMalformedTopicIsNotFoundRatherThanFatal;
var
    E: TExplanation;
begin
    AssertFalse(CurveTypeExplanationProvider.Explain('curve-type/not-a-guid', E));
    AssertFalse(CurveTypeExplanationProvider.Explain('curve-type/', E));
end;

procedure TCurveTypeExplanationsTest.EveryCurveTypeStatesAtLeastOneLimitation;
var
    Topics: TStringArray;
    E: TExplanation;
    i: longint;
begin
    //  An educational explanation that lists no limitation reads as a claim
    //  that the shape fits everything - the one thing no line shape does.
    Topics := CurveTypeExplanationProvider.StaticTopics;
    AssertTrue('there are types to check', Length(Topics) > 0);
    for i := 0 to High(Topics) do
    begin
        AssertTrue(Topics[i], CurveTypeExplanationProvider.Explain(Topics[i], E));
        AssertTrue(E.Title + ' states a limitation', Length(E.Limitations) > 0);
    end;
end;


procedure TCurveTypeExplanationsTest.ARegisteredTypesNameLeadsToItsTopic;
begin
    AssertEquals(CurveTypeTopic(TVoigtPointsSet),
        CurveTypeTopicForName(TVoigtPointsSet.GetCurveTypeName));
end;

procedure TCurveTypeExplanationsTest.ANameNoTypeCarriesLeadsNowhere;
begin
    AssertEquals('', CurveTypeTopicForName('Not a curve type'));
end;

procedure TCurveTypeExplanationsTest.AnEmptyNameLeadsNowhere;
begin
    AssertEquals('', CurveTypeTopicForName(''));
end;


procedure TCurveTypeExplanationsTest.ATopicOfAnotherNamespaceIsNotItsToExplain;
var
    E: TExplanation;
begin
    //  Asked directly, not through the registry that would never route it
    //  here: a provider answers only for its own namespace, so a surface that
    //  asks the wrong one gets "nothing to explain", not somebody else's text.
    AssertFalse(CurveTypeExplanationProvider.Explain('pack/rule/P4', E));
    AssertEquals('', E.Topic);
end;

initialization
    RegisterTest('unit', TCurveTypeExplanationsTest);
end.
