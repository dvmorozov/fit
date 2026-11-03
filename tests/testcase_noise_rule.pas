// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Every curve type names the rule its field's noise is measured by -
by construction - and every rule explains itself.)

WHY. Whether a difference is noise depends on the noise, and the noise is the
field's: a count's variance grows with the count, a price's with the square of
the price. Judged by one field's rule whatever the type said, a century of the
S&P 500 had its quiet years named as structure (findings.md, "The Difference
structure strip judged a price as counts"). So a curve type that names no rule
can be neither registered nor made, and these tests walk the registry, so the
next type - the framework's tests' or a module's - that arrives without one, or
with one that does not explain itself, fails here by name.
}
unit testcase_noise_rule;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation,
    explanation_registry, named_points_set, noise_rule, fit_reduction,
    curve_types_singleton, int_curve_type_iterator, curve_type_registration,
    curve_type_explanations, test_curve_types, test_noise_rules;

type
    { A curve type that names no rule. Never registered. }
    TRuleLessPointsSet = class(TNamedPointsSet)
    public
        class function GetCurveTypeName: string; override;
        class function GetCurveTypeId: TCurveTypeId; override;
    end;

    { The same type naming one. }
    TRuledPointsSet = class(TRuleLessPointsSet)
    public
        class function NoiseRule: TNoiseRuleClass; override;
    end;

    TNoiseRuleTest = class(TTestCase)
    protected
        procedure SetUp; override;
    published
        //  By construction: no rule, no instance and no registration.
        procedure ATypeThatNamesNoRuleCannotBeMade;
        procedure ATypeThatNamesNoRuleIsNotRegistered;
        procedure ATypeThatNamesARuleIsMade;
        procedure TheBareBaseIsTheClientsShellAndNamesNone;
        //  Over the registry.
        procedure EveryRegisteredCurveTypeNamesARuleThatMeasuresEveryPoint;
        procedure EveryRuleIsListedOnceAndExplainsItselfCompletely;
        procedure EveryCurveTypeLinksTheRuleItNames;
        procedure ATopicNamingNoRuleIsNotFound;
    end;

implementation

const
    RuleLessId: TGuid = '{6E2B9D41-3A7C-4F15-9B08-2C4D6E8F1A35}';

class function TRuleLessPointsSet.GetCurveTypeName: string;
begin
    Result := 'A type with no noise rule (test)';
end;

class function TRuleLessPointsSet.GetCurveTypeId: TCurveTypeId;
begin
    Result := RuleLessId;
end;

class function TRuledPointsSet.NoiseRule: TNoiseRuleClass;
begin
    Result := TTestCountsNoiseRule;
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

function Joined(const AItems: TStringArray): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AItems) do
        Result := Result + AItems[i] + LineEnding;
end;

procedure TNoiseRuleTest.SetUp;
begin
    RegisterAllCurveTypes;
end;

procedure TNoiseRuleTest.ATypeThatNamesNoRuleCannotBeMade;
var
    Raised: boolean;
begin
    Raised := False;
    try
        TRuleLessPointsSet.Create(nil).Free;
    except
        on E: ECurveTypeWithoutNoiseRule do
        begin
            Raised := True;
            AssertTrue('the refusal names the type: ' + E.Message,
                Pos('TRuleLessPointsSet', E.Message) > 0);
        end;
    end;
    AssertTrue('refused', Raised);
end;

procedure TNoiseRuleTest.ATypeThatNamesNoRuleIsNotRegistered;
var
    Before: longint;
    Raised: boolean;
begin
    Before := RegisteredClassCount;
    Raised := False;
    try
        TCurveTypesSingleton.CreateCurveFactory.RegisterCurveType(
            TRuleLessPointsSet);
    except
        on E: ECurveTypeWithoutNoiseRule do
            Raised := True;
    end;
    AssertTrue('refused', Raised);
    AssertEquals('and nothing added', Before, RegisteredClassCount);
    AssertNull('not found', FindCurveClassById(RuleLessId));
end;

procedure TNoiseRuleTest.ATypeThatNamesARuleIsMade;
var
    C: TRuledPointsSet;
begin
    C := TRuledPointsSet.Create(nil);
    try
        AssertTrue('its rule', C.NoiseRule = TTestCountsNoiseRule);
    finally
        C.Free;
    end;
end;

{ The client draws the server's points in a bare TNamedPointsSet
  (wire_point_sets): no curve type - no name, no id - and judging nothing. }
procedure TNoiseRuleTest.TheBareBaseIsTheClientsShellAndNamesNone;
var
    C: TNamedPointsSet;
begin
    AssertNull('names none', TNamedPointsSet.NoiseRule);
    C := TNamedPointsSet.Create(nil);
    C.Free;
end;

procedure TNoiseRuleTest.EveryRegisteredCurveTypeNamesARuleThatMeasuresEveryPoint;
const
    Y: array[0..7] of double = (5, 7, 6, 9, 12, 10, 8, 7);
var
    It: ICurveTypeIterator;
    Cls: TNamedPointsSetClass;
    V: TNoiseVariances;
    i: longint;
begin
    It := TCurveTypesSingleton.CreateCurveTypeIterator;
    It.FirstCurveType;
    repeat
        Cls := It.GetCurrentCurveClass;
        AssertTrue(Cls.GetCurveTypeName + ' names a noise rule',
            Assigned(Cls.NoiseRule));
        V := Cls.NoiseRule.Variances(Y);
        AssertEquals(Cls.GetCurveTypeName + ': a variance per point',
            Length(Y), Length(V));
        for i := 0 to High(V) do
            AssertTrue(Format('%s: a variance above zero at %d',
                [Cls.GetCurveTypeName, i]), V[i] > 0);
        if It.EndCurveType then
            Break;
        It.NextCurveType;
    until False;
end;

procedure TNoiseRuleTest.EveryRuleIsListedOnceAndExplainsItselfCompletely;
var
    Providers: TExplanationProviders;
    Topics: TStringArray;
    Seen: TStringList;
    i: longint;
begin
    Topics := NoiseRuleExplanationProvider.StaticTopics;
    AssertTrue('there are rules', Length(Topics) > 0);
    Seen := TStringList.Create;
    try
        for i := 0 to High(Topics) do
        begin
            AssertEquals(Topics[i] + ' listed once', -1, Seen.IndexOf(Topics[i]));
            Seen.Add(Topics[i]);
        end;
        //  The rule the suite's own peak names: a rule is listed because a
        //  registered type names it, not because some field's rule exists.
        AssertTrue('the rule the test peak names among them',
            Seen.IndexOf(NoiseRuleTopic(TTestGaussPointsSet.NoiseRule)) >= 0);
    finally
        Seen.Free;
    end;
    SetLength(Providers, 2);
    Providers[0] := CurveTypeExplanationProvider;
    Providers[1] := NoiseRuleExplanationProvider;
    AssertEquals('', Joined(ExplanationFindings(Providers)));
end;

procedure TNoiseRuleTest.EveryCurveTypeLinksTheRuleItNames;
var
    Topics: TStringArray;
    E: TExplanation;
    i, j: longint;
    Linked: boolean;
    Cls: TNamedPointsSetClass;
    It: ICurveTypeIterator;
begin
    It := TCurveTypesSingleton.CreateCurveTypeIterator;
    It.FirstCurveType;
    repeat
        Cls := It.GetCurrentCurveClass;
        AssertTrue(CurveTypeExplanationProvider.Explain(CurveTypeTopic(Cls), E));
        Linked := False;
        for j := 0 to High(E.Related) do
            Linked := Linked or (E.Related[j] = NoiseRuleTopic(Cls.NoiseRule));
        AssertTrue(Cls.GetCurveTypeName + ' links its noise rule', Linked);
        if It.EndCurveType then
            Break;
        It.NextCurveType;
    until False;
    //  And the link leads somewhere.
    Topics := NoiseRuleExplanationProvider.StaticTopics;
    for i := 0 to High(Topics) do
        AssertTrue(Topics[i], RegisteredTopicResolves(Topics[i]));
end;

procedure TNoiseRuleTest.ATopicNamingNoRuleIsNotFound;
var
    E: TExplanation;
begin
    AssertFalse(NoiseRuleExplanationProvider.Explain('noise-rule/none', E));
    AssertNull(FindNoiseRuleByTopic('noise-rule/none'));
end;

initialization
    RegisterTest('unit', TNoiseRuleTest);
end.
