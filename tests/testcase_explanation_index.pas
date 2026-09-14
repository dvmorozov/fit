// SPDX-License-Identifier: GPL-3.0-or-later
{ Tests for the index of everything this build can explain.

  WHY THESE EXIST. Help > Explain Everything is how a user who does not yet know
  what to point at finds what the application knows: every curve type, every
  rule a module enforces. A topic missing from it is a lesson nobody can find,
  and an index in registration order is one nobody can read - so the grouping,
  the order and the wording of each heading are pinned here, over providers a
  test controls. }
unit testcase_explanation_index;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation,
    explanation_registry, explanation_index, mock_explanation_provider;

type
    TExplanationIndexTest = class(TTestCase)
    private
        FCurves, FWaves: TMockExplanationProvider;
        function Both: TExplanationProviders;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ANamespaceIsCaptionedInWords;
        procedure EachNamespaceHeadsItsOwnTopics;
        procedure NamespacesKeepTheOrderTheyWereRegisteredIn;
        procedure TopicsAreOrderedByTitleWithinTheirNamespace;
        procedure ATopicRowReadsAsItsTitle;
        procedure AnUntitledTopicReadsAsItsTopic;
        procedure AHeaderCarriesNoTopic;
        procedure ATopicThatDoesNotResolveIsLeftOut;
        procedure AProviderWithNothingToListHasNoHeader;
        procedure NoProvidersIsAnEmptyIndex;
    end;

implementation

function Explained(const ATopic, ATitle: string): TExplanation;
begin
    Result := NewExplanation(ATopic, ATitle, 'About ' + ATitle + '.',
        esConvention);
    AddParagraph(Result, 'Body.');
end;

procedure TExplanationIndexTest.SetUp;
begin
    FCurves := TMockExplanationProvider.Create('curve-type');
    FCurves.Add(Explained('curve-type/v', 'Voigt'));
    FCurves.Add(Explained('curve-type/g', 'gaussian'));
    FCurves.Add(Explained('curve-type/l', 'Lorentzian'));
    FWaves := TMockExplanationProvider.Create('waves');
    FWaves.Add(Explained('waves/rule/P4', 'Triangles never appear alone as wave 2'));
end;

procedure TExplanationIndexTest.TearDown;
begin
    FCurves.Free;
    FWaves.Free;
end;

function TExplanationIndexTest.Both: TExplanationProviders;
begin
    SetLength(Result, 2);
    Result[0] := FCurves;
    Result[1] := FWaves;
end;

procedure TExplanationIndexTest.ANamespaceIsCaptionedInWords;
begin
    AssertEquals('Curve type', NamespaceCaption('curve-type'));
    AssertEquals('Waves', NamespaceCaption('waves'));
    AssertEquals('A b c', NamespaceCaption('a-b-c'));
    AssertEquals('', NamespaceCaption(''));
end;

procedure TExplanationIndexTest.EachNamespaceHeadsItsOwnTopics;
var
    Rows: TExplanationIndexRows;
begin
    Rows := ExplanationIndexOf(Both);
    AssertEquals(6, Length(Rows));
    AssertTrue(Rows[0].IsHeader);
    AssertEquals('Curve type', Rows[0].Caption);
    AssertFalse(Rows[1].IsHeader);
    AssertTrue(Rows[4].IsHeader);
    AssertEquals('Waves', Rows[4].Caption);
    AssertEquals('waves/rule/P4', Rows[5].Topic);
end;

procedure TExplanationIndexTest.NamespacesKeepTheOrderTheyWereRegisteredIn;
var
    Providers: TExplanationProviders;
    Rows: TExplanationIndexRows;
begin
    SetLength(Providers, 2);
    Providers[0] := FWaves;
    Providers[1] := FCurves;
    Rows := ExplanationIndexOf(Providers);
    AssertEquals('Waves', Rows[0].Caption);
    AssertEquals('Curve type', Rows[2].Caption);
end;

procedure TExplanationIndexTest.TopicsAreOrderedByTitleWithinTheirNamespace;
var
    Rows: TExplanationIndexRows;
begin
    //  Case does not decide the order: 'gaussian' is not after 'Voigt'.
    Rows := ExplanationIndexOf(Both);
    AssertEquals('curve-type/g', Rows[1].Topic);
    AssertEquals('curve-type/l', Rows[2].Topic);
    AssertEquals('curve-type/v', Rows[3].Topic);
end;

procedure TExplanationIndexTest.ATopicRowReadsAsItsTitle;
begin
    AssertEquals('Lorentzian', ExplanationIndexOf(Both)[2].Caption);
end;

procedure TExplanationIndexTest.AHeaderCarriesNoTopic;
begin
    AssertEquals('', ExplanationIndexOf(Both)[0].Topic);
end;

procedure TExplanationIndexTest.ATopicThatDoesNotResolveIsLeftOut;
var
    Rows: TExplanationIndexRows;
    i: longint;
begin
    FCurves.ListWithoutExplaining('curve-type/listed-only');
    Rows := ExplanationIndexOf(Both);
    for i := 0 to High(Rows) do
        AssertFalse(Rows[i].Topic = 'curve-type/listed-only');
    AssertEquals(6, Length(Rows));
end;

procedure TExplanationIndexTest.AProviderWithNothingToListHasNoHeader;
var
    Empty: TMockExplanationProvider;
    Providers: TExplanationProviders;
    Rows: TExplanationIndexRows;
begin
    Empty := TMockExplanationProvider.Create('empty');
    try
        SetLength(Providers, 2);
        Providers[0] := Empty;
        Providers[1] := FWaves;
        Rows := ExplanationIndexOf(Providers);
        AssertEquals(2, Length(Rows));
        AssertEquals('Waves', Rows[0].Caption);
    finally
        Providers := nil;
        Empty.Free;
    end;
end;

procedure TExplanationIndexTest.NoProvidersIsAnEmptyIndex;
begin
    AssertEquals(0, Length(ExplanationIndexOf(nil)));
end;


procedure TExplanationIndexTest.AnUntitledTopicReadsAsItsTopic;
var
    Untitled: TMockExplanationProvider;
    Providers: TExplanationProviders;
    Rows: TExplanationIndexRows;
begin
    //  A row with no caption is a blank line in a list the user reads to find
    //  something; the topic at least names what is there.
    Untitled := TMockExplanationProvider.Create('bare');
    try
        Untitled.Add(Explained('bare/thing', ''));
        SetLength(Providers, 1);
        Providers[0] := Untitled;
        Rows := ExplanationIndexOf(Providers);
        AssertEquals(2, Length(Rows));
        AssertEquals('bare/thing', Rows[1].Caption);
    finally
        Providers := nil;
        Untitled.Free;
    end;
end;

initialization
    RegisterTest('unit', TExplanationIndexTest);
end.
