// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(An explanation provider whose topics a test decides.)

WHY IT EXISTS. The registry routes a topic to whichever provider owns its
namespace, and judges whether what comes back is worth showing. The interesting
cases are providers that are WRONG - one that lists a topic it cannot explain,
one that answers a different topic from the one asked, one whose explanation is
incomplete - and no real provider is built to be wrong on purpose.

A plain TObject, per mock_support: everything compiles -SIcorba, so the fixture
owns this outright. A test that registers one into the process-wide registry
must keep it alive for the rest of the run, because the registry holds the
interface and has no removal; the ones here that are only passed in arrays can
be freed as usual.
}
unit mock_explanation_provider;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation, explanation_registry;

type
    TMockExplanationProvider = class(TObject, IExplanationProvider)
    private
        FNamespace: string;
        FStatic: TStringArray;
        FKnown: TExplanations;
        FAnswerAs: TStringList;
        FExplainCalls: longint;
    public
        constructor Create(const ANamespace: string);
        destructor Destroy; override;
        function AsObject: TObject;

        { Makes AExplanation answerable, and lists its topic as static unless
          AStatic is False - which is what a computed topic looks like. }
        procedure Add(const AExplanation: TExplanation;
            AStatic: boolean = True);
        { Lists a topic as static without being able to explain it. }
        procedure ListWithoutExplaining(const ATopic: string);
        { When asked for AAsked, answers with the explanation for AAnswered -
          the provider that answers a question it was not asked. }
        procedure AnswerAs(const AAsked, AAnswered: string);

        property ExplainCalls: longint read FExplainCalls;

        //  IExplanationProvider
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

implementation

constructor TMockExplanationProvider.Create(const ANamespace: string);
begin
    inherited Create;
    FNamespace := ANamespace;
    FAnswerAs := TStringList.Create;
end;

destructor TMockExplanationProvider.Destroy;
begin
    FAnswerAs.Free;
    inherited;
end;

function TMockExplanationProvider.AsObject: TObject;
begin
    Result := Self;
end;

procedure TMockExplanationProvider.Add(const AExplanation: TExplanation;
    AStatic: boolean);
begin
    SetLength(FKnown, Length(FKnown) + 1);
    FKnown[High(FKnown)] := AExplanation;
    if AStatic then
        ListWithoutExplaining(AExplanation.Topic);
end;

procedure TMockExplanationProvider.ListWithoutExplaining(const ATopic: string);
begin
    SetLength(FStatic, Length(FStatic) + 1);
    FStatic[High(FStatic)] := ATopic;
end;

procedure TMockExplanationProvider.AnswerAs(const AAsked, AAnswered: string);
begin
    FAnswerAs.Values[AAsked] := AAnswered;
end;

function TMockExplanationProvider.Namespace: string;
begin
    Result := FNamespace;
end;

function TMockExplanationProvider.StaticTopics: TStringArray;
begin
    Result := Copy(FStatic);
end;

function TMockExplanationProvider.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
var
    Wanted: string;
    i: longint;
begin
    Inc(FExplainCalls);
    AExplanation := Default(TExplanation);
    Wanted := ATopic;
    if FAnswerAs.IndexOfName(ATopic) >= 0 then
        Wanted := FAnswerAs.Values[ATopic];
    for i := 0 to High(FKnown) do
        if FKnown[i].Topic = Wanted then
        begin
            AExplanation := FKnown[i];
            Exit(True);
        end;
    Result := False;
end;

end.
