// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A provider whose topics are written out in advance, in reading order.)

WHY THIS EXISTS. Most of what the application explains is not computed from a
document: what a menu command does, how a project file is kept, what the fit
engines are. Each of those chapters would otherwise repeat the same provider
class - a namespace, a list, a lookup - and differ only in the prose. Here the
chapter supplies the prose and nothing else.

A BUILDER, NOT A LIST. The explanations are built on every question rather than
once, so a chapter may describe what is registered right now - the engines this
build offers, the formats it reads - and say what the application actually does
rather than what it did when the text was written.

READING ORDER IS KEPT. StaticTopics lists the topics in the order the builder
returns them. The published guide reads them in that order; the Explain
Everything index sorts them by title for looking up, which is its own decision.
}
unit static_explanations;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry;

type
    { Every explanation of one namespace, in reading order. }
    TExplanationsBuilder = function: TExplanations;

    TStaticExplanationProvider = class(TObject, IExplanationProvider)
    private
        FNamespace: string;
        FBuild: TExplanationsBuilder;
    public
        constructor Create(const ANamespace: string; ABuild: TExplanationsBuilder);
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

{ Appends AExplanation to AList, for a builder that assembles its chapter one
  topic at a time. }
procedure AppendExplanation(var AList: TExplanations;
    const AExplanation: TExplanation);

implementation

procedure AppendExplanation(var AList: TExplanations;
    const AExplanation: TExplanation);
begin
    SetLength(AList, Length(AList) + 1);
    AList[High(AList)] := AExplanation;
end;

constructor TStaticExplanationProvider.Create(const ANamespace: string;
    ABuild: TExplanationsBuilder);
begin
    inherited Create;
    if not Assigned(ABuild) then
        raise Exception.CreateFmt('the explanation chapter "%s" has no builder',
            [ANamespace]);
    FNamespace := ANamespace;
    FBuild := ABuild;
end;

function TStaticExplanationProvider.Namespace: string;
begin
    Result := FNamespace;
end;

function TStaticExplanationProvider.StaticTopics: TStringArray;
var
    All: TExplanations;
    i: longint;
begin
    All := FBuild();
    Result := nil;
    SetLength(Result, Length(All));
    for i := 0 to High(All) do
        Result[i] := All[i].Topic;
end;

function TStaticExplanationProvider.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
var
    All: TExplanations;
    i: longint;
begin
    AExplanation := Default(TExplanation);
    Result := False;
    All := FBuild();
    for i := 0 to High(All) do
        if All[i].Topic = ATopic then
        begin
            AExplanation := All[i];
            Result := True;
            Exit;
        end;
end;

end.
