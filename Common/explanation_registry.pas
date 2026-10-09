// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where any surface finds the explanation for a topic.)

WHY A REGISTRY OF PROVIDERS rather than an explanation field on each record that
has one. Explanations come from many kinds of thing - a curve type, a module's
rule, a verdict computed for one menu entry that exists only while the menu is
open - and many surfaces show them: the Explain pane, a hint, the registry dump,
the generated guide. With one lookup by topic, any surface shows any explanation
without knowing where it came from. That is the capability rule applied to
prose: whoever owns a thing explains it, and nothing central enumerates what can
be explained.

A TOPIC IS ROUTED BY ITS NAMESPACE, the text before its first slash, to the one
provider that registered that namespace. A provider answers topics it lists
(StaticTopics: everything that exists whatever the document) and topics it
computes (a subdivision verdict for one parent, leg and child), so a surface can
link to a computed topic that no list could hold.

A PROVIDER IS NOT TRUSTED to answer the question it was asked: an explanation
that comes back under another topic is refused, because showing one thing's
explanation under another's name is confidently wrong. ExplanationFindings is
where every other way a provider can be wrong is caught; each module's
completeness test calls it over the registry, so an explanation that does not
resolve, is incomplete, or links nowhere fails a build rather than a reader.

REGISTERING IS IDEMPOTENT BY NAMESPACE, like RegisterUiModule: every host that
links a module calls its front door, and twice is ordinary.
}
unit explanation_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, explanation;

type
    IExplanationProvider = interface
        ['{5B2D8E41-7C93-4A06-B1F8-2E6D09A4C375}']
        { The first segment of every topic this provider answers. Non-empty and
          without a slash. }
        function Namespace: string;
        { Every topic that exists independent of any document - what the
          completeness tests and the generated guide walk. }
        function StaticTopics: TStringArray;
        { Answers a listed or a computed topic. False when this provider has
          nothing to say about it. }
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

    TExplanationProviders = array of IExplanationProvider;

{ The text before the first slash, or '' when there is none to route by. }
function TopicNamespace(const ATopic: string): string;

procedure RegisterExplanationProvider(AProvider: IExplanationProvider);
function RegisteredExplanationProviders: TExplanationProviders;

{ The explanation for ATopic from the provider owning its namespace. False when
  no provider owns it, the provider cannot explain it, or the provider answered
  a different topic. }
function FindExplanationIn(const AProviders: TExplanationProviders;
    const ATopic: string; out AExplanation: TExplanation): boolean;
function FindExplanation(const ATopic: string;
    out AExplanation: TExplanation): boolean;

{ Whether ATopic resolves through the process-wide registry, and its title or ''.
  Shaped as the plain functions the Explain pane's decisions take, so a window
  passes these rather than wrapping FindExplanation itself. }
function RegisteredTopicResolves(const ATopic: string): boolean;
function RegisteredTopicTitle(const ATopic: string): string;

{ Every listed topic, provider by provider, in registration order. }
function StaticTopicsOf(const AProviders: TExplanationProviders): TStringArray;
function AllStaticTopics: TStringArray;

{ Every way these providers fail to explain themselves, one sentence each:
  a namespace claimed twice; a listed topic outside its provider's namespace,
  listed twice, not resolving, or incomplete; a related topic resolving nowhere.
  Empty when they are well formed. }
function ExplanationFindings(
    const AProviders: TExplanationProviders): TStringArray;

implementation

var
    Providers: TExplanationProviders;

function TopicNamespace(const ATopic: string): string;
var
    Slash: longint;
begin
    Slash := Pos('/', ATopic);
    if Slash <= 1 then
        Result := ''
    else
        Result := Copy(ATopic, 1, Slash - 1);
end;

procedure RegisterExplanationProvider(AProvider: IExplanationProvider);
var
    i: longint;
begin
    if not Assigned(AProvider) then
        raise Exception.Create('a nil explanation provider was registered');
    if AProvider.Namespace = '' then
        raise Exception.Create(
            'an explanation provider was registered with no namespace');
    if Pos('/', AProvider.Namespace) > 0 then
        raise Exception.CreateFmt('the explanation namespace "%s" contains a ' +
            'slash, so no topic could be routed to it', [AProvider.Namespace]);

    for i := 0 to High(Providers) do
        if Providers[i].Namespace = AProvider.Namespace then
            Exit;

    SetLength(Providers, Length(Providers) + 1);
    Providers[High(Providers)] := AProvider;
end;

function RegisteredExplanationProviders: TExplanationProviders;
begin
    Result := Providers;
end;

function FindExplanationIn(const AProviders: TExplanationProviders;
    const ATopic: string; out AExplanation: TExplanation): boolean;
var
    Namespace_: string;
    Found: TExplanation;
    i: longint;
begin
    AExplanation := Default(TExplanation);
    Result := False;
    Namespace_ := TopicNamespace(ATopic);
    if Namespace_ = '' then
        Exit;

    for i := 0 to High(AProviders) do
        if AProviders[i].Namespace = Namespace_ then
        begin
            if AProviders[i].Explain(ATopic, Found) and
                (Found.Topic = ATopic) then
            begin
                AExplanation := Found;
                Result := True;
            end;
            //  THE FIRST OWNER ONLY. A second provider claiming the namespace
            //  is a finding, not a fallback: which one answered would depend on
            //  registration order.
            Exit;
        end;
end;

function FindExplanation(const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    Result := FindExplanationIn(Providers, ATopic, AExplanation);
end;

function RegisteredTopicResolves(const ATopic: string): boolean;
var
    E: TExplanation;
begin
    Result := FindExplanation(ATopic, E);
end;

function RegisteredTopicTitle(const ATopic: string): string;
var
    E: TExplanation;
begin
    Result := '';
    if FindExplanation(ATopic, E) then
        Result := E.Title;
end;

function StaticTopicsOf(const AProviders: TExplanationProviders): TStringArray;
var
    i, j: longint;
    Topics: TStringArray;
begin
    Result := nil;
    for i := 0 to High(AProviders) do
    begin
        Topics := AProviders[i].StaticTopics;
        for j := 0 to High(Topics) do
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Topics[j];
        end;
    end;
end;

function AllStaticTopics: TStringArray;
begin
    Result := StaticTopicsOf(Providers);
end;

function ExplanationFindings(
    const AProviders: TExplanationProviders): TStringArray;
var
    Findings, Seen: TStringList;
    i, j, k: longint;
    Namespace_, Topic, Missing: string;
    Topics: TStringArray;
    E, Linked: TExplanation;
begin
    Findings := TStringList.Create;
    Seen := TStringList.Create;
    try
        for i := 0 to High(AProviders) do
            for j := 0 to i - 1 do
                if AProviders[j].Namespace = AProviders[i].Namespace then
                begin
                    Findings.Add(Format('the namespace "%s" is claimed by ' +
                        'more than one provider', [AProviders[i].Namespace]));
                    Break;
                end;

        for i := 0 to High(AProviders) do
        begin
            Namespace_ := AProviders[i].Namespace;
            Topics := AProviders[i].StaticTopics;
            for j := 0 to High(Topics) do
            begin
                Topic := Topics[j];
                if TopicNamespace(Topic) <> Namespace_ then
                begin
                    Findings.Add(Format('%s is listed by the "%s" provider ' +
                        'but lies outside its namespace', [Topic, Namespace_]));
                    Continue;
                end;
                if Seen.IndexOf(Topic) >= 0 then
                begin
                    Findings.Add(Format('%s is listed more than once',
                        [Topic]));
                    Continue;
                end;
                Seen.Add(Topic);

                if not FindExplanationIn(AProviders, Topic, E) then
                begin
                    Findings.Add(Format('%s is listed but does not resolve ' +
                        'to an explanation', [Topic]));
                    Continue;
                end;
                if not ExplanationIsComplete(E, Missing) then
                    Findings.Add(Format('%s is incomplete: %s',
                        [Topic, Missing]));
                for k := 0 to High(E.Related) do
                    if not FindExplanationIn(AProviders, E.Related[k],
                        Linked) then
                        Findings.Add(Format('%s names the related topic %s, ' +
                            'which resolves nowhere', [Topic, E.Related[k]]));
            end;
        end;

        SetLength(Result, Findings.Count);
        for i := 0 to Findings.Count - 1 do
            Result[i] := Findings[i];
    finally
        Seen.Free;
        Findings.Free;
    end;
end;

end.
