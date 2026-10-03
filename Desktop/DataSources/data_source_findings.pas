// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What is wrong with the sources a build registered, if anything.)

THE WALK, NOT A LIST. This is ExplanationFindings' shape: it asks the registry
what is in it and checks each entry against the contract, so the next source
registered without an explanation - in this repository or in a module nobody
here has heard of - fails by NAME rather than failing in front of a user.

Each finding is one sentence naming the source and what it is missing, and the
test asserts the whole report is empty. A test that counted findings would pass
while a different one appeared.

WHY THE LOADER CHECK LIVES HERE. A source that can only hand over files no
reader in this build handles is a dead end: the user finds the record, picks the
file and is then told it cannot be opened. That is a build-time fact, so it is a
build-time failure - and it is also what stops a module shipping a source whose
loader it forgot to register.
}
unit data_source_findings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, data_source_registry;

type
    TStringArray = array of string;

{ Everything wrong with what is registered, one sentence each. Empty when the
  build is sound. AResolves answers whether an explanation topic resolves - the
  explanation registry is passed in rather than reached for, so this unit stays
  a rule and the test can drive both answers. }
type
    TTopicResolves = function(const ATopic: string): boolean;

function DataSourceFindings(AResolves: TTopicResolves): TStringArray;

{ The same, over a GIVEN set of sources rather than the registry.

  WHY IT EXISTS: a test that wants to see what an incomplete source is reported
  as must not have to REGISTER one to find out. The registry has no way to take
  a source back out - nothing in the application ever needs one - so a probe
  registered by a test would stay in it for every later test in the process, and
  the next walk would report a fault nobody put in the build. That happened: a
  module's suite failed over three broken probes the framework's own suite had
  left behind. }
function FindingsFor(const ASources: TDataSourceClasses;
    AResolves: TTopicResolves): TStringArray;

{ The findings as one text, for an assertion that reads as the violated rule. }
function JoinedFindings(const AFindings: TStringArray): string;

implementation

uses
    data_loader_registry;

function ChoiceOffered(const AChoices: TInputChoices;
    const AValue: string): boolean;
var
    i: longint;
begin
    for i := 0 to High(AChoices) do
        if AChoices[i].Value = AValue then
            Exit(True);
    Result := False;
end;

procedure Add(var AList: TStringArray; const AText: string);
begin
    SetLength(AList, Length(AList) + 1);
    AList[High(AList)] := AText;
end;

function FindingsFor(const ASources: TDataSourceClasses;
    AResolves: TTopicResolves): TStringArray;
var
    Sources: TDataSourceClasses;
    Info: TDataSourceInfo;
    Extensions: TStringList;
    Seen: TStringList;
    i, j, k, Guard: longint;
    Name_: string;
begin
    Result := nil;
    Sources := ASources;
    Seen := TStringList.Create;
    try
        for i := 0 to High(Sources) do
        begin
            Info := Sources[i].Info;
            Name_ := Sources[i].ClassName;

            if Trim(Info.Id) = '' then
                Add(Result, Name_ + ' declares no id');
            if Seen.IndexOf(LowerCase(Info.Id)) >= 0 then
                Add(Result, Name_ + ' registers the id "' + Info.Id +
                    '", which another source already uses');
            Seen.Add(LowerCase(Info.Id));

            if Trim(Info.Title) = '' then
                Add(Result, Name_ + ' declares no title, so the wizard would ' +
                    'offer a nameless source');
            if Trim(Info.Category) = '' then
                Add(Result, Name_ + ' declares no category, so it would be ' +
                    'grouped under nothing');
            if Trim(Info.Summary) = '' then
                Add(Result, Name_ + ' declares no summary, so a user would ' +
                    'choose it with nothing to go on');

            if Trim(Info.Topic) = '' then
                Add(Result, Name_ + ' declares no explanation topic')
            else if Assigned(AResolves) and not AResolves(Info.Topic) then
                Add(Result, Name_ + ' names the topic "' + Info.Topic +
                    '", which resolves to no explanation');

            if Trim(Info.ProducesExtensions) = '' then
                Add(Result, Name_ + ' does not say what kind of file it ' +
                    'produces, so nothing can tell whether this build can ' +
                    'read what it fetches')
            else
            begin
                Extensions := TStringList.Create;
                try
                    Extensions.Delimiter := ';';
                    Extensions.StrictDelimiter := True;
                    Extensions.DelimitedText := Info.ProducesExtensions;
                    for j := 0 to Extensions.Count - 1 do
                        if Trim(Extensions[j]) <> '' then
                            if FindDataLoaderClass('x' + Trim(Extensions[j])) = nil then
                                Add(Result, Name_ + ' produces ' +
                                    Trim(Extensions[j]) +
                                    ' files, which no loader in this build reads');
                finally
                    Extensions.Free;
                end;
            end;

            for j := 0 to High(Info.QueryFields) do
            begin
                if Trim(Info.QueryFields[j].Id) = '' then
                    Add(Result, Name_ + ' declares a query field with no id');
                if Trim(Info.QueryFields[j].Caption) = '' then
                    Add(Result, Name_ + ' declares the query field "' +
                        Info.QueryFields[j].Id + '" with no caption');
                if (Info.QueryFields[j].Kind = ifChoice) and
                    (Length(Info.QueryFields[j].Choices) = 0) then
                    Add(Result, Name_ + ' declares the choice field "' +
                        Info.QueryFields[j].Id + '" with nothing to choose from');
                //  A FIELD ASKED ONLY WHEN ANOTHER SAYS SO must name a field
                //  that exists, and an answer it can give - or it is greyed
                //  for good, with a reason naming nothing on screen.
                if Info.QueryFields[j].EnabledWhenField <> '' then
                begin
                    Guard := -1;
                    for k := 0 to High(Info.QueryFields) do
                        if SameText(Info.QueryFields[k].Id,
                            Info.QueryFields[j].EnabledWhenField) then
                            Guard := k;
                    if Guard < 0 then
                        Add(Result, Name_ + ' asks "' + Info.QueryFields[j].Id +
                            '" only when "' + Info.QueryFields[j].EnabledWhenField +
                            '" says so, and declares no such field')
                    else if (Info.QueryFields[Guard].Kind = ifChoice) and
                        not ChoiceOffered(Info.QueryFields[Guard].Choices,
                            Info.QueryFields[j].EnabledWhenValue) then
                        Add(Result, Name_ + ' asks "' + Info.QueryFields[j].Id +
                            '" only when "' + Info.QueryFields[Guard].Id +
                            '" is "' + Info.QueryFields[j].EnabledWhenValue +
                            '", which it does not offer');
                end;
                //  A CHOICE WITH NO CAPTION would put the service's own
                //  identifier in front of the user, which is the one thing
                //  captions exist to prevent.
                for k := 0 to High(Info.QueryFields[j].Choices) do
                    if Trim(Info.QueryFields[j].Choices[k].Caption) = '' then
                        Add(Result, Name_ + ' offers a choice in "' +
                            Info.QueryFields[j].Id + '" with no name, so the ' +
                            'user would be shown "' +
                            Info.QueryFields[j].Choices[k].Value + '"');
            end;
        end;
    finally
        Seen.Free;
    end;
end;

function DataSourceFindings(AResolves: TTopicResolves): TStringArray;
begin
    Result := FindingsFor(RegisteredDataSources, AResolves);
end;

function JoinedFindings(const AFindings: TStringArray): string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(AFindings) do
    begin
        if Result <> '' then
            Result := Result + LineEnding;
        Result := Result + AFindings[i];
    end;
end;

end.
