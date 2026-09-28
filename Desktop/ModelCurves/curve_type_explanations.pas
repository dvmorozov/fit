// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Every registered curve type, explained through the one registry.)

WHY A PROVIDER OVER THE REGISTRY rather than one per curve unit. A curve type
already states everything else about itself as class functions on
TNamedPointsSet, and its explanation is one more of those. What is left to do
centrally is only to route: list whatever is registered, and answer a topic by
asking the class it names. So a module's curve type becomes explainable by
registering as a curve type - with no second registration a module author could
forget.

THE TOPIC IS THE TYPE'S ID, not its name. Names are display text and a module
may reword one; the id is what the registry, the project file and the wire
already key a type by.

THE PROVIDER STAMPS IDENTITY. It sets the topic, and the title when the class
leaves it empty, so an override states only content and cannot mislabel itself.
}
unit curve_type_explanations;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry, named_points_set;

const
    CurveTypeNamespace = 'curve-type';

{ 'curve-type/<id>', the id in lower case without braces. }
function CurveTypeTopic(AClass: TNamedPointsSetClass): string;

{ The topic of the registered curve type called AName, or '' when no type or
  more than one has that name - two types sharing a name cannot say which one
  a curve is, and explaining the wrong one is worse than explaining none. }
function CurveTypeTopicForName(const AName: string): string;

{ The one provider for every registered curve type. }
function CurveTypeExplanationProvider: IExplanationProvider;

{ Registers that provider. Called from RegisterAllCurveTypes, which every binary
  that creates curves already calls at start-up; idempotent. }
procedure RegisterCurveTypeExplanations;

implementation

uses
    curve_types_singleton, int_curve_type_iterator;

type
    TCurveTypeExplanationProvider = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

var
    Instance: TCurveTypeExplanationProvider = nil;

function CurveTypeTopic(AClass: TNamedPointsSetClass): string;
var
    Id: string;
begin
    Id := LowerCase(GUIDToString(AClass.GetCurveTypeId));
    Result := CurveTypeNamespace + '/' + Copy(Id, 2, Length(Id) - 2);
end;

function TCurveTypeExplanationProvider.Namespace: string;
begin
    Result := CurveTypeNamespace;
end;

function TCurveTypeExplanationProvider.StaticTopics: TStringArray;
var
    It: ICurveTypeIterator;
    Cls: TNamedPointsSetClass;
begin
    Result := nil;
    It := TCurveTypesSingleton.CreateCurveTypeIterator;
    It.FirstCurveType;
    //  EndCurveType means "this is the last one", not "past the end".
    while True do
    begin
        Cls := It.GetCurrentCurveClass;
        if Assigned(Cls) then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := CurveTypeTopic(Cls);
        end;
        if It.EndCurveType then
            Break;
        It.NextCurveType;
    end;
end;

function TCurveTypeExplanationProvider.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
var
    Prefix, IdText: string;
    Id: TGuid;
    Cls: TNamedPointsSetClass;
begin
    AExplanation := Default(TExplanation);
    Result := False;
    Prefix := CurveTypeNamespace + '/';
    if Copy(ATopic, 1, Length(Prefix)) <> Prefix then
        Exit;
    IdText := Copy(ATopic, Length(Prefix) + 1, MaxInt);
    //  A malformed topic is a link that leads nowhere, which a surface shows as
    //  "nothing to explain" - never a fault in front of the user.
    if not TryStringToGUID('{' + IdText + '}', Id) then
        Exit;
    Cls := FindCurveClassById(Id);
    if not Assigned(Cls) then
        Exit;

    AExplanation := Cls.Explanation;
    AExplanation.Topic := CurveTypeTopic(Cls);
    if AExplanation.Title = '' then
        AExplanation.Title := Cls.GetCurveTypeName;
    //  Asked in another spelling of the same id, it is still the same type -
    //  but the registry believes only an answer to the topic it asked, so the
    //  canonical spelling is the one that resolves.
    Result := AExplanation.Topic = ATopic;
end;

function CurveTypeTopicForName(const AName: string): string;
var
    Found: TNamedPointsSetClass;
begin
    Result := '';
    Found := FindCurveClassByName(AName);
    if Assigned(Found) then
        Result := CurveTypeTopic(Found);
end;

function CurveTypeExplanationProvider: IExplanationProvider;
begin
    if not Assigned(Instance) then
        Instance := TCurveTypeExplanationProvider.Create;
    Result := Instance;
end;

procedure RegisterCurveTypeExplanations;
begin
    RegisterExplanationProvider(CurveTypeExplanationProvider);
end;

finalization
    Instance.Free;
end.
