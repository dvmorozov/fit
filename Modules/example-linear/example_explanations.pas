// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The smallest explanation provider a module can have.)

WHAT A MODULE OWES THE USER, besides its curve types: an explanation for every
rule it applies. The curve type itself already explains itself (TLinearPointsSet
overrides TNamedPointsSet.Explanation), and the framework's curve-type provider
answers for it. A rule of the module's own is answered here, under the module's
own namespace - copy this for a module of your own, change the namespace, and
list every topic in StaticTopics so the registry's completeness check walks it.
}
unit example_explanations;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry;

const
    ExampleNamespace = 'example';
    { The one rule this module applies, and the topic it is explained under. }
    PositiveWidthTopic = 'example/rule/positive-width';

function ExampleExplanationProvider: IExplanationProvider;
procedure RegisterExampleExplanations;

implementation

uses
    curve_type_explanations, linear_points_set;

type
    TExampleExplanationProvider = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

var
    Instance: TExampleExplanationProvider = nil;

function TExampleExplanationProvider.Namespace: string;
begin
    Result := ExampleNamespace;
end;

function TExampleExplanationProvider.StaticTopics: TStringArray;
begin
    Result := nil;
    SetLength(Result, 1);
    Result[0] := PositiveWidthTopic;
end;

function TExampleExplanationProvider.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    AExplanation := Default(TExplanation);
    Result := ATopic = PositiveWidthTopic;
    if not Result then
        Exit;
    //  THIS SOFTWARE'S CHOICE, and said so: no field's literature states a rule
    //  about a teaching example. A canonical standing would need a source.
    AExplanation := NewExplanation(PositiveWidthTopic,
        'The ramp has a positive width',
        'The ramp''s width sigma is kept above zero, so it always spans at ' +
        'least one sample.', esModelChoice);
    AddParagraph(AExplanation, 'A ramp of zero width is zero everywhere, so the ' +
        'fit could not tell its amplitude or its slope from anything - both ' +
        'would be free to wander without changing the model at all.');
    AddLimitation(AExplanation, 'A genuinely instantaneous step cannot be ' +
        'described by this curve; the framework''s Step type is the one for that.');
    AddRelated(AExplanation, CurveTypeTopic(TLinearPointsSet));
end;

function ExampleExplanationProvider: IExplanationProvider;
begin
    if not Assigned(Instance) then
        Instance := TExampleExplanationProvider.Create;
    Result := Instance;
end;

procedure RegisterExampleExplanations;
begin
    RegisterExplanationProvider(ExampleExplanationProvider);
end;

finalization
    Instance.Free;
end.
