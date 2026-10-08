// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What the live progress view and Animation Mode say about themselves.)

A CHART THAT BECOMES ANOTHER CHART is something a user meets without having asked
for it, so it explains itself like everything else here: what is drawn and why on
that scale, what "no intermediate progress" means, and what Animation Mode shows
and costs. Framework words only - which engine or model is fitting is not this
explanation's business, and none of it depends on either.

REGISTERED BY THE WINDOW, unconditionally: every build can fit.
}
unit fit_progress_explanations;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry;

const
    FitProgressNamespace = 'fit-progress';
    LiveFitProgressTopic = 'fit-progress/live-progress';
    AnimationModeTopic = 'fit-progress/animation-mode';
    RFactorScaleTopic = 'fit-progress/r-factor-scale';

function FitProgressExplanationProvider: IExplanationProvider;
procedure RegisterFitProgressExplanations;

implementation

type
    TFitProgressExplanationProvider = class(TObject, IExplanationProvider)
    public
        function Namespace: string;
        function StaticTopics: TStringArray;
        function Explain(const ATopic: string;
            out AExplanation: TExplanation): boolean;
    end;

var
    Instance: TFitProgressExplanationProvider = nil;

function TFitProgressExplanationProvider.Namespace: string;
begin
    Result := FitProgressNamespace;
end;

function TFitProgressExplanationProvider.StaticTopics: TStringArray;
begin
    Result := nil;
    SetLength(Result, 3);
    Result[0] := LiveFitProgressTopic;
    Result[1] := AnimationModeTopic;
    Result[2] := RFactorScaleTopic;
end;

function TFitProgressExplanationProvider.Explain(const ATopic: string;
    out AExplanation: TExplanation): boolean;
begin
    AExplanation := Default(TExplanation);
    Result := True;
    //  THIS SOFTWARE'S CHOICE, both: how progress is shown is decided here, and
    //  no source states it.
    if ATopic = LiveFitProgressTopic then
    begin
        AExplanation := NewExplanation(LiveFitProgressTopic, 'Fit progress',
            'While a fit runs, the chart area shows how far it has got instead of ' +
            'an empty chart.', esModelChoice);
        AddParagraph(AExplanation, 'The line above the chart says what is being ' +
            'minimised and by which engine, the R-factor reached so far - in the ' +
            'form chosen under View > R-factor Scale - and how far it has fallen ' +
            'since the start. The status bar under the chart shows the elapsed ' +
            'time and the R-factor itself, and says to use Stop to end the fit.');
        AddParagraph(AExplanation, 'How far it has fallen is counted in orders ' +
            'of magnitude: "down 1.00" means ten times lower, "down 2.00" a ' +
            'hundred times lower. A percentage would stop near 100 % while the ' +
            'fit was still getting ten times better, and look as if it had ' +
            'stopped.');
        AddParagraph(AExplanation, 'Once the engine reports, the chart shows the ' +
            'R-factor against elapsed time, on a logarithmic scale unless you ' +
            'choose another under View > R-factor Scale, because a fit improves ' +
            'by factors of ten: on a linear scale everything after the first ' +
            'moments would be a flat line along the bottom.');
        AddParagraph(AExplanation, 'Until then the data stays on screen. An engine ' +
            'that reports no intermediate progress is said to be one, and its ' +
            'result appears when it finishes.');
        AddParagraph(AExplanation, 'The value shown is the same R-factor the status ' +
            'bar reports while the fit runs. When the fit is over, the status bar ' +
            'shows the reduced chi-squared and R2 of the result instead.');
        AddLimitation(AExplanation, 'The engine records a point each time it finds ' +
            'a better fit, not on every evaluation, so a long flat stretch means ' +
            'no better fit was found there rather than that nothing happened.');
        AddRelated(AExplanation, RFactorScaleTopic);
    end
    else if ATopic = AnimationModeTopic then
    begin
        AExplanation := NewExplanation(AnimationModeTopic, 'Animation Mode',
            'With View > Animation Mode ticked, a running fit redraws the model ' +
            'over the data as it improves, instead of the chart of its R-factor.',
            esModelChoice);
        AddParagraph(AExplanation, 'The curves and the computed profile are ' +
            'redrawn about twice a second, each as it stood at the last better ' +
            'fit the engine found. The tables beside the chart keep the last ' +
            'finished result until the fit is over.');
        AddParagraph(AExplanation, 'Animating makes a fit slower: the engine ' +
            'rebuilds the whole model for every frame it sends. The setting is ' +
            'remembered between sessions.');
        AddLimitation(AExplanation, 'A frame shows the model at the last better ' +
            'fit the engine found, not at every evaluation, so the curves move in ' +
            'steps rather than smoothly.');
    end
    else if ATopic = RFactorScaleTopic then
    begin
        AExplanation := NewExplanation(RFactorScaleTopic, 'R-factor scale',
            'View > R-factor Scale chooses how the R-factor of a running fit is ' +
            'drawn on the progress chart; right-clicking that chart offers the ' +
            'same choices.', esModelChoice);
        AddParagraph(AExplanation, 'View > R-factor Scale > Automatic draws it on ' +
            'a logarithmic scale, because a fit improves by factors of ten: on a ' +
            'linear scale everything after the first moments would be a flat ' +
            'line along the bottom. View > R-factor Scale > Logarithmic chooses ' +
            'that scale outright, so it stays even if what Automatic means ' +
            'changes. Either way the marks on the axis are R-factor values.');
        AddParagraph(AExplanation, 'View > R-factor Scale > Linear draws the ' +
            'R-factor as it is. It shows the first, large improvements well and ' +
            'hides the later, small ones.');
        AddParagraph(AExplanation, 'View > R-factor Scale > Custom R-factor... ' +
            'draws it through a formula of your own, of x, the R-factor, and asks ' +
            'for the formula back from the drawn value, as a custom axis of the ' +
            'data does. log(x), the base-10 logarithm, with 10^x back draws the ' +
            'logarithm itself as a number.');
        AddParagraph(AExplanation, 'The line above the chart gives the R-factor in ' +
            'the same form: "R-factor 1.32312E-6" on the linear scale, "log10 ' +
            'R-factor -5.87840" on the logarithmic one, and the value of your ' +
            'formula under the name you gave it on a custom one. The logarithm ' +
            'keeps moving visibly while a long fit improves by small steps. The ' +
            'R-factor itself is always on the status bar.');
        AddParagraph(AExplanation, 'The choice can be made while a fit runs, and ' +
            'the chart follows at its next update. It is saved with the project, ' +
            'like the axes of the data.');
        AddParagraph(AExplanation, 'An analysis module may offer more ways of ' +
            'drawing it; each says in its own explanation what it does.');
        AddLimitation(AExplanation, 'The scale changes only how the R-factor is ' +
            'drawn, never what the fit minimises or how it gets there.');
    end
    else
        Result := False;
end;

function FitProgressExplanationProvider: IExplanationProvider;
begin
    if not Assigned(Instance) then
        Instance := TFitProgressExplanationProvider.Create;
    Result := Instance;
end;

procedure RegisterFitProgressExplanations;
begin
    RegisterExplanationProvider(FitProgressExplanationProvider);
end;

finalization
    Instance.Free;
end.
