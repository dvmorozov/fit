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
    SetLength(Result, 2);
    Result[0] := LiveFitProgressTopic;
    Result[1] := AnimationModeTopic;
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
        AddParagraph(AExplanation, 'The line above the chart says how long the ' +
            'fit has run, the R-factor it has reached, and how far that is below ' +
            'where it started. Use Stop to end the fit.');
        AddParagraph(AExplanation, 'Once the engine reports, the chart shows the ' +
            'R-factor against elapsed time on a logarithmic scale, because a fit ' +
            'improves by factors of ten: on a linear scale everything after the ' +
            'first moments would be a flat line along the bottom.');
        AddParagraph(AExplanation, 'Until then the data stays on screen. An engine ' +
            'that reports no intermediate progress is said to be one, and its ' +
            'result appears when it finishes.');
        AddParagraph(AExplanation, 'The value shown is the same R-factor the status ' +
            'bar reports while the fit runs. When the fit is over, the status bar ' +
            'shows the reduced chi-squared and R2 of the result instead.');
        AddLimitation(AExplanation, 'The engine records a point each time it finds ' +
            'a better fit, not on every evaluation, so a long flat stretch means ' +
            'no better fit was found there rather than that nothing happened.');
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
