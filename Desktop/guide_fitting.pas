// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide's chapter on fitting: the Fit menu, the number a fit
reports, the engines and where they run.)

WHAT IT COVERS. Everything a user meets between "the model is built" and "the
result is on screen": the three fit commands and Stop, the automatic
decomposition and the rule that decides how many curves it keeps, the R-factor
and its ceiling, curve scaling, the minimizer, the loss function, weighting,
the compute server, why a fit command is greyed and what a fit may change
about what was selected. Each statement was checked against the code that
decides it - Server/fit_service.pas, Server/fit_task.pas, Server/fit_advice.pas,
Server/service_state_rules.pas, Desktop/action_state.pas and the form - and a
number here is the code's number.

TWO TOPICS DESCRIBE WHAT IS REGISTERED, not what was written down: the engines
under Fit > Minimizer are read from minimizer_registry and the objectives under
Fit > Loss Function from fit_loss, at the moment the explanation is asked for.
The static provider builds the chapter on every question for exactly this
reason. An engine registry that is empty - a host that explains before the
engines are registered - gets the two engines this framework ships, described
from the same declarations minimizer_registration makes.

LINKS stay inside this chapter, the window chapter and the fit-progress topics,
written as literals so no guide unit depends on another.
}
unit guide_fitting;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    FittingNamespace = 'fitting';
    FittingOverviewTopic = 'fitting/overview';
    AutomaticDecompositionTopic = 'fitting/automatically';
    HowManyCurvesTopic = 'fitting/how-many-curves';
    MinimizeNumberOfCurvesTopic = 'fitting/minimize-number-of-curves';
    MinimizeDifferenceTopic = 'fitting/minimize-difference';
    StopFitTopic = 'fitting/stop';
    FitCommandsGreyedTopic = 'fitting/why-fit-commands-are-greyed';
    RFactorTopic = 'fitting/r-factor';
    MaxAcceptableDifferenceTopic = 'fitting/max-acceptable-difference';
    CurveScalingTopic = 'fitting/curve-scaling';
    MinimizerTopic = 'fitting/minimizer';
    LossFunctionTopic = 'fitting/loss-function';
    WeightingTopic = 'fitting/weighting';
    FitAdviceTopic = 'fitting/what-the-fit-will-do';
    FitResultTopic = 'fitting/when-a-fit-ends';
    ComputeServerTopic = 'fitting/compute-server';
    ComputeBackendsTopic = 'fitting/compute-backends';
    PythonSetupTopic = 'fitting/python-setup';

{ Every topic of the chapter, in reading order. }
function FittingExplanations: TExplanations;

implementation

uses
    static_explanations, minimizer_registry, fit_loss;

const
    Bullet = #$E2#$80#$A2 + ' ';
    PlusMinus = #$C2#$B1;

    //  The two topics of fit_progress_explanations this chapter points to.
    LiveProgressTopic = 'fit-progress/live-progress';
    AnimationModeTopic = 'fit-progress/animation-mode';
    //  In the window chapter (guide_window), by literal.
    StatusBarTopic = 'the-window/status-bar';
    ToolsTabTopic = 'the-window/tools-tab';
    CurveAttributesTableTopic = 'the-window/curve-attributes-table';

function AsSentence(const AText: string): string;
begin
    Result := Trim(AText);
    if (Result <> '') and not (Result[Length(Result)] in ['.', '!', '?']) then
        Result := Result + '.';
end;

function FittingOverview: TExplanation;
begin
    Result := NewExplanation(FittingOverviewTopic, 'Fitting a model',
        'A fit adjusts the parameters of the model''s curves until the ' +
        'calculated profile matches the measured one as closely as it can.',
        esModelChoice);
    AddParagraph(Result, 'A fit needs a profile to fit and a curve type to ' +
        'fit it with. Everything else - where the curves start, which ' +
        'stretches of the profile are scored, the background - can be ' +
        'prepared by hand or left to the program, which fills in whatever is ' +
        'missing before it starts.');
    AddParagraph(Result, 'There are three ways to start a fit, all in the ' +
        'Fit menu:');
    AddParagraph(Result, Bullet + 'Fit > Automatically subtracts the ' +
        'background, places curves on the peaks, marks fit intervals and then ' +
        'removes every curve the data does not need. Use it for a first answer ' +
        'to "how many curves, and where?".');
    AddParagraph(Result, Bullet + 'Fit > Minimize Number of Curves (F4) runs ' +
        'the same removal on the curves and intervals you already have.');
    AddParagraph(Result, Bullet + 'Fit > Minimize Difference (F5) fits the ' +
        'model as it stands and removes nothing. The Fit button in the Fit ' +
        'group of the Tools tab runs the same command.');
    AddParagraph(Result, 'While a fit runs, the chart area shows its progress ' +
        'and every command that would change the model is greyed. Fit > Stop, ' +
        'or the Stop button of the Tools tab, ends it.');
    AddParagraph(Result, 'The fit itself runs in the compute server the ' +
        'window is connected to (Fit > Compute Server), not in the window. ' +
        'What it makes as small as possible is chosen under Fit > Loss ' +
        'Function, and the algorithm that does it under Fit > Minimizer.');
    AddLimitation(Result, 'A fit refines the curves from where they start. It ' +
        'finds the best match near that start, which is not always the best ' +
        'match there is: curves started far from the peaks can settle on a ' +
        'poorer answer.');
    AddRelated(Result, AutomaticDecompositionTopic);
    AddRelated(Result, MinimizeDifferenceTopic);
    AddRelated(Result, FitCommandsGreyedTopic);
    AddRelated(Result, RFactorTopic);
    AddRelated(Result, ToolsTabTopic);
    AddRelated(Result, LiveProgressTopic);
end;

function AutomaticDecomposition: TExplanation;
begin
    Result := NewExplanation(AutomaticDecompositionTopic,
        'Automatic decomposition',
        'Fit > Automatically decides how many curves the profile needs: it ' +
        'starts with far more than any profile needs and removes every one ' +
        'the fit can do without.', esModelChoice);
    AddParagraph(Result, 'Fit > Automatically - also the last button of the ' +
        'toolbar above the chart - runs five steps in the compute server:');
    AddParagraph(Result, Bullet + '1. It subtracts the background, unless the ' +
        'background has already been subtracted from this profile. The ' +
        'background points are proposed from the data; points you had picked ' +
        'are discarded.');
    AddParagraph(Result, Bullet + '2. It places a curve on every point of every ' +
        'peak. A peak is found by starting at the highest point not yet taken ' +
        'and walking down both of its flanks; the search stops at the first ' +
        'peak whose top is lower than a fraction of the highest point - 1/30 ' +
        'by default, set with Model > Background > Set Background Fraction. ' +
        'If you placed curve positions yourself, those are used instead.');
    AddParagraph(Result, Bullet + '3. It marks a fit interval around each ' +
        'peak, replacing any intervals you had marked.');
    AddParagraph(Result, Bullet + '4. In each fit interval separately, it fits ' +
        'the curves and then removes them one at a time for as long as the ' +
        'fit stays within the maximum acceptable difference (Fit > Set Max ' +
        'Acceptable Difference). Which curve goes, and when it stops, is the ' +
        'subject of "How many curves".');
    AddParagraph(Result, Bullet + '5. It fits the curves that are left to full ' +
        'precision.');
    AddParagraph(Result, 'Every curve is of the curve type selected on the ' +
        'Tools tab. When the run ends, the chart, the tables and the Model panel ' +
        'show the curves that remain. The removed curves are gone together with ' +
        'the positions they were placed from, so a later edit or fit does not ' +
        'bring them back.');
    AddParagraph(Result, 'The one setting that decides the answer is the ' +
        'maximum acceptable difference, 0.01 % by default. Ask for less ' +
        'accuracy and you get fewer, broader curves; ask for more and the ' +
        'program keeps the curves it needs to deliver it.');
    AddParagraph(Result, 'It is offered as soon as a profile is open, including ' +
        'before the background has been dealt with - which is exactly what ' +
        'its first step does.');
    AddLimitation(Result, 'The fit intervals and background points you had ' +
        'marked are discarded by steps 1 and 3; only the curve positions you ' +
        'picked are kept. Use Fit > Minimize Number of Curves to keep your ' +
        'intervals.');
    AddLimitation(Result, 'The background proposed in step 1 assumes a ' +
        'bowl-shaped background, lowest in the middle and rising to both ' +
        'sides. On a background of another shape, subtract it yourself first.');
    AddRelated(Result, HowManyCurvesTopic);
    AddRelated(Result, MaxAcceptableDifferenceTopic);
    AddRelated(Result, MinimizeNumberOfCurvesTopic);
    AddRelated(Result, LiveProgressTopic);
end;

function HowManyCurves: TExplanation;
begin
    Result := NewExplanation(HowManyCurvesTopic,
        'How many curves: the difference ceiling',
        'Fit decides how many curves a profile needs with a ceiling on the ' +
        'difference that you choose, rather than with an information ' +
        'criterion.', esModelChoice);
    AddParagraph(Result, 'With enough curves any profile can be fitted ' +
        'perfectly, noise included, so a least-squares fit alone cannot say ' +
        'how many curves there are. The usual answer weighs how well a model ' +
        'fits against how many parameters it has, with an information ' +
        'criterion such as AIC, AICc or BIC, and keeps whichever model scores ' +
        'best.');
    AddParagraph(Result, 'Fit answers differently. You state how good the fit ' +
        'has to be - the maximum acceptable difference, set with Fit > Set Max ' +
        'Acceptable Difference - and the program removes every curve it can ' +
        'remove without breaking that promise. The ceiling is not a penalty to ' +
        'calibrate; it is the accuracy your measurement deserves: its noise ' +
        'level, its counting statistics, the precision you are prepared to ' +
        'claim.');
    AddParagraph(Result, 'Each fit interval is decomposed on its own, in a ' +
        'loop of attempts that are each allowed to fail:');
    AddParagraph(Result, Bullet + 'Fit the curves, and keep a copy of their ' +
        'parameters: it is the best result so far.');
    AddParagraph(Result, Bullet + 'Drop every curve whose amplitude has fallen ' +
        'below a thousandth of the largest one in the interval. A curve with ' +
        'no amplitude left is not a component of the profile.');
    AddParagraph(Result, Bullet + 'Drop one more curve, chosen by the rule of ' +
        'the pass (below).');
    AddParagraph(Result, Bullet + 'If the difference is still within the ' +
        'ceiling, those curves were surplus: keep the smaller model and try ' +
        'again. If it is not, fit again without them.');
    AddParagraph(Result, Bullet + 'If that fit cannot bring the difference ' +
        'back within the ceiling, the last curve was needed: restore the saved ' +
        'parameters, put that curve back and end the pass. Curves dropped for ' +
        'having no amplitude stay gone.');
    AddParagraph(Result, 'The loop runs twice. The first pass removes the ' +
        'curve standing where the measured profile is steepest: on a flank, a ' +
        'curve is often doing the work of a shoulder that its neighbours can ' +
        'take over. The second pass removes the curve with the smallest ' +
        'amplitude, the classic candidate for being noise. A pass also ends ' +
        'when one curve is left. A final fit then polishes what survived.');
    AddParagraph(Result, 'A re-fit inside the loop only has to show that the ' +
        'ceiling can be met, so with the built-in engine it stops as soon as ' +
        'the difference is back under it, with a looser tolerance and no ' +
        'restarts. Only the final fit runs to full precision. That is what ' +
        'keeps the method quick although it fits once for every curve it ' +
        'removes: on the two-peak sample Data/2.dat, with two-branch ' +
        'Pseudo-Voigt curves and a ceiling of 0.01 %, it ends with a handful ' +
        'of curves in well under a minute.');
    AddParagraph(Result, 'Removing weak components, re-fitting and rolling ' +
        'back when the result is worse is not new in itself; GaussPy+ does so ' +
        'and keeps the smaller model only if its AICc improves. What is ' +
        'uncommon here is that the test is a fixed ceiling you choose rather ' +
        'than an improvement in an information criterion, and that one pass ' +
        'picks the curve on the steepest slope of the measured profile.');
    AddLimitation(Result, 'The passes run only while the difference is within ' +
        'the ceiling. If the first fit of an interval does not reach it, no ' +
        'curve is removed from that interval.');
    AddLimitation(Result, 'The difference compared with the ceiling is always ' +
        'the R-factor of the interval, whichever loss function is selected.');
    AddLimitation(Result, 'Curves are removed one at a time and a needed curve ' +
        'ends the pass, so the result is a small model that meets the ceiling, ' +
        'not necessarily the smallest one that could.');
    AddLimitation(Result, 'The steepest-slope pass measures the slope from a ' +
        'curve''s starting position to the next point of the profile, so it ' +
        'cannot remove a curve that starts on the profile''s last point.');
    AddReference(Result, 'Riener, M. et al., "GaussPy+: A fully automated ' +
        'Gaussian decomposition package for emission line spectra", ' +
        'Astronomy & Astrophysics 628, A78 (2019)', '',
        'https://doi.org/10.1051/0004-6361/201935519');
    AddReference(Result, 'Akaike, H., "A new look at the statistical model ' +
        'identification", IEEE Transactions on Automatic Control 19(6), ' +
        '716-723 (1974)', '', 'https://doi.org/10.1109/TAC.1974.1100705');
    AddReference(Result, 'Schwarz, G., "Estimating the dimension of a model", ' +
        'The Annals of Statistics 6(2), 461-464 (1978)', '',
        'https://doi.org/10.1214/aos/1176344136');
    AddRelated(Result, AutomaticDecompositionTopic);
    AddRelated(Result, MaxAcceptableDifferenceTopic);
    AddRelated(Result, RFactorTopic);
end;

function MinimizeNumberOfCurves: TExplanation;
begin
    Result := NewExplanation(MinimizeNumberOfCurvesTopic,
        'Minimize Number of Curves',
        'Removes every curve the fit can do without, starting from the curves ' +
        'and fit intervals you already have.', esModelChoice);
    AddParagraph(Result, 'Fit > Minimize Number of Curves (F4) runs steps 4 ' +
        'and 5 of Fit > Automatically - the removal passes and the final fit - ' +
        'on the model as it stands. It does not touch the background, and it ' +
        'keeps the positions and fit intervals you placed.');
    AddParagraph(Result, 'What it needs is filled in only where it is ' +
        'missing: with fewer than two interval bounds it marks a fit interval ' +
        'around each peak, and with no curve positions it places a curve on ' +
        'every point of every peak, as the automatic run does.');
    AddParagraph(Result, 'Use it when you have placed more curves than you ' +
        'think are needed and want the data to decide which of them stay, ' +
        'within the maximum acceptable difference (Fit > Set Max Acceptable ' +
        'Difference).');
    AddParagraph(Result, 'Curves it removes are gone together with their ' +
        'positions.');
    AddLimitation(Result, 'It is greyed until the background has been ' +
        'subtracted or you have started building the model; see "Why a fit ' +
        'command is greyed".');
    AddLimitation(Result, 'On a profile with many points, "a curve on every ' +
        'point of every peak" can be a great many curves, and the first fit ' +
        'then takes correspondingly long. Placing positions first avoids it.');
    AddRelated(Result, HowManyCurvesTopic);
    AddRelated(Result, AutomaticDecompositionTopic);
    AddRelated(Result, FitCommandsGreyedTopic);
end;

function MinimizeDifference: TExplanation;
begin
    Result := NewExplanation(MinimizeDifferenceTopic, 'Minimize Difference',
        'Fits the model as it stands: every curve is kept and only their ' +
        'parameters change.', esModelChoice);
    AddParagraph(Result, 'Fit > Minimize Difference (F5), and the Fit button in ' +
        'the Fit group of the Tools tab, adjust the parameters of every curve ' +
        'until the selected loss function can be made no smaller. No curve is ' +
        'added or removed, and the maximum acceptable difference plays no ' +
        'part.');
    AddParagraph(Result, 'Anything missing is filled in first, as for the ' +
        'other fit commands: a fit interval around each peak when fewer than ' +
        'two interval bounds are marked, and a curve on every point of every ' +
        'peak when there are no curve positions.');
    AddParagraph(Result, 'Each fit interval is fitted as a problem of its ' +
        'own. The difference reported afterwards pools all of them into one ' +
        'figure.');
    AddParagraph(Result, 'Use it after placing curves by hand, after changing ' +
        'a setting such as the loss function or the engine, or to fit again ' +
        'from where the last fit left off.');
    AddLimitation(Result, 'It is greyed until the background has been ' +
        'subtracted or you have started building the model; see "Why a fit ' +
        'command is greyed".');
    AddLimitation(Result, 'Without curve positions it seeds a curve on every ' +
        'point of every peak, which on a finely sampled profile is a great ' +
        'many curves. Place positions first, or use Fit > Automatically.');
    AddRelated(Result, FittingOverviewTopic);
    AddRelated(Result, LossFunctionTopic);
    AddRelated(Result, MinimizerTopic);
    AddRelated(Result, FitResultTopic);
end;

function StopFit: TExplanation;
begin
    Result := NewExplanation(StopFitTopic, 'Stop',
        'Ends the fit or other long computation that is running.',
        esModelChoice);
    AddParagraph(Result, 'Fit > Stop, and the Stop button in the Fit group of ' +
        'the Tools tab, are available only while a computation runs; the rest ' +
        'of the time they are greyed.');
    AddParagraph(Result, 'The engine is told to end, and does so at its next ' +
        'step. The window then shows what the fit had reached, exactly as it ' +
        'does when a fit finishes by itself: the curves, the tables and the ' +
        'statistics in the status bar are refreshed.');
    AddParagraph(Result, 'Stopping an automatic decomposition ends its passes ' +
        'where they are: curves already removed stay removed.');
    AddLimitation(Result, 'A stopped fit has not converged. Its numbers are ' +
        'the best found so far, not the best the model can do; run Fit > ' +
        'Minimize Difference to carry on from there.');
    AddRelated(Result, FitResultTopic);
    AddRelated(Result, LiveProgressTopic);
end;

function FitCommandsGreyed: TExplanation;
begin
    Result := NewExplanation(FitCommandsGreyedTopic,
        'Why a fit command is greyed',
        'The fit commands are offered only when there is something to fit, ' +
        'the background has been dealt with, and nothing else is running.',
        esModelChoice);
    AddParagraph(Result, 'The window decides which commands it offers from ' +
        'the state of the compute server. The rules, in the order they apply:');
    AddParagraph(Result, Bullet + 'Nothing open: every fit command is greyed ' +
        'until a profile is imported or a project is opened.');
    AddParagraph(Result, Bullet + 'A computation running: only Fit > Stop is ' +
        'offered. Every command that would change the model waits until it is ' +
        'over, so that nothing is changed under a fit.');
    AddParagraph(Result, Bullet + 'Background not yet dealt with: right after ' +
        'a profile is imported or reloaded, or a point of it is edited, Fit > ' +
        'Minimize Number of Curves, Fit > Minimize Difference and the Tools ' +
        'Fit button are greyed and Fit > Automatically is the fit on offer. ' +
        'Fitting curves to a profile that still carries its background is ' +
        'something to ask for deliberately, not to reach by the ordinary ' +
        'command. They become available once the background has been ' +
        'subtracted or you start building the model - placing curve ' +
        'positions or fit intervals.');
    AddParagraph(Result, Bullet + 'Nothing running: Fit > Stop is greyed.');
    AddParagraph(Result, 'Within the settings, single entries can be greyed ' +
        'with a reason in their hint, shown in the status bar:');
    AddParagraph(Result, Bullet + 'An engine under Fit > Minimizer that fits by ' +
        'evaluating a formula, when the selected curve type has none: "This ' +
        'curve type has no formula, so it is fitted by the native engine."');
    AddParagraph(Result, Bullet + 'An objective under Fit > Loss Function that ' +
        'cannot be used with the selected curve type, with the reason.');
    AddParagraph(Result, 'Fit > Set Max Acceptable Difference, Fit > Enable ' +
        'Curve Scaling and Fit > Compute Server are never greyed: they are ' +
        'settings, and a changed setting applies to the next fit.');
    AddLimitation(Result, 'The compute server itself would accept a manual ' +
        'fit before the background is subtracted and complete what is missing; ' +
        'it is the window that does not offer it. A program talking to the ' +
        'server directly is not held to this rule.');
    AddRelated(Result, FittingOverviewTopic);
    AddRelated(Result, AutomaticDecompositionTopic);
    AddRelated(Result, FitAdviceTopic);
end;

function RFactor: TExplanation;
begin
    Result := NewExplanation(RFactorTopic, 'The difference (R-factor)',
        'The number Fit reports for how well the model matches the data is an ' +
        'R-factor: the squared differences divided by the square of the ' +
        'summed data.', esModelChoice);
    AddParagraph(Result, 'Throughout the program "difference" and "R-factor" ' +
        'name the same number. Over the points inside the fit intervals it is');
    AddParagraph(Result, 'R = sum( (s * calc - obs)^2 ) / ( sum obs )^2');
    AddParagraph(Result, 'where obs is a measured value, calc the model''s value ' +
        'at the same point, and s the curve-scaling factor of the interval ' +
        '(1 when Fit > Enable Curve Scaling is off). With several fit ' +
        'intervals, the sums are pooled over all of them before dividing, so ' +
        'the figure means the same however the profile is divided up.');
    AddParagraph(Result, 'Dividing by the data makes it a relative measure: ' +
        'the same sample measured for ten times as long gives the same ' +
        'R-factor. A perfect fit gives 0. It is shown as a plain number, such ' +
        'as 7.7E-6 - not in percent - while Fit > Set Max Acceptable ' +
        'Difference asks for its ceiling in percent.');
    AddParagraph(Result, 'It is the default loss function, so it is usually ' +
        'also what the fit minimises. Whatever loss function is selected, the ' +
        'number reported and the number compared with the maximum acceptable ' +
        'difference is this R-factor.');
    AddParagraph(Result, 'While a fit runs, it is shown in the status bar and ' +
        'above the progress chart. When the fit ends, the status bar shows the ' +
        'reduced chi-squared and R-squared of the result in its place.');
    AddParagraph(Result, 'In diffraction the name R-factor usually means the ' +
        'profile R-factors of Rietveld refinement: Rp, the summed absolute ' +
        'differences over the summed data, and Rwp, the square root of the ' +
        'weighted squared differences over the weighted squared data. Fit''s ' +
        'number shares their intent - a difference relative to the data - but ' +
        'is neither: it is unweighted, squared, and has no square root, so it ' +
        'is not directly comparable with an Rp or Rwp quoted elsewhere.');
    AddLimitation(Result, 'Only the points inside the fit intervals count. ' +
        'Parts of the profile outside every interval can be fitted badly ' +
        'without changing the number.');
    AddReference(Result, 'McCusker, L. B., Von Dreele, R. B., Cox, D. E., ' +
        'Louer, D. and Scardi, P., "Rietveld refinement guidelines", Journal ' +
        'of Applied Crystallography 32, 36-50 (1999)', '',
        'https://doi.org/10.1107/S0021889898009856');
    AddRelated(Result, MaxAcceptableDifferenceTopic);
    AddRelated(Result, LossFunctionTopic);
    AddRelated(Result, CurveScalingTopic);
end;

function MaxAcceptableDifference: TExplanation;
begin
    Result := NewExplanation(MaxAcceptableDifferenceTopic,
        'Max Acceptable Difference',
        'The ceiling on the R-factor that decides how many curves the ' +
        'automatic decomposition keeps.', esModelChoice);
    AddParagraph(Result, 'Fit > Set Max Acceptable Difference opens the Max ' +
        'Acceptable Difference dialog, which has one field: "Max acceptable ' +
        'difference (percent):". Type the ceiling in percent, with a full stop ' +
        'as the decimal separator, and press OK. The default is 0.01, meaning ' +
        'an R-factor of 0.0001.');
    AddParagraph(Result, 'Anything that is not a number is refused with ' +
        '"Please enter a valid number (for example, 1.5)." and the dialog ' +
        'stays open. Cancel leaves the ceiling as it was.');
    AddParagraph(Result, 'The ceiling is used by Fit > Automatically and Fit ' +
        '> Minimize Number of Curves: a curve is removed only if the fit ' +
        'interval it belongs to still meets the ceiling without it. Fit > ' +
        'Minimize Difference does not use it.');
    AddParagraph(Result, 'Choose it near the level the noise of your data ' +
        'allows. Too low, and noise is fitted with extra curves; too high, and ' +
        'real components are merged away.');
    AddParagraph(Result, 'It is saved in the project file, so a project ' +
        'reopens with the ceiling it was fitted with.');
    AddLimitation(Result, 'The ceiling is compared with the R-factor, ' +
        'whichever loss function is selected, and with the R-factor of each ' +
        'fit interval on its own rather than of the whole profile.');
    AddRelated(Result, HowManyCurvesTopic);
    AddRelated(Result, RFactorTopic);
    AddRelated(Result, AutomaticDecompositionTopic);
end;

function CurveScaling: TExplanation;
begin
    Result := NewExplanation(CurveScalingTopic, 'Curve scaling',
        'With curve scaling on, the model is multiplied by one factor per fit ' +
        'interval so that its area matches the data before the two are ' +
        'compared.', esModelChoice);
    AddParagraph(Result, 'Fit > Enable Curve Scaling is a tick, and it is on ' +
        'by default. While it applies, the built-in engine compares s ' +
        'times the model with the data, where s is the sum of the measured ' +
        'values in the fit interval divided by the sum of the model''s. The ' +
        'curves then only have to get the shape right, and the factor takes ' +
        'care of the overall height, which helps the fit converge.');
    AddParagraph(Result, 'It does not apply in two cases, whatever the tick ' +
        'says:');
    AddParagraph(Result, Bullet + 'With the Python engine selected: it fits ' +
        'the amplitudes itself, so scaling afterwards would rescale values ' +
        'already fitted.');
    AddParagraph(Result, Bullet + 'With a curve type that sets its own ' +
        'amplitude, free to grow. A second, overall multiplier would let the ' +
        'fit flatten the shape while the multiplier absorbs the difference. ' +
        'Hovering over the status bar explains it for this case.');
    AddParagraph(Result, 'The setting is saved in the project file.');
    AddLimitation(Result, 'Combined with the R-factor (legacy) loss function, ' +
        'scaling lets a fit lower the number by inflating the model rather ' +
        'than by matching the data; that combination is refused for curve ' +
        'types whose amplitude is free.');
    AddRelated(Result, RFactorTopic);
    AddRelated(Result, LossFunctionTopic);
    AddRelated(Result, FitAdviceTopic);
end;

{ The engines to describe: what is registered, or - for a host that explains
  before registering - the two this framework ships, declared as
  minimizer_registration declares them. }
function EnginesToDescribe: TMinimizerInfoArray;
begin
    Result := RegisteredMinimizers;
    if Length(Result) > 0 then
        Exit;
    SetLength(Result, 2);
    Result[0] := Default(TMinimizerInfo);
    Result[0].Name := 'Downhill Simplex (native)';
    Result[0].Description := 'The original algorithm. Needs no Python and fits ' +
        'any curve type, including those with no formula.';
    Result[0].SupportsCurveScaling := True;
    Result[1] := Default(TMinimizerInfo);
    Result[1].Name := 'Levenberg-Marquardt (Python/lmfit)';
    Result[1].Description := 'Trust-region least squares with uncertainties. ' +
        'Needs the Python sidecar, and a curve type that has a formula.';
    Result[1].NeedsFormula := True;
    Result[1].NeedsPythonSidecar := True;
    Result[1].SupportsWeighting := True;
end;

function Minimizer: TExplanation;
var
    Engines: TMinimizerInfoArray;
    Line: string;
    i: longint;
begin
    Result := NewExplanation(MinimizerTopic, 'Minimizer',
        'Fit > Minimizer chooses the algorithm that searches for the best ' +
        'parameters; the first one, the default, needs nothing extra.',
        esModelChoice);
    AddParagraph(Result, 'The entries of Fit > Minimizer are the engines this ' +
        'build offers, in order. The first is the default, and is also what a ' +
        'saved choice falls back to when it names an engine this build does ' +
        'not have. The choice is remembered between sessions.');
    Engines := EnginesToDescribe;
    for i := 0 to High(Engines) do
    begin
        Line := Bullet + 'Fit > Minimizer > ' + Engines[i].Name;
        if i = 0 then
            Line := Line + ' (the default)';
        Line := Line + ': ' + AsSentence(Engines[i].Description);
        if Engines[i].NeedsPythonSidecar then
            Line := Line + ' It runs in a Python process the compute server ' +
                'starts on first use.';
        if Engines[i].SupportsWeighting then
            Line := Line + ' While it is selected, Fit > Weighting is shown.';
        if not Engines[i].SupportsCurveScaling then
            Line := Line + ' Curve scaling does not apply to it.';
        AddParagraph(Result, Line);
    end;
    AddParagraph(Result, 'Downhill Simplex (native) is the Nelder-Mead simplex ' +
        'method. It needs only the value of the loss function, not its ' +
        'derivatives, so it fits any curve type, including those that compute ' +
        'their points without a formula. It gives no uncertainties for the ' +
        'parameters.');
    AddParagraph(Result, 'Levenberg-Marquardt (Python/lmfit) uses lmfit''s ' +
        'least_squares method - despite the name, scipy''s Trust Region ' +
        'Reflective solver. It reports an uncertainty for each fitted ' +
        'parameter, shown after the value in the Curve Attributes table as ' +
        '"value ' + PlusMinus + ' error".');
    AddParagraph(Result, 'An engine that fits by evaluating a formula is greyed ' +
        'while the selected curve type has none, and if it was selected, the ' +
        'default engine is selected instead. An engine that can only minimise a ' +
        'sum of squares leaves a loss function that is not one to the built-in ' +
        'engine; a dialog and the status bar say so.');
    AddParagraph(Result, 'Without Python: the Python engine needs Python and ' +
        'three libraries set up on the machine that runs the compute server ' +
        '(Help > Compute Backends says how). If they are missing, a fit with ' +
        'that engine does not start: the server answers that the Python ' +
        'backend is not available. Nothing switches engines behind your back; ' +
        'choose Downhill Simplex (native), which needs no Python, to fit.');
    AddLimitation(Result, 'Neither engine searches globally. Both refine the ' +
        'curves from where they start, and can stop at a poorer answer when ' +
        'the start is far from a good one.');
    AddReference(Result, 'Nelder, J. A. and Mead, R., "A simplex method for ' +
        'function minimization", The Computer Journal 7(4), 308-313 (1965)',
        '', 'https://doi.org/10.1093/comjnl/7.4.308');
    AddReference(Result, 'Newville, M. et al., "LMFIT: Non-Linear ' +
        'Least-Squares Minimization and Curve-Fitting for Python"', '',
        'https://doi.org/10.5281/zenodo.11813');
    AddReference(Result, 'Branch, M. A., Coleman, T. F. and Li, Y., "A ' +
        'subspace, interior, and conjugate gradient method for large-scale ' +
        'bound-constrained minimization problems", SIAM Journal on Scientific ' +
        'Computing 21(1), 1-23 (1999)', '', '');
    AddRelated(Result, ComputeBackendsTopic);
    AddRelated(Result, WeightingTopic);
    AddRelated(Result, FitAdviceTopic);
    AddRelated(Result, CurveAttributesTableTopic);
end;

function LossFunction: TExplanation;
var
    Losses: TLossInfoArray;
    Line: string;
    i: longint;
begin
    Result := NewExplanation(LossFunctionTopic, 'Loss Function',
        'Fit > Loss Function chooses the number a fit makes as small as ' +
        'possible; the default, R-factor, suits almost every fit.',
        esModelChoice);
    AddParagraph(Result, 'Fit > Loss Function is the last entry of the Fit ' +
        'menu. Its entries are the objectives this build offers, the default ' +
        'first; the choice is remembered between sessions. Each entry''s hint ' +
        'says what it measures:');
    Losses := RegisteredLosses;
    for i := 0 to High(Losses) do
    begin
        Line := Bullet + 'Fit > Loss Function > ' + Losses[i].Name;
        if i = 0 then
            Line := Line + ' (the default)';
        Line := Line + ': ' + AsSentence(Losses[i].Description);
        if not Losses[i].IsLeastSquares then
            Line := Line + ' It is not a sum of squares, so a fit with the ' +
                'Python engine runs on the built-in engine instead.';
        if Losses[i].IsSelfNormalising then
            Line := Line + ' It is greyed for a curve type whose amplitude is ' +
                'free to grow.';
        AddParagraph(Result, Line);
    end;
    AddParagraph(Result, 'R-factor and Sum of squares differ only by a ' +
        'constant factor, so they find the same parameters; only the number ' +
        'differs. Relative deviation sums absolute rather than squared ' +
        'differences, so a few large misfits count for less. R-factor ' +
        '(legacy) divides by the model instead of the data: with curve scaling ' +
        'on, a fit can lower it by inflating the model without matching the ' +
        'data any better, which is why it is greyed for a curve type whose ' +
        'amplitude is free, and replaced by R-factor if it was selected.');
    AddParagraph(Result, 'Whatever is chosen, the difference the program ' +
        'reports, and compares with the maximum acceptable difference, is the ' +
        'R-factor. The statistics shown after a fit do not depend on the ' +
        'choice either; only the fitted parameters do.');
    AddLimitation(Result, 'Every objective scores only the points inside the ' +
        'fit intervals.');
    AddRelated(Result, RFactorTopic);
    AddRelated(Result, FitAdviceTopic);
    AddRelated(Result, CurveScalingTopic);
end;

function Weighting: TExplanation;
begin
    Result := NewExplanation(WeightingTopic, 'Weighting',
        'Weighting decides how much each point counts in a fit by the Python ' +
        'engine; counting data are conventionally weighted by one over the ' +
        'square root of the count.', esConvention);
    AddParagraph(Result, 'The Fit > Weighting submenu is shown only while an ' +
        'engine that can be weighted is selected under Fit > Minimizer - the ' +
        'Python one. The built-in engine always fits unweighted, so the menu ' +
        'is hidden under it. It has two entries:');
    AddParagraph(Result, Bullet + 'Fit > Weighting > Poisson (counting ' +
        'statistics), the default: each difference is divided by the square ' +
        'root of the measured value, with values below 1 taken as 1. This is ' +
        'the usual choice for counts - of photons, neutrons, electrons - whose ' +
        'statistical error is the square root of the count, so a faint feature ' +
        'counts for as much as its precision allows.');
    AddParagraph(Result, Bullet + 'Fit > Weighting > None (unweighted): every ' +
        'point counts equally, the same objective the built-in engine ' +
        'minimises. Use it for data that are not counts, or to compare the ' +
        'Python result term for term with the built-in one.');
    AddParagraph(Result, 'The choice is remembered between sessions. The ' +
        'reduced chi-squared shown in the status bar after any fit uses the ' +
        'Poisson weights, whichever engine ran.');
    AddLimitation(Result, 'Poisson weighting assumes the values are counts. ' +
        'Values that have been rescaled, normalised or had a background ' +
        'subtracted no longer have the square root of the value as their ' +
        'error, and the weights are then only approximate.');
    AddReference(Result, 'McCusker, L. B., Von Dreele, R. B., Cox, D. E., ' +
        'Louer, D. and Scardi, P., "Rietveld refinement guidelines", Journal ' +
        'of Applied Crystallography 32, 36-50 (1999)', '',
        'https://doi.org/10.1107/S0021889898009856');
    AddRelated(Result, MinimizerTopic);
    AddRelated(Result, ComputeBackendsTopic);
end;

function FitAdvice: TExplanation;
begin
    Result := NewExplanation(FitAdviceTopic, 'What the fit will actually do',
        'When a choice cannot be honoured, the fit changes it and says so: ' +
        'always in the status bar, and in a dialog when you make the choice.',
        esModelChoice);
    AddParagraph(Result, 'The right-hand end of the status bar always says ' +
        'what the next fit will do - for example "Minimising R-factor." or ' +
        '"Fitting with the built-in engine, minimising Relative deviation." ' +
        'When something was changed, hovering over the status bar shows why.');
    AddParagraph(Result, 'Three things can be changed. The compute server ' +
        'decides with the same rule the status bar reports, so the two cannot ' +
        'disagree:');
    AddParagraph(Result, Bullet + 'The loss function: R-factor (legacy) is ' +
        'replaced by R-factor for a curve type whose amplitude is free to ' +
        'grow.');
    AddParagraph(Result, Bullet + 'The engine: with the Python engine selected, ' +
        'the fit runs on the built-in engine when the curve type has no ' +
        'formula, or when the loss function is not a sum of squares. Your ' +
        'objective is kept; what you lose is the per-parameter uncertainties.');
    AddParagraph(Result, Bullet + 'Curve scaling: switched off for a curve type ' +
        'that sets its own amplitude.');
    AddParagraph(Result, 'When you change the engine, the loss function or ' +
        'the curve type and the result would be changed, a dialog titled ' +
        '"About this fit" explains it. It is not repeated while you stay with ' +
        'that selection, and not shown at start-up for a setting chosen ' +
        'earlier. Curve scaling being switched off never opens the dialog; ' +
        'hovering over the status bar explains it.');
    AddRelated(Result, LossFunctionTopic);
    AddRelated(Result, MinimizerTopic);
    AddRelated(Result, CurveScalingTopic);
    AddRelated(Result, StatusBarTopic);
end;

function FitResult: TExplanation;
begin
    Result := NewExplanation(FitResultTopic, 'When a fit ends',
        'The chart, the tables and the Model panel are refreshed from the ' +
        'result, and the status bar shows how long the fit took and how good ' +
        'it is.', esModelChoice);
    AddParagraph(Result, 'While a fit runs, the chart area shows its progress ' +
        '- or, with View > Animation Mode ticked, the model moving - and the ' +
        'tables keep the last finished result.');
    AddParagraph(Result, 'When it ends, whether it finished or was stopped, ' +
        'the window:');
    AddParagraph(Result, Bullet + 'draws the profile again with the model over ' +
        'it: each curve, their sum (Total Amplitude) and the Difference between ' +
        'data and model;');
    AddParagraph(Result, Bullet + 'draws the curve positions you picked and the ' +
        'fitted positions where the fit put the curves, and the fit interval ' +
        'bounds;');
    AddParagraph(Result, Bullet + 'refills the Curve Positions and Fit Intervals ' +
        'tables, the Curve Attributes table with every curve''s parameters, and ' +
        'the Summary table with the data, the model and each curve at every ' +
        'point of every interval;');
    AddParagraph(Result, Bullet + 'rebuilds the list of curves in the Model ' +
        'panel;');
    AddParagraph(Result, Bullet + 'shows in the status bar the elapsed time and, ' +
        'when they can be computed, the reduced chi-squared and R-squared of ' +
        'the fit.');
    AddParagraph(Result, 'The elapsed time counts only the time the computer ' +
        'was awake. A fit left running while the machine slept or hibernated ' +
        'does not count the sleep, and neither does the time axis of the ' +
        'progress chart.');
    AddParagraph(Result, 'The result is part of the project from then on, and ' +
        'is kept on disk when the project is saved.');
    AddLimitation(Result, 'The R-factor itself is on screen only while the fit ' +
        'runs; when it ends, the status bar shows the reduced chi-squared and ' +
        'R-squared in its place.');
    AddRelated(Result, LiveProgressTopic);
    AddRelated(Result, AnimationModeTopic);
    AddRelated(Result, StatusBarTopic);
    AddRelated(Result, RFactorTopic);
end;

function ComputeServer: TExplanation;
begin
    Result := NewExplanation(ComputeServerTopic, 'Compute Server',
        'The window does no fitting itself: every computation runs in a ' +
        'compute server, fit_server, and Fit > Compute Server says where it ' +
        'is.', esModelChoice);
    AddParagraph(Result, 'Fit > Compute Server opens a box titled Compute ' +
        'Server that asks for the "Server URL (fit_server must be running ' +
        'there):". It offers the address in use, http://127.0.0.1:8787 - a ' +
        'server on this computer - unless another has been set. Leaving it ' +
        'empty also means that default.');
    AddParagraph(Result, 'On Windows, the launcher the installed shortcut ' +
        'starts makes sure a server is answering at the default address before ' +
        'it opens the window.');
    AddParagraph(Result, 'Enter the address of a fit_server running elsewhere ' +
        '- a machine with more cores, say - to fit there. After OK the window ' +
        'checks at once whether anything answers. If nothing does, it says ' +
        '"No server answered at" the address, and "Start fit_server there - ' +
        'the application cannot fit without it." If a server answers and a ' +
        'profile is open, the profile is sent to it.');
    AddParagraph(Result, 'The address is remembered between sessions. A ' +
        'running fit_server describes its own interface: open the address ' +
        'followed by /docs in a web browser.');
    AddLimitation(Result, 'Only the profile is sent to the new server. The ' +
        'curves, positions, intervals and background points held by the old ' +
        'one are not; save the project before switching and open it again ' +
        'afterwards to take the whole model across.');
    AddRelated(Result, ComputeBackendsTopic);
    AddRelated(Result, MinimizerTopic);
end;

function ComputeBackends: TExplanation;
begin
    Result := NewExplanation(ComputeBackendsTopic, 'Compute backends',
        'Help > Compute Backends summarises the engines a fit can run on and ' +
        'how to set up the optional Python one.', esModelChoice);
    AddParagraph(Result, 'Fitting runs in the compute server the window is ' +
        'connected to (Fit > Compute Server). Behind it there are two engines, ' +
        'chosen under Fit > Minimizer:');
    AddParagraph(Result, Bullet + 'The built-in engine, Downhill Simplex ' +
        '(native), is part of fit_server. It needs nothing extra and fits every ' +
        'curve type.');
    AddParagraph(Result, Bullet + 'The Python engine, Levenberg-Marquardt ' +
        '(Python/lmfit), adds an uncertainty for each fitted parameter. It ' +
        'needs Python 3 with numpy, scipy and lmfit at pinned versions, ' +
        'installed in a separate environment so that your own Python is left ' +
        'alone.');
    AddParagraph(Result, 'fit_server looks for that environment in a folder ' +
        'named sidecar under %LOCALAPPDATA%\Fit\py on Windows and ' +
        '~/.local/share/fit/py elsewhere ($XDG_DATA_HOME replaces ' +
        '~/.local/share when it is set), or under the folder named by the ' +
        'environment variable FIT_PY_HOME when that is set. The libraries and ' +
        'their versions are listed in Worker/py/requirements.txt.');
    AddParagraph(Result, 'You never start the Python engine yourself: ' +
        'fit_server starts it the first time a fit asks for it and stops it ' +
        'when it exits. The window only ever talks to fit_server.');
    AddParagraph(Result, 'A third arrangement needs no engine choice at all: ' +
        'the same fit_server on another machine, reached through Fit > ' +
        'Compute Server.');
    AddLimitation(Result, 'The box Help > Compute Backends opens is a short ' +
        'summary; it names the setup steps rather than performing them.');
    AddRelated(Result, PythonSetupTopic);
    AddRelated(Result, MinimizerTopic);
    AddRelated(Result, ComputeServerTopic);
    AddRelated(Result, WeightingTopic);
end;

function PythonSetup: TExplanation;
begin
    //  THE ONE PLACE these steps are written. The server's refusal when the
    //  Python component is missing names this topic, and the build guide
    //  points here rather than keeping a copy of its own.
    Result := NewExplanation(PythonSetupTopic, 'Setting up the Python engine',
        'The Python engine needs a one-time setup: a private Python environment ' +
        'with numpy, scipy and lmfit at the tested versions.', esModelChoice);
    AddParagraph(Result, 'You need Python 3.10 to 3.13 and the list of pinned ' +
        'libraries, Worker/py/requirements.txt, from the source repository ' +
        '(https://github.com/dvmorozov/fit). The libraries go into a separate ' +
        'virtual environment at exactly those versions, so your own Python is ' +
        'not touched and fitted numbers reproduce.');
    AddParagraph(Result, 'On Linux and macOS, from the folder holding the ' +
        'repository:');
    AddParagraph(Result, Bullet + 'python3 -m venv ~/.local/share/fit/py/sidecar');
    AddParagraph(Result, Bullet + '~/.local/share/fit/py/sidecar/bin/python -m pip ' +
        'install --only-binary=:all: -r Worker/py/requirements.txt');
    AddParagraph(Result, 'On Windows, in PowerShell:');
    AddParagraph(Result, Bullet + 'py -3.12 -m venv "$env:LOCALAPPDATA\Fit\py\sidecar"');
    AddParagraph(Result, Bullet + '& "$env:LOCALAPPDATA\Fit\py\sidecar\Scripts\' +
        'python.exe" -m pip install --only-binary=:all: -r ' +
        'Worker/py/requirements.txt');
    AddParagraph(Result, '--only-binary=:all: stops pip from compiling numpy or ' +
        'scipy from source when your Python is newer than the pinned versions ' +
        'have ready-made packages for; it fails instead, and a Python in the ' +
        'range above is the fix.');
    AddParagraph(Result, 'The environment belongs to the machine, not to a copy ' +
        'of the program: every installation and every checkout on the machine ' +
        'shares it. Set the environment variable FIT_PY_HOME to keep it ' +
        'elsewhere; it then goes in the sidecar folder under that path.');
    AddParagraph(Result, 'To check it, start the Python engine by hand with ' +
        'sidecar/bin/python Worker/py/fit_backend.py --port 8788 (on Windows, ' +
        'sidecar\Scripts\python.exe) and open http://127.0.0.1:8788/health in ' +
        'a browser: it answers {"ok": true, ...}. Stop it with Ctrl+C - ' +
        'fit_server starts its own when a fit needs it.');
    AddLimitation(Result, 'On Debian and Ubuntu, creating the environment ' +
        'needs the separate python3-venv package; without it venv fails on ' +
        'ensurepip. Delete the half-made folder before trying again.');
    AddLimitation(Result, 'It is deliberately not installed system-wide: ' +
        'Debian, Fedora and Homebrew refuse such an install (PEP 668), and the ' +
        'next unrelated pip install into a shared Python would change the ' +
        'versions the results depend on.');
    AddRelated(Result, ComputeBackendsTopic);
    AddRelated(Result, MinimizerTopic);
end;

function FittingExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, FittingOverview);
    AppendExplanation(Result, AutomaticDecomposition);
    AppendExplanation(Result, HowManyCurves);
    AppendExplanation(Result, MinimizeNumberOfCurves);
    AppendExplanation(Result, MinimizeDifference);
    AppendExplanation(Result, StopFit);
    AppendExplanation(Result, FitCommandsGreyed);
    AppendExplanation(Result, RFactor);
    AppendExplanation(Result, MaxAcceptableDifference);
    AppendExplanation(Result, CurveScaling);
    AppendExplanation(Result, Minimizer);
    AppendExplanation(Result, LossFunction);
    AppendExplanation(Result, Weighting);
    AppendExplanation(Result, FitAdvice);
    AppendExplanation(Result, FitResult);
    AppendExplanation(Result, ComputeServer);
    AppendExplanation(Result, ComputeBackends);
    AppendExplanation(Result, PythonSetup);
end;

end.
