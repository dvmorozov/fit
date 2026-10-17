// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide's chapter on the model: the curve type, the curve
positions, the fit intervals, the tables and panels that show the model,
deleting and clearing curves.)

WHY IT IS ONE CHAPTER. The Model menu, the Tools tab and the tables at the
bottom of the window are three views of one thing - what is fitted to the
profile - and a user building a model moves between all three. The code is
spread wider still: form_main, ui_commands, action_state, pick_guidance,
pick_target, model_clearing, model_outline, curve_type_menu, curve_list_grid,
parameter_kinds, parameter_roles, formula_editing, the user-curve dialogs and
the engine's fit_service, whose automatic searches decide what Compute
Automatically actually finds.

WHAT THE MENU DOES NOT SAY IS SAID HERE. A user-defined curve's parameters can be shared but not frozen;
there is no Model > Delete Curve entry. Each of these is what a user would
otherwise find out by being surprised. When the code changes, the sentence here
that describes it has to change with it.

THE BACKGROUND IS NOT HERE. It is the scientific module's, and so is its
chapter (Modules/scientific/scientific_guide.pas): a product without the module
has no Background menu, and this chapter must not describe one. Nothing here
links to it - a link to a module's topic links nowhere where the module is left
out.

CURVE TYPES ARE NOT REPEATED. Each explains itself through
curve_type_explanations; this chapter says only how to choose one and where its
explanation is shown.
}
unit guide_model;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    ModelNamespace = 'model';
    ModelTopic = 'model/the-model';
    CurveTypeTopic = 'model/curve-type';
    ToolsPaneTopic = 'model/tools-pane';
    PickingTopic = 'model/picking-on-the-chart';
    CurvePositionsTopic = 'model/curve-positions';
    FitIntervalsTopic = 'model/fit-intervals';
    ModelPanelTopic = 'model/model-panel';
    CurveAttributesTopic = 'model/curve-attributes';
    ParameterKindsTopic = 'model/parameter-kinds';
    ParameterLimitsTopic = 'model/parameter-limits';
    DeleteCurveTopic = 'model/delete-curve';
    ClearModelTopic = 'model/clear-model';
    ModelHistoryTopic = 'model/model-history';

{ Every topic of the chapter, in reading order. }
function ModelExplanations: TExplanations;

implementation

uses
    static_explanations;   // AppendExplanation

const
    //  A paragraph that starts with U+2022 and a space is a bullet.
    Bullet = #$E2#$80#$A2 + ' ';

    //  Topics of other chapters, written out rather than taken from their
    //  units: a chapter does not use another chapter's unit.
    ProfileTopic = 'data/the-profile';
    ArgumentAxesTopic = 'data/argument-axes';
    DataIntervalTopic = 'data/data-interval';
    ReloadProfileTopic = 'projects-and-files/reload-profile';
    HistoryTabTopic = 'the-window/history-tab';
    ProjectContentsTopic = 'projects-and-files/project-contents';

function Model: TExplanation;
begin
    Result := NewExplanation(ModelTopic, 'The model',
        'The model is what the program fits to the profile: curves of a chosen ' +
        'shape, placed at positions and fitted over intervals.',
        esModelChoice);
    AddParagraph(Result, 'It is built from three parts:');
    AddParagraph(Result, Bullet + 'The curve type - the shape of every curve ' +
        'placed from a position, chosen from Model > Curve Type or the list at ' +
        'the top of the Tools tab.');
    AddParagraph(Result, Bullet + 'The curve positions - one per curve, the ' +
        'argument where that curve is placed. They are listed in the Curve ' +
        'Positions table at the bottom of the window.');
    AddParagraph(Result, Bullet + 'The fit intervals - stretches of the ' +
        'profile, each fitted as a problem of its own, marked by pairs of ' +
        'bounds and listed in the Fit Intervals table.');
    AddParagraph(Result, 'A module included in the build may add parts of its ' +
        'own - a background under the curves, for one - and its chapter of ' +
        'this guide says what they are.');
    AddParagraph(Result, 'Once the model has at least one curve position and at ' +
        'least one fit interval bound, the program builds a curve at every ' +
        'position and draws it, with the computed profile and its difference ' +
        'from the data, before any fit. The curves are listed in the Model tab ' +
        'on the right and their parameters in the Curve Attributes table.');
    AddParagraph(Result, 'Each part can be picked by hand on the chart or ' +
        'proposed by the program - the Compute Automatically entries of the ' +
        'Model menu and the Auto buttons of the Tools tab. A fit also proposes ' +
        'what is missing: Fit > Minimize Difference computes the fit intervals ' +
        'when fewer than two bounds are set, and the curve positions when there ' +
        'are none.');
    AddParagraph(Result, 'The curve positions and the fit intervals are saved ' +
        'with the project, together with the settings that shape them.');
    AddLimitation(Result, 'Every curve placed from a position has the selected ' +
        'curve type. Choosing another type changes the shape of all of them, and ' +
        'the values an earlier fit found for the old shape are not carried over.');
    AddRelated(Result, CurveTypeTopic);
    AddRelated(Result, CurvePositionsTopic);
    AddRelated(Result, FitIntervalsTopic);
    AddRelated(Result, ToolsPaneTopic);
    AddRelated(Result, ProfileTopic);
end;

function CurveType: TExplanation;
begin
    Result := NewExplanation(CurveTypeTopic, 'Choosing a curve type',
        'The curve type is the shape every curve placed from a position takes, ' +
        'and is chosen from Model > Curve Type or from the list at the top of ' +
        'the Tools tab.', esModelChoice);
    AddParagraph(Result, 'Model > Curve Type opens a submenu of groups, one for ' +
        'each module included in the build that brings curve types - its ' +
        'chapter of this guide says what they are. The type in use is ticked.');
    AddParagraph(Result, 'The same types are listed at the top of the Tools tab ' +
        'on the left, under the same group names. Click a type to select it; ' +
        'clicking a group name selects the first type under it.');
    AddParagraph(Result, 'Every curve type explains itself. Rest the pointer on ' +
        'a type in the Tools list to read a one-line summary of it. When you ' +
        'select a type, the Explain pane under the Model tab on the right shows ' +
        'its full explanation - what its formula means, what each parameter ' +
        'controls, where the shape is used and what it does not cover - and ' +
        'every type can also be found under Help > Explain Everything.');
    AddParagraph(Result, 'A type can be chosen before any data is loaded. While ' +
        'the axis rules are Automatic and the model is empty, choosing a type ' +
        'also sets the axes, when the data does not say what they are.');
    AddParagraph(Result, 'A model is made of the curve types of ONE module. ' +
        'Standard is one; a curve pack included in the build is another. While ' +
        'the model is empty any type can be chosen - that is how a model gets ' +
        'its module. Once it holds a pick or a curve, the types of every other ' +
        'module stay listed but greyed, in the menu and in the Tools list, and ' +
        'choosing one says why: each module covers its own kind of task, and ' +
        'curves of two of them fit nothing either understands. To change ' +
        'module, start a new project with File > New Project, or empty the ' +
        'model with Model > Clear Model.');
    AddLimitation(Result, 'This is this software''s choice, not a rule of any ' +
        'field: one module per model keeps every model one that some module ' +
        'fully understands - its rules, its report and its fit intervals.');
    AddRelated(Result, ModelTopic);
    AddRelated(Result, ArgumentAxesTopic);
end;

function ToolsPane: TExplanation;
begin
    Result := NewExplanation(ToolsPaneTopic, 'Building the model from the Tools tab',
        'The Tools tab on the left carries the model-building commands as ' +
        'buttons, so a round of picking, fitting and adjusting needs no trip ' +
        'through the menus.', esModelChoice);
    AddParagraph(Result, 'At the top is the list of curve types. Below it the ' +
        'buttons are grouped in the order the work is done:');
    AddParagraph(Result, Bullet + 'Positions: Pick, Auto and Clear run Model > ' +
        'Curve Positions > Start Manual Selection, Compute Automatically and ' +
        'Remove All.');
    AddParagraph(Result, Bullet + 'Fit intervals: Pick, Auto and Clear run ' +
        'Model > Fit Intervals > Start Manual Selection, Compute Automatically ' +
        'and Remove All.');
    AddParagraph(Result, Bullet + 'Fit: Fit and Stop run Fit > Minimize ' +
        'Difference and Fit > Stop.');
    AddParagraph(Result, 'A button runs exactly the command of its menu entry, ' +
        'shows the same hint, and is greyed whenever that entry is. A Pick button ' +
        'stays pressed while its picking mode runs; pressing it again ends the ' +
        'mode.');
    AddParagraph(Result, 'Each heading counts what the model holds - Positions ' +
        '(3) means three curve positions. A zero is shown rather than hidden.');
    AddParagraph(Result, 'A curve pack included in the build may add buttons of ' +
        'its own, under its own heading. The bar between the list and the ' +
        'buttons can be dragged to give either more room.');
    AddLimitation(Result, 'The buttons carry the commands used most: the rest ' +
        'of a submenu - Model > Curve Positions > At Every Point, for one - is ' +
        'reached through the menu.');
    AddRelated(Result, CurveTypeTopic);
    AddRelated(Result, PickingTopic);
    AddRelated(Result, ModelTopic);
end;

function Picking: TExplanation;
begin
    Result := NewExplanation(PickingTopic, 'Picking points on the chart',
        'In a picking mode each left click on the chart adds the data point ' +
        'nearest the pointer to the set being built, and a click on a point ' +
        'already in the set takes it out.', esModelChoice);
    AddParagraph(Result, 'Several commands start a picking mode: the Start ' +
        'Manual Selection entries for curve positions, fit intervals, the data ' +
        'range and any other set a module picks, and the Pick buttons of the ' +
        'Tools tab. ' +
        'Only one mode runs at a time: starting another ends the first, and ' +
        'choosing the same entry or button again ends it.');
    AddParagraph(Result, 'The entries for curve positions and fit intervals ' +
        'say whether their mode is on. They read Start Visual ' +
        'Position Selection or Start Visual Selection, and Stop Visual Position ' +
        'Selection or Stop Visual Selection while it runs; the matching Pick ' +
        'button stays pressed. The other picking entries are ticked instead.');
    AddParagraph(Result, 'A pick is made with the left button, and only if the ' +
        'mouse does not move between press and release; a drag zooms the chart ' +
        'instead. The pick goes to the data point nearest the pointer, measured ' +
        'on the screen in both directions, so every pick is a real point of the ' +
        'profile. A click that lands on a curve or another series drawn over the ' +
        'data is taken as a click on the data beneath it.');
    AddParagraph(Result, 'Clicking a point already in the set removes it. For ' +
        'curve positions this also removes the curve placed there. In a mode ' +
        'that takes a set number of picks - two ends of a range, three points of ' +
        'a peak - clicks after the last one are ignored.');
    AddParagraph(Result, 'The right button never picks: it opens the chart''s ' +
        'menu of axis rules. The status line at the bottom of the window says ' +
        'what to pick next.');
    AddParagraph(Result, 'A curve position or a fit interval bound can also be ' +
        'moved, by typing its new position into the Curve Positions or the Fit ' +
        'Intervals table. A moved position keeps what a fit found for its curve; ' +
        'taking a position out with a click and picking another places a new ' +
        'curve, which starts again from the data.');
    AddLimitation(Result, 'A pick cannot be dragged on the chart.');
    AddLimitation(Result, 'Picks are refused while a fit or another calculation ' +
        'is running, with a message saying a calculation is already in progress.');
    AddRelated(Result, CurvePositionsTopic);
    AddRelated(Result, FitIntervalsTopic);
    AddRelated(Result, DataIntervalTopic);
end;

function CurvePositions: TExplanation;
begin
    Result := NewExplanation(CurvePositionsTopic, 'Curve positions',
        'A curve position is the argument where one curve of the model is ' +
        'placed; positions can be picked on the chart, proposed by the program ' +
        'or put at every data point.', esModelChoice);
    AddParagraph(Result, 'Each position seeds one curve: the curve is placed ' +
        'there, and its height is taken from the data at that point. The Model > ' +
        'Curve Positions submenu offers four ways to set them:');
    AddParagraph(Result, Bullet + 'Model > Curve Positions > Start Manual ' +
        'Selection, shown as Start Visual Position Selection, or the Pick button ' +
        'under Positions - pick positions on the chart. The status line invites ' +
        'the next pick or a fit.');
    AddParagraph(Result, Bullet + 'Model > Curve Positions > Compute ' +
        'Automatically, or Auto - replaces the positions with ones the program ' +
        'finds. Working down from the highest point, it takes every peak that ' +
        'stands above the background fraction of that point and places a ' +
        'position at each local maximum - or minimum, or both, as the curve type ' +
        'asks. A curve type placed from its own markup is placed by its pack ' +
        'instead: the pack adds what it finds in the data to that markup, only ' +
        'where nothing is yet, and its guide says how.');
    AddParagraph(Result, Bullet + 'Model > Curve Positions > At Every Point - ' +
        'puts a position at every point of the profile, or of the data interval ' +
        'in force. It is a starting point for Fit > Minimize Number of Curves, ' +
        'which removes the curves that are not needed. A curve type placed from ' +
        'its own markup refuses it with a message.');
    AddParagraph(Result, Bullet + 'Model > Curve Positions > Remove All, or ' +
        'Clear - removes every position, and with them the curves built from ' +
        'them: with nothing picked there is no model.');
    AddParagraph(Result, 'The positions are listed in the Curve Positions table ' +
        '(Position and Amplitude) and drawn as the Curve positions series. After ' +
        'a fit a second series, Fitted positions, shows where each curve ended ' +
        'up; your picks never move. When nothing was picked - a model placed from ' +
        'a curve type''s own markup - the table lists where the model''s curves ' +
        'are instead.');
    AddParagraph(Result, 'The submenu and the Positions buttons are greyed until ' +
        'a profile is loaded, and while a fit runs.');
    AddLimitation(Result, 'At Every Point makes one curve per data point, which ' +
        'on a long profile is a great many curves to fit.');
    AddRelated(Result, PickingTopic);
    AddRelated(Result, DeleteCurveTopic);
    AddRelated(Result, FitIntervalsTopic);
end;

function FitIntervals: TExplanation;
begin
    Result := NewExplanation(FitIntervalsTopic, 'Fit intervals',
        'A fit interval is a stretch of the profile fitted as a problem of its ' +
        'own, marked by a pair of bounds.', esModelChoice);
    AddParagraph(Result, 'Bounds come in pairs, and each pair encloses one ' +
        'interval. Splitting a profile ' +
        'into intervals keeps each fit small, and lets a stretch with no peaks in ' +
        'it be left out.');
    AddParagraph(Result, Bullet + 'Model > Fit Intervals > Start Manual ' +
        'Selection, shown as Start Visual Selection, or the Pick button under Fit ' +
        'intervals - pick bounds on the chart. The status line asks for the left ' +
        'and the right point of each peak in turn.');
    AddParagraph(Result, Bullet + 'Model > Fit Intervals > Compute ' +
        'Automatically, or Auto - enters the same picking mode and divides the ' +
        'model as the module it belongs to divides it. A curve pack splits its ' +
        'own model - its own guide says how - and its intervals replace the ' +
        'ones there were. For the program''s own curve types, which are peaks, ' +
        'it adds bounds around every peak it finds, with the search the ' +
        'automatic curve positions use: each stretch standing above the ' +
        'background fraction of the highest point gets a bound at each end, ' +
        'and bounds you picked are kept. A model whose pack proposes nothing - ' +
        'nothing marked yet - is not searched for peaks it is not made of: the ' +
        'command says so instead. An EMPTY model of a type placed from a ' +
        'pack''s markup is searched by that pack for what its instances would ' +
        'span; where it finds nothing, and for any other empty model, the data ' +
        'is searched for peaks.');
    AddParagraph(Result, Bullet + 'Model > Fit Intervals > Remove All, or Clear ' +
        '- removes every bound. Without a fit interval nothing can be built, so ' +
        'the model is emptied too; your curve positions are kept, and picking ' +
        'bounds again rebuilds the curves from them.');
    AddParagraph(Result, 'The intervals are listed in the Fit Intervals table ' +
        '(Starting Position and Final Position) and drawn as the Fit intervals ' +
        'series. An interval with only one bound picked so far shows its second ' +
        'cell empty.');
    AddParagraph(Result, 'A fit started with fewer than two bounds computes the ' +
        'intervals itself, as Compute Automatically does.');
    AddParagraph(Result, 'Each interval is fitted on its own - nothing in one ' +
        'reads another - so the intervals are fitted side by side, at most one ' +
        'per processor of the machine running the compute server. The model ' +
        'they reach is exactly the one fitting them one after another would ' +
        'reach; only the time differs. An engine running in another process ' +
        '(the Python one, or a remote compute server) is given one interval at ' +
        'a time, as before.');
    AddLimitation(Result, 'The peak search Auto runs for the program''s own ' +
        'curve types is this program''s own heuristic, not a published method: ' +
        'peaks whose flanks run into each other without a rise between them ' +
        'land in one interval. Pick the bounds by hand to fit them apart.');
    AddRelated(Result, PickingTopic);
    AddRelated(Result, CurvePositionsTopic);
end;

function ModelPanel: TExplanation;
begin
    Result := NewExplanation(ModelPanelTopic, 'The Model tab',
        'The Model tab on the right lists the curves of the model, one row per ' +
        'curve with where it sits, and is where one curve is chosen to be ' +
        'explained or deleted.', esModelChoice);
    AddParagraph(Result, 'Each row names a curve and its position. Curves a ' +
        'curve pack placed from its own markup are shown the way that pack ' +
        'describes them instead - a pattern with the patterns nested in it ' +
        'beneath it, as a tree you can collapse and expand. That does not ' +
        'depend on the curve type selected in the Tools list, which only says ' +
        'what you place next. Any other curves of the model follow, one row ' +
        'each.');
    AddParagraph(Result, 'Once the model holds anything, the tab''s title names ' +
        'the module the model belongs to - "Model: Standard" for the ' +
        'framework''s own curve types, or the name of the curve pack. A model ' +
        'holds one module''s curves only, so the other modules'' curve types ' +
        'are greyed until the model is cleared; the title is why. An empty ' +
        'model is just "Model": every curve type is open to it.');
    AddParagraph(Result, 'When there is nothing to list, the tab says why: no ' +
        'data file is open, no curve has been placed yet, or the selected curve ' +
        'type is placed from its own markup and none has been made.');
    AddParagraph(Result, 'Selecting a row shows the explanation of that curve''s ' +
        'type in the Explain pane below the list. Right-clicking a row selects ' +
        'it and opens the commands that act on one curve, such as Delete curve.');
    AddRelated(Result, DeleteCurveTopic);
    AddRelated(Result, CurveTypeTopic);
    AddRelated(Result, CurveAttributesTopic);
end;

function CurveAttributes: TExplanation;
begin
    Result := NewExplanation(CurveAttributesTopic, 'The Curve Attributes table',
        'The Curve Attributes tab at the bottom of the window lists every curve ' +
        'of the model with the value of each of its parameters.', esModelChoice);
    AddParagraph(Result, 'There is one row per curve and one column per ' +
        'parameter name. When curves of different types share the model, the ' +
        'columns cover every parameter any of them has, and a cell is blank where ' +
        'a curve has no such parameter.');
    AddParagraph(Result, 'Values are shown with four decimals. Where the engine ' +
        'estimated an uncertainty it follows the value after a plus-minus sign; ' +
        'the built-in engine does not estimate one. Positions are shown on the ' +
        'argument axis in force, as on the chart.');
    AddParagraph(Result, 'Colour shows how each value is treated, and the row ' +
        'under the table names the colours: Fitted, Shared, Fixed and Computed. ' +
        'Rest the pointer on one to read what it means.');
    AddParagraph(Result, 'Select whole rows and choose Edit > Delete to remove ' +
        'those curves. A button above the table copies what is selected to the ' +
        'clipboard.');
    AddParagraph(Result, 'A value the fit varies can be typed over: click the ' +
        'cell, type the number and leave it. The engine rebuilds the model with ' +
        'it and the table and the chart are redrawn, so the next fit starts from ' +
        'there. A value shown in the table''s units - an angle, for instance - ' +
        'is typed in those units.');
    AddParagraph(Result, 'A value outside the range its parameter allows - a ' +
        'width wider than the fit interval, say - is held at the nearest one ' +
        'allowed, and a message says so (see Limits on parameter values).');
    AddLimitation(Result, 'A parameter computed from the others cannot be ' +
        'typed, and text that is not a number is refused; a message says which.');
    AddLimitation(Result, 'Edit > Delete acts only on whole rows. A selection ' +
        'that does not span every column deletes nothing.');
    AddRelated(Result, ParameterLimitsTopic);
    AddRelated(Result, ParameterKindsTopic);
    AddRelated(Result, DeleteCurveTopic);
    AddRelated(Result, ModelPanelTopic);
end;

function ParameterKinds: TExplanation;
begin
    Result := NewExplanation(ParameterKindsTopic,
        'Parameter kinds: fitted, shared, fixed and computed',
        'Each parameter value in the Curve Attributes table is one of four kinds, ' +
        'which say whether and how the fit moves it.', esModelChoice);
    AddParagraph(Result, Bullet + 'Fitted - varied by the fit to match the data. ' +
        'The ordinary case and most of the table; it keeps the table''s own row ' +
        'colour.');
    AddParagraph(Result, Bullet + 'Shared - varied by the fit, but held to one ' +
        'value across the curves of an interval.');
    AddParagraph(Result, Bullet + 'Fixed - set when the curve was placed and not ' +
        'varied; for a wave pattern, the points you picked.');
    AddParagraph(Result, Bullet + 'Computed - neither fitted nor entered: it ' +
        'follows from the other parameters and is recomputed whenever they ' +
        'change.');
    AddParagraph(Result, 'The engine tells more kinds apart - which parameter is ' +
        'the amplitude, which the width, which the position - but to a reader of ' +
        'the table those differ only in how the fit starts them, so they are all ' +
        'shown as Fitted.');
    AddParagraph(Result, 'For the program''s own curve types the kinds are part ' +
        'of the type. For a type whose shape is a formula you write, you choose ' +
        'them yourself when you define it.');
    AddRelated(Result, CurveAttributesTopic);
end;

function ParameterLimits: TExplanation;
begin
    Result := NewExplanation(ParameterLimitsTopic, 'Limits on parameter values',
        'Every parameter keeps to a range of its own, and a value outside it - ' +
        'proposed by a fit, typed into the table or read from a saved project - ' +
        'is held at the nearest one the range allows.', esModelChoice);
    AddParagraph(Result, 'The ranges are these:');
    AddParagraph(Result, Bullet + 'An amplitude is not below zero; a curve ' +
        'whose amplitude would be negative is a peak turned upside down, which ' +
        'no measured peak is.');
    AddParagraph(Result, Bullet + 'A peak is no wider at half its maximum than ' +
        'the fit interval it is fitted in, and no width is wider than the fit ' +
        'interval allows. A peak wider than the stretch of data it is fitted to ' +
        'is not a peak any more but a hump under the whole interval, and the fit ' +
        'then trades it against the background: in one pattern a peak''s width ' +
        'reached three times its interval and the background under it went far ' +
        'below zero to cancel the hump, with a good R-factor and a meaningless ' +
        'result.');
    AddParagraph(Result, 'The limit is on the peak, not on the number, because ' +
        'a width means different things in different curve types. A ' +
        'pseudo-Voigt''s or a Pearson VII''s sigma is its full width at half ' +
        'maximum, so it may be as wide as the interval; a Gaussian''s sigma is ' +
        'a standard deviation, and the peak is 2.35 of them wide at half its ' +
        'maximum, so it may be the interval divided by 2.35. A Voigt''s gamma ' +
        'and a Doniach-Sunjic sigma are half widths; an exponentially modified ' +
        'Gaussian''s tail tau falls to half in ln 2 of its length; how wide a ' +
        'Moffat or a Doniach-Sunjic line is per unit of sigma depends on its ' +
        'shape, and the limit follows the shape the fit has reached. A Voigt''s ' +
        'two widths are held together, so the peak they make fits. Both widths ' +
        'of a two-branch pseudo-Voigt are held, and the difference of an ' +
        'asymmetric pseudo-Voigt''s two side widths too.');
    AddParagraph(Result, Bullet + 'A mixing fraction, such as a pseudo-Voigt''s ' +
        'eta, lies between 0 (all Gaussian) and 1 (all Lorentzian).');
    AddParagraph(Result, Bullet + 'A position stays between the samples either ' +
        'side of where it was picked.');
    AddParagraph(Result, Bullet + 'A background curve is not below zero where ' +
        'the data under it are not, because a background is then a count of ' +
        'what lies under the peaks. Where any sample of the fit interval is ' +
        'below zero - data with a background already subtracted, or a ' +
        'difference - the background may go below zero too, since a floor ' +
        'there would refuse the fit the data need.');
    AddParagraph(Result, 'Where it is held, the level of an Exponential or a ' +
        'Power-law background is kept at zero or above. A Constant, Linear or ' +
        'Quadratic background, whose sign no single coefficient decides, is ' +
        'lifted after every calculation until its lowest point in the fit ' +
        'interval touches zero, and its level b0 carries the lift - so the ' +
        'coefficients in the table describe the curve that is drawn. A lift ' +
        'rather than a cut at zero, because a cut background has parts no ' +
        'coefficient can move, and rather than a penalty, because a penalty ' +
        'only discourages going below zero.');
    AddParagraph(Result, Bullet + 'A curve type you define yourself keeps the ' +
        'range of the role you give each parameter in its dialog: an Amplitude ' +
        'is not below zero, and a Width is above zero and held where the peak ' +
        'your formula draws is as wide as the fit interval - measured from the ' +
        'formula, since the program cannot know whether it reads the Width as ' +
        'a standard deviation, a half width or a full width. A parameter with ' +
        'any other role takes any value.');
    AddParagraph(Result, 'The same limits hold whichever engine fits: the ' +
        'Python engine and a compute server are handed each range, told which ' +
        'curves may not go below zero, and return a lifted background at the ' +
        'level it is drawn at. A limit that follows the shape is handed to the ' +
        'Python engine for the shape the fit starts from; when the fit reaches ' +
        'one where it no longer holds, it is fitted again with the limits of ' +
        'that shape, three times at most.');
    AddParagraph(Result, 'When a value is held at another, you are told: a ' +
        'value typed into the Curve Attributes table, with a message as you ' +
        'leave the cell; a project, or a model from its History, saved before a ' +
        'limit existed, with a message naming each value as it is put back. A ' +
        'program writing a value over the REST interface is answered with the ' +
        'value held and the same reason.');
    AddLimitation(Result, 'These are bounds on each value, not on how values ' +
        'combine: two peaks may still overlap until one of them carries the ' +
        'other, and the model may still be fitted to features the data does ' +
        'not support.');
    AddLimitation(Result, 'A Voigt''s full width is the Olivero-Longbothum ' +
        'approximation, within 0.02 %. A user-defined Width is measured once ' +
        'for each fit interval, which is exact for a width that scales the ' +
        'curve and approximate for one that also changes its shape; a formula ' +
        'with no peak to measure counts its Width as a full width. A narrower ' +
        'interval is the way to hold a peak tighter.');
    AddRelated(Result, CurveAttributesTopic);
    AddRelated(Result, FitIntervalsTopic);
    AddRelated(Result, ParameterKindsTopic);
end;

function DeleteCurve: TExplanation;
begin
    Result := NewExplanation(DeleteCurveTopic, 'Deleting a curve',
        'Right-click a curve in the Model tab and choose Delete curve to remove ' +
        'it, together with the position it was placed from.', esModelChoice);
    AddParagraph(Result, 'In the Model tab on the right, right-click the curve''s ' +
        'row - the click also selects it - and choose Delete curve. The entry is ' +
        'greyed when the row does not stand for a curve, and while a fit runs.');
    AddParagraph(Result, 'There is no Model > Delete Curve entry in the menu ' +
        'bar: deleting one curve needs a curve to act on, and the selected row of ' +
        'the Model tab is what names it.');
    AddParagraph(Result, 'The position the curve was placed from goes with it - ' +
        'the model is rebuilt from the positions, so a position left behind would ' +
        'put the curve straight back. The other curves keep what the last fit ' +
        'found for them.');
    AddParagraph(Result, 'There are two other ways to remove curves: select whole ' +
        'rows of the Curve Attributes table and choose Edit > Delete, or, while ' +
        'picking curve positions, click a curve''s position on the chart.');
    AddParagraph(Result, 'If the engine cannot remove a curve on its own - one a ' +
        'curve pack placed from its markup, for example - it refuses, and the ' +
        'message says why.');
    AddLimitation(Result, 'Deleting a curve cannot be undone as such. The model ' +
        'as the last fit left it is still in the History tab, and making it ' +
        'current brings the curve back with what that fit found for it.');
    AddRelated(Result, ClearModelTopic);
    AddRelated(Result, ModelPanelTopic);
    AddRelated(Result, CurvePositionsTopic);
end;

function ClearModel: TExplanation;
begin
    Result := NewExplanation(ClearModelTopic, 'Clearing the model',
        'Model > Clear Model removes every curve from the model at once, after ' +
        'asking.', esModelChoice);
    AddParagraph(Result, 'It is the last entry of the Model menu. It asks whether ' +
        'to remove all the curves, naming how many, says that their picks go with ' +
        'them and that this cannot be undone, and does nothing unless you answer ' +
        'Yes.');
    AddParagraph(Result, 'Each curve is removed as Delete curve removes one, its ' +
        'position included, so you start again from an empty model on the same ' +
        'data. The fit intervals, the background points and the curve type are ' +
        'kept.');
    AddParagraph(Result, 'The entry is greyed while the model holds no curve, ' +
        'until a profile is loaded, and while a fit runs.');
    AddParagraph(Result, 'A curve the engine keeps stays where it is: one that ' +
        'the curve type builds per fit interval rather than from a position is ' +
        'rebuilt at once, and a curve pack may decline to give one up. A single ' +
        'message then says how many curves stayed and why.');
    AddLimitation(Result, 'Clearing cannot be undone as such. Every model a ' +
        'fit reached is still in the History tab, and making one current brings ' +
        'it back; changes made since the last fit are not recorded there.');
    AddRelated(Result, DeleteCurveTopic);
    AddRelated(Result, ModelTopic);
    AddRelated(Result, ModelHistoryTopic);
end;

function ModelHistory: TExplanation;
begin
    Result := NewExplanation(ModelHistoryTopic, 'The model history',
        'Every run that ends records the model it reached, so you can go back ' +
        'to any of them, compare them and carry on from the best.',
        esModelChoice);
    AddParagraph(Result, 'A model is recorded when Fit > Minimize Difference, ' +
        'Fit > Minimize Number of Curves or Fit > Automatically ends - also when ' +
        'Stop cut it short, because what a stopped run reached is the model from ' +
        'then on. A run that the engine refused records nothing, and so does a ' +
        'run that left the model exactly as it was, so pressing Fit on a ' +
        'converged model does not add a row.');
    AddParagraph(Result, 'What is recorded is the whole model, as a project ' +
        'file holds one: the curve type and the settings, the fit intervals, ' +
        'the background, the curve positions, the fitted values of every curve, ' +
        'what any analysis module keeps about it, and the profile it was fitted ' +
        'to. The R-factor and the statistics it reached are recorded with it.');
    AddParagraph(Result, 'Make Current - a right-click on the row in the ' +
        'History tab, its button, a double-click on the row, or Enter - puts the selected model back exactly as it was ' +
        'recorded, on the profile it was fitted to, and measures it so that ' +
        'its R-factor shows again. The next fit starts from it, and the model ' +
        'that fit reaches is recorded as fitted from it, which is how a branch ' +
        'starts.');
    AddParagraph(Result, 'Making a model current never loses work. If the ' +
        'current model holds changes no recorded model has - a position moved ' +
        'or a value typed since the last fit - it is recorded first, as ' +
        '"Edited, not fitted", and you can come back to it like any other.');
    AddParagraph(Result, 'Rename..., on the same right-click menu, gives the ' +
        'selected model a name of your own, shown first in its row. Delete... ' +
        'removes it after asking; the current model is not changed, and the ' +
        'models fitted from the one deleted are shown as fitted from the one ' +
        'before it. If the deleted model was the current one, the next model ' +
        'recorded from the window - by a fit, or as "Edited, not fitted" - is ' +
        'shown as fitted from the one before it too. All three are greyed while a fit runs, and Make Current is ' +
        'greyed for the model that is already current.');
    AddParagraph(Result, 'The history is saved in the project and comes back ' +
        'when it is opened. New Project starts with none. Importing another ' +
        'profile keeps it: each model carries its own profile, so making one ' +
        'current puts that profile back too.');
    AddLimitation(Result, 'The R-factor of a model is comparable only with ' +
        'models fitted to the same data with the same objective; the ' +
        #$E2#$98#$85 + ' mark compares only those, and says nothing about ' +
        'whether a model with fewer curves or other settings is a better ' +
        'description of the data.');
    AddLimitation(Result, 'Edits made after the last fit are recorded only when ' +
        'another model is made current. Closing, or clearing the model, does ' +
        'not record them; save the project to keep them.');
    AddLimitation(Result, 'Two models cannot be merged into one, and there is ' +
        'no side-by-side comparison of two models yet.');
    AddRelated(Result, HistoryTabTopic);
    AddRelated(Result, ClearModelTopic);
    AddRelated(Result, ProjectContentsTopic);
end;

function ModelExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, Model);
    AppendExplanation(Result, CurveType);
    AppendExplanation(Result, ToolsPane);
    AppendExplanation(Result, Picking);
    AppendExplanation(Result, CurvePositions);
    AppendExplanation(Result, FitIntervals);
    AppendExplanation(Result, ModelPanel);
    AppendExplanation(Result, CurveAttributes);
    AppendExplanation(Result, ParameterKinds);
    AppendExplanation(Result, ParameterLimits);
    AppendExplanation(Result, DeleteCurve);
    AppendExplanation(Result, ClearModel);
    AppendExplanation(Result, ModelHistory);
end;

end.
