// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide's chapter on the model: the curve type, the curve
positions, the fit intervals, the background, the tables and panels that show
the model, deleting and clearing curves, and user-defined curves.)

WHY IT IS ONE CHAPTER. The Model menu, the Tools tab and the tables at the
bottom of the window are three views of one thing - what is fitted to the
profile - and a user building a model moves between all three. The code is
spread wider still: form_main, ui_commands, action_state, pick_guidance,
pick_target, model_clearing, model_outline, curve_type_menu, curve_list_grid,
parameter_kinds, parameter_roles, formula_editing, the user-curve dialogs and
the engine's fit_service, whose automatic searches decide what Compute
Automatically actually finds.

WHAT THE MENU DOES NOT SAY IS SAID HERE. A user-defined curve's parameters can be shared but not frozen; the
Background Fraction governs the peak search, not the
background points its name suggests; there is no Model > Delete Curve entry. Each of these is what a user would
otherwise find out by being surprised. When the code changes, the sentence here
that describes it has to change with it.

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
    BackgroundTopic = 'model/background';
    BackgroundCurveTopic = 'model/background-curve';
    BackgroundSubtractionTopic = 'model/background-subtraction';
    BackgroundFractionTopic = 'model/background-fraction';
    BackgroundVariationTopic = 'model/background-variation';
    ModelPanelTopic = 'model/model-panel';
    CurveAttributesTopic = 'model/curve-attributes';
    ParameterKindsTopic = 'model/parameter-kinds';
    ParameterLimitsTopic = 'model/parameter-limits';
    DeleteCurveTopic = 'model/delete-curve';
    ClearModelTopic = 'model/clear-model';
    ModelHistoryTopic = 'model/model-history';
    UserCurvesTopic = 'model/user-defined-curves';
    UserCurveFormulaTopic = 'model/user-curve-formula';
    UserCurveRolesTopic = 'model/user-curve-roles';
    UserCurveLibraryTopic = 'model/user-curve-library';

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
        'shape, placed at positions, fitted over intervals, above a background.',
        esModelChoice);
    AddParagraph(Result, 'It is built from four parts:');
    AddParagraph(Result, Bullet + 'The curve type - the shape of every curve ' +
        'placed from a position, chosen from Model > Curve Type or the list at ' +
        'the top of the Tools tab.');
    AddParagraph(Result, Bullet + 'The curve positions - one per curve, the ' +
        'argument where that curve is placed. They are listed in the Curve ' +
        'Positions table at the bottom of the window.');
    AddParagraph(Result, Bullet + 'The fit intervals - stretches of the ' +
        'profile, each fitted as a problem of its own, marked by pairs of ' +
        'bounds and listed in the Fit Intervals table.');
    AddParagraph(Result, Bullet + 'The background - the slowly varying level ' +
        'under the peaks: a curve of the model fitted with the peaks ' +
        '(Model > Background > Curve), a quadratic the fit varies (Enable ' +
        'Variation), or subtracted from the profile before fitting.');
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
    AddParagraph(Result, 'The curve positions, fit intervals and background ' +
        'points are saved with the project, together with the settings that ' +
        'shape them.');
    AddLimitation(Result, 'Every curve placed from a position has the selected ' +
        'curve type. Choosing another type changes the shape of all of them, and ' +
        'the values an earlier fit found for the old shape are not carried over.');
    AddRelated(Result, CurveTypeTopic);
    AddRelated(Result, CurvePositionsTopic);
    AddRelated(Result, FitIntervalsTopic);
    AddRelated(Result, BackgroundTopic);
    AddRelated(Result, ToolsPaneTopic);
    AddRelated(Result, ProfileTopic);
end;

function CurveType: TExplanation;
begin
    Result := NewExplanation(CurveTypeTopic, 'Choosing a curve type',
        'The curve type is the shape every curve placed from a position takes, ' +
        'and is chosen from Model > Curve Type or from the list at the top of ' +
        'the Tools tab.', esModelChoice);
    AddParagraph(Result, 'Model > Curve Type opens a submenu of groups. Standard ' +
        'holds the types the program ships; a curve pack included in the build ' +
        'may add groups of its own after it; User comes last and holds New User ' +
        'Curve and the curve types you have defined yourself. The type in use is ' +
        'ticked.');
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
    AddParagraph(Result, 'New User Curve does not name a shape: it opens the ' +
        'dialogs that define one. Cancelling them leaves the previous type in ' +
        'use.');
    AddParagraph(Result, 'A model is made of the curve types of ONE module. ' +
        'Standard is one; a curve pack included in the build is another. While ' +
        'the model is empty any type can be chosen - that is how a model gets ' +
        'its module. Once it holds a pick or a curve, the types of every other ' +
        'module stay listed but greyed, in the menu and in the Tools list, and ' +
        'choosing one says why: each module covers its own kind of task, and ' +
        'curves of two of them fit nothing either understands. To change ' +
        'module, start a new project with File > New Project, or empty the ' +
        'model with Model > Clear Model.');
    AddParagraph(Result, 'A module may also say that its curves take no ' +
        'separate background - its curves describing the whole of the data, ' +
        'level included. Model > Background > Curve then offers only None, and ' +
        'says why.');
    AddLimitation(Result, 'This is this software''s choice, not a rule of any ' +
        'field: one module per model keeps every model one that some module ' +
        'fully understands - its rules, its report and its fit intervals.');
    AddLimitation(Result, 'The curve types you have defined are listed only in ' +
        'the menu, under Model > Curve Type > User. The Tools list offers New ' +
        'User Curve but not the types already made.');
    AddRelated(Result, ModelTopic);
    AddRelated(Result, UserCurvesTopic);
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
    AddParagraph(Result, Bullet + 'Background: Pick, Auto and Clear run Model > ' +
        'Background > Points > Start Manual Selection, Compute Automatically and ' +
        'Remove All; Subtract runs Model > Background > Subtract > Automatically.');
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
    AddLimitation(Result, 'Subtract always subtracts automatically, ignoring the ' +
        'background points you picked. To subtract through your own points use ' +
        'Model > Background > Subtract > By Selected Points.');
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
        'Manual Selection entries for curve positions, fit intervals, background ' +
        'points and the data range, and the Pick buttons of the Tools tab. ' +
        'Only one mode runs at a time: starting another ends the first, and ' +
        'choosing the same entry or button again ends it.');
    AddParagraph(Result, 'The entries for curve positions, fit intervals and ' +
        'background points say whether their mode is on. They read Start Visual ' +
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
    AddRelated(Result, BackgroundTopic);
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
        'asks.');
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
    AddRelated(Result, BackgroundFractionTopic);
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
        'command says so instead. An EMPTY model belongs to no pack, whatever ' +
        'curve type is selected, so it is searched for peaks.');
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
    AddRelated(Result, BackgroundFractionTopic);
    AddRelated(Result, CurvePositionsTopic);
end;

function Background: TExplanation;
begin
    Result := NewExplanation(BackgroundTopic, 'The background',
        'The background is the slowly varying level under the peaks; the ' +
        'program can fit it as a curve of the model, or mark it with background ' +
        'points and subtract it.', esModelChoice);
    AddParagraph(Result, 'There are three ways to deal with it, and the first ' +
        'leaves the profile as it was measured: a background curve in the model ' +
        '(Model > Background > Curve), fitted with the peaks; the quadratic that ' +
        'Model > Background > Enable Variation adds inside a fit; or subtracting ' +
        'it from the profile (Model > Background > Subtract). Fit > ' +
        'Automatically uses the first.');
    AddParagraph(Result, 'Background points are points of the profile that lie ' +
        'on the background rather than on a peak. Once they are marked, the ' +
        'background they describe can be subtracted from the profile. The ' +
        'Model > Background > Points submenu sets them:');
    AddParagraph(Result, Bullet + 'Model > Background > Points > Start Manual ' +
        'Selection, shown as Start Visual Selection, or the Pick button under ' +
        'Background - pick background points on the chart, as many as you like.');
    AddParagraph(Result, Bullet + 'Model > Background > Points > Compute ' +
        'Automatically, or Auto - enters the same picking mode and replaces the ' +
        'background points with ones the program proposes. It starts from the ' +
        'lowest point of the profile and walks outwards in both directions, each ' +
        'step taking the lowest point not below the last one taken. You can then ' +
        'add or remove points by clicking.');
    AddParagraph(Result, Bullet + 'Model > Background > Points > Remove All, or ' +
        'Clear - discards the background points.');
    AddParagraph(Result, 'The points are listed in the Background Points table ' +
        '(Position and Amplitude) and drawn as the Background points series. The ' +
        'small toolbar above that table has buttons for computing the points, ' +
        'subtracting automatically, subtracting by the selected points and ' +
        'removing them all. Typing into the table does not change the points.');
    AddParagraph(Result, 'The Background submenu and the Background buttons are ' +
        'greyed until a profile is loaded, and while a fit runs.');
    AddLimitation(Result, 'The automatic proposal assumes a background that is ' +
        'lowest in one place and rises on either side of it, as in a neutron ' +
        'diffractogram. On a background of another shape it still proposes ' +
        'points, but they need not lie on the background; look at them on the ' +
        'chart before subtracting.');
    AddRelated(Result, BackgroundCurveTopic);
    AddRelated(Result, BackgroundSubtractionTopic);
    AddRelated(Result, BackgroundVariationTopic);
    AddRelated(Result, PickingTopic);
end;

function BackgroundCurve: TExplanation;
begin
    Result := NewExplanation(BackgroundCurveTopic, 'The background curve',
        'Model > Background > Curve puts the background under the peaks as a ' +
        'curve of the model, fitted with them, drawn and saved with the ' +
        'project - and leaves the profile as it was measured.', esModelChoice);
    AddParagraph(Result, 'The submenu lists None and every background shape: ' +
        'Constant, Linear and Quadratic backgrounds, an Exponential background ' +
        'for a tail that decays, and a Power-law background. The tick shows the ' +
        'one the model has. Choosing a shape puts one curve of it in every fit ' +
        'interval, after the peaks, starting from the level of the data''s own ' +
        'baseline there; Model > Background > Curve > None takes it out ' +
        'again.');
    AddParagraph(Result, 'It is fitted with the curves by every fit, drawn with ' +
        'them, listed in the Model tab and the Curve Attributes table like them, ' +
        'and saved in the project with what the fit found - so a project ' +
        'reopens with the same background, still fitted. Selecting its row in ' +
        'the Model tab and deleting it (Model > Delete Curve) removes the ' +
        'background from every interval, as None does.');
    AddParagraph(Result, 'Its coefficients are measured from xr, the start of ' +
        'the fit interval: b0 is the background there, b1 the slope, b2 the ' +
        'curvature; tau is an exponential''s decay length and p a power law''s ' +
        'exponent. Reducing the number of curves never removes it, and a model ' +
        'of only a background can be drawn and fitted.');
    AddParagraph(Result, 'A background is not below zero anywhere in its fit ' +
        'interval when the data there are not: one that would dip below is ' +
        'lifted until its lowest point touches zero, and its level b0 carries ' +
        'the lift. Over data that go below zero it may follow them. See ' +
        'Limits on parameter values.');
    AddParagraph(Result, 'Fit > Automatically adds a Quadratic background when ' +
        'the model has none, and keeps the one you chose when it has. It ' +
        'subtracts nothing from the profile.');
    AddParagraph(Result, 'The shapes are greyed while Model > Background > ' +
        'Enable Variation is on, and the variation is greyed while the model ' +
        'has a background curve: both put a background under the peaks, and ' +
        'with both it would be counted twice. The hint says so.');
    AddLimitation(Result, 'With curve scaling on, the coefficients shown carry ' +
        'the one factor that scales the whole model onto the data - as a ' +
        'peak''s amplitude does - so the curve drawn is the background, and the ' +
        'ratios of its coefficients are what the fit found.');
    AddLimitation(Result, 'Intensity the model has no curve for - small peaks ' +
        'nobody placed - is taken up by the background, which then rises ' +
        'under them. Place the peaks, or narrow the fit interval.');
    AddLimitation(Result, 'One shape serves every fit interval. The Power-law ' +
        'background is refused for data that reaches x = 0 or below.');
    AddRelated(Result, BackgroundTopic);
    AddRelated(Result, BackgroundVariationTopic);
    AddRelated(Result, BackgroundSubtractionTopic);
    AddRelated(Result, DeleteCurveTopic);
end;

function BackgroundSubtraction: TExplanation;
begin
    Result := NewExplanation(BackgroundSubtractionTopic,
        'Subtracting the background',
        'Model > Background > Subtract joins the background points by straight ' +
        'lines and subtracts those lines from the profile.', esModelChoice);
    AddParagraph(Result, Bullet + 'Model > Background > Subtract > By Selected ' +
        'Points uses the background points you picked or had proposed. It needs ' +
        'at least two; with fewer it says so and does nothing.');
    AddParagraph(Result, Bullet + 'Model > Background > Subtract > Automatically ' +
        'ignores any points you picked, proposes its own as Compute Automatically ' +
        'does, and subtracts through them. The Subtract button under Background ' +
        'in the Tools tab runs this one.');
    AddParagraph(Result, 'Between each two neighbouring background points the ' +
        'straight line through them is subtracted from the amplitudes, so every ' +
        'background point itself ends at zero. The profile on the chart and in ' +
        'the Data table is replaced by the result and the background points are ' +
        'cleared. With a data interval selected, only that interval is changed.');
    AddParagraph(Result, 'Subtract is greyed until a profile is loaded, and ' +
        'while a fit runs.');
    AddLimitation(Result, 'Points outside the outermost background points are ' +
        'left as they were, so place background points near both ends of the ' +
        'stretch you fit.');
    AddLimitation(Result, 'Subtraction cannot be undone, and it can be repeated: ' +
        'each run subtracts again from what the last one left. File > Reload ' +
        'Profile brings back the data as measured.');
    AddLimitation(Result, 'It changes the data, not the model. To keep the ' +
        'profile as measured and fit the background instead, use Model > ' +
        'Background > Curve - which is what Fit > Automatically does.');
    AddRelated(Result, BackgroundTopic);
    AddRelated(Result, BackgroundCurveTopic);
    AddRelated(Result, BackgroundVariationTopic);
    AddRelated(Result, ReloadProfileTopic);
end;

function BackgroundFraction: TExplanation;
begin
    Result := NewExplanation(BackgroundFractionTopic, 'Background fraction',
        'The background fraction decides how small a peak the automatic searches ' +
        'still count: a point below 1/N of the highest point, for the factor N ' +
        'you set, is treated as background.', esModelChoice);
    AddParagraph(Result, 'Model > Background > Set Background Fraction opens the ' +
        'Background Factor dialog, whose field is labelled Background factor ' +
        'value 1 /. It must be a number greater than 1; anything else is ' +
        'refused with a message. The default is 30, so anything lower than one ' +
        'thirtieth of the highest point is not searched for peaks.');
    AddParagraph(Result, 'The factor is used by Model > Curve Positions > Compute ' +
        'Automatically, by Model > Fit Intervals > Compute Automatically, and by ' +
        'a fit that proposes positions or intervals itself, Fit > Automatically ' +
        'included. A larger factor finds smaller peaks; a smaller one keeps only ' +
        'the tall ones.');
    AddParagraph(Result, 'The factor is saved with the project.');
    AddLimitation(Result, 'Despite its name, the factor has no effect on the ' +
        'automatic background points, which are proposed without it.');
    AddLimitation(Result, 'The threshold is a fraction of the highest point, not ' +
        'a level above the background: where the background is high, the ' +
        'background itself may stand above it. Fit > Automatically searches a ' +
        'copy of the profile with the background taken off for exactly this ' +
        'reason; the Compute Automatically commands search the profile itself, ' +
        'so subtract the background first when using them on a high one.');
    AddRelated(Result, CurvePositionsTopic);
    AddRelated(Result, FitIntervalsTopic);
    AddRelated(Result, BackgroundSubtractionTopic);
end;

function BackgroundVariation: TExplanation;
begin
    Result := NewExplanation(BackgroundVariationTopic, 'Background variation',
        'With Model > Background > Enable Variation ticked, a fit also adjusts a ' +
        'smooth background of its own alongside the curves.', esModelChoice);
    AddParagraph(Result, 'The background the fit varies is a quadratic curve ' +
        'about a centre, described by four coefficients: curvature, slope, ' +
        'offset and centre. The curvature and the offset are kept from going ' +
        'negative, so the background never bends downwards and never sits below ' +
        'zero.');
    AddParagraph(Result, 'It is off until you turn it on. Choosing the entry ' +
        'turns it on or off, and a tick shows that it is on. The setting is ' +
        'saved with the project.');
    AddParagraph(Result, 'Use it when a background is left under the peaks that ' +
        'subtraction did not remove, or when you would rather fit the background ' +
        'than subtract it.');
    AddParagraph(Result, 'It is greyed while the model has a background curve ' +
        '(Model > Background > Curve), which puts a background under the peaks ' +
        'already - with both it would be counted twice. With it on, Fit > ' +
        'Automatically adds no background curve and subtracts nothing, and says ' +
        'that the variation is acting as the background.');
    AddLimitation(Result, 'The varied background is always a quadratic. A ' +
        'background of another shape - one with a step or a separate hump - ' +
        'cannot be described by it.');
    AddRelated(Result, BackgroundTopic);
    AddRelated(Result, BackgroundSubtractionTopic);
    AddRelated(Result, BackgroundCurveTopic);
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
        'of the type. For a user-defined curve you choose them yourself when you ' +
        'define it.');
    AddRelated(Result, CurveAttributesTopic);
    AddRelated(Result, UserCurveRolesTopic);
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
    AddRelated(Result, BackgroundCurveTopic);
    AddRelated(Result, FitIntervalsTopic);
    AddRelated(Result, ParameterKindsTopic);
    AddRelated(Result, UserCurveRolesTopic);
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
    AddParagraph(Result, 'Model > History > Make Current - or a double-click on ' +
        'the row, or Enter - puts the selected model back exactly as it was ' +
        'recorded, on the profile it was fitted to, and measures it so that ' +
        'its R-factor shows again. The next fit starts from it, and the model ' +
        'that fit reaches is recorded as fitted from it, which is how a branch ' +
        'starts.');
    AddParagraph(Result, 'Making a model current never loses work. If the ' +
        'current model holds changes no recorded model has - a position moved ' +
        'or a value typed since the last fit - it is recorded first, as ' +
        '"Edited, not fitted", and you can come back to it like any other.');
    AddParagraph(Result, 'Model > History > Rename gives the selected model a ' +
        'name of your own, shown first in its row. Model > History > Delete ' +
        'removes it after asking; the current model is not changed, and the ' +
        'models fitted from the one deleted are shown as fitted from the one ' +
        'before it. All three are greyed while a fit runs, and Make Current is ' +
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

function UserCurves: TExplanation;
begin
    Result := NewExplanation(UserCurvesTopic, 'User-defined curves',
        'You can fit a shape of your own by typing its formula; the program ' +
        'turns it into a curve type that is saved and offered like the built-in ' +
        'ones.', esModelChoice);
    AddParagraph(Result, 'Choose Model > Curve Type > User > New User Curve, or ' +
        'New User Curve in the Tools list. Two dialogs follow: Create New Curve ' +
        'Type, where you name the curve and write its formula, and Set Curve Type ' +
        'Properties, where you say which parameter plays which role and give ' +
        'starting values.');
    AddParagraph(Result, 'When you press Done the curve type is saved and ' +
        'selected. It appears under Model > Curve Type > User, below New User ' +
        'Curve, and is ticked while it is in use. It is kept between sessions: ' +
        'each one is stored as a file in the program''s settings folder.');
    AddParagraph(Result, 'A user-defined curve is fitted like any other: place ' +
        'positions, set fit intervals, fit. Its formula is saved with the ' +
        'project. It says nothing about its axes, so under the Automatic axis ' +
        'rules it is shown against what the data says, or a plain Position.');
    AddParagraph(Result, 'Cancelling either dialog creates nothing: a curve ' +
        'type saved when its formula was accepted is removed again if you then ' +
        'cancel the second dialog.');
    AddRelated(Result, UserCurveFormulaTopic);
    AddRelated(Result, UserCurveRolesTopic);
    AddRelated(Result, UserCurveLibraryTopic);
    AddRelated(Result, CurveTypeTopic);
end;

function UserCurveFormula: TExplanation;
begin
    Result := NewExplanation(UserCurveFormulaTopic,
        'Writing the formula of a user-defined curve',
        'In the Create New Curve Type dialog you give the curve a name and a ' +
        'formula in x; every other name in the formula becomes a parameter.',
        esModelChoice);
    AddParagraph(Result, 'Name is what the curve type is called in the menu ' +
        '(MyCurve until you change it). Expression is the formula, filled in with ' +
        'a bell curve to start from: A*exp(-((x-x0)/SIGMA)^2).');
    AddParagraph(Result, 'x is the argument, the variable running along the ' +
        'axis. Some names mean something, whatever their case: x0 is the ' +
        'position, where the curve is placed, and the fit does not move it; A is ' +
        'taken as the amplitude and SIGMA as the width, and SIGMA starts at 0.25 ' +
        'rather than 0 so that a formula dividing by it can be evaluated. Every ' +
        'other name is an ordinary fitted value starting at 0. The next dialog ' +
        'can change all of this.');
    AddParagraph(Result, 'The keypad inserts at the cursor. A function is ' +
        'inserted with its brackets, and the cursor left between them. It offers ' +
        'Sin, Cos, Tg, Ctg, the hyperbolic Sh, Ch, Th, Cth, Sch and Csch, the ' +
        'inverses Arcsin, Arccos, Arctg, Arcctg, Arsh, Arch, Arth and Arcth, Ln ' +
        '(natural logarithm), Log (base 10), Exp, Abs and Sqrt, the digits, the ' +
        'operators + - * / ^, brackets and a decimal point. Names are not ' +
        'case-sensitive, a comma typed on the keyboard becomes a decimal point, ' +
        'and sqr and pi can be typed though they have no button.');
    AddParagraph(Result, 'Continue >> checks the formula before anything is ' +
        'saved. If it cannot be used a message says why and the dialog opens ' +
        'again for you to correct it:');
    AddParagraph(Result, Bullet + 'the Expression is empty;');
    AddParagraph(Result, Bullet + 'the formula could not be understood - a typo, ' +
        'an unmatched bracket or an unknown function;');
    AddParagraph(Result, Bullet + 'the formula does not use x as its argument;');
    AddParagraph(Result, Bullet + 'the formula cannot be evaluated at its ' +
        'starting values - most often a parameter used as a denominator left at ' +
        '0.');
    AddParagraph(Result, 'Cancel abandons the definition.');
    AddLimitation(Result, 'Pressing a function button while text is selected ' +
        'replaces the selection with an empty call rather than wrapping it: the ' +
        'selected text is lost.');
    AddRelated(Result, UserCurveRolesTopic);
    AddRelated(Result, UserCurvesTopic);
end;

function UserCurveRoles: TExplanation;
begin
    Result := NewExplanation(UserCurveRolesTopic,
        'Parameter roles and starting values',
        'The Set Curve Type Properties dialog says which parameter of a ' +
        'user-defined curve is its argument, position, amplitude and width, ' +
        'which are held, and where each starts.', esModelChoice);
    AddParagraph(Result, 'Expression shows the formula. Four lists give the ' +
        'roles, and each role is held by at most one parameter - giving it to one ' +
        'takes it from any other:');
    AddParagraph(Result, Bullet + 'Argument - the variable along the axis, x ' +
        'unless you choose another.');
    AddParagraph(Result, Bullet + 'Position parameter - the one that places the ' +
        'curve at a pick. The parameter chosen here starts out held.');
    AddParagraph(Result, Bullet + 'Amplitude parameter - the height, started ' +
        'from the data peak, and never below zero.');
    AddParagraph(Result, Bullet + 'Width parameter - the width, started from the ' +
        'fit interval, and never wider than it (see Limits on parameter ' +
        'values).');
    AddParagraph(Result, 'Amplitude and Width offer (none), which leaves the ' +
        'role unheld.');
    AddParagraph(Result, 'Shared parameters lists the position and the ordinary ' +
        'parameters - not the argument, the amplitude or the width. Ticking ' +
        'the position parameter makes the fit leave it where it was picked; unticking it lets the fit move it. Ticking any ' +
        'other parameter makes it shared: one value, varied by the fit and held ' +
        'equal across the curves of an interval.');
    AddParagraph(Result, 'Held parameters lists the shared and the ordinary ' +
        'parameters. Ticking one holds it at its starting value: the fit does ' +
        'not vary it. The tick is kept with the curve and in the project.');
    AddParagraph(Result, 'To give a starting value, select a parameter in Shared ' +
        'parameters, type the value in Initial value and press Apply. A value ' +
        'that is not a number is refused with a message.');
    AddParagraph(Result, 'Done saves the roles and selects the curve type. ' +
        '<< Backtrack discards this draft and returns to the formula dialog. ' +
        'Cancel closes the dialog and removes the draft.');
    AddParagraph(Result, 'Once the curves are placed, a value can also be typed ' +
        'into the Curve Attributes table.');
    AddRelated(Result, ParameterKindsTopic);
    AddRelated(Result, UserCurveFormulaTopic);
    AddRelated(Result, UserCurvesTopic);
end;

function UserCurveLibrary: TExplanation;
begin
    Result := NewExplanation(UserCurveLibraryTopic,
        'Selecting and deleting user-defined curves',
        'The curve types you have defined are chosen and deleted under ' +
        'Model > Curve Type > User.', esModelChoice);
    AddParagraph(Result, 'Below New User Curve and a separator, the User group ' +
        'lists every curve type you have defined. Choosing one selects it, and it ' +
        'is ticked while in use. New User Curve itself is never ticked: it names ' +
        'no curve, it makes one.');
    AddParagraph(Result, 'Model > Curve Type > User > Delete User Curve opens a ' +
        'list of the same curve types. Choosing one deletes it, and its file, at ' +
        'once and without asking. The entry is there only while at least one ' +
        'curve type of your own exists.');
    AddParagraph(Result, 'If you delete the curve type in use, the program says ' +
        'so: the model is left with no curve type, and a fit is refused until you ' +
        'choose another under Model > Curve Type.');
    AddParagraph(Result, 'A curve type saved without a formula - by an older ' +
        'version, or by a session interrupted while defining it - cannot be ' +
        'selected; choosing it says so and suggests deleting it and making it ' +
        'again.');
    AddLimitation(Result, 'Deleting cannot be undone.');
    AddRelated(Result, UserCurvesTopic);
    AddRelated(Result, CurveTypeTopic);
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
    AppendExplanation(Result, Background);
    AppendExplanation(Result, BackgroundCurve);
    AppendExplanation(Result, BackgroundSubtraction);
    AppendExplanation(Result, BackgroundFraction);
    AppendExplanation(Result, BackgroundVariation);
    AppendExplanation(Result, ModelPanel);
    AppendExplanation(Result, CurveAttributes);
    AppendExplanation(Result, ParameterKinds);
    AppendExplanation(Result, ParameterLimits);
    AppendExplanation(Result, DeleteCurve);
    AppendExplanation(Result, ClearModel);
    AppendExplanation(Result, ModelHistory);
    AppendExplanation(Result, UserCurves);
    AppendExplanation(Result, UserCurveFormula);
    AppendExplanation(Result, UserCurveRoles);
    AppendExplanation(Result, UserCurveLibrary);
end;

end.
