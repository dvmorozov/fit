// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide's chapter on the main window: its parts, the chart,
the legend, the Model panel and Explain pane, the tables, the Edit and View
menus and the status bar.)

WHAT IT COVERS. Where things are and what each part of the window does when it
is touched, for someone who has never seen the program: the two tabs on each
side of the chart, the chart's gestures and readout, the five tables along the
bottom and their toolbars, the Edit menu and what it acts on, the status bar,
screen scaling and what is remembered between sessions. What a command DOES to
the model belongs to the chapters on data, the model and fitting; this one says
where the command is and what the window shows for it.

CHECKED AGAINST THE WINDOW, not against a screenshot: the layout is partly
designed (form_main.lfm) and partly built in code (the Tools, Data, Graphs and
Model tabs, the Explain pane, the report tabs), and every caption quoted here is
the one the code sets. The decisions behind the surfaces are in the units the
comments name - action_state for what is greyed, chart_panning and TAGraph for
the gestures, series_style, series_palette and legend_layout for the legend,
model_outline and explanation_focus for the Model panel and the Explain pane,
points_tables, grid_edit and summary_table for the tables.

LINKS stay inside this chapter, the fitting chapter and the fit-progress topics,
written as literals so no guide unit depends on another.
}
unit guide_window;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    WindowNamespace = 'the-window';
    WindowLayoutTopic = 'the-window/layout';
    ToolsTabTopic = 'the-window/tools-tab';
    ChartTopic = 'the-window/chart';
    ZoomTopic = 'the-window/zoom-and-scroll';
    ViewMarkersTopic = 'the-window/view-markers';
    GraphsTabTopic = 'the-window/graphs-tab';
    ModelPanelTopic = 'the-window/model-panel';
    ExplainPaneTopic = 'the-window/explain-pane';
    ExplainEverythingTopic = 'the-window/explain-everything';
    TablesTopic = 'the-window/tables';
    BackgroundPointsTableTopic = 'the-window/background-points-table';
    FitIntervalsTableTopic = 'the-window/fit-intervals-table';
    CurvePositionsTableTopic = 'the-window/curve-positions-table';
    CurveAttributesTableTopic = 'the-window/curve-attributes-table';
    SummaryTableTopic = 'the-window/summary-table';
    EditMenuTopic = 'the-window/edit-menu';
    StatusBarTopic = 'the-window/status-bar';
    ScreenScalingTopic = 'the-window/screen-scaling';
    RememberedSettingsTopic = 'the-window/remembered-settings';
    ModulesInTheWindowTopic = 'the-window/modules';

{ Every topic of the chapter, in reading order. }
function WindowExplanations: TExplanations;

implementation

uses
    static_explanations;

const
    Bullet = #$E2#$80#$A2 + ' ';
    PlusMinus = #$C2#$B1;

    //  The fit-progress topics, and topics of the fitting chapter
    //  (guide_fitting), by literal.
    AnimationModeTopic = 'fit-progress/animation-mode';
    LiveProgressTopic = 'fit-progress/live-progress';
    FitResultTopic = 'fitting/when-a-fit-ends';
    FitAdviceTopic = 'fitting/what-the-fit-will-do';
    MinimizeDifferenceTopic = 'fitting/minimize-difference';
    RFactorTopic = 'fitting/r-factor';
    MinimizerTopic = 'fitting/minimizer';

function WindowLayout: TExplanation;
begin
    Result := NewExplanation(WindowLayoutTopic, 'The main window',
        'Data and tools on the left, the chart in the middle, the legend and ' +
        'the model on the right, tables along the bottom and a status bar ' +
        'under them.', esModelChoice);
    AddParagraph(Result, 'The window is divided into five parts:');
    AddParagraph(Result, Bullet + 'On the left, two tabs. Tools holds the list ' +
        'of curve types and the buttons that build and fit the model. Data ' +
        'holds the measured profile as a table of Position and Amplitude ' +
        'values - the one table in the window you can type into.');
    AddParagraph(Result, Bullet + 'In the middle, the chart: the data and the ' +
        'model drawn over it, with scroll bars below and to the right for ' +
        'moving around a zoomed view. While a fit runs, the chart area shows ' +
        'the fit''s progress instead.');
    AddParagraph(Result, Bullet + 'On the right, two tabs. Graphs is the ' +
        'legend: one row for each thing drawn, with a tick to show or hide it. ' +
        'Model lists the curves of the model, with the Explain pane under the ' +
        'list. Below the tabs, Position and Intensity give the coordinates of ' +
        'the chart point nearest the pointer.');
    AddParagraph(Result, Bullet + 'Along the bottom, five tables on tabs of ' +
        'their own: Background Points, Fit Intervals, Curve Positions, Curve ' +
        'Attributes and Summary.');
    AddParagraph(Result, Bullet + 'At the very bottom, the status bar.');
    AddParagraph(Result, 'Above the chart is a toolbar with four buttons: ' +
        'Import Profile (File > Import Profile), Zoom In and Zoom Out (View > ' +
        'Zoom In and View > Zoom Out), and Automatically (Fit > ' +
        'Automatically). Hovering over any button shows what it does in a ' +
        'tooltip and in the status bar.');
    AddParagraph(Result, 'The borders between the parts - left and right of ' +
        'the chart, above the tables, between the curve-type list and the ' +
        'buttons, and above the Explain pane - can be dragged to give any part ' +
        'more room.');
    AddParagraph(Result, 'A build that contains a module can add a submenu, a ' +
        'group of buttons, rows in the Model panel and a tab of its own at the ' +
        'bottom.');
    AddLimitation(Result, 'The size and position of the window, and the ' +
        'room given to each part, are not remembered between sessions.');
    AddRelated(Result, ToolsTabTopic);
    AddRelated(Result, ChartTopic);
    AddRelated(Result, GraphsTabTopic);
    AddRelated(Result, ModelPanelTopic);
    AddRelated(Result, TablesTopic);
    AddRelated(Result, StatusBarTopic);
end;

function ToolsTab: TExplanation;
begin
    Result := NewExplanation(ToolsTabTopic, 'The Tools tab',
        'The Tools tab holds the list of curve types and a group of buttons ' +
        'for each step of building and fitting a model.', esModelChoice);
    AddParagraph(Result, 'The list at the top offers every curve type of this ' +
        'build, grouped under headings - the same types as Model > Curve Type. ' +
        'Click one to make it the type every curve of the model is made of. ' +
        'Click the selected type again and the Explain pane on the Model tab ' +
        'explains it. Hovering over a type shows its one-line summary.');
    AddParagraph(Result, 'Under it are the buttons, in the order the work is ' +
        'done. Each group''s heading shows how many items it holds now:');
    AddParagraph(Result, Bullet + 'Positions (n): Pick starts or stops placing ' +
        'curve positions with the mouse, Auto finds them in the data, Clear ' +
        'removes them all - the commands of Model > Curve Positions.');
    AddParagraph(Result, Bullet + 'Fit intervals (n): Pick, Auto and Clear do ' +
        'the same for the stretches of the profile a fit scores - Model > Fit ' +
        'Intervals.');
    AddParagraph(Result, Bullet + 'Background (n): Pick, Auto and Clear for the ' +
        'background points (Model > Background > Points), and Subtract, which ' +
        'subtracts a background found from the data (Model > Background > ' +
        'Subtract > Automatically).');
    AddParagraph(Result, Bullet + 'Fit: Fit fits the model as it stands - the ' +
        'same as Fit > Minimize Difference - and Stop ends a running ' +
        'computation - Fit > Stop.');
    AddParagraph(Result, 'A Pick button stays pressed while its picking mode ' +
        'runs; press it again to stop. Every button follows the menu entry it ' +
        'stands for: when the entry is greyed, so is the button, and both show ' +
        'the same hint.');
    AddParagraph(Result, 'A module can add groups of its own below these.');
    AddLimitation(Result, 'Only the commonest commands have a button. Setting ' +
        'the background fraction, choosing an interval of the data, and the ' +
        'fit settings are in the menus only.');
    AddRelated(Result, WindowLayoutTopic);
    AddRelated(Result, MinimizeDifferenceTopic);
    AddRelated(Result, ExplainPaneTopic);
end;

function Chart: TExplanation;
begin
    Result := NewExplanation(ChartTopic, 'The chart',
        'The chart draws the measured profile and everything the model adds ' +
        'to it, and shows the coordinates of the point nearest the pointer.',
        esModelChoice);
    AddParagraph(Result, 'Everything drawn is listed on the Graphs tab, where ' +
        'each can be hidden: the profile; each curve of the model; their sum, ' +
        'Total Amplitude; the Difference between data and model; the ' +
        'background points; the fit interval bounds, drawn as lines down from ' +
        'the top; the curve positions you picked, drawn as crosses, and the ' +
        'fitted positions where the fit put the curves, drawn as circles; and ' +
        'points being picked, drawn as lines up from the bottom.');
    AddParagraph(Result, 'A crosshair follows the pointer, snapping to the ' +
        'nearest drawn point. Position and Intensity, under the tabs on the ' +
        'right, show that point''s two coordinates, to two decimal places.');
    AddParagraph(Result, 'A left click on the chart places a point while one ' +
        'of the picking modes is on - see the Pick buttons of the Tools tab. A ' +
        'right click opens the menu of argument axes, the same choices as Data ' +
        '> Argument Transformation > Use Rule.');
    AddParagraph(Result, 'Dragging with the mouse zooms, and the scroll bars ' +
        'move a zoomed view; see "Zooming and scrolling the chart".');
    AddParagraph(Result, 'While a fit runs, the chart gives way to a chart of ' +
        'the fit''s progress. With View > Animation Mode ticked, it shows the ' +
        'model moving over the data instead.');
    AddLimitation(Result, 'The readout shows the coordinates of the nearest ' +
        'drawn point, not of the pointer itself, so it cannot be used to read ' +
        'a value between points.');
    AddRelated(Result, ZoomTopic);
    AddRelated(Result, ViewMarkersTopic);
    AddRelated(Result, GraphsTabTopic);
    AddRelated(Result, AnimationModeTopic);
    AddRelated(Result, LiveProgressTopic);
end;

function Zoom: TExplanation;
begin
    Result := NewExplanation(ZoomTopic, 'Zooming and scrolling the chart',
        'Drag a rectangle from its top-left corner to its bottom-right corner ' +
        'to zoom into it; drag in any other direction to see everything again.',
        esModelChoice);
    AddParagraph(Result, 'With the mouse: press the button at the top-left ' +
        'corner of the part you want to see, drag to its bottom-right corner ' +
        'and release. A rectangle follows the pointer while you drag. Dragging ' +
        'in any other direction brings back the whole of what is drawn. The ' +
        'status bar reminds you: "Drag mouse from top-left to bottom-right to ' +
        'zoom".');
    AddParagraph(Result, 'View > Zoom In narrows the view by a tenth of its ' +
        'width and height on every side, keeping its centre; View > Zoom Out ' +
        'widens it by the same amount. Both are also buttons on the toolbar ' +
        'above the chart, and both are greyed while nothing is drawn.');
    AddParagraph(Result, 'When the chart is zoomed, the scroll bars below and ' +
        'to the right of it move the view along each axis without changing its ' +
        'size.');
    AddLimitation(Result, 'Zooming changes only what the chart shows. It does ' +
        'not select an interval of the data and does not change what is ' +
        'fitted; the Data > Range commands do that.');
    AddRelated(Result, ChartTopic);
end;

function ViewMarkers: TExplanation;
begin
    Result := NewExplanation(ViewMarkersTopic, 'View Markers',
        'View > View Markers marks every point of the profile and of the model ' +
        'curves, not only the line through them.', esModelChoice);
    AddParagraph(Result, 'Without it, the profile, each curve, Total Amplitude ' +
        'and Difference are drawn as lines. With View > View Markers ticked, ' +
        'each of their points gets a small square as well - hollow on the ' +
        'profile, so the model drawn over it stays visible.');
    AddParagraph(Result, 'Series that consist of markers alone - curve ' +
        'positions, fitted positions, fit interval bounds and picked points - ' +
        'are always drawn and are not affected. The background points, circles ' +
        'joined by a line, follow the tick like the profile.');
    AddParagraph(Result, 'The tick starts cleared in every session, and the ' +
        'entry is greyed while nothing is drawn.');
    AddLimitation(Result, 'On a densely sampled profile the markers touch and ' +
        'the line reads as a band; zoom in to see single points.');
    AddRelated(Result, ChartTopic);
    AddRelated(Result, GraphsTabTopic);
end;

function GraphsTab: TExplanation;
begin
    Result := NewExplanation(GraphsTabTopic, 'The Graphs tab',
        'The Graphs tab is the chart''s legend: one row per series, with a tick ' +
        'to show or hide it and a square of its colour.', esModelChoice);
    AddParagraph(Result, 'Each row names one series: Profile, Total Amplitude, ' +
        'Difference, Background points, Fit intervals, Curve positions, Fitted ' +
        'positions, Selected interval while an interval of the data is ' +
        'selected, and each curve of the model by its type and number, such as ' +
        '"Gaussian [2]". The square at the right-hand end of a row is the ' +
        'colour the series is drawn in.');
    AddParagraph(Result, 'Clear a row''s tick to hide that series from the ' +
        'chart, and tick it again to show it. The space bar toggles the ' +
        'selected row.');
    AddParagraph(Result, 'The curves of the model take their colours from a ' +
        'palette of sixteen, in order, so the seventeenth curve has the ' +
        'colour of the first.');
    AddParagraph(Result, 'When a curve is selected - in the Model panel or in ' +
        'the Curve Attributes table - it is drawn three times as thick and on ' +
        'top of every other series, and its row here turns bold.');
    AddLimitation(Result, 'Hiding a series only stops it being drawn. A hidden ' +
        'curve is still part of the model and of every fit.');
    AddLimitation(Result, 'The colours are fixed; they cannot be chosen.');
    AddRelated(Result, ChartTopic);
    AddRelated(Result, ModelPanelTopic);
end;

function ModelPanel: TExplanation;
begin
    Result := NewExplanation(ModelPanelTopic, 'The Model panel',
        'The Model tab lists the curves the model is made of, one row per ' +
        'curve, with the Explain pane under the list.', esModelChoice);
    AddParagraph(Result, 'For a model built from curve positions, each row is ' +
        'one curve: its type and number, and where it sits - for example ' +
        '"Gaussian [1]  at 23.5". The list fills as soon as positions are ' +
        'placed, before any fit, and is rebuilt when a fit ends.');
    AddParagraph(Result, 'Select a row to pick out that curve: it is drawn ' +
        'thick and on top on the chart, its legend row turns bold, and the ' +
        'Explain pane shows its curve type. Selecting a row of the Curve ' +
        'Attributes table selects the same curve here.');
    AddParagraph(Result, 'Right-click a row for what can be done to that ' +
        'curve; Delete curve removes it from the model, together with the ' +
        'position it was placed from. The entries are greyed over a row that ' +
        'names no curve.');
    AddParagraph(Result, 'A curve type that is placed from markup of its own - ' +
        'a pattern a module provides, for example - fills the panel itself, ' +
        'possibly as a tree, and may offer commands of its own on the ' +
        'right-click menu.');
    AddParagraph(Result, 'An empty panel says why it is empty: "Open a data ' +
        'file to start building a model.", "No curves yet - place some with ' +
        'Positions on the Tools tab." or, for a type placed from its own ' +
        'markup, "No curves yet - this curve type is placed from its own ' +
        'markup."');
    AddLimitation(Result, 'While a fit runs the panel is not rebuilt: it shows ' +
        'the model as it was when the fit started until the fit ends.');
    AddRelated(Result, ExplainPaneTopic);
    AddRelated(Result, CurveAttributesTableTopic);
    AddRelated(Result, GraphsTabTopic);
end;

function ExplainPane: TExplanation;
begin
    Result := NewExplanation(ExplainPaneTopic, 'The Explain pane',
        'The pane under the Model panel explains what you are looking at: the ' +
        'selected curve, the curve type you clicked, or an entry of the Model ' +
        'panel''s right-click menu.', esModelChoice);
    AddParagraph(Result, 'It shows one explanation at a time: what the thing ' +
        'is, what stands behind it - the field''s own sources, common practice ' +
        'or a choice of this program - what it does not cover, where to read ' +
        'more, and related topics. A related topic opens in the pane; a ' +
        'reference to the literature opens in the web browser.');
    AddParagraph(Result, 'Which topic it shows:');
    AddParagraph(Result, Bullet + 'While the Model panel''s right-click menu is ' +
        'open, the entry under the pointer, when that entry has an ' +
        'explanation.');
    AddParagraph(Result, Bullet + 'Otherwise whichever you chose last: the row ' +
        'selected in the Model panel - its curve''s type - or the curve type ' +
        'you chose, in the list on the Tools tab or under Model > Curve Type. ' +
        'If that one ' +
        'cannot be explained any more, the other.');
    AddParagraph(Result, Bullet + 'After you follow a link, the topic it led ' +
        'to.');
    AddParagraph(Result, Bullet + 'With nothing to show: "Select a curve type, ' +
        'a row of the Model panel or a menu entry to see it explained."');
    AddParagraph(Result, 'Every topic it can show is also listed by Help > ' +
        'Explain Everything.');
    AddLimitation(Result, 'On macOS the pane cannot follow the pointer through ' +
        'a menu: the system does not report which entry the pointer rests on. ' +
        'Select a row or a curve type instead.');
    AddLimitation(Result, 'Only the Model panel''s right-click menu reports the ' +
        'entry under the pointer. The entries of the main menus show their ' +
        'hint in the status bar instead.');
    AddRelated(Result, ModelPanelTopic);
    AddRelated(Result, ExplainEverythingTopic);
end;

function ExplainEverything: TExplanation;
begin
    Result := NewExplanation(ExplainEverythingTopic, 'Explain Everything',
        'Help > Explain Everything opens every explanation this build can give: ' +
        'this guide, every curve type and every rule a module adds.',
        esModelChoice);
    AddParagraph(Result, 'The window it opens lists every topic on the left, ' +
        'under a heading for each chapter: first the chapters of this guide ' +
        'and Fit progress, then the curve types and what the build''s modules ' +
        'explain. Within a chapter the topics are in alphabetical order.');
    AddParagraph(Result, 'Click a topic to read it on the right. Links in it ' +
        'lead to related topics; references to the literature open in the web ' +
        'browser.');
    AddParagraph(Result, 'The window stays open beside the main one, so it can ' +
        'be read while you work.');
    AddLimitation(Result, 'It lists what this build can explain: a module that ' +
        'is not part of the build adds nothing to it.');
    AddRelated(Result, ExplainPaneTopic);
    AddRelated(Result, ModulesInTheWindowTopic);
end;

function Tables: TExplanation;
begin
    Result := NewExplanation(TablesTopic, 'The tables at the bottom',
        'Five tabs along the bottom show the model''s inputs and results as ' +
        'numbers: Background Points, Fit Intervals, Curve Positions, Curve ' +
        'Attributes and Summary.', esModelChoice);
    AddParagraph(Result, 'Each tab has a small toolbar at its left edge; ' +
        'hovering over a button says what it does.');
    AddParagraph(Result, Bullet + 'Background Points - the points the ' +
        'background is drawn through.');
    AddParagraph(Result, Bullet + 'Fit Intervals - the stretches of the ' +
        'profile a fit scores.');
    AddParagraph(Result, Bullet + 'Curve Positions - where the curves were ' +
        'placed.');
    AddParagraph(Result, Bullet + 'Curve Attributes - every parameter of every ' +
        'curve.');
    AddParagraph(Result, Bullet + 'Summary - the data, the model and each ' +
        'curve at every point of every fit interval.');
    AddParagraph(Result, 'These tables are filled by the program. To change ' +
        'what they hold, pick on the chart or use the buttons and the Model ' +
        'menu; the profile on the Data tab is the one table you type into.');
    AddParagraph(Result, 'To take numbers to a spreadsheet, click in a table ' +
        'and use Edit > Select All and Edit > Copy to Clipboard.');
    AddParagraph(Result, 'A saved project remembers which of these tabs was in ' +
        'front.');
    AddRelated(Result, BackgroundPointsTableTopic);
    AddRelated(Result, FitIntervalsTableTopic);
    AddRelated(Result, CurvePositionsTableTopic);
    AddRelated(Result, CurveAttributesTableTopic);
    AddRelated(Result, SummaryTableTopic);
    AddRelated(Result, EditMenuTopic);
end;

function BackgroundPointsTable: TExplanation;
begin
    Result := NewExplanation(BackgroundPointsTableTopic,
        'The Background Points table',
        'Lists the background points by Position and Amplitude, beside the ' +
        'buttons that find, subtract and remove them.', esModelChoice);
    AddParagraph(Result, 'Each row is one point of the background, picked on ' +
        'the chart or found from the data.');
    AddParagraph(Result, 'The toolbar, from the top:');
    AddParagraph(Result, Bullet + 'Compute background points automatically - ' +
        'Model > Background > Points > Compute Automatically.');
    AddParagraph(Result, Bullet + 'Subtract a background found from the data - ' +
        'Model > Background > Subtract > Automatically.');
    AddParagraph(Result, Bullet + 'Subtract the background through the points ' +
        'in the table - Model > Background > Subtract > By Selected Points. ' +
        'With fewer than two points, it finds them from the data first.');
    AddParagraph(Result, Bullet + 'Remove the background points - Model > ' +
        'Background > Points > Remove All.');
    AddParagraph(Result, 'A point can also be moved by typing: change its ' +
        'Position or its Amplitude and leave the cell, and the point moves, on ' +
        'the chart and in the engine.');
    AddRelated(Result, TablesTopic);
end;

function FitIntervalsTable: TExplanation;
begin
    Result := NewExplanation(FitIntervalsTableTopic, 'The Fit Intervals table',
        'Lists each fit interval by its Starting Position and Final Position.',
        esModelChoice);
    AddParagraph(Result, 'Intervals are picked as pairs of bounds on the ' +
        'chart, and each row is one pair. While you are halfway through ' +
        'picking one, the last row has a Starting Position and an empty Final ' +
        'Position.');
    AddParagraph(Result, 'Only positions are shown: an interval is a stretch of ' +
        'the horizontal axis, and the height at its bounds means nothing.');
    AddParagraph(Result, 'The toolbar has two buttons: compute fit interval ' +
        'bounds automatically - Model > Fit Intervals > Compute Automatically - ' +
        'and remove all fit interval bounds - Model > Fit Intervals > Remove ' +
        'All.');
    AddParagraph(Result, 'A bound can be moved by typing: type a new position ' +
        'over it and leave the cell. It goes to the data point nearest what you ' +
        'typed, and the curves are rebuilt over the changed interval.');
    AddLimitation(Result, 'A bound moves only between the bounds beside it: ' +
        'past one, the intervals would pair up differently. A position outside ' +
        'the profile, or text that is not a number, is refused with a message.');
    AddRelated(Result, TablesTopic);
    AddRelated(Result, RFactorTopic);
end;

function CurvePositionsTable: TExplanation;
begin
    Result := NewExplanation(CurvePositionsTableTopic,
        'The Curve Positions table',
        'Lists where curves are placed: the positions you picked, or where the ' +
        'fit put the curves when you picked none.', esModelChoice);
    AddParagraph(Result, 'Each row is one position, by Position and ' +
        'Amplitude. When you have picked positions, those are shown, because ' +
        'they are what you put there and can change. When you have picked none ' +
        '- a model placed from a module''s markup, for example - the positions ' +
        'of the model''s curves are shown instead.');
    AddParagraph(Result, 'The toolbar has two buttons: compute curve positions ' +
        'automatically - Model > Curve Positions > Compute Automatically - and ' +
        'remove all curve positions - Model > Curve Positions > Remove All.');
    AddParagraph(Result, 'A picked position can be moved by typing: type a new ' +
        'Position over it and leave the cell. It goes to the data point nearest ' +
        'what you typed, and its curve moves with it, keeping the shape a fit ' +
        'found for it. The Amplitude is the profile''s value there and is not ' +
        'typed.');
    AddLimitation(Result, 'A position already picked, one outside the profile, ' +
        'or text that is not a number is refused with a message. Positions shown ' +
        'when none were picked are where the model''s curves sit, and are moved ' +
        'by fitting rather than by typing.');
    AddParagraph(Result, 'On the chart the picked positions are crosses and the ' +
        'fitted positions circles; after a good fit they sit close together.');
    AddRelated(Result, TablesTopic);
    AddRelated(Result, ChartTopic);
end;

function CurveAttributesTable: TExplanation;
begin
    Result := NewExplanation(CurveAttributesTableTopic,
        'The Curve Attributes table',
        'One row per curve and one column per parameter, holding the values ' +
        'the last fit found.', esModelChoice);
    AddParagraph(Result, 'The columns are the parameters of the curves in the ' +
        'model. A cell is left blank where a curve''s type has no such ' +
        'parameter. Values are shown to four decimal places; when the engine ' +
        'estimated an uncertainty - the Python engine does - it follows the ' +
        'value as "' + PlusMinus + ' error".');
    AddParagraph(Result, 'The colour of a cell shows how the value is ' +
        'treated, and the key under the table names the colours:');
    AddParagraph(Result, Bullet + 'Fitted - varied by the fit to match the ' +
        'data.');
    AddParagraph(Result, Bullet + 'Shared - varied by the fit, but held to one ' +
        'value across the curves of an interval.');
    AddParagraph(Result, Bullet + 'Fixed - set when the curve was placed and ' +
        'not varied.');
    AddParagraph(Result, Bullet + 'Computed - not fitted and not entered: it ' +
        'follows from the other parameters.');
    AddParagraph(Result, 'Click a row to select that curve: it is highlighted ' +
        'on the chart and selected in the Model panel. Select one or more rows ' +
        'and use Edit > Delete to remove those curves from the model.');
    AddParagraph(Result, 'The toolbar''s first button writes the table to a ' +
        'text file, like File > Export > Curve Parameters; its Copy to Clipboard ' +
        'button copies the selected rows, like Edit > Copy to Clipboard.');
    AddLimitation(Result, 'The values cannot be edited here; they are what the ' +
        'model holds, and a fit changes them.');
    AddRelated(Result, TablesTopic);
    AddRelated(Result, EditMenuTopic);
    AddRelated(Result, ModelPanelTopic);
    AddRelated(Result, MinimizerTopic);
end;

function SummaryTable: TExplanation;
begin
    Result := NewExplanation(SummaryTableTopic, 'The Summary table',
        'For every point inside every fit interval: the measured value, the ' +
        'model''s value, their difference and each curve''s own contribution.',
        esModelChoice);
    AddParagraph(Result, 'The columns are Position, Amplitude (the measured ' +
        'value), Total Amplitude (the model''s value), Difference, and then ' +
        'one column for each curve. Each fit interval starts with a heading ' +
        'row of its own.');
    AddParagraph(Result, 'A curve''s column is filled only on the rows where ' +
        'the curve has points, so a curve covering part of an interval leaves ' +
        'the rest of its column blank.');
    AddParagraph(Result, 'The table is empty until the model has both curves ' +
        'and fit intervals, and it is refilled when a fit ends.');
    AddParagraph(Result, 'The toolbar''s first button writes the table to a ' +
        'text file, like File > Export > Summary Table; its Copy to Clipboard ' +
        'button copies the selected rows, like Edit > Copy to Clipboard.');
    AddLimitation(Result, 'Points outside every fit interval are not listed.');
    AddRelated(Result, TablesTopic);
    AddRelated(Result, FitResultTopic);
end;

function EditMenu: TExplanation;
begin
    Result := NewExplanation(EditMenuTopic, 'The Edit menu',
        'Edit > Copy to Clipboard, Edit > Delete and Edit > Select All act on ' +
        'the table that has the keyboard focus.', esModelChoice);
    AddParagraph(Result, 'Click in a table first: one of the tables at the ' +
        'bottom, or the Data table on the left. With no table in focus, all ' +
        'three entries are greyed.');
    AddParagraph(Result, Bullet + 'Edit > Copy to Clipboard copies the selected ' +
        'cells as text, one line per row with the cells separated by tabs, ' +
        'ready to paste into a spreadsheet. It needs a selection: a single cell ' +
        'is only where the cursor is. The Copy to Clipboard buttons on the ' +
        'Curve Attributes and Summary toolbars do the same.');
    AddParagraph(Result, Bullet + 'Edit > Delete removes curves from the model. ' +
        'It is offered only in the Curve Attributes table, with one or more ' +
        'rows selected; the curve of each selected row is removed together with ' +
        'the position it was placed from - the same as Delete curve on the ' +
        'Model panel''s right-click menu.');
    AddParagraph(Result, Bullet + 'Edit > Select All selects the whole table, ' +
        'ready to copy. It is greyed when everything is already selected.');
    AddLimitation(Result, 'There is no undo: a curve removed with Edit > Delete ' +
        'is gone. Save the project first if you may want it back.');
    AddLimitation(Result, 'Edit > Delete never removes data points, background ' +
        'points, positions or intervals; those are removed with the Clear and ' +
        'Remove All commands.');
    AddRelated(Result, TablesTopic);
    AddRelated(Result, CurveAttributesTableTopic);
end;

function StatusBar: TExplanation;
begin
    Result := NewExplanation(StatusBarTopic, 'The status bar',
        'Four panels along the bottom: the elapsed time, the fit''s figures, a ' +
        'hint, and what the next fit will do.', esModelChoice);
    AddParagraph(Result, 'From left to right:');
    AddParagraph(Result, Bullet + 'Elapsed time: how long the last computation ' +
        'ran, as days, then hours, minutes and seconds. Only time the computer ' +
        'was awake is counted; sleep and hibernation are left out.');
    AddParagraph(Result, Bullet + 'The fit''s figures. While a fit runs, ' +
        '"R-factor:" and the value reached. When it ends, "Reduced Chi2:" and ' +
        '"R2:" for the result - the reduced chi-squared, with each point ' +
        'weighted by one over the square root of its value, and R-squared. ' +
        'Empty when they cannot be computed.');
    AddParagraph(Result, Bullet + 'A hint: what the pointer is over, or what ' +
        'the program expects next, such as "Drag mouse from top-left to ' +
        'bottom-right to zoom" or "Calculation started. Please wait".');
    AddParagraph(Result, Bullet + 'What the next fit will do: the objective, and ' +
        'which engine when the Python one is selected, as they will actually be ' +
        'used - for example "Minimising R-factor.". When a choice of yours was ' +
        'changed, hovering over the status bar explains why.');
    AddRelated(Result, FitAdviceTopic);
    AddRelated(Result, FitResultTopic);
    AddRelated(Result, RFactorTopic);
end;

function ScreenScaling: TExplanation;
begin
    Result := NewExplanation(ScreenScalingTopic, 'Screen scaling',
        'The window scales itself to the screen''s pixel density; the /DPI ' +
        'switch or the FIT_UI_DPI variable sets the density when the guess is ' +
        'wrong.', esModelChoice);
    AddParagraph(Result, 'Normally the density is what the system reports, and ' +
        'nothing needs doing.');
    AddParagraph(Result, 'A Linux build that uses GTK2 cannot see a scaled ' +
        'desktop - it reports 96 dots per inch for a 200 % display. There the ' +
        'program looks in turn at GDK_SCALE, QT_SCALE_FACTOR and the Xft.dpi ' +
        'setting of the X server, and uses the first it finds.');
    AddParagraph(Result, 'To set the density yourself, start the program with ' +
        '/DPI=<ppi> - for example /DPI=192 for a 200 % display - or set the ' +
        'environment variable FIT_UI_DPI to the same number. The switch wins ' +
        'over the variable, and both win over everything the program would ' +
        'otherwise detect.');
    AddLimitation(Result, 'A density outside 48 to 960 is ignored as a ' +
        'mistake, and one that is not a positive whole number is ignored too; ' +
        'the log records either.');
    AddRelated(Result, WindowLayoutTopic);
end;

function RememberedSettings: TExplanation;
begin
    Result := NewExplanation(RememberedSettingsTopic,
        'What the window remembers',
        'A few choices are kept between sessions in a settings file; ' +
        'everything about the model is kept in the project instead.',
        esModelChoice);
    AddParagraph(Result, 'Kept in a folder named Fit in your home folder - in ' +
        'config.xml, and the curve types you defined in files of their own ' +
        'beside it:');
    AddParagraph(Result, Bullet + 'the argument axis you chose, and the ' +
        'definition of a custom one;');
    AddParagraph(Result, Bullet + 'the selected curve type, and the curve types ' +
        'you defined yourself;');
    AddParagraph(Result, Bullet + 'the engine (Fit > Minimizer), the loss ' +
        'function (Fit > Loss Function) and the weighting;');
    AddParagraph(Result, Bullet + 'the compute server address (Fit > Compute ' +
        'Server);');
    AddParagraph(Result, Bullet + 'View > Animation Mode;');
    AddParagraph(Result, Bullet + 'the list of recent projects and the one ' +
        'last open.');
    AddParagraph(Result, 'Kept in the project file instead, and restored when ' +
        'the project is opened: the data, the model and its fit, the maximum ' +
        'acceptable difference, curve scaling, and which table tab was in ' +
        'front.');
    AddLimitation(Result, 'Not kept at all: the size and position of the ' +
        'window and of its parts, View > View Markers, and the zoom of the ' +
        'chart.');
    AddRelated(Result, WindowLayoutTopic);
    AddRelated(Result, ScreenScalingTopic);
end;

function ModulesInTheWindow: TExplanation;
begin
    Result := NewExplanation(ModulesInTheWindowTopic, 'Modules in the window',
        'A build that contains a module shows its commands in a submenu of ' +
        'Model and a group on the Tools tab, and can give it a table tab of ' +
        'its own.', esModelChoice);
    AddParagraph(Result, 'A module extends the program for one field or one ' +
        'kind of model. What it adds to the window:');
    AddParagraph(Result, Bullet + 'a submenu of the Model menu, named after the ' +
        'module;');
    AddParagraph(Result, Bullet + 'its commands as buttons on the Tools tab, in ' +
        'a group under the module''s heading or the heading of its submenu;');
    AddParagraph(Result, Bullet + 'rows of its own in the Model panel for the ' +
        'curve types it places from its own markup, and right-click commands ' +
        'over them;');
    AddParagraph(Result, Bullet + 'markers of its own on the chart;');
    AddParagraph(Result, Bullet + 'if it reports on the model, a tab after ' +
        'Summary along the bottom, captioned by the module. It stays hidden ' +
        'until the module has something to report, and its links lead to the ' +
        'parts of the model they concern;');
    AddParagraph(Result, Bullet + 'its own explanations, in Help > Explain ' +
        'Everything.');
    AddParagraph(Result, 'A build without modules shows none of this.');
    AddLimitation(Result, 'A module''s report tab is not restored as the tab in ' +
        'front when a project is opened, because it is still hidden at that ' +
        'moment.');
    AddRelated(Result, WindowLayoutTopic);
    AddRelated(Result, ExplainEverythingTopic);
end;

function WindowExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, WindowLayout);
    AppendExplanation(Result, ToolsTab);
    AppendExplanation(Result, Chart);
    AppendExplanation(Result, Zoom);
    AppendExplanation(Result, ViewMarkers);
    AppendExplanation(Result, GraphsTab);
    AppendExplanation(Result, ModelPanel);
    AppendExplanation(Result, ExplainPane);
    AppendExplanation(Result, ExplainEverything);
    AppendExplanation(Result, Tables);
    AppendExplanation(Result, BackgroundPointsTable);
    AppendExplanation(Result, FitIntervalsTable);
    AppendExplanation(Result, CurvePositionsTable);
    AppendExplanation(Result, CurveAttributesTable);
    AppendExplanation(Result, SummaryTable);
    AppendExplanation(Result, EditMenu);
    AppendExplanation(Result, StatusBar);
    AppendExplanation(Result, ScreenScaling);
    AppendExplanation(Result, RememberedSettings);
    AppendExplanation(Result, ModulesInTheWindow);
end;

end.
