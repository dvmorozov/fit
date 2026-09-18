// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The first chapter of the user guide: what Fit is, how it is started,
and how a first fit goes.)

WHY THIS CHAPTER EXISTS. Someone who has never used the program opens Help >
Explain Everything, or the published guide, and has to find out three things
before any command means anything to them: what the program is for, why it is
two programs, and what a first fit looks like from start to finish. The later
chapters explain each command in full; this one only walks the path through
them once, and says how to get help on the way.

WHAT IS CLAIMED HERE IS READ OFF THE CODE. The port comes from
http_fit_service.DEFAULT_SERVER_URL and launcher_rules, the standing labels from
explanation.StandingCaption, the About dialog from about_box_dialog. When one of
those changes, the sentence here that repeats it is wrong - so the sentences stay
at the level of what a user sees, and the details live in the chapter that owns
them.
}
unit guide_getting_started;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    GettingStartedNamespace = 'getting-started';
    WhatFitIsTopic = 'getting-started/what-fit-is';
    TwoProgramsTopic = 'getting-started/two-programs';
    MainWindowTopic = 'getting-started/main-window';
    FirstFitTopic = 'getting-started/first-fit';
    FittingByHandTopic = 'getting-started/fitting-by-hand';
    ExplainPaneTopic = 'getting-started/explain-pane';
    ExplainEverythingTopic = 'getting-started/explain-everything';
    StandingTopic = 'getting-started/standing';
    AboutTopic = 'getting-started/about';

{ Every topic of the chapter, in reading order. }
function GettingStartedExplanations: TExplanations;

implementation

uses
    static_explanations;   // AppendExplanation

const
    //  A paragraph that starts with U+2022 and a space is a bullet.
    Bullet = #$E2#$80#$A2 + ' ';

    //  Topics of the next chapter, written out rather than taken from its unit:
    //  a chapter does not use another chapter's unit.
    ProjectsTopic = 'projects-and-files/projects';
    ImportProfileTopic = 'projects-and-files/import-profile';
    SampleDataTopic = 'projects-and-files/sample-data';
    CommandLineTopic = 'projects-and-files/command-line';
    LogFilesTopic = 'projects-and-files/log-files';
    ExportCurveParametersTopic = 'projects-and-files/export-curve-parameters';
    SaveProjectTopic = 'projects-and-files/save-project';

    //  In the fitting chapter.
    AutomaticallyTopic = 'fitting/automatically';

function WhatFitIs: TExplanation;
begin
    Result := NewExplanation(WhatFitIsTopic, 'What Fit is',
        'Fit describes a measured profile as a sum of curves and tells you the ' +
        'parameters of each curve.', esModelChoice);
    AddParagraph(Result, 'A profile is a list of points: an argument, such as ' +
        'an angle, an energy or a time, and a measured value at each one. Where ' +
        'several peaks overlap, the profile alone does not say how large each ' +
        'peak is or where exactly it sits. Fit answers that by building a model - ' +
        'a set of curves of a chosen type - and adjusting the curves until their ' +
        'sum matches the data as closely as it can.');
    AddParagraph(Result, 'This is usually called curve fitting, or profile ' +
        'decomposition. The result is a position, a width, an amplitude and ' +
        'whatever else the curve type has, for every curve, together with how ' +
        'good the whole fit is.');
    AddParagraph(Result, 'The curve types this build offers are listed under ' +
        'Model > Curve Type and in the Tools tab: Gaussian, Lorentzian, ' +
        'Pseudo-Voigt and others, and curves you define yourself by a formula. ' +
        'You can place the curves by hand, or let Fit > Automatically decide how ' +
        'many curves the profile needs.');
    AddParagraph(Result, 'Fit is meant for research and for teaching alike. For ' +
        'research, a project file keeps everything needed to reproduce and ' +
        'continue a fit. For teaching, everything the program shows explains ' +
        'itself: the Explain pane and Help > Explain Everything say what a thing ' +
        'is, why it behaves as it does, and how much authority stands behind ' +
        'that.');
    AddLimitation(Result, 'Fit finds the curves that best match the data for ' +
        'the curve type and settings you chose. Whether those curves mean ' +
        'something physically is a judgement the program cannot make for you.');
    AddRelated(Result, TwoProgramsTopic);
    AddRelated(Result, FirstFitTopic);
    AddRelated(Result, StandingTopic);
end;

function TwoPrograms: TExplanation;
begin
    Result := NewExplanation(TwoProgramsTopic, 'The window and the compute server',
        'Fit is two programs: the window you work in, and a compute server, ' +
        'fit_server, that does every fit.', esModelChoice);
    AddParagraph(Result, 'The window holds your data, draws the chart and the ' +
        'tables, and sends every calculation to the compute server over HTTP. ' +
        'The window has no fitting engine of its own, so without a server it can ' +
        'show a profile but cannot fit anything.');
    AddParagraph(Result, 'By default the window looks for the server at ' +
        'http://127.0.0.1:8787, that is, on port 8787 of this computer. ' +
        'Fit > Compute Server changes that address; the address is remembered ' +
        'between sessions, and an empty one means the default. If nothing ' +
        'answers at the address you give, the window says so and names the ' +
        'address it tried.');
    AddParagraph(Result, 'You do not normally start the server yourself. The ' +
        'installed shortcut does not start the window directly but a small ' +
        'launcher: on Windows fit_launcher.exe, on Linux and macOS a launcher ' +
        'script. The launcher asks whether a server already answers on port ' +
        '8787. If none does, it starts fit_server in the background and waits up ' +
        'to ten seconds for it to say it is listening. Then it starts the window.');
    AddParagraph(Result, 'The server keeps running after you close the window. ' +
        'The next start finds it and is immediate, and a second window shares ' +
        'the same server instead of taking it away from the first in the middle ' +
        'of a fit.');
    AddParagraph(Result, 'A server can also run on another machine: start ' +
        'fit_server there with --host and --port, and give its address in ' +
        'Fit > Compute Server. Help > Compute Backends explains the engines the ' +
        'server can fit with, including the optional Python one.');
    AddLimitation(Result, 'A profile imported while no server answered stays ' +
        'in the window only. Naming a server that answers, through Fit > Compute ' +
        'Server, hands the profile over to it.');
    AddParagraph(Result, 'The environment variable FIT_PORT moves the server to ' +
        'another port: the launchers start it there, and the window looks for ' +
        'it there unless Fit > Compute Server names another address.');
    AddRelated(Result, LogFilesTopic);
    AddRelated(Result, CommandLineTopic);
end;

function MainWindow: TExplanation;
begin
    Result := NewExplanation(MainWindowTopic, 'The main window at a glance',
        'The window has a chart in the middle, tools on the left, the model on ' +
        'the right and tables of numbers along the bottom.', esModelChoice);
    AddParagraph(Result, 'From top to bottom and left to right:');
    AddParagraph(Result, Bullet + 'The menu bar: File, Data, Edit, View, Model, ' +
        'Fit and Help, and a menu of its own for any analysis module the build ' +
        'contains. The menus hold every command the program has.');
    AddParagraph(Result, Bullet + 'The toolbar: Import Profile, Zoom In, Zoom ' +
        'Out and Fit > Automatically, as buttons.');
    AddParagraph(Result, Bullet + 'On the left, the Tools tab, with the curve ' +
        'types and the everyday commands as buttons, and the Data tab, with the ' +
        'profile as a table of numbers.');
    AddParagraph(Result, Bullet + 'In the middle, the chart: the profile, the ' +
        'curves of the model, their sum and the difference between data and ' +
        'model. While a fit runs, this area shows how far the fit has got.');
    AddParagraph(Result, Bullet + 'On the right, the Graphs tab, listing the ' +
        'series on the chart, and the Model tab, listing the curves of the ' +
        'model, with the Explain pane below them.');
    AddParagraph(Result, Bullet + 'Along the bottom, the tables: Background ' +
        'Points, Fit Intervals, Curve Positions, Curve Attributes (the ' +
        'parameters of every curve) and Summary (data, model and difference at ' +
        'every point).');
    AddParagraph(Result, Bullet + 'The status bar: how long the last operation ' +
        'took, how good the fit is, and a hint about whatever the pointer is ' +
        'over.');
    AddParagraph(Result, 'The same command is often reachable in several ways - ' +
        'a menu entry, a toolbar button, a button in the Tools tab - and they all ' +
        'do the same thing.');
    AddRelated(Result, FirstFitTopic);
    AddRelated(Result, ExplainPaneTopic);
end;

function FirstFit: TExplanation;
begin
    Result := NewExplanation(FirstFitTopic, 'A first fit',
        'Import a profile, choose a curve type, run Fit > Automatically and read ' +
        'the result in the tables below the chart.', esModelChoice);
    AddParagraph(Result, Bullet + 'Import a profile with File > Import Profile ' +
        '(F2, or the first toolbar button). The sample files that come with Fit ' +
        'are a good start: Data/2.dat holds two overlapping peaks in about fifty ' +
        'points. The profile appears on the chart and in the Data tab.');
    AddParagraph(Result, Bullet + 'Choose a curve type in the list at the top of ' +
        'the Tools tab, or under Model > Curve Type. Pseudo-Voigt is a common ' +
        'choice for diffraction peaks. The choice is remembered for the next ' +
        'session.');
    AddParagraph(Result, Bullet + 'Run Fit > Automatically, or press its toolbar ' +
        'button. Fit subtracts the background unless that has already been ' +
        'done, places curves, marks a fit ' +
        'interval around each peak and then removes curves for as long as the ' +
        'fit stays good enough. While it works, the chart shows how far it has ' +
        'got; Fit > Stop ends it and keeps what it has reached.');
    AddParagraph(Result, Bullet + 'Read the result. The curves are drawn over ' +
        'the data, and the Difference series shows what the model does not ' +
        'explain: a systematic bump in it is a peak the model has missed. The ' +
        'Curve Attributes table lists the parameters of every curve, the ' +
        'Summary table lists data, model and difference point by point, and the ' +
        'status bar shows how long the fit took and figures of how good it is.');
    AddParagraph(Result, Bullet + 'Keep the work with File > Save Project, which ' +
        'writes everything needed to carry on later. To take the numbers to a ' +
        'spreadsheet, use File > Export > Curve Parameters or File > Export > ' +
        'Summary Table.');
    AddParagraph(Result, 'Fit > Automatically is greyed out until a profile has ' +
        'been imported or a project opened, and while another fit is running.');
    AddLimitation(Result, 'How many curves Fit > Automatically keeps is decided ' +
        'by one setting, Fit > Set Max Acceptable Difference, 0.01 percent by ' +
        'default. Your own data may need another value.');
    AddRelated(Result, AutomaticallyTopic);
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, SampleDataTopic);
    AddRelated(Result, FittingByHandTopic);
    AddRelated(Result, ExportCurveParametersTopic);
    AddRelated(Result, SaveProjectTopic);
    AddRelated(Result, 'fit-progress/live-progress');
end;

function FittingByHand: TExplanation;
begin
    Result := NewExplanation(FittingByHandTopic, 'Fitting by hand, step by step',
        'Instead of letting the program decide, you can say where the ' +
        'background is, what to fit and where the curves go, and then fit.',
        esModelChoice);
    AddParagraph(Result, 'Doing it by hand takes longer and gives you control. ' +
        'The steps below are an overview; each command has its own explanation.');
    AddParagraph(Result, Bullet + 'Background. Under Model > Background, either ' +
        'let the program find background points (Points > Compute ' +
        'Automatically) or pick them on the chart yourself (Points > Start ' +
        'Manual Selection), then subtract the background (Subtract > ' +
        'Automatically or Subtract > By Selected Points).');
    AddParagraph(Result, Bullet + 'Fit intervals. A fit interval is a stretch of ' +
        'the profile fitted as one problem. Under Model > Fit Intervals, compute ' +
        'them or pick their bounds on the chart. With no interval, the whole ' +
        'profile is one interval.');
    AddParagraph(Result, Bullet + 'Curve positions. Under Model > Curve ' +
        'Positions, compute them, put one at every point, or pick them on the ' +
        'chart - usually one at each peak.');
    AddParagraph(Result, Bullet + 'Curve type. Choose it under Model > Curve ' +
        'Type or in the Tools tab.');
    AddParagraph(Result, Bullet + 'Fit. Fit > Minimize Difference (F5) adjusts ' +
        'the curves you placed until their sum matches the data as well as it ' +
        'can. Fit > Minimize Number of Curves (F4) instead also removes curves ' +
        'that are not needed.');
    AddParagraph(Result, 'Then look at the Difference series, add or delete ' +
        'curves where it says something is missing or superfluous, and fit ' +
        'again: each fit starts from where the last one finished.');
    AddParagraph(Result, 'The Tools tab carries the same commands as buttons, ' +
        'which is quicker when you go round this loop many times.');
    AddLimitation(Result, 'While a fit runs, the commands that change the model ' +
        'are greyed out; only Fit > Stop is offered.');
    AddRelated(Result, FirstFitTopic);
    AddRelated(Result, MainWindowTopic);
end;

function ExplainPane: TExplanation;
begin
    Result := NewExplanation(ExplainPaneTopic, 'The Explain pane',
        'A pane below the model list that explains whatever you are looking at, ' +
        'as you look at it.', esModelChoice);
    AddParagraph(Result, 'The pane sits at the bottom of the Model tab, on the ' +
        'right of the window. It follows your attention: the curve type you ' +
        'chose, the row you selected in the model list, or a menu entry the ' +
        'pointer rests on while its menu is open, when that entry has an ' +
        'explanation.');
    AddParagraph(Result, 'Each explanation has a title, a one-sentence summary, ' +
        'a label saying how much authority stands behind it, the explanation ' +
        'itself, and where they apply its limitations, its references and ' +
        'related topics. A related topic is a link: following it shows that ' +
        'topic in the pane. A link to a web page opens in your browser.');
    AddParagraph(Result, 'When nothing explainable is in focus, the pane is ' +
        'empty. To browse everything instead, use Help > Explain Everything.');
    AddLimitation(Result, 'On macOS the system does not tell a program which ' +
        'menu entry the pointer rests on, so there the pane follows the curve ' +
        'type and the model list only.');
    AddRelated(Result, ExplainEverythingTopic);
    AddRelated(Result, StandingTopic);
end;

function ExplainEverything: TExplanation;
begin
    Result := NewExplanation(ExplainEverythingTopic, 'Explain Everything',
        'Help > Explain Everything opens a window listing every explanation ' +
        'this build can give, including this user guide.', esModelChoice);
    AddParagraph(Result, 'The list on the left is grouped under headings: the ' +
        'chapters of this guide first, in the order they are meant to be read, ' +
        'then what the curve types and any analysis modules explain about ' +
        'themselves. Within a heading the entries are in alphabetical order, so ' +
        'you can find one by eye.');
    AddParagraph(Result, 'Click an entry to read it on the right. Links to ' +
        'related topics open those topics in the same window; links to web ' +
        'pages open in your browser.');
    AddParagraph(Result, 'The window stays open beside the main window while you ' +
        'work, so you can keep an explanation in view while you try what it ' +
        'describes.');
    AddParagraph(Result, 'The same explanations are published as the user ' +
        'guide on the web, generated from what the program itself carries, so ' +
        'the two say the same thing.');
    AddLimitation(Result, 'The list shows what this build contains. A curve ' +
        'type or module that is not in your build is not listed.');
    AddRelated(Result, ExplainPaneTopic);
    AddRelated(Result, StandingTopic);
end;

function Standing: TExplanation;
begin
    Result := NewExplanation(StandingTopic, 'How much authority an explanation has',
        'Every explanation carries one of four labels saying whether it rests on ' +
        'the field''s sources, on common practice, or on a choice this program ' +
        'made.', esModelChoice);
    AddParagraph(Result, 'A program that teaches has to say not only what it does ' +
        'but on what grounds. The label under each explanation''s title does ' +
        'that:');
    AddParagraph(Result, Bullet + 'Canonical: stated by the field''s ' +
        'authoritative sources. The explanation quotes the source word for word ' +
        'and names where to read it.');
    AddParagraph(Result, Bullet + 'Convention: common practice or a guideline ' +
        'in the field, not a rule its sources require.');
    AddParagraph(Result, Bullet + 'This software''s choice: a decision made by ' +
        'this program rather than by the field''s sources - a default, a bound, ' +
        'a way of showing something. The explanation says why it was made. Most ' +
        'of this guide has this label, because it describes how the program ' +
        'behaves.');
    AddParagraph(Result, Bullet + 'Not settled by the sources: the sources ' +
        'disagree or are silent, so nothing treats it as a rule. An explanation ' +
        'that states no label at all is shown with this one, never as ' +
        'canonical.');
    AddParagraph(Result, 'The difference matters when you report a result: a ' +
        'canonical rule can be cited, while a choice of this program should be ' +
        'named as such.');
    AddLimitation(Result, 'The label is the program''s own claim about its ' +
        'text. For a canonical explanation, check the quoted source yourself ' +
        'when it matters.');
    AddRelated(Result, ExplainPaneTopic);
    AddRelated(Result, ExplainEverythingTopic);
end;

function About: TExplanation;
begin
    Result := NewExplanation(AboutTopic, 'About Fit',
        'Help > About shows which version of Fit you are running.',
        esModelChoice);
    AddParagraph(Result, 'The dialog shows the program''s name and icon, the ' +
        'version number read from the running program itself, its author, and ' +
        'a View at GitHub link that opens the author''s GitHub page in your ' +
        'browser. OK closes it.');
    AddParagraph(Result, 'Quote the version when you report a problem or ' +
        'describe how a result was obtained. A project file records the version ' +
        'that wrote it as well.');
    AddLimitation(Result, 'On macOS the entry is not in the Help menu: it is ' +
        'About Fit in the application menu, where macOS programs keep it.');
    AddRelated(Result, WhatFitIsTopic);
    AddRelated(Result, ProjectsTopic);
end;

function GettingStartedExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, WhatFitIs);
    AppendExplanation(Result, TwoPrograms);
    AppendExplanation(Result, MainWindow);
    AppendExplanation(Result, FirstFit);
    AppendExplanation(Result, FittingByHand);
    AppendExplanation(Result, ExplainPane);
    AppendExplanation(Result, ExplainEverything);
    AppendExplanation(Result, Standing);
    AppendExplanation(Result, About);
end;

end.
