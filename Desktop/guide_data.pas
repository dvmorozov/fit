// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The user guide's chapter on the data: the profile, the Data tab, the
argument axes, the data interval, smoothing and the characteristic points.)

WHY IT IS ONE CHAPTER. Everything here acts on the measurement itself rather
than on the model fitted to it: which numbers were loaded, how their argument is
shown, which stretch of them the program works on, and what smoothing does to
them. The Data menu is that list, and the code behind it is spread over
form_main, fit_client, coordinate_axis, custom_axis, mscr_specimen_list,
grid_edit, pick_guidance and the engine's fit_service.

THE AXIS IS A PICTURE, AND THIS CHAPTER SAYS SO. A user who switches to
sin(Theta)/lambda and sees the numbers change will reasonably wonder whether the
fit changed with them; coordinate_axis states that it never does, and the text
here repeats it wherever an axis is chosen. The same holds for the value axis,
logarithmic or not.

THE MENU ENTRIES ARE REGISTERED, NOT WRITTEN INTO THE FORM (axis_mode_registry),
so a module may add its own. This chapter names the ones the framework build
ships; a module names its own in its own chapter.

WHAT THE MENU DOES NOT SAY IS SAID HERE; when the code changes, the sentence
that describes it has to change with it.
}
unit guide_data;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

const
    DataNamespace = 'data';
    ProfileTopic = 'data/the-profile';
    DataTableTopic = 'data/data-table';
    ArgumentAxesTopic = 'data/argument-axes';
    ValueAxesTopic = 'data/value-axes';
    LogarithmicAxisTopic = 'data/logarithmic-axis';
    DiffractionAnglesTopic = 'data/diffraction-angles';
    WavelengthTopic = 'data/wavelength';
    CustomAxisTopic = 'data/custom-axis';
    DataIntervalTopic = 'data/data-interval';
    SmoothProfileTopic = 'data/smoothing';
    DataSourcesTopic = 'data/data-sources';
    SamplesSourceTopic = 'data/source-samples';
    UrlSourceTopic = 'data/source-url';
    DoiSourceTopic = 'data/source-doi';

{ Every topic of the chapter, in reading order. }
function DataExplanations: TExplanations;

implementation

uses
    static_explanations;   // AppendExplanation

const
    //  A paragraph that starts with U+2022 and a space is a bullet.
    Bullet = #$E2#$80#$A2 + ' ';

    //  Topics of other chapters, written out rather than taken from their
    //  units: a chapter does not use another chapter's unit.
    ModelTopic = 'model/the-model';
    PickingTopic = 'model/picking-on-the-chart';
    CurvePositionsTopic = 'model/curve-positions';
    BackgroundTopic = 'model/background';
    CurveTypeTopic = 'model/curve-type';
    UserCurvesTopic = 'model/user-defined-curves';
    ImportProfileTopic = 'projects-and-files/import-profile';
    ReloadProfileTopic = 'projects-and-files/reload-profile';

function Profile: TExplanation;
begin
    Result := NewExplanation(ProfileTopic, 'The profile',
        'The profile is the measured data a model is fitted to: a list of ' +
        'points, each an argument and the amplitude measured at it.',
        esModelChoice);
    AddParagraph(Result, 'When you import a data file (File > Import Profile) ' +
        'or open a project, its points become the profile. Each point has two ' +
        'numbers: the argument, which runs along the horizontal axis - an angle, ' +
        'an energy, a time or any other position - and the amplitude, the value ' +
        'measured there.');
    AddParagraph(Result, 'The profile is drawn on the chart and listed in the ' +
        'Data tab on the left, beside the Tools tab. Everything the program ' +
        'proposes or fits is computed from it: the curve positions it finds, the ' +
        'fit intervals, the background, and the difference between the model and ' +
        'the data.');
    AddParagraph(Result, 'The argument is stored exactly as it was read. What ' +
        'the horizontal axis shows - its name, its unit and its numbers - is ' +
        'chosen separately under Data > Argument Transformation, and changing it ' +
        'never changes the stored values or the fit.');
    AddParagraph(Result, 'The Data menu works on the profile itself: it chooses ' +
        'how the argument is shown, narrows the work to a stretch of the profile ' +
        '(Data > Range) and smooths the amplitudes (Data > Smooth Profile). The ' +
        'tables at the bottom of the window - Background Points, Fit Intervals, ' +
        'Curve Positions and Curve Attributes - describe the model rather than ' +
        'the data.');
    AddParagraph(Result, 'Almost every command of the Data and Model menus is ' +
        'greyed until a profile is loaded, and while a fit or another ' +
        'calculation is running.');
    AddLimitation(Result, 'Smoothing, subtracting a background and editing a ' +
        'point all change the profile the program holds, not the data file. ' +
        'File > Reload Profile reads the file again, which is the way back to ' +
        'the data as measured.');
    AddRelated(Result, DataTableTopic);
    AddRelated(Result, ArgumentAxesTopic);
    AddRelated(Result, DataIntervalTopic);
    AddRelated(Result, ModelTopic);
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, ReloadProfileTopic);
end;

function DataTable: TExplanation;
begin
    Result := NewExplanation(DataTableTopic, 'The Data table',
        'The Data tab on the left lists every point of the profile, or of the ' +
        'data interval in force, and lets you correct a value by typing over it.',
        esModelChoice);
    AddParagraph(Result, 'The table has two columns: Position, the argument, ' +
        'and Amplitude, the value measured there. While a data interval is ' +
        'selected (Data > Range) it lists only the points of that interval.');
    AddParagraph(Result, 'To correct a point, click its cell, type the new ' +
        'number and leave the cell. The point is replaced in the profile the ' +
        'engine fits, and the chart is redrawn from it.');
    AddParagraph(Result, 'The tab shares the left panel with the Tools tab; ' +
        'click its tab to bring it forward.');
    AddParagraph(Result, 'A cell holding anything that is not a number - a ' +
        'typo, a stray letter, a unit typed after the value - is refused: a ' +
        'message beside the table names what was typed, and the point stays ' +
        'where it was.');
    AddLimitation(Result, 'An edit changes the profile the program holds, not ' +
        'the data file. Save the project to keep it; File > Reload Profile ' +
        'discards it.');
    AddRelated(Result, ProfileTopic);
    AddRelated(Result, DataIntervalTopic);
end;

function ArgumentAxes: TExplanation;
begin
    Result := NewExplanation(ArgumentAxesTopic, 'Argument axes and units',
        'The horizontal axis can show the stored argument in several ways - as ' +
        'the model and the data say it should, as a plain position, as a ' +
        'diffraction angle, on a logarithmic scale or through formulas of your ' +
        'own - without changing the data or the fit.',
        esModelChoice);
    AddParagraph(Result, 'The program stores the argument of every point ' +
        'exactly as it was loaded. An axis rule is a display transform: it ' +
        'decides the name and unit written under the chart, the numbers along ' +
        'the axis, the caption and reading of the pointer''s position in the ' +
        'panel on the right, and how positions read in the Curve Attributes ' +
        'table. The stored data and the fit are the same whichever rule is in ' +
        'force.');
    AddParagraph(Result, 'Choose a rule under Data > Argument Transformation > ' +
        'Use Rule, or right-click the chart and choose it under Argument in the ' +
        'menu that opens there. Both offer the same entries, and the one in ' +
        'force is ticked in both:');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'Automatic - what the model and the data say the argument is. The ' +
        'program asks, in order: the curves the model holds, when they agree; ' +
        'then the data, whose reader says what its argument is; then what the ' +
        'model''s curves assume when the data says nothing; then, only ' +
        'while the model is empty, the curve type selected on the Tools tab. ' +
        'When none of them says, the argument is shown as Position.');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'General Position - the argument as loaded, labelled Position, with no ' +
        'unit and no wavelength needed.');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'Logarithmic - the argument on a logarithmic scale, marked and read in ' +
        'the argument itself. See the logarithmic axis.');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'Custom Position - an axis defined by a formula of your own and its ' +
        'inverse.');
    AddParagraph(Result, Bullet + 'Theta, 2 * Theta and Sin Theta / Lambda - ' +
        'the diffraction angles, for data recorded against the scattering angle ' +
        '2 Theta.');
    AddParagraph(Result, 'A module can add entries of its own to this menu; ' +
        'its own chapter of the guide says what they are.');
    AddParagraph(Result, 'Until you pick a rule yourself the program uses ' +
        'Automatic, so the axis follows the model and the data. Once you pick ' +
        'any rule, the choice is yours, and neither the model nor the data ' +
        'changes it. Choose Automatic to hand the axis back.');
    AddParagraph(Result, 'The choice belongs to the project, not to the ' +
        'program: it is saved with the project and comes back when the project ' +
        'is reopened, and File > Save Project counts it as a change to save. A ' +
        'new project, and a different data file imported into the one open, ' +
        'start on Automatic again, so one project''s axis never follows you ' +
        'into the next.');
    AddParagraph(Result, 'Use Rule is greyed while nothing is drawn on the ' +
        'chart. The axis label always reads as the name followed by the unit in ' +
        'square brackets, or the name alone when the rule has no unit; the ' +
        'caption beside the pointer''s position on the right is the same name.');
    AddLimitation(Result, 'A rule saved in a project that cannot be used - Sin ' +
        'Theta / Lambda with no wavelength known, a custom axis without both of ' +
        'its formulas, or an entry a module added to a build that no longer ' +
        'contains that module - is replaced by Automatic while the project is ' +
        'open.');
    AddLimitation(Result, 'Every entry is offered whatever the data is. The ' +
        'program cannot tell whether your argument really is an angle or a ' +
        'date, and on data that is not, those entries show numbers that mean ' +
        'nothing.');
    AddRelated(Result, ValueAxesTopic);
    AddRelated(Result, LogarithmicAxisTopic);
    AddRelated(Result, DiffractionAnglesTopic);
    AddRelated(Result, WavelengthTopic);
    AddRelated(Result, CustomAxisTopic);
    AddRelated(Result, CurveTypeTopic);
end;

function ValueAxes: TExplanation;
begin
    Result := NewExplanation(ValueAxesTopic, 'Value axes',
        'The vertical axis is chosen the same way as the horizontal one: what ' +
        'the model and the data say the value is, a plain value, a logarithmic ' +
        'scale or formulas of your own - without changing the data or the fit.',
        esModelChoice);
    AddParagraph(Result, 'The value of every point is stored exactly as it was ' +
        'loaded. A value rule decides the name written beside the vertical axis, ' +
        'the numbers along it, where each point is drawn, and the caption and ' +
        'reading of the pointer''s value in the panel on the right.');
    AddParagraph(Result, 'Choose a rule under Data > Value Transformation > Use ' +
        'Rule, or right-click the chart and choose it under Value:');
    AddParagraph(Result, Bullet + 'Data > Value Transformation > Use Rule > ' +
        'Automatic - what the model and the data say the value is, asked in the ' +
        'same order as for the argument: a model of diffraction peaks is drawn ' +
        'against Intensity. When ' +
        'nothing says, the value is shown as Value.');
    AddParagraph(Result, Bullet + 'Data > Value Transformation > Use Rule > ' +
        'General Value - the value as loaded, labelled Value.');
    AddParagraph(Result, Bullet + 'Data > Value Transformation > Use Rule > ' +
        'Logarithmic - a logarithmic chart. See the logarithmic axis.');
    AddParagraph(Result, Bullet + 'Data > Value Transformation > Use Rule > ' +
        'Custom Value - an axis defined by a formula of your own and its ' +
        'inverse, in the same dialog as a custom argument axis.');
    AddParagraph(Result, Bullet + 'Data > Value Transformation > Use Rule > ' +
        'Intensity - the value as loaded, under the name a diffraction pattern ' +
        'gives it.');
    AddParagraph(Result, 'A module can add entries of its own to this menu ' +
        'too; its own chapter of the guide says what they are.');
    AddParagraph(Result, 'As with the argument, a rule you pick is saved with ' +
        'the project, and Automatic hands the choice back.');
    AddLimitation(Result, 'The rule moves the drawn points and the readout, not ' +
        'the tables: the Data, Background Points and Curve Positions tables, ' +
        'and a curve''s Amplitude in Curve Attributes, show the stored values. ' +
        'An amplitude is a height above whatever lies beneath it, not a point on ' +
        'the vertical axis, so a logarithm of it would not be where the curve is ' +
        'drawn.');
    AddLimitation(Result, 'The fit is always made against the stored values. A ' +
        'logarithmic chart shows the data differently; it does not fit its ' +
        'logarithm.');
    AddRelated(Result, ArgumentAxesTopic);
    AddRelated(Result, LogarithmicAxisTopic);
    AddRelated(Result, CustomAxisTopic);
end;

function LogarithmicAxis: TExplanation;
begin
    Result := NewExplanation(LogarithmicAxisTopic, 'The logarithmic axis',
        'Logarithmic draws a coordinate at the decimal logarithm of its value, ' +
        'so equal ratios are equal distances, and marks and reads it in the ' +
        'value itself.', esConvention);
    AddParagraph(Result, 'It is offered for both axes: Data > Argument ' +
        'Transformation > Use Rule > Logarithmic and Data > Value ' +
        'Transformation > Use Rule > Logarithmic. On the value axis it is the ' +
        'ordinary logarithmic chart of prices, counts or concentrations, on which ' +
        'a steady percentage growth is a straight line; on the argument axis it ' +
        'is how a dose or a concentration series is usually plotted.');
    AddParagraph(Result, 'The axis keeps the quantity''s own name and adds ", ' +
        'log scale" - Intensity, log scale. Its marks are the values themselves ' +
        '(100, 316.2, 1000), not their logarithms, and so is the reading of the ' +
        'pointer''s position on the right.');
    AddLimitation(Result, 'Zero and negative numbers have no logarithm. A point ' +
        'whose value is zero or below is left out of the chart - its line stops ' +
        'on either side of it and the pointer does not snap to it - while it ' +
        'stays in the data and in the fit. On the value axis the Difference ' +
        'curve, which is negative wherever the model lies above the data, is ' +
        'therefore drawn only where it is positive.');
    AddLimitation(Result, 'The spacing between marks is chosen in the ' +
        'logarithm, so over less than one decade the marks fall at values such ' +
        'as 112.2 and 125.9 rather than at round numbers.');
    AddRelated(Result, ValueAxesTopic);
    AddRelated(Result, ArgumentAxesTopic);
end;

function DiffractionAngles: TExplanation;
begin
    Result := NewExplanation(DiffractionAnglesTopic,
        'Diffraction angles: Theta, 2 Theta and sin(Theta)/lambda',
        'For a diffraction pattern the stored argument is read as the ' +
        'scattering angle 2 Theta in degrees, and the axis can show it as ' +
        '2 Theta, as Theta, or as sin(Theta)/lambda.', esConvention);
    AddParagraph(Result, 'Diffraction patterns are usually recorded against the ' +
        'scattering angle 2 Theta, and the program takes the stored argument to ' +
        'be 2 Theta in degrees whenever one of these three rules is in force:');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        '2 * Theta - the stored value itself, labelled 2*Theta [deg].');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'Theta - half of it, labelled Theta [deg].');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'Sin Theta / Lambda - sin(Theta) divided by the wavelength lambda in ' +
        'angstrom, labelled Sin(Theta)/Lambda [1/A].');
    AddParagraph(Result, 'The value of a diffraction profile is the measured ' +
        'Intensity, and Data > Value Transformation > Use Rule > Intensity names ' +
        'it so; a model of the peak shapes the program ships is shown against ' +
        'it by Automatic.');
    AddParagraph(Result, 'By Bragg''s law, lambda = 2 d sin(Theta), the value ' +
        'sin(Theta)/lambda equals 1/(2d), where d is the spacing of the ' +
        'diffracting planes. Peaks recorded at different wavelengths therefore ' +
        'line up on this axis, which is why it is common in crystallography.');
    AddParagraph(Result, 'Only Sin Theta / Lambda needs a wavelength. The first ' +
        'time you choose it with no wavelength known, the program asks for one ' +
        'before switching; Theta and 2 * Theta need none.');
    AddLimitation(Result, 'The program cannot tell whether your argument really ' +
        'is 2 Theta in degrees. On data recorded against anything else these ' +
        'three rules show numbers that mean nothing; use General Position or ' +
        'Automatic instead.');
    AddRelated(Result, ArgumentAxesTopic);
    AddRelated(Result, WavelengthTopic);
end;

function Wavelength: TExplanation;
begin
    Result := NewExplanation(WavelengthTopic, 'Wavelength',
        'The wavelength of the radiation, in angstrom, is the number the ' +
        'Sin Theta / Lambda axis divides by.', esModelChoice);
    AddParagraph(Result, 'Set it with Data > Argument Transformation > Set Rule ' +
        'Parameters > Wavelength. A dialog titled Wavelength asks for Wavelength ' +
        '(angstrom). It accepts only a positive number - 1.5406, for example - ' +
        'and keeps asking until it gets one or you cancel. On OK the wavelength ' +
        'is stored and the axis switches to Sin Theta / Lambda.');
    AddParagraph(Result, 'Set Rule Parameters is greyed unless the axis in ' +
        'force is a diffraction angle - Theta, 2 * Theta or Sin Theta / Lambda, ' +
        'or Automatic while it resolves to one. To give ' +
        'a wavelength for the first time you can also choose Data > Argument ' +
        'Transformation > Use Rule > Sin Theta / Lambda: with none known, it ' +
        'asks for one first.');
    AddParagraph(Result, 'The wavelength is saved with the project.');
    AddLimitation(Result, 'Only what is shown depends on the wavelength. The ' +
        'fit works on the stored argument, so a wrong wavelength mislabels the ' +
        'axis without changing any fitted value.');
    AddRelated(Result, DiffractionAnglesTopic);
    AddRelated(Result, ArgumentAxesTopic);
end;

function CustomAxis: TExplanation;
begin
    Result := NewExplanation(CustomAxisTopic, 'A custom argument axis',
        'Data > Argument Transformation > Use Rule > Custom Position shows the ' +
        'argument through a formula of your own, given together with its ' +
        'inverse.', esModelChoice);
    AddParagraph(Result, 'The entry opens the Custom Argument Axis dialog, which ' +
        'has four fields:');
    AddParagraph(Result, Bullet + 'Display name and Unit - the label under the ' +
        'axis, written as the name followed by the unit in square brackets. The ' +
        'unit may be left empty.');
    AddParagraph(Result, Bullet + 'Displayed value f(x) - the value shown, as a ' +
        'formula of the stored value x.');
    AddParagraph(Result, Bullet + 'Inverse g(x) - the way back, from a shown ' +
        'value to the stored one. It is needed because the chart converts in ' +
        'both directions: reading a position off the axis is the inverse of ' +
        'drawing it.');
    AddParagraph(Result, 'For a natural-logarithm axis, write ln(x) as f(x) and ' +
        'exp(x) as g(x). The formulas are written as for a user-defined curve: ' +
        'ln is the natural logarithm, log is base 10, and exp, sqrt, abs, sin, ' +
        'cos and the operators + - * / ^ can be used with brackets.');
    AddParagraph(Result, 'The first time, the dialog is filled in with the name ' +
        'Custom, no unit, and x for both formulas - the axis you already have - ' +
        'so pressing OK unchanged changes nothing. After that it opens on the ' +
        'definition you last gave, which is saved with the project.');
    AddParagraph(Result, 'If either formula is left empty, a message says both ' +
        'are required and the axis stays as it was.');
    AddParagraph(Result, 'The value axis has one too, at Data > Value ' +
        'Transformation > Use Rule > Custom Value, which opens the same dialog ' +
        'for the vertical axis. The two definitions are kept apart.');
    AddLimitation(Result, 'The program does not check that g(x) really is the ' +
        'inverse of f(x). If it is not, values read back from the chart land in ' +
        'the wrong place, and nothing warns you.');
    AddLimitation(Result, 'Like every axis rule, a custom axis changes only ' +
        'what is shown; the fit works on the stored values.');
    AddRelated(Result, ArgumentAxesTopic);
    AddRelated(Result, UserCurvesTopic);
end;

function DataInterval: TExplanation;
begin
    Result := NewExplanation(DataIntervalTopic, 'Working on part of the profile',
        'Data > Range narrows the program''s work to the stretch of the profile ' +
        'between two points you pick, and brings the whole profile back.',
        esModelChoice);
    AddParagraph(Result, 'Choose Data > Range > Start Manual Selection and click ' +
        'two points of the profile on the chart, one at each end of the stretch ' +
        'you want. The entry is ticked while this picking mode runs, the status ' +
        'line says which click comes next, and the picks are drawn as the Area ' +
        'Limits series. Until the second pick, clicking the first again takes ' +
        'it back; once both are picked, further clicks are ignored.');
    AddParagraph(Result, 'Then choose Data > Range > Select Data Interval. It is ' +
        'greyed until exactly two points are picked. The profile is cut to the ' +
        'points between them, both ends included; the chart and the Data table ' +
        'show only that stretch, and the picking mode ends.');
    AddParagraph(Result, 'From then on the automatic commands - proposing curve ' +
        'positions, fit intervals and background points, and subtracting the ' +
        'background - work on that interval only, and so does the fit.');
    AddParagraph(Result, 'Data > Range > Select Entire Profile goes back to the ' +
        'whole profile. It is offered only while an interval is in force and no ' +
        'picking mode is running.');
    AddParagraph(Result, 'A selected interval is saved with the project, as the ' +
        'numbers of its first and last points.');
    AddParagraph(Result, 'Selecting another interval while one is in force ' +
        'replaces it: the fit runs over the new one.');
    AddRelated(Result, PickingTopic);
    AddRelated(Result, SmoothProfileTopic);
    AddRelated(Result, ProfileTopic);
end;

function SmoothProfile: TExplanation;
begin
    Result := NewExplanation(SmoothProfileTopic, 'Smoothing the profile',
        'Data > Smooth Profile replaces each amplitude by the mean of itself and ' +
        'its two neighbours, keeping the total of all amplitudes the same.',
        esModelChoice);
    AddParagraph(Result, 'Each point''s amplitude becomes the mean of three ' +
        'values: the point before it, the point itself and the point after it. ' +
        'At either end the missing neighbour is replaced by the end point itself. ' +
        'The smoothed amplitudes are then scaled so that their sum equals the sum ' +
        'before smoothing: the area under the profile is kept, and the highest ' +
        'peaks come down a little.');
    AddParagraph(Result, 'Use it on noisy data before the automatic commands, ' +
        'which look for local maxima and are easily misled by noise. It can be ' +
        'applied more than once; each application smooths further.');
    AddParagraph(Result, 'The entry is offered whenever a profile is loaded and ' +
        'no fit is running. The chart and the Data table show the smoothed ' +
        'profile at once.');
    AddLimitation(Result, 'Smoothing changes the data you fit, and a fit to a ' +
        'smoothed profile describes the smoothed data rather than the ' +
        'measurement. It cannot be undone except by File > Reload Profile.');
    AddLimitation(Result, 'It smooths the whole profile. A data interval ' +
        'selected before smoothing keeps its values as they were, so smooth first ' +
        'and then select the interval.');
    AddRelated(Result, ProfileTopic);
    AddRelated(Result, DataIntervalTopic);
    AddRelated(Result, CurvePositionsTopic);
end;

function DataSources: TExplanation;
begin
    Result := NewExplanation(DataSourcesTopic, 'Data sources',
        'File > New Project from Data Source finds data in a published ' +
        'collection or at an address, downloads it and starts a project with ' +
        'it.', esModelChoice);
    AddParagraph(Result, 'The window has the sources down its left-hand ' +
        'side, grouped by what they hold; a module adds its own, so the list ' +
        'depends on which modules were built in. They stay there the whole ' +
        'time, so another source is one click away at any point. Choosing one ' +
        'fills the right-hand side with what that source needs: what to look ' +
        'for, which file of a record to take, and a preview of the data ' +
        'before anything is imported.');
    AddParagraph(Result, 'The line of steps above that panel says which of ' +
        'them you are on and what is still to come. A step already answered ' +
        'shows the answer beside its name and can be clicked to go back to ' +
        'it, as can the Back button; a step not yet reached is greyed, and ' +
        'clicking it says what has to be finished first.');
    AddParagraph(Result, 'Nothing has to be looked up first. Where a source ' +
        'knows what it holds, it offers it by name and the identifier it uses ' +
        'behind the scenes is never shown; where a service knows too much to ' +
        'list - every substance, every published dataset - the common answers ' +
        'are offered and anything else can be typed. Dates are chosen from a ' +
        'calendar rather than typed in a format to get wrong.');
    AddParagraph(Result, 'A question that matters only for one answer to ' +
        'another is greyed until that answer is chosen, and the line under it ' +
        'says which - dates, say, are asked only when a range is chosen as ' +
        'custom dates rather than as the whole series or the last few years. ' +
        'What is left in a greyed box is not sent.');
    AddParagraph(Result, 'While a file is being fetched the window keeps ' +
        'working: the line at the bottom shows what has arrived and how long ' +
        'it has taken, and Cancel becomes Stop, which gives up on the ' +
        'download and leaves the wizard as it was.');
    AddParagraph(Result, 'The preview step says where the file was saved and ' +
        'lets you change it: Save in shows the folder, and Change moves what ' +
        'was just fetched into another one. The choice is remembered, so it ' +
        'is a row to read rather than a question to answer every time. By ' +
        'default it is a folder of downloaded data under your user account.');
    AddParagraph(Result, 'The project is then started from that file exactly ' +
        'as File > Import Profile starts one from a file you already have. It ' +
        'records where the data came from - the source, what was asked for, ' +
        'the address and when it was fetched - and stores the data itself, so ' +
        'reopening it never needs the network. The project itself is not ' +
        'saved until you save it, with File > Save Project.');
    AddParagraph(Result, 'The preview is read by the same reader that will ' +
        'import the file, so what the preview shows is what the project gets. A ' +
        'file whose kind this build has no reader for is shown greyed, with the ' +
        'reason.');
    AddParagraph(Result, 'Looking is free of consequences. Every file the ' +
        'wizard fetches is written to disk so that it can be previewed, and ' +
        'the ones that did not become a project are thrown away when Fit ' +
        'closes - so trying six series leaves one file, not six. A file kept ' +
        'in a folder you chose yourself is never deleted, and neither is one ' +
        'a project was made from: that project reads it again when you ask ' +
        'for File > Reload Profile.');
    AddLimitation(Result, 'Fetching data needs a network connection and the ' +
        'curl program: every macOS and Windows 10 (version 1803) or later ' +
        'includes it, and the Linux packages install it. Downloads use the ' +
        'system''s own secure connections and certificates, and a proxy set ' +
        'in the http_proxy or https_proxy environment variable. Fitting itself ' +
        'never needs the network: a project, once saved, holds its data.');
    AddLimitation(Result, 'Fit fetches data and nothing else. It does not ' +
        'search for a matching phase, identify a substance or interpret what it ' +
        'downloads.');
    AddLimitation(Result, 'Each service decides what it allows and how often ' +
        'it may be asked. The terms shown beside a source are what its ' +
        'publisher states; a download is the user''s own use of that service.');
    AddRelated(Result, SamplesSourceTopic);
    AddRelated(Result, UrlSourceTopic);
    AddRelated(Result, DoiSourceTopic);
    AddRelated(Result, ImportProfileTopic);
    AddRelated(Result, ProfileTopic);
end;

function SamplesSource: TExplanation;
begin
    Result := NewExplanation(SamplesSourceTopic, 'Sample data',
        'The Sample data source offers the example files installed with Fit, ' +
        'so there is something to fit before any data has been found.',
        esModelChoice);
    AddParagraph(Result, 'It lists the files in Fit''s own data directory that ' +
        'this build has a reader for - the diffraction profiles 1.dat to 9.dat ' +
        'and the example price series. Nothing is downloaded and no connection ' +
        'is used.');
    AddParagraph(Result, 'They are ordinary files: the same profiles can be ' +
        'opened with File > Import Profile.');
    AddLimitation(Result, 'An installation that did not install the sample ' +
        'directory has nothing to offer here, and the source says so rather ' +
        'than showing an empty list.');
    AddRelated(Result, DataSourcesTopic);
end;

function UrlSource: TExplanation;
begin
    Result := NewExplanation(UrlSourceTopic, 'Web address',
        'The Web address source downloads a data file from an address you ' +
        'paste in.', esModelChoice);
    AddParagraph(Result, 'Use it for a file whose address you already have: a ' +
        'link from a colleague, a supplementary file of a paper, or a service ' +
        'Fit has no source of its own for. The address must begin with http:// ' +
        'or https://.');
    AddParagraph(Result, 'Which reader opens the file is decided by the name ' +
        'at the end of the address, and by what the server says the file is ' +
        'called when the address names nothing.');
    AddLimitation(Result, 'An address that answers with a web page rather than ' +
        'a data file downloads that page, which then reads as no data points. ' +
        'The preview shows this before anything becomes a project.');
    AddLimitation(Result, 'A file on this computer needs no download: open it ' +
        'with File > Import Profile.');
    AddRelated(Result, DataSourcesTopic);
end;

function DoiSource: TExplanation;
begin
    Result := NewExplanation(DoiSourceTopic, 'Published data (DOI)',
        'The DOI source opens a Zenodo or figshare record by its DOI and lists ' +
        'the files it publishes.', esModelChoice);
    AddParagraph(Result, 'Paste a DOI - 10.5281/zenodo.1234567, or the same ' +
        'thing as a doi.org link - and Fit asks the DOI system where it ' +
        'resolves to, then asks that repository which files the record holds. ' +
        'Choose one and it is previewed and imported like any other file.');
    AddParagraph(Result, 'Words rather than a DOI search Zenodo for records ' +
        'whose description matches them.');
    AddLimitation(Result, 'Only Zenodo and figshare publish a file list Fit ' +
        'can read. A DOI that resolves anywhere else is refused, naming where ' +
        'it went, so that the file can be fetched with the Web address source ' +
        'instead.');
    AddLimitation(Result, 'A record may publish its data in a form no reader ' +
        'here understands - an archive, a spreadsheet, an instrument''s own ' +
        'format. Those files are listed and greyed rather than hidden, so that ' +
        'what the record contains is still visible.');
    AddRelated(Result, DataSourcesTopic);
end;

function DataExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, Profile);
    AppendExplanation(Result, DataTable);
    AppendExplanation(Result, ArgumentAxes);
    AppendExplanation(Result, ValueAxes);
    AppendExplanation(Result, LogarithmicAxis);
    AppendExplanation(Result, DiffractionAngles);
    AppendExplanation(Result, Wavelength);
    AppendExplanation(Result, CustomAxis);
    AppendExplanation(Result, DataInterval);
    AppendExplanation(Result, SmoothProfile);
    AppendExplanation(Result, DataSources);
    AppendExplanation(Result, SamplesSource);
    AppendExplanation(Result, UrlSource);
    AppendExplanation(Result, DoiSource);
end;

end.
