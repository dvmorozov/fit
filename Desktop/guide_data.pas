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
form_main, fit_client, argument_axis, custom_axis, mscr_specimen_list,
grid_edit, pick_guidance and the engine's fit_service.

THE AXIS IS A PICTURE, AND THIS CHAPTER SAYS SO. A user who switches to
sin(Theta)/lambda and sees the numbers change will reasonably wonder whether the
fit changed with them; argument_axis states that it never does, and the text
here repeats it wherever an axis is chosen.

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
    DiffractionAnglesTopic = 'data/diffraction-angles';
    WavelengthTopic = 'data/wavelength';
    CustomAxisTopic = 'data/custom-axis';
    DataIntervalTopic = 'data/data-interval';
    SmoothProfileTopic = 'data/smoothing';
    PriceDataTopic = 'data/price-data';

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
        'the curve type suggests, as a plain position, as a diffraction angle ' +
        'or through formulas of your own - without changing the data or the fit.',
        esModelChoice);
    AddParagraph(Result, 'The program stores the argument of every point ' +
        'exactly as it was loaded. An axis rule is a display transform: it ' +
        'decides the name and unit written under the chart, the numbers along ' +
        'the axis, and how positions read in the Curve Attributes table. The ' +
        'stored data and the fit are the same whichever rule is in force.');
    AddParagraph(Result, 'Choose a rule under Data > Argument Transformation > ' +
        'Use Rule, or right-click the chart and choose it from the menu that ' +
        'opens there. Both offer the same six entries, and the one in force is ' +
        'ticked in both:');
    AddParagraph(Result, Bullet + 'Theta, 2 * Theta and Sin Theta / Lambda - ' +
        'the diffraction angles, for data recorded against the scattering angle ' +
        '2 Theta.');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'From Curve Type - the axis the selected curve type defines. The peak ' +
        'shapes the program ships show 2*Theta [deg]; a user-defined curve, and ' +
        'a curve type that defines no axis of its own, show Position.');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'General Position - the argument as loaded, labelled Position, with no ' +
        'unit and no wavelength needed. Use it for any data that is not a ' +
        'diffraction pattern.');
    AddParagraph(Result, Bullet + 'Data > Argument Transformation > Use Rule > ' +
        'Custom Position - an axis defined by a formula of your own and its ' +
        'inverse.');
    AddParagraph(Result, 'Until you pick a rule yourself the program uses From ' +
        'Curve Type, so the axis follows the curve type you select. Once you pick ' +
        'any rule, the choice is yours: it is remembered between sessions and the ' +
        'curve type no longer changes it. Choose From Curve Type to hand the axis ' +
        'back to the model.');
    AddParagraph(Result, 'Use Rule is greyed while nothing is drawn on the ' +
        'chart. The axis label always reads as the name followed by the unit in ' +
        'square brackets, or the name alone when the rule has no unit.');
    AddLimitation(Result, 'A remembered rule that cannot be used when the ' +
        'program starts - Sin Theta / Lambda with no wavelength known, or a ' +
        'custom axis without both of its formulas - is replaced by From Curve ' +
        'Type for that session.');
    AddRelated(Result, DiffractionAnglesTopic);
    AddRelated(Result, WavelengthTopic);
    AddRelated(Result, CustomAxisTopic);
    AddRelated(Result, CurveTypeTopic);
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
        'From Curve Type instead.');
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
        'or From Curve Type while the selected curve type defines one. To give ' +
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
        'definition you last gave, which is remembered between sessions.');
    AddParagraph(Result, 'If either formula is left empty, a message says both ' +
        'are required and the axis stays as it was.');
    AddLimitation(Result, 'The program does not check that g(x) really is the ' +
        'inverse of f(x). If it is not, values read back from the chart land in ' +
        'the wrong place, and nothing warns you.');
    AddLimitation(Result, 'Like every axis rule, a custom axis changes only ' +
        'what is shown; the fit works on the stored argument.');
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

function PriceData: TExplanation;
begin
    Result := NewExplanation(PriceDataTopic, 'Price data',
        'Data > Price Data chooses which column of a price-data (.csv) file ' +
        'becomes the value, and what its argument is.', esModelChoice);
    AddParagraph(Result, 'Under Value:');
    AddParagraph(Result, Bullet + 'Data > Price Data > Value > Close - the ' +
        'closing price, taken from an Adj Close column when the file has one. ' +
        'The default.');
    AddParagraph(Result, Bullet + 'Data > Price Data > Value > Open, Data > ' +
        'Price Data > Value > High and Data > Price Data > Value > Low - the ' +
        'other prices of each row.');
    AddParagraph(Result, 'Under Argument:');
    AddParagraph(Result, Bullet + 'Data > Price Data > Argument > Bar Number - ' +
        'the rows numbered 0, 1, 2 and so on, so that weekends and holidays leave ' +
        'no gaps and a pattern''s length is counted in bars. The default.');
    AddParagraph(Result, Bullet + 'Data > Price Data > Argument > Date - each ' +
        'row at its date, keeping the calendar gaps. Dates written year first ' +
        '(2024-01-31) are read as they are; day-first and month-first dates are ' +
        'told apart from a day above 12 somewhere in the column.');
    AddParagraph(Result, 'The choice applies to the next File > Import Profile ' +
        'or File > Reload Profile of a price-data file - the profile already ' +
        'loaded is not read again by itself - and it is kept between sessions. ' +
        'It has no effect on .dat files.');
    AddLimitation(Result, 'With Date chosen, a file whose dates fit both ' +
        'day-first and month-first order in every row, or fit each in different ' +
        'rows, is refused rather than read in a guessed order; so is a file with ' +
        'no date column, or without the chosen price column. The message says ' +
        'which, and what to choose instead.');
    AddRelated(Result, ProfileTopic);
end;

function DataExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, Profile);
    AppendExplanation(Result, DataTable);
    AppendExplanation(Result, ArgumentAxes);
    AppendExplanation(Result, DiffractionAngles);
    AppendExplanation(Result, Wavelength);
    AppendExplanation(Result, CustomAxis);
    AppendExplanation(Result, DataInterval);
    AppendExplanation(Result, SmoothProfile);
    AppendExplanation(Result, PriceData);
end;

end.
