// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The scientific module's chapter of the user guide: the background,
the diffraction angles, the wavelength, the Residual Spectrum and the user's
own formulas.)

WHY A CHAPTER OF ITS OWN. These were the core guide's - the Model chapter's
background, the Data chapter's angles and wavelength, the window's Background
Points table and Residual Spectrum - while the core held the shapes and the
angles. The module holds them now (2026-10-16), and a product without it read
about menus it does not have. So the module explains what it offers, as every
module does (non-negotiable 10), and the core chapters name none of it
(testcase_client_explanations.TheCoreChaptersLeaveTheShapesAndTheAnglesToTheirModule).

ITS TOPICS LINK INTO THE CORE CHAPTERS, NEVER THE OTHER WAY. A core topic that
linked here would link nowhere in a product without this module, which is what
ExplanationFindings reports; a module may rely on the core it is built on.

THE USER'S FORMULAS JOINED THEM (2026-10-17), with the formula type: the
window offers the User group only in a build with a formula type
(build_capabilities.boUserFormula), so a product without this module has no
New User Curve to read about.

THE TOPIC IDS MOVED WITH THE TEXT ('model/background' became
'peaks-and-diffraction/background'): a topic is routed by its namespace to the
one provider that owns it (explanation_registry), so a module cannot answer
inside a core chapter. Nothing outside the guide named them.

WHAT THE MENU DOES NOT SAY IS SAID HERE. The Background Fraction governs the
peak search, not the background points its name suggests. When the code
changes, the sentence here that describes it has to change with it.
}
unit scientific_guide;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation, explanation_registry,
    diffraction_axis_modes;   // DiffractionAnglesTopic, which the modes name

const
    ScientificGuideNamespace = 'peaks-and-diffraction';
    BackgroundTopic = 'peaks-and-diffraction/background';
    BackgroundCurveTopic = 'peaks-and-diffraction/background-curve';
    BackgroundSubtractionTopic = 'peaks-and-diffraction/background-subtraction';
    BackgroundFractionTopic = 'peaks-and-diffraction/background-fraction';
    BackgroundVariationTopic = 'peaks-and-diffraction/background-variation';
    BackgroundPointsTableTopic = 'peaks-and-diffraction/background-points-table';
    WavelengthTopic = 'peaks-and-diffraction/wavelength';
    InstrumentFormatsTopic = 'peaks-and-diffraction/instrument-formats';
    ResidualSpectrumTopic = 'peaks-and-diffraction/residual-spectrum';
    UserCurvesTopic = 'peaks-and-diffraction/user-defined-curves';
    UserCurveFormulaTopic = 'peaks-and-diffraction/user-curve-formula';
    UserCurveRolesTopic = 'peaks-and-diffraction/user-curve-roles';
    UserCurveLibraryTopic = 'peaks-and-diffraction/user-curve-library';
    SampleProfilesTopic = 'peaks-and-diffraction/sample-profiles';
    FirstPeakFitTopic = 'peaks-and-diffraction/first-peak-fit';
    OwnLimitsTopic = 'peaks-and-diffraction/parameter-limits';

{ Every topic of the chapter, in reading order. }
function ScientificGuideExplanations: TExplanations;
{ The chapter as one provider, made once. }
function ScientificGuideProvider: IExplanationProvider;

implementation

uses
    static_explanations;   // AppendExplanation, TStaticExplanationProvider

const
    //  A paragraph that starts with U+2022 and a space is a bullet.
    Bullet = #$E2#$80#$A2 + ' ';

    //  Topics of the core chapters this one leads back to, written out rather
    //  than taken from their units: a chapter does not use another's unit.
    ArgumentAxesTopic = 'data/argument-axes';
    PickingTopic = 'model/picking-on-the-chart';
    CurvePositionsTopic = 'model/curve-positions';
    FitIntervalsTopic = 'model/fit-intervals';
    DeleteCurveTopic = 'model/delete-curve';
    ReloadProfileTopic = 'projects-and-files/reload-profile';
    ResidualRandomnessTopic = 'fitting/is-the-difference-noise';
    TablesTopic = 'the-window/tables';
    ChartTopic = 'the-window/chart';
    ZoomTopic = 'the-window/zoom-and-scroll';
    CurveTypeTopic = 'model/curve-type';
    ParameterKindsTopic = 'model/parameter-kinds';
    SampleDataTopic = 'projects-and-files/sample-data';
    ImportProfileTopic = 'projects-and-files/import-profile';
    FirstFitTopic = 'getting-started/first-fit';
    ParameterLimitsTopic = 'model/parameter-limits';
    AutomaticallyTopic = 'fitting/automatically';

var
    Instance: TStaticExplanationProvider;

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
    AddParagraph(Result, 'The Tools tab carries the same commands as buttons ' +
        'under Background: Pick, Auto and Clear run Model > Background > Points ' +
        '> Start Manual Selection, Compute Automatically and Remove All; ' +
        'Subtract runs Model > Background > Subtract > Automatically. The Pick ' +
        'button stays pressed while its picking mode runs, and the menu entry ' +
        'reads Stop Visual Selection.');
    AddParagraph(Result, 'The Background submenu and the Background buttons are ' +
        'greyed until a profile is loaded, and while a fit runs.');
    AddLimitation(Result, 'The Subtract button always subtracts automatically, ' +
        'ignoring the background points you picked. To subtract through your ' +
        'own points use Model > Background > Subtract > By Selected Points.');
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
    AddParagraph(Result, 'A curve type may say that its curves take no ' +
        'separate background - its curves describing the whole of the data, ' +
        'level included. Model > Background > Curve then offers only None, and ' +
        'says why.');
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

{ The examples this module installs - in the core's Sample data topic until
  the files became the module's (module.json "data", 2026-10-16), which left
  a product without peaks describing files it does not ship. }
function SampleProfiles: TExplanation;
begin
    Result := NewExplanation(SampleProfilesTopic, 'Sample diffraction profiles',
        'Nine diffraction profiles come with the peaks, in the Data folder, to ' +
        'try a fit on.', esModelChoice);
    AddParagraph(Result, Bullet + '1.dat - a whole diffraction pattern, 1692 ' +
        'points with arguments from 3.0 to 172.1 in steps of 0.1.');
    AddParagraph(Result, Bullet + '2.dat to 9.dat - short stretches of 20 to 70 ' +
        'points, each around one or a few peaks. 2.dat, with two overlapping ' +
        'peaks between 116 and 121, is a good first fit.');
    AddParagraph(Result, Bullet + 'test_data.zip - the nine .dat files packed in ' +
        'one archive.');
    AddParagraph(Result, 'A .dat file is read as a diffraction profile - its ' +
        'argument an angle - and File > Import Profile names the format so.');
    AddLimitation(Result, 'They are examples to learn on, not reference ' +
        'measurements: nothing about the sample or the instrument they came from ' +
        'is recorded with them.');
    AddRelated(Result, SampleDataTopic);
    AddRelated(Result, ImportProfileTopic);
end;

{ The core's A first fit, on this module's data - it was the core's own text
  until a product without peaks showed its users a peak tutorial
  (2026-10-18). }
function FirstPeakFit: TExplanation;
begin
    Result := NewExplanation(FirstPeakFitTopic, 'A first fit of peaks',
        'Two overlapping peaks in 2.dat, fitted automatically in a few clicks.',
        esModelChoice);
    AddParagraph(Result, Bullet + 'File > Import Profile, and choose Data/2.dat: ' +
        'two overlapping peaks in about fifty points.');
    AddParagraph(Result, Bullet + 'Choose Pseudo-Voigt under Model > Curve Type, ' +
        'a common choice for diffraction peaks.');
    AddParagraph(Result, Bullet + 'Run Fit > Automatically. Fit puts a ' +
        'background curve under the peaks, places curves, marks a fit interval ' +
        'around each peak and removes curves for as long as the fit stays good ' +
        'enough.');
    AddParagraph(Result, Bullet + 'Read the result: a systematic bump in the ' +
        'Difference series is a peak the model has missed; the Curve Attributes ' +
        'table gives each peak''s position, width and amplitude.');
    AddLimitation(Result, 'Two peaks this close are resolved by the shape the ' +
        'model assumes: another curve type can split the same data differently, ' +
        'and the program cannot say which is physically right.');
    AddRelated(Result, FirstFitTopic);
    AddRelated(Result, SampleProfilesTopic);
    AddRelated(Result, AutomaticallyTopic);
end;

{ The limits this module's own types keep, moved out of the core's
  Limits on parameter values with the types themselves (2026-10-18). }
function OwnLimits: TExplanation;
begin
    Result := NewExplanation(OwnLimitsTopic,
        'Limits on the peaks, the backgrounds and your own formulas',
        'How wide each peak shape may be, how the backgrounds keep above zero, ' +
        'and what the parameters of a formula you define may take.',
        esModelChoice);
    AddParagraph(Result, 'A peak''s width limit is on the peak, not on the number, because ' +
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
    AddLimitation(Result, 'A Width''s limit is measured from the formula as ' +
        'drawn, so a formula whose width does not follow its Width parameter ' +
        'is held where it does not matter.');
    AddRelated(Result, ParameterLimitsTopic);
    AddRelated(Result, BackgroundCurveTopic);
    AddRelated(Result, UserCurveRolesTopic);
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

function ResidualSpectrum: TExplanation;
begin
    Result := NewExplanation(ResidualSpectrumTopic, 'The Residual Spectrum',
        'The Residual Spectrum tab of the chart area maps where, and at which ' +
        'length of rhythm, the difference between the data and the model holds ' +
        'more than noise.', esModelChoice);
    AddParagraph(Result, 'It draws the wavelet transform the program judges the ' +
        'difference with after every fit (see "Is the difference only noise?") ' +
        'as a map: the model''s positions across, the length of a rhythm ' +
        'upwards - 4, 8, 16 points per cycle and so on - and at each place and ' +
        'rhythm what the difference holds there. A cell in the colour of the ' +
        'shaded stretches is a wave the model misses: exactly the cells that ' +
        'judgement named, so the map and the stretches always agree, and a ' +
        'patch says both where the model falls short and what length of wave ' +
        'it left out. A dark grey cell is as strong as noise is only one time ' +
        'in twenty, but among hundreds of cells some are that strong by chance, ' +
        'so it is not named; a light grey one is noise above its average, and ' +
        'the background is noise. The key under the map names every colour, ' +
        'and the line under its title says how to read it.');
    AddParagraph(Result, 'THE MODEL''S CHART AND THE RESIDUAL SPECTRUM SHOW ONE ' +
        'VIEW of the model''s x. Zooming or scrolling either does the same to ' +
        'the other, and making either show everything again shows everything ' +
        'on both, values included. Up and down is each chart''s own: the ' +
        'model''s values on its chart, the wave lengths on the map.');
    AddParagraph(Result, 'Below the map a scroll bar moves the view along x, ' +
        'always agreeing with the one below the model''s chart; the one to its ' +
        'right moves it along the wave lengths.');
    AddLimitation(Result, 'Near the ends of a fit interval the edge shapes ' +
        'what is seen, so those cells, like the gaps between intervals, are ' +
        'greyed as not judged.');
    AddRelated(Result, ResidualRandomnessTopic);
    AddRelated(Result, ChartTopic);
    AddRelated(Result, ZoomTopic);
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
    AddParagraph(Result, 'New User Curve does not name a shape: it opens the ' +
        'dialogs that define one. Cancelling them leaves the previous type in ' +
        'use. The User group comes last in Model > Curve Type and in the Tools ' +
        'list.');
    AddParagraph(Result, 'Cancelling either dialog creates nothing: a curve ' +
        'type saved when its formula was accepted is removed again if you then ' +
        'cancel the second dialog.');
    AddLimitation(Result, 'The curve types you have defined are listed only in ' +
        'the menu, under Model > Curve Type > User. The Tools list offers New ' +
        'User Curve but not the types already made.');
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

function InstrumentFormats: TExplanation;
begin
    Result := NewExplanation(InstrumentFormatsTopic,
        'Diffractometer and refinement files',
        'Besides plain columns, Fit reads a powder pattern from the files ' +
        'diffractometers and refinement programs write: .xrdml, .uxd, GSAS ' +
        'files, a powder CIF, .xye and .chi.', esConvention);
    AddParagraph(Result, 'Each is read as 2 Theta, in degrees, against ' +
        'intensity, and the axes are labelled so:');
    AddParagraph(Result, Bullet + '.xrdml (PANalytical): the first scan. Its ' +
        '2Theta positions - a start and an end, or a list - and its ' +
        'intensities; the other axes a scan lists, Omega among them, are not ' +
        'the argument.');
    AddParagraph(Result, Bullet + '.uxd (Bruker): counts stepped from _START ' +
        '(or _2THETA) by _STEPSIZE, or 2 Theta and count pairs after ' +
        '_2THETACOUNTS. Only the first range is read.');
    AddParagraph(Result, Bullet + '.gsa, .gss, .gsas, .fxye (GSAS): the ' +
        'first bank, binned CONST, in STD, ESD or FXYE layout, its centidegrees ' +
        'turned into degrees.');
    AddParagraph(Result, Bullet + '.cif: a powder pattern written as pdCIF - ' +
        'the first loop of _pd_ intensities, against its 2theta column or ' +
        'the 2theta range the counts were measured over. Measured or processed ' +
        'intensities are preferred to calculated ones.');
    AddParagraph(Result, Bullet + '.xye and .chi: columns of numbers, the ' +
        'angle first, read as a .dat file is; an esd in the third column is ' +
        'left out.');
    AddLimitation(Result, 'Only the pattern is read: the wavelength, the ' +
        'instrument''s settings and the esds are not kept - set the wavelength ' +
        'under Data, as for any profile.');
    AddLimitation(Result, 'A file of several scans, ranges or banks gives ' +
        'its first. A GSAS bank binned in time of flight or logarithmically is ' +
        'refused, as its argument is not an angle, and so is a .cif describing ' +
        'a crystal structure rather than a measured pattern.');
    AddLimitation(Result, 'Bruker''s binary .raw and .brml and Rigaku''s ' +
        '.raw are not read: their layouts are not published, and a reader ' +
        'guessing at one would read the wrong numbers without saying so. ' +
        'Export the scan from the instrument''s software as .xy, .uxd or ' +
        '.xrdml.');
    AddReference(Result, 'Larson A. C., Von Dreele R. B., General Structure ' +
        'Analysis System (GSAS) - the powder data file format',
        'Los Alamos National Laboratory Report LAUR 86-748, 2004',
        'https://11bm.xray.aps.anl.gov/documents/GSASManual.pdf');
    AddReference(Result, 'The powder CIF dictionary (pdCIF)',
        'International Union of Crystallography',
        'https://www.iucr.org/resources/cif/dictionaries/cif_pd');
    AddReference(Result, 'The XRDML schema, as an .xrdml file names it',
        'PANalytical', 'http://www.xrdml.com/XRDMeasurement/1.6');
    AddRelated(Result, DiffractionAnglesTopic);
    AddRelated(Result, WavelengthTopic);
end;

function ScientificGuideExplanations: TExplanations;
begin
    Result := nil;
    AppendExplanation(Result, Background);
    AppendExplanation(Result, BackgroundCurve);
    AppendExplanation(Result, BackgroundSubtraction);
    AppendExplanation(Result, BackgroundFraction);
    AppendExplanation(Result, BackgroundVariation);
    AppendExplanation(Result, BackgroundPointsTable);
    AppendExplanation(Result, DiffractionAngles);
    AppendExplanation(Result, Wavelength);
    AppendExplanation(Result, InstrumentFormats);
    AppendExplanation(Result, SampleProfiles);
    AppendExplanation(Result, FirstPeakFit);
    AppendExplanation(Result, OwnLimits);
    AppendExplanation(Result, ResidualSpectrum);
    AppendExplanation(Result, UserCurves);
    AppendExplanation(Result, UserCurveFormula);
    AppendExplanation(Result, UserCurveRoles);
    AppendExplanation(Result, UserCurveLibrary);
end;

function ScientificGuideProvider: IExplanationProvider;
begin
    if not Assigned(Instance) then
        Instance := TStaticExplanationProvider.Create(ScientificGuideNamespace,
            @ScientificGuideExplanations);
    Result := Instance;
end;

finalization
    Instance.Free;
end.
