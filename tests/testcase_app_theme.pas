// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which palette the window paints with, and that each one is readable.)

TWO PALETTES, ONE DECISION. The chart, the grids and the HTML panes all read
their colours from the palette app_theme makes current, and which one that is
follows from the user's View > Theme choice and, under Follow System, from the
system's appearance. These tests pin the decision and walk both palettes, so a
colour added later that cannot be read on its own background fails by name.
}
unit testcase_app_theme;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, app_theme, series_palette,
    parameter_kinds;

type
    TAppThemeTest = class(TTestCase)
    private
        FChanges: longint;
        FSystemDark: boolean;
        procedure Changed(Sender: TObject);
        function SystemIsDark: boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure FollowSystemTakesTheSystemsAppearance;
        procedure AnExplicitChoiceIgnoresTheSystem;
        procedure TheSettingRoundTripsEveryMode;
        procedure AnUnknownOrEmptySettingFollowsTheSystem;
        procedure ContrastOfAColourWithItselfIsOne;
        procedure BlackOnWhiteIsTheMaximumContrast;
        procedure DarkAndLightColoursAreToldApart;
        procedure EveryPaletteColourMeetsTheContrastFloor;
        procedure ThePalettesAreTheOnesTheirNamesSay;
        procedure ACurveColourIsNeverRepeatedWithinAPalette;
        procedure ALightPaletteKeepsTheReportColoursItAlwaysHad;
        procedure AnUnreadableColourIsMadeReadable;
        procedure AReadableColourIsLeftAlone;
        procedure HtmlColourIsRedGreenBlue;
        procedure AWindowsSettingOfZeroMeansDark;
        procedure ChoosingDarkMakesTheDarkPaletteCurrent;
        procedure ChoosingTheModeInForceAgainNotifiesNobody;
        procedure ASystemChangeMattersOnlyUnderFollowSystem;
        procedure EveryHistoryLaneIsReadableOnALightAndADarkList;
        procedure HistoryLanesAreTellApartAndRepeatInTurn;
        procedure OnAMacAnExplicitChoiceAlsoSetsTheControlsAppearance;
        procedure OnAMacTheSystemsOwnSettingSaysWhetherItIsDark;
    end;

implementation

const
    White = $FFFFFF;
    Black = $000000;

procedure TAppThemeTest.Changed(Sender: TObject);
begin
    Inc(FChanges);
end;

function TAppThemeTest.SystemIsDark: boolean;
begin
    Result := FSystemDark;
end;

procedure TAppThemeTest.SetUp;
begin
    FChanges := 0;
    FSystemDark := False;
end;

procedure TAppThemeTest.TearDown;
begin
    //  The palette is process-wide; every other test expects the light one.
    UseDarkPalette(False);
end;

procedure TAppThemeTest.FollowSystemTakesTheSystemsAppearance;
begin
    AssertTrue('a dark system', EffectiveThemeIsDark(tmSystem, True));
    AssertFalse('a light system', EffectiveThemeIsDark(tmSystem, False));
end;

procedure TAppThemeTest.AnExplicitChoiceIgnoresTheSystem;
begin
    AssertTrue('dark on a light system', EffectiveThemeIsDark(tmDark, False));
    AssertTrue('dark on a dark system', EffectiveThemeIsDark(tmDark, True));
    AssertFalse('light on a dark system', EffectiveThemeIsDark(tmLight, True));
    AssertFalse('light on a light system', EffectiveThemeIsDark(tmLight, False));
end;

procedure TAppThemeTest.TheSettingRoundTripsEveryMode;
var
    M: TThemeMode;
begin
    for M := Low(TThemeMode) to High(TThemeMode) do
        AssertTrue(ThemeModeSetting(M),
            ThemeModeFromSetting(ThemeModeSetting(M)) = M);
end;

procedure TAppThemeTest.AnUnknownOrEmptySettingFollowsTheSystem;
begin
    //  Empty is what every settings file written before the choice existed
    //  says, and it must open as it always would have on that system.
    AssertTrue('empty', ThemeModeFromSetting('') = tmSystem);
    AssertTrue('a newer build''s value', ThemeModeFromSetting('sepia') = tmSystem);
    AssertTrue('any case', ThemeModeFromSetting('DARK') = tmDark);
end;

procedure TAppThemeTest.ContrastOfAColourWithItselfIsOne;
begin
    AssertEquals(1.0, ContrastRatio($336699, $336699), 1e-9);
end;

procedure TAppThemeTest.BlackOnWhiteIsTheMaximumContrast;
begin
    AssertEquals(21.0, ContrastRatio(Black, White), 1e-6);
    AssertEquals('symmetric', 21.0, ContrastRatio(White, Black), 1e-6);
end;

procedure TAppThemeTest.DarkAndLightColoursAreToldApart;
begin
    AssertTrue('black', ColorIsDark(Black));
    AssertTrue('a dark window background', ColorIsDark($1E1E1E));
    AssertFalse('white', ColorIsDark(White));
    AssertFalse('the light window background of a Mac', ColorIsDark($ECECEC));
end;

procedure TAppThemeTest.EveryPaletteColourMeetsTheContrastFloor;

    procedure Check(const AName: string; const P: TThemePalette;
        AColor: TThemeColor; AFloor: double);
    begin
        AssertTrue(Format('%s: %s has %.2f:1 against its background, below %.1f:1',
            [P.Name, AName, ContrastRatio(AColor, P.Background), AFloor]),
            ContrastRatio(AColor, P.Background) >= AFloor);
    end;

var
    Dark: boolean;
    P: TThemePalette;
    i: longint;
begin
    //  THE SELF-ENFORCING WALK. A colour added to either palette that cannot be
    //  read on that palette's own background - yellow on white was the one that
    //  started this - fails here by name.
    for Dark := False to True do
    begin
        P := PaletteFor(Dark);
        Check('Text', P, P.Text, MinTextContrast);
        Check('Link', P, P.Link, MinTextContrast);
        Check('Fail', P, P.Fail, MinTextContrast);
        Check('Warning', P, P.Warning, MinTextContrast);
        Check('Axis', P, P.Axis, MinGraphicContrast);
        Check('Experiment', P, P.Experiment, MinGraphicContrast);
        Check('Computed', P, P.Computed, MinGraphicContrast);
        Check('Residual', P, P.Residual, MinGraphicContrast);
        Check('BackgroundCurve', P, P.BackgroundCurve, MinGraphicContrast);
        Check('IntervalBound', P, P.IntervalBound, MinGraphicContrast);
        Check('Position', P, P.Position, MinGraphicContrast);
        Check('PickedPoint', P, P.PickedPoint, MinGraphicContrast);
        Check('Loss', P, P.Loss, MinGraphicContrast);
        for i := 1 to SeriesColorCount do
            Check(Format('curve colour %d', [i]), P, P.Curves[i],
                MinGraphicContrast);
    end;
end;

procedure TAppThemeTest.ThePalettesAreTheOnesTheirNamesSay;
begin
    AssertTrue('the dark palette has a dark background',
        ColorIsDark(PaletteFor(True).Background));
    AssertFalse('the light palette has a light background',
        ColorIsDark(PaletteFor(False).Background));
    AssertTrue(PaletteFor(True).Dark);
    AssertFalse(PaletteFor(False).Dark);
end;

procedure TAppThemeTest.ACurveColourIsNeverRepeatedWithinAPalette;
var
    Dark: boolean;
    P: TThemePalette;
    i, j: longint;
begin
    //  The old palette held black twice, so curves five and sixteen could not be
    //  told apart. Sixteen colours that cycle are only worth having distinct.
    for Dark := False to True do
    begin
        P := PaletteFor(Dark);
        for i := 1 to SeriesColorCount do
            for j := i + 1 to SeriesColorCount do
                AssertTrue(Format('%s: curves %d and %d share a colour',
                    [P.Name, i, j]), P.Curves[i] <> P.Curves[j]);
    end;
end;

procedure TAppThemeTest.ALightPaletteKeepsTheReportColoursItAlwaysHad;
begin
    //  Additive: a report read in the light palette looks as it did.
    AssertEquals('#B00020', HtmlColor(PaletteFor(False).Fail));
    AssertEquals('#8A5A00', HtmlColor(PaletteFor(False).Warning));
end;

procedure TAppThemeTest.AnUnreadableColourIsMadeReadable;
var
    Navy, Dark: TThemeColor;
begin
    //  A module's navy marker on the dark chart: 1.04:1 as given.
    Navy := $800000;
    Dark := PaletteFor(True).Background;
    AssertTrue('it starts unreadable',
        ContrastRatio(Navy, Dark) < MinGraphicContrast);
    AssertTrue('it ends readable',
        ContrastRatio(ReadableOn(Navy, Dark), Dark) >= MinGraphicContrast);
    //  And in the other direction: a pale colour on white.
    AssertTrue('yellow on white ends readable',
        ContrastRatio(ReadableOn($00FFFF, White), White) >= MinGraphicContrast);
end;

procedure TAppThemeTest.AReadableColourIsLeftAlone;
begin
    //  A module chose its colour; the framework changes it only when it must.
    AssertEquals($800000, ReadableOn($800000, White));
end;

procedure TAppThemeTest.HtmlColourIsRedGreenBlue;
begin
    //  TColor is $00BBGGRR; HTML is #RRGGBB.
    AssertEquals('#FF0000', HtmlColor($0000FF));
    AssertEquals('#0000FF', HtmlColor($FF0000));
    AssertEquals('#123456', HtmlColor($563412));
end;

procedure TAppThemeTest.AWindowsSettingOfZeroMeansDark;
begin
    //  HKCU\...\Themes\Personalize\AppsUseLightTheme: 0 is dark, 1 is light,
    //  and no value at all is an older Windows, which is light.
    AssertTrue('zero', WindowsAppsAreDark(True, 0));
    AssertFalse('one', WindowsAppsAreDark(True, 1));
    AssertFalse('absent', WindowsAppsAreDark(False, 0));
end;

procedure TAppThemeTest.ChoosingDarkMakesTheDarkPaletteCurrent;
var
    T: TThemeController;
begin
    T := TThemeController.Create(@SystemIsDark);
    try
        T.OnChange := @Changed;
        T.Mode := tmDark;
        AssertTrue('the palette everything reads is the dark one',
            CurrentPalette.Dark);
        AssertEquals('one notification', 1, FChanges);
        T.Mode := tmLight;
        AssertFalse('and back', CurrentPalette.Dark);
        AssertEquals('two notifications', 2, FChanges);
    finally
        T.Free;
    end;
end;

procedure TAppThemeTest.ChoosingTheModeInForceAgainNotifiesNobody;
var
    T: TThemeController;
begin
    //  A redraw of the whole window is not free; a click on the ticked entry
    //  must not cost one.
    T := TThemeController.Create(@SystemIsDark);
    try
        T.OnChange := @Changed;
        T.Mode := tmSystem;
        AssertEquals('Follow System on a light system is what started', 0, FChanges);
        T.Mode := tmLight;
        AssertEquals('Light on a light system changes no colour', 0, FChanges);
    finally
        T.Free;
    end;
end;

procedure TAppThemeTest.ASystemChangeMattersOnlyUnderFollowSystem;
var
    T: TThemeController;
begin
    T := TThemeController.Create(@SystemIsDark);
    try
        T.OnChange := @Changed;
        FSystemDark := True;
        T.SystemAppearanceChanged;
        AssertTrue('Follow System follows', CurrentPalette.Dark);
        AssertEquals(1, FChanges);

        T.Mode := tmLight;
        AssertEquals('Light, chosen', 2, FChanges);
        FSystemDark := False;
        T.SystemAppearanceChanged;
        FSystemDark := True;
        T.SystemAppearanceChanged;
        AssertFalse('an explicit choice stays', CurrentPalette.Dark);
        AssertEquals('and nothing redraws', 2, FChanges);
    finally
        T.Free;
    end;
end;

procedure TAppThemeTest.EveryHistoryLaneIsReadableOnALightAndADarkList;
const
    Backgrounds: array[0..3] of TThemeColor = ($FFFFFF, $1E1E1E, $ECECEC, $2A2A2A);
var
    Lane, k: longint;
    Background: TThemeColor;
begin
    //  The History tab's list is the SYSTEM's, not the palette's, so its lanes
    //  are made readable against whatever the list actually is. Two of the six
    //  fixed lane colours fell to 2.8:1 on a dark list.
    for k := 0 to High(Backgrounds) do
    begin
        Background := Backgrounds[k];
        for Lane := 0 to HistoryLaneCount - 1 do
            AssertTrue(Format('lane %d on %s', [Lane, HtmlColor(Background)]),
                ContrastRatio(HistoryLaneColor(Lane, Background), Background) >=
                MinGraphicContrast);
    end;
end;

procedure TAppThemeTest.HistoryLanesAreTellApartAndRepeatInTurn;
var
    i, j: longint;
begin
    for i := 0 to HistoryLaneCount - 1 do
        for j := i + 1 to HistoryLaneCount - 1 do
            AssertTrue(Format('lanes %d and %d', [i, j]),
                HistoryLaneColor(i, $FFFFFF) <> HistoryLaneColor(j, $FFFFFF));
    AssertEquals('the lane after the last is the first again',
        HistoryLaneColor(0, $FFFFFF), HistoryLaneColor(HistoryLaneCount, $FFFFFF));
end;

procedure TAppThemeTest.OnAMacAnExplicitChoiceAlsoSetsTheControlsAppearance;
begin
    //  Empty: the application follows the system's appearance, which is what
    //  every window did before View > Theme existed.
    AssertEquals('Follow System', '', MacAppearanceNameFor(tmSystem));
    AssertEquals('Light', 'NSAppearanceNameAqua', MacAppearanceNameFor(tmLight));
    AssertEquals('Dark', 'NSAppearanceNameDarkAqua', MacAppearanceNameFor(tmDark));
end;

procedure TAppThemeTest.OnAMacTheSystemsOwnSettingSaysWhetherItIsDark;
begin
    //  AppleInterfaceStyle: 'Dark' when the system is dark, absent when light.
    //  Read rather than the window colour, because an explicit choice now
    //  sets the application's appearance and the window colour with it.
    AssertTrue('dark', MacInterfaceStyleIsDark('Dark'));
    AssertFalse('absent', MacInterfaceStyleIsDark(''));
    AssertFalse('anything else', MacInterfaceStyleIsDark('Light'));
end;

initialization
    RegisterTest('unit', TAppThemeTest);
end.
