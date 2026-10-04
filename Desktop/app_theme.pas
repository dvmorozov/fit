// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which palette the window paints with: light or dark, and the colours of each.)

TWO PALETTES, ONE DECISION. Everything this application paints itself - the
chart, the grid stripes and tints, the HTML panes - reads its colours from the
palette made current here, and nowhere else. Which one is current follows from
the user's View > Theme choice and, under Follow System, from the system's
appearance; TThemeController holds that rule, and the window only asks it.

WHY TWO FIXED PALETTES and not the system colours alone. A system colour such as
clWindow does follow the theme, but a curve colour is not a system colour: the
old palette held clYellow, unreadable on white, and clBlack and clNavy, which
vanish on a dark chart. Each palette here is chosen for its own background and
the test walks both against a contrast floor, so the next colour added that
cannot be read fails by name. A module's colour, which this unit cannot choose,
is passed through ReadableOn instead - a rule, not a table naming modules.

WHY NOT TColor. This unit is reached from the light test suite, which compiles
without the widget set; TThemeColor is the same 24-bit $00BBGGRR encoding the
LCL's TColor uses (as module_view_types.TModuleColor is), so the view passes it
straight through.

WINDOWS. The Win32 widget set of Lazarus 4.8 cannot darken native controls -
menus, scroll bars, a grid's own frame - and this application does not reach for
an undocumented API or a third-party package to make it. So on Windows Dark
changes what this application paints itself, and the chrome around it stays
light. That is a recorded limitation of the theme's explanation, not an
oversight.
}
unit app_theme;

{$mode objfpc}{$H+}

interface

uses
    Classes, series_palette, parameter_kinds;

type
    { A colour as $00BBGGRR - the LCL's TColor encoding, without its unit. }
    TThemeColor = longint;

    { What the user chose under View > Theme. }
    TThemeMode = (tmSystem, tmLight, tmDark);

    { Every colour this application paints with, for one background. }
    TThemePalette = record
        { 'light' or 'dark' - what a failing contrast test names. }
        Name: string;
        Dark: boolean;
        { The chart's and the HTML panes' background, and their text. }
        Background, Text: TThemeColor;
        { Axes, ticks, axis labels, the frame and the crosshair. }
        Axis: TThemeColor;
        { One per series_style.TSeriesColorRole other than a model curve. }
        Experiment, Computed, Residual, BackgroundCurve, IntervalBound,
        Position, PickedPoint: TThemeColor;
        { The live loss chart a running fit draws. }
        Loss: TThemeColor;
        { The model curves, cycled through by series_palette.SeriesColorIndex. }
        Curves: array[1..SeriesColorCount] of TThemeColor;
        { The numeric grids' stripes and states. }
        OddRow, EvenRow, SelectedRegion, DisabledCell: TThemeColor;
        { A parameter cell's tint by kind; ThemeNoColor leaves the stripe. }
        Tints: array[TParameterKind] of TThemeColor;
        { The HTML panes' links, and a report's failed and warned lines. }
        Link, Fail, Warning: TThemeColor;
    end;

    { Asks the system whether its appearance is dark. A method rather than a
      plain function so a test can answer it from a field. }
    TSystemIsDark = function: boolean of object;

    { THE RULE BEHIND THE MENU. Holds the user's choice, decides from it and
      the system which palette is current, makes that palette current, and says
      so - only when the colours actually change, because what listens redraws
      the whole window. }
    TThemeController = class
    private
        FMode: TThemeMode;
        FIsDark: boolean;
        FOnChange: TNotifyEvent;
        FSystemIsDark: TSystemIsDark;
        procedure SetMode(AValue: TThemeMode);
        procedure Decide;
    public
        { Starts on Follow System, deciding from ASystemIsDark at once - and
          makes that palette current without a notification, since there is
          nothing painted yet to repaint. }
        constructor Create(ASystemIsDark: TSystemIsDark);
        { The system's appearance changed. Matters only under Follow System. }
        procedure SystemAppearanceChanged;
        property Mode: TThemeMode read FMode write SetMode;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

const
    { The LCL's clNone, for a tint that leaves the row's own colour. }
    ThemeNoColor = $1FFFFFFF;
    { WCAG 2.x: 3:1 for a graphical object, 4.5:1 for body text. }
    MinGraphicContrast = 3.0;
    MinTextContrast = 4.5;

{ The word settings.json keeps a mode as, and back. Anything not recognised -
  an empty value from a file written before the choice existed, or a newer
  build's mode - reads as Follow System, which is how such a file always opened. }
function ThemeModeSetting(AMode: TThemeMode): string;
function ThemeModeFromSetting(const AValue: string): TThemeMode;

{ Whether the dark palette applies, given the choice and the system. }
function EffectiveThemeIsDark(AMode: TThemeMode; ASystemIsDark: boolean): boolean;

{ The WCAG contrast ratio of two colours, from 1 (identical) to 21. }
function ContrastRatio(A, B: TThemeColor): double;

{ Whether a colour is closer to black than to white in contrast terms - which
  is how a system background is read as a dark appearance. }
function ColorIsDark(AColor: TThemeColor): boolean;

{ AColor, unchanged when it already meets MinGraphicContrast against
  ABackground; otherwise moved towards white on a dark background or black on a
  light one, just far enough to meet it. }
function ReadableOn(AColor, ABackground: TThemeColor): TThemeColor;

{ '#RRGGBB', for markup. }
function HtmlColor(AColor: TThemeColor): string;

{ Windows' own answer: AppsUseLightTheme 0 is dark; no value is an older
  Windows, which has no dark mode and is light. }
function WindowsAppsAreDark(AFound: boolean; AAppsUseLightTheme: longint): boolean;

{ The macOS appearance the application is put in for AMode, by name - '' to
  follow the system's. An explicit Light or Dark sets it so the controls the
  SYSTEM draws (buttons, panels, lists, dialogs, frames) agree with what this
  application paints; Follow System leaves it to the system. }
function MacAppearanceNameFor(AMode: TThemeMode): string;

{ Whether the system's own setting - the AppleInterfaceStyle default - is dark.
  Read instead of the window colour on macOS, because an explicit Light or
  Dark puts the application in that appearance and the window colour with it. }
function MacInterfaceStyleIsDark(const AStyle: string): boolean;

{ How many colours the History tab's lineage graph cycles through. }
const
    HistoryLaneCount = 6;

{ The colour of lineage lane ALane on a list whose background is ABackground.
  The list is the SYSTEM's, not the palette's, so each lane is made readable
  against what is actually behind it (ReadableOn). }
function HistoryLaneColor(ALane: longint; ABackground: TThemeColor): TThemeColor;

function PaletteFor(ADark: boolean): TThemePalette;

{ The palette everything paints with now. Light until something says otherwise. }
function CurrentPalette: TThemePalette;
procedure UseDarkPalette(ADark: boolean);

implementation

uses
    SysUtils, Math;

const
    ThemeModeWords: array[TThemeMode] of string = ('system', 'light', 'dark');

    LightPalette: TThemePalette = (
        Name: 'light';
        Dark: False;
        Background: $00FFFFFF;
        Text: $00000000;
        Axis: $00757575;          //  #757575
        Experiment: $002F2FD3;    //  #D32F2F
        Computed: $00000000;
        Residual: $00327D2E;      //  #2E7D32
        BackgroundCurve: $00757575;
        IntervalBound: $00C06515; //  #1565C0
        Position: $00000000;
        PickedPoint: $00327D2E;
        Loss: $00C06515;          //  #1565C0, the blue it always had
        //  The old order of hues, each darkened until white can carry it.
        Curves: (
            $002828C6,   //  #C62828 red
            $00327D2E,   //  #2E7D32 green
            $000063A6,   //  #A66300 amber, for the yellow white cannot carry
            $00C06515,   //  #1565C0 blue
            $00212121,   //  #212121 near-black
            $00616161,   //  #616161 grey
            $005714AD,   //  #AD1457 magenta
            $006B7900,   //  #00796B teal
            $00933528,   //  #283593 indigo
            $001F1F7B,   //  #7B1F1F maroon
            $002F8B55,   //  #558B2F olive green
            $00006D6D,   //  #6D6D00 olive
            $009A1B6A,   //  #6A1B9A purple
            $00485579,   //  #795548 brown
            $00BD7702,   //  #0277BD sky blue
            $001543D8);  //  #D84315 burnt orange
        //  What form_main.lfm always set, so a light window looks as it did.
        OddRow: $00FFFFFF;
        EvenRow: $00E8E8E8;
        SelectedRegion: $00808080;
        DisabledCell: $00C0C0C0;
        Tints: (
            ThemeNoColor,     //  pkFitted   - the row's own colour
            $00F4D6E2,        //  pkShared   - violet
            $00AADEFA,        //  pkFixed    - amber
            $00F8E8D6);       //  pkComputed - blue
        Link: $00AD4506;          //  #0645AD
        Fail: $002000B0;          //  #B00020, as report_html always had it
        Warning: $00005A8A);      //  #8A5A00

    DarkPalette: TThemePalette = (
        Name: 'dark';
        Dark: True;
        Background: $001E1E1E;    //  #1E1E1E
        Text: $00E0E0E0;
        Axis: $009E9E9E;
        Experiment: $006E6EFF;    //  #FF6E6E
        Computed: $00FFFFFF;
        Residual: $0084C781;      //  #81C784
        BackgroundCurve: $00BDBDBD;
        IntervalBound: $00F6B564; //  #64B5F6
        Position: $00FFFFFF;
        PickedPoint: $0084C781;
        Loss: $00F6B564;          //  #64B5F6
        Curves: (
            $006B6BFF,   //  #FF6B6B red
            $006ABB66,   //  #66BB6A green
            $004FD5FF,   //  #FFD54F yellow
            $00F6B564,   //  #64B5F6 blue
            $00E0E0E0,   //  #E0E0E0 near-white
            $009E9E9E,   //  #9E9E9E grey
            $009262F0,   //  #F06292 pink
            $00ACB64D,   //  #4DB6AC teal
            $00DAA89F,   //  #9FA8DA lavender
            $00658AFF,   //  #FF8A65 coral
            $0033CAC0,   //  #C0CA33 lime
            $00A4AABC,   //  #BCAAA4 taupe
            $00C868BA,   //  #BA68C8 orchid
            $004DB7FF,   //  #FFB74D orange
            $00E1D04D,   //  #4DD0E1 cyan
            $0081D5AE);  //  #AED581 pale green
        OddRow: $00262626;
        EvenRow: $00303030;
        SelectedRegion: $005A5A5A;
        DisabledCell: $003A3A3A;
        //  The same three hues as the light tints, dark enough for light text.
        Tints: (
            ThemeNoColor,
            $0050354A,        //  #4A3550 violet
            $0020455A,        //  #5A4520 amber
            $004F3823);       //  #23384F blue
        Link: $00F8B48A;          //  #8AB4F8
        Fail: $00808AFF;          //  #FF8A80
        Warning: $004DB7FF);      //  #FFB74D

var
    GDark: boolean = False;

function ThemeModeSetting(AMode: TThemeMode): string;
begin
    Result := ThemeModeWords[AMode];
end;

function ThemeModeFromSetting(const AValue: string): TThemeMode;
var
    M: TThemeMode;
begin
    for M := Low(TThemeMode) to High(TThemeMode) do
        if SameText(AValue, ThemeModeWords[M]) then
            Exit(M);
    Result := tmSystem;
end;

function EffectiveThemeIsDark(AMode: TThemeMode; ASystemIsDark: boolean): boolean;
begin
    case AMode of
        tmLight: Result := False;
        tmDark:  Result := True;
    else
        Result := ASystemIsDark;
    end;
end;

function Channel(AColor: TThemeColor; AShift: longint): double;
var
    C: double;
begin
    C := ((AColor shr AShift) and $FF) / 255;
    if C <= 0.03928 then
        Result := C / 12.92
    else
        Result := Power((C + 0.055) / 1.055, 2.4);
end;

function RelativeLuminance(AColor: TThemeColor): double;
begin
    //  $00BBGGRR: red is the low byte.
    Result := 0.2126 * Channel(AColor, 0) + 0.7152 * Channel(AColor, 8) +
        0.0722 * Channel(AColor, 16);
end;

function ContrastRatio(A, B: TThemeColor): double;
var
    LA, LB: double;
begin
    LA := RelativeLuminance(A);
    LB := RelativeLuminance(B);
    Result := (Max(LA, LB) + 0.05) / (Min(LA, LB) + 0.05);
end;

function ColorIsDark(AColor: TThemeColor): boolean;
begin
    //  Darker than the luminance at which white and black contrast equally
    //  (sqrt(1.05 * 0.05) - 0.05, about 0.179): white text reads better on it.
    Result := ContrastRatio(AColor, $FFFFFF) > ContrastRatio(AColor, $000000);
end;

function Mix(AColor, ATarget: TThemeColor; AAmount: double): TThemeColor;
var
    Shift, C, T: longint;
begin
    Result := 0;
    Shift := 0;
    while Shift <= 16 do
    begin
        C := (AColor shr Shift) and $FF;
        T := (ATarget shr Shift) and $FF;
        Result := Result or (Round(C + (T - C) * AAmount) shl Shift);
        Inc(Shift, 8);
    end;
end;

function ReadableOn(AColor, ABackground: TThemeColor): TThemeColor;
var
    Target: TThemeColor;
    Step: longint;
begin
    Result := AColor and $FFFFFF;
    if ContrastRatio(Result, ABackground) >= MinGraphicContrast then
        Exit(AColor);
    //  Towards whichever extreme the background is furthest from, in tenths:
    //  the hue survives as long as it can, and black or white - which always
    //  meets 3:1 against any background - is where it stops.
    if ColorIsDark(ABackground) then
        Target := $FFFFFF
    else
        Target := $000000;
    for Step := 1 to 10 do
    begin
        Result := Mix(AColor and $FFFFFF, Target, Step / 10);
        if ContrastRatio(Result, ABackground) >= MinGraphicContrast then
            Exit;
    end;
end;

function HtmlColor(AColor: TThemeColor): string;
begin
    Result := Format('#%.2X%.2X%.2X',
        [AColor and $FF, (AColor shr 8) and $FF, (AColor shr 16) and $FF]);
end;

function WindowsAppsAreDark(AFound: boolean; AAppsUseLightTheme: longint): boolean;
begin
    Result := AFound and (AAppsUseLightTheme = 0);
end;

const
    { One per lane, so a branch can be followed down the list - moved here
      from the window, where they were fixed and two of them read at 2.8:1 on
      a dark list. }
    HistoryLanes: array[0..HistoryLaneCount - 1] of TThemeColor = (
        $00C06020, $00309040, $002050D0, $00A040A0, $00909020, $004080C0);

function MacAppearanceNameFor(AMode: TThemeMode): string;
begin
    case AMode of
        tmLight: Result := 'NSAppearanceNameAqua';
        tmDark:  Result := 'NSAppearanceNameDarkAqua';
    else
        Result := '';
    end;
end;

function MacInterfaceStyleIsDark(const AStyle: string): boolean;
begin
    Result := SameText(AStyle, 'Dark');
end;

function HistoryLaneColor(ALane: longint; ABackground: TThemeColor): TThemeColor;
begin
    Result := ReadableOn(HistoryLanes[Abs(ALane) mod HistoryLaneCount],
        ABackground);
end;

function PaletteFor(ADark: boolean): TThemePalette;
begin
    if ADark then
        Result := DarkPalette
    else
        Result := LightPalette;
end;

function CurrentPalette: TThemePalette;
begin
    Result := PaletteFor(GDark);
end;

procedure UseDarkPalette(ADark: boolean);
begin
    GDark := ADark;
end;

{ TThemeController }

constructor TThemeController.Create(ASystemIsDark: TSystemIsDark);
begin
    inherited Create;
    FSystemIsDark := ASystemIsDark;
    FMode := tmSystem;
    FIsDark := EffectiveThemeIsDark(FMode, FSystemIsDark());
    UseDarkPalette(FIsDark);
end;

procedure TThemeController.Decide;
var
    Dark: boolean;
begin
    Dark := EffectiveThemeIsDark(FMode, FSystemIsDark());
    if Dark = FIsDark then
        Exit;
    FIsDark := Dark;
    UseDarkPalette(FIsDark);
    if Assigned(FOnChange) then
        FOnChange(Self);
end;

procedure TThemeController.SetMode(AValue: TThemeMode);
begin
    FMode := AValue;
    Decide;
end;

procedure TThemeController.SystemAppearanceChanged;
begin
    if FMode = tmSystem then
        Decide;
end;

end.
