// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(How a pixel density is decided, without asking a display.)

WHY IT IS ITS OWN UNIT. ui_dpi answers one question - what ppi should this
interface be laid out for - and the answer is arithmetic and string parsing all
the way down: a scale factor written as text, an X resource database, a range a
real display falls in, an order of precedence between sources. None of it needs a
screen. All of it used to sit inside a unit that opens with `uses Forms` and
reaches into gdk, so none of it could be tested, and the parts most likely to be
wrong are exactly the parts that only appear on somebody else's desktop.

What could not be tested is what could not be checked either. The Xft.dpi reader
exists because a 200 % Linux desktop reports itself as 100 %; getting its
line-matching wrong produces a plausible number from the wrong resource, and the
symptom is a window that is the wrong size on a machine the author does not have.

WHAT STAYS BEHIND. Reading the environment, reading the root window's property,
asking the widget set, and writing the answer into ScreenInfo. Those are the
syscalls; the decisions are here.
}
unit ui_scaling;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, StrUtils;

const
    { The ppi every pixel literal in this application is quoted at. }
    DesignPPI = 96;
    { A ppi outside this range is a misconfiguration, not a display. Refusing it
      is the difference between a badly scaled window and one whose controls are
      larger than the screen. }
    MinPPI = 48;
    MaxPPI = 960;

type
    { Where a ppi came from. A CLOSED SET rather than free text, because the log
      line naming it and the precedence between sources are two views of the same
      list - and when it was a string, only one of them existed. }
    TPPISource = (
        psNone,           //  nothing answered
        psOverride,       //  /DPI on the command line
        psEnvironment,    //  FIT_UI_DPI
        psGdkScale,       //  GDK_SCALE
        psQtScaleFactor,  //  QT_SCALE_FACTOR
        psXftResource,    //  the Xft.dpi X resource
        psWidgetSet       //  what the widget set already thought
        );

    { What to do with a detected ppi. }
    TScalingAction = (
        saRefuse,       //  outside the range a display can be
        saAlreadyRight, //  the widget set had it right; say so and change nothing
        saApply         //  correct the widget set
        );

{ A toolkit scale factor as the toolkits write it - '2', '1.5' - turned into a
  ppi. Zero for anything that is not a positive number, which includes an unset
  variable.

  PARSED WITH A FULL STOP whatever the user's locale says, because that is how
  the toolkits write it. Read through a locale that uses a comma, '1.5' parses
  as 15 and the interface comes up ten times too large. }
function PPIFromScaleFactorText(const AText: string): integer;

{ A ppi written out as an integer. Zero when it is not a positive whole number. }
function PPIFromText(const AText: string): integer;

{ Xft.dpi out of an X resource database.

  THE ONE THAT MATTERS ON A SCALED LINUX DESKTOP. GTK2 has no idea the desktop is
  scaled - it predates the notion, and what Plasma and GNOME publish is the GTK3
  scheme it does not understand - so it reports 96 for a 200 % display with
  complete confidence. The same desktop still writes 'Xft.dpi: 192' into the root
  window's resource database, because that is what Qt and Xft read.

  MATCHED AT THE START OF A LINE. The database is one 'name:<tab>value' per line
  and names are free-form, so a resource whose name merely ends in Xft.dpi must
  not be allowed to answer for the display. }
function XftDpiFromDatabase(const ADatabase: string; out APPI: integer): boolean;

{ True when APPI is a display rather than a misconfiguration. }
function IsUsablePPI(APPI: integer): boolean;

{ How the source reads in the log. }
function PPISourceName(ASource: TPPISource): string;

{ What to do, given what was detected and what the widget set already thought. }
function ScalingAction(ADetected, AFromWidgetSet: integer): TScalingAction;

{ True when a caption ANeeded pixels wide fits a control AWidth pixels wide.

  A button draws its caption inside a border and some padding, so equal widths
  are already clipped; a plain label is not. One tolerance for both, deliberately
  loose - this is here to find the captions that are wrong, not to police the
  last pixel. }
function CaptionFits(ANeeded, AWidth, APadding: integer): boolean;

implementation

function PPIFromScaleFactorText(const AText: string): integer;
var
    Text: string;
    Factor: double;
    Sep: TFormatSettings;
begin
    Result := 0;
    Text := Trim(AText);
    if Text = '' then
        Exit;
    Sep := DefaultFormatSettings;
    Sep.DecimalSeparator := '.';
    if not TryStrToFloat(Text, Factor, Sep) then
        Exit;
    if Factor <= 0 then
        Exit;
    Result := Round(DesignPPI * Factor);
end;

function PPIFromText(const AText: string): integer;
begin
    Result := StrToIntDef(Trim(AText), 0);
    if Result < 0 then
        Result := 0;
end;

function XftDpiFromDatabase(const ADatabase: string; out APPI: integer): boolean;
const
    Key = 'Xft.dpi:';
var
    Line: string;
    LineEnd, KeyAt, Value: integer;
begin
    Result := False;
    APPI := 0;
    KeyAt := Pos(Key, ADatabase);
    while KeyAt > 0 do
    begin
        if (KeyAt = 1) or (ADatabase[KeyAt - 1] = #10) then
        begin
            LineEnd := KeyAt;
            while (LineEnd <= Length(ADatabase)) and
                (ADatabase[LineEnd] <> #10) do
                Inc(LineEnd);
            Line := Trim(Copy(ADatabase, KeyAt + Length(Key),
                LineEnd - KeyAt - Length(Key)));
            if TryStrToInt(Line, Value) and (Value > 0) then
            begin
                APPI := Value;
                Result := True;
            end;
            //  THE FIRST MATCHING LINE DECIDES, whether or not its value is
            //  usable. A database holding 'Xft.dpi:' twice is already
            //  contradictory, and reading on to find one that parses would pick
            //  the entry the rest of the desktop is not using.
            Exit;
        end;
        KeyAt := PosEx(Key, ADatabase, KeyAt + 1);
    end;
end;

function IsUsablePPI(APPI: integer): boolean;
begin
    Result := (APPI >= MinPPI) and (APPI <= MaxPPI);
end;

function PPISourceName(ASource: TPPISource): string;
begin
    case ASource of
        psOverride: Result := '/DPI on the command line';
        psEnvironment: Result := 'FIT_UI_DPI';
        psGdkScale: Result := 'GDK_SCALE';
        psQtScaleFactor: Result := 'QT_SCALE_FACTOR';
        psXftResource: Result := 'the Xft.dpi X resource';
        psWidgetSet: Result := 'the widget set';
        else
            //  Named rather than blank: "we asked and the display really is 96"
            //  and "we never found out" must not look identical in the log.
            Result := 'nothing';
    end;
end;

function ScalingAction(ADetected, AFromWidgetSet: integer): TScalingAction;
begin
    if not IsUsablePPI(ADetected) then
        Result := saRefuse
    else if ADetected = AFromWidgetSet then
        Result := saAlreadyRight
    else
        Result := saApply;
end;

function CaptionFits(ANeeded, AWidth, APadding: integer): boolean;
begin
    Result := ANeeded <= AWidth - APadding;
end;

end.
