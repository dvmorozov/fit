// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The system's appearance: whether it is dark, and - on macOS - putting the application in one.)

ONLY THE OPERATING-SYSTEM CALLS ARE HERE. What an answer means and which
appearance a choice asks for are app_theme's - ColorIsDark, WindowsAppsAreDark,
MacInterfaceStyleIsDark and MacAppearanceNameFor, all tested - and this unit is
the calls in front of them, kept small because none of it runs headlessly.

macOS reads the system's own setting (the AppleInterfaceStyle default), NOT the
window colour: an explicit Light or Dark puts the application in that appearance
(UseAppAppearance), and the window colour then reports the application's choice
rather than the system's - so Follow System, chosen after Dark, would have
decided "dark" on a light Mac. The setting is the system's alone.

macOS IS ALSO WHERE A CHOICE REACHES THE CONTROLS THE SYSTEM DRAWS. Without it,
Light or Dark chosen against the system changed only what this application
paints - the chart dark, the buttons and lists around it light. Setting the
application's appearance makes Cocoa draw every control, dialog and frame in it,
and the LCL repaints on the change it observes (cocoaapplication.pas). Follow
System clears it, so the application follows the system again. The bundle must
not force Aqua for any of this (NSRequiresAquaSystemAppearance, findings.md,
"The window stayed light on a dark Mac").

LINUX reads the window background the widget set reports, which Qt takes from
the desktop palette; there is no application-wide appearance to set.

WINDOWS reads the registry, because the window colour stays white under the dark
setting there: the Win32 widget set does not follow it, and cannot be put in a
dark appearance without an undocumented API this application does not use.
}
unit system_appearance;

{ THE WIDGET SET, NOT THE OPERATING SYSTEM, decides the macOS branch: the test
  suite is built for macOS with the headless nogui widget set, which has no
  Cocoa application to ask or to set. }

{$mode objfpc}{$H+}
{$IFDEF LCLCOCOA}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
    app_theme;

{ Whether the system is set to a dark appearance now. }
function SystemAppearanceIsDark: boolean;

{ Puts the application in the appearance AMode asks for, so the controls the
  system draws agree with the palette - on macOS. Does nothing elsewhere. }
procedure UseAppAppearance(AMode: TThemeMode);

implementation

uses
{$IFDEF WINDOWS}
    Registry, Windows;
{$ELSE}
{$IFDEF LCLCOCOA}
    CocoaAll, cocoa_extra;
{$ELSE}
    Graphics;
{$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
function SystemAppearanceIsDark: boolean;
const
    Key = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
    Value = 'AppsUseLightTheme';
var
    R: TRegistry;
    Found: boolean;
    Light: longint;
begin
    Found := False;
    Light := 1;
    R := TRegistry.Create(KEY_READ);
    try
        R.RootKey := HKEY_CURRENT_USER;
        if R.OpenKeyReadOnly(Key) and R.ValueExists(Value) then
        begin
            Light := R.ReadInteger(Value);
            Found := True;
        end;
    finally
        R.Free;
    end;
    Result := WindowsAppsAreDark(Found, Light);
end;

procedure UseAppAppearance(AMode: TThemeMode);
begin
end;
{$ELSE}
{$IFDEF LCLCOCOA}
function SystemAppearanceIsDark: boolean;
var
    Style: NSString;
begin
    Style := NSUserDefaults.standardUserDefaults.stringForKey(
        NSString.stringWithUTF8String('AppleInterfaceStyle'));
    if Assigned(Style) then
        Result := MacInterfaceStyleIsDark(Style.UTF8String)
    else
        Result := MacInterfaceStyleIsDark('');
end;

procedure UseAppAppearance(AMode: TThemeMode);
var
    Name: string;
begin
    //  setAppearance: is macOS 10.14; the bundle requires 11.0.
    if not NSApp.respondsToSelector(ObjCSelector('setAppearance:')) then
        Exit;
    Name := MacAppearanceNameFor(AMode);
    if Name = '' then
        NSApp.setAppearance(nil)
    else
        NSApp.setAppearance(NSAppearance.appearanceNamed(
            NSString.stringWithUTF8String(PChar(Name))));
end;
{$ELSE}
function SystemAppearanceIsDark: boolean;
begin
    Result := ColorIsDark(ColorToRGB(clWindow));
end;

procedure UseAppAppearance(AMode: TThemeMode);
begin
end;
{$ENDIF}
{$ENDIF}

end.
