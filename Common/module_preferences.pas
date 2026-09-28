// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A module's own choices that outlive a session.)

WHY A MODULE NEEDS THIS. A module brings its own menu, and a menu choice that is
forgotten at every restart - which price column a .csv file is read by - is one
the user makes again every day. The framework used to keep that choice in its
own settings, under a name of its own: a field's choice in the framework's file,
which is the thing a module exists to prevent.

WHY NOT THE SETTINGS FILE. Two reasons. The framework's settings are a published
component whose properties are its own; a module adding one would be a module
editing a framework file. And the window reads its settings AFTER it has built
the modules' menus - which a module declares with the choice already ticked -
so a module could not read its choice back in time. This store is read on first
use, whenever that is.

THE KEYS ARE THE MODULE'S, and prefixed with its name ('mymodule.price-column'),
so two modules cannot overwrite each other. The values are strings; what they
mean is the module's business.

NOTHING IS WRITTEN UNTIL THE APPLICATION NAMES A FILE (UseModulePreferencesFile).
Until then choices are kept in memory: a test binary that never names one cannot
write a user's configuration.
}
unit module_preferences;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ The choice stored under AKey, or ADefault when none is. }
function ModulePreference(const AKey: string; const ADefault: string = ''): string;

{ Stores AValue under AKey, and writes the file when there is one. A file that
  cannot be written costs the choice between sessions, never the session:
  nothing is raised. A line break in AValue is replaced by a space. }
procedure SetModulePreference(const AKey, AValue: string);

{ Keep the choices in APath from now on, reading it on the next use; '' keeps
  them in memory only. Whatever was held before is forgotten. }
procedure UseModulePreferencesFile(const APath: string);

{ Where the application keeps them: beside its settings, for the current user. }
function DefaultModulePreferencesFile: string;

implementation

uses
    log;

var
    Preferences: TStringList = nil;
    PreferencesFile: string = '';

function Loaded: TStringList;
begin
    if not Assigned(Preferences) then
    begin
        Preferences := TStringList.Create;
        //  Keys compare as the registries compare ids: without regard to case.
        Preferences.CaseSensitive := False;
        if (PreferencesFile <> '') and FileExists(PreferencesFile) then
            try
                Preferences.LoadFromFile(PreferencesFile);
            except
                //  An unreadable file is no choices yet, not a failure to start.
                Preferences.Clear;
            end;
    end;
    Result := Preferences;
end;

function ModulePreference(const AKey: string; const ADefault: string): string;
var
    i: longint;
begin
    i := Loaded.IndexOfName(Trim(AKey));
    if i < 0 then
        Exit(ADefault);
    Result := Loaded.ValueFromIndex[i];
end;

procedure SetModulePreference(const AKey, AValue: string);
var
    Value: string;
begin
    //  ONE LINE PER CHOICE: a break in the value would read back as a second,
    //  forged key.
    Value := StringReplace(StringReplace(AValue, #13, ' ', [rfReplaceAll]),
        #10, ' ', [rfReplaceAll]);
    Loaded.Values[Trim(AKey)] := Value;
    if PreferencesFile = '' then
        Exit;
    try
        Loaded.SaveToFile(PreferencesFile);
    except
        //  Kept for the session; see the declaration.
    end;
end;

procedure UseModulePreferencesFile(const APath: string);
begin
    FreeAndNil(Preferences);
    PreferencesFile := APath;
end;

function DefaultModulePreferencesFile: string;
begin
    Result := GetConfigDir;
    if Result <> '' then
        Result := Result + 'module-preferences.txt';
end;

finalization
    FreeAndNil(Preferences);
end.
