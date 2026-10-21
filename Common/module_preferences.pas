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

WHY NOT THE WINDOW'S SETTINGS. Two reasons. The framework's settings are a
published component whose properties are its own; a module adding one would be a
module editing a framework file. And the window reads its settings AFTER it has
built the modules' menus - which a module declares with the choice already
ticked - so a module could not read its choice back in time. These are read
whenever they are first asked for.

WHERE THEY ARE KEPT: the 'preferences' section of this machine's settings.json
(machine_settings), which was a file of its own, module-preferences.txt, until
everything this machine remembers became one file. Each choice is written the
moment it is made.

THE KEYS ARE THE MODULE'S, and prefixed with its name ('mymodule.price-column'),
so two modules cannot overwrite each other. They compare in any case. The values
are strings; what they mean is the module's business.

NOTHING IS WRITTEN UNTIL THE APPLICATION NAMES THE FILE
(machine_settings.UseMachineSettings). Until then choices are kept in memory: a
test binary that never names one cannot write a user's configuration.
}
unit module_preferences;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

const
    { The section of this machine's settings.json the choices are kept in. }
    ModulePreferencesSection = 'preferences';

{ The choice stored under AKey, or ADefault when none is. }
function ModulePreference(const AKey: string; const ADefault: string = ''): string;

{ Stores AValue under AKey, and writes the file when there is one. A file that
  cannot be written costs the choice between sessions, never the session:
  nothing is raised. A line break in AValue is replaced by a space. }
procedure SetModulePreference(const AKey, AValue: string);

implementation

uses
    machine_settings;

function ModulePreference(const AKey: string; const ADefault: string): string;
begin
    Result := MachineSettings.Str(ModulePreferencesSection, Trim(AKey), ADefault);
end;

procedure SetModulePreference(const AKey, AValue: string);
var
    Value: string;
begin
    //  ONE LINE, AS IT HAS ALWAYS BEEN. The file it was first kept in had a line
    //  per choice; JSON would carry the break, but a module reads back what this
    //  contract has always given it, and nothing a module keeps is a paragraph.
    Value := StringReplace(StringReplace(AValue, #13, ' ', [rfReplaceAll]),
        #10, ' ', [rfReplaceAll]);
    MachineSettings.SetStr(ModulePreferencesSection, Trim(AKey), Value);
end;

end.
