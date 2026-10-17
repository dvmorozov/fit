// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Opening this machine's settings.json - and, the first time, carrying
into it what the three files it replaced held.)

THE THREE FILES: config.xml (the window's settings) and module-preferences.txt
(the modules' choices) in the settings folder, recent-projects.txt (the recent
list) in this machine's folder. The first run of a version with one file finds
them, carries what they hold into settings.json, and removes them.

ONLY WHEN settings.json IS NOT THERE YET. Once it is, it is the newer answer: an
old file beside it was written by an older build run since, and importing it
again would put back what the user has changed in the meantime. Such a file is
left alone - it is that build's.

A FILE IS REMOVED ONLY WHEN IT WAS CARRIED OVER, and only once settings.json
has been written: one that cannot be read, or a file that cannot be written,
loses nothing.

THE OLD FILES ARE LOOKED FOR WHERE AN INSTALLED COPY KEPT THEM, AND IN THE
PROFILE FOLDER (FIT_PROFILE_DIR) when there is one. A copy run from a checkout
kept the same machine's settings in var/profile only because every folder used
to give way to that one; they are this machine's settings like any other. An
early draft skipped them as "a developer's runs", which by the definition this
file is built on - machine settings belong to the machine, wherever an older
rule put them - was wrong. Carried after the installed copy's, so where both
say something the checkout's win: they are what the person starting this build
has been using.

THE CURVE TYPES A USER DEFINED (*.cpr) that a copy started from a checkout kept
in var/profile move to the one folder every copy keeps them in
(app_data_root.AppConfigDirIn) - at every start, not only the first, since a
file of one is a document rather than a setting: nothing newer can be lost by
moving it, and one already at its destination is never overwritten.

THE WINDOW CHECKING ITSELF (/CHECK_UI) opens nothing and imports nothing: it
keeps its settings in memory (machine_settings.MachineSettingsFileFor).
}
unit legacy_settings_import;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, app_data_root;

type
    { Where the files settings.json replaced were kept; '' for one that has no
      place, when the environment names no home. }
    TLegacySettingsFiles = record
        Config: string;       { config.xml }
        Preferences: string;  { module-preferences.txt }
        Recent: string;       { recent-projects.txt }
        { The same two in the profile folder, or '' with none. }
        ProfileConfig: string;
        ProfilePreferences: string;
    end;

{ Where an installed copy kept them, and where a copy run with AEnv's profile
  folder kept theirs. }
function LegacySettingsFilesIn(const AEnv: TUserDirsEnvironment): TLegacySettingsFiles;

{ Names this process's settings file - none while the window checks itself -
  and, when it is not there yet, carries the old files into it and removes
  them. The ONE call the application makes. }
procedure OpenMachineSettings(ACheckingItself: boolean; const AEnv: TUserDirsEnvironment);

implementation

uses
    Laz_XMLCfg, app_settings, machine_settings, module_preferences,
    recent_project_store;

type
    { The class finder the component reader asks, as a method: what a
      settings file may name is app_settings.SettingsComponentClass's. }
    TSettingsClassFinder = class
        procedure Find(Reader: TReader; const AClassName: string;
            var ComponentClass: TComponentClass);
    end;

procedure TSettingsClassFinder.Find(Reader: TReader; const AClassName: string;
    var ComponentClass: TComponentClass);
begin
    ComponentClass := SettingsComponentClass(AClassName);
end;

function Under(const ADir, AName: string): string;
begin
    if ADir = '' then
        Exit('');
    Result := IncludeTrailingPathDelimiter(ADir) + AName;
end;

function LegacySettingsFilesIn(const AEnv: TUserDirsEnvironment): TLegacySettingsFiles;
var
    Installed: TUserDirsEnvironment;
begin
    Installed := AEnv;
    Installed.Profile := '';
    Result.Config := Under(AppConfigDirIn(Installed), 'config.xml');
    Result.Preferences := Under(AppConfigDirIn(Installed), 'module-preferences.txt');
    Result.Recent := Under(AppMachineStateDirIn(Installed), 'recent-projects.txt');
    Result.ProfileConfig := Under(AEnv.Profile, 'config.xml');
    Result.ProfilePreferences := Under(AEnv.Profile, 'module-preferences.txt');
end;

{ The window's settings as config.xml held them, into the 'app' section. }
function ImportConfig(const APath: string; AMachine: TMachineSettings): boolean;
var
    Cfg: TXMLConfig;
    Settings: TComponent;
    Finder: TSettingsClassFinder;
begin
    Result := False;
    Settings := Settings_v1.Create(nil);
    Finder := TSettingsClassFinder.Create;
    try
        try
            Cfg := TXMLConfig.Create(APath);
            try
                ReadComponentFromXMLConfig(Cfg, 'Component', Settings,
                    @Finder.Find, nil);
            finally
                Cfg.Free;
            end;
            WriteAppSettings(AMachine, Settings_v1(Settings));
            Result := True;
        except
            //  Not a settings file this build can read: left where it is.
        end;
    finally
        Finder.Free;
        Settings.Free;
    end;
end;

{ Lines of a key=value file; nil when it cannot be read. }
function ReadLines(const APath: string): TStringList;
begin
    Result := TStringList.Create;
    try
        Result.LoadFromFile(APath);
    except
        FreeAndNil(Result);
    end;
end;

{ Every choice module-preferences.txt held, into the 'preferences' section. }
function ImportPreferences(const APath: string; AMachine: TMachineSettings): boolean;
var
    Lines: TStringList;
    i: longint;
begin
    Lines := ReadLines(APath);
    Result := Assigned(Lines);
    if not Result then
        Exit;
    try
        for i := 0 to Lines.Count - 1 do
            if Trim(Lines.Names[i]) <> '' then
                AMachine.SetStr(ModulePreferencesSection, Trim(Lines.Names[i]),
                    Lines.ValueFromIndex[i]);
    finally
        Lines.Free;
    end;
end;

{ The project to reopen and the recent list, into the 'recent' section. }
function ImportRecent(const APath: string; AMachine: TMachineSettings): boolean;
var
    Lines: TStringList;
    Store: TRecentProjectStore;
begin
    Lines := ReadLines(APath);
    Result := Assigned(Lines);
    if not Result then
        Exit;
    Store := TRecentProjectStore.Create(AMachine);
    try
        //  The two keys recent-projects.txt was written with.
        Store.Adopt(Lines.Values['LastProjectFile'], Lines.Values['RecentProjects']);
    finally
        Store.Free;
        Lines.Free;
    end;
end;

{ The curve types a copy started with AEnv's profile folder kept there, moved
  to where every copy keeps them; one whose name is already taken stays. }
procedure MoveProfileCurveTypes(const AEnv: TUserDirsEnvironment);
var
    S: TSearchRec;
    Names: TStringList;
    Name, Target: string;
begin
    Target := AppConfigDirIn(AEnv);
    if (AEnv.Profile = '') or (Target = '') or not DirectoryExists(AEnv.Profile) then
        Exit;
    //  Listed first and moved after, as app_data_root.MoveLegacyUserFiles
    //  does: renaming inside a FindFirst loop is not something every platform
    //  promises to survive.
    Names := TStringList.Create;
    try
        if FindFirst(IncludeTrailingPathDelimiter(AEnv.Profile) + '*.cpr', faAnyFile, S) = 0 then
        try
            repeat
                if (S.Attr and faDirectory) = 0 then
                    Names.Add(S.Name);
            until FindNext(S) <> 0;
        finally
            FindClose(S);
        end;
        if Names.Count = 0 then
            Exit;
        ForceDirectories(Target);
        for Name in Names do
            if not FileExists(Under(Target, Name)) then
                RenameFile(Under(AEnv.Profile, Name), Under(Target, Name));
    finally
        Names.Free;
    end;
end;

type
    TImporter = function(const APath: string; AMachine: TMachineSettings): boolean;

procedure OpenMachineSettings(ACheckingItself: boolean; const AEnv: TUserDirsEnvironment);
var
    FileName: string;
    Old: TLegacySettingsFiles;
    Carried: TStringList;

    procedure Carry(const APath: string; AImport: TImporter);
    begin
        if (APath <> '') and FileExists(APath) and AImport(APath, MachineSettings) then
            Carried.Add(APath);
    end;

var
    Path: string;
begin
    FileName := MachineSettingsFileFor(ACheckingItself, AEnv);
    UseMachineSettings(FileName);
    if FileName = '' then
        Exit;
    MoveProfileCurveTypes(AEnv);
    if FileExists(FileName) then
        Exit;
    Old := LegacySettingsFilesIn(AEnv);
    Carried := TStringList.Create;
    try
        Carry(Old.Config, @ImportConfig);
        Carry(Old.Preferences, @ImportPreferences);
        Carry(Old.Recent, @ImportRecent);
        //  AFTER the installed copy's, so the checkout's win - see the header.
        Carry(Old.ProfileConfig, @ImportConfig);
        Carry(Old.ProfilePreferences, @ImportPreferences);
        //  Only once the new file holds them: a folder that cannot be written
        //  costs nothing that was there.
        if FileExists(FileName) then
            for Path in Carried do
                DeleteFile(Path);
    finally
        Carried.Free;
    end;
end;

end.
