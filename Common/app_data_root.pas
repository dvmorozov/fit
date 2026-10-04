// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where this application keeps a user's things that are not
documents: the settings, the logs, and the per-user data.)

SETTINGS AND LOGS, added later. Earlier versions had every process put both in
a folder called Fit in the HOME folder itself (APPDATA\Fit on Windows), decided inside
log.pas. That is no platform's convention, and it is where a developer's builds
and tests left a hundred megabytes of logs. Each now goes where its platform
says (AppConfigDirIn, AppLogDirIn); the old folder is emptied into them the first
time this version runs (MoveLegacyUserFiles).

THE PROFILE FOLDER (FIT_PROFILE_DIR), which the build gives every process it
starts, holds what a build's runs PRODUCE - the logs and the downloaded data -
so building and testing leave none of it in the home folder. It does NOT hold
what the user chose or made: the settings (machine_settings) and the curve types
the user defined (AppConfigDirIn) are the same for an installed copy and for a
copy started from a checkout on the same machine. Both used to give way to the
profile folder too, and each copy then had a set of its own.

THE DATA ROOT:

WHERE IT CAME FROM. This was inside sidecar_launch, deciding where the Python
virtual environment lives. It is not about Python: it is "where does this
application keep per-user data that is not a document?", and the second answer
needed is where a downloaded data file is cached. Two copies of the rule would
put one of them under the roaming profile the day somebody corrected only one.

WHY THE ENVIRONMENT IS AN ARGUMENT rather than something read here: every branch
is then reachable from a test without setting a variable in the test process, and
a machine that can name none of them gets '' rather than a path rooted at ''.

LOCALAPPDATA RATHER THAN APPDATA on Windows: the roaming profile would carry
compiled extensions and cached downloads between machines, where the first mean
nothing and the second are simply large.

THE NAME DIFFERS BY PLATFORM - 'Fit' on Windows, 'fit' under XDG - because that
is what each platform's own conventions look like, and because it is what the
sidecar has always used: changing it would strand the environments already
installed on every machine.
}
unit app_data_root;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ The directory holding this application's per-user data, or '' when the
  environment names no home at all. Each argument is one environment variable,
  empty when it is unset; which ones exist on this platform is the caller's
  business, and the unused ones arrive empty. }
function AppDataRootFrom(const ALocalAppData, AXdgData, AHome: string): string;

{ The same, reading this platform's variables - and FIT_PROFILE_DIR, which puts
  it under the profile folder. No file is opened and no directory is listed. }
function AppDataRoot: string;

{ ASubdirectory of the root, or '' when there is no root - so a caller cannot
  turn "nowhere" into a relative path that resolves against the current
  directory, which is wherever the application happened to be started. }
function AppDataDir(const ASubdirectory: string): string;

type
    { What this program reads to decide where a user's things go: one field per
      environment variable, empty when it is unset. A record rather than more
      arguments, so that every rule below is a function of its argument alone and
      every platform's branch is reachable from a test. }
    TUserDirsEnvironment = record
        { FIT_PROFILE_DIR: ONE folder for everything, used instead of the
          platform's places. The build sets it for every process it starts, so a
          developer's runs and tests keep their state in the checkout, apart from
          an installed copy's. Never set by an installer. }
        Profile: string;
        Home: string;          { HOME }
        AppData: string;       { APPDATA - Windows, the roaming profile }
        LocalAppData: string;  { LOCALAPPDATA - Windows }
        XdgConfigHome: string; { XDG_CONFIG_HOME }
        XdgDataHome: string;   { XDG_DATA_HOME }
        XdgStateHome: string;  { XDG_STATE_HOME }
    end;

{ This process's environment, as the rules below take it. The ONE place here,
  beside AppDataRoot, that reads a variable. }
function UserDirsEnvironment: TUserDirsEnvironment;

{ The folder holding the curve types the user defined (*.cpr) - and, before
  settings.json, the settings (config.xml, module-preferences.txt) - or '' when
  the environment names no home. The SAME FOLDER under a profile folder: the
  curve types are the user's, whichever copy of Fit is running. What this
  program remembers about the machine is not here: it is machine_settings', in
  AppMachineStateDirIn. }
function AppConfigDirIn(const AEnv: TUserDirsEnvironment): string;

{ The folder holding the logs, or ''. Never the settings folder: a clean removes
  the logs and keeps the settings, and a bug report attaches only the logs. }
function AppLogDirIn(const AEnv: TUserDirsEnvironment): string;

{ The folder holding what is true of THIS MACHINE only - settings.json, which
  holds the projects opened on it, whose paths mean nothing anywhere else, and
  every other setting (machine_settings) - or '' when the environment names no
  home.

  NEVER THE PROFILE FOLDER, although every other folder here gives way to it.
  That folder is in a checkout, and a checkout is copied, synced and shared
  between machines: the recent list kept there went with it, and a Mac offered a
  Linux machine's /mnt/data paths in File > Open Recent. So a build's runs share
  this folder with an installed copy on the same machine - which is right for a
  list of the user's own files - and a test never reaches it, because nothing
  writes here until the application names the file (machine_settings).

  AND NOT THE SETTINGS FOLDER ON WINDOWS, which is the ROAMING profile and would
  carry the paths to every machine the account signs in to. }
function AppMachineStateDirIn(const AEnv: TUserDirsEnvironment): string;

{ AppDataRootFrom, unless a profile folder takes its place. }
function AppDataRootIn(const AEnv: TUserDirsEnvironment): string;

{ The folder earlier versions kept settings AND logs in - HOME/Fit, or
  APPDATA\Fit on Windows - or '' when there is nothing to adopt from it: no home,
  or a profile folder, whose processes must never move an installed copy's
  settings into a checkout. }
function LegacyUserDirIn(const AEnv: TUserDirsEnvironment): string;

{ Where a file named AName in the old folder goes: AConfigDir for the settings
  and the curve types the user defined, ALogDir for every process's log - or ''
  for a file this program did not write, for the settings when AConfigDir IS the
  old folder (Windows), and when there is no folder to go to. }
function LegacyFileDestination(const AName, ALegacyDir, AConfigDir,
    ALogDir: string): string;

{ Moves what an earlier version left in ALegacyDir: the settings to AConfigDir,
  the logs to ALogDir. A file already at its destination is newer and is kept,
  the old one left where it was; a file this program did not write is left too.
  ALegacyDir is removed once it is empty. Best effort throughout: a file that
  cannot be moved stays, and the program runs on. }
procedure MoveLegacyUserFiles(const ALegacyDir, AConfigDir, ALogDir: string);

implementation

{ Base + Name, or '' when there is no Base - so "nowhere" never becomes a
  relative path, which resolves against wherever the program was started. }
function Under(const ABase, AName: string): string;
begin
    if ABase = '' then
        Exit('');
    Result := IncludeTrailingPathDelimiter(ABase) + AName;
end;

function UserDirsEnvironment: TUserDirsEnvironment;
begin
    Result.Profile := GetEnvironmentVariable('FIT_PROFILE_DIR');
    Result.Home := GetEnvironmentVariable('HOME');
    Result.AppData := GetEnvironmentVariable('APPDATA');
    Result.LocalAppData := GetEnvironmentVariable('LOCALAPPDATA');
    Result.XdgConfigHome := GetEnvironmentVariable('XDG_CONFIG_HOME');
    Result.XdgDataHome := GetEnvironmentVariable('XDG_DATA_HOME');
    Result.XdgStateHome := GetEnvironmentVariable('XDG_STATE_HOME');
end;

{ Each platform's own convention, and nothing in the home folder itself - that
  is what HOME/Fit was, and what this replaced:
    Windows - settings roam with the account (APPDATA), logs are large and about
              one machine, so they stay on it (LOCALAPPDATA\Fit\Logs);
    macOS   - ~/Library/Application Support/Fit, and ~/Library/Logs/Fit, which
              is where Console lists every application's logs;
    others  - the XDG base directories: settings under XDG_CONFIG_HOME
              (~/.config), logs under XDG_STATE_HOME (~/.local/state), which the
              specification names for logs.
  The name is 'Fit' where the platform capitalises, 'fit' under XDG, as
  AppDataRootFrom already does. }
function AppConfigDirIn(const AEnv: TUserDirsEnvironment): string;
begin
    //  NO PROFILE BRANCH - see the header: the profile folder is for what a
    //  build's runs produce, not for what the user made.
{$IFDEF WINDOWS}
    Result := Under(AEnv.AppData, 'Fit');
{$ELSE}
{$IFDEF DARWIN}
    Result := Under(AEnv.Home, 'Library/Application Support/Fit');
{$ELSE}
    if AEnv.XdgConfigHome <> '' then
        Result := Under(AEnv.XdgConfigHome, 'fit')
    else
        Result := Under(AEnv.Home, '.config/fit');
{$ENDIF}
{$ENDIF}
end;

function AppLogDirIn(const AEnv: TUserDirsEnvironment): string;
begin
    if AEnv.Profile <> '' then
        Exit(Under(AEnv.Profile, 'logs'));
{$IFDEF WINDOWS}
    Result := Under(AEnv.LocalAppData, 'Fit\Logs');
{$ELSE}
{$IFDEF DARWIN}
    Result := Under(AEnv.Home, 'Library/Logs/Fit');
{$ELSE}
    if AEnv.XdgStateHome <> '' then
        Result := Under(AEnv.XdgStateHome, 'fit')
    else
        Result := Under(AEnv.Home, '.local/state/fit');
{$ENDIF}
{$ENDIF}
end;

{ Each platform's place for data that persists and is not portable:
    Windows - LOCALAPPDATA\Fit, the local half of the profile, which is already
              the data root;
    macOS   - ~/Library/Application Support/Fit, beside the settings: nothing
              synchronises that folder between machines;
    others  - XDG_STATE_HOME (~/.local/state), which the specification names for
              "recently used files" among other state not worth carrying. }
function AppMachineStateDirIn(const AEnv: TUserDirsEnvironment): string;
begin
{$IFDEF WINDOWS}
    Result := Under(AEnv.LocalAppData, 'Fit');
{$ELSE}
{$IFDEF DARWIN}
    Result := Under(AEnv.Home, 'Library/Application Support/Fit');
{$ELSE}
    if AEnv.XdgStateHome <> '' then
        Result := Under(AEnv.XdgStateHome, 'fit')
    else
        Result := Under(AEnv.Home, '.local/state/fit');
{$ENDIF}
{$ENDIF}
end;

function AppDataRootIn(const AEnv: TUserDirsEnvironment): string;
begin
    if AEnv.Profile <> '' then
        Exit(Under(AEnv.Profile, 'data'));
    Result := AppDataRootFrom(AEnv.LocalAppData, AEnv.XdgDataHome, AEnv.Home);
end;

function LegacyUserDirIn(const AEnv: TUserDirsEnvironment): string;
begin
    if AEnv.Profile <> '' then
        Exit('');
{$IFDEF WINDOWS}
    Result := Under(AEnv.AppData, 'Fit');
{$ELSE}
    Result := Under(AEnv.Home, 'Fit');
{$ENDIF}
end;

{ The names this program wrote there. Recognised by name rather than by "every
  file", because the folder was a plain one in the home folder and anything
  else in it is somebody else's - the build kept vm.json there, for one.
  config.xml and module-preferences.txt still go to the settings folder: that
  is where legacy_settings_import looks for them, to carry them into
  settings.json on the same first run. }
function IsSettingsFile(const AName: string): boolean;
begin
    Result := (AName = 'config.xml') or (AName = 'module-preferences.txt') or
        (LowerCase(ExtractFileExt(AName)) = '.cpr');
end;

{ Every process's log, with its rotated generation (.1) and the copies the
  build sets aside (.before-check-ui): log.txt, fit_client.log,
  fit_server_log.txt, fit_sidecar_log.txt. }
function IsLogFile(const AName: string): boolean;
var
    Lower: string;
begin
    Lower := LowerCase(AName);
    Result := (Pos('log.txt', Lower) > 0) or (ExtractFileExt(Lower) = '.log') or
        (Pos('.log.', Lower) > 0);
end;

procedure MoveInto(const AFrom, AToDir, AName: string);
var
    Target: string;
begin
    Target := IncludeTrailingPathDelimiter(AToDir) + AName;
    if FileExists(Target) then
        Exit;
    if not DirectoryExists(AToDir) and not ForceDirectories(AToDir) then
        Exit;
    RenameFile(AFrom, Target);
end;

function SameDir(const A, B: string): boolean;
begin
    Result := SameFileName(ExcludeTrailingPathDelimiter(A),
        ExcludeTrailingPathDelimiter(B));
end;

function LegacyFileDestination(const AName, ALegacyDir, AConfigDir,
    ALogDir: string): string;
begin
    Result := '';
    if IsSettingsFile(AName) then
    begin
        if not SameDir(ALegacyDir, AConfigDir) then
            Result := AConfigDir;
    end
    else if IsLogFile(AName) then
        Result := ALogDir;
end;

procedure MoveLegacyUserFiles(const ALegacyDir, AConfigDir, ALogDir: string);
var
    S: TSearchRec;
    Names: TStringList;
    Name, Dest: string;
begin
    if (ALegacyDir = '') or not DirectoryExists(ALegacyDir) then
        Exit;
    //  Listed first and moved after: renaming inside a FindFirst loop is not
    //  something every platform's directory reading promises to survive.
    Names := TStringList.Create;
    try
        if FindFirst(IncludeTrailingPathDelimiter(ALegacyDir) + '*', faAnyFile, S) = 0 then
        try
            repeat
                if (S.Attr and faDirectory) = 0 then
                    Names.Add(S.Name);
            until FindNext(S) <> 0;
        finally
            FindClose(S);
        end;
        for Name in Names do
        begin
            Dest := LegacyFileDestination(Name, ALegacyDir, AConfigDir, ALogDir);
            if Dest <> '' then
                MoveInto(IncludeTrailingPathDelimiter(ALegacyDir) + Name, Dest, Name);
        end;
    finally
        Names.Free;
    end;
    //  Fails, harmlessly, while anything is left in it.
    if not SameDir(ALegacyDir, AConfigDir) then
        RemoveDir(ALegacyDir);
end;

function AppDataRootFrom(const ALocalAppData, AXdgData, AHome: string): string;
var
    Base: string;
begin
    Result := '';
{$IFDEF WINDOWS}
    Base := ALocalAppData;
    if Base <> '' then
        Result := IncludeTrailingPathDelimiter(Base) + 'Fit';
{$ELSE}
    Base := AXdgData;
    if Base = '' then
    begin
        Base := AHome;
        if Base <> '' then
            Base := IncludeTrailingPathDelimiter(Base) + '.local/share';
    end;
    if Base <> '' then
        Result := IncludeTrailingPathDelimiter(Base) + 'fit';
{$ENDIF}
end;

function AppDataRoot: string;
begin
    Result := AppDataRootIn(UserDirsEnvironment);
end;

function AppDataDir(const ASubdirectory: string): string;
begin
    Result := AppDataRoot;
    if Result <> '' then
        Result := IncludeTrailingPathDelimiter(Result) + ASubdirectory;
end;

end.
