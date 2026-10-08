// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The one file holding what this program remembers about this machine:
settings.json.)

WHY ONE FILE. There were three - config.xml (the window's settings),
module-preferences.txt (the modules' choices and the framework's own update and
notice keys) and recent-projects.txt (the project to reopen and File > Open
Recent) - in two folders whose rules differed by platform and by whether a
build's FIT_PROFILE_DIR was set. Each had its own format, its own writer and its
own answer to "where is it". A setting is now a key in a section of this file,
and adding one is never a new file.

WHY THIS MACHINE'S FOLDER (app_data_root.AppMachineStateDirIn), AND NEVER THE
PROFILE FOLDER. These settings are about one machine by definition: the paths
of the projects opened on it, the server it reaches, the size its panes are read
at. A profile folder is in a checkout, and a checkout is copied, synced and
shared - a Mac offered a Linux machine's /mnt/data paths in File > Open Recent -
and the Windows settings folder roams with the account. So a build's runs share
this file with an installed copy on the same machine, and what must not write
it - a test, the window checking itself - is given no file name rather than a
different one (MachineSettingsFileFor).

WHY SECTIONS, AND WHY A WRITE IS ONE SECTION. Each owner has one top-level
object - 'app' (app_settings), 'recent' (recent_project_store), 'preferences'
(module_preferences) - and writes only that. A write reads the file again and
replaces its own section in what it finds, so a second window open at the same
time does not put the first one's section back as it was when it started: three
separate files used to guarantee that for free, and one file must not lose it.
What this build does not know - a newer build's section, a key in a known
section - is carried through every rewrite untouched, which is what makes the
format extensible without a migration each time.

WHY KEYS COMPARE IN ANY CASE: module_preferences always did, as the registries
compare ids, and Pascal property names do too. A key spelled differently in two
places is one setting and one entry in the file.

AN UNREADABLE FILE IS SET ASIDE, NOT OVERWRITTEN. A crash part way through an
editor's save, or a hand edit that is not JSON: the session runs on its
defaults, and the file is renamed to settings.json.unreadable before anything
is written, so nothing the user had is destroyed without a copy. The file
itself is replaced atomically (safe_replace), so this program never leaves one
half written.

THE DISK IS BEHIND ReadText, WriteText AND SetAsideUnreadable, which a test
double overrides - the shape recent_project_store had - so every rule here is a
unit test and no test writes a user's file.
}
unit machine_settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpjson, app_data_root;

const
    MachineSettingsFileName = 'settings.json';
    { What the file says it is. Raised only when a section changes what it
      MEANS; a section or a key added is not a new format. }
    MachineSettingsFormat = 1;

type
    TMachineSettings = class
    private
        FFileName: string;
        FDoc: TJSONObject;
        { Takes the file as it is now, when there is one. One that cannot be
          read is set aside and what is held is kept: a memory-only store, and
          a session whose file went bad, go on with what they have. }
        procedure Refresh;
        { ASection's object in FDoc, or nil when it has none. }
        function SectionObject(const ASection: string): TJSONObject;
    protected
        { The seam in front of the disk. ReadText answers False when there is no
          file; WriteText never raises - a file that cannot be written costs the
          next session the change, never this session anything. }
        function ReadText(out AText: string): boolean; virtual;
        procedure WriteText(const AText: string); virtual;
        { Moves an unreadable file out of the way, keeping it. }
        procedure SetAsideUnreadable; virtual;
    public
        { Reads AFileName when it is there. '' keeps everything in memory only. }
        constructor Create(const AFileName: string);
        destructor Destroy; override;
        { A copy of ASection, empty when there is none; the caller frees it. }
        function Section(const ASection: string): TJSONObject;
        { Makes AObject the whole of ASection - taking it over - and writes the
          file: re-read, this section replaced, every other one as found. }
        procedure ReplaceSection(const ASection: string; AObject: TJSONObject);
        { The text stored under AKey in ASection, in any case, or ADefault when
          there is none or it is not text. }
        function Str(const ASection, AKey: string; const ADefault: string = ''): string;
        { Stores AValue under AKey in ASection, replacing the key in whatever
          case it was spelled, and writes the file. }
        procedure SetStr(const ASection, AKey, AValue: string);
        property FileName: string read FFileName;
    end;

{ The value under AKey in AObject - a section, or an object inside one -
  compared in any case as every key in the file is, or nil when there is none.
  For an owner reading what Str does not: a list, a number. }
function ValueIn(AObject: TJSONObject; const AKey: string): TJSONData;

{ The file this process uses: in this machine's own folder whatever profile
  folder the environment names, or '' when it names no home - and none at all
  when the window is checking itself
  (/CHECK_UI), whose changes are never the user's. A recording is not such a
  run: it is OF the user's settings and the project last open, so it reads
  them like any other. }
function MachineSettingsFileFor(ACheckingItself: boolean;
    const AEnv: TUserDirsEnvironment): string;

{ The settings this process keeps. In memory until UseMachineSettings names a
  file, so a binary that never names one - every test - writes nothing. }
function MachineSettings: TMachineSettings;

{ Keep the settings in AFileName from now on, reading it now; '' keeps them in
  memory only. Whatever was held before is forgotten. }
procedure UseMachineSettings(const AFileName: string);

implementation

uses
    jsonparser, safe_replace;

{ The index of AKey among AObject's names, compared in any case, or -1. }
function KeyIndex(AObject: TJSONObject; const AKey: string): longint;
var
    i: longint;
begin
    for i := 0 to AObject.Count - 1 do
        if CompareText(AObject.Names[i], AKey) = 0 then
            Exit(i);
    Result := -1;
end;

function ValueIn(AObject: TJSONObject; const AKey: string): TJSONData;
var
    i: longint;
begin
    i := KeyIndex(AObject, AKey);
    if i < 0 then
        Exit(nil);
    Result := AObject.Items[i];
end;

constructor TMachineSettings.Create(const AFileName: string);
begin
    inherited Create;
    FFileName := AFileName;
    FDoc := TJSONObject.Create;
    Refresh;
end;

destructor TMachineSettings.Destroy;
begin
    FDoc.Free;
    inherited Destroy;
end;

procedure TMachineSettings.Refresh;
var
    Text: string;
    Data: TJSONData;
begin
    if not ReadText(Text) then
        Exit;
    try
        Data := GetJSON(Text);
    except
        Data := nil;
    end;
    if Data is TJSONObject then
    begin
        FDoc.Free;
        FDoc := TJSONObject(Data);
    end
    else
    begin
        Data.Free;
        SetAsideUnreadable;
    end;
end;

function TMachineSettings.ReadText(out AText: string): boolean;
var
    Lines: TStringList;
begin
    AText := '';
    Result := False;
    if (FFileName = '') or not FileExists(FFileName) then
        Exit;
    Lines := TStringList.Create;
    try
        try
            Lines.LoadFromFile(FFileName);
            AText := Lines.Text;
            Result := True;
        except
            //  An unreadable file is a machine with nothing remembered yet.
        end;
    finally
        Lines.Free;
    end;
end;

procedure TMachineSettings.WriteText(const AText: string);
var
    Content: TStringStream;
    Fault: string;
begin
    if FFileName = '' then
        Exit;
    Content := TStringStream.Create(AText);
    try
        try
            //  A first run: the platform's folder may not exist yet.
            ForceDirectories(ExtractFileDir(FFileName));
            ReplaceFileWith(FFileName, Content, Fault);
        except
            //  See the declaration.
        end;
    finally
        Content.Free;
    end;
end;

procedure TMachineSettings.SetAsideUnreadable;
var
    Aside: string;
begin
    if FFileName = '' then
        Exit;
    Aside := FFileName + '.unreadable';
    DeleteFile(Aside);
    RenameFile(FFileName, Aside);
end;

function TMachineSettings.SectionObject(const ASection: string): TJSONObject;
var
    i: longint;
begin
    Result := nil;
    i := KeyIndex(FDoc, ASection);
    if (i >= 0) and (FDoc.Items[i] is TJSONObject) then
        Result := TJSONObject(FDoc.Items[i]);
end;

function TMachineSettings.Section(const ASection: string): TJSONObject;
var
    S: TJSONObject;
begin
    S := SectionObject(ASection);
    if Assigned(S) then
        Result := TJSONObject(S.Clone)
    else
        Result := TJSONObject.Create;
end;

procedure TMachineSettings.ReplaceSection(const ASection: string; AObject: TJSONObject);
var
    i: longint;
begin
    //  AS THE FILE IS NOW, not as it was when this process read it.
    Refresh;
    i := KeyIndex(FDoc, ASection);
    if i >= 0 then
        FDoc.Delete(i);
    FDoc.Add(ASection, AObject);
    if KeyIndex(FDoc, 'format') < 0 then
        FDoc.Add('format', MachineSettingsFormat);
    WriteText(FDoc.FormatJSON);
end;

function TMachineSettings.Str(const ASection, AKey, ADefault: string): string;
var
    S: TJSONObject;
    i: longint;
begin
    Result := ADefault;
    S := SectionObject(ASection);
    if not Assigned(S) then
        Exit;
    i := KeyIndex(S, AKey);
    if (i >= 0) and (S.Items[i] is TJSONString) then
        Result := S.Items[i].AsString;
end;

procedure TMachineSettings.SetStr(const ASection, AKey, AValue: string);
var
    S: TJSONObject;
    i: longint;
begin
    //  THE SECTION AS IT IS NOW, so a key another window wrote into it since
    //  is not put back as it was.
    Refresh;
    S := Section(ASection);
    i := KeyIndex(S, AKey);
    if i >= 0 then
        S.Delete(i);
    S.Add(AKey, AValue);
    ReplaceSection(ASection, S);
end;

{ The file in this machine's own folder, or '' when there is no home. }
function MachineSettingsFileIn(const AEnv: TUserDirsEnvironment): string;
begin
    Result := AppMachineStateDirIn(AEnv);
    if Result <> '' then
        Result := IncludeTrailingPathDelimiter(Result) + MachineSettingsFileName;
end;

function MachineSettingsFileFor(ACheckingItself: boolean;
    const AEnv: TUserDirsEnvironment): string;
begin
    if ACheckingItself then
        Exit('');
    Result := MachineSettingsFileIn(AEnv);
end;

var
    Current: TMachineSettings = nil;

function MachineSettings: TMachineSettings;
begin
    if not Assigned(Current) then
        Current := TMachineSettings.Create('');
    Result := Current;
end;

procedure UseMachineSettings(const AFileName: string);
begin
    FreeAndNil(Current);
    Current := TMachineSettings.Create(AFileName);
end;

finalization
    FreeAndNil(Current);
end.
