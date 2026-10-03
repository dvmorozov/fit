// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The guide's tables of where Fit keeps its files, against the rules
that decide it.)

WHY. The guide said the argument axis was kept in the settings for a long time
after it was not, and named a Linux server log path the launcher had never
written. A table of folders is exactly that kind of sentence: true the day it is
written, and wrong after the next change to app_data_root. So each cell is
checked against the function that decides it - for THIS platform's column, the
only one the rules can be asked about here; the CI runs this on all three.

THE ENVIRONMENT IS THE TABLE'S OWN NOTATION. A home of '~' and Windows folders
of '%APPDATA%' and '%LOCALAPPDATA%' make each rule return the text the guide
writes, so a cell is compared as it reads rather than through a translation that
could hide a difference.
}
unit testcase_guide_file_locations;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, explanation,
    app_data_root, machine_settings, sidecar_launch, guide_projects,
    app_settings, recent_project_store, module_preferences, window_layout;

type
    TGuideFileLocationsTest = class(TTestCase)
    private
        { The table row of AExplanation whose first cell begins with AWhat. }
        function Row(const ATopic, AWhat: string): TStringArray;
        { The cell of this platform's column in that row. }
        function Here(const AWhat: string): string;
        { The cell of the column for a copy built from source. }
        function FromSource(const AWhat: string): string;
    published
        procedure TheTableHasAColumnForEachPlatformAndForASourceBuild;
        procedure TheSettingsAreWhereMachineSettingsKeepsThem;
        procedure ASourceBuildSharesTheSettingsWithAnInstalledCopy;
        procedure TheCurveTypesAreInTheSettingsFolder;
        procedure TheLogsAreInTheLogFolder;
        procedure TheDownloadsAreUnderTheDataFolder;
        procedure ThePythonEngineIsUnderTheDataFolder;
        procedure EverySectionOfTheFileIsDescribed;
    end;

implementation

const
    WhatSettings = 'Settings';
    WhatCurveTypes = 'Curve types';
    WhatLogs = 'Logs';
    WhatDownloads = 'Downloaded data';
    WhatPython = 'Python engine';
    ProfileFolder = 'var/profile';

function TableNotation: TUserDirsEnvironment;
begin
    Result := Default(TUserDirsEnvironment);
    Result.Home := '~';
    Result.AppData := '%APPDATA%';
    Result.LocalAppData := '%LOCALAPPDATA%';
end;

function UnderProfile: TUserDirsEnvironment;
begin
    Result := TableNotation;
    Result.Profile := ProfileFolder;
end;

{ A source build's folders are written with '/' in the guide, on every
  platform, as the tree's own paths are. }
function Slashed(const APath: string): string;
begin
    Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
end;

function Topic(const ATopic: string): TExplanation;
var
    E: TExplanation;
begin
    for E in ProjectsExplanations do
        if E.Topic = ATopic then
            Exit(E);
    raise Exception.Create('no topic ' + ATopic);
end;

function TGuideFileLocationsTest.Row(const ATopic, AWhat: string): TStringArray;
var
    Para: string;
    Cells: TStringArray;
begin
    for Para in Topic(ATopic).Body do
        if IsTableRow(Para) then
        begin
            Cells := TableCells(Para);
            if Pos(AWhat, Cells[0]) = 1 then
                Exit(Cells);
        end;
    Fail('the table in ' + ATopic + ' has no row for ' + AWhat);
    Result := nil;
end;

function TGuideFileLocationsTest.Here(const AWhat: string): string;
begin
{$IFDEF WINDOWS}
    Result := Row(FileLocationsTopic, AWhat)[1];
{$ELSE}
{$IFDEF DARWIN}
    Result := Row(FileLocationsTopic, AWhat)[2];
{$ELSE}
    Result := Row(FileLocationsTopic, AWhat)[3];
{$ENDIF}
{$ENDIF}
end;

function TGuideFileLocationsTest.FromSource(const AWhat: string): string;
begin
    Result := Row(FileLocationsTopic, AWhat)[4];
end;

procedure TGuideFileLocationsTest.TheTableHasAColumnForEachPlatformAndForASourceBuild;
var
    Header: TStringArray;
    Para: string;
begin
    Header := nil;
    for Para in Topic(FileLocationsTopic).Body do
        if IsTableRow(Para) then
        begin
            Header := TableCells(Para);
            Break;
        end;
    AssertEquals(5, Length(Header));
    AssertEquals('Windows', Header[1]);
    AssertEquals('macOS', Header[2]);
    AssertEquals('Linux', Header[3]);
    for Para in Topic(FileLocationsTopic).Body do
        if IsTableRow(Para) then
            AssertEquals(Para, 5, Length(TableCells(Para)));
end;

procedure TGuideFileLocationsTest.TheSettingsAreWhereMachineSettingsKeepsThem;
begin
    AssertEquals(ExtractFileDir(MachineSettingsFileFor(False, TableNotation)),
        Here(WhatSettings));
end;

procedure TGuideFileLocationsTest.ASourceBuildSharesTheSettingsWithAnInstalledCopy;
begin
    //  THE ONE ROW whose source-build cell is not a folder of the tree: the
    //  settings are this machine's, and a checkout travels.
    AssertEquals(MachineSettingsFileFor(False, TableNotation),
        MachineSettingsFileFor(False, UnderProfile));
    AssertTrue(FromSource(WhatSettings),
        Pos(ProfileFolder, FromSource(WhatSettings)) = 0);
end;

procedure TGuideFileLocationsTest.TheCurveTypesAreInTheSettingsFolder;
begin
    AssertEquals(AppConfigDirIn(TableNotation), Here(WhatCurveTypes));
    //  The user's, whichever copy runs: no folder of the tree.
    AssertEquals(AppConfigDirIn(TableNotation), AppConfigDirIn(UnderProfile));
    AssertTrue(FromSource(WhatCurveTypes),
        Pos(ProfileFolder, FromSource(WhatCurveTypes)) = 0);
end;

procedure TGuideFileLocationsTest.TheLogsAreInTheLogFolder;
begin
    AssertEquals(AppLogDirIn(TableNotation), Here(WhatLogs));
    AssertEquals(Slashed(AppLogDirIn(UnderProfile)), FromSource(WhatLogs));
end;

procedure TGuideFileLocationsTest.TheDownloadsAreUnderTheDataFolder;
begin
    //  download_cache.DownloadsRoot: AppDataDir('downloads').
    AssertEquals(IncludeTrailingPathDelimiter(AppDataRootIn(TableNotation)) +
        'downloads', Here(WhatDownloads));
    AssertEquals(Slashed(IncludeTrailingPathDelimiter(AppDataRootIn(UnderProfile)) +
        'downloads'), FromSource(WhatDownloads));
end;

procedure TGuideFileLocationsTest.ThePythonEngineIsUnderTheDataFolder;
var
    E: TUserDirsEnvironment;
begin
    E := TableNotation;
    AssertEquals(SidecarPyHomeFrom('', E.LocalAppData, E.XdgDataHome, E.Home),
        Here(WhatPython));
end;

procedure TGuideFileLocationsTest.EverySectionOfTheFileIsDescribed;
const
    //  A typed array, not an inline [...]: FPC 3.2.2 sizes an inline array of
    //  string constants to its FIRST element, so 'recent' arrived as 'rec'
    //  behind 'app' - on CI's compiler only.
    Sections: array[0..3] of string = (AppSettingsSection, RecentSection,
        ModulePreferencesSection, LayoutSection);
var
    Section: string;
begin
    //  SELF-ENFORCING: a section added to settings.json without a row here
    //  fails by name.
    for Section in Sections do
        AssertEquals(Section, Row(RememberedSettingsTopic, Section)[0]);
end;

initialization
    //  The guide and the rules, both as plain values: no file is touched.
    RegisterTest('unit', TGuideFileLocationsTest);
end.
