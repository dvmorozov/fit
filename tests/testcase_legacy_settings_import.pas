// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The first run of a version that keeps one settings file: what the
three it replaces held is carried into it, and they are removed.)

WHAT MUST NOT HAPPEN: a user upgrading and finding the server address, the
minimizer, the recent list and every module's choice gone - or finding them
gone a second time because the old files were imported again over newer
settings. A copy run from a checkout kept the same machine's settings in its
var/profile folder, and they are carried over too.

TOpenMachineSettingsTest enters where Fit.lpr does: OpenMachineSettings, over a
whole user folder laid out in the temporary directory.
}
unit testcase_legacy_settings_import;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, Laz_XMLCfg,
    app_data_root, app_settings, machine_settings, module_preferences,
    recent_project, recent_project_store, legacy_settings_import;

type
    { Where the old files are looked for: no file is touched. }
    TLegacySettingsFilesTest = class(TTestCase)
    published
        procedure TheOldFilesAreWhereTheirVersionsKeptThem;
        procedure AProfileFolderIsLookedInAsWell;
        procedure WithNoProfileFolderThereIsNoSourceBuildFile;
        procedure WithNoHomeThereAreNoOldFiles;
    end;

    { A user's whole folder, in the temporary directory. }
    TOpenMachineSettingsTest = class(TTestCase)
    private
        FRoot: string;
        FEnv: TUserDirsEnvironment;
        FOld: TLegacySettingsFiles;
        procedure WriteText(const APath, AText: string);
        { An old config.xml whose server is AServerUrl. }
        procedure WriteOldConfig(const AServerUrl: string);
        procedure WriteAllThreeOldFiles;
        function NewFile: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure EverythingTheOldFilesHeldIsInTheNewOne;
        procedure TheOldFilesAreRemovedOnceTheyAreCarriedOver;
        procedure AnExistingSettingsFileIsNotOverwrittenByOldOnes;
        procedure AnUnreadableOldFileIsLeftWhereItWas;
        procedure ASourceBuildsSettingsAreCarriedOverAndWin;
        procedure ASourceBuildsCurveTypesJoinTheInstalledOnes;
        procedure TheWindowCheckingItselfNeitherImportsNorWrites;
        procedure WithNoOldFilesNothingIsWritten;
    end;

implementation

function PlainUser: TUserDirsEnvironment;
begin
    Result := Default(TUserDirsEnvironment);
{$IFDEF WINDOWS}
    Result.Home := 'C:\Users\u';
    Result.AppData := 'C:\Users\u\AppData\Roaming';
    Result.LocalAppData := 'C:\Users\u\AppData\Local';
{$ELSE}
    Result.Home := '/home/u';
{$ENDIF}
end;

procedure TLegacySettingsFilesTest.TheOldFilesAreWhereTheirVersionsKeptThem;
var
    F: TLegacySettingsFiles;
begin
    F := LegacySettingsFilesIn(PlainUser);
{$IFDEF WINDOWS}
    AssertEquals('C:\Users\u\AppData\Roaming\Fit\config.xml', F.Config);
    AssertEquals('C:\Users\u\AppData\Roaming\Fit\module-preferences.txt', F.Preferences);
    AssertEquals('C:\Users\u\AppData\Local\Fit\recent-projects.txt', F.Recent);
{$ELSE}
{$IFDEF DARWIN}
    AssertEquals('/home/u/Library/Application Support/Fit/config.xml', F.Config);
    AssertEquals('/home/u/Library/Application Support/Fit/module-preferences.txt',
        F.Preferences);
    AssertEquals('/home/u/Library/Application Support/Fit/recent-projects.txt', F.Recent);
{$ELSE}
    AssertEquals('/home/u/.config/fit/config.xml', F.Config);
    AssertEquals('/home/u/.config/fit/module-preferences.txt', F.Preferences);
    AssertEquals('/home/u/.local/state/fit/recent-projects.txt', F.Recent);
{$ENDIF}
{$ENDIF}
end;

procedure TLegacySettingsFilesTest.AProfileFolderIsLookedInAsWell;
var
    E: TUserDirsEnvironment;
    F: TLegacySettingsFiles;
begin
    //  THE SAME MACHINE'S SETTINGS, kept in the checkout only because every
    //  folder used to give way to the profile folder - and the installed
    //  copy's are still where they were.
    E := PlainUser;
    E.Profile := '/src/fit/var/profile';
    F := LegacySettingsFilesIn(E);
    AssertEquals(IncludeTrailingPathDelimiter('/src/fit/var/profile') + 'config.xml',
        F.ProfileConfig);
    AssertEquals(IncludeTrailingPathDelimiter('/src/fit/var/profile') +
        'module-preferences.txt', F.ProfilePreferences);
    AssertEquals(LegacySettingsFilesIn(PlainUser).Config, F.Config);
    AssertEquals(LegacySettingsFilesIn(PlainUser).Preferences, F.Preferences);
end;

procedure TLegacySettingsFilesTest.WithNoProfileFolderThereIsNoSourceBuildFile;
begin
    AssertEquals('', LegacySettingsFilesIn(PlainUser).ProfileConfig);
    AssertEquals('', LegacySettingsFilesIn(PlainUser).ProfilePreferences);
end;

procedure TLegacySettingsFilesTest.WithNoHomeThereAreNoOldFiles;
var
    F: TLegacySettingsFiles;
begin
    F := LegacySettingsFilesIn(Default(TUserDirsEnvironment));
    AssertEquals('', F.Config);
    AssertEquals('', F.Preferences);
    AssertEquals('', F.Recent);
end;

{ ---- the whole folder ------------------------------------------------------ }

procedure DeleteTree(const ADir: string);
var
    S: TSearchRec;
begin
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, S) = 0 then
    try
        repeat
            if (S.Name = '.') or (S.Name = '..') then
                Continue;
            if (S.Attr and faDirectory) <> 0 then
                DeleteTree(IncludeTrailingPathDelimiter(ADir) + S.Name)
            else
                DeleteFile(IncludeTrailingPathDelimiter(ADir) + S.Name);
        until FindNext(S) <> 0;
    finally
        FindClose(S);
    end;
    RemoveDir(ADir);
end;

procedure TOpenMachineSettingsTest.SetUp;
begin
    FRoot := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-legacy-import-test';
    DeleteTree(FRoot);
    FEnv := Default(TUserDirsEnvironment);
    FEnv.Home := FRoot;
{$IFDEF WINDOWS}
    FEnv.AppData := IncludeTrailingPathDelimiter(FRoot) + 'Roaming';
    FEnv.LocalAppData := IncludeTrailingPathDelimiter(FRoot) + 'Local';
{$ENDIF}
    FOld := LegacySettingsFilesIn(FEnv);
end;

procedure TOpenMachineSettingsTest.TearDown;
begin
    UseMachineSettings('');
    DeleteTree(FRoot);
end;

procedure TOpenMachineSettingsTest.WriteText(const APath, AText: string);
var
    L: TStringList;
begin
    ForceDirectories(ExtractFileDir(APath));
    L := TStringList.Create;
    try
        L.Text := AText;
        L.SaveToFile(APath);
    finally
        L.Free;
    end;
end;

procedure TOpenMachineSettingsTest.WriteOldConfig(const AServerUrl: string);
var
    Cfg: TXMLConfig;
    S: Settings_v1;
begin
    //  Exactly as the window wrote it.
    ForceDirectories(ExtractFileDir(FOld.Config));
    S := Settings_v1.Create(nil);
    Cfg := TXMLConfig.Create(FOld.Config);
    try
        S.ServerUrl := AServerUrl;
        S.MinimizerKind := 2;
        WriteComponentToXMLConfig(Cfg, 'Component', S);
        Cfg.Flush;
    finally
        Cfg.Free;
        S.Free;
    end;
end;

procedure TOpenMachineSettingsTest.WriteAllThreeOldFiles;
begin
    WriteOldConfig('http://compute.example:8080');
    WriteText(FOld.Preferences, 'probe.column=high' + LineEnding +
        'fit.update-auto=0');
    WriteText(FOld.Recent, 'LastProjectFile=/p/b.fitproj' + LineEnding +
        'RecentProjects=/p/b.fitproj' + RecentSeparator + '/p/a.fitproj');
end;

function TOpenMachineSettingsTest.NewFile: string;
begin
    Result := MachineSettingsFileFor(False, FEnv);
end;

procedure TOpenMachineSettingsTest.EverythingTheOldFilesHeldIsInTheNewOne;
var
    S: Settings_v1;
    Store: TRecentProjectStore;
begin
    WriteAllThreeOldFiles;
    OpenMachineSettings(False, FEnv);
    AssertEquals('the file the application now uses', NewFile,
        MachineSettings.FileName);
    AssertTrue('and it is written', FileExists(NewFile));
    //  Read back as the window, the modules and the recent list read it.
    S := Settings_v1.Create(nil);
    try
        ReadAppSettings(MachineSettings, S);
        AssertEquals('the server', 'http://compute.example:8080', S.ServerUrl);
        AssertEquals('the minimizer', 2, S.MinimizerKind);
    finally
        S.Free;
    end;
    AssertEquals('a module''s choice', 'high', ModulePreference('probe.column'));
    AssertEquals('the framework''s own', '0', ModulePreference('fit.update-auto'));
    Store := TRecentProjectStore.Create(MachineSettings);
    try
        AssertEquals('the project to reopen', '/p/b.fitproj', Store.LastProject);
        AssertEquals('and the list', '/p/b.fitproj' + RecentSeparator + '/p/a.fitproj',
            Store.Recent);
    finally
        Store.Free;
    end;
end;

procedure TOpenMachineSettingsTest.TheOldFilesAreRemovedOnceTheyAreCarriedOver;
begin
    //  ONCE: an old file left behind would be imported again over whatever
    //  the user has chosen since - or, kept and ignored, would be a second
    //  answer to "where are my settings".
    WriteAllThreeOldFiles;
    OpenMachineSettings(False, FEnv);
    AssertFalse('config.xml', FileExists(FOld.Config));
    AssertFalse('module-preferences.txt', FileExists(FOld.Preferences));
    AssertFalse('recent-projects.txt', FileExists(FOld.Recent));
end;

procedure TOpenMachineSettingsTest.AnExistingSettingsFileIsNotOverwrittenByOldOnes;
var
    S: Settings_v1;
begin
    //  The new file is the newer: an old one beside it was left by an older
    //  build run since, and is not this machine's current answer.
    UseMachineSettings(NewFile);
    MachineSettings.SetStr(AppSettingsSection, 'ServerUrl', 'http://current');
    UseMachineSettings('');
    WriteOldConfig('http://stale');
    OpenMachineSettings(False, FEnv);
    S := Settings_v1.Create(nil);
    try
        ReadAppSettings(MachineSettings, S);
        AssertEquals('http://current', S.ServerUrl);
    finally
        S.Free;
    end;
    AssertTrue('and the old file is not this build''s to remove',
        FileExists(FOld.Config));
end;

procedure TOpenMachineSettingsTest.AnUnreadableOldFileIsLeftWhereItWas;
begin
    //  Nothing is deleted that was not carried over.
    WriteAllThreeOldFiles;
    WriteText(FOld.Config, 'this is not a settings file');
    OpenMachineSettings(False, FEnv);
    AssertTrue('the one that could not be read stays', FileExists(FOld.Config));
    AssertFalse('the others went', FileExists(FOld.Preferences));
    AssertEquals('high', ModulePreference('probe.column'));
end;

procedure TOpenMachineSettingsTest.ASourceBuildsSettingsAreCarriedOverAndWin;
var
    E: TUserDirsEnvironment;
    Profile: string;
    S: Settings_v1;
    Cfg: TXMLConfig;
begin
    //  The installed copy's settings AND the checkout's: the checkout's are
    //  the ones the person starting this build has been using, so where both
    //  say something, they win.
    WriteAllThreeOldFiles;
    Profile := IncludeTrailingPathDelimiter(FRoot) + 'checkout' + PathDelim + 'profile';
    E := FEnv;
    E.Profile := Profile;
    ForceDirectories(Profile);
    S := Settings_v1.Create(nil);
    Cfg := TXMLConfig.Create(LegacySettingsFilesIn(E).ProfileConfig);
    try
        S.ServerUrl := 'http://from-the-checkout';
        WriteComponentToXMLConfig(Cfg, 'Component', S);
        Cfg.Flush;
    finally
        Cfg.Free;
        S.Free;
    end;
    WriteText(LegacySettingsFilesIn(E).ProfilePreferences, 'probe.column=low' +
        LineEnding + 'probe.only-here=1');
    OpenMachineSettings(False, E);
    S := Settings_v1.Create(nil);
    try
        ReadAppSettings(MachineSettings, S);
        AssertEquals('the checkout''s server', 'http://from-the-checkout', S.ServerUrl);
    finally
        S.Free;
    end;
    AssertEquals('the checkout''s choice', 'low', ModulePreference('probe.column'));
    AssertEquals('one only the checkout had', '1', ModulePreference('probe.only-here'));
    AssertEquals('one only the installed copy had', '0',
        ModulePreference('fit.update-auto'));
    AssertFalse('the checkout''s config.xml is gone',
        FileExists(LegacySettingsFilesIn(E).ProfileConfig));
    AssertFalse('and its preferences',
        FileExists(LegacySettingsFilesIn(E).ProfilePreferences));
    AssertFalse('and the installed copy''s', FileExists(FOld.Config));
end;

procedure TOpenMachineSettingsTest.ASourceBuildsCurveTypesJoinTheInstalledOnes;
var
    E: TUserDirsEnvironment;
    Profile, Library_: string;
begin
    //  ONE LIBRARY OF CURVE TYPES for every copy on the machine: those a copy
    //  started from a checkout kept in var/profile move to where an installed
    //  copy keeps its own - beside them, as each file is named by the moment
    //  it was made, and never over one already there.
    Profile := IncludeTrailingPathDelimiter(FRoot) + 'checkout' + PathDelim + 'profile';
    E := FEnv;
    E.Profile := Profile;
    Library_ := AppConfigDirIn(FEnv);
    WriteText(IncludeTrailingPathDelimiter(Profile) + '100.cpr', 'from the checkout');
    WriteText(IncludeTrailingPathDelimiter(Profile) + '200.cpr', 'checkout copy');
    WriteText(IncludeTrailingPathDelimiter(Library_) + '200.cpr', 'installed copy');
    OpenMachineSettings(False, E);
    AssertTrue('moved', FileExists(IncludeTrailingPathDelimiter(Library_) + '100.cpr'));
    AssertFalse('and not left behind',
        FileExists(IncludeTrailingPathDelimiter(Profile) + '100.cpr'));
    AssertTrue('one already there is kept, and so is the one that met it',
        FileExists(IncludeTrailingPathDelimiter(Profile) + '200.cpr'));
end;

procedure TOpenMachineSettingsTest.TheWindowCheckingItselfNeitherImportsNorWrites;
begin
    WriteAllThreeOldFiles;
    OpenMachineSettings(True, FEnv);
    AssertEquals('memory only', '', MachineSettings.FileName);
    AssertTrue('the old files are untouched', FileExists(FOld.Config));
    AssertFalse('and nothing new is written', FileExists(NewFile));
end;

procedure TOpenMachineSettingsTest.WithNoOldFilesNothingIsWritten;
begin
    //  A first run on a new machine writes the file when there is something in
    //  it, not at start-up.
    OpenMachineSettings(False, FEnv);
    AssertEquals(NewFile, MachineSettings.FileName);
    AssertFalse(FileExists(NewFile));
end;

initialization
    RegisterTest('unit', TLegacySettingsFilesTest);
    //  Real files in a temporary user folder.
    RegisterTest('integration', TOpenMachineSettingsTest);
end.
