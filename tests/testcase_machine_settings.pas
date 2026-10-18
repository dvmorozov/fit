// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The one file holding what this program remembers about this machine.)

THE DEFECT IT REPLACES: three files - config.xml, module-preferences.txt and
recent-projects.txt - in two folders that differed by platform, one of which
roamed with a Windows account and moved into a checkout under a build. Settings
that are about one machine belong to it alone, so they are now one file
(settings.json) in this machine's own folder, which no profile folder moves.

THE FILE IS BEHIND ReadText AND WriteText, which TMemoryMachineSettings
(mock_machine_settings) overrides, so every rule below is a unit test;
TMachineSettingsFileTest is the one suite that writes a real file.
}
unit testcase_machine_settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    app_data_root, machine_settings, mock_machine_settings;

type
    TMachineSettingsTest = class(TTestCase)
    private
        FSettings: TMemoryMachineSettings;
        function Started(AHasFile: boolean; const AStored: string = ''): TMemoryMachineSettings;
        { The stored text, parsed; the caller frees it. }
        function StoredJSON: TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFirstRunHoldsNothingAndWritesNothing;
        procedure AValueIsReadBack;
        procedure AValueSurvivesARestart;
        procedure AKeyIsTheSameKeyInAnyCase;
        procedure TheFileSaysWhichFormatItIs;
        procedure ASectionThisBuildDoesNotKnowSurvivesARewrite;
        procedure AWriteReplacesOnlyItsOwnSection;
        procedure AKeyWrittenElsewhereInTheSameSectionIsKept;
        procedure ASectionIsACopyTheCallerOwns;
        procedure ReplacingASectionReplacesAllOfIt;
        procedure AnUnreadableFileIsSetAsideNotOverwritten;
        procedure AFileThatIsNotAnObjectIsUnreadable;
        procedure AValueThatIsNotTextReadsAsTheDefault;

        //  Where the file is.
        procedure TheFileIsInThisMachinesOwnFolder;
        procedure AProfileFolderDoesNotMoveIt;
        procedure WithNoHomeThereIsNoFile;
        procedure TheWindowCheckingItselfKeepsItsSettingsInMemory;

        //  The one the application uses.
        procedure UntilAFileIsNamedNothingIsWritten;
    end;

    { The same on a real file, in the temporary directory. }
    TMachineSettingsFileTest = class(TTestCase)
    private
        FDir, FPath: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AValueSurvivesARestart;
        procedure TheFolderIsMadeWhenItIsNotThere;
        procedure NoTemporaryFileIsLeftBehind;
        procedure TwoWritersKeepEachOthersSections;
        procedure AnUnreadableFileIsKeptBesideTheNewOne;
        procedure AFileThatCannotBeWrittenIsNotAFault;
    end;

implementation

{ ---- the rules ------------------------------------------------------------ }

procedure TMachineSettingsTest.SetUp;
begin
    FSettings := nil;
end;

procedure TMachineSettingsTest.TearDown;
begin
    FreeAndNil(FSettings);
    UseMachineSettings('');
end;

function TMachineSettingsTest.Started(AHasFile: boolean;
    const AStored: string): TMemoryMachineSettings;
begin
    FreeAndNil(FSettings);
    FSettings := TMemoryMachineSettings.CreateWith(AHasFile, AStored);
    Result := FSettings;
end;

function TMachineSettingsTest.StoredJSON: TJSONObject;
var
    Data: TJSONData;
begin
    Data := GetJSON(FSettings.Stored);
    AssertTrue('the file is one JSON object', Data is TJSONObject);
    Result := TJSONObject(Data);
end;

procedure TMachineSettingsTest.AFirstRunHoldsNothingAndWritesNothing;
begin
    Started(False);
    AssertEquals('a value never set is its default', 'dflt',
        FSettings.Str('app', 'Weighting', 'dflt'));
    AssertEquals('and reading writes nothing', 0, FSettings.Writes);
end;

procedure TMachineSettingsTest.AValueIsReadBack;
begin
    Started(False);
    FSettings.SetStr('preferences', 'probe.column', 'high');
    AssertEquals('high', FSettings.Str('preferences', 'probe.column'));
    AssertEquals('written at once', 1, FSettings.Writes);
end;

procedure TMachineSettingsTest.AValueSurvivesARestart;
var
    Written: string;
begin
    Started(False);
    FSettings.SetStr('preferences', 'probe.column', 'high');
    Written := FSettings.Stored;
    Started(True, Written);
    AssertEquals('high', FSettings.Str('preferences', 'probe.column'));
end;

procedure TMachineSettingsTest.AKeyIsTheSameKeyInAnyCase;
var
    J: TJSONObject;
begin
    //  As the registries compare ids, and as module_preferences always has: a
    //  module that spells its key differently in two places still has ONE
    //  choice, and the file one entry for it.
    Started(False);
    FSettings.SetStr('preferences', 'Probe.Column', 'high');
    FSettings.SetStr('preferences', 'probe.column', 'low');
    AssertEquals('low', FSettings.Str('preferences', 'PROBE.COLUMN'));
    J := StoredJSON;
    try
        AssertEquals('one entry, not two', 1, J.Objects['preferences'].Count);
    finally
        J.Free;
    end;
end;

procedure TMachineSettingsTest.TheFileSaysWhichFormatItIs;
var
    J: TJSONObject;
begin
    //  So a later build that changes what a section means can tell an old file
    //  from a new one, rather than guessing from what happens to be in it.
    Started(False);
    FSettings.SetStr('app', 'Weighting', 'none');
    J := StoredJSON;
    try
        AssertEquals(MachineSettingsFormat, J.Integers['format']);
    finally
        J.Free;
    end;
end;

procedure TMachineSettingsTest.ASectionThisBuildDoesNotKnowSurvivesARewrite;
var
    J: TJSONObject;
begin
    //  THE EXTENSIBILITY. A newer build, or a module this build does not
    //  contain, keeps something here; an older build writing its own section
    //  must not erase it.
    Started(True, '{"format": 1, "future": {"x": [1, 2, 3]}, ' +
        '"app": {"Weighting": "poisson", "FromANewerBuild": "kept"}}');
    FSettings.SetStr('app', 'Weighting', 'none');
    J := StoredJSON;
    try
        AssertEquals('the unknown section', 3, J.Objects['future'].Arrays['x'].Count);
        AssertEquals('and an unknown key in a known one', 'kept',
            J.Objects['app'].Strings['FromANewerBuild']);
        AssertEquals('none', J.Objects['app'].Strings['Weighting']);
    finally
        J.Free;
    end;
end;

procedure TMachineSettingsTest.AWriteReplacesOnlyItsOwnSection;
var
    J: TJSONObject;
begin
    //  TWO WINDOWS OPEN AT ONCE. Each read the file when it started; the other
    //  one has written its own section since. A write of this one's section
    //  must not put back the other's as it was when this one started - which
    //  three separate files used to guarantee for free.
    Started(True, '{"format": 1, "recent": {"last": "/old.fitproj"}}');
    FSettings.Stored := '{"format": 1, "recent": {"last": "/new.fitproj"}}';
    FSettings.SetStr('app', 'Weighting', 'none');
    J := StoredJSON;
    try
        AssertEquals('the other window''s section, as it left it',
            '/new.fitproj', J.Objects['recent'].Strings['last']);
        AssertEquals('and this one''s', 'none', J.Objects['app'].Strings['Weighting']);
    finally
        J.Free;
    end;
    AssertEquals('and this one now sees it too', '/new.fitproj',
        FSettings.Str('recent', 'last'));
end;

procedure TMachineSettingsTest.AKeyWrittenElsewhereInTheSameSectionIsKept;
begin
    //  The same two windows, now both setting a preference: one key each, in
    //  the one section. Setting a key changes that key, not the section as
    //  this window last saw it.
    Started(True, '{"preferences": {"a": "1"}}');
    FSettings.Stored := '{"preferences": {"a": "1", "b": "2"}}';
    FSettings.SetStr('preferences', 'c', '3');
    AssertEquals('the other window''s key', '2', FSettings.Str('preferences', 'b'));
    AssertEquals('and this one''s', '3', FSettings.Str('preferences', 'c'));
end;

procedure TMachineSettingsTest.ASectionIsACopyTheCallerOwns;
var
    S: TJSONObject;
begin
    Started(True, '{"recent": {"last": "/a.fitproj"}}');
    S := FSettings.Section('recent');
    try
        S.Strings['last'] := '/changed.fitproj';
    finally
        S.Free;
    end;
    AssertEquals('changing the copy changes nothing', '/a.fitproj',
        FSettings.Str('recent', 'last'));
    S := FSettings.Section('absent');
    try
        AssertEquals('a section never written is empty', 0, S.Count);
    finally
        S.Free;
    end;
end;

procedure TMachineSettingsTest.ReplacingASectionReplacesAllOfIt;
var
    S: TJSONObject;
begin
    Started(True, '{"recent": {"last": "/a.fitproj", "gone": "x"}}');
    S := TJSONObject.Create(['last', '/b.fitproj']);
    FSettings.ReplaceSection('recent', S);
    AssertEquals('/b.fitproj', FSettings.Str('recent', 'last'));
    AssertEquals('what the owner left out is gone', '',
        FSettings.Str('recent', 'gone'));
    AssertEquals(1, FSettings.Writes);
end;

procedure TMachineSettingsTest.AnUnreadableFileIsSetAsideNotOverwritten;
begin
    //  A file a crash left half-written, or one edited by hand into something
    //  that is not JSON. The session runs on its defaults; the file is moved
    //  aside before anything is written, so nothing the user had is destroyed
    //  without a copy.
    Started(True, '{"app": {"Weighting": ');
    AssertEquals('defaults', 'poisson', FSettings.Str('app', 'Weighting', 'poisson'));
    AssertEquals('set aside once', 1, FSettings.SetAside);
    FSettings.SetStr('app', 'Weighting', 'none');
    AssertEquals('and a new file is written', 'none',
        FSettings.Str('app', 'Weighting'));
end;

procedure TMachineSettingsTest.AFileThatIsNotAnObjectIsUnreadable;
begin
    Started(True, '[1, 2, 3]');
    AssertEquals('', FSettings.Str('app', 'Weighting'));
    AssertEquals(1, FSettings.SetAside);
end;

procedure TMachineSettingsTest.AValueThatIsNotTextReadsAsTheDefault;
begin
    //  A hand edit that wrote a number, or a section that is not an object.
    Started(True, '{"app": {"Weighting": 3}, "recent": "flat"}');
    AssertEquals('dflt', FSettings.Str('app', 'Weighting', 'dflt'));
    AssertEquals('dflt', FSettings.Str('recent', 'last', 'dflt'));
end;

{ ---- where the file is ---------------------------------------------------- }

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

procedure TMachineSettingsTest.TheFileIsInThisMachinesOwnFolder;
begin
    //  Any run but the window checking itself - a recording among them: it is
    //  OF the user's settings and the project last open, so it must read them.
{$IFDEF WINDOWS}
    //  LOCAL, not the roaming profile config.xml was in: what is true of one
    //  machine must not follow the account to every other.
    AssertEquals('C:\Users\u\AppData\Local\Fit\settings.json',
        MachineSettingsFileFor(False, PlainUser));
{$ELSE}
{$IFDEF DARWIN}
    AssertEquals('/home/u/Library/Application Support/Fit/settings.json',
        MachineSettingsFileFor(False, PlainUser));
{$ELSE}
    AssertEquals('/home/u/.local/state/fit/settings.json',
        MachineSettingsFileFor(False, PlainUser));
{$ENDIF}
{$ENDIF}
end;

procedure TMachineSettingsTest.AProfileFolderDoesNotMoveIt;
var
    E: TUserDirsEnvironment;
begin
    //  A PROFILE FOLDER IS IN A CHECKOUT, and a checkout is copied, synced and
    //  shared between machines: a Mac offered a Linux machine's /mnt/data paths
    //  in File > Open Recent. What is about this machine stays on it.
    E := PlainUser;
    E.Profile := '/src/fit/var/profile';
    AssertEquals(MachineSettingsFileFor(False, PlainUser), MachineSettingsFileFor(False, E));
end;

procedure TMachineSettingsTest.WithNoHomeThereIsNoFile;
begin
    //  '' and not a relative path, which would resolve against wherever the
    //  program happened to be started.
    AssertEquals('', MachineSettingsFileFor(False, Default(TUserDirsEnvironment)));
end;

procedure TMachineSettingsTest.TheWindowCheckingItselfKeepsItsSettingsInMemory;
begin
    //  THE WINDOW CHECKING ITSELF (/CHECK_UI) changes the model, the zoom and
    //  the curve type, and writes its settings when it closes. Under a build it
    //  used to write them into the checkout; with the file now shared with the
    //  installed copy, it must write nothing at all.
    AssertEquals('', MachineSettingsFileFor(True, PlainUser));
end;

procedure TMachineSettingsTest.UntilAFileIsNamedNothingIsWritten;
begin
    //  A test binary that never names a file cannot write the user's own.
    UseMachineSettings('');
    AssertEquals('', MachineSettings.FileName);
    MachineSettings.SetStr('preferences', 'probe.column', 'high');
    AssertEquals('kept for the session', 'high',
        MachineSettings.Str('preferences', 'probe.column'));
    UseMachineSettings('');
    AssertEquals('and forgotten with it', '',
        MachineSettings.Str('preferences', 'probe.column'));
end;

{ ---- on a real file ------------------------------------------------------- }

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

function FileText(const APath: string): string;
var
    L: TStringList;
begin
    L := TStringList.Create;
    try
        L.LoadFromFile(APath);
        Result := L.Text;
    finally
        L.Free;
    end;
end;

procedure TMachineSettingsFileTest.SetUp;
begin
    FDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-machine-settings-test';
    DeleteTree(FDir);
    FPath := IncludeTrailingPathDelimiter(FDir) + 'state' + PathDelim +
        MachineSettingsFileName;
end;

procedure TMachineSettingsFileTest.TearDown;
begin
    UseMachineSettings('');
    DeleteTree(FDir);
end;

procedure TMachineSettingsFileTest.AValueSurvivesARestart;
var
    S: TMachineSettings;
begin
    S := TMachineSettings.Create(FPath);
    try
        S.SetStr('preferences', 'probe.column', 'high');
    finally
        S.Free;
    end;
    S := TMachineSettings.Create(FPath);
    try
        AssertEquals('high', S.Str('preferences', 'probe.column'));
    finally
        S.Free;
    end;
end;

procedure TMachineSettingsFileTest.TheFolderIsMadeWhenItIsNotThere;
begin
    //  A first run on a machine: the platform's folder is not there yet.
    UseMachineSettings(FPath);
    MachineSettings.SetStr('preferences', 'probe.column', 'high');
    AssertTrue(FileExists(FPath));
end;

procedure TMachineSettingsFileTest.NoTemporaryFileIsLeftBehind;
var
    S: TSearchRec;
    Names: string;
begin
    //  Written beside itself and renamed over, so a crash part-way leaves the
    //  old file whole rather than half of a new one - and nothing else behind.
    UseMachineSettings(FPath);
    MachineSettings.SetStr('preferences', 'a', '1');
    MachineSettings.SetStr('preferences', 'b', '2');
    Names := '';
    if FindFirst(IncludeTrailingPathDelimiter(ExtractFileDir(FPath)) + '*',
        faAnyFile, S) = 0 then
    try
        repeat
            if (S.Name <> '.') and (S.Name <> '..') then
                Names := Names + S.Name + ' ';
        until FindNext(S) <> 0;
    finally
        FindClose(S);
    end;
    AssertEquals(MachineSettingsFileName + ' ', Names);
end;

procedure TMachineSettingsFileTest.TwoWritersKeepEachOthersSections;
var
    A, B: TMachineSettings;
begin
    //  Two windows open at once, on the one file.
    A := TMachineSettings.Create(FPath);
    B := TMachineSettings.Create(FPath);
    try
        A.SetStr('recent', 'last', '/a.fitproj');
        B.SetStr('app', 'Weighting', 'none');
    finally
        B.Free;
        A.Free;
    end;
    A := TMachineSettings.Create(FPath);
    try
        AssertEquals('/a.fitproj', A.Str('recent', 'last'));
        AssertEquals('none', A.Str('app', 'Weighting'));
    finally
        A.Free;
    end;
end;

procedure TMachineSettingsFileTest.AnUnreadableFileIsKeptBesideTheNewOne;
var
    L: TStringList;
    S: TMachineSettings;
begin
    ForceDirectories(ExtractFileDir(FPath));
    L := TStringList.Create;
    try
        L.Text := 'not json';
        L.SaveToFile(FPath);
    finally
        L.Free;
    end;
    S := TMachineSettings.Create(FPath);
    try
        S.SetStr('app', 'Weighting', 'none');
    finally
        S.Free;
    end;
    AssertTrue('the old one is kept', FileExists(FPath + '.unreadable'));
    AssertEquals('as it was', 'not json', Trim(FileText(FPath + '.unreadable')));
    S := TMachineSettings.Create(FPath);
    try
        AssertEquals('and the new one is read', 'none', S.Str('app', 'Weighting'));
    finally
        S.Free;
    end;
end;

procedure TMachineSettingsFileTest.AFileThatCannotBeWrittenIsNotAFault;
var
    L: TStringList;
    S: TMachineSettings;
    Blocker: string;
begin
    //  A folder that cannot be made - here a FILE stands where it would go -
    //  costs the choice between sessions, never the session.
    ForceDirectories(FDir);
    Blocker := IncludeTrailingPathDelimiter(FDir) + 'blocked';
    L := TStringList.Create;
    try
        L.Text := 'x';
        L.SaveToFile(Blocker);
    finally
        L.Free;
    end;
    S := TMachineSettings.Create(IncludeTrailingPathDelimiter(Blocker) +
        MachineSettingsFileName);
    try
        S.SetStr('preferences', 'probe.column', 'high');
        AssertEquals('still kept for the session', 'high',
            S.Str('preferences', 'probe.column'));
    finally
        S.Free;
    end;
end;

initialization
    RegisterTest('unit', TMachineSettingsTest);
    //  Writes and reads real files.
    RegisterTest('integration', TMachineSettingsFileTest);
end.
