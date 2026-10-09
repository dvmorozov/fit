// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where the projects opened on this machine are remembered.)

THE DEFECT: the project last open and the list behind File > Open Recent were
kept in config.xml, with the settings. A build keeps its settings in the
checkout, and a checkout travels: one copied from a Linux machine to a Mac
offered the Linux machine's /mnt/data paths, none of which exist on the Mac.
On Windows the settings roam with the account and carried the list to every
machine it signs in to. Paths are about one machine, so they are kept in that
machine's own settings - first in a file of their own, recent-projects.txt, and
now in the 'recent' section of settings.json (machine_settings), which no
profile folder moves.

THE FILE IS BEHIND TMachineSettings, which mock_machine_settings keeps in
memory, so every rule here is a unit test; TRecentProjectFileTest is the one
suite that writes a real file.
}
unit testcase_recent_project_store;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
    machine_settings, mock_machine_settings, recent_project, recent_project_store;

type
    TRecentProjectStoreTest = class(TTestCase)
    private
        FSettings: TMemoryMachineSettings;
        FStore: TRecentProjectStore;
        { A store started on a machine whose settings file holds AStored, or
          that has no file at all when AHasFile is False. Owned by the fixture. }
        function Started(AHasFile: boolean; const AStored: string = ''): TRecentProjectStore;
        { The 'recent' section as the file holds it; the caller frees it. }
        function StoredSection: TJSONObject;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFirstRunRemembersNothing;
        procedure AProjectShownIsTheLastAndHeadsTheList;
        procedure ItIsWrittenAtOnceNotAtShutdown;
        procedure TheNextSessionReadsWhatThisOneWrote;
        procedure ANewProjectKeepsTheLastOneRemembered;
        procedure ForgettingTheLastTakesItOutOfTheListToo;
        procedure APathWithALineBreakIsNotRemembered;
        procedure ASectionWithoutTheEntriesRemembersNothing;
        procedure AnEntryThatIsNotAPathIsSkipped;
        procedure TheKeysAreReadInAnyCase;

        //  What the file holds.
        procedure TheListIsKeptInTheMachinesSettings;
        procedure TheListIsAnArrayOfPaths;
    end;

    { The same store on a real file, in the temporary directory. }
    TRecentProjectFileTest = class(TTestCase)
    private
        FPath: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheListSurvivesARestart;
    end;

implementation

procedure TRecentProjectStoreTest.SetUp;
begin
    FSettings := nil;
    FStore := nil;
end;

procedure TRecentProjectStoreTest.TearDown;
begin
    FreeAndNil(FStore);
    FreeAndNil(FSettings);
end;

function TRecentProjectStoreTest.Started(AHasFile: boolean;
    const AStored: string): TRecentProjectStore;
begin
    FreeAndNil(FStore);
    FreeAndNil(FSettings);
    FSettings := TMemoryMachineSettings.CreateWith(AHasFile, AStored);
    FStore := TRecentProjectStore.Create(FSettings);
    Result := FStore;
end;

function TRecentProjectStoreTest.StoredSection: TJSONObject;
var
    Data: TJSONData;
begin
    Data := GetJSON(FSettings.Stored);
    try
        AssertTrue('the file is one JSON object', Data is TJSONObject);
        Result := TJSONObject(TJSONObject(Data).Objects[RecentSection].Clone);
    finally
        Data.Free;
    end;
end;

procedure TRecentProjectStoreTest.AFirstRunRemembersNothing;
begin
    Started(False);
    AssertEquals('no project to reopen', '', FStore.LastProject);
    AssertEquals('and nothing to offer', '', FStore.Recent);
    AssertEquals('and nothing written for it', 0, FSettings.Writes);
end;

procedure TRecentProjectStoreTest.AProjectShownIsTheLastAndHeadsTheList;
begin
    Started(False);
    FStore.Showing('/here/a.fitproj');
    FStore.Showing('/here/b.fitproj');
    AssertEquals('/here/b.fitproj', FStore.LastProject);
    AssertEquals('/here/b.fitproj' + RecentSeparator + '/here/a.fitproj',
        FStore.Recent);
end;

procedure TRecentProjectStoreTest.ItIsWrittenAtOnceNotAtShutdown;
begin
    //  The window's settings are written when it closes, and a session that
    //  ends any other way - a crash, a killed process - loses them. A project
    //  opened is worth remembering the moment it is opened.
    Started(False);
    FStore.Showing('/here/a.fitproj');
    AssertEquals(1, FSettings.Writes);
end;

procedure TRecentProjectStoreTest.TheNextSessionReadsWhatThisOneWrote;
var
    Written: string;
begin
    Started(False);
    FStore.Showing('/here/a.fitproj');
    FStore.Showing('/here/b.fitproj');
    Written := FSettings.Stored;
    Started(True, Written);
    AssertEquals('/here/b.fitproj', FStore.LastProject);
    AssertEquals('/here/b.fitproj' + RecentSeparator + '/here/a.fitproj',
        FStore.Recent);
end;

procedure TRecentProjectStoreTest.ANewProjectKeepsTheLastOneRemembered;
begin
    //  recent_project's rule, reached through the store: a document with no
    //  path is no project to reopen.
    Started(False);
    FStore.Showing('/here/a.fitproj');
    FStore.Showing('');
    AssertEquals('/here/a.fitproj', FStore.LastProject);
    AssertEquals('/here/a.fitproj', FStore.Recent);
end;

procedure TRecentProjectStoreTest.ForgettingTheLastTakesItOutOfTheListToo;
begin
    Started(False);
    FStore.Showing('/here/a.fitproj');
    FStore.Showing('/here/b.fitproj');
    FStore.ForgetLast;
    AssertEquals('nothing to reopen', '', FStore.LastProject);
    AssertEquals('and the gone one is no line', '/here/a.fitproj', FStore.Recent);
    AssertEquals('and that is written too', 3, FSettings.Writes);
end;

procedure TRecentProjectStoreTest.APathWithALineBreakIsNotRemembered;
begin
    //  No file system a project is opened from names files so, and a menu
    //  line cannot show one.
    Started(False);
    FStore.Showing('/here/a.fitproj');
    FStore.Showing('/here/b' + LineEnding + 'x.fitproj');
    AssertEquals('/here/a.fitproj', FStore.LastProject);
    AssertEquals('/here/a.fitproj', FStore.Recent);
end;

procedure TRecentProjectStoreTest.ASectionWithoutTheEntriesRemembersNothing;
begin
    //  A file edited by hand, or written by a build that kept something else
    //  there: what it does not say is not remembered.
    Started(True, '{"recent": {"something": "else"}}');
    AssertEquals('', FStore.LastProject);
    AssertEquals('', FStore.Recent);
    Started(True, '{"recent": "not a section"}');
    AssertEquals('', FStore.LastProject);
    AssertEquals('', FStore.Recent);
end;

procedure TRecentProjectStoreTest.AnEntryThatIsNotAPathIsSkipped;
begin
    Started(True, '{"recent": {"last": 3, "projects": ["/a.fitproj", 7, null, ' +
        '"/b.fitproj"]}}');
    AssertEquals('', FStore.LastProject);
    AssertEquals('/a.fitproj' + RecentSeparator + '/b.fitproj', FStore.Recent);
end;

procedure TRecentProjectStoreTest.TheKeysAreReadInAnyCase;
begin
    //  As every key in the file is (machine_settings): a hand edit that wrote
    //  "Projects" still names the list.
    Started(True, '{"recent": {"Last": "/a.fitproj", "Projects": ["/a.fitproj", ' +
        '"/b.fitproj"]}}');
    AssertEquals('/a.fitproj', FStore.LastProject);
    AssertEquals('/a.fitproj' + RecentSeparator + '/b.fitproj', FStore.Recent);
end;

procedure TRecentProjectStoreTest.TheListIsKeptInTheMachinesSettings;
var
    S: TJSONObject;
begin
    //  ONE FILE FOR WHAT THIS MACHINE REMEMBERS: the list is a section of it,
    //  beside the window's settings and the modules' choices.
    Started(True, '{"preferences": {"probe.column": "high"}}');
    FStore.Showing('/here/a.fitproj');
    S := StoredSection;
    try
        AssertEquals('/here/a.fitproj', S.Strings['last']);
    finally
        S.Free;
    end;
    AssertEquals('and the rest of the file is as it was', 'high',
        FSettings.Str('preferences', 'probe.column'));
end;

procedure TRecentProjectStoreTest.TheListIsAnArrayOfPaths;
var
    S: TJSONObject;
begin
    //  AN ARRAY, not recent_project's separated string: the file is read by
    //  people too, and a path is one entry whatever characters it holds.
    Started(False);
    FStore.Showing('/here/a.fitproj');
    FStore.Showing('/here/b.fitproj');
    S := StoredSection;
    try
        AssertEquals(2, S.Arrays['projects'].Count);
        AssertEquals('/here/b.fitproj', S.Arrays['projects'].Strings[0]);
        AssertEquals('/here/a.fitproj', S.Arrays['projects'].Strings[1]);
    finally
        S.Free;
    end;
end;

{ ---- on a real file -------------------------------------------------------- }

procedure TRecentProjectFileTest.SetUp;
begin
    FPath := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-recent-store-test.json';
    DeleteFile(FPath);
end;

procedure TRecentProjectFileTest.TearDown;
begin
    DeleteFile(FPath);
end;

procedure TRecentProjectFileTest.TheListSurvivesARestart;
var
    Settings: TMachineSettings;
    Store: TRecentProjectStore;
begin
    Settings := TMachineSettings.Create(FPath);
    Store := TRecentProjectStore.Create(Settings);
    try
        Store.Showing('/p/a.fitproj');
        Store.Showing('/p/b.fitproj');
    finally
        Store.Free;
        Settings.Free;
    end;
    Settings := TMachineSettings.Create(FPath);
    Store := TRecentProjectStore.Create(Settings);
    try
        AssertEquals('/p/b.fitproj', Store.LastProject);
        AssertEquals('/p/b.fitproj' + RecentSeparator + '/p/a.fitproj', Store.Recent);
    finally
        Store.Free;
        Settings.Free;
    end;
end;

initialization
    RegisterTest('unit', TRecentProjectStoreTest);
    RegisterTest('integration', TRecentProjectFileTest);
end.
