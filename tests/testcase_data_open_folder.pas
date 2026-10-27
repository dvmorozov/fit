// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(File > Import Profile's dialog opens where the user's data is, never
in the program's own folder.)

The existence check is passed in, so the file system here is a list of folders
the test says are there.
}
unit testcase_data_open_folder;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_project_document,
    data_open_folder;

type
    TDataOpenFolderTest = class(TTestCase)
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ItOpensWhereTheProjectsDataIs;
        procedure FetchedDataIsNotWhereTheUserKeepsFiles;
        procedure WithoutProjectDataItOpensWhereTheLastChosenFileWas;
        procedure ThenBesideTheProjectFile;
        procedure OtherwiseInTheHomeFolder;
        procedure AFolderThatIsGoneIsPassedOver;
        procedure NothingKnownAndNoHomeLeavesItToTheDialog;
        procedure TheProjectSaysWhereItsDataCameFrom;
    end;

    { The one test that asks the real file system: the check the window uses
      is the RTL's, through the named wrapper. }
    TDataOpenFolderOnDiskTest = class(TTestCase)
    published
        procedure TheWindowsCheckFindsAFolderThatIsThere;
    end;

implementation

var
    Folders: TStringList = nil;

function Exists(const AFolder: string): boolean;
begin
    Result := Folders.IndexOf(AFolder) >= 0;
end;

function Context(const AData: string; AFetched: boolean; const ALast,
    AProject, AHome: string): TDataOpenContext;
begin
    Result.ProjectDataFile := AData;
    Result.ProjectDataFetched := AFetched;
    Result.LastChosenFile := ALast;
    Result.ProjectFile := AProject;
    Result.HomeFolder := AHome;
end;

function Dir(const APath: string): string;
begin
    Result := SetDirSeparators(APath);
end;

procedure TDataOpenFolderTest.SetUp;
begin
    Folders := TStringList.Create;
    Folders.Add(Dir('/w/data/'));
    Folders.Add(Dir('/w/last/'));
    Folders.Add(Dir('/w/projects/'));
    Folders.Add(Dir('/home/u/'));
    Folders.Add(Dir('/cache/downloads/'));
end;

procedure TDataOpenFolderTest.TearDown;
begin
    FreeAndNil(Folders);
end;

procedure TDataOpenFolderTest.ItOpensWhereTheProjectsDataIs;
begin
    AssertEquals(Dir('/w/data/'), DataOpenFolder(Context(Dir('/w/data/x.dat'),
        False, Dir('/w/last/y.dat'), Dir('/w/projects/p.fitproj'), Dir('/home/u/')),
        @Exists));
end;

procedure TDataOpenFolderTest.FetchedDataIsNotWhereTheUserKeepsFiles;
begin
    AssertEquals(Dir('/w/last/'), DataOpenFolder(Context(
        Dir('/cache/downloads/brent.csv'), True, Dir('/w/last/y.dat'),
        Dir('/w/projects/p.fitproj'), Dir('/home/u/')), @Exists));
end;

procedure TDataOpenFolderTest.WithoutProjectDataItOpensWhereTheLastChosenFileWas;
begin
    AssertEquals(Dir('/w/last/'), DataOpenFolder(Context('', False,
        Dir('/w/last/y.dat'), Dir('/w/projects/p.fitproj'), Dir('/home/u/')),
        @Exists));
end;

procedure TDataOpenFolderTest.ThenBesideTheProjectFile;
begin
    AssertEquals(Dir('/w/projects/'), DataOpenFolder(Context(
        Dir('/cache/downloads/brent.csv'), True, '', Dir('/w/projects/p.fitproj'),
        Dir('/home/u/')), @Exists));
end;

procedure TDataOpenFolderTest.OtherwiseInTheHomeFolder;
begin
    AssertEquals(Dir('/home/u/'), DataOpenFolder(Context('', False, '', '',
        Dir('/home/u/')), @Exists));
end;

procedure TDataOpenFolderTest.AFolderThatIsGoneIsPassedOver;
begin
    AssertEquals('the data''s folder is gone', Dir('/w/last/'), DataOpenFolder(
        Context(Dir('/gone/x.dat'), False, Dir('/w/last/y.dat'), '',
        Dir('/home/u/')), @Exists));
    AssertEquals('every folder is gone but home', Dir('/home/u/'), DataOpenFolder(
        Context(Dir('/gone/x.dat'), False, Dir('/gone/y.dat'),
        Dir('/gone/p.fitproj'), Dir('/home/u/')), @Exists));
end;

procedure TDataOpenFolderTest.NothingKnownAndNoHomeLeavesItToTheDialog;
begin
    AssertEquals('', DataOpenFolder(Context('', False, '', '', ''), @Exists));
end;

procedure TDataOpenFolderTest.TheProjectSaysWhereItsDataCameFrom;
var
    P: TProjectProvenance;
    C: TDataOpenContext;
begin
    P := Default(TProjectProvenance);
    P.SourcePath := Dir('/w/data/x.dat');
    C := DataOpenContextOf(P, Dir('/w/projects/p.fitproj'), Dir('/w/last/y.dat'),
        Dir('/home/u/'));
    AssertEquals(Dir('/w/data/x.dat'), C.ProjectDataFile);
    AssertFalse('chosen on this computer', C.ProjectDataFetched);
    AssertEquals(Dir('/w/projects/p.fitproj'), C.ProjectFile);
    AssertEquals(Dir('/w/last/y.dat'), C.LastChosenFile);
    AssertEquals(Dir('/home/u/'), C.HomeFolder);
    P.OriginSourceId := 'fred';
    AssertTrue('fetched by a data source', DataOpenContextOf(P, '', '', '').
        ProjectDataFetched);
end;

procedure TDataOpenFolderOnDiskTest.TheWindowsCheckFindsAFolderThatIsThere;
var
    C: TDataOpenContext;
begin
    AssertTrue(DefaultFolderExists(GetTempDir));
    AssertFalse(DefaultFolderExists(IncludeTrailingPathDelimiter(GetTempDir) +
        'no-such-folder-' + IntToStr(GetProcessID)));
    C := Context(IncludeTrailingPathDelimiter(GetTempDir) + 'x.dat', False, '',
        '', '');
    AssertEquals('the production overload', IncludeTrailingPathDelimiter(
        GetTempDir), DataOpenFolder(C));
end;

initialization
    RegisterTest('unit', TDataOpenFolderTest);
    RegisterTest('integration', TDataOpenFolderOnDiskTest);
end.
