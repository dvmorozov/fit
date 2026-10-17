// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where a downloaded file lands, and what a remote service may call it.)

THE NAME COMES FROM SOMEWHERE ELSE, which is the whole hazard: a service names
the file, and a name is a path unless something stops it being one. These are
rules over strings with the existence check passed in, so every branch - the
traversal, the collision, the file nobody named - is reachable without a disk.
}
unit testcase_download_cache;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, download_cache, app_data_root;

type
    TDownloadCacheTest = class(TTestCase)
    published
        procedure ASeparatorCannotSurviveInAName;
        procedure AParentDirectoryCannotSurviveInAName;
        procedure ALeadingDotIsRemoved;
        procedure TheSourceDecidesTheExtension;
        procedure AnExtensionAlreadyRightIsNotRepeated;
        procedure TheServerNamesItWhenTheSourceDidNot;
        procedure AFileNobodyNamedStillHasAName;
        procedure ASourceNamingSeveralExtensionsAddsNone;
        procedure EachSourceKeepsItsOwnDirectory;
        procedure AnExistingFileIsNeverOverwritten;
        procedure WithNoHomeThereIsNoPath;
        procedure TheCacheSitsBesideTheOtherPerUserData;
        procedure AFolderTheUserChoseIsUsedInsteadOfTheDefault;
        procedure ChoosingNoFolderLeavesTheDefault;
        procedure TheSameFileInAnotherFolderKeepsItsName;
    end;

implementation

var
    TakenPath: string;

function PathIsTaken(const APath: string): boolean;
begin
    Result := SameText(APath, TakenPath);
end;

procedure TDownloadCacheTest.ASeparatorCannotSurviveInAName;
begin
    //  A downloaded file must not be able to decide where it lands.
    AssertEquals('etcpasswd', SanitisedFileName('../etc/passwd'));
    AssertEquals('windowssystem32', SanitisedFileName('\windows\system32'));
end;

procedure TDownloadCacheTest.AParentDirectoryCannotSurviveInAName;
begin
    AssertEquals('a.dat', SanitisedFileName('..a.dat'));
end;

procedure TDownloadCacheTest.ALeadingDotIsRemoved;
begin
    //  Not security: a file the user cannot see in their own file manager is a
    //  download they cannot find again.
    AssertEquals('profile', SanitisedFileName('.profile'));
end;

procedure TDownloadCacheTest.TheSourceDecidesTheExtension;
begin
    //  The URL may end in anything at all - a query, a record number, nothing.
    //  What decides the reader is the extension, so the source's own statement
    //  of what it fetched wins over whatever the name happened to carry.
    AssertEquals('SP500.csv', DownloadFileName('SP500', '', '.CSV'));
end;

procedure TDownloadCacheTest.AnExtensionAlreadyRightIsNotRepeated;
begin
    AssertEquals('series.csv', DownloadFileName('series.csv', '', '.csv'));
end;

procedure TDownloadCacheTest.TheServerNamesItWhenTheSourceDidNot;
begin
    AssertEquals('spectrum.xy', DownloadFileName('', 'spectrum.xy', '.XY'));
end;

procedure TDownloadCacheTest.AFileNobodyNamedStillHasAName;
begin
    //  Extensionless on purpose: the registry then says no reader handles it,
    //  which is exactly true, rather than a guessed format failing to parse.
    AssertEquals('download', DownloadFileName('', '', ''));
end;

procedure TDownloadCacheTest.ASourceNamingSeveralExtensionsAddsNone;
begin
    //  '.XY;.TXT' says the source can deliver either, not which this file is,
    //  so the name it already carries is the best evidence there is.
    AssertEquals('record.txt', DownloadFileName('record.txt', '', '.XY;.TXT'));
end;

procedure TDownloadCacheTest.EachSourceKeepsItsOwnDirectory;
var
    Path: string;
begin
    Path := CachePath('/data/downloads', 'doi', 'spectrum.xy');
    //  One directory per source, so a user can see what came from where.
    AssertTrue('the source names the directory: ' + Path,
        Pos('doi', Path) > 0);
    AssertTrue('and the file keeps its name',
        Pos('spectrum.xy', Path) > 0);
end;

procedure TDownloadCacheTest.AnExistingFileIsNeverOverwritten;
var
    Path: string;
begin
    //  Two imports of one symbol on different days are two files: the older
    //  one is what an earlier project's provenance points at.
    TakenPath := '/downloads/feed/series.csv';
    Path := FreeCachePath(TakenPath, @PathIsTaken);
    AssertEquals('the taken name is stepped over',
        '/downloads/feed/series (2).csv', Path);
    TakenPath := '';
    AssertEquals('a free name is used as it is', '/downloads/feed/series.csv',
        FreeCachePath('/downloads/feed/series.csv', @PathIsTaken));
end;

procedure TDownloadCacheTest.WithNoHomeThereIsNoPath;
begin
    //  '' rather than a relative path: a relative one resolves against
    //  whatever directory the program was started from, which is where a
    //  download would then silently appear.
    AssertEquals('', AppDataRootFrom('', '', ''));
    AssertEquals('', CachePath('', 'doi', 'x.xy'));
end;

procedure TDownloadCacheTest.TheCacheSitsBesideTheOtherPerUserData;
var
    Root: string;
begin
    //  One decision about where this program keeps per-user data, shared with
    //  the Python environment's location - two copies of that rule is how one
    //  of them ends up under the roaming profile.
    Root := AppDataRootFrom('C:\Users\u\AppData\Local', '/xdg', '/home/u');
    AssertTrue('the root is named after the application: ' + Root,
        (Pos('Fit', Root) > 0) or (Pos('fit', Root) > 0));
    AssertTrue('and the cache is under it',
        Pos(Root, AppDataDir(DownloadsDirName)) >= 0);
end;

procedure TDownloadCacheTest.AFolderTheUserChoseIsUsedInsteadOfTheDefault;
begin
    //  A download that lands somewhere the user cannot name is a file they
    //  cannot find again, so the folder is theirs to choose and is remembered.
    AssertEquals('/home/u/Data', ChosenDownloadsRoot('/home/u/Data'));
end;

procedure TDownloadCacheTest.ChoosingNoFolderLeavesTheDefault;
begin
    //  Empty means "they never chose one", not "nowhere": a machine's layout
    //  can change between sessions, so the default is worked out each time
    //  rather than written into the settings the first time.
    AssertEquals(DownloadsRoot, ChosenDownloadsRoot(''));
    AssertEquals(DownloadsRoot, ChosenDownloadsRoot('   '));
end;

procedure TDownloadCacheTest.TheSameFileInAnotherFolderKeepsItsName;
begin
    AssertEquals('/home/u/Data' + PathDelim + 'SP500.csv',
        PathInFolder('/home/u/Data', '/var/cache/fit/fred/SP500.csv'));
    //  Nothing to move it to, or nothing to move: unchanged rather than a
    //  path built out of an empty half.
    AssertEquals('/var/cache/x.csv', PathInFolder('', '/var/cache/x.csv'));
end;

initialization
    RegisterTest('unit', TDownloadCacheTest);
end.
