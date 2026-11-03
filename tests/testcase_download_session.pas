// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which downloaded files are the user's, and which are this program's litter.)

THE REPORT: 'SP500 (9).csv'. Every look at a data source writes a file - the
preview has to read something - and most looks end in "no": the wrong series,
the wrong record, a file that turned out to be a web page. Each "no" left a file
behind under a stepped-over name, forever, and the user had asked for none of
them.

WHAT MAKES THIS DELICATE is the other half: a file that DID become a project is
the user's. Their project's provenance names it and Reload profile reads it, so
deleting it would break something they built. And a file they moved into a
folder of their own is theirs by that act, whatever they did with it afterwards
- tidying somebody's Documents folder because they changed their mind about a
chart is not tidiness.

Nothing here touches a disk: deleting is passed in.
}
unit testcase_download_session;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, download_session;

type
    TDownloadSessionTest = class(TTestCase)
    private
        FSession: TDownloadSession;
        function Rubbish: string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AFileNoProjectUsesIsThrownAway;
        procedure AFileTheUserMovedElsewhereIsNeverTouched;
        procedure EveryLookThatEndedInNoIsSweptUp;
        procedure WithNoCacheDirectoryNothingIsDeleted;
        procedure TheSameFileWrittenTwiceIsOneFile;
        procedure WhatWasThrownAwayIsNotTriedAgain;
        procedure AFileThatWouldNotDeleteIsNotRetriedForever;
        procedure DiscardingWithNoWayToDeleteDoesNothing;
        //  Files an earlier session - or an earlier build - left in the cache.
        procedure ALeftoverOfAnEarlierSessionIsSweptToo;
        procedure AFileListedOutsideTheCacheIsNeverTouched;
        procedure AFileWrittenAndFoundIsOneFile;
    end;

implementation

const
    Cache = '/home/u/.local/share/fit/downloads';

var
    Deleted: TStringList;
    Refuse: string;

{ What the cache directory holds, as the session's lister reports it. }
var
    OnDisk: TStringArray;

function ListOnDisk(const ARoot: string): TStringArray;
begin
    Result := OnDisk;
end;

function RecordDelete(const APath: string): boolean;
begin
    Deleted.Add(APath);
    Result := not SameText(APath, Refuse);
end;

procedure TDownloadSessionTest.SetUp;
begin
    inherited SetUp;
    FSession := TDownloadSession.Create;
    FSession.CleanableRoot := Cache;
    OnDisk := nil;
    Deleted := TStringList.Create;
    Refuse := '';
end;

procedure TDownloadSessionTest.TearDown;
begin
    FreeAndNil(Deleted);
    FreeAndNil(FSession);
    inherited TearDown;
end;

function TDownloadSessionTest.Rubbish: string;
var
    Going: TStringArray;
    i: longint;
begin
    Result := '';
    Going := FSession.Rubbish;
    for i := 0 to High(Going) do
    begin
        if Result <> '' then
            Result := Result + ';';
        Result := Result + ExtractFileName(Going[i]);
    end;
end;

procedure TDownloadSessionTest.AFileNoProjectUsesIsThrownAway;
begin
    FSession.Wrote(Cache + '/fred/SP500.csv');
    AssertEquals('SP500.csv', Rubbish);
    AssertEquals('and it goes', 1, FSession.Discard(RecordDelete));
    AssertEquals(Cache + '/fred/SP500.csv', Deleted[0]);
end;

procedure TDownloadSessionTest.AFileTheUserMovedElsewhereIsNeverTouched;
begin
    //  Outside the application's own cache: somebody's own folder, which is
    //  never this program's to tidy.
    FSession.Wrote('/home/u/Documents/series/SP500.csv');
    AssertEquals('outside the cache, so not ours', '', Rubbish);
    AssertEquals(0, FSession.Discard(RecordDelete));
end;

procedure TDownloadSessionTest.EveryLookThatEndedInNoIsSweptUp;
begin
    //  A morning's work: three looks, one project - and the project holds
    //  its data, so all three go. The ones never wanted used to stay for good,
    //  and so did the one a project was made from.
    FSession.Wrote(Cache + '/fred/SP500.csv');
    FSession.Wrote(Cache + '/fred/SP500 (2).csv');
    FSession.Wrote(Cache + '/doi/notes.pdf');
    AssertEquals('SP500.csv;SP500 (2).csv;notes.pdf', Rubbish);
    AssertEquals(3, FSession.Discard(RecordDelete));
end;

procedure TDownloadSessionTest.WithNoCacheDirectoryNothingIsDeleted;
begin
    //  A machine whose environment names no home at all: the session does not
    //  know where its own cache is, and must not guess.
    FSession.CleanableRoot := '';
    FSession.Wrote(Cache + '/fred/SP500.csv');
    AssertEquals('', Rubbish);
    AssertEquals(0, FSession.Discard(RecordDelete));
end;

procedure TDownloadSessionTest.TheSameFileWrittenTwiceIsOneFile;
begin
    FSession.Wrote(Cache + '/fred/SP500.csv');
    FSession.Wrote(Cache + '/fred/SP500.csv');
    AssertEquals('SP500.csv', Rubbish);
end;

procedure TDownloadSessionTest.WhatWasThrownAwayIsNotTriedAgain;
begin
    FSession.Wrote(Cache + '/fred/SP500.csv');
    FSession.Discard(RecordDelete);
    Deleted.Clear;
    //  A second close - or a second sweep - has nothing left to do.
    AssertEquals(0, FSession.Discard(RecordDelete));
    AssertEquals(0, Deleted.Count);
end;

procedure TDownloadSessionTest.AFileThatWouldNotDeleteIsNotRetriedForever;
begin
    //  Open in another program, or on a disk that has gone away. Trying again
    //  at every close achieves nothing and slows every one of them.
    FSession.Wrote(Cache + '/fred/SP500.csv');
    Refuse := Cache + '/fred/SP500.csv';
    AssertEquals('it refused', 0, FSession.Discard(RecordDelete));
    AssertEquals('it was tried once', 1, Deleted.Count);
    AssertEquals('and forgotten', 0, FSession.Discard(RecordDelete));
end;

procedure TDownloadSessionTest.DiscardingWithNoWayToDeleteDoesNothing;
begin
    FSession.Wrote(Cache + '/fred/SP500.csv');
    AssertEquals(0, FSession.Discard(nil));
    //  And nothing was forgotten, so the next close still sweeps it.
    AssertEquals('SP500.csv', Rubbish);
end;

{ THE "(2)" IN A FILE NAME: a build that kept the file a project was made from
  left it in the cache for good, and the next download of the same file
  stepped over it. The session sweeps what it finds there, not only what it
  wrote (the user, 2026-10-20). }
procedure TDownloadSessionTest.ALeftoverOfAnEarlierSessionIsSweptToo;
begin
    FSession.ListFiles := ListOnDisk;
    SetLength(OnDisk, 1);
    OnDisk[0] := Cache + '/doi/Tb2Fe17.dat';
    AssertEquals('Tb2Fe17.dat', Rubbish);
    AssertEquals(1, FSession.Discard(RecordDelete));
end;

procedure TDownloadSessionTest.AFileListedOutsideTheCacheIsNeverTouched;
begin
    FSession.ListFiles := ListOnDisk;
    SetLength(OnDisk, 1);
    OnDisk[0] := '/home/u/Documents/Tb2Fe17.dat';
    AssertEquals('', Rubbish);
    AssertEquals(0, FSession.Discard(RecordDelete));
end;

procedure TDownloadSessionTest.AFileWrittenAndFoundIsOneFile;
begin
    FSession.ListFiles := ListOnDisk;
    FSession.Wrote(Cache + '/fred/SP500.csv');
    SetLength(OnDisk, 1);
    OnDisk[0] := Cache + '/fred/SP500.csv';
    AssertEquals('SP500.csv', Rubbish);
end;

initialization
    RegisterTest('unit', TDownloadSessionTest);
end.
