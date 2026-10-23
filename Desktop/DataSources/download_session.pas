// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which downloaded files were worth keeping, and which were not.)

WHAT THIS ENDS. Every look at a data source writes a file: the preview has to
read something, so the bytes go to disk before anyone has decided anything. Most
of those decisions are "no" - the wrong series, the wrong record, a file that
turned out to hold a web page - and each "no" used to leave a file behind
FOREVER, under a stepped-over name. 'SP500 (9).csv' is what that looks like
after a morning's work, and the user never asked for one of them.

THE RULE. Every file this program fetched is rubbish once it has been looked
at: one that became a project goes as soon as the import has run, because the
project stores the data itself, and the rest go when the program closes. So
does anything else found in the cache: an earlier session that ended before it
could sweep, or a build that still kept files, left it there.

IT WAS ONCE OTHERWISE, and the reason it changed is worth keeping: a file that
became a project used to be kept, so that Reload Profile could read it again and
the provenance pointed at something real. That left a folder of files the user
never asked for, beside a "Save in" choice they did not want either (the user,
2026-10-20). The provenance keeps the origin - source, query, address, day -
which is what fetching it again needs, and Reload Profile says the file is gone.

WHERE IT DOES NOT APPLY. Only the application's own downloads directory is ever
cleaned. Deleting something out of somebody's Documents folder is not tidiness.

NOTHING HERE TOUCHES A DISK. Deleting is passed in, so every rule above is
reachable from a test that creates no file at all.
}
unit download_session;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    TStringArray = array of string;

    { Deletes one file and says whether it went. Passed in, so the rules here
      are testable without a disk. }
    TDeleteFile = function(const APath: string): boolean;
    { Every file under a directory, at any depth. Passed in for the reason
      deleting is. }
    TListFiles = function(const ARoot: string): TStringArray;

    TDownloadSession = class(TObject)
    private
        FWritten: TStringList;
        FRoot: string;
        FListFiles: TListFiles;
    public
        constructor Create;
        destructor Destroy; override;

        { The directory this session may clean. Anything outside it is left
          alone, whatever happened to it - see the unit header. }
        property CleanableRoot: string read FRoot write FRoot;
        { What finds the files already in the cache - left by an earlier
          session, or by a build that kept them. nil finds none. }
        property ListFiles: TListFiles read FListFiles write FListFiles;

        { A file this session wrote. Called by whatever saved it. }
        procedure Wrote(const APath: string);
        { What would be deleted now, in the order it was written. }
        function Rubbish: TStringArray;
        { Deletes it, and answers how many went. The session forgets what it
          has thrown away, so a second call deletes nothing twice. }
        function Discard(ADelete: TDeleteFile): longint;
    end;

{ The session this application is in. One per process: the wizard writes into
  it, and the import and the window's closing empty it. }
function CurrentDownloadSession: TDownloadSession;

{ Deletes a file. The application's own, passed to Discard. }
function DefaultDeleteFile(const APath: string): boolean;

{ Every file under ARoot, at any depth; none when it is not there. The
  application's own lister, which CurrentDownloadSession uses. }
function DefaultListFiles(const ARoot: string): TStringArray;

implementation

var
    Session: TDownloadSession = nil;

function DefaultDeleteFile(const APath: string): boolean;
begin
    Result := DeleteFile(APath);
end;

procedure AddFilesUnder(const ADir: string; var AFiles: TStringArray);
var
    Found: TSearchRec;
    Dir: string;
begin
    Dir := IncludeTrailingPathDelimiter(ADir);
    if FindFirst(Dir + AllFilesMask, faAnyFile, Found) = 0 then
        try
            repeat
                if (Found.Name = '.') or (Found.Name = '..') then
                    Continue;
                if (Found.Attr and faDirectory) <> 0 then
                    AddFilesUnder(Dir + Found.Name, AFiles)
                else
                begin
                    SetLength(AFiles, Length(AFiles) + 1);
                    AFiles[High(AFiles)] := Dir + Found.Name;
                end;
            until FindNext(Found) <> 0;
        finally
            FindClose(Found);
        end;
end;

function DefaultListFiles(const ARoot: string): TStringArray;
begin
    Result := nil;
    if (Trim(ARoot) <> '') and DirectoryExists(ARoot) then
        AddFilesUnder(ARoot, Result);
end;

function CurrentDownloadSession: TDownloadSession;
begin
    if Session = nil then
    begin
        Session := TDownloadSession.Create;
        Session.ListFiles := @DefaultListFiles;
    end;
    Result := Session;
end;

constructor TDownloadSession.Create;
begin
    inherited Create;
    FWritten := TStringList.Create;
end;

destructor TDownloadSession.Destroy;
begin
    FWritten.Free;
    inherited Destroy;
end;

procedure TDownloadSession.Wrote(const APath: string);
begin
    if Trim(APath) = '' then
        Exit;
    if FWritten.IndexOf(APath) < 0 then
        FWritten.Add(APath);
end;

{ Whether APath is inside ARoot. Compared as text on a normalised path: both
  come from this program, and a downloaded file's name is sanitised before it
  is ever used. }
function Inside(const ARoot, APath: string): boolean;
var
    Root: string;
begin
    Root := Trim(ARoot);
    if Root = '' then
        //  NOTHING IS CLEANABLE when no root is stated. A session that does
        //  not know where its own cache is must not delete anything.
        Exit(False);
    Root := IncludeTrailingPathDelimiter(ExpandFileName(Root));
    Result := Pos(LowerCase(Root), LowerCase(ExpandFileName(APath))) = 1;
end;

function TDownloadSession.Rubbish: TStringArray;
var
    Candidates: TStringList;
    Found: TStringArray;
    i: longint;
begin
    Result := nil;
    Candidates := TStringList.Create;
    try
        Candidates.AddStrings(FWritten);
        //  AND WHAT IS ALREADY THERE, whoever wrote it: an earlier session
        //  that closed before it could sweep, or a build that kept the file a
        //  project was made from. Its next download of that file stepped over
        //  it - the "(2)" the user saw (2026-10-20).
        if Assigned(FListFiles) and (Trim(FRoot) <> '') then
        begin
            Found := FListFiles(FRoot);
            for i := 0 to High(Found) do
                if Candidates.IndexOf(Found[i]) < 0 then
                    Candidates.Add(Found[i]);
        end;
        for i := 0 to Candidates.Count - 1 do
        begin
            if not Inside(FRoot, Candidates[i]) then
                Continue;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Candidates[i];
        end;
    finally
        Candidates.Free;
    end;
end;

function TDownloadSession.Discard(ADelete: TDeleteFile): longint;
var
    Going: TStringArray;
    i: longint;
begin
    Result := 0;
    if not Assigned(ADelete) then
        Exit;
    Going := Rubbish;
    for i := 0 to High(Going) do
    begin
        if ADelete(Going[i]) then
            Inc(Result);
        //  FORGOTTEN EITHER WAY. A file that would not delete - open in
        //  another program, on a disk that has gone away - is not worth
        //  trying again and again at every close.
        //  A file only found, not written, is not on the list to forget.
        if FWritten.IndexOf(Going[i]) >= 0 then
            FWritten.Delete(FWritten.IndexOf(Going[i]));
    end;
end;

finalization
    Session.Free;
end.
