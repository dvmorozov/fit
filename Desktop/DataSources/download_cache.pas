// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where a downloaded data file is kept, and what it is called.)

WHY DOWNLOADS ARE KEPT AT ALL. A project stores the profile itself, so reopening
one never needs the file or the network. What the file is still needed for is
everything provenance promises: Reload profile, and being able to see what was
actually downloaded. A cache that the program can find again is what makes the
remembered source path true rather than decorative.

WHY UNDER THE PER-USER DATA DIRECTORY and not beside the project: the same series
is imported into many projects, the user did not choose to put a copy in their
documents, and the directory this uses is the one app_data_root already decides
for the Python environment.

THE DECISION IS SEPARATE FROM THE WRITING. Every rule here - sanitising a name a
remote service chose, keeping an existing file rather than overwriting it, what
a source with a hostile name can and cannot do to a path - is a function over
strings, so all of it is reachable from a test without a disk.

A REMOTE SERVICE NAMES THE FILE, so the name is treated as hostile: separators,
parent directories, colons, control characters and leading dots are removed
rather than escaped. A downloaded file must not be able to decide where it
lands.
}
unit download_cache;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    { Whether a path is really there: the check the caller passes in, so every
      branch below is reachable without writing a file.

      A NAMED TYPE RATHER THAN @FileExists at the call site, for the reason
      recent_project.DefaultPathExists spells out: the RTL has no one-argument
      FileExists, and in Delphi syntax mode the address of the wrong overload
      binds in silence. }
    TPathExists = function(const APath: string): boolean;

const
    { What the cache directory is called under the per-user data directory. }
    DownloadsDirName = 'downloads';
    { What a file is called when neither the server nor the source named it. }
    UnnamedDownload = 'download';

{ Where downloads are kept on this machine, or '' when the environment names no
  home at all - in which case the caller must say so rather than write into
  whatever directory the program was started from. }
function DownloadsRoot: string;

{ ANAME reduced to something that can be a file name here: no separators, no
  parent directory, no characters Windows refuses, nothing empty. }
function SanitisedFileName(const AName: string): string;

{ The name a downloaded file should have, from what the source called it, what
  the server suggested, and the extension the source says it produces. The
  SOURCE wins on the extension: it knows what it asked for, whereas a URL may
  end in anything at all, and the extension is what decides which loader reads
  the file. }
function DownloadFileName(const ASourceName, AServerName,
    AProducedExtension: string): string;

{ The full path a download lands at: one directory per source, so a user can see
  what came from where. }
function CachePath(const ARoot, ASourceId, AFileName: string): string;

{ APath, or the next free name beside it - 'series (2).csv' - when APath is
  taken. NOTHING IS OVERWRITTEN: two imports of the same symbol on different
  days are two files, and the older one is what an earlier project's provenance
  points at. }
function FreeCachePath(const APath: string; AExists: TPathExists): string;

{ Where downloads are kept, given what the user chose: their folder when they
  have chosen one, and the per-user default otherwise. One rule, so the wizard,
  the settings and what the window shows cannot disagree - and '' still means
  "this machine can say nowhere", which the caller must refuse rather than turn
  into a relative path. }
function ChosenDownloadsRoot(const AChosen: string): string;

{ The same file, in another folder. A rule over strings: what MOVING it means
  is the caller's, and this says where it would land. }
function PathInFolder(const AFolder, APath: string): string;

{ Copies a file. Here because moving a download between folders needs it when
  the two are on different disks, and because a copy that half-succeeds must
  not leave the original deleted - so the original is dropped only after this
  returns. }
procedure CopyFileTo(const AFrom, ATo: string);

{ Whether APath is really there. The application's own check, passed in. }
function DefaultPathExists(const APath: string): boolean;

{ What a user is told when the bytes arrived and the file could not be written:
  a full disk, a read-only home, a folder somebody removed underneath us. It
  names the PLACE, because that is what they can act on, and it says the fetch
  itself worked - otherwise a saving failure reads as a download failure and
  the user blames the service. }
function SaveFailureMessage(const ADirectory, ADetail: string): string;

implementation

uses
    app_data_root;

function DownloadsRoot: string;
begin
    Result := AppDataDir(DownloadsDirName);
end;

function ChosenDownloadsRoot(const AChosen: string): string;
begin
    Result := Trim(AChosen);
    if Result = '' then
        Result := DownloadsRoot;
end;

function PathInFolder(const AFolder, APath: string): string;
begin
    if (Trim(AFolder) = '') or (Trim(APath) = '') then
        Exit(APath);
    Result := IncludeTrailingPathDelimiter(AFolder) + ExtractFileName(APath);
end;

function SanitisedFileName(const AName: string): string;
const
    //  Everything Windows refuses in a name, plus the separators every platform
    //  reads as "go somewhere else".
    Forbidden = '<>:"/\|?*';
var
    i: longint;
    C: char;
begin
    Result := '';
    for i := 1 to Length(AName) do
    begin
        C := AName[i];
        if (C < #32) or (Pos(C, Forbidden) > 0) then
            Continue;
        Result := Result + C;
    end;
    //  '..' is the whole of the directory traversal, and a leading dot makes a
    //  file the user cannot see in their own file manager.
    Result := StringReplace(Result, '..', '.', [rfReplaceAll]);
    while (Result <> '') and ((Result[1] = '.') or (Result[1] = ' ')) do
        Delete(Result, 1, 1);
    Result := Trim(Result);
    //  Trailing dots and spaces are legal to write and impossible to open again
    //  on Windows.
    while (Result <> '') and ((Result[Length(Result)] = '.') or
        (Result[Length(Result)] = ' ')) do
        Delete(Result, Length(Result), 1);
end;

function DownloadFileName(const ASourceName, AServerName,
    AProducedExtension: string): string;
var
    Base, Ext, Produced: string;
begin
    Base := SanitisedFileName(ASourceName);
    if Base = '' then
        Base := SanitisedFileName(AServerName);
    if Base = '' then
        Base := UnnamedDownload;

    //  The source's declared extension is the one the loader registry will be
    //  asked about, so the file must actually carry it. A source naming several
    //  - '.XY;.TXT' - has not said which this file is, and then whatever the
    //  name already carries is the best evidence there is.
    Produced := Trim(AProducedExtension);
    if Pos(';', Produced) > 0 then
        Produced := '';
    if (Produced <> '') and (Produced[1] <> '.') then
        Produced := '.' + Produced;
    //  LOWER CASE, although the registry keeps extensions upper case: that is
    //  how the registry MATCHES them, and it upper-cases what it is given, so
    //  nothing is lost - while a directory of files called SP500.CSV is what a
    //  user has to look at afterwards.
    Produced := LowerCase(Produced);

    Ext := ExtractFileExt(Base);
    if Produced <> '' then
    begin
        if not SameText(Ext, Produced) then
            Base := Base + Produced;
    end
    else if Ext = '' then
        //  Nothing knows what this is. Leaving it extensionless is honest: the
        //  registry then says no reader handles it, which is exactly true.
        ;
    Result := Base;
end;

function CachePath(const ARoot, ASourceId, AFileName: string): string;
begin
    if ARoot = '' then
        Exit('');
    Result := IncludeTrailingPathDelimiter(ARoot) +
        SanitisedFileName(ASourceId);
    Result := IncludeTrailingPathDelimiter(Result) +
        SanitisedFileName(AFileName);
end;

function FreeCachePath(const APath: string; AExists: TPathExists): string;
var
    Dir, Base, Ext: string;
    n: longint;
begin
    Result := APath;
    if (APath = '') or not Assigned(AExists) or not AExists(APath) then
        Exit;
    Dir := ExtractFilePath(APath);
    Ext := ExtractFileExt(APath);
    Base := ChangeFileExt(ExtractFileName(APath), '');
    n := 2;
    repeat
        Result := Dir + Base + ' (' + IntToStr(n) + ')' + Ext;
        Inc(n);
        //  A bound rather than a loop that cannot end: a thousand copies of one
        //  file means something is wrong that another name will not fix.
    until not AExists(Result) or (n > 1000);
end;

procedure CopyFileTo(const AFrom, ATo: string);
var
    Source, Target: TFileStream;
begin
    Source := TFileStream.Create(AFrom, fmOpenRead or fmShareDenyWrite);
    try
        Target := TFileStream.Create(ATo, fmCreate);
        try
            Target.CopyFrom(Source, Source.Size);
        finally
            Target.Free;
        end;
    finally
        Source.Free;
    end;
end;

function DefaultPathExists(const APath: string): boolean;
begin
    Result := FileExists(APath);
end;

function SaveFailureMessage(const ADirectory, ADetail: string): string;
begin
    Result := 'The file was fetched, but could not be saved in ' +
        ADirectory + ': ' + ADetail +
        '. Check that there is room on the disk and that the folder can be ' +
        'written to.';
end;

end.
