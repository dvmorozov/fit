// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Where a project's data came from, and whether that file still says the
same thing.)

PROVENANCE, NOT A DEPENDENCY. The profile itself is inside the project, so a
project opens on a machine that has never seen the source file. What this records
is for the user: which file this started from, and whether that file has changed
since - so a result that no longer matches its input can be noticed rather than
puzzled over months later.

WHY A HASH AND NOT A TIMESTAMP. A modification date changes when a file is
copied, restored from a backup, or synchronised, and does NOT change when a file
is edited and the date is preserved. Neither direction is rare, and both are
wrong in the way that matters: one cries wolf, the other stays silent about a
real change. The contents are the only thing that answers the question asked.

MD5, AND WHY THAT IS NOT A SECURITY CLAIM. The question is "is this the same
file", against accident - a re-export, an edited column, a truncated download.
Nobody is attacking a project file with a chosen-prefix collision; if that ever
becomes the question, this is one function to change and the field is already a
string. It is in the RTL, which keeps the dependency count where it is.

THE PURE HALF IS THE TESTED HALF. Hashing bytes touches nothing, so it is a unit
test; only the wrapper that reads a file is an integration test, and it decides
nothing.
}
unit fit_project_provenance;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, md5, fit_project_document;

type
    { How the bytes of a file are obtained.

      A SEAM, and it carries this comment so it can be re-checked later.
      Everything below decides something worth testing - what a failed read
      means, what an absent hash means, whether "cannot tell" is "has changed" -
      and all of it was reachable only by writing files, which by this project's
      rule puts it in the half that is not measured. The syscall stays behind
      this one function; the decisions come out in front of it.

      Callers pass nothing and get the real reader. }
    TByteReader = function(const APath: string; out AOk: boolean): string;

{ The content hash of ABytes, as lower-case hex. Empty input hashes to the hash
  of empty input, which is a value like any other - it is not an error, and a
  data file that is empty is a real thing to record. }
function HashOfBytes(const ABytes: string): string;

{ Reads APath and describes it: path, size, content hash, and the loader name
  the caller supplies. False when the file cannot be read, leaving AProvenance
  with the path alone - which is still worth keeping, because "we came from
  there and it is not there now" is exactly what a user needs told. }
function DescribeSourceFile(const APath, ALoaderName: string;
    out AProvenance: TProjectProvenance;
    AReader: TByteReader = nil): boolean;

{ Whether the file APath now holds something other than what AProvenance
  recorded.

  FALSE WHEN IT CANNOT BE CHECKED - the file is gone, or unreadable, or the
  project recorded no hash because it was written before this existed. "Cannot
  tell" must not be reported as "has changed": a warning that fires on every
  project written by an older version is a warning nobody reads. }
function SourceHasChanged(const AProvenance: TProjectProvenance;
    AReader: TByteReader = nil): boolean;

{ What to tell the user about the source file, or '' when there is nothing to
  say - which is the ordinary case and must stay silent.

  A NOTICE, NOT A REFUSAL, and logged rather than put in a dialog. The project
  carries its own profile, so nothing is wrong: the numbers are still the
  numbers that were fitted. What has happened is that the file they came from no
  longer says what it said, and that is worth being able to find out months
  later - and not worth a modal dialog every time someone reorganises a data
  directory. }
function SourceChangeNotice(const AProvenance: TProjectProvenance;
    AReader: TByteReader): string; overload;
{ The same with the real reader.

  A SECOND OVERLOAD RATHER THAN A DEFAULTED PARAMETER, and the reason is
  practical: a defaulted parameter makes a different procedural type, so nothing
  can take the address of the one-argument form - and a caller that wants to
  inject this as a seam has to write a wrapper for it. One did, and the wrapper
  was two lines that only production ever ran. }
function SourceChangeNotice(
    const AProvenance: TProjectProvenance): string; overload;

implementation

function HashOfBytes(const ABytes: string): string;
begin
    Result := LowerCase(MD5Print(MD5String(ABytes)));
end;

{ The whole file as bytes. Empty string when it cannot be read - the callers
  above each decide what that means, and neither of them raises. }
function ReadAllBytes(const APath: string; out AOk: boolean): string;
var
    S: TFileStream;
begin
    Result := '';
    AOk := False;
    if not FileExists(APath) then
        Exit;
    try
        S := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
        try
            SetLength(Result, S.Size);
            if S.Size > 0 then
                S.ReadBuffer(Result[1], S.Size);
            AOk := True;
        finally
            S.Free;
        end;
    except
        //  Locked, on a disconnected drive, no permission. All of them mean the
        //  same thing here - the contents cannot be seen - and none of them is
        //  a fault in this program.
        on E: Exception do
        begin
            Result := '';
            AOk := False;
        end;
    end;
end;

function DescribeSourceFile(const APath, ALoaderName: string;
    out AProvenance: TProjectProvenance; AReader: TByteReader): boolean;
var
    Bytes: string;
begin
    if not Assigned(AReader) then
        AReader := @ReadAllBytes;
    AProvenance := Default(TProjectProvenance);
    //  KEPT EVEN WHEN THE READ FAILS. Where the data came from is worth
    //  recording whether or not the file is still there, and a project that
    //  records nothing cannot tell the user where to look.
    AProvenance.SourcePath := APath;
    AProvenance.LoaderName := ALoaderName;
    Bytes := AReader(APath, Result);
    if not Result then
        Exit;
    AProvenance.SourceSize := Length(Bytes);
    AProvenance.SourceHash := HashOfBytes(Bytes);
end;

function SourceHasChanged(const AProvenance: TProjectProvenance;
    AReader: TByteReader): boolean;
var
    Bytes: string;
    Ok: boolean;
begin
    Result := False;
    if not Assigned(AReader) then
        AReader := @ReadAllBytes;
    //  Written before provenance existed, or by a reader that recorded none.
    if (AProvenance.SourcePath = '') or (AProvenance.SourceHash = '') then
        Exit;
    Bytes := AReader(AProvenance.SourcePath, Ok);
    if not Ok then
        //  CANNOT TELL IS NOT HAS CHANGED. The file may be on a drive that is
        //  not mounted today, and saying the data has changed because of that
        //  is how a warning stops being read.
        Exit;
    Result := HashOfBytes(Bytes) <> AProvenance.SourceHash;
end;

function SourceChangeNotice(const AProvenance: TProjectProvenance;
    AReader: TByteReader): string;
begin
    Result := '';
    if not SourceHasChanged(AProvenance, AReader) then
        Exit;
    //  NAMES THE FILE, and says what it does not mean. "The data has changed"
    //  would read as though the project were now wrong; it is not, and telling
    //  someone their results are suspect when they are not is worse than saying
    //  nothing.
    Result := 'The data file this project came from, "' +
        AProvenance.SourcePath + '", is not the file it was saved from. ' +
        'The project still holds the data it was fitted to, so nothing here ' +
        'has changed.';
end;

function SourceChangeNotice(const AProvenance: TProjectProvenance): string;
begin
    Result := SourceChangeNotice(AProvenance, nil);
end;

end.
