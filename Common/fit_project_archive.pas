// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The project file's container: a set of named parts, stored as a ZIP.)

WHAT THIS IS AND IS NOT. It is a bag of named byte strings that can be written to
and read from a stream. It has NO opinion about what the parts mean - which of
them must be present, what version they are, how they parse. That belongs to the
document layer above, and keeping it out of here is what lets this be checked
exhaustively over bytes alone.

WHY PARTS RATHER THAN ONE DOCUMENT. A project file must carry sections that do
not exist yet. As members of one schema, every future feature would edit the one
file every other feature also edits. As parts, a feature adds one and nothing
else changes.

THE INVARIANT THAT MAKES THAT REAL: a part this build does not understand is read
and written back unchanged. Opening a newer project in an older build and saving
must not destroy what the newer build wrote - a property of the read/modify/write
path, not of a version number in a header. `WithPart` is the whole mechanism:
callers replace what they know and hand the rest back untouched.

WHY ZIP, AND WHY `zipper`. The container convention is the ordinary one - ODF,
.xlsx and .blend are all a ZIP of parts - so a project file can be opened with
any tool a user already has when something goes wrong. `zipper` ships with FPC,
so this costs one RTL unit rather than a dependency, and this project has been
removing dependencies rather than adding them.

WHY THE PUBLIC SURFACE IS A STREAM. The filesystem is an external dependency by
this project's testing rule, so a codec reachable only through a file could only
be covered by integration tests - the half that is not measured. Everything
decided here is decided over bytes; the two file wrappers are the only part that
needs a disk, and they decide nothing.
}
unit fit_project_archive;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, zipper;

type
    { One named section of a project file. Content is carried as bytes - the
      sections are UTF-8 JSON, and nothing here re-encodes anything. }
    TProjectPart = record
        { The name as it appears in the container, e.g. 'manifest.json' or
          'modules/sample.json'. A '/' is a path inside the archive and is NOT
          translated to the host's separator: a project written on one platform
          must name the same parts when read on another. }
        Name: string;
        Content: string;
    end;
    TProjectParts = array of TProjectPart;

{ Writes every part to AStream. False when the container could not be built. }
function WriteProjectArchive(const AParts: TProjectParts;
    AStream: TStream): boolean;

{ Reads every part AStream holds, in the order the container lists them.

  FALSE rather than an exception for anything that is not a readable archive:
  this reads a file the user chose, and choosing the wrong one is an ordinary
  mistake that has to be reported as "not a project" rather than as a fault. }
function ReadProjectArchive(AStream: TStream;
    out AParts: TProjectParts): boolean;

{ The position of the part named AName, or -1.

  EXACT comparison, no case folding and no nearest match: a part name is an
  identifier in a file format, and folding it would make a project readable on
  one platform and not another for reasons nothing shows. }
function IndexOfPart(const AParts: TProjectParts; const AName: string): longint;

{ The content of the part named AName. False - leaving AContent empty - when
  there is none, so "absent" and "present but empty" stay distinguishable. }
function PartContent(const AParts: TProjectParts; const AName: string;
    out AContent: string): boolean;

{ AParts with AName set to AContent: replaced where it is present, appended
  where it is not, and everything else returned untouched and in order.

  THIS IS THE PRESERVATION MECHANISM. A caller reads every part, replaces the
  ones it understands through this, and writes them all back - so a part it has
  never heard of survives without the caller knowing it exists. }
function WithPart(const AParts: TProjectParts;
    const AName, AContent: string): TProjectParts;

implementation

type
    { Feeds the unzipper from a stream and collects what comes out of it.

      TUnZipper reads a FILE by default. Its four stream hooks are what let the
      whole thing happen in memory: two supply the archive being read, two
      receive each part. Without them this unit could only be tested through the
      filesystem, which by this project's rule would move it out of the measured
      half of the suite entirely. }
    TArchiveReader = class
    private
        FSource: TStream;
        FParts: TProjectParts;
        procedure OpenInput(Sender: TObject; var AStream: TStream);
        procedure CloseInput(Sender: TObject; var AStream: TStream);
        procedure CreateOut(Sender: TObject; var AStream: TStream;
            AItem: TFullZipFileEntry);
        procedure DoneOut(Sender: TObject; var AStream: TStream;
            AItem: TFullZipFileEntry);
    public
        constructor Create(ASource: TStream);
        function Read: boolean;
        property Parts: TProjectParts read FParts;
    end;

constructor TArchiveReader.Create(ASource: TStream);
begin
    inherited Create;
    FSource := ASource;
    FParts := nil;
end;

procedure TArchiveReader.OpenInput(Sender: TObject; var AStream: TStream);
begin
    //  The caller's stream, positioned at its start. Not a copy: the unzipper
    //  seeks in it and never writes to it.
    FSource.Position := 0;
    AStream := FSource;
end;

procedure TArchiveReader.CloseInput(Sender: TObject; var AStream: TStream);
begin
    //  Deliberately NOT freed. The stream belongs to the caller, who may go on
    //  using it - and freeing another object's stream from inside a callback is
    //  the kind of ownership mistake that shows up as a crash somewhere else.
    AStream := nil;
end;

procedure TArchiveReader.CreateOut(Sender: TObject; var AStream: TStream;
    AItem: TFullZipFileEntry);
begin
    AStream := TMemoryStream.Create;
end;

procedure TArchiveReader.DoneOut(Sender: TObject; var AStream: TStream;
    AItem: TFullZipFileEntry);
var
    n: longint;
    Content: string;
begin
    Content := '';
    n := AStream.Size;
    if n > 0 then
    begin
        SetLength(Content, n);
        AStream.Position := 0;
        AStream.ReadBuffer(Content[1], n);
    end;
    //  ArchiveFileName, not the disk name the unzipper would have used: the
    //  latter is translated to the host's path separator, which would rename
    //  every module part when a project moved between platforms.
    SetLength(FParts, Length(FParts) + 1);
    FParts[High(FParts)].Name := AItem.ArchiveFileName;
    FParts[High(FParts)].Content := Content;
    FreeAndNil(AStream);
end;

function TArchiveReader.Read: boolean;
var
    Un: TUnZipper;
begin
    Result := False;
    Un := TUnZipper.Create;
    try
        Un.OnOpenInputStream := @OpenInput;
        Un.OnCloseInputStream := @CloseInput;
        Un.OnCreateStream := @CreateOut;
        Un.OnDoneStream := @DoneOut;
        try
            Un.UnZipAllFiles;
            Result := True;
        except
            //  Anything at all: not an archive, truncated, a member that will
            //  not inflate. All of them mean the same thing to a caller - this
            //  is not a project file - and none of them is this program's
            //  fault, so none of them escapes as an exception.
            on E: Exception do
                Result := False;
        end;
    finally
        Un.Free;
    end;
end;

function WriteProjectArchive(const AParts: TProjectParts;
    AStream: TStream): boolean;
var
    Z: TZipper;
    Sources: TList;
    Src: TMemoryStream;
    i: longint;
begin
    Result := False;
    Z := TZipper.Create;
    //  The entry streams have to outlive AddFileEntry: the zipper reads them
    //  when SaveToStream runs, not when they are added.
    Sources := TList.Create;
    try
        try
            for i := 0 to High(AParts) do
            begin
                Src := TMemoryStream.Create;
                Sources.Add(Src);
                if Length(AParts[i].Content) > 0 then
                    Src.WriteBuffer(AParts[i].Content[1],
                        Length(AParts[i].Content));
                Src.Position := 0;
                Z.Entries.AddFileEntry(Src, AParts[i].Name);
            end;
            Z.SaveToStream(AStream);
            Result := True;
        except
            on E: Exception do
                Result := False;
        end;
    finally
        for i := 0 to Sources.Count - 1 do
            TMemoryStream(Sources[i]).Free;
        Sources.Free;
        Z.Free;
    end;
end;

function ReadProjectArchive(AStream: TStream;
    out AParts: TProjectParts): boolean;
var
    Reader: TArchiveReader;
begin
    AParts := nil;
    Result := False;
    if not Assigned(AStream) then
        Exit;
    //  An empty stream is not an archive, and asking the unzipper about one
    //  costs an exception to find out.
    if AStream.Size = 0 then
        Exit;

    Reader := TArchiveReader.Create(AStream);
    try
        Result := Reader.Read;
        if Result then
            AParts := Reader.Parts;
    finally
        Reader.Free;
    end;
end;

function IndexOfPart(const AParts: TProjectParts; const AName: string): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to High(AParts) do
        if AParts[i].Name = AName then
            Exit(i);
end;

function PartContent(const AParts: TProjectParts; const AName: string;
    out AContent: string): boolean;
var
    Index_: longint;
begin
    AContent := '';
    Index_ := IndexOfPart(AParts, AName);
    Result := Index_ >= 0;
    if Result then
        AContent := AParts[Index_].Content;
end;

function WithPart(const AParts: TProjectParts;
    const AName, AContent: string): TProjectParts;
var
    Index_: longint;
begin
    Result := Copy(AParts, 0, Length(AParts));
    Index_ := IndexOfPart(Result, AName);
    if Index_ >= 0 then
    begin
        Result[Index_].Content := AContent;
        Exit;
    end;
    //  APPENDED, never inserted. The order parts appear in is the order they
    //  were first written, which keeps a saved file's byte layout stable when
    //  only its contents change - so two saves of an unedited project differ in
    //  nothing a reader has to explain.
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Name := AName;
    Result[High(Result)].Content := AContent;
end;

end.
