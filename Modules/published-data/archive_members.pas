// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What is inside a .zip, .gz, .tar or .tar.gz, and the bytes of one
member - read from memory, with nothing written to disk.)

WHY ARCHIVES AT ALL. Zip is the commonest kind of file in Zenodo's datasets -
181,000 records hold one, against 94,000 with a CSV (counted 2026-10-20) - and
gz is in the top ten. A record whose data is zipped was a record with nothing
Fit could open. So an archive is a FOLDER here: opened, it lists the members
Fit can read, and choosing one fetches only that member.

IN MEMORY, FROM WHAT WAS DOWNLOADED. The download is capped (web_client), so an
archive fits; extracting to disk would leave a tree in the user's temporary
folder for every record looked at, which the download session would not know
to clean.

GUARDED. A member's declared size is checked before it is inflated, and what is
inflated is counted while it arrives: a small archive that expands to gigabytes
is refused by size, not by running out of memory. Paths are only ever names -
nothing is written anywhere - so a member called ../../etc/passwd is just a
strange name.

THE FORMATS ARE FPC'S OWN (paszlib's zipper and zstream) plus forty lines of
tar, which has no reader there; nothing to install.
}
unit archive_members;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

type
    TArchiveKind = (akNone, akZip, akGzip, akTar, akTarGzip);

    { One file inside an archive. }
    TArchiveMember = record
        { Its path inside the archive, with '/' between folders. }
        Path: string;
        Size: int64;
    end;

    TArchiveMembers = array of TArchiveMember;

    { An archive that could not be read: damaged, truncated, or not what its
      name says. A fault rather than an answer, so the wizard logs it and
      points at the log. }
    EArchiveError = class(EDataSourceFault);

const
    { The largest member this will inflate. Larger than the download cap on
      purpose - compression is real - and small enough that a bomb is refused
      before it fills memory. }
    MaxMemberBytes = 256 * 1024 * 1024;
    { The most members listed from one archive: a dataset of a million tiny
      files is not browsed in a list. }
    MaxListedMembers = 10000;

{ What kind of archive a file of this name is, by its extension. }
function ArchiveKindOf(const AName: string): TArchiveKind;

{ Whether a file of this name is an archive this can open. }
function IsArchiveName(const AName: string): boolean;

{ The files in AData, an archive of AKind called AName (a .gz names its one
  member after itself). Folders are not members. Raises EArchiveError, in
  words, for an archive that cannot be read. }
function ListArchive(AKind: TArchiveKind; const AName: string;
    AData: TStream): TArchiveMembers;

{ The bytes of the member APath of AData into ADest. }
procedure ExtractMember(AKind: TArchiveKind; const AName: string;
    AData: TStream; const APath: string; ADest: TStream);

{ The name of the one file a .gz holds: what its header recorded, or its own
  name without the .gz. }
function GzipMemberName(const AName: string; AData: TStream): string;

implementation

uses
    zipper, zstream, StrUtils, Math;

function ArchiveKindOf(const AName: string): TArchiveKind;
var
    Lower: string;
begin
    Lower := LowerCase(Trim(AName));
    if EndsStr('.tar.gz', Lower) or EndsStr('.tgz', Lower) then
        Result := akTarGzip
    else if EndsStr('.zip', Lower) then
        Result := akZip
    else if EndsStr('.tar', Lower) then
        Result := akTar
    else if EndsStr('.gz', Lower) then
        Result := akGzip
    else
        Result := akNone;
end;

function IsArchiveName(const AName: string): boolean;
begin
    Result := ArchiveKindOf(AName) <> akNone;
end;

{ ------------------------------------ gzip ----------------------------------- }

const
    GzipFlagHeaderCrc = 2;
    GzipFlagExtra = 4;
    GzipFlagName = 8;
    GzipFlagComment = 16;

{ Reads a gzip header from AData, leaving it at the compressed bytes, and
  answers the name it recorded, or ''. }
function ReadGzipHeader(AData: TStream): string;
var
    Header: array[0..9] of byte;
    Flags: byte;
    ExtraLength: word;
    C: byte;
begin
    Result := '';
    if AData.Read(Header, SizeOf(Header)) <> SizeOf(Header) then
        raise EArchiveError.Create('The .gz file is too short to be one.');
    if (Header[0] <> 31) or (Header[1] <> 139) or (Header[2] <> 8) then
        raise EArchiveError.Create('The file is named .gz but is not a gzip ' +
            'file, so nothing could be read from it.');
    Flags := Header[3];
    if (Flags and GzipFlagExtra) <> 0 then
    begin
        ExtraLength := 0;
        AData.ReadBuffer(ExtraLength, 2);
        AData.Seek(LEtoN(ExtraLength), soCurrent);
    end;
    if (Flags and GzipFlagName) <> 0 then
        repeat
            C := 0;
            if AData.Read(C, 1) <> 1 then
                Break;
            if C <> 0 then
                Result := Result + Chr(C);
        until C = 0;
    if (Flags and GzipFlagComment) <> 0 then
        repeat
            C := 0;
            if AData.Read(C, 1) <> 1 then
                Break;
        until C = 0;
    if (Flags and GzipFlagHeaderCrc) <> 0 then
        AData.Seek(2, soCurrent);
end;

{ Inflates the deflate stream at AData's position into ADest, refusing more
  than MaxMemberBytes. }
procedure Inflate(AData, ADest: TStream);
var
    Inflater: TDecompressionStream;
    Buffer: array[0..65535] of byte;
    Got: longint;
    Total: int64;
begin
    Total := 0;
    Inflater := TDecompressionStream.Create(AData, True);
    try
        try
            repeat
                Got := Inflater.Read(Buffer, SizeOf(Buffer));
                if Got > 0 then
                begin
                    Inc(Total, Got);
                    if Total > MaxMemberBytes then
                        raise EArchiveError.Create(Format('The archive expands ' +
                            'to more than %d MB, which is more than Fit will ' +
                            'unpack.', [MaxMemberBytes div (1024 * 1024)]));
                    ADest.WriteBuffer(Buffer, Got);
                end;
            until Got <= 0;
        except
            on E: EArchiveError do
                raise;
            on E: Exception do
                raise EArchiveError.Create('The compressed data is damaged ' +
                    '(' + E.Message + '), so nothing could be read from it.');
        end;
    finally
        Inflater.Free;
    end;
end;

function GzipMemberName(const AName: string; AData: TStream): string;
begin
    AData.Position := 0;
    Result := ExtractFileName(ReadGzipHeader(AData));
    if Result = '' then
    begin
        Result := ExtractFileName(AName);
        if EndsText('.gz', Result) then
            SetLength(Result, Length(Result) - 3);
    end;
end;

{ AData's single member, inflated. }
function Gunzipped(AData: TStream): TMemoryStream;
begin
    Result := TMemoryStream.Create;
    try
        AData.Position := 0;
        ReadGzipHeader(AData);
        Inflate(AData, Result);
        Result.Position := 0;
    except
        Result.Free;
        raise;
    end;
end;

{ ------------------------------------ tar ------------------------------------ }

const
    TarBlock = 512;

{ An octal number from the header field of ALength bytes at AFrom. }
function TarOctal(const AHeader: array of byte; AFrom, ALength: longint): int64;
var
    i: longint;
begin
    Result := 0;
    for i := AFrom to AFrom + ALength - 1 do
        if Chr(AHeader[i]) in ['0'..'7'] then
            Result := Result * 8 + (AHeader[i] - Ord('0'))
        else if (AHeader[i] = 0) or (Chr(AHeader[i]) = ' ') then
        begin
            if Result > 0 then
                Break;
        end
        else
            Break;
end;

{ The zero-terminated text of the header field of ALength bytes at AFrom. }
function TarText(const AHeader: array of byte; AFrom, ALength: longint): string;
var
    i: longint;
begin
    Result := '';
    for i := AFrom to AFrom + ALength - 1 do
    begin
        if AHeader[i] = 0 then
            Break;
        Result := Result + Chr(AHeader[i]);
    end;
end;

type
    TTarVisit = function(const APath: string; ASize: int64; AData: TStream): boolean
        of object;

    { Walks a tar's members; one class so a visit can be a method. }
    TTarWalk = class(TObject)
    private
        FMembers: TArchiveMembers;
        FWanted: string;
        FDest: TStream;
        FFound: boolean;
        function List(const APath: string; ASize: int64; AData: TStream): boolean;
        function Extract(const APath: string; ASize: int64; AData: TStream): boolean;
        procedure Walk(AData: TStream; AVisit: TTarVisit);
    end;

procedure TTarWalk.Walk(AData: TStream; AVisit: TTarVisit);
var
    Header: array[0..TarBlock - 1] of byte;
    Name_, Prefix: string;
    Size, Next: int64;
    Kind: char;
begin
    AData.Position := 0;
    while AData.Position + TarBlock <= AData.Size do
    begin
        AData.ReadBuffer(Header, TarBlock);
        //  Two zero blocks end the archive; one is enough to stop reading.
        if Header[0] = 0 then
            Break;
        Name_ := TarText(Header, 0, 100);
        Size := TarOctal(Header, 124, 12);
        Kind := Chr(Header[156]);
        //  ustar keeps the start of a long path in a prefix field.
        if Copy(TarText(Header, 257, 6), 1, 5) = 'ustar' then
        begin
            Prefix := TarText(Header, 345, 155);
            if Prefix <> '' then
                Name_ := Prefix + '/' + Name_;
        end;
        if (Size < 0) or (AData.Position + Size > AData.Size) then
            raise EArchiveError.Create('The tar archive is cut short, so its ' +
                'members could not all be read.');
        Next := AData.Position + ((Size + TarBlock - 1) div TarBlock) * TarBlock;
        //  Ordinary files only: folders, links and the long-name extensions
        //  are not data.
        if (Kind in ['0', #0]) and (Name_ <> '') then
            if not AVisit(Name_, Size, AData) then
                Exit;
        AData.Position := Min(Next, AData.Size);
    end;
end;

function TTarWalk.List(const APath: string; ASize: int64; AData: TStream): boolean;
begin
    SetLength(FMembers, Length(FMembers) + 1);
    FMembers[High(FMembers)].Path := APath;
    FMembers[High(FMembers)].Size := ASize;
    Result := Length(FMembers) < MaxListedMembers;
end;

function TTarWalk.Extract(const APath: string; ASize: int64; AData: TStream): boolean;
begin
    Result := True;
    if APath <> FWanted then
        Exit;
    if ASize > MaxMemberBytes then
        raise EArchiveError.Create(Format('"%s" is %d MB, more than the %d MB ' +
            'Fit will unpack.', [APath, ASize div (1024 * 1024),
            MaxMemberBytes div (1024 * 1024)]));
    if ASize > 0 then
        FDest.CopyFrom(AData, ASize);
    FFound := True;
    Result := False;
end;

{ ------------------------------------ zip ------------------------------------ }

type
    { Feeds the unzipper from memory and keeps what it inflates. }
    TZipFeed = class(TObject)
    private
        FData: TStream;
        FDest: TStream;
        FWanted: string;
        procedure OpenInput(Sender: TObject; var AStream: TStream);
        procedure CreateOutput(Sender: TObject; var AStream: TStream;
            AItem: TFullZipFileEntry);
        procedure DoneOutput(Sender: TObject; var AStream: TStream;
            AItem: TFullZipFileEntry);
    end;

procedure TZipFeed.OpenInput(Sender: TObject; var AStream: TStream);
var
    Copy_: TMemoryStream;
begin
    //  A copy the unzipper may free when it is done, as it does any input.
    Copy_ := TMemoryStream.Create;
    FData.Position := 0;
    Copy_.CopyFrom(FData, FData.Size);
    Copy_.Position := 0;
    AStream := Copy_;
end;

procedure TZipFeed.CreateOutput(Sender: TObject; var AStream: TStream;
    AItem: TFullZipFileEntry);
begin
    AStream := FDest;
end;

procedure TZipFeed.DoneOutput(Sender: TObject; var AStream: TStream;
    AItem: TFullZipFileEntry);
begin
    //  Ours, not the unzipper's: it would free a stream it did not create.
    AStream := nil;
end;

{ An unzipper over FData, its directory read. }
function ExaminedZip(AFeed: TZipFeed): TUnZipper;
begin
    Result := TUnZipper.Create;
    try
        Result.OnOpenInputStream := @AFeed.OpenInput;
        Result.OnCreateStream := @AFeed.CreateOutput;
        Result.OnDoneStream := @AFeed.DoneOutput;
        try
            Result.Examine;
        except
            on E: Exception do
                raise EArchiveError.Create('The zip archive could not be read (' +
                    E.Message + '): it may be damaged, or not a zip archive.');
        end;
    except
        Result.Free;
        raise;
    end;
end;

{ ---------------------------------- either ----------------------------------- }

function ListArchive(AKind: TArchiveKind; const AName: string;
    AData: TStream): TArchiveMembers;
var
    Feed: TZipFeed;
    Zip: TUnZipper;
    Walk: TTarWalk;
    Inner: TMemoryStream;
    i: longint;
begin
    Result := nil;
    case AKind of
        akZip:
        begin
            Feed := TZipFeed.Create;
            try
                Feed.FData := AData;
                Zip := ExaminedZip(Feed);
                try
                    for i := 0 to Zip.Entries.Count - 1 do
                    begin
                        if Zip.Entries[i].IsDirectory then
                            Continue;
                        SetLength(Result, Length(Result) + 1);
                        Result[High(Result)].Path := Zip.Entries[i].ArchiveFileName;
                        Result[High(Result)].Size := Zip.Entries[i].Size;
                        if Length(Result) >= MaxListedMembers then
                            Break;
                    end;
                finally
                    Zip.Free;
                end;
            finally
                Feed.Free;
            end;
        end;
        akGzip:
        begin
            SetLength(Result, 1);
            Result[0].Path := GzipMemberName(AName, AData);
            Result[0].Size := 0;
        end;
        akTar, akTarGzip:
        begin
            Walk := TTarWalk.Create;
            Inner := nil;
            try
                if AKind = akTarGzip then
                begin
                    Inner := Gunzipped(AData);
                    Walk.Walk(Inner, @Walk.List);
                end
                else
                    Walk.Walk(AData, @Walk.List);
                Result := Walk.FMembers;
            finally
                Inner.Free;
                Walk.Free;
            end;
        end;
        else
            raise EArchiveError.Create('"' + AName + '" is not an archive Fit ' +
                'can open.');
    end;
end;

procedure ExtractMember(AKind: TArchiveKind; const AName: string;
    AData: TStream; const APath: string; ADest: TStream);
var
    Feed: TZipFeed;
    Zip: TUnZipper;
    Walk: TTarWalk;
    Inner: TMemoryStream;
    i: longint;
    Found: boolean;
begin
    case AKind of
        akZip:
        begin
            Feed := TZipFeed.Create;
            try
                Feed.FData := AData;
                Feed.FDest := ADest;
                Feed.FWanted := APath;
                Zip := ExaminedZip(Feed);
                try
                    Found := False;
                    for i := 0 to Zip.Entries.Count - 1 do
                        if Zip.Entries[i].ArchiveFileName = APath then
                        begin
                            Found := True;
                            if Zip.Entries[i].Size > MaxMemberBytes then
                                raise EArchiveError.Create(Format('"%s" is %d ' +
                                    'MB, more than the %d MB Fit will unpack.',
                                    [APath, Zip.Entries[i].Size div (1024 * 1024),
                                    MaxMemberBytes div (1024 * 1024)]));
                        end;
                    if not Found then
                        raise EArchiveError.Create('The archive holds no "' +
                            APath + '".');
                    try
                        Zip.UnZipFile(APath);
                    except
                        on E: EArchiveError do
                            raise;
                        on E: Exception do
                            raise EArchiveError.Create('"' + APath + '" could ' +
                                'not be unpacked (' + E.Message + ').');
                    end;
                finally
                    Zip.Free;
                end;
            finally
                Feed.Free;
            end;
        end;
        akGzip:
        begin
            AData.Position := 0;
            ReadGzipHeader(AData);
            Inflate(AData, ADest);
        end;
        akTar, akTarGzip:
        begin
            Walk := TTarWalk.Create;
            Inner := nil;
            try
                Walk.FWanted := APath;
                Walk.FDest := ADest;
                if AKind = akTarGzip then
                begin
                    Inner := Gunzipped(AData);
                    Walk.Walk(Inner, @Walk.Extract);
                end
                else
                    Walk.Walk(AData, @Walk.Extract);
                if not Walk.FFound then
                    raise EArchiveError.Create('The archive holds no "' +
                        APath + '".');
            finally
                Inner.Free;
                Walk.Free;
            end;
        end;
        else
            raise EArchiveError.Create('"' + AName + '" is not an archive Fit ' +
                'can open.');
    end;
end;

end.
