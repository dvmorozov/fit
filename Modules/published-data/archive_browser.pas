// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(An archive in a record's file list, opened like a folder: its
readable members listed, and one of them fetched.)

THE SOURCE KEEPS ONE OF THESE, and hands it every archive its lists hold. An
archive becomes a CONTAINER item; opening it downloads it once, into memory,
and lists the members this build can read (repository_filter) - an archive
inside it becomes a container in turn. Choosing a member hands the wizard that
member's bytes, so what is previewed and imported is the member, named as it is
named in the archive.

WHY ITEMS CARRY A KEY rather than the archive's address: a member's whereabouts
is a chain - this record's archive, the archive inside it, the file inside that
- and the wizard keeps items, not chains. The key names an entry here that
knows its archive's name, where it came from and, once fetched, its bytes.
Fetched bytes are kept for as long as the source is, so going back up and
down an archive downloads it once.
}
unit archive_browser;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, int_web_client, archive_members;

const
    ArchiveRefPrefix = 'archive:';
    MemberRefPrefix = 'member:';

type
    TArchiveBrowser = class(TObject)
    private
        FWeb: IWebClient;
        { One entry per archive met: its name, its address or its parent and
          path, and its bytes once fetched. }
        FEntries: TStringList;
        function EntryKey(const AName, AUrl, AParent, APath: string): string;
        function Bytes(const AKey: string): TStream;
        function NameOf(const AKey: string): string;
    public
        constructor Create(AWeb: IWebClient);
        destructor Destroy; override;
        { AItem - a file in a record - as a container, when it is an archive;
          AItem unchanged otherwise. }
        function AsContainer(const AItem: TDataSourceItem): TDataSourceItem;
        { Whether AItem is an archive or a member this browser made. }
        function Holds(const AItem: TDataSourceItem): boolean;
        { The readable members of the archive AItem, archives among them as
          containers. Raises when none is readable, saying how many there
          were. }
        function Children(const AItem: TDataSourceItem): TDataSourceItems;
        { The bytes of the member AItem, answering its name. }
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
    end;

implementation

uses
    log, repository_filter;

type
    TArchiveEntry = class(TObject)
    public
        Name: string;
        Url: string;
        Parent: string;
        Path: string;
        Data: TMemoryStream;
        destructor Destroy; override;
    end;

destructor TArchiveEntry.Destroy;
begin
    Data.Free;
    inherited Destroy;
end;

constructor TArchiveBrowser.Create(AWeb: IWebClient);
begin
    inherited Create;
    FWeb := AWeb;
    FEntries := TStringList.Create;
    FEntries.OwnsObjects := True;
end;

destructor TArchiveBrowser.Destroy;
begin
    FEntries.Free;
    inherited Destroy;
end;

function TArchiveBrowser.EntryKey(const AName, AUrl, AParent,
    APath: string): string;
var
    i: longint;
    Entry: TArchiveEntry;
begin
    //  THE SAME ARCHIVE MET AGAIN - a list redrawn after Back - is the same
    //  entry, so its bytes are fetched once.
    for i := 0 to FEntries.Count - 1 do
    begin
        Entry := TArchiveEntry(FEntries.Objects[i]);
        if (Entry.Url = AUrl) and (Entry.Parent = AParent) and
            (Entry.Path = APath) then
            Exit(FEntries[i]);
    end;
    Entry := TArchiveEntry.Create;
    Entry.Name := AName;
    Entry.Url := AUrl;
    Entry.Parent := AParent;
    Entry.Path := APath;
    Result := 'a' + IntToStr(FEntries.Count + 1);
    FEntries.AddObject(Result, Entry);
end;

function TArchiveBrowser.NameOf(const AKey: string): string;
var
    i: longint;
begin
    i := FEntries.IndexOf(AKey);
    if i < 0 then
        raise EArchiveError.Create('That archive is no longer open; go back ' +
            'and open its record again.');
    Result := TArchiveEntry(FEntries.Objects[i]).Name;
end;

function TArchiveBrowser.Bytes(const AKey: string): TStream;
var
    i: longint;
    Entry: TArchiveEntry;
    Data: TMemoryStream;
begin
    i := FEntries.IndexOf(AKey);
    if i < 0 then
        raise EArchiveError.Create('That archive is no longer open; go back ' +
            'and open its record again.');
    Entry := TArchiveEntry(FEntries.Objects[i]);
    if Entry.Data = nil then
    begin
        Data := TMemoryStream.Create;
        try
            if Entry.Parent = '' then
                FWeb.Download(Entry.Url, Data)
            else
                ExtractMember(ArchiveKindOf(NameOf(Entry.Parent)),
                    NameOf(Entry.Parent), Bytes(Entry.Parent), Entry.Path, Data);
            WriteLog('Published data: archive ' + Entry.Name + ' holds ' +
                IntToStr(Data.Size) + ' bytes', Debug);
        except
            Data.Free;
            raise;
        end;
        Entry.Data := Data;
    end;
    Entry.Data.Position := 0;
    Result := Entry.Data;
end;

function TArchiveBrowser.AsContainer(const AItem: TDataSourceItem): TDataSourceItem;
begin
    Result := AItem;
    if not AItem.IsLeaf or not IsArchiveName(AItem.FileName) then
        Exit;
    Result.Ref := ArchiveRefPrefix + EntryKey(AItem.FileName, AItem.Ref, '', '');
    Result.IsLeaf := False;
    if Result.Details = '' then
        Result.Details := 'archive - open it to see what it holds';
end;

function TArchiveBrowser.Holds(const AItem: TDataSourceItem): boolean;
begin
    Result := (Pos(ArchiveRefPrefix, AItem.Ref) = 1) or
        (Pos(MemberRefPrefix, AItem.Ref) = 1);
end;

function TArchiveBrowser.Children(const AItem: TDataSourceItem): TDataSourceItems;
var
    Key, Name_: string;
    Members: TArchiveMembers;
    Listed: TDataSourceItems;
    Item: TDataSourceItem;
    i, Dropped: longint;
begin
    Result := nil;
    Key := Copy(AItem.Ref, Length(ArchiveRefPrefix) + 1, MaxInt);
    Name_ := NameOf(Key);
    Members := ListArchive(ArchiveKindOf(Name_), Name_, Bytes(Key));
    Listed := nil;
    for i := 0 to High(Members) do
    begin
        Item := Default(TDataSourceItem);
        Item.Title := Members[i].Path;
        Item.FileName := ExtractFileName(StringReplace(Members[i].Path, '\', '/',
            [rfReplaceAll]));
        if Pos('/', Item.FileName) > 0 then
            Item.FileName := Copy(Item.FileName,
                LastDelimiter('/', Item.FileName) + 1, MaxInt);
        Item.Id := AItem.Id + '/' + Members[i].Path;
        Item.Size := Members[i].Size;
        if IsArchiveName(Item.FileName) then
        begin
            //  AN ARCHIVE IN AN ARCHIVE is a folder too.
            Item.Ref := ArchiveRefPrefix + EntryKey(Item.FileName, '', Key,
                Members[i].Path);
            Item.IsLeaf := False;
            Item.Details := 'archive - open it to see what it holds';
        end
        else
        begin
            Item.Ref := MemberRefPrefix + Key + ':' + Members[i].Path;
            Item.IsLeaf := True;
        end;
        SetLength(Listed, Length(Listed) + 1);
        Listed[High(Listed)] := Item;
    end;
    Result := UsableItems(Listed, Dropped);
    WriteLog('Published data: archive ' + Name_ + ' holds ' +
        IntToStr(Length(Listed)) + ' file(s), ' + IntToStr(Dropped) +
        ' left out as unreadable', Debug);
    if Length(Listed) = 0 then
        raise EDataSourceError.Create('The archive ' + Name_ + ' holds no ' +
            'files.');
    if Length(Result) = 0 then
        raise EDataSourceError.Create(NothingUsableMessage(Length(Listed),
            'files in the archive ' + Name_));
end;

function TArchiveBrowser.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
var
    Rest, Key, Path: string;
    P: longint;
begin
    Rest := Copy(AItem.Ref, Length(MemberRefPrefix) + 1, MaxInt);
    P := Pos(':', Rest);
    Key := Copy(Rest, 1, P - 1);
    Path := Copy(Rest, P + 1, MaxInt);
    ExtractMember(ArchiveKindOf(NameOf(Key)), NameOf(Key), Bytes(Key), Path,
        ADest);
    Result := AItem.FileName;
end;

end.
