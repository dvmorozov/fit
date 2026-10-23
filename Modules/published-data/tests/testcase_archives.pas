// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Archives opened like folders: what is listed, what is fetched, and
what is refused.)

Every archive here is built in memory by the test - a zip by FPC's own zipper,
a gzip and a tar byte by byte - so the reader is held to the formats, not to a
file somebody happened to have.
}
unit testcase_archives;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, archive_members, archive_browser, doi_source,
    int_web_client, mock_web_client;

type
    TArchiveMembersTest = class(TTestCase)
    published
        procedure TheKindIsTheExtension;
        procedure AZipListsItsFilesAndNotItsFolders;
        procedure AZipMemberIsUnpacked;
        procedure AMemberNotThereIsRefusedByName;
        procedure AGzipHoldsTheFileItsHeaderNames;
        procedure AGzipWithNoNameHoldsItsOwnNameWithoutGz;
        procedure AGzipIsUnpacked;
        procedure ATarListsAndUnpacksItsFiles;
        procedure ATarGzipIsATarInsideAGzip;
        procedure ADamagedZipIsRefusedInWords;
        procedure AFileNamedGzThatIsNotOneIsRefused;
        procedure ACutShortTarIsRefused;
        procedure DamagedCompressedDataIsRefusedInWords;
        procedure AMemberNotInATarIsRefusedByName;
        procedure AFileThatIsNoArchiveIsRefused;
        procedure ATarGzipMemberNotThereIsRefused;
    end;

    { The same archives in a record, through the DOI source. }
    TArchiveRecordTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FSource: TDoiSource;
        function RecordFiles: TDataSourceItems;
        procedure PublishRecordHolding(const AName, ABody: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnArchiveInARecordIsAFolder;
        procedure OpeningItListsOnlyWhatCanBeRead;
        procedure AMemberIsFetchedAsItsOwnFile;
        procedure TheArchiveIsDownloadedOnce;
        procedure AnArchiveInAnArchiveIsAFolderToo;
        procedure AnArchiveHoldingNothingReadableSaysHowMuch;
    end;

{ Bytes as a string, which is how the mock web client holds a reply. }
function StreamText(AStream: TStream): string;
{ A zip of the names and contents given in pairs. }
function ZipOf(const ANamesAndTexts: array of string): string;
{ A gzip of ATEXT, recording AName in its header when it is not ''. }
function GzipOf(const AName, AText: string): string;
{ A tar of the names and contents given in pairs. }
function TarOf(const ANamesAndTexts: array of string): string;

implementation

uses
    zipper, zstream, crc, data_loader_registration, data_source_registration,
    published_data_module;

function StreamText(AStream: TStream): string;
begin
    Result := '';
    SetLength(Result, AStream.Size);
    AStream.Position := 0;
    if AStream.Size > 0 then
        AStream.ReadBuffer(Result[1], AStream.Size);
end;

function ZipOf(const ANamesAndTexts: array of string): string;
var
    Zip: TZipper;
    Streams: TList;
    Each: TStringStream;
    Output: TMemoryStream;
    i: longint;
begin
    Zip := TZipper.Create;
    Streams := TList.Create;
    Output := TMemoryStream.Create;
    try
        i := 0;
        while i < High(ANamesAndTexts) do
        begin
            Each := TStringStream.Create(ANamesAndTexts[i + 1]);
            Streams.Add(Each);
            Zip.Entries.AddFileEntry(Each, ANamesAndTexts[i]);
            Inc(i, 2);
        end;
        Zip.SaveToStream(Output);
        Result := StreamText(Output);
    finally
        for i := 0 to Streams.Count - 1 do
            TObject(Streams[i]).Free;
        Streams.Free;
        Output.Free;
        Zip.Free;
    end;
end;

function GzipOf(const AName, AText: string): string;
const
    Plain: array[0..9] of byte = (31, 139, 8, 0, 0, 0, 0, 0, 0, 255);
var
    Output: TMemoryStream;
    Deflater: TCompressionStream;
    Header: array[0..9] of byte;
    Trailer: array[0..1] of cardinal;
    NameZ: string;
begin
    Output := TMemoryStream.Create;
    try
        Move(Plain, Header, SizeOf(Header));
        //  FNAME: the original file's name, zero-terminated, after the header.
        if AName <> '' then
            Header[3] := 8;
        Output.WriteBuffer(Header, SizeOf(Header));
        NameZ := AName + #0;
        if AName <> '' then
            Output.WriteBuffer(NameZ[1], Length(NameZ));
        Deflater := TCompressionStream.Create(clDefault, Output, True);
        try
            if AText <> '' then
                Deflater.WriteBuffer(AText[1], Length(AText));
        finally
            Deflater.Free;
        end;
        Trailer[0] := NtoLE(crc32(crc32(0, nil, 0), PByte(PChar(AText)),
            Length(AText)));
        Trailer[1] := NtoLE(cardinal(Length(AText)));
        Output.WriteBuffer(Trailer, SizeOf(Trailer));
        Result := StreamText(Output);
    finally
        Output.Free;
    end;
end;

function TarOf(const ANamesAndTexts: array of string): string;
var
    i, Pad: longint;
    Header: string;
    Size: string;
begin
    Result := '';
    i := 0;
    while i < High(ANamesAndTexts) do
    begin
        Header := StringOfChar(#0, 512);
        Move(ANamesAndTexts[i][1], Header[1], Length(ANamesAndTexts[i]));
        Size := OctStr(Length(ANamesAndTexts[i + 1]), 11);
        Move(Size[1], Header[125], 11);
        Header[157] := '0';
        Move(PChar('ustar')^, Header[258], 5);
        Result := Result + Header + ANamesAndTexts[i + 1];
        Pad := (512 - Length(ANamesAndTexts[i + 1]) mod 512) mod 512;
        Result := Result + StringOfChar(#0, Pad);
        Inc(i, 2);
    end;
    Result := Result + StringOfChar(#0, 1024);
end;

function StreamOf(const AText: string): TStringStream;
begin
    Result := TStringStream.Create(AText);
end;

{ --------------------------------- members ----------------------------------- }

procedure TArchiveMembersTest.TheKindIsTheExtension;
begin
    AssertTrue(ArchiveKindOf('data.zip') = akZip);
    AssertTrue(ArchiveKindOf('DATA.ZIP') = akZip);
    AssertTrue(ArchiveKindOf('pattern.xy.gz') = akGzip);
    AssertTrue(ArchiveKindOf('set.tar') = akTar);
    AssertTrue(ArchiveKindOf('set.tar.gz') = akTarGzip);
    AssertTrue(ArchiveKindOf('set.tgz') = akTarGzip);
    AssertTrue(ArchiveKindOf('pattern.xy') = akNone);
    AssertTrue(IsArchiveName('a.zip'));
    AssertFalse(IsArchiveName('a.pdf'));
end;

procedure TArchiveMembersTest.AZipListsItsFilesAndNotItsFolders;
var
    Data: TStringStream;
    Members: TArchiveMembers;
begin
    Data := StreamOf(ZipOf(['data/quartz.xy', '1 2', 'readme.txt', 'hello']));
    try
        Members := ListArchive(akZip, 'set.zip', Data);
        AssertEquals(2, Length(Members));
        AssertEquals('data/quartz.xy', Members[0].Path);
        AssertEquals(3, Members[0].Size);
    finally
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AZipMemberIsUnpacked;
var
    Data: TStringStream;
    Dest: TStringStream;
begin
    Data := StreamOf(ZipOf(['a.xy', '1 2', 'b.xy', '3 4']));
    Dest := TStringStream.Create('');
    try
        ExtractMember(akZip, 'set.zip', Data, 'b.xy', Dest);
        AssertEquals('3 4', Dest.DataString);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AMemberNotThereIsRefusedByName;
var
    Data: TStringStream;
    Dest: TStringStream;
    Message_: string;
begin
    Data := StreamOf(ZipOf(['a.xy', '1 2']));
    Dest := TStringStream.Create('');
    try
        Message_ := '';
        try
            ExtractMember(akZip, 'set.zip', Data, 'gone.xy', Dest);
        except
            on E: EArchiveError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('gone.xy', Message_) > 0);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AGzipHoldsTheFileItsHeaderNames;
var
    Data: TStringStream;
    Members: TArchiveMembers;
begin
    Data := StreamOf(GzipOf('pattern.xy', '1 2'));
    try
        Members := ListArchive(akGzip, 'download.gz', Data);
        AssertEquals(1, Length(Members));
        AssertEquals('pattern.xy', Members[0].Path);
    finally
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AGzipWithNoNameHoldsItsOwnNameWithoutGz;
var
    Data: TStringStream;
begin
    Data := StreamOf(GzipOf('', '1 2'));
    try
        AssertEquals('spectrum.csv', GzipMemberName('spectrum.csv.gz', Data));
    finally
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AGzipIsUnpacked;
var
    Data, Dest: TStringStream;
begin
    Data := StreamOf(GzipOf('pattern.xy', '10 100' + LineEnding + '11 120'));
    Dest := TStringStream.Create('');
    try
        ExtractMember(akGzip, 'pattern.xy.gz', Data, 'pattern.xy', Dest);
        AssertEquals('10 100' + LineEnding + '11 120', Dest.DataString);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.ATarListsAndUnpacksItsFiles;
var
    Data, Dest: TStringStream;
    Members: TArchiveMembers;
begin
    Data := StreamOf(TarOf(['run1/a.xy', '1 2', 'run1/b.xy', '3 4 5 6']));
    Dest := TStringStream.Create('');
    try
        Members := ListArchive(akTar, 'set.tar', Data);
        AssertEquals(2, Length(Members));
        AssertEquals('run1/b.xy', Members[1].Path);
        AssertEquals(7, Members[1].Size);
        ExtractMember(akTar, 'set.tar', Data, 'run1/b.xy', Dest);
        AssertEquals('3 4 5 6', Dest.DataString);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.ATarGzipIsATarInsideAGzip;
var
    Data, Dest: TStringStream;
    Members: TArchiveMembers;
begin
    Data := StreamOf(GzipOf('', TarOf(['a.xy', '1 2'])));
    Dest := TStringStream.Create('');
    try
        Members := ListArchive(akTarGzip, 'set.tgz', Data);
        AssertEquals(1, Length(Members));
        ExtractMember(akTarGzip, 'set.tgz', Data, 'a.xy', Dest);
        AssertEquals('1 2', Dest.DataString);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.ADamagedZipIsRefusedInWords;
var
    Data: TStringStream;
    Message_: string;
begin
    Data := StreamOf('PK this is not a zip at all');
    try
        Message_ := '';
        try
            ListArchive(akZip, 'set.zip', Data);
        except
            on E: EArchiveError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('could not be read', Message_) > 0);
    finally
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AFileNamedGzThatIsNotOneIsRefused;
var
    Data: TStringStream;
    Message_: string;
begin
    Data := StreamOf('<html>a login page</html>');
    try
        Message_ := '';
        try
            ListArchive(akGzip, 'data.gz', Data);
        except
            on E: EArchiveError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('not a gzip file', Message_) > 0);
    finally
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.ACutShortTarIsRefused;
var
    Text: string;
    Data: TStringStream;
    Message_: string;
begin
    Text := TarOf(['a.xy', StringOfChar('1', 2000)]);
    Data := StreamOf(Copy(Text, 1, 1024));
    try
        Message_ := '';
        try
            ListArchive(akTar, 'set.tar', Data);
        except
            on E: EArchiveError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('cut short', Message_) > 0);
    finally
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.DamagedCompressedDataIsRefusedInWords;
var
    Data, Dest: TStringStream;
    Text, Message_: string;
begin
    //  A gzip header, then bytes that are no deflate stream.
    Text := Copy(GzipOf('a.xy', StringOfChar('1', 200)), 1, 16) +
        StringOfChar(#255, 40);
    Data := StreamOf(Text);
    Dest := TStringStream.Create('');
    try
        Message_ := '';
        try
            ExtractMember(akGzip, 'a.xy.gz', Data, 'a.xy', Dest);
        except
            on E: EArchiveError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('damaged', Message_) > 0);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AMemberNotInATarIsRefusedByName;
var
    Data, Dest: TStringStream;
    Message_: string;
begin
    Data := StreamOf(TarOf(['a.xy', '1 2']));
    Dest := TStringStream.Create('');
    try
        Message_ := '';
        try
            ExtractMember(akTar, 'set.tar', Data, 'gone.xy', Dest);
        except
            on E: EArchiveError do
                Message_ := E.Message;
        end;
        AssertTrue(Message_, Pos('gone.xy', Message_) > 0);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.AFileThatIsNoArchiveIsRefused;
var
    Data, Dest: TStringStream;
    Raised: longint;
begin
    Data := StreamOf('plain');
    Dest := TStringStream.Create('');
    try
        Raised := 0;
        try
            ListArchive(akNone, 'a.xy', Data);
        except
            on E: EArchiveError do
                Inc(Raised);
        end;
        try
            ExtractMember(akNone, 'a.xy', Data, 'a', Dest);
        except
            on E: EArchiveError do
                Inc(Raised);
        end;
        AssertEquals('both refuse', 2, Raised);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

procedure TArchiveMembersTest.ATarGzipMemberNotThereIsRefused;
var
    Data, Dest: TStringStream;
    Raised: boolean;
begin
    Data := StreamOf(GzipOf('', TarOf(['a.xy', '1 2'])));
    Dest := TStringStream.Create('');
    try
        Raised := False;
        try
            ExtractMember(akTarGzip, 'set.tgz', Data, 'b.xy', Dest);
        except
            on E: EArchiveError do
                Raised := True;
        end;
        AssertTrue(Raised);
    finally
        Dest.Free;
        Data.Free;
    end;
end;

{ --------------------------------- records ----------------------------------- }

const
    HandleReply = '{"values":[{"type":"URL","data":{"value":' +
        '"https://zenodo.org/record/55"}}]}';

procedure TArchiveRecordTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    RegisterPublishedDataModule;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FSource := TDoiSource.Create(FWeb);
end;

procedure TArchiveRecordTest.TearDown;
begin
    FreeAndNil(FSource);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

procedure TArchiveRecordTest.PublishRecordHolding(const AName, ABody: string);
begin
    FMockObject.Reply('doi.org/api/handles/', HandleReply);
    FMockObject.Reply('zenodo.org/api/records/55',
        '{"files":[{"key":"' + AName + '","size":10,"links":{"self":' +
        '"https://zenodo.org/files/' + AName + '"}},' +
        '{"key":"paper.pdf","size":10,"links":{"self":"https://zenodo.org/files/p"}}]}');
    FMockObject.Reply('zenodo.org/files/' + AName, ABody);
end;

function TArchiveRecordTest.RecordFiles: TDataSourceItems;
var
    Query: TDataSourceQuery;
begin
    Query := nil;
    Query := WithQueryValue(Query, DoiFieldId, '10.5281/zenodo.55');
    Result := FSource.Children(FSource.Search(Query)[0]);
end;

procedure TArchiveRecordTest.AnArchiveInARecordIsAFolder;
var
    Files: TDataSourceItems;
begin
    PublishRecordHolding('data.zip', ZipOf(['a.xy', '1 2']));
    Files := RecordFiles;
    AssertEquals('the pdf is left out', 1, Length(Files));
    AssertEquals('data.zip', Files[0].Title);
    AssertFalse('opened, not read', Files[0].IsLeaf);
end;

procedure TArchiveRecordTest.OpeningItListsOnlyWhatCanBeRead;
var
    Members: TDataSourceItems;
begin
    PublishRecordHolding('data.zip', ZipOf(['run/a.xy', '1 2',
        'run/photo.jpg', 'x', 'notes.pdf', 'y']));
    Members := FSource.Children(RecordFiles[0]);
    AssertEquals(1, Length(Members));
    AssertEquals('named as the archive names it', 'run/a.xy', Members[0].Title);
    AssertEquals('and read by its own name', 'a.xy', Members[0].FileName);
    AssertTrue(Members[0].IsLeaf);
end;

procedure TArchiveRecordTest.AMemberIsFetchedAsItsOwnFile;
var
    Members: TDataSourceItems;
    Dest: TStringStream;
begin
    PublishRecordHolding('data.zip', ZipOf(['a.xy', '1 2', 'b.xy', '3 4']));
    Members := FSource.Children(RecordFiles[0]);
    Dest := TStringStream.Create('');
    try
        AssertEquals('b.xy', FSource.Download(Members[1], Dest));
        AssertEquals('3 4', Dest.DataString);
    finally
        Dest.Free;
    end;
end;

procedure TArchiveRecordTest.TheArchiveIsDownloadedOnce;
var
    Archive: TDataSourceItem;
    Members: TDataSourceItems;
    Dest: TStringStream;
begin
    PublishRecordHolding('data.zip', ZipOf(['a.xy', '1 2']));
    Archive := RecordFiles[0];
    FSource.Children(Archive);
    //  Back up, and in again: the same archive, met again.
    Archive := RecordFiles[0];
    Members := FSource.Children(Archive);
    Dest := TStringStream.Create('');
    try
        FSource.Download(Members[0], Dest);
    finally
        Dest.Free;
    end;
    AssertEquals('fetched once, however often it is opened', 1,
        FMockObject.Log.CountOf('Download(https://zenodo.org/files/data.zip)'));
end;

procedure TArchiveRecordTest.AnArchiveInAnArchiveIsAFolderToo;
var
    Outer, Inner: TDataSourceItems;
    Dest: TStringStream;
begin
    PublishRecordHolding('all.zip', ZipOf(['inner.tar', TarOf(['deep.xy', '5 6'])]));
    Outer := FSource.Children(RecordFiles[0]);
    AssertEquals(1, Length(Outer));
    AssertFalse('the inner archive is a folder', Outer[0].IsLeaf);
    Inner := FSource.Children(Outer[0]);
    AssertEquals('deep.xy', Inner[0].FileName);
    Dest := TStringStream.Create('');
    try
        FSource.Download(Inner[0], Dest);
        AssertEquals('5 6', Dest.DataString);
    finally
        Dest.Free;
    end;
end;

procedure TArchiveRecordTest.AnArchiveHoldingNothingReadableSaysHowMuch;
var
    Message_: string;
begin
    PublishRecordHolding('pictures.zip', ZipOf(['a.jpg', 'x', 'b.png', 'y']));
    Message_ := '';
    try
        FSource.Children(RecordFiles[0]);
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('Found 2 files in the archive pictures.zip',
        Message_) > 0);
end;

initialization
    RegisterTest('unit', TArchiveMembersTest);
    RegisterTest('unit', TArchiveRecordTest);
end.
