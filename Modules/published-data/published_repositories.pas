// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What Zenodo and figshare are asked, and what is made of their
answers - for the search and for the catalog alike.)

ONE PLACE FOR BOTH SOURCES. The DOI source's search and the catalog's kinds of
data ask the same two services the same kinds of question - datasets matching
something, the files of a record - and filter the answers by one rule
(repository_filter). Written twice, the two would disagree the first time
either service changed what it publishes.

WHAT EACH SERVICE IS ASKED, measured against the live services (2026-10-20):

  Zenodo  GET /api/records?type=dataset&q=..&size=25&page=..
          A hit carries its files, so a record with nothing readable is left
          out at no cost, and the query itself asks for records holding a file
          of a kind this build reads: files.entries.key:(*.xy OR *.csv ...).
          NOT file_type, which knows only the commonest types - file_type=xrdml
          finds nothing where the key query finds 32 records. 25 is the most a
          page may hold without an account; 50 is refused with a 400.
  figshare POST /v2/articles/search with a JSON query - search_for, which
          takes ':extension: xrdml OR :extension: xye' for a kind of file -
          item_type 3 = dataset. Its hits carry NO file list, so each record's
          files are fetched (GET /v2/articles/ID) before it is listed, which
          is why its page is small: each record is one more request.

A SERVICE THAT FAILS DOES NOT HIDE THE OTHER. Each is asked on its own; a
failure is logged and becomes a row saying so, and only both failing is a
refusal. A page of either ends in a "More" row while the service has more.
}
unit published_repositories;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpjson, data_source, int_web_client;

const
    ZenodoPrefix = 'zenodo:';
    FigsharePrefix = 'figshare:';
    { A row that opens the next page of a list. }
    MorePrefix = 'more:';
    { A row that says a service could not be asked, and opens to the reason. }
    NoticePrefix = 'notice:';

    { How many records a page of each asks for. figshare's is smaller because
      each of its records costs a request of its own (the header says why). }
    { Zenodo's most for a request without an account: a larger page is
      refused outright (400, "Page size cannot be greater than 25"). }
    ZenodoPageSize = 25;
    FigsharePageSize = 10;

type
    { What a list of datasets is a list of: words a user typed, or one of the
      catalog's kinds of data - the files a reader of this build reads. }
    TDatasetQueryKind = (dqWords, dqKind);

    TDatasetQuery = record
        Kind: TDatasetQueryKind;
        { The words typed, or the kind's name as the catalog shows it. }
        Words: string;
        { For a kind: its extensions, lower case, no dot, comma-separated -
          'xrdml,xye'. }
        Extensions: string;
    end;

    TPublishedRepositories = class(TObject)
    private
        FWeb: IWebClient;
        function ZenodoUrl(const AQuery: TDatasetQuery; APage: longint): string;
        function FigshareBody(const AQuery: TDatasetQuery; APage: longint): string;
        { One figshare hit as a record, its files asked for; ACount says how
          many of them this build reads. }
        function FigshareRecord(AHit: TJSONData;
            out ACount: longint): TDataSourceItem;
    public
        constructor Create(AWeb: IWebClient);

        { One page of Zenodo's datasets for AQuery, those holding a readable
          file; AFound says how many records the page held before filtering,
          ATotal how many matched in all. }
        function ZenodoDatasets(const AQuery: TDatasetQuery; APage: longint;
            out AFound, ATotal: longint): TDataSourceItems;
        { One page of figshare's, filtered the same way; AFound as above, and
          AMore whether figshare may have another page. }
        function FigshareDatasets(const AQuery: TDatasetQuery; APage: longint;
            out AFound: longint; out AMore: boolean): TDataSourceItems;
        { A page of both, each service's failure a notice row rather than the
          end of the list, each ending in a More row while it has more. Raises
          EDataSourceFault when both fail, and EDataSourceError, saying how
          much was found, when nothing readable is left. }
        function Datasets(const AQuery: TDatasetQuery;
            AZenodoPage, AFigsharePage: longint): TDataSourceItems;
        { What a More row opens: the next page of the one service it names. }
        function MoreDatasets(const ARef: string): TDataSourceItems;
        { The files of a record, as the record lists them - every one, for the
          caller to filter. }
        function RecordFiles(const ARef: string): TDataSourceItems;
    end;

{ The services this module searches, in words: 'Zenodo and figshare'. What every
  hint names, so a service added here is named everywhere at once. }
function SearchedRepositories: string;

{ ATEXT as a JSON string literal, quotes included. }
function JsonString(const AText: string): string;

{ A More row's reference, and the parts it is read back into. }
function MoreRef(const ARepository: string; APage: longint;
    const AQuery: TDatasetQuery): string;
function ReadMoreRef(const ARef: string; out ARepository: string;
    out APage: longint; out AQuery: TDatasetQuery): boolean;

{ A reply parsed, or a fault naming what was being read. The caller frees
  it. }
function ParsedReply(const AText, AWhat: string): TJSONData;

{ The Zenodo query asking for records holding a file with one of AEXTENSIONS
  (comma-separated): files.entries.key:(*.xrdml OR *.xye). }
function ZenodoFilesQuery(const AExtensions: string): string;

{ The figshare query asking the same: ':extension: xrdml OR :extension: xye'. }
function FigshareFilesQuery(const AExtensions: string): string;

{ Every extension this build reads, and the archives it opens, as a kind's
  extensions are written - what a word search asks for. }
function SearchedExtensions: string;

implementation

uses
    jsonparser, StrUtils, log, web_client, data_loader_registry,
    repository_filter;

const
    RepositoryNames: array[0..1] of string = ('Zenodo', 'figshare');

function SearchedRepositories: string;
var
    i: longint;
begin
    Result := '';
    for i := 0 to High(RepositoryNames) do
    begin
        if i > 0 then
            if i = High(RepositoryNames) then
                Result := Result + ' and '
            else
                Result := Result + ', ';
        Result := Result + RepositoryNames[i];
    end;
end;

function JsonString(const AText: string): string;
begin
    Result := '"' + StringToJSONString(AText) + '"';
end;

function ParsedReply(const AText, AWhat: string): TJSONData;
begin
    //  A SERVICE THAT ANSWERS WITH SOMETHING ELSE - a maintenance page, a
    //  login form, an error in HTML - would otherwise reach the user as a
    //  parser's complaint about a character at a position.
    try
        Result := GetJSON(AText);
    except
        on E: Exception do
            raise EDataSourceFault.Create('The answer to ' + AWhat +
                ' was not what this program expected, so nothing could be ' +
                'read from it. The service may be having trouble, or may ' +
                'have changed what it publishes.');
    end;
    if Result = nil then
        raise EDataSourceFault.Create('The answer to ' + AWhat + ' was empty.');
end;

{ The text at APath under ANode, or '' when it is not there - a service that
  leaves a field out is not a reason to stop reading the rest. }
function TextAt(ANode: TJSONData; const APath: string): string;
var
    Found: TJSONData;
begin
    Result := '';
    if ANode = nil then
        Exit;
    Found := ANode.FindPath(APath);
    if (Found <> nil) and not Found.IsNull then
        Result := Found.AsString;
end;

function MoreRef(const ARepository: string; APage: longint;
    const AQuery: TDatasetQuery): string;
begin
    Result := MorePrefix + ARepository + ':' + IntToStr(APage) + ':';
    if AQuery.Kind = dqKind then
        Result := Result + 'k:' + AQuery.Extensions + ':' + AQuery.Words
    else
        Result := Result + 'w::' + AQuery.Words;
end;

function ReadMoreRef(const ARef: string; out ARepository: string;
    out APage: longint; out AQuery: TDatasetQuery): boolean;
var
    Parts: TStringArray;
    Rest: string;
    i: longint;
begin
    Result := False;
    ARepository := '';
    APage := 0;
    AQuery := Default(TDatasetQuery);
    if not StartsStr(MorePrefix, ARef) then
        Exit;
    Rest := Copy(ARef, Length(MorePrefix) + 1, MaxInt);
    //  repository:page:kind:extensions:words - the words last, since they
    //  may hold colons themselves.
    Parts := Rest.Split([':']);
    if Length(Parts) < 5 then
        Exit;
    ARepository := Parts[0];
    APage := StrToIntDef(Parts[1], 0);
    if Parts[2] = 'k' then
        AQuery.Kind := dqKind
    else
        AQuery.Kind := dqWords;
    AQuery.Extensions := Parts[3];
    AQuery.Words := Parts[4];
    for i := 5 to High(Parts) do
        AQuery.Words := AQuery.Words + ':' + Parts[i];
    Result := APage > 0;
end;

constructor TPublishedRepositories.Create(AWeb: IWebClient);
begin
    inherited Create;
    FWeb := AWeb;
end;

const
    ArchiveExtensions = 'zip,gz,tar,tgz';

function SearchedExtensions: string;
var
    Extensions: TStringArray;
    i: longint;
    Ext: string;
begin
    Result := '';
    Extensions := RegisteredExtensions.Split([';']);
    for i := 0 to High(Extensions) do
    begin
        Ext := LowerCase(Trim(Extensions[i]));
        if StartsStr('.', Ext) then
            Delete(Ext, 1, 1);
        if Ext <> '' then
            Result := Result + Ext + ',';
    end;
    Result := Result + ArchiveExtensions;
end;

function ZenodoFilesQuery(const AExtensions: string): string;
var
    Parts: TStringArray;
    i: longint;
begin
    Result := '';
    Parts := AExtensions.Split([','], TStringSplitOptions.ExcludeEmpty);
    for i := 0 to High(Parts) do
    begin
        if Result <> '' then
            Result := Result + ' OR ';
        Result := Result + '*.' + Trim(Parts[i]);
    end;
    Result := 'files.entries.key:(' + Result + ')';
end;

function FigshareFilesQuery(const AExtensions: string): string;
var
    Parts: TStringArray;
    i: longint;
begin
    Result := '';
    Parts := AExtensions.Split([','], TStringSplitOptions.ExcludeEmpty);
    for i := 0 to High(Parts) do
    begin
        if Result <> '' then
            Result := Result + ' OR ';
        Result := Result + ':extension: ' + Trim(Parts[i]);
    end;
end;

function TPublishedRepositories.ZenodoUrl(const AQuery: TDatasetQuery;
    APage: longint): string;
var
    Query, Order: string;
begin
    //  WORDS AND READABLE FILES, or a kind's files alone, newest first: a kind
    //  has no words to rank by.
    if AQuery.Kind = dqKind then
    begin
        Query := ZenodoFilesQuery(AQuery.Extensions);
        Order := '&sort=newest';
    end
    else
    begin
        Query := '(' + AQuery.Words + ') AND ' +
            ZenodoFilesQuery(SearchedExtensions);
        Order := '';
    end;
    Result := 'https://zenodo.org/api/records?type=dataset&size=' +
        IntToStr(ZenodoPageSize) + '&page=' + IntToStr(APage) + Order + '&q=' +
        UrlEncoded(Query);
end;

{ The usable files among a record's names, counted, and their bytes added up
  in ABYTES - what the list's Size column shows for a record: what choosing
  its data would download, not the record's papers and pictures. }
function UsableCount(AFiles: TJSONData; const ANameField: string;
    out ABytes: int64): longint;
var
    i: longint;
begin
    Result := 0;
    ABytes := 0;
    if AFiles = nil then
        Exit;
    for i := 0 to AFiles.Count - 1 do
        if IsUsableFileName(TextAt(AFiles.Items[i], ANameField)) then
        begin
            Inc(Result);
            Inc(ABytes, StrToInt64Def(TextAt(AFiles.Items[i], 'size'), 0));
        end;
end;

function ReadableDetails(const ARepository, ADate: string; ACount: longint): string;
begin
    Result := ARepository;
    if ADate <> '' then
        Result := Result + ', ' + Copy(ADate, 1, 10);
    Result := Result + ', ' + IntToStr(ACount) + ' readable file';
    if ACount <> 1 then
        Result := Result + 's';
end;

function TPublishedRepositories.ZenodoDatasets(const AQuery: TDatasetQuery;
    APage: longint; out AFound, ATotal: longint): TDataSourceItems;
var
    Root, Hits, Entry: TJSONData;
    i, Count: longint;
    Bytes: int64;
    Item: TDataSourceItem;
    Title: string;
begin
    Result := nil;
    AFound := 0;
    ATotal := 0;
    Root := ParsedReply(FWeb.GetText(ZenodoUrl(AQuery, APage)),
        'the Zenodo search');
    try
        ATotal := StrToIntDef(TextAt(Root, 'hits.total'), 0);
        Hits := Root.FindPath('hits.hits');
        //  A SEARCH THAT ANSWERS WITHOUT ITS HITS has changed what it
        //  publishes; read as "nothing found" it would send the user to try
        //  other words. Naming the field is what makes the log diagnosable.
        if (Hits = nil) or (Hits.JSONType <> jtArray) then
            raise EDataSourceFault.Create('The answer to the Zenodo search ' +
                'has no hits.hits list, so its records could not be read. The ' +
                'service may have changed what it publishes.');
        AFound := Hits.Count;
        for i := 0 to Hits.Count - 1 do
        begin
            Entry := Hits.Items[i];
            Count := UsableCount(Entry.FindPath('files'), 'key', Bytes);
            //  NOTHING READABLE, NOT LISTED (repository_filter).
            if Count = 0 then
                Continue;
            Title := TextAt(Entry, 'metadata.title');
            if Title = '' then
                Title := TextAt(Entry, 'title');
            Item := Default(TDataSourceItem);
            Item.Id := ZenodoPrefix + TextAt(Entry, 'id');
            Item.Ref := Item.Id;
            Item.Title := Title;
            Item.Details := ReadableDetails('Zenodo',
                TextAt(Entry, 'metadata.publication_date'), Count);
            Item.Size := Bytes;
            Item.IsLeaf := False;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Item;
        end;
    finally
        Root.Free;
    end;
    WriteLog('Published data: Zenodo page ' + IntToStr(APage) + ' for "' +
        AQuery.Words + '" held ' + IntToStr(AFound) + ' of ' + IntToStr(ATotal) +
        ' record(s), ' + IntToStr(Length(Result)) + ' with a readable file', Debug);
end;

function TPublishedRepositories.FigshareBody(const AQuery: TDatasetQuery;
    APage: longint): string;
begin
    Result := '{"item_type":3,"page_size":' + IntToStr(FigsharePageSize) +
        ',"page":' + IntToStr(APage);
    if AQuery.Kind = dqKind then
        Result := Result + ',"order":"published_date","order_direction":' +
            '"desc","search_for":' + JsonString(FigshareFilesQuery(
            AQuery.Extensions))
    else
        Result := Result + ',"search_for":' + JsonString(AQuery.Words);
    Result := Result + '}';
end;

function TPublishedRepositories.FigshareRecord(AHit: TJSONData;
    out ACount: longint): TDataSourceItem;
var
    Hit, Article: TJSONData;
    Bytes: int64;
begin
    Hit := AHit;
    Result := Default(TDataSourceItem);
    Result.Id := FigsharePrefix + TextAt(Hit, 'id');
    Result.Ref := Result.Id;
    Result.Title := TextAt(Hit, 'title');
    //  ITS FILES ARE ASKED FOR NOW, so a record holding nothing readable never
    //  reaches the list (the header says why figshare needs this).
    Article := ParsedReply(FWeb.GetText(
        'https://api.figshare.com/v2/articles/' + TextAt(Hit, 'id')),
        'the figshare article');
    try
        ACount := UsableCount(Article.FindPath('files'), 'name', Bytes);
    finally
        Article.Free;
    end;
    Result.Details := ReadableDetails('figshare', TextAt(Hit, 'published_date'),
        ACount);
    Result.Size := Bytes;
    Result.IsLeaf := False;
end;

function TPublishedRepositories.FigshareDatasets(const AQuery: TDatasetQuery;
    APage: longint; out AFound: longint; out AMore: boolean): TDataSourceItems;
var
    Root: TJSONData;
    Status: longint;
    Headers, Reply: string;
    i, Count: longint;
    Item: TDataSourceItem;
begin
    Result := nil;
    AFound := 0;
    AMore := False;
    Reply := FWeb.PostJson('https://api.figshare.com/v2/articles/search',
        FigshareBody(AQuery, APage), Status, Headers);
    if (Status < 200) or (Status > 299) then
        raise EDataSourceFault.Create('figshare refused the search (status ' +
            IntToStr(Status) + '): ' + Copy(Reply, 1, 200));
    Root := ParsedReply(Reply, 'the figshare search');
    try
        if Root.JSONType <> jtArray then
            raise EDataSourceFault.Create('The answer to the figshare search ' +
                'was not a list of records, so nothing could be read from it.');
        AFound := Root.Count;
        AMore := Root.Count >= FigsharePageSize;
        for i := 0 to Root.Count - 1 do
        begin
            Item := FigshareRecord(Root.Items[i], Count);
            if Count = 0 then
                Continue;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Item;
        end;
    finally
        Root.Free;
    end;
    WriteLog('Published data: figshare page ' + IntToStr(APage) + ' for "' +
        AQuery.Words + '" held ' + IntToStr(AFound) + ' record(s), ' +
        IntToStr(Length(Result)) + ' with a readable file', Debug);
end;

function Notice(const ARepository, AReason: string): TDataSourceItem;
begin
    Result := Default(TDataSourceItem);
    Result.Id := NoticePrefix + ARepository;
    Result.Ref := NoticePrefix + AReason;
    Result.Title := ARepository + ' could not be searched';
    Result.Details := 'open this row to see why';
    Result.IsLeaf := False;
end;

function More(const ARepository: string; APage: longint;
    const AQuery: TDatasetQuery): TDataSourceItem;
begin
    Result := Default(TDataSourceItem);
    Result.Ref := MoreRef(ARepository, APage, AQuery);
    Result.Id := Result.Ref;
    Result.Title := 'More from ' + ARepository + '...';
    Result.Details := 'page ' + IntToStr(APage);
    Result.IsLeaf := False;
end;

procedure Append(var AItems: TDataSourceItems; const AMore: TDataSourceItems);
var
    i: longint;
begin
    for i := 0 to High(AMore) do
    begin
        SetLength(AItems, Length(AItems) + 1);
        AItems[High(AItems)] := AMore[i];
    end;
end;

procedure AppendOne(var AItems: TDataSourceItems; const AItem: TDataSourceItem);
begin
    SetLength(AItems, Length(AItems) + 1);
    AItems[High(AItems)] := AItem;
end;

function TPublishedRepositories.Datasets(const AQuery: TDatasetQuery;
    AZenodoPage, AFigsharePage: longint): TDataSourceItems;
var
    Part: TDataSourceItems;
    Found, Total, Failures, Asked, FoundAll, FigFound: longint;
    HasMore: boolean;
    Reasons: string;
begin
    Result := nil;
    Failures := 0;
    Asked := 0;
    FoundAll := 0;
    Reasons := '';
    if AZenodoPage > 0 then
    begin
        Inc(Asked);
        try
            Part := ZenodoDatasets(AQuery, AZenodoPage, Found, Total);
            Inc(FoundAll, Found);
            Append(Result, Part);
            if AZenodoPage * ZenodoPageSize < Total then
                AppendOne(Result, More('Zenodo', AZenodoPage + 1, AQuery));
        except
            on E: Exception do
            begin
                //  ONE SERVICE DOWN does not hide the other's records.
                if E is EWebCancelled then
                    raise;
                Inc(Failures);
                Reasons := Reasons + 'Zenodo: ' + E.Message + ' ';
                WriteLog('Published data: Zenodo could not be searched: ' +
                    E.ClassName + ': ' + E.Message, Warning);
                AppendOne(Result, Notice('Zenodo', E.Message));
            end;
        end;
    end;
    if AFigsharePage > 0 then
    begin
        Inc(Asked);
        try
            Part := FigshareDatasets(AQuery, AFigsharePage, FigFound, HasMore);
            Inc(FoundAll, FigFound);
            Append(Result, Part);
            if HasMore then
                AppendOne(Result, More('figshare', AFigsharePage + 1, AQuery));
        except
            on E: Exception do
            begin
                if E is EWebCancelled then
                    raise;
                Inc(Failures);
                Reasons := Reasons + 'figshare: ' + E.Message + ' ';
                WriteLog('Published data: figshare could not be searched: ' +
                    E.ClassName + ': ' + E.Message, Warning);
                AppendOne(Result, Notice('figshare', E.Message));
            end;
        end;
    end;
    if (Asked > 0) and (Failures = Asked) then
        raise EDataSourceFault.Create('Neither ' + SearchedRepositories +
            ' could be searched. ' + Trim(Reasons));
    //  A SERVICE THAT FAILED is already a row saying so: the list is that,
    //  not a refusal - "found nothing" would hide that it was never asked.
    if (Failures = 0) and (Length(Result) = 0) then
    begin
        //  Every service answered, and nothing they hold is readable.
        if (FoundAll = 0) and (AQuery.Kind = dqKind) then
            raise EDataSourceError.Create(SearchedRepositories + ' hold no ' +
                'dataset with ' + AQuery.Words + ' files. Try another kind, ' +
                'or search by words.');
        if FoundAll = 0 then
            raise EDataSourceError.Create(SearchedRepositories + ' found ' +
                'nothing for "' + AQuery.Words + '". Try fewer or different ' +
                'words, or paste a DOI.');
        raise EDataSourceError.Create(NothingUsableMessage(FoundAll,
            'records for "' + AQuery.Words + '"'));
    end;
end;

function TPublishedRepositories.MoreDatasets(const ARef: string): TDataSourceItems;
var
    Repository: string;
    Page: longint;
    Query: TDatasetQuery;
begin
    if not ReadMoreRef(ARef, Repository, Page, Query) then
        raise EDataSourceFault.Create('That row does not say which page to ' +
            'open: ' + ARef);
    if Repository = 'Zenodo' then
        Result := Datasets(Query, Page, 0)
    else
        Result := Datasets(Query, 0, Page);
end;

function TPublishedRepositories.RecordFiles(const ARef: string): TDataSourceItems;
var
    Root, Files, Entry: TJSONData;
    i: longint;
    Item: TDataSourceItem;
    Id, What, NameField, UrlField: string;
begin
    Result := nil;
    if StartsStr(ZenodoPrefix, ARef) then
    begin
        Id := Copy(ARef, Length(ZenodoPrefix) + 1, MaxInt);
        What := 'the Zenodo record';
        Root := ParsedReply(FWeb.GetText(
            'https://zenodo.org/api/records/' + Id), What);
        //  'key' is the file name; 'links.self' is where the bytes are.
        NameField := 'key';
        UrlField := 'links.self';
    end
    else if StartsStr(FigsharePrefix, ARef) then
    begin
        Id := Copy(ARef, Length(FigsharePrefix) + 1, MaxInt);
        What := 'the figshare article';
        Root := ParsedReply(FWeb.GetText(
            'https://api.figshare.com/v2/articles/' + Id), What);
        NameField := 'name';
        UrlField := 'download_url';
    end
    else
        Exit;
    try
        Files := Root.FindPath('files');
        if Files = nil then
            Exit;
        for i := 0 to Files.Count - 1 do
        begin
            Entry := Files.Items[i];
            Item := Default(TDataSourceItem);
            Item.Title := TextAt(Entry, NameField);
            Item.FileName := Item.Title;
            Item.Id := ARef + '/' + Item.Title;
            Item.Ref := TextAt(Entry, UrlField);
            Item.Size := StrToInt64Def(TextAt(Entry, 'size'), 0);
            Item.IsLeaf := True;
            //  A FILE THAT CANNOT BE DOWNLOADED is no file to offer.
            if (Item.Title = '') or (Item.Ref = '') then
                Continue;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Item;
        end;
    finally
        Root.Free;
    end;
end;

end.
