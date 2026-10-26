// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Searching Zenodo and figshare by words: what each is asked, what is
listed, and what a failure of one does to the other.)

The replies are cut-down shapes of what each service answered on 2026-10-20:
a Zenodo search hit carries its files, a figshare search hit does not, so
figshare's records are asked for their files one by one before they are
listed.
}
unit testcase_published_search;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, doi_source, published_repositories,
    int_web_client, mock_web_client;

type
    TPublishedSearchTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FSource: TDoiSource;
        function Search(const AWords: string): TDataSourceItems;
        function MessageOf(const AWords: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure WordsSearchBothRepositories;
        procedure EachRecordSaysWhereItIsAndHowManyFilesCanBeRead;
        procedure ZenodoIsAskedForDatasetsOfKindsThisBuildReads;
        procedure FigshareIsAskedForDatasetsInJson;
        procedure ARecordWithNothingReadableIsNotListed;
        procedure AFigsharesRecordFilesAreAskedBeforeItIsListed;
        procedure OneServiceDownStillShowsTheOther;
        procedure TheNoticeOpensToTheReason;
        procedure BothServicesDownIsAFaultNamingBoth;
        procedure NothingFoundSaysWhatToTry;
        procedure NothingReadableSaysHowMuchWasFound;
        procedure APageWithMoreEndsInAMoreRow;
        procedure AMoreRowOpensTheNextPageOfThatServiceOnly;
        procedure TheHintsNameTheServicesSearched;
        procedure AMoreRowsReferenceReadsBackWhateverTheWords;
        procedure AJsonStringIsEscaped;
        procedure AZenodoAnswerWithoutItsHitsIsAFaultNamingTheField;
        procedure AKindIsAskedForByItsFilesOnBoth;
        procedure TheFilesQueriesNameEveryExtension;
    end;

implementation

uses
    data_loader_registration, data_source_registration, published_data_module;

const
    TwoZenodoHits = '{"hits":{"total":2,"hits":[' +
        '{"id":11,"metadata":{"title":"Quartz XRD","publication_date":"2024-03-01"},' +
        '"files":[{"key":"q.xy","size":3,"links":{"self":"https://z/q.xy"}},' +
        '{"key":"paper.pdf","size":3,"links":{"self":"https://z/p.pdf"}}]},' +
        '{"id":12,"metadata":{"title":"Photos only"},' +
        '"files":[{"key":"a.jpg","size":3,"links":{"self":"https://z/a.jpg"}}]}]}}';
    OneFigshareHit = '[{"id":701,"title":"Calcite pattern",' +
        '"published_date":"2025-01-02T10:00:00Z"}]';
    FigshareArticle701 = '{"id":701,"files":[{"id":1,"name":"c.csv",' +
        '"size":9,"download_url":"https://f/1"},{"id":2,"name":"c.xy",' +
        '"size":9,"download_url":"https://f/2"}]}';

procedure TPublishedSearchTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    RegisterPublishedDataModule;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FSource := TDoiSource.Create(FWeb);
end;

procedure TPublishedSearchTest.TearDown;
begin
    FreeAndNil(FSource);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TPublishedSearchTest.Search(const AWords: string): TDataSourceItems;
var
    Query: TDataSourceQuery;
begin
    Query := nil;
    Query := WithQueryValue(Query, DoiFieldId, AWords);
    Result := FSource.Search(Query);
end;

function TPublishedSearchTest.MessageOf(const AWords: string): string;
begin
    Result := '';
    try
        Search(AWords);
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TPublishedSearchTest.WordsSearchBothRepositories;
var
    Items: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    FMockObject.Reply('api.figshare.com/v2/articles/search', OneFigshareHit);
    FMockObject.Reply('api.figshare.com/v2/articles/701', FigshareArticle701);
    Items := Search('carbonate diffraction');
    AssertEquals('one from each', 2, Length(Items));
    AssertEquals('zenodo:11', Items[0].Ref);
    AssertEquals('figshare:701', Items[1].Ref);
    AssertFalse('a record is opened, not read', Items[1].IsLeaf);
end;

procedure TPublishedSearchTest.EachRecordSaysWhereItIsAndHowManyFilesCanBeRead;
var
    Items: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    FMockObject.Reply('api.figshare.com/v2/articles/search', OneFigshareHit);
    FMockObject.Reply('api.figshare.com/v2/articles/701', FigshareArticle701);
    Items := Search('carbonate');
    AssertEquals('Zenodo, 2024-03-01, 1 readable file', Items[0].Details);
    AssertEquals('the size of what can be read, not of the paper beside it', 3,
        Items[0].Size);
    AssertEquals('both readable files of the figshare record', 18,
        Items[1].Size);
    AssertTrue(Items[1].Details, Pos('figshare, 2025-01-02', Items[1].Details) = 1);
    AssertTrue(Items[1].Details, Pos('readable files', Items[1].Details) > 0);
end;

procedure TPublishedSearchTest.ZenodoIsAskedForDatasetsOfKindsThisBuildReads;
var
    Asked: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Search('quartz');
    Asked := FMockObject.Log.AsText;
    AssertTrue('datasets only: ' + Asked, Pos('type=dataset', Asked) > 0);
    //  BY THE FILE'S NAME, not file_type: Zenodo's file_type knows only the
    //  commonest types and finds no .xrdml at all (measured 2026-10-20).
    AssertTrue('the words: ' + Asked, Pos('%28quartz%29', Asked) > 0);
    AssertTrue('and a file of a kind this build reads: ' + Asked,
        Pos('%2A.xy', Asked) > 0);
    AssertTrue('or an archive, which may hold one: ' + Asked,
        Pos('%2A.zip', Asked) > 0);
    AssertEquals('no file_type, which knows too few kinds', 0,
        Pos('file_type', Asked));
    AssertTrue('a page Zenodo accepts without an account: ' + Asked,
        Pos('size=25', Asked) > 0);
end;

procedure TPublishedSearchTest.FigshareIsAskedForDatasetsInJson;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Search('quartz "alpha"');
    AssertTrue(FMockObject.LastJson, Pos('"item_type":3', FMockObject.LastJson) > 0);
    AssertTrue('the words, escaped: ' + FMockObject.LastJson,
        Pos('"search_for":"quartz \"alpha\""', FMockObject.LastJson) > 0);
end;

procedure TPublishedSearchTest.ARecordWithNothingReadableIsNotListed;
var
    Items: TDataSourceItems;
    i: longint;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Items := Search('quartz');
    for i := 0 to High(Items) do
        AssertFalse('photos only is left out', Items[i].Title = 'Photos only');
end;

procedure TPublishedSearchTest.AFigsharesRecordFilesAreAskedBeforeItIsListed;
begin
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":0,"hits":[]}}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', OneFigshareHit);
    FMockObject.Reply('api.figshare.com/v2/articles/701', FigshareArticle701);
    Search('calcite');
    AssertEquals(1, FMockObject.Log.CountOf(
        'GetText(https://api.figshare.com/v2/articles/701)'));
end;

procedure TPublishedSearchTest.OneServiceDownStillShowsTheOther;
var
    Items: TDataSourceItems;
begin
    //  No reply for figshare: the mock refuses it, as a service that is down.
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    Items := Search('quartz');
    AssertEquals('Zenodo''s record and a notice', 2, Length(Items));
    AssertEquals('zenodo:11', Items[0].Ref);
    AssertEquals('figshare could not be searched', Items[1].Title);
end;

procedure TPublishedSearchTest.TheNoticeOpensToTheReason;
var
    Items: TDataSourceItems;
    Message_: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    Items := Search('quartz');
    Message_ := '';
    try
        FSource.Children(Items[1]);
    except
        on E: EDataSourceFault do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('figshare.com', Message_) > 0);
end;

procedure TPublishedSearchTest.BothServicesDownIsAFaultNamingBoth;
var
    Raised: boolean;
    Message_: string;
begin
    FMockObject.FailWith('Cannot fetch it: nothing answered.');
    Raised := False;
    Message_ := '';
    try
        Search('quartz');
    except
        on E: EDataSourceFault do
        begin
            Raised := True;
            Message_ := E.Message;
        end;
    end;
    AssertTrue('a fault, which the wizard points at the log', Raised);
    AssertTrue(Message_, Pos('Zenodo and figshare', Message_) > 0);
end;

procedure TPublishedSearchTest.NothingFoundSaysWhatToTry;
var
    Message_: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":0,"hits":[]}}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Message_ := MessageOf('quartz on mars');
    AssertTrue(Message_, Pos('found nothing for "quartz on mars"', Message_) > 0);
    AssertTrue(Message_, Pos('paste a DOI', Message_) > 0);
end;

procedure TPublishedSearchTest.NothingReadableSaysHowMuchWasFound;
var
    Message_: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":1,"hits":[' +
        '{"id":12,"metadata":{"title":"Photos"},"files":[{"key":"a.jpg",' +
        '"size":3,"links":{"self":"x"}}]}]}}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Message_ := MessageOf('photos');
    AssertTrue(Message_, Pos('Found 1 records', Message_) > 0);
    AssertTrue(Message_, Pos('Fit can read', Message_) > 0);
end;

procedure TPublishedSearchTest.APageWithMoreEndsInAMoreRow;
var
    Items: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', StringReplace(TwoZenodoHits,
        '"total":2', '"total":120', []));
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Items := Search('quartz');
    AssertEquals('More from Zenodo...', Items[High(Items)].Title);
    AssertFalse(Items[High(Items)].IsLeaf);
end;

procedure TPublishedSearchTest.AMoreRowOpensTheNextPageOfThatServiceOnly;
var
    Items: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', StringReplace(TwoZenodoHits,
        '"total":2', '"total":120', []));
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Items := Search('quartz');
    FSource.Children(Items[High(Items)]);
    AssertTrue('Zenodo''s second page: ' + FMockObject.Log.AsText,
        Pos('page=2', FMockObject.Log.AsText) > 0);
    AssertEquals('figshare is not asked again', 1,
        FMockObject.Log.CountOf('PostJson'));
end;

procedure TPublishedSearchTest.TheHintsNameTheServicesSearched;
var
    Info: TDataSourceInfo;
begin
    Info := TDoiSource.Info;
    AssertEquals('Zenodo and figshare', SearchedRepositories);
    AssertTrue(Info.Summary, Pos(SearchedRepositories, Info.Summary) > 0);
    AssertTrue(Info.QueryFields[0].Hint,
        Pos(SearchedRepositories, Info.QueryFields[0].Hint) > 0);
    AssertTrue(MessageOf(''), Pos(SearchedRepositories, MessageOf('')) > 0);
end;

procedure TPublishedSearchTest.AMoreRowsReferenceReadsBackWhateverTheWords;
var
    Query, Back: TDatasetQuery;
    Repository: string;
    Page: longint;
begin
    Query := Default(TDatasetQuery);
    Query.Kind := dqWords;
    Query.Words := 'a:b c';
    AssertTrue(ReadMoreRef(MoreRef('figshare', 3, Query), Repository, Page, Back));
    AssertEquals('figshare', Repository);
    AssertEquals(3, Page);
    AssertTrue(Back.Kind = dqWords);
    AssertEquals('a:b c', Back.Words);
    AssertFalse('a record is not a More row', ReadMoreRef('zenodo:1', Repository,
        Page, Back));
end;

procedure TPublishedSearchTest.AZenodoAnswerWithoutItsHitsIsAFaultNamingTheField;
var
    Items: TDataSourceItems;
    Message_: string;
begin
    //  Read as "nothing found", a changed service would send the user to try
    //  other words; it is a fault, shown as the notice row, naming the field.
    FMockObject.Reply('zenodo.org/api/records?', '{"results":[]}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Items := Search('quartz');
    AssertEquals('Zenodo could not be searched', Items[0].Title);
    Message_ := '';
    try
        FSource.Children(Items[0]);
    except
        on E: EDataSourceFault do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('hits.hits', Message_) > 0);
end;

procedure TPublishedSearchTest.AJsonStringIsEscaped;
begin
    AssertEquals('"a \"b\" \\ c"', JsonString('a "b" \ c'));
end;

procedure TPublishedSearchTest.AKindIsAskedForByItsFilesOnBoth;
var
    Repositories: TPublishedRepositories;
    Query: TDatasetQuery;
begin
    FMockObject.Reply('zenodo.org/api/records?', TwoZenodoHits);
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Repositories := TPublishedRepositories.Create(FWeb);
    try
        Query := Default(TDatasetQuery);
        Query.Kind := dqKind;
        Query.Extensions := 'xrdml,xye';
        Query.Words := 'Diffraction scans';
        Repositories.Datasets(Query, 1, 1);
    finally
        Repositories.Free;
    end;
    AssertTrue('Zenodo, by the files, newest first: ' + FMockObject.Log.AsText,
        Pos('sort=newest', FMockObject.Log.AsText) > 0);
    AssertTrue(FMockObject.Log.AsText,
        Pos('%2A.xrdml+OR+%2A.xye', FMockObject.Log.AsText) > 0);
    AssertTrue('figshare, by extension: ' + FMockObject.LastJson,
        Pos(':extension: xrdml OR :extension: xye', FMockObject.LastJson) > 0);
end;

procedure TPublishedSearchTest.TheFilesQueriesNameEveryExtension;
begin
    AssertEquals('files.entries.key:(*.a OR *.b)', ZenodoFilesQuery('a,b'));
    AssertEquals(':extension: a OR :extension: b', FigshareFilesQuery('a,b'));
    AssertTrue('every reader and every archive: ' + SearchedExtensions,
        (Pos('xy', SearchedExtensions) > 0) and
        (Pos('zip', SearchedExtensions) > 0));
end;

initialization
    RegisterTest('unit', TPublishedSearchTest);
end.
