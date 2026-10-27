// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The DOI source over canned replies, and what it says when a
repository answers badly.)

MOVED WITH THE SOURCE (2026-10-20) from the framework's testcase_builtin_sources
and testcase_download_failures: the source left the framework for this module,
and a framework test naming a module is what the leak gate fails. The wizard's
own container steps are still tested there, through a probe source that names
no service (mock_record_source).

NO TEST HERE TOUCHES THE NETWORK. The replies are recorded shapes of what each
service answers, cut down to what the source reads.
}
unit testcase_doi_source;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_wizard, doi_source,
    int_web_client, mock_web_client;

type
    { What the source asks for, and what it makes of the answer. }
    TDoiSourceTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        function QueryOf(const AId, AValue: string): TDataSourceQuery;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ADoiIsToldApartFromWordsToSearchFor;
        procedure ADoiIsRecognisedHoweverItWasPasted;
        procedure ADoiResolvingToZenodoBecomesARecord;
        procedure ARecordListsItsFilesWithTheirSizes;
        procedure ADoiResolvingToZenodosDoiPageListsTheSameRecord;
        procedure AFigshareArticleListsItsFilesToo;
        procedure ADoiSomewhereElseIsRefusedNamingWhereItWent;
        procedure WordsSearchZenodoRatherThanResolving;
        procedure ARecordHoldingNothingReadableSaysHowMuchItHeld;
    end;

    { The same source's refusals, where the user meets them: the wizard. }
    TDoiFailureTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        function MessageOfSearch(const ASourceId, AFieldId, AValue: string): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnAnswerThatIsNotWhatWasExpectedSaysSo;
        procedure ADoiThatResolvesNowhereSaysSo;
        procedure ARecordWithNoFilesSaysWhatToDoInstead;
        procedure ZenodoFindingNothingSaysWhatToTryInstead;
        procedure AnEmptyAnswerFromACatalogueIsSaidToBeEmpty;
        procedure ARecordThisProgramCannotListIsRefusedByRepository;
        procedure ARecordsFileIsFetchedFromWhereTheRecordSaid;
    end;

implementation

uses
    data_loader_registration, data_source_registration, published_data_module;

const
    { What the handle system answers for a DOI published on Zenodo, cut to the
      one value the source reads. }
    ZenodoHandleReply =
        '{"responseCode":1,"handle":"10.5281/zenodo.1234567","values":[' +
        '{"index":1,"type":"URL","data":{"format":"string",' +
        '"value":"https://zenodo.org/record/1234567"}}]}';

    ZenodoRecordReply =
        '{"id":1234567,"metadata":{"title":"Raman of quartz"},"files":[' +
        '{"key":"quartz.xy","size":48123,' +
        '"links":{"self":"https://zenodo.org/api/records/1234567/files/quartz.xy/content"}},' +
        '{"key":"notes.pdf","size":90000,' +
        '"links":{"self":"https://zenodo.org/api/records/1234567/files/notes.pdf/content"}}]}';

    FigshareHandleReply =
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://figshare.com/articles/dataset/some_slug/7654321"}}]}';

    FigshareArticleReply =
        '{"id":7654321,"files":[{"id":11,"name":"series.txt","size":1024,' +
        '"download_url":"https://ndownloader.figshare.com/files/11"}]}';

    ZenodoSearchReply =
        '{"hits":{"total":1,"hits":[{"id":222,"metadata":{' +
        '"title":"Quartz spectra","publication_date":"2024-03-01"},' +
        '"files":[{"key":"q.xy","size":3,"links":{"self":"https://z/q.xy"}}]}]}}';

procedure RegisterEverything;
begin
    //  The loaders first: a source declares what it produces by asking them.
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    RegisterPublishedDataModule;
end;

function TDoiSourceTest.QueryOf(const AId, AValue: string): TDataSourceQuery;
begin
    Result := nil;
    Result := WithQueryValue(Result, AId, AValue);
end;

procedure TDoiSourceTest.SetUp;
begin
    inherited SetUp;
    RegisterEverything;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
end;

procedure TDoiSourceTest.TearDown;
begin
    //  The interface first, then the object: a corba interface is a bare
    //  pointer and nothing counts it.
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

procedure TDoiSourceTest.ADoiIsToldApartFromWordsToSearchFor;
begin
    AssertTrue('a DOI', LooksLikeDoi('10.5281/zenodo.1234567'));
    AssertFalse('words', LooksLikeDoi('quartz raman spectra'));
    //  The '/' must come AFTER the prefix, or a measurement in a phrase reads
    //  as an identifier.
    AssertFalse('a phrase with a number in it',
        LooksLikeDoi('gamma-10.5 spectra'));
end;

procedure TDoiSourceTest.ADoiIsRecognisedHoweverItWasPasted;
begin
    AssertEquals('10.5281/zenodo.1', DoiOf('https://doi.org/10.5281/zenodo.1'));
    AssertEquals('10.5281/zenodo.1', DoiOf('doi:10.5281/zenodo.1'));
    AssertEquals('10.5281/zenodo.1', DoiOf('  10.5281/zenodo.1 '));
end;

procedure TDoiSourceTest.ADoiResolvingToZenodoBecomesARecord;
var
    Source: TDoiSource;
    Items: TDataSourceItems;
begin
    FMockObject.Reply('doi.org/api/handles/', ZenodoHandleReply);
    Source := TDoiSource.Create(FWeb);
    try
        Items := Source.Search(QueryOf(DoiFieldId, '10.5281/zenodo.1234567'));
        AssertEquals('one record', 1, Length(Items));
        AssertFalse('a record holds files; it is not one', Items[0].IsLeaf);
        AssertTrue('and it remembers which repository it is in: ' + Items[0].Ref,
            Pos('zenodo:', Items[0].Ref) > 0);
    finally
        Source.Free;
    end;
end;

procedure TDoiSourceTest.ARecordListsItsFilesWithTheirSizes;
var
    Source: TDoiSource;
    Record_: TDataSourceItem;
    Files: TDataSourceItems;
begin
    FMockObject.Reply('doi.org/api/handles/', ZenodoHandleReply);
    FMockObject.Reply('zenodo.org/api/records/1234567', ZenodoRecordReply);
    Source := TDoiSource.Create(FWeb);
    try
        Record_ := Source.Search(QueryOf(DoiFieldId, '10.5281/zenodo.1234567'))[0];
        Files := Source.Children(Record_);
        //  NOTHING THIS BUILD CANNOT USE IS LISTED (the user, 2026-10-20): the
        //  pdf beside the data is left out rather than greyed.
        AssertEquals('only the readable file is listed', 1, Length(Files));
        AssertEquals('named as the record names it', 'quartz.xy',
            Files[0].Title);
        AssertEquals('with its size', 48123, Files[0].Size);
    finally
        Source.Free;
    end;
end;

{ WHAT doi.org ANSWERS NOW for a Zenodo DOI, copied from the live handle
  service (10.5281/zenodo.18522088, 2026-10-20): not .../records/N but
  .../doi/10.5281/zenodo.N, whose last segment is "zenodo.N". Taking that
  segment whole asked Zenodo for record "zenodo.N", which it answers with 404. }
procedure TDoiSourceTest.ADoiResolvingToZenodosDoiPageListsTheSameRecord;
var
    Source: TDoiSource;
    Record_: TDataSourceItem;
    Files: TDataSourceItems;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://zenodo.org/doi/10.5281/zenodo.1234567"}}]}');
    FMockObject.Reply('zenodo.org/api/records/1234567', ZenodoRecordReply);
    Source := TDoiSource.Create(FWeb);
    try
        Record_ := Source.Search(QueryOf(DoiFieldId, '10.5281/zenodo.1234567'))[0];
        AssertEquals('the record is named by its number', 'zenodo:1234567',
            Record_.Ref);
        Files := Source.Children(Record_);
        AssertEquals('and its readable file is listed', 1, Length(Files));
    finally
        Source.Free;
    end;
end;

procedure TDoiSourceTest.AFigshareArticleListsItsFilesToo;
var
    Source: TDoiSource;
    Record_: TDataSourceItem;
    Files: TDataSourceItems;
begin
    FMockObject.Reply('doi.org/api/handles/', FigshareHandleReply);
    FMockObject.Reply('api.figshare.com/v2/articles/7654321', FigshareArticleReply);
    Source := TDoiSource.Create(FWeb);
    try
        Record_ := Source.Search(QueryOf(DoiFieldId, '10.6084/m9.figshare.7654321'))[0];
        //  The article id is the last all-digit segment: the address carries a
        //  slug after the kind and often a version after the id.
        AssertTrue('the article id is found in the slugged address: ' +
            Record_.Ref, Pos('7654321', Record_.Ref) > 0);
        Files := Source.Children(Record_);
        AssertEquals('one file', 1, Length(Files));
        AssertEquals('series.txt', Files[0].Title);
    finally
        Source.Free;
    end;
end;

procedure TDoiSourceTest.ADoiSomewhereElseIsRefusedNamingWhereItWent;
var
    Source: TDoiSource;
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://link.springer.com/article/10.1007/s00269"}}]}');
    Source := TDoiSource.Create(FWeb);
    try
        Message_ := '';
        try
            Source.Search(QueryOf(DoiFieldId, '10.1007/s00269'));
        except
            on E: EDataSourceError do
                Message_ := E.Message;
        end;
        //  Falling through to a download of the landing page would produce a
        //  file that parses to nothing, which is the quiet failure a refusal
        //  exists to prevent.
        AssertTrue('the refusal names where the DOI went: "' + Message_ + '"',
            Pos('springer', LowerCase(Message_)) > 0);
        AssertTrue('and what to do instead',
            Pos('Web address', Message_) > 0);
    finally
        Source.Free;
    end;
end;

procedure TDoiSourceTest.WordsSearchZenodoRatherThanResolving;
var
    Source: TDoiSource;
    Items: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', ZenodoSearchReply);
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Source := TDoiSource.Create(FWeb);
    try
        Items := Source.Search(QueryOf(DoiFieldId, 'quartz spectra'));
        AssertEquals('the search results are records', 1, Length(Items));
        AssertEquals('Quartz spectra', Items[0].Title);
        AssertEquals('the handle system was never asked', 0,
            FMockObject.Log.CountOf('GetText(https://doi.org/api/handles/quartz+spectra)'));
        AssertFalse('the handle system was never asked',
            Pos('doi.org/api/handles', FMockObject.Log.AsText) > 0);
    finally
        Source.Free;
    end;
end;

procedure TDoiSourceTest.ARecordHoldingNothingReadableSaysHowMuchItHeld;
var
    Source: TDoiSource;
    Record_: TDataSourceItem;
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/', ZenodoHandleReply);
    FMockObject.Reply('zenodo.org/api/records/1234567',
        '{"files":[{"key":"paper.pdf","size":1,"links":{"self":"a"}},' +
        '{"key":"photo.jpg","size":1,"links":{"self":"b"}}]}');
    Source := TDoiSource.Create(FWeb);
    try
        Record_ := Source.Search(QueryOf(DoiFieldId, '10.5281/zenodo.1234567'))[0];
        Message_ := '';
        try
            Source.Children(Record_);
        except
            on E: EDataSourceError do
                Message_ := E.Message;
        end;
        //  An empty list would read as a broken source; the record held
        //  something, and the refusal says how much and why none is shown.
        AssertTrue(Message_, Pos('Found 2 files', Message_) > 0);
        AssertTrue(Message_, Pos('Fit can read', Message_) > 0);
    finally
        Source.Free;
    end;
end;

{ ------------------------------ in the wizard ------------------------------- }

procedure TDoiFailureTest.SetUp;
begin
    inherited SetUp;
    RegisterEverything;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.CacheRoot := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-doi-failures';
end;

procedure TDoiFailureTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TDoiFailureTest.MessageOfSearch(const ASourceId, AFieldId,
    AValue: string): string;
begin
    Result := '';
    FWizard.ChooseSource(ASourceId);
    if AFieldId <> '' then
        FWizard.SetField(AFieldId, AValue);
    try
        FWizard.Search;
    except
        on E: Exception do
            Result := E.Message;
    end;
end;

procedure TDoiFailureTest.AnAnswerThatIsNotWhatWasExpectedSaysSo;
var
    Message_: string;
begin
    //  A maintenance page, a login form, an error in HTML: anything that is
    //  not the JSON the service publishes. The parser's complaint about a
    //  character at a position names nothing a user can act on.
    FMockObject.Reply('doi.org/api/handles/', '<html>We are down</html>');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.1');
    AssertTrue(Message_, Pos('not what this program expected', Message_) > 0);
end;

procedure TDoiFailureTest.ADoiThatResolvesNowhereSaysSo;
var
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/', '{"responseCode":100,"values":[]}');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.404');
    AssertTrue(Message_, Pos('does not resolve', Message_) > 0);
    AssertTrue('and says what to check: ' + Message_,
        Pos('copied whole', Message_) > 0);
end;

procedure TDoiFailureTest.ARecordWithNoFilesSaysWhatToDoInstead;
var
    Record_: TDataSourceItem;
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":"https://zenodo.org/record/7"}}]}');
    FMockObject.Reply('zenodo.org/api/records/7', '{"id":7,"files":[]}');
    FWizard.ChooseSource(DoiSourceId);
    FWizard.SetField(DoiFieldId, '10.5281/zenodo.7');
    FWizard.Search;
    Record_ := FWizard.Item(0);
    Message_ := '';
    try
        FWizard.Open(0);
    except
        on E: Exception do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('no files', Message_) > 0);
    AssertTrue('and where to look instead: ' + Message_,
        Pos('browser', Message_) > 0);
end;

procedure TDoiFailureTest.ZenodoFindingNothingSaysWhatToTryInstead;
var
    Message_: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":0,"hits":[]}}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, 'quartz on mars');
    //  Nothing found is an ANSWER, not a fault - so it says what was searched
    //  for and what else to try, rather than reading as a broken search.
    AssertTrue(Message_, Pos('quartz on mars', Message_) > 0);
    AssertTrue(Message_, Pos('paste a DOI', Message_) > 0);
end;

procedure TDoiFailureTest.AnEmptyAnswerFromACatalogueIsSaidToBeEmpty;
var
    Message_: string;
begin
    //  A service that answers with nothing at all - which is what the failure
    //  that prompted this file looked like, one layer up.
    FMockObject.Reply('doi.org/api/handles/', '');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5281/zenodo.1');
    AssertTrue(Message_, Pos('empty', Message_) > 0);
end;

procedure TDoiFailureTest.ARecordThisProgramCannotListIsRefusedByRepository;
var
    Message_: string;
begin
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":' +
        '"https://datadryad.org/stash/dataset/doi:10.5061/dryad.1"}}]}');
    Message_ := MessageOfSearch(DoiSourceId, DoiFieldId, '10.5061/dryad.1');
    //  Named, so the user knows where their data actually is - and told how to
    //  fetch it anyway.
    AssertTrue(Message_, Pos('datadryad.org', Message_) > 0);
    AssertTrue(Message_, Pos('Web address', Message_) > 0);
end;

procedure TDoiFailureTest.ARecordsFileIsFetchedFromWhereTheRecordSaid;
var
    Source: TDoiSource;
    Files: TDataSourceItems;
    Dest: TStringStream;
    Query: TDataSourceQuery;
begin
    //  The address a file is fetched from is the one the RECORD gave, not one
    //  built out of the record's own address - which is what a repository
    //  changing its storage host would otherwise break silently.
    FMockObject.Reply('doi.org/api/handles/',
        '{"values":[{"type":"URL","data":{"value":"https://zenodo.org/record/9"}}]}');
    FMockObject.Reply('zenodo.org/api/records/9',
        '{"files":[{"key":"quartz.xy","size":6,"links":{"self":' +
        '"https://storage.example.org/blobs/quartz.xy"}}]}');
    FMockObject.Reply('storage.example.org/blobs/quartz.xy', '1 2');
    Source := TDoiSource.Create(FWeb);
    Dest := TStringStream.Create('');
    try
        Query := nil;
        Query := WithQueryValue(Query, DoiFieldId, '10.5281/zenodo.9');
        Files := Source.Children(Source.Search(Query)[0]);
        AssertEquals('named as the record names it', 'quartz.xy',
            Source.Download(Files[0], Dest));
        AssertEquals('1 2', Dest.DataString);
        AssertTrue('fetched from the storage host the record gave',
            Pos('storage.example.org', FMockObject.Log.AsText) > 0);
    finally
        Dest.Free;
        Source.Free;
    end;
end;

initialization
    RegisterTest('unit', TDoiSourceTest);
    RegisterTest('unit', TDoiFailureTest);
end.
