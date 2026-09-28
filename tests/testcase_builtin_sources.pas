// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The three sources the framework itself ships, over canned replies.)

NO TEST HERE TOUCHES THE NETWORK, and that is a rule rather than a convenience:
a test that reached a public service would fail when the service is slow or
rate-limits it, and pass tomorrow for reasons nobody here decided. The replies
are recorded shapes of what each service answers, cut down to what the source
reads.

WHAT IS WORTH PINNING is what each source ASKS FOR and what it MAKES OF THE
ANSWER - a source quietly addressing the wrong endpoint is a defect that looks
exactly like a service with nothing to say.
}
unit testcase_builtin_sources;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, samples_source, url_source, doi_source,
    data_loader_registry,
    int_web_client, mock_web_client;

type
    TBuiltinSourcesTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        function QueryOf(const AId, AValue: string): TDataSourceQuery;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  Web address.
        procedure AnAddressBecomesOneFileToFetch;
        procedure TheNameComesFromTheAddressWithoutItsQuery;
        procedure APageAddressOnThisComputerIsRefusedWithAdvice;
        procedure SomethingThatIsNotAnAddressIsRefused;
        procedure TheFileIsFetchedFromTheAddressGiven;
        //  DOI.
        procedure ADoiIsToldApartFromWordsToSearchFor;
        procedure ADoiIsRecognisedHoweverItWasPasted;
        procedure ADoiResolvingToZenodoBecomesARecord;
        procedure ARecordListsItsFilesWithTheirSizes;
        procedure AFigshareArticleListsItsFilesToo;
        procedure ADoiSomewhereElseIsRefusedNamingWhereItWent;
        procedure WordsSearchZenodoRatherThanResolving;
        //  Samples.
        procedure TheSamplesAreLookedForWhereTheyAreInstalled;
        procedure TheBundleKeepsThemWhereMacOsPutsResources;
        procedure APackageUnderItsOwnNameIsLookedUpUnderThatName;
        procedure AnInstallationWithoutSamplesSaysSo;
        procedure TheSamplesAreWhateverThisBuildReads;
    end;

implementation

uses
    data_loader_registration;

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
        '{"id":7654321,"files":[{"id":11,"name":"series.csv","size":1024,' +
        '"download_url":"https://ndownloader.figshare.com/files/11"}]}';

    ZenodoSearchReply =
        '{"hits":{"total":1,"hits":[{"id":222,"metadata":{' +
        '"title":"Quartz spectra","publication_date":"2024-03-01"}}]}}';

function TBuiltinSourcesTest.QueryOf(const AId, AValue: string): TDataSourceQuery;
begin
    Result := nil;
    Result := WithQueryValue(Result, AId, AValue);
end;

procedure TBuiltinSourcesTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
end;

procedure TBuiltinSourcesTest.TearDown;
begin
    //  The interface first, then the object: a corba interface is a bare
    //  pointer and nothing counts it.
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

{ --------------------------------- URL -------------------------------------- }

procedure TBuiltinSourcesTest.AnAddressBecomesOneFileToFetch;
var
    Source: TUrlSource;
    Items: TDataSourceItems;
begin
    Source := TUrlSource.Create(FWeb);
    try
        Items := Source.Search(QueryOf(UrlFieldId,
            'https://example.org/data/spectrum.xy'));
        AssertEquals('one address is one result', 1, Length(Items));
        AssertTrue('and it is a file rather than a record', Items[0].IsLeaf);
        //  NOTHING IS FETCHED to answer the query: a mistyped address costs
        //  nothing, and the wizard's steps mean the same for every source.
        AssertEquals('nothing was downloaded to list it', 0,
            FMockObject.Log.CountOf('Download'));
    finally
        Source.Free;
    end;
end;

procedure TBuiltinSourcesTest.TheNameComesFromTheAddressWithoutItsQuery;
begin
    AssertEquals('spectrum.xy',
        FileNameFromUrl('https://example.org/d/spectrum.xy?token=9#top'));
    //  An address whose file is chosen by its query names no file at all -
    //  and neither does one ending in a word that names an endpoint, which is
    //  what leaves the server's own suggestion free to say what the file is.
    AssertEquals('', FileNameFromUrl('https://example.org/download/?id=SP500'));
    AssertEquals('', FileNameFromUrl('https://example.org/download?id=SP500'));
end;

procedure TBuiltinSourcesTest.APageAddressOnThisComputerIsRefusedWithAdvice;
var
    Source: TUrlSource;
    Message_: string;
begin
    Source := TUrlSource.Create(FWeb);
    try
        Message_ := '';
        try
            Source.Search(QueryOf(UrlFieldId, 'file:///home/u/data.dat'));
        except
            on E: EDataSourceError do
                Message_ := E.Message;
        end;
        //  Not a snub: the command that opens a local file is in the same menu.
        AssertTrue('a local address is answered with the command that opens ' +
            'one: got "' + Message_ + '"', Pos('Import Profile', Message_) > 0);
    finally
        Source.Free;
    end;
end;

procedure TBuiltinSourcesTest.SomethingThatIsNotAnAddressIsRefused;
begin
    AssertTrue('a bare word is not an address',
        Pos('http', UrlRefusal('quartz spectra')) > 0);
    AssertEquals('a full address is accepted', '',
        UrlRefusal('https://example.org/a.xy'));
end;

procedure TBuiltinSourcesTest.TheFileIsFetchedFromTheAddressGiven;
var
    Source: TUrlSource;
    Items: TDataSourceItems;
    Dest: TStringStream;
    Name_: string;
begin
    FMockObject.Reply('example.org/data/spectrum.xy', '1 2'#10'3 4');
    Source := TUrlSource.Create(FWeb);
    Dest := TStringStream.Create('');
    try
        Items := Source.Search(QueryOf(UrlFieldId,
            'https://example.org/data/spectrum.xy'));
        Name_ := Source.Download(Items[0], Dest);
        AssertEquals('the bytes at that address', '1 2'#10'3 4', Dest.DataString);
        AssertEquals('saved under the name the address gave', 'spectrum.xy',
            Name_);
    finally
        Dest.Free;
        Source.Free;
    end;
end;

{ --------------------------------- DOI -------------------------------------- }

procedure TBuiltinSourcesTest.ADoiIsToldApartFromWordsToSearchFor;
begin
    AssertTrue('a DOI', LooksLikeDoi('10.5281/zenodo.1234567'));
    AssertFalse('words', LooksLikeDoi('quartz raman spectra'));
    //  The '/' must come AFTER the prefix, or a measurement in a phrase reads
    //  as an identifier.
    AssertFalse('a phrase with a number in it',
        LooksLikeDoi('gamma-10.5 spectra'));
end;

procedure TBuiltinSourcesTest.ADoiIsRecognisedHoweverItWasPasted;
begin
    AssertEquals('10.5281/zenodo.1', DoiOf('https://doi.org/10.5281/zenodo.1'));
    AssertEquals('10.5281/zenodo.1', DoiOf('doi:10.5281/zenodo.1'));
    AssertEquals('10.5281/zenodo.1', DoiOf('  10.5281/zenodo.1 '));
end;

procedure TBuiltinSourcesTest.ADoiResolvingToZenodoBecomesARecord;
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

procedure TBuiltinSourcesTest.ARecordListsItsFilesWithTheirSizes;
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
        AssertEquals('both files are listed, readable or not', 2, Length(Files));
        AssertEquals('the first is named as the record names it', 'quartz.xy',
            Files[0].Title);
        AssertEquals('with its size', 48123, Files[0].Size);
        //  A file whose kind nothing here reads is LISTED rather than hidden:
        //  what the record contains is part of what the user came to see, and
        //  the refusal that follows names the kind.
        AssertEquals('and the unreadable one is still visible', 'notes.pdf',
            Files[1].Title);
    finally
        Source.Free;
    end;
end;

procedure TBuiltinSourcesTest.AFigshareArticleListsItsFilesToo;
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
        AssertEquals('series.csv', Files[0].Title);
    finally
        Source.Free;
    end;
end;

procedure TBuiltinSourcesTest.ADoiSomewhereElseIsRefusedNamingWhereItWent;
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

procedure TBuiltinSourcesTest.WordsSearchZenodoRatherThanResolving;
var
    Source: TDoiSource;
    Items: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', ZenodoSearchReply);
    Source := TDoiSource.Create(FWeb);
    try
        Items := Source.Search(QueryOf(DoiFieldId, 'quartz spectra'));
        AssertEquals('the search results are records', 1, Length(Items));
        AssertEquals('Quartz spectra', Items[0].Title);
        AssertEquals('the handle system was never asked', 0,
            FMockObject.Log.CountOf('GetText: https://doi.org/api/handles/'));
    finally
        Source.Free;
    end;
end;

{ ------------------------------- Samples ------------------------------------ }

{ A machine with nothing installed anywhere. }
function NothingExists(const APath: string): boolean;
begin
    Result := False;
end;

procedure TBuiltinSourcesTest.TheSamplesAreLookedForWhereTheyAreInstalled;
var
    Candidates: TStringList;
begin
    Candidates := SampleDirectoryCandidatesFor('/opt/fit/bin/', 'fit');
    try
        //  A source tree, a portable archive, a bundle and an installed
        //  package put them in different places, and all of them are right.
        AssertTrue('beside the program', Candidates.IndexOf('/opt/fit/bin/Data') >= 0);
        AssertTrue('and where a package puts read-only program data',
            Candidates.IndexOf('/opt/fit/share/fit/Data') >= 0);
    finally
        Candidates.Free;
    end;
end;

procedure TBuiltinSourcesTest.TheBundleKeepsThemWhereMacOsPutsResources;
var
    Candidates: TStringList;
begin
    //  A macOS bundle has the binary in Contents/MacOS and its read-only data
    //  in Contents/Resources - which is where the packaging puts it. Looking
    //  only one directory up finds Contents, and the samples silently are not
    //  there on the one platform whose layout differs.
    Candidates := SampleDirectoryCandidates('/Applications/Fit.app/Contents/MacOS/');
    try
        AssertTrue('the bundle''s resources are among the candidates',
            Candidates.IndexOf('/Applications/Fit.app/Contents/Resources/Data') >= 0);
    finally
        Candidates.Free;
    end;
end;

procedure TBuiltinSourcesTest.APackageUnderItsOwnNameIsLookedUpUnderThatName;
var
    Candidates: TStringList;
begin
    //  A Linux package installs its data under ITS OWN NAME - /usr/share/fit
    //  for one product and /usr/share/fit-pro for the other - so the directory
    //  is derived from the program's own file name rather than written out.
    //  Without this the second product finds the first's samples, or none.
    Candidates := SampleDirectoryCandidatesFor('/usr/bin/', 'fit-pro');
    try
        AssertTrue('its own share directory',
            Candidates.IndexOf('/usr/share/fit-pro/Data') >= 0);
    finally
        Candidates.Free;
    end;
end;

procedure TBuiltinSourcesTest.AnInstallationWithoutSamplesSaysSo;
var
    Nowhere: string;
begin
    //  The existence check is passed in rather than taken from the disk, and
    //  this is why: one candidate is the absolute /usr/share/fit/Data, so on a
    //  machine that HAS Fit installed a test asking the real disk would be
    //  answering a question about that machine instead of about this rule.
    Nowhere := SamplesDirectoryFor('/no/such/place/', @NothingExists);
    //  An empty list would read as "this build has no samples in it", which is
    //  a different and more alarming thing than "they were not installed".
    AssertEquals('nothing is found where nothing is installed', '', Nowhere);
end;

procedure TBuiltinSourcesTest.TheSamplesAreWhateverThisBuildReads;
var
    Declared: TStringList;
    Loaders: TDataLoaderInfoArray;
    Own: TStringList;
    i, j: longint;
begin
    //  DERIVED, NOT LISTED. The samples are chosen from the installed folder by
    //  whether this build reads them, so what the source declares must be
    //  exactly that: a fixed list named .CSV in a build with no price reader,
    //  which the registry walk then refused.
    RegisterAllDataLoaders;
    Declared := TStringList.Create;
    Own := TStringList.Create;
    try
        Declared.Delimiter := ';';
        Declared.StrictDelimiter := True;
        Declared.DelimitedText := TSamplesSource.Info.ProducesExtensions;
        Own.Delimiter := ';';
        Own.StrictDelimiter := True;
        Loaders := RegisteredDataLoaders;
        for i := 0 to High(Loaders) do
        begin
            Own.DelimitedText := Loaders[i].Extensions;
            for j := 0 to Own.Count - 1 do
                AssertTrue(Own[j] + ' is read here, so it is offered',
                    Declared.IndexOf(Own[j]) >= 0);
        end;
        for i := 0 to Declared.Count - 1 do
            AssertTrue(Declared[i] + ' is offered, so something reads it',
                FindDataLoaderClass('x' + Declared[i]) <> nil);
    finally
        Own.Free;
        Declared.Free;
    end;
end;

initialization
    RegisterTest('unit', TBuiltinSourcesTest);
end.
