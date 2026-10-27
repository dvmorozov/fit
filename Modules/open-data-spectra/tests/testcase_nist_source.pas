// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What the NIST source asks for, and what it makes of the answer.)

OVER RECORDED PAGES, never the live service - see tests/fixtures/README.md. The
two things worth pinning are the two halves of a scraper: WHICH ADDRESS it asks
for, because a source quietly asking the wrong one looks exactly like a service
with nothing to say, and WHAT IT DOES WITH A PAGE IT DOES NOT UNDERSTAND, which
must be a refusal in words rather than an empty list.
}
unit testcase_nist_source;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_registry, data_source_findings,
    data_loader_registration, data_source_registration, client_explanations,
    explanation, explanation_registry,
    int_web_client, mock_web_client,
    nist_webbook_source, jcamp_dx_loader, open_data_spectra_module,
    spectra_explanations;

type
    TNistSourceTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FSource: TNistWebBookSource;
        function Fixture(const AName: string): string;
        function SearchFor(const AName: string): TDataSourceItems;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure OneMatchBecomesOneSubstance;
        procedure SeveralMatchesAreListed;
        procedure ANameNobodyKnowsIsRefusedWithAdvice;
        procedure APageThatCannotBeReadIsRefusedRatherThanEmpty;
        procedure ASubstanceOffersOnlyTheSpectraItHas;
        procedure ASpectrumIsNamedSoThatItsReaderIsChosen;
        procedure TheDownloadAsksForTheJcampOfThatSubstance;
        //  What the module contributes, walked.
        procedure TheModuleRegistersItsFormatAndItsSource;
        procedure EverythingTheModuleRegistersExplainsItself;
    end;

implementation

uses
    data_loader_registry;

function TNistSourceTest.Fixture(const AName: string): string;
var
    Path: string;
    Lines: TStringList;
begin
    Path := ExpandFileName(ExtractFilePath(ParamStr(0)) +
        '../Modules/open-data-spectra/tests/fixtures/' + AName);
    Lines := TStringList.Create;
    try
        Lines.LoadFromFile(Path);
        Result := Lines.Text;
    finally
        Lines.Free;
    end;
end;

procedure TNistSourceTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    //  WHAT THE APPLICATION DOES AT START-UP, in its order: the guide's
    //  chapters, the framework's own sources, then the module's front door.
    //  The walk below is over EVERYTHING registered, so a build that had only
    //  half of it would report the other half as unexplained - which is a true
    //  finding about a build nobody runs.
    RegisterClientExplanations;
    RegisterAllDataSources;
    RegisterOpenDataSpectraModule;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FSource := TNistWebBookSource.Create(FWeb);
end;

procedure TNistSourceTest.TearDown;
begin
    FreeAndNil(FSource);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TNistSourceTest.SearchFor(const AName: string): TDataSourceItems;
var
    Query: TDataSourceQuery;
begin
    Query := nil;
    Query := WithQueryValue(Query, NistNameFieldId, AName);
    Result := FSource.Search(Query);
end;

procedure TNistSourceTest.OneMatchBecomesOneSubstance;
var
    Items: TDataSourceItems;
begin
    FMockObject.Reply('Name=benzene', Fixture('nist-species-benzene.html'));
    Items := SearchFor('benzene');
    AssertEquals('one substance', 1, Length(Items));
    AssertEquals('named as its page is', 'Benzene', Items[0].Title);
    //  A substance holds spectra, so it is something to open rather than
    //  something to download.
    AssertFalse('it is not a file', Items[0].IsLeaf);
    AssertTrue('and it carries the species id: ' + Items[0].Ref,
        Pos('C71432', Items[0].Ref) > 0);
end;

procedure TNistSourceTest.SeveralMatchesAreListed;
var
    Items: TDataSourceItems;
begin
    FMockObject.Reply('Name=phenol', Fixture('nist-matches-phenol.html'));
    Items := SearchFor('phenol');
    AssertEquals('both matches', 2, Length(Items));
    AssertEquals('Phenol', Items[0].Title);
    AssertEquals('1,2,3-Benzenetriol', Items[1].Title);
end;

procedure TNistSourceTest.ANameNobodyKnowsIsRefusedWithAdvice;
var
    Message_: string;
begin
    FMockObject.Reply('Name=quartz', Fixture('nist-not-found.html'));
    Message_ := '';
    try
        SearchFor('quartz');
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    //  A refusal rather than no results: an empty list reads as "this build
    //  cannot search", which sends the user to look in the wrong place.
    AssertTrue('refused: "' + Message_ + '"', Pos('quartz', Message_) > 0);
    AssertTrue('and says what to try instead', Pos('chemical name', Message_) > 0);
end;

procedure TNistSourceTest.APageThatCannotBeReadIsRefusedRatherThanEmpty;
var
    Message_: string;
begin
    //  THE DAY NIST CHANGES ITS MARKUP. A scraper that answers "nothing found"
    //  then is indistinguishable from a service that holds nothing, and the
    //  user has no way to know which.
    FMockObject.Reply('Name=benzene',
        '<html><head><title>Search Results</title></head><body>' +
        '<p>Now rendered by a script.</p></body></html>');
    Message_ := '';
    try
        SearchFor('benzene');
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    AssertTrue('refused: "' + Message_ + '"', Pos('may have changed', Message_) > 0);
end;

procedure TNistSourceTest.ASubstanceOffersOnlyTheSpectraItHas;
var
    Substance: TDataSourceItem;
    Spectra: TDataSourceItems;
begin
    FMockObject.Reply('Name=benzene', Fixture('nist-species-benzene.html'));
    FMockObject.Reply('ID=C71432', Fixture('nist-species-benzene.html'));
    Substance := SearchFor('benzene')[0];
    Spectra := FSource.Children(Substance);
    //  The page's own anchors say which kinds NIST holds. Offering all three
    //  regardless would download "Spectrum not found" and blame the reader.
    AssertEquals('the three this page links', 3, Length(Spectra));
    AssertEquals('Infrared spectrum', Spectra[0].Title);
    AssertTrue('each is a file to fetch', Spectra[0].IsLeaf);
end;

procedure TNistSourceTest.ASpectrumIsNamedSoThatItsReaderIsChosen;
var
    Spectra: TDataSourceItems;
begin
    FMockObject.Reply('Name=benzene', Fixture('nist-species-benzene.html'));
    FMockObject.Reply('ID=C71432', Fixture('nist-species-benzene.html'));
    Spectra := FSource.Children(SearchFor('benzene')[0]);
    //  The address ends in a query and names no file, so the source states the
    //  name - and its extension is what the loader registry is asked about.
    AssertTrue('named as a JCAMP file: ' + Spectra[0].FileName,
        Pos('.jdx', LowerCase(Spectra[0].FileName)) > 0);
    AssertTrue('which this build has a reader for',
        FindDataLoaderClass(Spectra[0].FileName) = TJcampDxLoader);
end;

procedure TNistSourceTest.TheDownloadAsksForTheJcampOfThatSubstance;
var
    Spectra: TDataSourceItems;
    Dest: TStringStream;
begin
    FMockObject.Reply('Name=benzene', Fixture('nist-species-benzene.html'));
    FMockObject.Reply('ID=C71432', Fixture('nist-species-benzene.html'));
    FMockObject.Reply('JCAMP=C71432', '##TITLE=Benzene'#10'##END=');
    Spectra := FSource.Children(SearchFor('benzene')[0]);
    Dest := TStringStream.Create('');
    try
        FSource.Download(Spectra[0], Dest);
        AssertTrue('what came back is what was asked for',
            Pos('Benzene', Dest.DataString) > 0);
        //  The address is pinned because a source asking for the wrong one
        //  looks exactly like a service with nothing to say.
        AssertTrue(FMockObject.Log.AsText,
            Pos('Type=IR', FMockObject.Log.AsText) > 0);
    finally
        Dest.Free;
    end;
end;

procedure TNistSourceTest.TheModuleRegistersItsFormatAndItsSource;
begin
    //  What the front door is FOR. Called twice by SetUp across tests, which is
    //  also the idempotence a front door owes its host.
    RegisterOpenDataSpectraModule;
    AssertTrue('the JCAMP reader is installed',
        FindDataLoaderClass('spectrum.jdx') = TJcampDxLoader);
    AssertTrue('and the source that produces one',
        FindDataSourceClass(NistSourceId) = TNistWebBookSource);
end;

procedure TNistSourceTest.EverythingTheModuleRegistersExplainsItself;
var
    Findings: string;
begin
    RegisterOpenDataSpectraModule;
    //  THE WALK, over this build - which contains this module. A source
    //  registered without an explanation, or producing a file nothing here
    //  reads, fails by name.
    Findings := JoinedFindings(DataSourceFindings(@RegisteredTopicResolves));
    AssertEquals('', Findings);
end;

initialization
    RegisterTest('unit', TNistSourceTest);
end.
