// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The data catalog: one row per kind of data this build reads, a
kind's newest datasets asked of both repositories, browsed through the wizard
as the window does.)
}
unit testcase_catalog_source;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    data_source, data_source_wizard, catalog_source, published_repositories,
    int_web_client, mock_web_client, data_source_view;

type
    TCatalogSourceTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FSource: TCatalogSource;
        function KindNamed(const APrefix: string): TDataSourceItem;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ItAsksNothingAndHoldsFolders;
        procedure ItSaysWhichRepositoriesItBrowses;
        procedure TheKindsAreTheReadersThisBuildHas;
        procedure EachKindIsNamedWithWhatItReads;
        procedure ArchivesAreAKindToo;
        procedure EachKindSaysWhatItsFilesHold;
        procedure PlainTextIsNoKindOfItsOwn;
        procedure TheCatalogAsksNoServiceForItsKinds;
        procedure AKindOpensToTheNewestDatasetsOnBoth;
        procedure AKindWithNothingSaysSo;
        procedure ARecordInAKindOpensToItsFiles;
        procedure AKindsReferenceReadsBackWhateverItsName;
        procedure AMoreRowOfAKindAsksForTheSameKind;
    end;

    { The catalog where the user meets it. }
    TCatalogWizardTest = class(TTestCase)
    private
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ChoosingTheCatalogListsTheKindsAtOnce;
        procedure NextOnAKindGoesIntoItAndBackComesOut;
    end;

implementation

uses
    data_loader_registration, data_source_registration, published_data_module,
    data_loader_registry;

const
    ZenodoOfKind = '{"hits":{"total":1,"hits":[' +
        '{"id":41,"metadata":{"title":"Spectra of salts"},' +
        '"files":[{"key":"s.tsv","size":3,"links":{"self":"https://z/s.tsv"}}]}]}}';
    FigshareOfKind = '[{"id":801,"title":"Raman of calcite"}]';
    Figshare801 = '{"id":801,"files":[{"id":5,"name":"r.tsv","size":3,' +
        '"download_url":"https://f/5"}]}';

procedure RegisterEverything;
begin
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    RegisterPublishedDataModule;
end;

procedure TCatalogSourceTest.SetUp;
begin
    inherited SetUp;
    RegisterEverything;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FSource := TCatalogSource.Create(FWeb);
end;

procedure TCatalogSourceTest.TearDown;
begin
    FreeAndNil(FSource);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

function TCatalogSourceTest.KindNamed(const APrefix: string): TDataSourceItem;
var
    Items: TDataSourceItems;
    i: longint;
begin
    Items := FSource.Search(nil);
    for i := 0 to High(Items) do
        if Pos(APrefix, Items[i].Title) = 1 then
            Exit(Items[i]);
    Fail('no kind called ' + APrefix);
end;

procedure TCatalogSourceTest.ItAsksNothingAndHoldsFolders;
var
    Info: TDataSourceInfo;
begin
    Info := TCatalogSource.Info;
    AssertEquals('nothing to type', 0, Length(Info.QueryFields));
    AssertTrue('its rows are folders', Info.HasContainers);
    AssertTrue('each saying what it opens to',
        FSource.Search(nil)[0].Details <> '');
    AssertEquals('Repositories', Info.Category);
end;

procedure TCatalogSourceTest.ItSaysWhichRepositoriesItBrowses;
begin
    AssertTrue(TCatalogSource.Info.Summary,
        Pos(SearchedRepositories, TCatalogSource.Info.Summary) > 0);
end;

procedure TCatalogSourceTest.TheKindsAreTheReadersThisBuildHas;
var
    Kinds: TDataKinds;
    i: longint;
    Seen: boolean;
begin
    //  The framework's table reader is registered here; its row says so.
    Kinds := DataKinds;
    Seen := False;
    for i := 0 to High(Kinds) do
        if Pos('Table of numbers', Kinds[i].Title) = 1 then
            Seen := True;
    AssertTrue('a reader of this build is a row', Seen);
end;

procedure TCatalogSourceTest.EachKindIsNamedWithWhatItReads;
begin
    //  .tsv at least: a price module may hold .csv in the build this runs in.
    AssertTrue(KindNamed('Table of numbers').Title,
        Pos('.tsv)', KindNamed('Table of numbers').Title) > 0);
end;

procedure TCatalogSourceTest.EachKindSaysWhatItsFilesHold;
begin
    AssertEquals('the format''s own description',
        DataFormatDescription('Table of numbers'),
        KindNamed('Table of numbers').Details);
    AssertTrue(KindNamed('Archives').Details,
        Pos('opened like folders', KindNamed('Archives').Details) > 0);
end;

procedure TCatalogSourceTest.ArchivesAreAKindToo;
var
    Kinds: TDataKinds;
begin
    Kinds := DataKinds;
    AssertEquals('Archives (.zip, .gz, .tar, .tgz)', Kinds[High(Kinds)].Title);
    AssertEquals('zip,gz,tar,tgz', Kinds[High(Kinds)].Extensions);
end;

procedure TCatalogSourceTest.PlainTextIsNoKindOfItsOwn;
var
    Kinds: TDataKinds;
    i: longint;
begin
    Kinds := DataKinds;
    for i := 0 to High(Kinds) do
        AssertEquals('every dataset has a README.txt: ' + Kinds[i].Title, 0,
            Pos('txt', Kinds[i].Extensions));
end;

procedure TCatalogSourceTest.TheCatalogAsksNoServiceForItsKinds;
begin
    FSource.Search(nil);
    AssertEquals('drawn from this build''s readers', '',
        Trim(FMockObject.Log.AsText));
end;

procedure TCatalogSourceTest.AKindOpensToTheNewestDatasetsOnBoth;
var
    Datasets: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', ZenodoOfKind);
    FMockObject.Reply('api.figshare.com/v2/articles/search', FigshareOfKind);
    FMockObject.Reply('api.figshare.com/v2/articles/801', Figshare801);
    Datasets := FSource.Children(KindNamed('Table of numbers'));
    AssertEquals(2, Length(Datasets));
    AssertEquals('zenodo:41', Datasets[0].Ref);
    AssertEquals('figshare:801', Datasets[1].Ref);
    AssertTrue('Zenodo, for records holding such a file, newest first: ' +
        FMockObject.Log.AsText,
        Pos('sort=newest', FMockObject.Log.AsText) > 0);
    AssertTrue(FMockObject.Log.AsText,
        Pos('files.entries.key%3A%28', FMockObject.Log.AsText) > 0);
    AssertTrue(FMockObject.Log.AsText,
        Pos('%2A.tsv%29', FMockObject.Log.AsText) > 0);
    AssertTrue('figshare, by extension, newest first: ' + FMockObject.LastJson,
        Pos(':extension: tsv', FMockObject.LastJson) > 0);
    AssertTrue(FMockObject.LastJson,
        Pos('"order_direction":"desc"', FMockObject.LastJson) > 0);
end;

procedure TCatalogSourceTest.AKindWithNothingSaysSo;
var
    Message_: string;
begin
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":0,"hits":[]}}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Message_ := '';
    try
        FSource.Children(KindNamed('Table of numbers'));
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    AssertTrue(Message_, Pos('hold no dataset with Table of numbers', Message_) > 0);
end;

procedure TCatalogSourceTest.ARecordInAKindOpensToItsFiles;
var
    Datasets, Files: TDataSourceItems;
begin
    FMockObject.Reply('zenodo.org/api/records?', ZenodoOfKind);
    FMockObject.Reply('zenodo.org/api/records/41', '{"files":[{"key":"s.tsv",' +
        '"size":3,"links":{"self":"https://z/s.tsv"}}]}');
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    Datasets := FSource.Children(KindNamed('Table of numbers'));
    Files := FSource.Children(Datasets[0]);
    AssertEquals('s.tsv', Files[0].FileName);
    AssertTrue(Files[0].IsLeaf);
end;

procedure TCatalogSourceTest.AKindsReferenceReadsBackWhateverItsName;
var
    Kind, Back: TDataKind;
begin
    Kind.Title := 'Odd: name (.a)';
    Kind.Extensions := 'a,b';
    AssertTrue(ReadKindRef(KindRef(Kind), Back));
    AssertEquals('a,b', Back.Extensions);
    AssertEquals('Odd: name (.a)', Back.Title);
    AssertFalse(ReadKindRef('zenodo:1', Back));
end;

procedure TCatalogSourceTest.AMoreRowOfAKindAsksForTheSameKind;
var
    Query, Back: TDatasetQuery;
    Repository: string;
    Page: longint;
begin
    Query := Default(TDatasetQuery);
    Query.Kind := dqKind;
    Query.Extensions := 'csv,tsv';
    Query.Words := 'Table: of numbers';
    AssertTrue(ReadMoreRef(MoreRef('Zenodo', 2, Query), Repository, Page, Back));
    AssertTrue(Back.Kind = dqKind);
    AssertEquals('csv,tsv', Back.Extensions);
    AssertEquals('Table: of numbers', Back.Words);
end;

{ --------------------------------- wizard ------------------------------------ }

procedure TCatalogWizardTest.SetUp;
begin
    inherited SetUp;
    RegisterEverything;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FWizard := TDataSourceWizard.Create(FWeb);
end;

procedure TCatalogWizardTest.TearDown;
begin
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    inherited TearDown;
end;

procedure TCatalogWizardTest.ChoosingTheCatalogListsTheKindsAtOnce;
begin
    //  WHAT CHOOSING IT IN THE WINDOW'S LIST DOES.
    FWizard.ChooseSource(CatalogSourceId);
    FWizard.EnterChosenSource;
    AssertTrue('straight to the list', FWizard.Step = wsChoose);
    AssertEquals(Length(DataKinds), FWizard.ItemCount);
    AssertFalse('a kind has no size, so the column gives way',
        AnyRowHasASize(FWizard));
end;

procedure TCatalogWizardTest.NextOnAKindGoesIntoItAndBackComesOut;
var
    i, Tables: longint;
begin
    FMockObject.Reply('zenodo.org/api/records?', ZenodoOfKind);
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');
    FWizard.ChooseSource(CatalogSourceId);
    FWizard.EnterChosenSource;
    Tables := -1;
    for i := 0 to FWizard.ItemCount - 1 do
        if Pos('Table of numbers', FWizard.Item(i).Title) = 1 then
            Tables := i;
    FWizard.SelectIfAllowed(Tables);
    FWizard.Advance;
    AssertTrue('inside the kind', FWizard.InsideContainer);
    AssertEquals('Spectra of salts', FWizard.Item(0).Title);
    AssertTrue('a dataset shows the size of its readable files',
        AnyRowHasASize(FWizard));
    AssertTrue('and Back can be pressed to come out',
        WizardView(FWizard).BackEnabled);
    FWizard.GoBack;
    AssertFalse('back among the kinds', FWizard.InsideContainer);
end;

initialization
    RegisterTest('unit', TCatalogSourceTest);
    RegisterTest('unit', TCatalogWizardTest);
end.
