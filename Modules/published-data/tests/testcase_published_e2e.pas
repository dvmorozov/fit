// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Published data becoming a project, through the surface a user
reaches: the wizard's own calls, a real download onto disk, the ordinary
import, and a real fit_server asked what it holds.)

THE GAP THIS CLOSES (backlog: "the DOI and NIST sources are never tested
through the wizard's import path"). Every step below is the window's: choosing
a row is EnterChosenSource, Next is Advance, the preview downloads when
NeedsDownload says so, and Create Project is ImportDownload. Only the network
is faked; the archive is unpacked, the table read, the column chosen and the
profile sent to a real server by the code the application runs.
}
unit testcase_published_e2e;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    worker_process_harness, title_points_set, fit_client_app,
    data_source_wizard, data_source_import, int_web_client, mock_web_client,
    testcase_data_source_e2e;

type
    TPublishedEndToEndTest = class(TWorkerProcessTest)
    private
        FApp: TFitClientApp;
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        FHostObject: TRecordingImportHost;
        FHost: IImportHost;
        FCacheDir: string;
        function ServerValues: TTitlePointsSet;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AWordSearchedTableInAnArchiveReachesTheServer;
        procedure TheColumnChosenInThePreviewIsTheProfile;
        procedure ACatalogKindsDatasetReachesTheServer;
    end;

implementation

uses
    data_loader_registration, data_source_registration, published_data_module,
    doi_source, catalog_source, testcase_archives;

const
    Table = 'angle'#9'intensity'#9'background' + LineEnding +
        '10'#9'100'#9'5' + LineEnding + '20'#9'200'#9'6' + LineEnding +
        '30'#9'300'#9'7' + LineEnding;

procedure RemoveTree(const APath: string);
var
    Found: TSearchRec;
    Dir: string;
begin
    if (APath = '') or not DirectoryExists(APath) then
        Exit;
    Dir := IncludeTrailingPathDelimiter(APath);
    if FindFirst(Dir + '*', faAnyFile, Found) = 0 then
        try
            repeat
                if (Found.Name = '.') or (Found.Name = '..') then
                    Continue;
                if (Found.Attr and faDirectory) <> 0 then
                    RemoveTree(Dir + Found.Name)
                else
                    DeleteFile(Dir + Found.Name);
            until FindNext(Found) <> 0;
        finally
            FindClose(Found);
        end;
    RemoveDir(APath);
end;

procedure TPublishedEndToEndTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    RegisterPublishedDataModule;
    FApp := TFitClientApp.Create;
    FApp.FitClient.FitService := FSvc;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FCacheDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-published-e2e';
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.CacheRoot := FCacheDir;
    FHostObject := TRecordingImportHost.Create(FApp, True);
    FHost := FHostObject;

    //  A Zenodo record holding a zipped table and a paper.
    FMockObject.Reply('zenodo.org/api/records?', '{"hits":{"total":1,"hits":[' +
        '{"id":71,"metadata":{"title":"Calcite XRD"},"files":[' +
        '{"key":"calcite.zip","size":10,"links":{"self":"https://z/calcite.zip"}},' +
        '{"key":"paper.pdf","size":10,"links":{"self":"https://z/paper.pdf"}}]}]}}');
    FMockObject.Reply('zenodo.org/api/records/71', '{"files":[' +
        '{"key":"calcite.zip","size":10,"links":{"self":"https://z/calcite.zip"}},' +
        '{"key":"paper.pdf","size":10,"links":{"self":"https://z/paper.pdf"}}]}');
    FMockObject.Reply('https://z/calcite.zip', ZipOf(['run/calcite.tsv', Table,
        'run/notes.pdf', 'x']));
    FMockObject.Reply('api.figshare.com/v2/articles/search', '[]');

end;

procedure TPublishedEndToEndTest.TearDown;
begin
    FHost := nil;
    FreeAndNil(FHostObject);
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    FreeAndNil(FApp);
    RemoveTree(FCacheDir);
    inherited TearDown;
end;

function TPublishedEndToEndTest.ServerValues: TTitlePointsSet;
begin
    Result := FSvc.GetProfilePointsSet;
    AssertTrue('the server has a profile', Result <> nil);
end;

procedure TPublishedEndToEndTest.AWordSearchedTableInAnArchiveReachesTheServer;
var
    FromServer: TTitlePointsSet;
begin
    FWizard.ChooseSource(DoiSourceId);
    FWizard.EnterChosenSource;
    FWizard.SetField(DoiFieldId, 'calcite');
    //  Next: the search, and its one record opened by itself.
    FWizard.Advance;
    AssertTrue('the record''s files', FWizard.InsideContainer);
    AssertEquals('the pdf is not listed', 1, FWizard.ItemCount);
    AssertEquals('calcite.zip', FWizard.Item(0).Title);
    //  The archive is a folder: Next goes into it.
    FWizard.SelectIfAllowed(0);
    FWizard.Advance;
    AssertEquals('only the table inside it', 1, FWizard.ItemCount);
    AssertTrue(FWizard.SelectIfAllowed(0));
    FWizard.Advance;
    AssertTrue('at the preview', FWizard.NeedsDownload);
    FWizard.Download;
    AssertTrue('read: ' + FWizard.PreviewError, FWizard.CreateVerdict.Allowed);
    AssertTrue(ImportDownload(FHost, FWizard.FilePath, FWizard.Origin));
    FromServer := ServerValues;
    try
        AssertEquals(3, FromServer.PointsCount);
        AssertEquals(300, FromServer.PointYCoord[2], 1e-9);
    finally
        FromServer.Free;
    end;
end;

procedure TPublishedEndToEndTest.TheColumnChosenInThePreviewIsTheProfile;
var
    FromServer: TTitlePointsSet;
begin
    FWizard.ChooseSource(DoiSourceId);
    FWizard.EnterChosenSource;
    FWizard.SetField(DoiFieldId, 'calcite');
    FWizard.Advance;
    FWizard.SelectIfAllowed(0);
    FWizard.Advance;
    FWizard.SelectIfAllowed(0);
    FWizard.Advance;
    FWizard.Download;
    AssertEquals('intensity', FWizard.ValueColumns[0]);
    FWizard.ChooseValueColumn(1);
    AssertTrue(ImportDownload(FHost, FWizard.FilePath, FWizard.Origin));
    FromServer := ServerValues;
    try
        AssertEquals('the background, as chosen', 7, FromServer.PointYCoord[2], 1e-9);
    finally
        FromServer.Free;
    end;
    AssertTrue('and the origin says so: ' + FHostObject.Origin.Query,
        Pos('value column background', FHostObject.Origin.Query) > 0);
end;

procedure TPublishedEndToEndTest.ACatalogKindsDatasetReachesTheServer;
var
    FromServer: TTitlePointsSet;
    i, Archives: longint;
begin
    FWizard.ChooseSource(CatalogSourceId);
    FWizard.EnterChosenSource;
    Archives := -1;
    for i := 0 to FWizard.ItemCount - 1 do
        if Pos('Archives', FWizard.Item(i).Title) = 1 then
            Archives := i;
    FWizard.SelectIfAllowed(Archives);
    FWizard.Advance;
    AssertEquals('Calcite XRD', FWizard.Item(0).Title);
    FWizard.SelectIfAllowed(0);
    FWizard.Advance;
    FWizard.SelectIfAllowed(0);
    FWizard.Advance;
    FWizard.SelectIfAllowed(0);
    FWizard.Advance;
    FWizard.Download;
    AssertTrue(ImportDownload(FHost, FWizard.FilePath, FWizard.Origin));
    FromServer := ServerValues;
    try
        AssertEquals(3, FromServer.PointsCount);
    finally
        FromServer.Free;
    end;
    AssertEquals(CatalogSourceId, FHostObject.Origin.SourceId);
end;

initialization
    RegisterTest('integration', TPublishedEndToEndTest);
end.
