// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A data source becoming a project, through the surface a user reaches.)

THE TEST THIS WHOLE FEATURE WAS WRITTEN AGAINST, and it was red first. It starts
where the user's gesture starts - the wizard - and ends where the data has to
end up: inside a real fit_server, over HTTP, read back from it.

WHY NOT IN PROCESS. Every "worked in tests, not in the app" defect recorded in
findings.md was correct code that the production path never ran: a loader the
injector never reached, a profile that never left the client, a unit linked into
the test binary and not into the server. A test that stops at the client's own
copy of the profile proves none of that. This one asks the server.

WHAT IS AND IS NOT FAKED. The network is faked - and only the network: the
source, the cache, the file on disk, the loader, the client and the server are
all the real ones. A test that reached a public service would fail when that
service is busy and pass tomorrow for reasons nobody here decided.
}
unit testcase_data_source_e2e;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    worker_process_harness, http_fit_service, title_points_set,
    fit_client_app, data_loader_registration, data_source,
    data_source_registration, data_source_wizard, data_source_import,
    url_source, samples_source, int_web_client, mock_web_client;

type
    { The window's part of an import, as the test can hold it: exactly the four
      things IImportHost names, recorded so the ORDER can be asserted - the
      origin must be recorded after the import, because the import fills
      provenance from the file it read. }
    TRecordingImportHost = class(TObject, IImportHost)
    private
        FApp: TFitClientApp;
        FLog: TStringList;
        FOrigin: TDownloadOrigin;
        FAllow: boolean;
    public
        constructor Create(AApp: TFitClientApp; AAllow: boolean);
        destructor Destroy; override;
        function MayReplaceDocument: boolean;
        procedure NewProject;
        procedure ImportProfileFile(const APath: string);
        procedure RememberOrigin(const AOrigin: TDownloadOrigin);
        { Where downloads are kept. This fixture keeps none between runs: it
          points the wizard at a directory of its own. }
        function DownloadFolder: string;
        procedure RememberDownloadFolder(const APath: string);
        function AsObject: TObject;
        property Log: TStringList read FLog;
        property Origin: TDownloadOrigin read FOrigin;
    end;

    TDataSourceEndToEndTest = class(TWorkerProcessTest)
    private
        FApp: TFitClientApp;
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        FHostObject: TRecordingImportHost;
        FHost: IImportHost;
        FCacheDir: string;
        procedure DownloadTheFixture;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure DownloadedDataReachesTheServerAsTheProfile;
        procedure TheProjectRecordsWhereTheDataCameFrom;
        procedure DecliningToDiscardKeepsTheDownload;
        procedure TheInstalledSamplesReachTheServerWithNoNetworkAtAll;
    end;

implementation

const
    { Four points, in the shape a two-column data file has. }
    FixtureBody = '10 100' + LineEnding + '20 200' + LineEnding +
        '30 300' + LineEnding + '40 400' + LineEnding;
    FixtureUrl = 'https://example.org/data/probe.dat';

{ Removes the test's own cache directory and what this test put in it. Written
  out rather than taken from FileUtil, which is LCL: this suite also builds
  without Lazarus. }
procedure RemoveCacheDir(const APath: string);
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
                    RemoveCacheDir(Dir + Found.Name)
                else
                    DeleteFile(Dir + Found.Name);
            until FindNext(Found) <> 0;
        finally
            FindClose(Found);
        end;
    RemoveDir(APath);
end;

constructor TRecordingImportHost.Create(AApp: TFitClientApp; AAllow: boolean);
begin
    inherited Create;
    FApp := AApp;
    FAllow := AAllow;
    FLog := TStringList.Create;
end;

destructor TRecordingImportHost.Destroy;
begin
    FLog.Free;
    inherited Destroy;
end;

function TRecordingImportHost.MayReplaceDocument: boolean;
begin
    FLog.Add('MayReplaceDocument');
    Result := FAllow;
end;

procedure TRecordingImportHost.NewProject;
begin
    FLog.Add('NewProject');
end;

procedure TRecordingImportHost.ImportProfileFile(const APath: string);
begin
    FLog.Add('ImportProfileFile');
    //  THE ORDINARY IMPORT, the one File > Import Profile uses: the loader is
    //  chosen by extension, the profile is copied into the client and pushed
    //  to the server.
    FApp.FitClient.LoadDataSet(APath);
end;

procedure TRecordingImportHost.RememberOrigin(const AOrigin: TDownloadOrigin);
begin
    FLog.Add('RememberOrigin');
    FOrigin := AOrigin;
end;

function TRecordingImportHost.DownloadFolder: string;
begin
    //  '' is "the user never chose one", which is this fixture's case too.
    Result := '';
end;

procedure TRecordingImportHost.RememberDownloadFolder(const APath: string);
begin
    //  Nothing remembers anything between runs here.
end;

function TRecordingImportHost.AsObject: TObject;
begin
    Result := Self;
end;

procedure TDataSourceEndToEndTest.SetUp;
begin
    inherited SetUp;
    RegisterAllDataLoaders;
    RegisterAllDataSources;

    FApp := TFitClientApp.Create;
    FApp.FitClient.FitService := FSvc;

    FMockObject := TMockWebClient.Create;
    FMockObject.Reply('example.org/data/probe.dat', FixtureBody);
    FWeb := FMockObject;

    FCacheDir := IncludeTrailingPathDelimiter(GetTempDir) +
        'fit-data-source-test';
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.CacheRoot := FCacheDir;

    FHostObject := TRecordingImportHost.Create(FApp, True);
    FHost := FHostObject;
end;

procedure TDataSourceEndToEndTest.TearDown;
begin
    FHost := nil;
    FreeAndNil(FHostObject);
    FreeAndNil(FWizard);
    FWeb := nil;
    FreeAndNil(FMockObject);
    FreeAndNil(FApp);
    //  Left behind, the next run's download would land beside it under a
    //  stepped-over name - correct, and confusing to read in a failure.
    RemoveCacheDir(FCacheDir);
    inherited TearDown;
end;

{ What the user does: choose the source, answer its question, take the file it
  found, and look at it. }
procedure TDataSourceEndToEndTest.DownloadTheFixture;
begin
    FWizard.ChooseSource(UrlSourceId);
    FWizard.SetField(UrlFieldId, FixtureUrl);
    FWizard.Search;
    FWizard.Select(0);
    FWizard.Download;
end;

procedure TDataSourceEndToEndTest.DownloadedDataReachesTheServerAsTheProfile;
var
    FromServer: TTitlePointsSet;
begin
    DownloadTheFixture;
    AssertTrue('the preview parsed the download: ' + FWizard.PreviewError,
        FWizard.Preview <> nil);
    AssertTrue('and the wizard would create a project from it',
        FWizard.CreateVerdict.Allowed);

    AssertTrue('the import ran', ImportDownload(FHost, FWizard.FilePath,
        FWizard.Origin));

    //  THE ASSERTION THAT MATTERS: read the profile back FROM THE SERVER. A
    //  client-side copy would be true of a build whose data never left the
    //  client, which is a defect this project has actually shipped.
    FromServer := FSvc.GetProfilePointsSet;
    try
        AssertTrue('the server has a profile', FromServer <> nil);
        AssertEquals('every point of the downloaded file reached it', 4,
            FromServer.PointsCount);
        AssertEquals('with its first argument', 10, FromServer.PointXCoord[0], 1E-9);
        AssertEquals('and its last value', 400, FromServer.PointYCoord[3], 1E-9);
    finally
        FromServer.Free;
    end;
end;

procedure TDataSourceEndToEndTest.TheProjectRecordsWhereTheDataCameFrom;
begin
    DownloadTheFixture;
    ImportDownload(FHost, FWizard.FilePath, FWizard.Origin);

    //  The order is the contract: provenance is filled by the import from the
    //  file it read - which for a download is a name in a cache directory -
    //  so the origin is recorded after it.
    AssertEquals('asked, started, imported, then recorded',
        'MayReplaceDocument;NewProject;ImportProfileFile;RememberOrigin',
        StringReplace(Trim(FHostObject.Log.Text), LineEnding, ';',
        [rfReplaceAll]));

    AssertEquals('the source is named', UrlSourceId,
        FHostObject.Origin.SourceId);
    AssertEquals('and the address it came from', FixtureUrl,
        FHostObject.Origin.Address);
    AssertTrue('and when', FHostObject.Origin.RetrievedAt > 0);
end;

procedure TDataSourceEndToEndTest.DecliningToDiscardKeepsTheDownload;
var
    Declining: TRecordingImportHost;
    DecliningHost: IImportHost;
begin
    DownloadTheFixture;
    Declining := TRecordingImportHost.Create(FApp, False);
    DecliningHost := Declining;
    try
        AssertFalse('a user who will not discard their work imports nothing',
            ImportDownload(DecliningHost, FWizard.FilePath, FWizard.Origin));
        AssertEquals('and nothing was touched', 'MayReplaceDocument',
            Trim(Declining.Log.Text));
        //  The download is still in hand, so saying no costs nothing fetched:
        //  the user can save their work and press Create again.
        AssertTrue('the downloaded file is still there',
            FileExists(FWizard.FilePath));
    finally
        DecliningHost := nil;
        Declining.Free;
    end;
end;

procedure TDataSourceEndToEndTest.TheInstalledSamplesReachTheServerWithNoNetworkAtAll;
var
    FromServer: TTitlePointsSet;
    i, Chosen: longint;
begin
    //  THE PATH A BUILD WITH NO CONNECTION TAKES, end to end. Every other
    //  source is driven over a faked network; this one is driven over none at
    //  all, which is the only way to prove the wizard, the cache, the loader
    //  and the import work without one.
    FWizard.ChooseSource(SamplesSourceId);
    FWizard.Search;
    AssertTrue('this checkout has samples installed beside it',
        FWizard.ItemCount > 0);

    //  A .dat sample: the profile shapes, rather than the price series, so the
    //  assertion below is about a measurement.
    Chosen := -1;
    for i := 0 to FWizard.ItemCount - 1 do
        if LowerCase(ExtractFileExt(FWizard.Item(i).FileName)) = '.dat' then
        begin
            Chosen := i;
            Break;
        end;
    AssertTrue('a .dat sample is among them', Chosen >= 0);

    FWizard.Select(Chosen);
    FWizard.Download;
    AssertTrue('the sample was read: ' + FWizard.PreviewError,
        FWizard.Preview <> nil);
    AssertTrue('the import ran', ImportDownload(FHost, FWizard.FilePath,
        FWizard.Origin));

    FromServer := FSvc.GetProfilePointsSet;
    try
        AssertTrue('the sample reached the server', FromServer <> nil);
        AssertTrue('with its points', FromServer.PointsCount > 10);
    finally
        FromServer.Free;
    end;
    //  And nothing was fetched: a source that needs no network must not touch
    //  one, or a build with no connection would fail here rather than in a
    //  place a user can see.
    AssertEquals('no request was made', 0,
        FMockObject.Log.CountOf('Download') + FMockObject.Log.CountOf('GetText'));
end;

initialization
    RegisterTest('integration', TDataSourceEndToEndTest);
end.
