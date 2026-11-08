// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A NIST WebBook spectrum becoming a project, through the surface a
user reaches.)

WHAT THE USER DOES, step by step, in the window's own calls: File > New Project
from Data Source, the NIST WebBook chosen (EnterChosenSource), a substance named
and Next pressed (Advance - the search, and the one substance it finds opened by
itself), a spectrum chosen and Next pressed again (the preview downloads the
JCAMP-DX file and reads it with this module's loader), and Create Project
(ImportDownload). The profile is then read back FROM A REAL fit_server.

The source's own tests pin which address it asks for and what it makes of each
answer. None of them reaches the join: a spectrum the wizard cannot choose, a
file named so that no reader is found for it, a reader the client's import does
not reach, a profile that never leaves the client. Each of those is a defect
this project has shipped in another source, with every half's tests green.

ONLY THE NETWORK IS FAKED, through the IWebClient the source is given, with the
pages recorded in fixtures/ - see fixtures/README.md. The wizard, the cache, the
file on disk, the loader, the client and the server are the real ones.
}
unit testcase_nist_e2e;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    worker_process_harness, title_points_set, fit_client_app,
    data_source, data_source_wizard, data_source_import, int_web_client,
    mock_web_client, testcase_data_source_e2e;

type
    TNistEndToEndTest = class(TWorkerProcessTest)
    private
        FApp: TFitClientApp;
        FMockObject: TMockWebClient;
        FWeb: IWebClient;
        FWizard: TDataSourceWizard;
        FHostObject: TRecordingImportHost;
        FHost: IImportHost;
        FCacheDir: string;
        function Fixture(const AName: string): string;
        procedure FindSubstance(const AName: string);
        function IndexOfTitle(const ATitle: string): longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnInfraredSpectrumReachesTheServerAsTheProfile;
        procedure TheProjectRecordsWhichSpectrumItWasMadeFrom;
        procedure ANameNobodyKnowsStopsAtTheQuestionAndTouchesNothing;
    end;

implementation

uses
    data_loader_registration, data_source_registration,
    open_data_spectra_module, nist_webbook_source;

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

{ A recorded answer, byte for byte: the spectrum is read as NIST wrote it, line
  endings included, rather than as a string list would rewrite it. }
function TNistEndToEndTest.Fixture(const AName: string): string;
var
    Path: string;
    Input: TFileStream;
begin
    Path := ExpandFileName(ExtractFilePath(ParamStr(0)) +
        '../Modules/open-data-spectra/tests/fixtures/' + AName);
    Input := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    try
        SetLength(Result, Input.Size);
        if Input.Size > 0 then
            Input.ReadBuffer(Result[1], Input.Size);
    finally
        Input.Free;
    end;
end;

procedure TNistEndToEndTest.SetUp;
begin
    inherited SetUp;
    //  WHAT THE CLIENT DOES AT START-UP: the framework's readers and sources,
    //  then the module's front door - which is the only thing that puts the
    //  JCAMP-DX reader and the WebBook into this build.
    RegisterAllDataLoaders;
    RegisterAllDataSources;
    RegisterOpenDataSpectraModule;
    FApp := TFitClientApp.Create;
    FApp.FitClient.FitService := FSvc;
    FMockObject := TMockWebClient.Create;
    FWeb := FMockObject;
    FCacheDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fit-nist-e2e';
    FWizard := TDataSourceWizard.Create(FWeb);
    FWizard.CacheRoot := FCacheDir;
    FHostObject := TRecordingImportHost.Create(FApp, True);
    FHost := FHostObject;

    //  The answers recorded in fixtures/: the search lands on benzene's own
    //  page, which is also what its id asks for, and the infrared spectrum.
    //  The download is matched on its Type, so a source asking for another
    //  kind than the one chosen finds no reply and fails by saying so.
    FMockObject.Reply('Name=benzene', Fixture('nist-species-benzene.html'));
    FMockObject.Reply('Name=quartz', Fixture('nist-not-found.html'));
    FMockObject.Reply('ID=C71432', Fixture('nist-species-benzene.html'));
    FMockObject.Reply('JCAMP=C71432&Index=0&Type=IR', Fixture('benzene-ir.jdx'));
end;

procedure TNistEndToEndTest.TearDown;
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

{ The source chosen, the substance named, Next pressed. }
procedure TNistEndToEndTest.FindSubstance(const AName: string);
begin
    FWizard.ChooseSource(NistSourceId);
    FWizard.EnterChosenSource;
    FWizard.SetField(NistNameFieldId, AName);
    FWizard.Advance;
end;

function TNistEndToEndTest.IndexOfTitle(const ATitle: string): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to FWizard.ItemCount - 1 do
        if FWizard.Item(i).Title = ATitle then
            Exit(i);
end;

procedure TNistEndToEndTest.AnInfraredSpectrumReachesTheServerAsTheProfile;
var
    Infrared: longint;
    Fetched: string;
    FromServer: TTitlePointsSet;
begin
    FindSubstance('benzene');
    //  ONE SUBSTANCE IS NOTHING TO CHOOSE BETWEEN: the wizard went into it, and
    //  the list is its spectra.
    AssertTrue('inside the substance', FWizard.InsideContainer);
    Infrared := IndexOfTitle('Infrared spectrum');
    AssertTrue('the infrared spectrum is offered', Infrared >= 0);
    AssertTrue('and may be chosen: ' + FWizard.ChooseVerdict(Infrared).Reason,
        FWizard.SelectIfAllowed(Infrared));
    FWizard.Advance;
    AssertTrue('Next reached a preview that needs the file',
        FWizard.NeedsDownload);
    FWizard.Download;
    AssertTrue('the preview read the spectrum: ' + FWizard.PreviewError,
        FWizard.CreateVerdict.Allowed);
    Fetched := FWizard.FilePath;
    AssertEquals('kept under the name that chose its reader', '.jdx',
        LowerCase(ExtractFileExt(Fetched)));

    AssertTrue('the project was made',
        ImportDownload(FHost, Fetched, FWizard.Origin));

    //  THE ASSERTION THAT MATTERS, asked of the server: the spectrum NIST
    //  published, every point and its scale, and not the client's copy of it.
    FromServer := FSvc.GetProfilePointsSet;
    try
        AssertTrue('the server has a profile', FromServer <> nil);
        AssertEquals('every point NIST published', 880, FromServer.PointsCount);
        AssertEquals('from ##FIRSTX', 450, FromServer.PointXCoord[0], 1E-6);
        AssertEquals('to ##LASTX', 3966,
            FromServer.PointXCoord[FromServer.PointsCount - 1], 1E-6);
        AssertEquals('at ##FIRSTY, scaled by ##YFACTOR', 0.033074,
            FromServer.PointYCoord[0], 1E-6);
    finally
        FromServer.Free;
    end;
    AssertFalse('and the download is gone, the project holding the data',
        FileExists(Fetched));
end;

procedure TNistEndToEndTest.TheProjectRecordsWhichSpectrumItWasMadeFrom;
begin
    FindSubstance('benzene');
    FWizard.SelectIfAllowed(IndexOfTitle('Infrared spectrum'));
    FWizard.Advance;
    FWizard.Download;
    AssertTrue('the project was made',
        ImportDownload(FHost, FWizard.FilePath, FWizard.Origin));
    //  WHAT A READER OF A PUBLISHED RESULT NEEDS: which service, what was asked
    //  of it, and the address of the very file - so the spectrum can be
    //  fetched again and checked.
    AssertEquals('the source', NistSourceId, FHostObject.Origin.SourceId);
    AssertTrue('the substance asked for: ' + FHostObject.Origin.Query,
        Pos('benzene', FHostObject.Origin.Query) > 0);
    AssertTrue('the spectrum''s own address: ' + FHostObject.Origin.Address,
        (Pos('JCAMP=C71432', FHostObject.Origin.Address) > 0) and
        (Pos('Type=IR', FHostObject.Origin.Address) > 0));
    AssertTrue('and when', FHostObject.Origin.RetrievedAt > 0);
end;

procedure TNistEndToEndTest.ANameNobodyKnowsStopsAtTheQuestionAndTouchesNothing;
var
    Message_: string;
begin
    Message_ := '';
    try
        FindSubstance('quartz');
    except
        on E: EDataSourceError do
            Message_ := E.Message;
    end;
    //  THE SOURCE'S REFUSAL, in its own words, where the user typed the name:
    //  an empty list on the next page would read as a build that cannot search.
    AssertTrue('refused: "' + Message_ + '"', Pos('quartz', Message_) > 0);
    AssertTrue('the user is still at the question', FWizard.Step = wsFind);
    AssertEquals('and the project was not touched', '',
        Trim(FHostObject.Log.Text));
    AssertEquals('nothing was downloaded', 0, FMockObject.Log.CountOf('Download'));
end;

initialization
    RegisterTest('integration', TNistEndToEndTest);
end.
