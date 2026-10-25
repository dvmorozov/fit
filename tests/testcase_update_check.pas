// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Checking for an update, asking, downloading, verifying and handing
the installer over - through the calls Help > Check for Updates makes.)

The checker is driven as the window drives it: a web client serving a
recorded feed, a host standing where the window stands (it answers the
question, records what it was told, and records what it was asked to install),
the module preferences in memory, and a clock of the test's own. Downloading
writes a file, so the class that does is an integration test.
}
unit testcase_update_check;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, Math, fpcunit, testregistry, int_web_client,
    web_client, curl_client, mock_web_client, module_preferences, update_feed,
    update_check, sha256_digest, product_identity;

type
    TScriptedUpdateHost = class(TObject, IUpdateHost)
    public
        Answer: TUpdateAnswer;
        Asked, Told, Installed: TStringList;
        { Everything the checker did to the host, in order: ask, tell,
          install, quit, done, and progress:<stage>:<bytes>/<total>. }
        Events: TStringList;
        InstallWorks, Quit_: boolean;
        { The checker a progress report in CancelIn cancels, as the window's
          Cancel would - a download only once some of it has arrived. }
        Checker: TUpdateChecker;
        CancelIn: set of TUpdateStage;
        constructor Create;
        destructor Destroy; override;
        function AskToInstall(const AVersion, ANotes: string): TUpdateAnswer;
        procedure Tell(const AText: string);
        function Install(const AFile: string; APlatform: TInstallerPlatform;
            out AWhy: string): boolean;
        procedure Quit;
        procedure Progress(const AProgress: TUpdateProgress);
        procedure ProgressDone;
    end;

    { What the feed and the installer are served as, by address ending. }
    TServedFiles = class
    public
        Names, Bodies: TStringList;
        constructor Create;
        destructor Destroy; override;
    end;

    { The real curl client with its process replaced: it serves from the
      table in chunks of four bytes, asking the caller after each one, as
      curl's output is read. }
    TServingCurl = class(TCurlClient)
    private
        FFiles: TServedFiles;
    protected
        function Run(const AUrl: string; ADest: TStream;
            AOnChunk: TCurlChunkEvent; out AHeaders, AStderr: string;
            out AStopped: boolean): longint; override;
    end;

    { THE WEB CLIENT THE APPLICATION USES - its cap, its guard, its progress
      and its words - over a curl whose process is the table above. The cap
      that refused the update was in this class, so a double of the
      interface could never have shown it. }
    TServingWebClient = class(TWebClient)
    private
        FFiles: TServedFiles;
    protected
        function CreateCurl: TCurlClient; override;
    public
        constructor Create(AFiles: TServedFiles);
    end;

    TUpdateCheckTest = class(TTestCase)
    protected
        FWebObject: TMockWebClient;
        FWeb: IWebClient;
        FHostObject: TScriptedUpdateHost;
        FHost: IUpdateHost;
        FChecker: TUpdateChecker;
        procedure SetUp; override;
        procedure TearDown; override;
        procedure Serve(const AVersion, ASha: string);
    published
        procedure TheDefaultClockIsTheSystemsInUtc;
        procedure AskedWithNothingNewItSaysSo;
        procedure AnUnreachableFeedIsSaidWhenAskedAndQuietOtherwise;
        procedure AnAutomaticCheckWithinSixHoursDoesNotReachTheNetwork;
        procedure ASkippedVersionIsNotOfferedAgainByItself;
        procedure ABuildWithNoUpdateSourceSaysSoWhenAsked;
        procedure ABuildIsUpdatedAsTheProductItsIdentityNames;
        procedure AFeedSetInThePreferencesIsTheOneAsked;
        procedure AReleaseAModuleHoldsBackIsAnnouncedNotOffered;
        procedure LaterChangesNothing;
        procedure AutomaticChecksAreOnUntilTurnedOff;
        procedure AFeedThatIsNotAManifestIsSaidWhenAskedAndQuietOtherwise;
        procedure AnInstallersCapIsTheFeedsSizeOrTheCeiling;
        procedure OnlyOneCheckRunsAtATime;
        procedure AnAskedCheckShowsItIsCheckingAndClosesThatBeforeItSpeaks;
        procedure AnAutomaticCheckShowsNothingUntilThereIsSomethingToInstall;
        procedure AnAutomaticCheckThatFindsAVersionOnlyAsks;
        procedure ACancelledCheckSaysNothing;
        procedure ACancelWhileCheckingEndsTheCheckQuietly;
    end;

    { Integration: these download to a file. }
    TUpdateInstallTest = class(TUpdateCheckTest)
    published
        procedure InstallDownloadsVerifiesAndHandsTheInstallerOver;
        procedure AnInstallerWithTheWrongDigestIsNotInstalled;
        procedure AnInstallerTheFeedGivesNoDigestForIsNotInstalled;
        procedure ADownloadThatFailsIsSaidAndNothingIsInstalled;
        procedure AnInstallerThatCannotBeStartedIsSaidAndTheProgramStays;
        procedure TheInstallerIsDownloadedUnderTheCapItsSizeGives;
        procedure TheDownloadIsShownUntilTheInstallerIsHandedOver;
        procedure AFailedDownloadClosesTheProgressBeforeItSpeaks;
    end;

    { Integration: the application's own web client, over a served curl. }
    TUpdateDownloadTest = class(TTestCase)
    private
        FFiles: TServedFiles;
        FWebObject: TServingWebClient;
        FWeb: IWebClient;
        FHostObject: TScriptedUpdateHost;
        FHost: IUpdateHost;
        FChecker: TUpdateChecker;
        procedure ServeFeed(const ASizeField: string);
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AnInstallerLargerThanADataDownloadIsDownloaded;
        procedure AnInstallerWhoseFeedGivesNoSizeIsDownloadedToo;
        procedure ADownloadReportsItsProgressAgainstTheFeedsSize;
        procedure ACancelledDownloadInstallsNothingAndSaysNothing;
    end;

implementation

const
    Feed = 'https://example.org/releases/latest/download/';
    InstallerBody = 'MZ pretend installer';

var
    ClockValue: TDateTime;

function TestClock: TDateTime;
begin
    Result := ClockValue;
end;

{ ---- the scripted host ---- }

constructor TScriptedUpdateHost.Create;
begin
    inherited Create;
    Asked := TStringList.Create;
    Told := TStringList.Create;
    Installed := TStringList.Create;
    Events := TStringList.Create;
    InstallWorks := True;
end;

destructor TScriptedUpdateHost.Destroy;
begin
    Asked.Free;
    Told.Free;
    Installed.Free;
    Events.Free;
    inherited Destroy;
end;

function TScriptedUpdateHost.AskToInstall(const AVersion, ANotes: string): TUpdateAnswer;
begin
    Events.Add('ask');
    Asked.Add(AVersion);
    Result := Answer;
end;

procedure TScriptedUpdateHost.Tell(const AText: string);
begin
    Events.Add('tell');
    Told.Add(AText);
end;

const
    StageNames: array[TUpdateStage] of string = ('checking', 'downloading',
        'verifying', 'installing');

procedure TScriptedUpdateHost.Progress(const AProgress: TUpdateProgress);
begin
    Events.Add(Format('progress:%s:%d/%d', [StageNames[AProgress.Stage],
        AProgress.Bytes, AProgress.Total]));
    if (AProgress.Stage in CancelIn) and
        ((AProgress.Stage <> usDownloading) or (AProgress.Bytes > 0)) then
        Checker.Cancel;
end;

procedure TScriptedUpdateHost.ProgressDone;
begin
    Events.Add('done');
end;

{ ---- the served files ---- }

constructor TServedFiles.Create;
begin
    inherited Create;
    Names := TStringList.Create;
    Bodies := TStringList.Create;
end;

destructor TServedFiles.Destroy;
begin
    Names.Free;
    Bodies.Free;
    inherited Destroy;
end;

function TServingCurl.Run(const AUrl: string; ADest: TStream;
    AOnChunk: TCurlChunkEvent; out AHeaders, AStderr: string;
    out AStopped: boolean): longint;
var
    i, At: integer;
    Body: string;
begin
    AStopped := False;
    AStderr := '';
    Result := 0;
    for i := 0 to FFiles.Names.Count - 1 do
        if Copy(AUrl, Length(AUrl) - Length(FFiles.Names[i]) + 1, MaxInt) =
            FFiles.Names[i] then
        begin
            AHeaders := 'HTTP/1.1 200 OK'#13#10#13#10;
            Body := FFiles.Bodies[i];
            At := 1;
            while At <= Length(Body) do
            begin
                ADest.WriteBuffer(Body[At], Min(4, Length(Body) - At + 1));
                Inc(At, 4);
                if Assigned(AOnChunk) and
                    not AOnChunk(Min(At - 1, Length(Body))) then
                begin
                    AStopped := True;
                    Exit;
                end;
            end;
            Exit;
        end;
    AHeaders := 'HTTP/1.1 404 Not Found'#13#10#13#10;
end;

constructor TServingWebClient.Create(AFiles: TServedFiles);
begin
    inherited Create;
    FFiles := AFiles;
end;

function TServingWebClient.CreateCurl: TCurlClient;
var
    Curl: TServingCurl;
begin
    Curl := TServingCurl.Create('curl');
    Curl.FFiles := FFiles;
    Result := Curl;
end;

function TScriptedUpdateHost.Install(const AFile: string;
    APlatform: TInstallerPlatform; out AWhy: string): boolean;
var
    S: TStringList;
begin
    Events.Add('install');
    S := TStringList.Create;
    try
        S.LoadFromFile(AFile);
        Installed.Add(ExtractFileName(AFile) + '=' + Trim(S.Text));
    finally
        S.Free;
    end;
    AWhy := 'the installer could not be started';
    Result := InstallWorks;
end;

procedure TScriptedUpdateHost.Quit;
begin
    Events.Add('quit');
    Quit_ := True;
end;

{ ---- the fixture ---- }

procedure TUpdateCheckTest.SetUp;
begin
    SetModulePreference(UpdateFeedKey, '');
    SetModulePreference(UpdateSkipKey, '');
    SetModulePreference(UpdateLastCheckedKey, '');
    FWebObject := TMockWebClient.Create;
    FWeb := FWebObject;
    FHostObject := TScriptedUpdateHost.Create;
    FHost := FHostObject;
    ClockValue := EncodeDateTime(2027, 3, 2, 12, 0, 0, 0);
    FChecker := TUpdateChecker.Create(FWeb, FHost);
    FChecker.Current := '1.2.0.1980';
    FChecker.Platform := ipWindows;
    FChecker.Channel := ParseInstallChannel('channel=direct');
    FChecker.Source := UpdateSourceOf('Fit', Feed);
    FChecker.Clock := @TestClock;
    FHostObject.Checker := FChecker;
end;

procedure TUpdateCheckTest.TearDown;
begin
    FreeAndNil(FChecker);
    FHost := nil;
    FreeAndNil(FHostObject);
    FWeb := nil;
    FreeAndNil(FWebObject);
    SetModulePreference(UpdateFeedKey, '');
    SetModulePreference(UpdateSkipKey, '');
    SetModulePreference(UpdateLastCheckedKey, '');
end;

procedure TUpdateCheckTest.Serve(const AVersion, ASha: string);
begin
    FWebObject.Reply('latest.json', '{"version":"' + AVersion + '",' +
        '"notes":"What is new.","assets":[{"name":"Fit-windows-setup.exe",' +
        '"url":"Fit-windows-setup.exe","sha256":"' + ASha + '"}]}');
    FWebObject.Reply('Fit-windows-setup.exe', InstallerBody);
end;

procedure TUpdateCheckTest.AskedWithNothingNewItSaysSo;
begin
    Serve('1.2.0.1980', '');
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Asked.Count);
    AssertEquals(1, FHostObject.Told.Count);
    AssertTrue(FHostObject.Told[0], Pos('latest version', FHostObject.Told[0]) > 0);
    AssertEquals('the wait shown, and closed before the answer',
        'progress:checking:0/0' + LineEnding + 'done' + LineEnding +
        'tell' + LineEnding, FHostObject.Events.Text);
end;

procedure TUpdateCheckTest.AnUnreachableFeedIsSaidWhenAskedAndQuietOtherwise;
begin
    FWebObject.FailWith('The server could not be reached.');
    FChecker.Run(False);
    AssertEquals('an automatic check says nothing', 0, FHostObject.Told.Count);
    FChecker.Run(True);
    AssertEquals(1, FHostObject.Told.Count);
    AssertTrue(FHostObject.Told[0], Pos('could not be reached', FHostObject.Told[0]) > 0);
end;

procedure TUpdateCheckTest.AnAutomaticCheckWithinSixHoursDoesNotReachTheNetwork;
begin
    Serve('1.2.0.1980', '');
    FChecker.Run(False);
    AssertEquals(1, FWebObject.Log.CountOf('GetText'));
    ClockValue := ClockValue + 1 / 24;
    FChecker.Run(False);
    AssertEquals('not again within six hours', 1, FWebObject.Log.CountOf('GetText'));
    FChecker.Run(True);
    AssertEquals('but when asked', 2, FWebObject.Log.CountOf('GetText'));
end;

procedure TUpdateCheckTest.ASkippedVersionIsNotOfferedAgainByItself;
begin
    Serve('1.3.0.2100', '');
    FHostObject.Answer := uaSkip;
    FChecker.Run(True);
    AssertEquals(1, FHostObject.Asked.Count);
    AssertEquals('1.3.0.2100', ModulePreference(UpdateSkipKey));
    ClockValue := ClockValue + 1;
    FChecker.Run(False);
    AssertEquals('not asked again by itself', 1, FHostObject.Asked.Count);
    FChecker.Run(True);
    AssertEquals('but when asked for', 2, FHostObject.Asked.Count);
end;

procedure TUpdateCheckTest.ABuildIsUpdatedAsTheProductItsIdentityNames;
begin
    //  One mechanism: the product identity the build links names its
    //  installers and its feed - Fit's in Fit, a product's in every product -
    //  so no module has to declare them again, and a product whose module
    //  forgot to could not be updated as Fit.
    AssertEquals(ProductUpdateProduct, BuildUpdateSource.Product);
    AssertEquals(ProductUpdateFeedUrl, BuildUpdateSource.FeedBase);
end;

procedure TUpdateCheckTest.ABuildWithNoUpdateSourceSaysSoWhenAsked;
begin
    FChecker.Source := UpdateSourceOf('FitPro', '');
    FChecker.Run(False);
    AssertEquals(0, FHostObject.Told.Count);
    FChecker.Run(True);
    AssertEquals(1, FHostObject.Told.Count);
    AssertTrue(FHostObject.Told[0], Pos('no update source', FHostObject.Told[0]) > 0);
    AssertEquals('and asks nothing', 0, FWebObject.Log.CountOf('GetText'));
end;

procedure TUpdateCheckTest.AFeedSetInThePreferencesIsTheOneAsked;
begin
    SetModulePreference(UpdateFeedKey, 'https://mirror.example.net/fit');
    FWebObject.Reply('mirror.example.net/fit/latest.json', '{"version":"1.2.0.1980"}');
    FChecker.Run(True);
    AssertTrue(FWebObject.Log.AsText, Pos('https://mirror.example.net/fit/latest.json',
        FWebObject.Log.AsText) > 0);
end;

function HoldEverything(const AVersion: string; AReleaseDate: TDateTime;
    out AReason: string): boolean;
begin
    AReason := 'Your updates have ended.';
    Result := False;
end;

procedure TUpdateCheckTest.AReleaseAModuleHoldsBackIsAnnouncedNotOffered;
begin
    Serve('1.3.0.2100', '');
    FChecker.Gates := [@HoldEverything];
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Asked.Count);
    AssertTrue(FHostObject.Told.Text, Pos('updates have ended', FHostObject.Told.Text) > 0);
end;

procedure TUpdateCheckTest.LaterChangesNothing;
begin
    Serve('1.3.0.2100', '');
    FHostObject.Answer := uaLater;
    FChecker.Run(True);
    AssertEquals('', ModulePreference(UpdateSkipKey));
    AssertEquals(0, FHostObject.Installed.Count);
    AssertFalse(FHostObject.Quit_);
end;

procedure TUpdateCheckTest.AutomaticChecksAreOnUntilTurnedOff;
begin
    SetModulePreference(UpdateAutoKey, '');
    AssertTrue('on by default', AutomaticUpdateChecks);
    SetAutomaticUpdateChecks(False);
    AssertFalse(AutomaticUpdateChecks);
    SetAutomaticUpdateChecks(True);
    AssertTrue(AutomaticUpdateChecks);
    SetModulePreference(UpdateAutoKey, '');
end;

procedure TUpdateCheckTest.AFeedThatIsNotAManifestIsSaidWhenAskedAndQuietOtherwise;
begin
    FWebObject.Reply('latest.json', '<html>Not found</html>');
    FChecker.Run(False);
    AssertEquals('quiet by itself', 0, FHostObject.Told.Count);
    SetModulePreference(UpdateLastCheckedKey, '');
    FChecker.Run(True);
    AssertTrue(FHostObject.Told.Text, Pos('Could not check for updates', FHostObject.Told.Text) > 0);
end;

procedure TUpdateCheckTest.AnInstallersCapIsTheFeedsSizeOrTheCeiling;
begin
    AssertEquals('exactly what the feed says', 68646041,
        InstallerDownloadCap(68646041));
    AssertEquals('the ceiling when it says nothing', InstallerMaxBytes,
        InstallerDownloadCap(0));
    AssertTrue('a ceiling far above any installer',
        InstallerMaxBytes >= int64(512) * 1024 * 1024);
end;

{ Two Help > Check for Updates in a row: the second starts nothing, and says
  why; one at start-up while another runs starts nothing and says nothing. }
procedure TUpdateCheckTest.OnlyOneCheckRunsAtATime;
var
    Why: string;
begin
    AssertTrue('the first may start', ClaimUpdateCheck(True, Why));
    try
        AssertEquals('', Why);
        AssertFalse('the second may not', ClaimUpdateCheck(True, Why));
        AssertTrue(Why, Pos('already', Why) > 0);
        AssertFalse(ClaimUpdateCheck(False, Why));
        AssertEquals('nobody asked, nothing said', '', Why);
    finally
        ReleaseUpdateCheck;
    end;
    AssertTrue('and once it ends, the next may', ClaimUpdateCheck(False, Why));
    ReleaseUpdateCheck;
end;

{ THE USER ASKED, SO THE WAIT IS SHOWN - and gone before the answer, which
  would otherwise sit under a window still saying it is checking. }
procedure TUpdateCheckTest.AnAskedCheckShowsItIsCheckingAndClosesThatBeforeItSpeaks;
begin
    Serve('1.3.0.2100', '');
    FHostObject.Answer := uaLater;
    FChecker.Run(True);
    AssertEquals('progress:checking:0/0' + LineEnding + 'done' + LineEnding +
        'ask' + LineEnding, FHostObject.Events.Text);
end;

{ A CHECK NOBODY ASKED FOR SHOWS NOTHING of its own: a window flashing up at
  every start would be noise, and on a slow network a puzzle. }
procedure TUpdateCheckTest.AnAutomaticCheckShowsNothingUntilThereIsSomethingToInstall;
begin
    //  Up to date: nothing at all.
    Serve('1.2.0.1980', '');
    FChecker.Run(False);
    AssertEquals('', FHostObject.Events.Text);
end;

procedure TUpdateCheckTest.AnAutomaticCheckThatFindsAVersionOnlyAsks;
begin
    Serve('1.3.0.2100', '');
    FHostObject.Answer := uaLater;
    FChecker.Run(False);
    AssertEquals('ask' + LineEnding, FHostObject.Events.Text);
end;

{ STOPPING IS NOT FAILING: the user who pressed Cancel is not told that the
  check could not be made. }
procedure TUpdateCheckTest.ACancelledCheckSaysNothing;
begin
    FWebObject.CancelNext;
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Told.Count);
    AssertEquals('progress:checking:0/0' + LineEnding + 'done' + LineEnding,
        FHostObject.Events.Text);
end;

{ Cancel pressed as the feed arrived: the check ends there, with no question
  the user has just said they do not want. }
procedure TUpdateCheckTest.ACancelWhileCheckingEndsTheCheckQuietly;
begin
    Serve('1.3.0.2100', '');
    FHostObject.Answer := uaInstall;
    FHostObject.CancelIn := [usChecking];
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Asked.Count);
    AssertEquals(0, FHostObject.Told.Count);
    AssertEquals(0, FWebObject.Log.CountOf('Download'));
    AssertEquals('done', FHostObject.Events[FHostObject.Events.Count - 1]);
end;

{ ---- installing ---- }

procedure TUpdateInstallTest.TheInstallerIsDownloadedUnderTheCapItsSizeGives;
begin
    FWebObject.Reply('latest.json', '{"version":"1.3.0.2100","assets":[{"name":' +
        '"Fit-windows-setup.exe","url":"Fit-windows-setup.exe","sha256":"' +
        Sha256Hex(InstallerBody) + '","size":' + IntToStr(Length(InstallerBody)) +
        '}]}');
    FWebObject.Reply('Fit-windows-setup.exe', InstallerBody);
    FHostObject.Answer := uaInstall;
    FChecker.Run(False);
    AssertEquals(Length(InstallerBody), FWebObject.LastMaxBytes);
end;

procedure TUpdateInstallTest.TheDownloadIsShownUntilTheInstallerIsHandedOver;
var
    Last: integer;
begin
    Serve('1.3.0.2100', Sha256Hex(InstallerBody));
    FHostObject.Answer := uaInstall;
    FChecker.Run(False);
    Last := FHostObject.Events.Count - 1;
    AssertEquals(FHostObject.Events.Text, 'ask', FHostObject.Events[0]);
    AssertEquals(FHostObject.Events.Text, 'progress:downloading:0/0',
        FHostObject.Events[1]);
    AssertTrue(FHostObject.Events.Text,
        FHostObject.Events.IndexOf('progress:verifying:20/20') > 1);
    AssertEquals(FHostObject.Events.Text, 'progress:installing:20/20',
        FHostObject.Events[Last - 3]);
    AssertEquals('install', FHostObject.Events[Last - 2]);
    AssertEquals('done', FHostObject.Events[Last - 1]);
    AssertEquals('quit', FHostObject.Events[Last]);
end;

procedure TUpdateInstallTest.AFailedDownloadClosesTheProgressBeforeItSpeaks;
var
    Last: integer;
begin
    FWebObject.Reply('latest.json', '{"version":"1.3.0.2100","assets":[{"name":' +
        '"Fit-windows-setup.exe","url":"missing/Fit-windows-setup.exe","sha256":"ab"}]}');
    FHostObject.Answer := uaInstall;
    FChecker.Run(False);
    Last := FHostObject.Events.Count - 1;
    AssertEquals(FHostObject.Events.Text, 'tell', FHostObject.Events[Last]);
    AssertEquals(FHostObject.Events.Text, 'done', FHostObject.Events[Last - 1]);
end;

procedure TUpdateInstallTest.InstallDownloadsVerifiesAndHandsTheInstallerOver;
begin
    Serve('1.3.0.2100', Sha256Hex(InstallerBody));
    FHostObject.Answer := uaInstall;
    FChecker.Run(True);
    AssertEquals(1, FHostObject.Installed.Count);
    AssertEquals('Fit-windows-setup.exe=' + InstallerBody, FHostObject.Installed[0]);
    AssertTrue('and the program quits for it', FHostObject.Quit_);
end;

procedure TUpdateInstallTest.AnInstallerWithTheWrongDigestIsNotInstalled;
begin
    Serve('1.3.0.2100', Sha256Hex('something else'));
    FHostObject.Answer := uaInstall;
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Installed.Count);
    AssertFalse(FHostObject.Quit_);
    AssertTrue(FHostObject.Told.Text, Pos('did not match', FHostObject.Told.Text) > 0);
end;

{ AN INSTALLER NOBODY CAN CHECK IS NOT RUN: no digest is not a match. }
procedure TUpdateInstallTest.AnInstallerTheFeedGivesNoDigestForIsNotInstalled;
begin
    Serve('1.3.0.2100', '');
    FHostObject.Answer := uaInstall;
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Installed.Count);
    AssertFalse(FHostObject.Quit_);
end;

procedure TUpdateInstallTest.AnInstallerThatCannotBeStartedIsSaidAndTheProgramStays;
begin
    Serve('1.3.0.2100', Sha256Hex(InstallerBody));
    FHostObject.Answer := uaInstall;
    FHostObject.InstallWorks := False;
    FChecker.Run(True);
    AssertFalse(FHostObject.Quit_);
    AssertTrue(FHostObject.Told.Text, Pos('could not be started', FHostObject.Told.Text) > 0);
end;

{ The feed names an installer the server does not have. }
procedure TUpdateInstallTest.ADownloadThatFailsIsSaidAndNothingIsInstalled;
begin
    FWebObject.Reply('latest.json', '{"version":"1.3.0.2100","assets":[{"name":' +
        '"Fit-windows-setup.exe","url":"missing/Fit-windows-setup.exe","sha256":"ab"}]}');
    FHostObject.Answer := uaInstall;
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Installed.Count);
    AssertFalse(FHostObject.Quit_);
    AssertTrue(FHostObject.Told.Text, Pos('could not be downloaded', FHostObject.Told.Text) > 0);
end;


{ The clock a checker starts with, before a test replaces it: the system's,
  in UTC, against which "checked within the last day" is measured. }
procedure TUpdateCheckTest.TheDefaultClockIsTheSystemsInUtc;
var
    C: TUpdateChecker;
begin
    C := TUpdateChecker.Create(FWeb, FHost);
    try
        AssertEquals(LocalTimeToUniversal(Now), C.Clock(), 5 / SecsPerDay);
    finally
        C.Free;
    end;
end;

{ ---- through the application's web client ---- }

{ Longer than the cap the fixture gives the client, which stands for the
  64 MB a data download may bring in - and the feed is shorter than it. }
function LargeInstaller: string;
begin
    Result := 'MZ' + StringOfChar('x', 1022);
end;

procedure TUpdateDownloadTest.SetUp;
begin
    SetModulePreference(UpdateFeedKey, '');
    SetModulePreference(UpdateSkipKey, '');
    SetModulePreference(UpdateLastCheckedKey, '');
    FFiles := TServedFiles.Create;
    FWebObject := TServingWebClient.Create(FFiles);
    FWebObject.MaxBytes := 512;
    FWeb := FWebObject;
    FHostObject := TScriptedUpdateHost.Create;
    FHost := FHostObject;
    FHostObject.Answer := uaInstall;
    ClockValue := EncodeDateTime(2027, 3, 2, 12, 0, 0, 0);
    FChecker := TUpdateChecker.Create(FWeb, FHost);
    FChecker.Current := '1.2.0.1980';
    FChecker.Platform := ipWindows;
    FChecker.Channel := ParseInstallChannel('channel=direct');
    FChecker.Source := UpdateSourceOf('Fit', Feed);
    FChecker.Clock := @TestClock;
    FHostObject.Checker := FChecker;
end;

procedure TUpdateDownloadTest.TearDown;
begin
    FreeAndNil(FChecker);
    FHost := nil;
    FreeAndNil(FHostObject);
    FWeb := nil;
    FreeAndNil(FWebObject);
    FreeAndNil(FFiles);
    SetModulePreference(UpdateLastCheckedKey, '');
end;

procedure TUpdateDownloadTest.ServeFeed(const ASizeField: string);
begin
    FFiles.Names.Add('latest.json');
    FFiles.Bodies.Add('{"version":"1.3.0.2100","assets":[{"name":' +
        '"Fit-windows-setup.exe","url":"Fit-windows-setup.exe","sha256":"' +
        Sha256Hex(LargeInstaller) + '"' + ASizeField + '}]}');
    FFiles.Names.Add('Fit-windows-setup.exe');
    FFiles.Bodies.Add(LargeInstaller);
end;

{ THE DEFECT THIS FIXES (2026-10-21): Install now refused a 65.5 MB installer
  because the web client caps every download at the 64 MB meant for a data
  file, and said "Choose a smaller file" to someone who had chosen nothing. }
procedure TUpdateDownloadTest.AnInstallerLargerThanADataDownloadIsDownloaded;
begin
    ServeFeed(',"size":' + IntToStr(Length(LargeInstaller)));
    FChecker.Run(True);
    AssertEquals(FHostObject.Told.Text, 0, FHostObject.Told.Count);
    AssertEquals('Fit-windows-setup.exe=' + LargeInstaller,
        Trim(FHostObject.Installed.Text));
    AssertTrue(FHostObject.Quit_);
end;

{ A feed published before the size was: every release up to 1.2.0.2131. }
procedure TUpdateDownloadTest.AnInstallerWhoseFeedGivesNoSizeIsDownloadedToo;
begin
    ServeFeed('');
    FChecker.Run(True);
    AssertEquals(FHostObject.Told.Text, 0, FHostObject.Told.Count);
    AssertEquals(1, FHostObject.Installed.Count);
end;

{ The server's length never reaches the client - curl streams the body - so
  the bar is measured against the size the feed gives. }
procedure TUpdateDownloadTest.ADownloadReportsItsProgressAgainstTheFeedsSize;
var
    Total: string;
begin
    ServeFeed(',"size":' + IntToStr(Length(LargeInstaller)));
    FChecker.Run(True);
    Total := IntToStr(Length(LargeInstaller));
    AssertTrue(FHostObject.Events.Text,
        FHostObject.Events.IndexOf('progress:downloading:0/' + Total) >= 0);
    AssertTrue(FHostObject.Events.Text,
        FHostObject.Events.IndexOf('progress:downloading:4/' + Total) >= 0);
    AssertTrue(FHostObject.Events.Text,
        FHostObject.Events.IndexOf('progress:downloading:' + Total + '/' +
        Total) >= 0);
end;

procedure TUpdateDownloadTest.ACancelledDownloadInstallsNothingAndSaysNothing;
begin
    ServeFeed(',"size":' + IntToStr(Length(LargeInstaller)));
    FHostObject.CancelIn := [usDownloading];
    FChecker.Run(True);
    AssertEquals(0, FHostObject.Installed.Count);
    AssertEquals(FHostObject.Told.Text, 0, FHostObject.Told.Count);
    AssertFalse(FHostObject.Quit_);
    AssertEquals('done', FHostObject.Events[FHostObject.Events.Count - 1]);
end;

initialization
    RegisterTest('unit', TUpdateCheckTest);
    RegisterTest('integration', TUpdateInstallTest);
    RegisterTest('integration', TUpdateDownloadTest);
end.
