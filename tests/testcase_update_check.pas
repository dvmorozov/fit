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
    Classes, SysUtils, DateUtils, fpcunit, testregistry, int_web_client,
    mock_web_client, module_preferences, update_feed, update_check, sha256_digest;

type
    TScriptedUpdateHost = class(TObject, IUpdateHost)
    public
        Answer: TUpdateAnswer;
        Asked, Told, Installed: TStringList;
        InstallWorks, Quit_: boolean;
        constructor Create;
        destructor Destroy; override;
        function AskToInstall(const AVersion, ANotes: string): TUpdateAnswer;
        procedure Tell(const AText: string);
        function Install(const AFile: string; APlatform: TInstallerPlatform;
            out AWhy: string): boolean;
        procedure Quit;
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
        procedure AskedWithNothingNewItSaysSo;
        procedure AnUnreachableFeedIsSaidWhenAskedAndQuietOtherwise;
        procedure AnAutomaticCheckWithinSixHoursDoesNotReachTheNetwork;
        procedure ASkippedVersionIsNotOfferedAgainByItself;
        procedure ABuildWithNoUpdateSourceSaysSoWhenAsked;
        procedure AFeedSetInThePreferencesIsTheOneAsked;
        procedure AReleaseAModuleHoldsBackIsAnnouncedNotOffered;
        procedure LaterChangesNothing;
        procedure AutomaticChecksAreOnUntilTurnedOff;
        procedure AFeedThatIsNotAManifestIsSaidWhenAskedAndQuietOtherwise;
    end;

    { Integration: these download to a file. }
    TUpdateInstallTest = class(TUpdateCheckTest)
    published
        procedure InstallDownloadsVerifiesAndHandsTheInstallerOver;
        procedure AnInstallerWithTheWrongDigestIsNotInstalled;
        procedure AnInstallerTheFeedGivesNoDigestForIsNotInstalled;
        procedure ADownloadThatFailsIsSaidAndNothingIsInstalled;
        procedure AnInstallerThatCannotBeStartedIsSaidAndTheProgramStays;
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
    InstallWorks := True;
end;

destructor TScriptedUpdateHost.Destroy;
begin
    Asked.Free;
    Told.Free;
    Installed.Free;
    inherited Destroy;
end;

function TScriptedUpdateHost.AskToInstall(const AVersion, ANotes: string): TUpdateAnswer;
begin
    Asked.Add(AVersion);
    Result := Answer;
end;

procedure TScriptedUpdateHost.Tell(const AText: string);
begin
    Told.Add(AText);
end;

function TScriptedUpdateHost.Install(const AFile: string;
    APlatform: TInstallerPlatform; out AWhy: string): boolean;
var
    S: TStringList;
begin
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

{ ---- installing ---- }

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

initialization
    RegisterTest('unit', TUpdateCheckTest);
    RegisterTest('integration', TUpdateInstallTest);
end.
