// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What an update feed says, and what the program does about it.)

Every rule the updater follows is a plain function here: reading latest.json,
comparing versions, choosing this platform's installer by its exact name,
reading the install-channel stamp packaging leaves beside the program, whether
a check is due, what a skipped version means, how Linux installs a package, and
whether a module holds a release back. No network, no disk, no clock.
}
unit testcase_update_feed;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, DateUtils, fpcunit, testregistry, update_feed;

type
    TUpdateFeedTest = class(TTestCase)
    published
        //  the manifest
        procedure AManifestIsReadWithItsAssetsResolvedAgainstTheFeed;
        procedure AnAssetWithNoNameOrAddressIsDropped;
        procedure SomethingThatIsNotAManifestIsSaidToBeSo;
        procedure AStoreChannelServesTheVersionItActuallyHas;
        procedure ACopyThatInstallsItsOwnUpdatesIsOfferedTheVersionOfTheInstallers;
        //  versions
        procedure VersionsCompareNumberByNumber;
        procedure AMissingNumberIsZeroAndAVIsIgnored;
        //  the installer
        procedure EachPlatformHasOneInstallerName;
        procedure TheInstallerIsChosenByItsExactName;
        procedure LinuxPrefersThePackageKindOfItsDistribution;
        //  the stamp
        procedure TheStampNamesTheChannelAndHowItUpdates;
        procedure NoStampMeansABuildFromSource;
        procedure ABuildFromSourceIsToldButInstallsNothing;
        procedure OnlyADirectInstallInstallsItsOwnUpdates;
        //  when, and what
        procedure ACheckIsDueEverySixHoursOrWhenAsked;
        procedure ANewerVersionIsOffered;
        procedure TheSameOrAnOlderVersionIsNot;
        procedure ASkippedVersionIsNotOfferedUnlessAsked;
        procedure AStoreInstallIsToldTheCommandInsteadOfInstalling;
        procedure NoInstallerForThisPlatformSaysSo;
        procedure AModuleCanHoldAReleaseBackAndSaysWhy;
        //  Linux
        procedure ADebIsInstalledByAptAndAnRpmByDnf;
        procedure WithoutAWayToAskForAdminThereIsNoCommand;
        procedure ADistributionSaysWhichPackageItTakes;
        procedure WithoutDnfAnRpmIsInstalledByYumOrRpm;
        //  the edges
        procedure AFeedGivenWithoutItsSlashStillFindsItsInstallers;
        procedure AStampLineThatIsNotASettingIsPassedOver;
        procedure AStoreWithoutANameIsNamedByItsChannel;
        procedure NoGateIsRegisteredForNothing;
    end;

implementation

const
    Feed = 'https://example.org/releases/latest/download/';
    ManifestText =
        '{"version":"1.3.0.2100","releaseDate":"2027-03-01",' +
        '"notes":"# What''s new\n- Updates install themselves.",' +
        '"assets":[' +
        '{"name":"Fit-windows-setup.exe","url":"Fit-windows-setup.exe","sha256":"AB12"},' +
        '{"name":"Fit-linux.deb","url":"https://cdn.example.org/fit.deb","sha256":"cd34"},' +
        '{"name":"","url":"x"}],' +
        '"channels":{"winget":"1.2.9.2000"}}';

function Inputs: TUpdateInputs; forward;

function Manifest: TUpdateManifest;
var
    Why: string;
begin
    if not ParseUpdateManifest(ManifestText, Feed, Result, Why) then
        raise Exception.Create(Why);
end;

procedure TUpdateFeedTest.AManifestIsReadWithItsAssetsResolvedAgainstTheFeed;
var
    M: TUpdateManifest;
begin
    M := Manifest;
    AssertEquals('1.3.0.2100', M.Version);
    AssertEquals(EncodeDate(2027, 3, 1), M.ReleaseDate, 0);
    AssertTrue(Pos('install themselves', M.Notes) > 0);
    AssertEquals(2, Length(M.Assets));
    AssertEquals('relative to the feed', Feed + 'Fit-windows-setup.exe', M.Assets[0].Url);
    AssertEquals('an absolute one kept', 'https://cdn.example.org/fit.deb', M.Assets[1].Url);
    AssertEquals('the digest in lower case', 'ab12', M.Assets[0].Sha256);
end;

procedure TUpdateFeedTest.AnAssetWithNoNameOrAddressIsDropped;
begin
    AssertEquals(2, Length(Manifest.Assets));
end;

procedure TUpdateFeedTest.SomethingThatIsNotAManifestIsSaidToBeSo;
var
    M: TUpdateManifest;
    Why: string;
begin
    AssertFalse(ParseUpdateManifest('<html>Not found</html>', Feed, M, Why));
    AssertTrue(Why, Why <> '');
    AssertFalse('no version', ParseUpdateManifest('{"assets":[]}', Feed, M, Why));
end;

procedure TUpdateFeedTest.AStoreChannelServesTheVersionItActuallyHas;
begin
    AssertEquals('1.2.9.2000', VersionForChannel(Manifest, 'winget'));
    AssertEquals('1.3.0.2100', VersionForChannel(Manifest, 'direct'));
end;

procedure TUpdateFeedTest.VersionsCompareNumberByNumber;
begin
    AssertTrue(CompareVersions('1.2.0.1980', '1.2.0.1981') < 0);
    AssertTrue(CompareVersions('1.10.0.1', '1.9.9.9') > 0);
    AssertEquals(0, CompareVersions('1.2.0.1980', '1.2.0.1980'));
end;

procedure TUpdateFeedTest.AMissingNumberIsZeroAndAVIsIgnored;
begin
    AssertEquals(0, CompareVersions('v1.2', '1.2.0.0'));
    AssertTrue(CompareVersions('1.2', '1.2.0.1') < 0);
end;

procedure TUpdateFeedTest.EachPlatformHasOneInstallerName;
begin
    //  The names the release publishes and the site links.
    AssertEquals('Fit-windows-setup.exe', InstallerName('Fit', ipWindows));
    AssertEquals('Fit-linux.deb', InstallerName('Fit', ipLinuxDeb));
    AssertEquals('Fit-linux.rpm', InstallerName('Fit', ipLinuxRpm));
    AssertEquals('Fit-macos.dmg', InstallerName('Fit', ipMac));
    AssertEquals('another product, its own names', 'FitPro-windows-setup.exe',
        InstallerName('FitPro', ipWindows));
end;

procedure TUpdateFeedTest.TheInstallerIsChosenByItsExactName;
var
    A: TUpdateAsset;
begin
    AssertTrue(FindAsset(Manifest, ['Fit-windows-setup.exe'], A));
    AssertEquals('ab12', A.Sha256);
    AssertFalse('not by a prefix', FindAsset(Manifest, ['Fit-windows'], A));
end;

procedure TUpdateFeedTest.LinuxPrefersThePackageKindOfItsDistribution;
var
    Names: TStringArray;
begin
    Names := InstallerPreference('Fit', ipLinuxRpm);
    AssertEquals('Fit-linux.rpm', Names[0]);
    AssertEquals('Fit-linux.deb', Names[1]);
    AssertEquals('Fit-linux.deb', InstallerPreference('Fit', ipLinuxDeb)[0]);
    AssertEquals(1, Length(InstallerPreference('Fit', ipWindows)));
end;

{ THE ASSETS ARE THE FEED'S VERSION: a copy that installs them is offered that
  version, whatever a channel entry of the same name says - otherwise it would
  announce one version and install another. }
procedure TUpdateFeedTest.ACopyThatInstallsItsOwnUpdatesIsOfferedTheVersionOfTheInstallers;
var
    M: TUpdateManifest;
    D: TUpdateDecision;
begin
    M := Manifest;
    M.Channels := ['direct=1.2.9.2000'];
    D := DecideUpdate(M, Inputs);
    AssertTrue(D.Kind = udInstall);
    AssertEquals('1.3.0.2100', D.Version);
    AssertTrue('with its notes', Pos('install themselves', D.Notes) > 0);
end;

procedure TUpdateFeedTest.TheStampNamesTheChannelAndHowItUpdates;
var
    C: TInstallChannel;
begin
    C := ParseInstallChannel('# written by packaging' + LineEnding +
        'channel=Winget' + LineEnding + 'name=winget' + LineEnding +
        'update_command=winget upgrade Fit' + LineEnding +
        'launcher=/usr/bin/fit' + LineEnding + 'unknown=kept quiet');
    AssertEquals('winget', C.Channel);
    AssertEquals('winget', C.Name);
    AssertEquals('winget upgrade Fit', C.UpdateCommand);
    AssertEquals('/usr/bin/fit', C.Launcher);
end;

{ ONLY PACKAGING WRITES A STAMP: a copy without one was built from source, and
  installing a package over it would replace a working tree's build with
  something else. }
procedure TUpdateFeedTest.NoStampMeansABuildFromSource;
begin
    AssertEquals(SourceChannel, ParseInstallChannel('').Channel);
    AssertFalse(OwnsItsUpdates(ParseInstallChannel('')));
end;

procedure TUpdateFeedTest.ABuildFromSourceIsToldButInstallsNothing;
var
    I: TUpdateInputs;
    D: TUpdateDecision;
begin
    I := Inputs;
    I.Channel := ParseInstallChannel('');
    D := DecideUpdate(Manifest, I);
    AssertTrue(D.Kind = udManagedElsewhere);
    AssertTrue(D.Message, Pos('built from source', D.Message) > 0);
end;

procedure TUpdateFeedTest.OnlyADirectInstallInstallsItsOwnUpdates;
var
    C: TInstallChannel;
begin
    C := ParseInstallChannel('channel=direct');
    AssertTrue(OwnsItsUpdates(C));
    C.Channel := 'snap';
    AssertFalse(OwnsItsUpdates(C));
end;

procedure TUpdateFeedTest.ACheckIsDueEverySixHoursOrWhenAsked;
var
    T: TDateTime;
begin
    T := EncodeDateTime(2027, 3, 1, 12, 0, 0, 0);
    AssertTrue('never checked', CheckDue(0, T, False));
    AssertFalse('an hour ago', CheckDue(T - 1 / 24, T, False));
    AssertTrue('seven hours ago', CheckDue(T - 7 / 24, T, False));
    AssertTrue('asked', CheckDue(T - 1 / 24, T, True));
    AssertTrue('a clock behind the last check', CheckDue(T + 1, T, False));
end;

function Inputs: TUpdateInputs;
begin
    Result := Default(TUpdateInputs);
    Result.Current := '1.2.0.1980';
    Result.Product := 'Fit';
    Result.Platform := ipWindows;
    Result.Channel := ParseInstallChannel('channel=direct');
end;

procedure TUpdateFeedTest.ANewerVersionIsOffered;
var
    D: TUpdateDecision;
begin
    D := DecideUpdate(Manifest, Inputs);
    AssertTrue(D.Kind = udInstall);
    AssertEquals('1.3.0.2100', D.Version);
    AssertEquals(Feed + 'Fit-windows-setup.exe', D.Asset.Url);
    AssertTrue(Pos('install themselves', D.Notes) > 0);
end;

procedure TUpdateFeedTest.TheSameOrAnOlderVersionIsNot;
var
    I: TUpdateInputs;
begin
    I := Inputs;
    I.Current := '1.3.0.2100';
    AssertTrue(DecideUpdate(Manifest, I).Kind = udUpToDate);
    I.Current := '1.4.0.1';
    AssertTrue(DecideUpdate(Manifest, I).Kind = udUpToDate);
end;

procedure TUpdateFeedTest.ASkippedVersionIsNotOfferedUnlessAsked;
var
    I: TUpdateInputs;
begin
    I := Inputs;
    I.Skipped := '1.3.0.2100';
    AssertTrue(DecideUpdate(Manifest, I).Kind = udUpToDate);
    I.Asked := True;
    AssertTrue('asked for, it is offered', DecideUpdate(Manifest, I).Kind = udInstall);
end;

procedure TUpdateFeedTest.AStoreInstallIsToldTheCommandInsteadOfInstalling;
var
    I: TUpdateInputs;
    D: TUpdateDecision;
begin
    I := Inputs;
    I.Channel := ParseInstallChannel('channel=snap' + LineEnding +
        'name=Snap Store' + LineEnding + 'update_command=sudo snap refresh fit');
    D := DecideUpdate(Manifest, I);
    AssertTrue(D.Kind = udManagedElsewhere);
    AssertTrue(D.Message, Pos('Snap Store', D.Message) > 0);
    AssertTrue(D.Message, Pos('sudo snap refresh fit', D.Message) > 0);
end;

procedure TUpdateFeedTest.NoInstallerForThisPlatformSaysSo;
var
    I: TUpdateInputs;
    D: TUpdateDecision;
begin
    I := Inputs;
    I.Platform := ipMac;
    D := DecideUpdate(Manifest, I);
    AssertTrue(D.Kind = udNoInstaller);
    AssertTrue(D.Message, Pos('1.3.0.2100', D.Message) > 0);
end;

function HoldBack(const AVersion: string; AReleaseDate: TDateTime;
    out AReason: string): boolean;
begin
    Result := AReleaseDate <= EncodeDate(2027, 2, 1);
    if not Result then
        AReason := 'Your updates ended on 1 February 2027.';
end;

procedure TUpdateFeedTest.AModuleCanHoldAReleaseBackAndSaysWhy;
var
    I: TUpdateInputs;
    D: TUpdateDecision;
begin
    I := Inputs;
    SetLength(I.Gates, 1);
    I.Gates[0] := @HoldBack;
    D := DecideUpdate(Manifest, I);
    AssertTrue(D.Kind = udHeldBack);
    AssertTrue(D.Message, Pos('updates ended', D.Message) > 0);
end;

procedure TUpdateFeedTest.ADebIsInstalledByAptAndAnRpmByDnf;
var
    Tools: TLinuxTools;
    Cmd: TStringArray;
begin
    Tools := Default(TLinuxTools);
    Tools.Pkexec := '/usr/bin/pkexec';
    Tools.AptGet := '/usr/bin/apt-get';
    Tools.Dnf := '/usr/bin/dnf';
    Cmd := LinuxInstallCommand('/tmp/u/fit-linux-amd64.deb', Tools);
    AssertEquals('/usr/bin/pkexec', Cmd[0]);
    AssertEquals('/usr/bin/apt-get', Cmd[1]);
    AssertEquals('install', Cmd[2]);
    AssertEquals('/tmp/u/fit-linux-amd64.deb', Cmd[High(Cmd)]);
    Cmd := LinuxInstallCommand('/tmp/u/fit-linux-x86_64.rpm', Tools);
    AssertEquals('/usr/bin/dnf', Cmd[1]);
end;

procedure TUpdateFeedTest.WithoutAWayToAskForAdminThereIsNoCommand;
var
    Tools: TLinuxTools;
begin
    Tools := Default(TLinuxTools);
    Tools.AptGet := '/usr/bin/apt-get';
    AssertEquals(0, Length(LinuxInstallCommand('/tmp/u/fit.deb', Tools)));
end;

procedure TUpdateFeedTest.WithoutDnfAnRpmIsInstalledByYumOrRpm;
var
    Tools: TLinuxTools;
    Cmd: TStringArray;
begin
    Tools := Default(TLinuxTools);
    Tools.Pkexec := '/usr/bin/pkexec';
    Tools.Yum := '/usr/bin/yum';
    Tools.Rpm := '/usr/bin/rpm';
    Cmd := LinuxInstallCommand('/tmp/u/fit.rpm', Tools);
    AssertEquals('an older Red Hat: yum', '/usr/bin/yum', Cmd[1]);
    Tools.Yum := '';
    Cmd := LinuxInstallCommand('/tmp/u/fit.rpm', Tools);
    AssertEquals('and rpm itself, replacing what is there', '/usr/bin/rpm', Cmd[1]);
    AssertEquals('--replacepkgs', Cmd[3]);
end;

procedure TUpdateFeedTest.AFeedGivenWithoutItsSlashStillFindsItsInstallers;
var
    M: TUpdateManifest;
    Why: string;
begin
    AssertTrue(Why, ParseUpdateManifest(ManifestText, 'https://example.org/feed', M, Why));
    AssertEquals('https://example.org/feed/Fit-windows-setup.exe', M.Assets[0].Url);
end;

procedure TUpdateFeedTest.AStampLineThatIsNotASettingIsPassedOver;
begin
    AssertEquals('direct', ParseInstallChannel('written by hand' + LineEnding +
        'channel=direct').Channel);
end;

procedure TUpdateFeedTest.AStoreWithoutANameIsNamedByItsChannel;
var
    I: TUpdateInputs;
    D: TUpdateDecision;
begin
    I := Inputs;
    I.Channel := ParseInstallChannel('channel=winget');
    D := DecideUpdate(Manifest, I);
    AssertTrue(D.Message, Pos('installed from winget', D.Message) > 0);
end;

procedure TUpdateFeedTest.NoGateIsRegisteredForNothing;
var
    Before: longint;
begin
    Before := Length(RegisteredUpdateGates);
    RegisterUpdateGate(nil);
    AssertEquals(Before, Length(RegisteredUpdateGates));
end;

procedure TUpdateFeedTest.ADistributionSaysWhichPackageItTakes;
begin
    AssertTrue('past the lines that say nothing of it', LinuxPlatformOf(
        'NAME="Fedora Linux"' + LineEnding + 'ID=fedora') = ipLinuxRpm);
    AssertTrue(LinuxPlatformOf('ID=fedora' + LineEnding + 'VERSION_ID=42') = ipLinuxRpm);
    AssertTrue(LinuxPlatformOf('ID="rocky"' + LineEnding + 'ID_LIKE="rhel centos fedora"') = ipLinuxRpm);
    AssertTrue(LinuxPlatformOf('ID=opensuse-tumbleweed' + LineEnding + 'ID_LIKE="opensuse suse"') = ipLinuxRpm);
    AssertTrue(LinuxPlatformOf('ID=ubuntu' + LineEnding + 'ID_LIKE=debian') = ipLinuxDeb);
    AssertTrue('when it cannot say', LinuxPlatformOf('') = ipLinuxDeb);
end;

initialization
    RegisterTest('unit', TUpdateFeedTest);
end.
