// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Checking for an update: reading the feed, asking, downloading,
verifying, and handing the installer over.)

WHAT IT DECIDES IS update_feed'S; what it cannot do itself is the host's
(IUpdateHost): asking the user, telling them, handing a file to the system's
installer, quitting. The window gives a host of dialogs, a test a scripted one,
and both go through every line here.

ONE REQUEST IS ALL A CHECK SENDS: the feed's latest.json, fetched through the
framework's web client (non-negotiable 11). No identifier, no version, no
platform - everything is decided here, from what the feed says.

AN AUTOMATIC CHECK IS QUIET: a feed that cannot be reached, or a build with no
update source, says nothing unless the user asked. It reaches the network at
most every six hours (update_feed.CheckDue).

VERIFIED BEFORE ANYTHING RUNS: the installer is downloaded to a folder of its
own and its SHA-256 compared with the feed's; a mismatch deletes it, says so
and installs nothing.

Copyright (C) Dmitry Morozov
}
unit update_check;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_web_client, update_feed;

type
    TUpdateAnswer = (uaLater, uaSkip, uaInstall);

    { What a check is doing while the user waits for it. }
    TUpdateStage = (
        usChecking,    //  reading the feed, for a check the user asked for
        usDownloading, //  bringing the installer in
        usVerifying,   //  comparing its digest with the feed's
        usInstalling); //  handing it to the system's installer

    TUpdateProgress = record
        Stage: TUpdateStage;
        { The version being installed; '' while checking. }
        Version: string;
        { Bytes of the installer in so far, and how many there are - 0 when
          neither the feed nor the server said. }
        Bytes, Total: int64;
    end;

    IUpdateHost = interface
        { Shows AVersion and its notes; the user's answer. }
        function AskToInstall(const AVersion, ANotes: string): TUpdateAnswer;
        procedure Tell(const AText: string);
        { Stops what must not run during an install - the compute server - and
          hands AFile to the system's installer. False, with AWhy, when it
          could not be started; the program then goes on. }
        function Install(const AFile: string; APlatform: TInstallerPlatform;
            out AWhy: string): boolean;
        { Ends the program, so the installer can replace it. }
        procedure Quit;
        { Something the user is waiting for is under way, or has moved on.
          Called from the checker's thread, as often as bytes arrive: a host
          keeps the latest and draws it at its own pace, on its own thread.
          Never called for a check that runs by itself until it has something
          to install, so a quiet check stays quiet. }
        procedure Progress(const AProgress: TUpdateProgress);
        { Nothing is under way any more. Called before every question and
          every message, and when a check that showed progress ends. }
        procedure ProgressDone;
    end;

    { Which product's installers to look for, and where its feed is. }
    TUpdateSource = record
        Product: string;
        FeedBase: string;
    end;

    TUtcNow = function: TDateTime;

    TUpdateChecker = class
    private
        FWeb: IWebClient;
        FHost: IUpdateHost;
        FCurrent: string;
        FPlatform: TInstallerPlatform;
        FChannel: TInstallChannel;
        FSource: TUpdateSource;
        FClock: TUtcNow;
        FGates: TUpdateGates;
        FCancelled: boolean;
        FShowing: boolean;
        FProgress: TUpdateProgress;
        function FeedBase: string;
        procedure Report(AStage: TUpdateStage; ABytes, ATotal: int64);
        procedure Done;
        { What the web client calls as the installer arrives. }
        procedure Arrived(ABytes, ATotal: int64);
        { Done, then tells the user AText. }
        procedure Say(const AText: string);
        { Everything Run does; Run makes sure the progress ends with it. }
        procedure Check(AAsked: boolean);
        procedure InstallFrom(const ADecision: TUpdateDecision);
    public
        constructor Create(AWeb: IWebClient; AHost: IUpdateHost);
        { A check: AAsked when the user chose Help > Check for Updates. }
        procedure Run(AAsked: boolean);
        { Stops the check or the download under way; nothing is installed and
          nothing is said. Safe from another thread: it sets flags and nothing
          else - the window's Cancel calls it while Run waits on the network. }
        procedure Cancel;
        property Current: string read FCurrent write FCurrent;
        property Platform: TInstallerPlatform read FPlatform write FPlatform;
        property Channel: TInstallChannel read FChannel write FChannel;
        property Source: TUpdateSource read FSource write FSource;
        property Clock: TUtcNow read FClock write FClock;
        property Gates: TUpdateGates read FGates write FGates;
    end;

const
    { The preferences a check keeps (module_preferences), the framework's own. }
    UpdateFeedKey = 'fit.update-feed';
    UpdateSkipKey = 'fit.update-skip';
    UpdateLastCheckedKey = 'fit.update-last-checked';
    UpdateAutoKey = 'fit.update-auto';
    { The most an installer whose feed gives no size may bring in. }
    InstallerMaxBytes = int64(1024) * 1024 * 1024;

var
    { What the window calls for Help > Check for Updates (AAsked True). Set by
      the program file, which owns the dialogs; nil in a build with none. }
    UpdateCheckRequested: procedure(AAsked: boolean) = nil;

{ Whether a check runs by itself at start-up: on unless the user turned it off
  (Help > Check for Updates Automatically). }
function AutomaticUpdateChecks: boolean;
procedure SetAutomaticUpdateChecks(AOn: boolean);

function UpdateSourceOf(const AProduct, AFeedBase: string): TUpdateSource;

{ The most the download of an installer ASIZE bytes long may bring in: exactly
  that, when the feed said - a longer answer is not the file the feed's digest
  is of - and InstallerMaxBytes when it did not. }
function InstallerDownloadCap(ASize: int64): int64;

{ The update source of this build: the installers and the feed of the product
  it is, as its identity names them (product_identity - Fit's own, or the copy
  a product's folder puts first on the search path). An empty feed means the
  product has none yet. Rejected: a module declaring the source at start-up
  (UseUpdateSource, until 2026-10-16) - a second way to say what the identity
  already says, and a product whose module did not say it was updated as Fit. }
function BuildUpdateSource: TUpdateSource;

{ ONE CHECK AT A TIME. True when a check may start now, and it is then this
  caller's until ReleaseUpdateCheck; False while another runs - with AWHY
  saying so when the user asked, and '' for a check at start-up, which has
  nothing to tell anyone. Safe from any thread.

  Two Help > Check for Updates in a row used to start two checks, each with
  its own progress window and its own download of the same installer. }
function ClaimUpdateCheck(AAsked: boolean; out AWhy: string): boolean;
procedure ReleaseUpdateCheck;

implementation

uses
    Classes, DateUtils, module_preferences, sha256_digest, product_identity,
    web_client;

var
    { 1 while a check runs; changed only by an interlocked exchange. }
    CheckRunning: longint = 0;

function ClaimUpdateCheck(AAsked: boolean; out AWhy: string): boolean;
begin
    AWhy := '';
    Result := InterlockedCompareExchange(CheckRunning, 1, 0) = 0;
    //  A REPEATED ASK IS ANSWERED, not dropped: the user chose the menu entry
    //  and would otherwise see nothing happen.
    if (not Result) and AAsked then
        AWhy := 'A check for updates is already running.';
end;

procedure ReleaseUpdateCheck;
begin
    InterlockedExchange(CheckRunning, 0);
end;

function AutomaticUpdateChecks: boolean;
begin
    Result := ModulePreference(UpdateAutoKey) <> 'off';
end;

procedure SetAutomaticUpdateChecks(AOn: boolean);
begin
    if AOn then
        SetModulePreference(UpdateAutoKey, 'on')
    else
        SetModulePreference(UpdateAutoKey, 'off');
end;

function UpdateSourceOf(const AProduct, AFeedBase: string): TUpdateSource;
begin
    Result.Product := AProduct;
    Result.FeedBase := AFeedBase;
end;

function BuildUpdateSource: TUpdateSource;
begin
    Result := UpdateSourceOf(ProductUpdateProduct, ProductUpdateFeedUrl);
end;

function InstallerDownloadCap(ASize: int64): int64;
begin
    if ASize > 0 then
        Result := ASize
    else
        Result := InstallerMaxBytes;
end;

procedure TUpdateChecker.Cancel;
begin
    //  Two flags: the web client's stops the transfer running now, and this
    //  one stops what Run would do next - a cancel that lands between the
    //  feed and the download would otherwise be lost with the transfer.
    FCancelled := True;
    FWeb.Cancel;
end;

procedure TUpdateChecker.Report(AStage: TUpdateStage; ABytes, ATotal: int64);
begin
    FProgress.Stage := AStage;
    FProgress.Bytes := ABytes;
    FProgress.Total := ATotal;
    FShowing := True;
    FHost.Progress(FProgress);
end;

procedure TUpdateChecker.Done;
begin
    if not FShowing then
        Exit;
    FShowing := False;
    FHost.ProgressDone;
end;

procedure TUpdateChecker.Say(const AText: string);
begin
    //  NEVER UNDER A PROGRESS WINDOW: the message is the end of what that
    //  window said was under way.
    Done;
    FHost.Tell(AText);
end;

procedure TUpdateChecker.Arrived(ABytes, ATotal: int64);
begin
    //  CURL STREAMS THE BODY, so the web client never knows the length (0);
    //  the feed's size is what the bar is measured against.
    if ATotal <= 0 then
        ATotal := FProgress.Total;
    Report(usDownloading, ABytes, ATotal);
end;

function SystemUtcNow: TDateTime;
begin
    Result := LocalTimeToUniversal(Now);
end;

constructor TUpdateChecker.Create(AWeb: IWebClient; AHost: IUpdateHost);
begin
    inherited Create;
    FWeb := AWeb;
    FHost := AHost;
    FChannel := ParseInstallChannel('');
    FSource := BuildUpdateSource;
    FClock := @SystemUtcNow;
    FGates := RegisteredUpdateGates;
end;

function TUpdateChecker.FeedBase: string;
begin
    Result := Trim(ModulePreference(UpdateFeedKey));
    if Result = '' then
        Result := FSource.FeedBase;
    if (Result <> '') and (Result[Length(Result)] <> '/') then
        Result := Result + '/';
end;

function IsoOf(ATime: TDateTime): string;
begin
    Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', ATime);
end;

function TimeOf(const AText: string): TDateTime;
var
    Y, Mo, D, H, Mi, S: integer;
begin
    Result := 0;
    if (Length(AText) >= 19) and TryStrToInt(Copy(AText, 1, 4), Y) and
        TryStrToInt(Copy(AText, 6, 2), Mo) and TryStrToInt(Copy(AText, 9, 2), D) and
        TryStrToInt(Copy(AText, 12, 2), H) and TryStrToInt(Copy(AText, 15, 2), Mi) and
        TryStrToInt(Copy(AText, 18, 2), S) then
        TryEncodeDateTime(Y, Mo, D, H, Mi, S, 0, Result);
end;

procedure TUpdateChecker.Run(AAsked: boolean);
begin
    FCancelled := False;
    FShowing := False;
    FProgress := Default(TUpdateProgress);
    try
        Check(AAsked);
    finally
        //  However the check ended, nothing is under way any more.
        Done;
    end;
end;

procedure TUpdateChecker.Check(AAsked: boolean);
var
    Base, Text, Why: string;
    M: TUpdateManifest;
    Inputs: TUpdateInputs;
    D: TUpdateDecision;
    Answer: TUpdateAnswer;
begin
    Base := FeedBase;
    if Base = '' then
    begin
        if AAsked then
            Say('This build has no update source, so it cannot check for ' +
                'updates.');
        Exit;
    end;
    if not CheckDue(TimeOf(ModulePreference(UpdateLastCheckedKey)), FClock(),
        AAsked) then
        Exit;
    //  SHOWN ONLY WHEN ASKED: the user chose the menu entry and is waiting
    //  for an answer; a check at start-up stays out of sight until it has
    //  something to say.
    if AAsked then
        Report(usChecking, 0, 0);
    try
        Text := FWeb.GetText(Base + 'latest.json');
    except
        //  STOPPING IS NOT FAILING: the user pressed Cancel, and nothing is
        //  wrong to tell them about.
        on EWebCancelled do
            Exit;
        on E: EWebError do
        begin
            if AAsked then
                Say('Could not check for updates: ' + E.Message);
            Exit;
        end;
    end;
    if FCancelled then
        Exit;
    SetModulePreference(UpdateLastCheckedKey, IsoOf(FClock()));
    if not ParseUpdateManifest(Text, Base, M, Why) then
    begin
        if AAsked then
            Say('Could not check for updates: ' + Why);
        Exit;
    end;

    Inputs := Default(TUpdateInputs);
    Inputs.Current := FCurrent;
    Inputs.Product := FSource.Product;
    Inputs.Platform := FPlatform;
    Inputs.Channel := FChannel;
    Inputs.Skipped := ModulePreference(UpdateSkipKey);
    Inputs.Asked := AAsked;
    Inputs.Gates := FGates;
    D := DecideUpdate(M, Inputs);
    case D.Kind of
        udUpToDate:
            if AAsked then
                Say('You have the latest version (' + FCurrent + ').');
        udManagedElsewhere, udNoInstaller, udHeldBack:
            //  Announced every time the check reaches the network: a new
            //  version exists, and this is what stands between it and here.
            Say(D.Message);
        udInstall:
        begin
            Done;
            Answer := FHost.AskToInstall(D.Version, D.Notes);
            case Answer of
                uaSkip: SetModulePreference(UpdateSkipKey, D.Version);
                uaInstall: InstallFrom(D);
                uaLater: ;
            end;
        end;
    end;
end;

procedure TUpdateChecker.InstallFrom(const ADecision: TUpdateDecision);
var
    Dir, Path, Why: string;
    F: TFileStream;
    Size: int64;
    Installed: boolean;
begin
    if FCancelled then
        Exit;
    Dir := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fit-update-' +
        IntToStr(GetProcessID) + '-' + FormatDateTime('hhnnsszzz', Now);
    ForceDirectories(Dir);
    Path := IncludeTrailingPathDelimiter(Dir) + ADecision.Asset.Name;
    Size := 0;
    FProgress.Version := ADecision.Version;
    Report(usDownloading, 0, ADecision.Asset.Size);
    FWeb.SetProgress(@Arrived);
    try
        try
            F := TFileStream.Create(Path, fmCreate);
            try
                //  THE INSTALLER'S OWN CAP, NOT A DATA FILE'S. The web
                //  client's cap is meant for what a data source hands over,
                //  and an installer is larger: 65.5 MB against 64 refused
                //  the update to 1.2.0.2131 with "Choose a smaller file".
                FWeb.Download(ADecision.Asset.Url, F,
                    InstallerDownloadCap(ADecision.Asset.Size));
                Size := F.Size;
            finally
                F.Free;
            end;
        finally
            FWeb.SetProgress(nil);
        end;
    except
        on E: EWebError do
        begin
            DeleteFile(Path);
            RemoveDir(Dir);
            if not (E is EWebCancelled) then
                Say('The update could not be downloaded: ' + E.Message);
            Exit;
        end;
    end;
    //  A Cancel that arrived with the last chunk: the user said stop, and
    //  nothing is installed behind their back.
    if FCancelled then
    begin
        DeleteFile(Path);
        RemoveDir(Dir);
        Exit;
    end;
    Report(usVerifying, Size, Size);
    //  VERIFIED BEFORE ANYTHING RUNS. A feed that names no digest is refused
    //  by the same comparison: no computed digest is empty.
    if Sha256HexOfFile(Path) <> ADecision.Asset.Sha256 then
    begin
        DeleteFile(Path);
        RemoveDir(Dir);
        Say('The downloaded installer did not match the checksum the ' +
            'update feed gives for it, so it was deleted and nothing was ' +
            'installed.');
        Exit;
    end;
    //  SHOWN WHILE IT IS HANDED OVER: on Linux that waits for the password
    //  the package manager asks for, which takes as long as the user does.
    Report(usInstalling, Size, Size);
    Installed := FHost.Install(Path, FPlatform, Why);
    Done;
    if Installed then
        FHost.Quit
    else
        Say('The update was downloaded but ' + Why + '. It is at ' + Path + '.');
end;

end.
