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
        function FeedBase: string;
        procedure InstallFrom(const ADecision: TUpdateDecision);
    public
        constructor Create(AWeb: IWebClient; AHost: IUpdateHost);
        { A check: AAsked when the user chose Help > Check for Updates. }
        procedure Run(AAsked: boolean);
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

var
    { What the window calls for Help > Check for Updates (AAsked True). Set by
      the program file, which owns the dialogs; nil in a build with none. }
    UpdateCheckRequested: procedure(AAsked: boolean) = nil;

{ Whether a check runs by itself at start-up: on unless the user turned it off
  (Help > Check for Updates Automatically). }
function AutomaticUpdateChecks: boolean;
procedure SetAutomaticUpdateChecks(AOn: boolean);

function UpdateSourceOf(const AProduct, AFeedBase: string): TUpdateSource;

{ The update source of this build: the framework's own, unless a module that
  makes the build a product of its own declared another - an empty feed meaning
  the product has none yet. }
function BuildUpdateSource: TUpdateSource;
procedure UseUpdateSource(const AProduct, AFeedBase: string);

implementation

uses
    Classes, DateUtils, module_preferences, sha256_digest, project_identity;

var
    DeclaredSource: TUpdateSource;
    SourceDeclared: boolean;

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
    if SourceDeclared then
        Result := DeclaredSource
    else
        Result := UpdateSourceOf(ProjectUpdateProduct, ProjectUpdateFeed);
end;

procedure UseUpdateSource(const AProduct, AFeedBase: string);
begin
    DeclaredSource := UpdateSourceOf(AProduct, AFeedBase);
    SourceDeclared := True;
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
var
    Base, Text, Why: string;
    M: TUpdateManifest;
    Inputs: TUpdateInputs;
    D: TUpdateDecision;
begin
    Base := FeedBase;
    if Base = '' then
    begin
        if AAsked then
            FHost.Tell('This build has no update source, so it cannot check for ' +
                'updates.');
        Exit;
    end;
    if not CheckDue(TimeOf(ModulePreference(UpdateLastCheckedKey)), FClock(),
        AAsked) then
        Exit;
    try
        Text := FWeb.GetText(Base + 'latest.json');
    except
        on E: EWebError do
        begin
            if AAsked then
                FHost.Tell('Could not check for updates: ' + E.Message);
            Exit;
        end;
    end;
    SetModulePreference(UpdateLastCheckedKey, IsoOf(FClock()));
    if not ParseUpdateManifest(Text, Base, M, Why) then
    begin
        if AAsked then
            FHost.Tell('Could not check for updates: ' + Why);
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
                FHost.Tell('You have the latest version (' + FCurrent + ').');
        udManagedElsewhere, udNoInstaller, udHeldBack:
            //  Announced every time the check reaches the network: a new
            //  version exists, and this is what stands between it and here.
            FHost.Tell(D.Message);
        udInstall:
            case FHost.AskToInstall(D.Version, D.Notes) of
                uaSkip: SetModulePreference(UpdateSkipKey, D.Version);
                uaInstall: InstallFrom(D);
                uaLater: ;
            end;
    end;
end;

procedure TUpdateChecker.InstallFrom(const ADecision: TUpdateDecision);
var
    Dir, Path, Why: string;
    F: TFileStream;
begin
    Dir := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'fit-update-' +
        IntToStr(GetProcessID) + '-' + FormatDateTime('hhnnsszzz', Now);
    ForceDirectories(Dir);
    Path := IncludeTrailingPathDelimiter(Dir) + ADecision.Asset.Name;
    try
        F := TFileStream.Create(Path, fmCreate);
        try
            FWeb.Download(ADecision.Asset.Url, F);
        finally
            F.Free;
        end;
    except
        on E: EWebError do
        begin
            DeleteFile(Path);
            RemoveDir(Dir);
            FHost.Tell('The update could not be downloaded: ' + E.Message);
            Exit;
        end;
    end;
    //  VERIFIED BEFORE ANYTHING RUNS. A feed that names no digest is refused
    //  by the same comparison: no computed digest is empty.
    if Sha256HexOfFile(Path) <> ADecision.Asset.Sha256 then
    begin
        DeleteFile(Path);
        RemoveDir(Dir);
        FHost.Tell('The downloaded installer did not match the checksum the ' +
            'update feed gives for it, so it was deleted and nothing was ' +
            'installed.');
        Exit;
    end;
    if FHost.Install(Path, FPlatform, Why) then
        FHost.Quit
    else
        FHost.Tell('The update was downloaded but ' + Why + '. It is at ' + Path + '.');
end;

end.
