// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(An update feed, and what the program does about what it says.)

THE DESIGN IS THE AUTHOR'S OTHER APPLICATION'S (MindMap Chat), ported: one
latest.json at the feed's base naming the newest version, its notes and one
installer per platform by EXACT file name, each with its SHA-256. A copy
installed by a store says which in an install-channel stamp packaging leaves
beside the program; such a copy is told the store's own command rather than
installing anything. Nothing is sent with a check but the request itself: the
feed is read, and everything is decided here.

EVERY RULE IS A PLAIN FUNCTION over plain values - no network, no disk, no
clock - so each is tested exhaustively (tests/testcase_update_feed.pas). What
fetches, asks and installs is update_check, on the client.

FIT'S VERSIONS HAVE FOUR NUMBERS (major.minor.revision.build), compared number
by number; a missing one is zero.

A MODULE MAY HOLD A RELEASE BACK - a licence whose updates ended before it was
released, say - through an update gate: the release is announced, with the
gate's reason, and not installed.

Copyright (C) Dmitry Morozov
}
unit update_feed;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    TUpdateAsset = record
        Name, Url, Sha256: string;
    end;
    TUpdateAssets = array of TUpdateAsset;

    TUpdateManifest = record
        Version: string;
        Notes: string;
        { UTC day the release was made; 0 when the feed does not say. }
        ReleaseDate: TDateTime;
        Assets: TUpdateAssets;
        { name=version per store channel: the version that store serves. }
        Channels: TStringArray;
    end;

    TInstallerPlatform = (ipWindows, ipLinuxDeb, ipLinuxRpm, ipMac);

    TInstallChannel = record
        { 'direct' when the program was installed from its own installer. }
        Channel: string;
        Name: string;
        UpdateCommand: string;
        { What a Linux install starts again afterwards; '' for nothing. }
        Launcher: string;
    end;

    { Holds back a release: False, with AReason, to announce it and not install
      it. }
    TUpdateGate = function(const AVersion: string; AReleaseDate: TDateTime;
        out AReason: string): boolean;
    TUpdateGates = array of TUpdateGate;

    TUpdateInputs = record
        { This build's version. }
        Current: string;
        { The installer names' prefix: 'fit', or a product of a module's. }
        Product: string;
        Platform: TInstallerPlatform;
        Channel: TInstallChannel;
        { The version the user chose to skip, or ''. }
        Skipped: string;
        { The user asked (Help > Check for Updates): a skipped version is
          offered all the same. }
        Asked: boolean;
        Gates: TUpdateGates;
    end;

    TUpdateDecisionKind = (udUpToDate, udInstall, udManagedElsewhere,
        udNoInstaller, udHeldBack);

    TUpdateDecision = record
        Kind: TUpdateDecisionKind;
        Version: string;
        Notes: string;
        Asset: TUpdateAsset;
        { What to tell the user when nothing is installed here. }
        Message: string;
    end;

    { Where the tools a Linux install needs are, '' for absent. }
    TLinuxTools = record
        Pkexec, AptGet, Dnf, Yum, Rpm: string;
    end;

const
    { The channel of a copy installed from the program's own installer - the
      only one that installs its own updates. Packaging writes it. }
    DirectChannel = 'direct';
    { A copy with no stamp: built from source. Told of a new version, but
      nothing is installed over a working tree's build. }
    SourceChannel = 'source';
    { How often an automatic check may reach the network. }
    CheckEveryHours = 6;
    { The file packaging leaves beside the program. }
    InstallChannelStampName = 'install_channel';

{ Reads latest.json. Each asset's url is resolved against AFeedBase. False,
  with AWhy, for text that is not a manifest with a version. }
function ParseUpdateManifest(const AText, AFeedBase: string;
    out AManifest: TUpdateManifest; out AWhy: string): boolean;

{ The version AChannel actually serves: its own entry when the feed has one,
  the feed's version otherwise. }
function VersionForChannel(const AManifest: TUpdateManifest;
    const AChannel: string): string;

{ <0, 0, >0 as A is older than, the same as, or newer than B. }
function CompareVersions(const A, B: string): integer;

{ The installer's exact file name for AProduct on APlatform. }
function InstallerName(const AProduct: string; APlatform: TInstallerPlatform): string;
{ The names to look for, in order: a Linux machine takes the other package
  kind when its own is absent. }
function InstallerPreference(const AProduct: string;
    APlatform: TInstallerPlatform): TStringArray;
{ The first asset of AManifest named exactly one of ANames, in their order. }
function FindAsset(const AManifest: TUpdateManifest; const ANames: array of string;
    out AAsset: TUpdateAsset): boolean;

{ The install-channel stamp (key=value lines, # comments). Only packaging
  writes one, so no stamp at all is a build from source. }
function ParseInstallChannel(const AText: string): TInstallChannel;
function OwnsItsUpdates(const AChannel: TInstallChannel): boolean;

{ Whether a check may reach the network now: always when asked, otherwise
  when the last one is older than CheckEveryHours - or in the future, which a
  clock set back makes it. }
function CheckDue(ALastChecked, ANow: TDateTime; AAsked: boolean): boolean;

{ What to do about AManifest. }
function DecideUpdate(const AManifest: TUpdateManifest;
    const AInputs: TUpdateInputs): TUpdateDecision;

{ The command that installs APackage (.deb or .rpm) with admin rights asked
  for through pkexec; empty when this machine cannot ask for them. }
function LinuxInstallCommand(const APackage: string;
    const ATools: TLinuxTools): TStringArray;

{ The package kind a Linux distribution takes, from its /etc/os-release: an
  rpm for the Red Hat and SUSE families, a deb otherwise. }
function LinuxPlatformOf(const AOsRelease: string): TInstallerPlatform;

{ Update gates modules register (a module's front door); asked by every
  decision the program makes. }
procedure RegisterUpdateGate(AGate: TUpdateGate);
function RegisteredUpdateGates: TUpdateGates;

implementation

uses
    Classes, DateUtils, fpjson, jsonparser;

var
    Gates: TUpdateGates;

procedure RegisterUpdateGate(AGate: TUpdateGate);
var
    G: TUpdateGate;
begin
    if not Assigned(AGate) then
        Exit;
    for G in Gates do
        if G = AGate then
            Exit;
    SetLength(Gates, Length(Gates) + 1);
    Gates[High(Gates)] := AGate;
end;

function RegisteredUpdateGates: TUpdateGates;
begin
    Result := Copy(Gates);
end;

function Resolved(const AUrl, ABase: string): string;
var
    Base: string;
begin
    if (Pos('://', AUrl) > 0) then
        Exit(AUrl);
    Base := ABase;
    if (Base <> '') and (Base[Length(Base)] <> '/') then
        Base := Base + '/';
    Result := Base + AUrl;
end;

function DayOf(const AText: string): TDateTime;
var
    Y, M, D: integer;
begin
    Result := 0;
    if (Length(AText) >= 10) and TryStrToInt(Copy(AText, 1, 4), Y) and
        TryStrToInt(Copy(AText, 6, 2), M) and TryStrToInt(Copy(AText, 9, 2), D) then
        TryEncodeDate(Y, M, D, Result);
end;

function ParseUpdateManifest(const AText, AFeedBase: string;
    out AManifest: TUpdateManifest; out AWhy: string): boolean;
var
    Data: TJSONData;
    O, A, Ch: TJSONObject;
    Arr: TJSONArray;
    i: integer;
    Asset: TUpdateAsset;
begin
    AManifest := Default(TUpdateManifest);
    AWhy := '';
    try
        Data := GetJSON(AText);
    except
        Data := nil;
    end;
    try
        if not (Data is TJSONObject) then
        begin
            AWhy := 'The update feed''s answer is not an update manifest.';
            Exit(False);
        end;
        O := TJSONObject(Data);
        AManifest.Version := Trim(O.Get('version', ''));
        if AManifest.Version = '' then
        begin
            AWhy := 'The update feed names no version.';
            Exit(False);
        end;
        AManifest.Notes := O.Get('notes', '');
        AManifest.ReleaseDate := DayOf(O.Get('releaseDate', ''));
        Arr := O.Find('assets', jtArray) as TJSONArray;
        if Assigned(Arr) then
            for i := 0 to Arr.Count - 1 do
                if Arr.Types[i] = jtObject then
                begin
                    A := Arr.Objects[i];
                    Asset.Name := A.Get('name', '');
                    Asset.Url := A.Get('url', '');
                    Asset.Sha256 := LowerCase(A.Get('sha256', ''));
                    if (Asset.Name = '') or (Asset.Url = '') then
                        Continue;
                    Asset.Url := Resolved(Asset.Url, AFeedBase);
                    SetLength(AManifest.Assets, Length(AManifest.Assets) + 1);
                    AManifest.Assets[High(AManifest.Assets)] := Asset;
                end;
        Ch := O.Find('channels', jtObject) as TJSONObject;
        if Assigned(Ch) then
            for i := 0 to Ch.Count - 1 do
                if Ch.Items[i].JSONType = jtString then
                begin
                    SetLength(AManifest.Channels, Length(AManifest.Channels) + 1);
                    AManifest.Channels[High(AManifest.Channels)] :=
                        LowerCase(Ch.Names[i]) + '=' + Ch.Items[i].AsString;
                end;
        Result := True;
    finally
        Data.Free;
    end;
end;

function VersionForChannel(const AManifest: TUpdateManifest;
    const AChannel: string): string;
var
    Entry: string;
begin
    for Entry in AManifest.Channels do
        if Copy(Entry, 1, Length(AChannel) + 1) = LowerCase(AChannel) + '=' then
            Exit(Copy(Entry, Length(AChannel) + 2, MaxInt));
    Result := AManifest.Version;
end;

function CompareVersions(const A, B: string): integer;

    function Parts(const AText: string): TStringArray;
    var
        T: string;
    begin
        T := Trim(AText);
        if (T <> '') and (UpCase(T[1]) = 'V') then
            Delete(T, 1, 1);
        Result := T.Split(['.']);
    end;

var
    PA, PB: TStringArray;
    i, X, Y: integer;
begin
    PA := Parts(A);
    PB := Parts(B);
    for i := 0 to 3 do
    begin
        X := 0;
        Y := 0;
        if i < Length(PA) then
            X := StrToIntDef(PA[i], 0);
        if i < Length(PB) then
            Y := StrToIntDef(PB[i], 0);
        if X <> Y then
            Exit(Ord(X > Y) * 2 - 1);
    end;
    Result := 0;
end;

function InstallerName(const AProduct: string; APlatform: TInstallerPlatform): string;
begin
    case APlatform of
        //  THE NAMES THE RELEASE ALREADY PUBLISHES, which the site links too
        //  (.github/workflows/public-release.yml): one fixed name per platform,
        //  so /releases/latest/download/ can serve it.
        ipWindows: Result := AProduct + '-windows-setup.exe';
        ipLinuxDeb: Result := AProduct + '-linux.deb';
        ipLinuxRpm: Result := AProduct + '-linux.rpm';
        ipMac: Result := AProduct + '-macos.dmg';
    end;
end;

function InstallerPreference(const AProduct: string;
    APlatform: TInstallerPlatform): TStringArray;
begin
    case APlatform of
        ipLinuxDeb: Result := [InstallerName(AProduct, ipLinuxDeb),
            InstallerName(AProduct, ipLinuxRpm)];
        ipLinuxRpm: Result := [InstallerName(AProduct, ipLinuxRpm),
            InstallerName(AProduct, ipLinuxDeb)];
        else
            Result := [InstallerName(AProduct, APlatform)];
    end;
end;

function FindAsset(const AManifest: TUpdateManifest; const ANames: array of string;
    out AAsset: TUpdateAsset): boolean;
var
    Name: string;
    i: integer;
begin
    AAsset := Default(TUpdateAsset);
    for Name in ANames do
        for i := 0 to High(AManifest.Assets) do
            if AManifest.Assets[i].Name = Name then
            begin
                AAsset := AManifest.Assets[i];
                Exit(True);
            end;
    Result := False;
end;

function ParseInstallChannel(const AText: string): TInstallChannel;
var
    Lines: TStringList;
    i, Eq: integer;
    Line, Key, Value: string;
begin
    Result := Default(TInstallChannel);
    Result.Channel := SourceChannel;
    Lines := TStringList.Create;
    try
        Lines.Text := AText;
        for i := 0 to Lines.Count - 1 do
        begin
            Line := Trim(Lines[i]);
            if (Line = '') or (Line[1] = '#') then
                Continue;
            Eq := Pos('=', Line);
            if Eq = 0 then
                Continue;
            Key := LowerCase(Trim(Copy(Line, 1, Eq - 1)));
            Value := Trim(Copy(Line, Eq + 1, MaxInt));
            if (Key = 'channel') and (Value <> '') then
                Result.Channel := LowerCase(Value)
            else if Key = 'name' then
                Result.Name := Value
            else if Key = 'update_command' then
                Result.UpdateCommand := Value
            else if Key = 'launcher' then
                Result.Launcher := Value;
        end;
    finally
        Lines.Free;
    end;
end;

function OwnsItsUpdates(const AChannel: TInstallChannel): boolean;
begin
    Result := AChannel.Channel = DirectChannel;
end;

function CheckDue(ALastChecked, ANow: TDateTime; AAsked: boolean): boolean;
begin
    Result := AAsked or (ALastChecked = 0) or (ALastChecked > ANow) or
        (ANow - ALastChecked >= CheckEveryHours / 24);
end;

function DecideUpdate(const AManifest: TUpdateManifest;
    const AInputs: TUpdateInputs): TUpdateDecision;
var
    Latest, Reason, Store: string;
    Gate: TUpdateGate;
begin
    Result := Default(TUpdateDecision);
    //  A copy that installs its own updates installs the feed's assets, which
    //  are the feed's version; a channel entry is what a STORE serves, and
    //  offering one while installing another would announce the wrong version.
    if OwnsItsUpdates(AInputs.Channel) then
        Latest := AManifest.Version
    else
        Latest := VersionForChannel(AManifest, AInputs.Channel.Channel);
    Result.Version := Latest;
    if (CompareVersions(AInputs.Current, Latest) >= 0) or
        ((not AInputs.Asked) and (AInputs.Skipped = Latest)) then
    begin
        Result.Kind := udUpToDate;
        Exit;
    end;
    for Gate in AInputs.Gates do
        if not Gate(Latest, AManifest.ReleaseDate, Reason) then
        begin
            Result.Kind := udHeldBack;
            Result.Message := 'Version ' + Latest + ' is available. ' + Reason;
            Exit;
        end;

    if AInputs.Channel.Channel = SourceChannel then
    begin
        Result.Kind := udManagedElsewhere;
        Result.Message := 'Version ' + Latest + ' is available. This copy was ' +
            'built from source; update it the way it was built.';
        Exit;
    end;
    if not OwnsItsUpdates(AInputs.Channel) then
    begin
        Result.Kind := udManagedElsewhere;
        Store := AInputs.Channel.Name;
        if Store = '' then
            Store := AInputs.Channel.Channel;
        Result.Message := 'Version ' + Latest + ' is available. This copy was ' +
            'installed from ' + Store + ', which manages its updates.';
        if AInputs.Channel.UpdateCommand <> '' then
            Result.Message := Result.Message + ' To update: ' +
                AInputs.Channel.UpdateCommand;
        Exit;
    end;

    if not FindAsset(AManifest, InstallerPreference(AInputs.Product,
        AInputs.Platform), Result.Asset) then
    begin
        Result.Kind := udNoInstaller;
        Result.Message := 'Version ' + Latest + ' is available, but not yet as an ' +
            'installer for this system.';
        Exit;
    end;
    //  Only here are notes shown, and here Latest is the feed's own version.
    Result.Kind := udInstall;
    Result.Notes := AManifest.Notes;
end;

function LinuxPlatformOf(const AOsRelease: string): TInstallerPlatform;
const
    RpmFamilies: array[0..8] of string = ('fedora', 'rhel', 'centos', 'rocky',
        'almalinux', 'suse', 'opensuse', 'sles', 'amzn');
var
    Lines: TStringList;
    i: integer;
    Line, Family: string;
begin
    Result := ipLinuxDeb;
    Lines := TStringList.Create;
    try
        Lines.Text := LowerCase(AOsRelease);
        for i := 0 to Lines.Count - 1 do
        begin
            Line := Lines[i];
            if (Pos('id=', Line) <> 1) and (Pos('id_like=', Line) <> 1) then
                Continue;
            for Family in RpmFamilies do
                if Pos(Family, Copy(Line, Pos('=', Line) + 1, MaxInt)) > 0 then
                    Exit(ipLinuxRpm);
        end;
    finally
        Lines.Free;
    end;
end;

function LinuxInstallCommand(const APackage: string;
    const ATools: TLinuxTools): TStringArray;
begin
    Result := nil;
    if ATools.Pkexec = '' then
        Exit;
    if LowerCase(ExtractFileExt(APackage)) = '.deb' then
    begin
        if ATools.AptGet <> '' then
            Result := [ATools.Pkexec, ATools.AptGet, 'install', '-y', APackage];
    end
    else if ATools.Dnf <> '' then
        Result := [ATools.Pkexec, ATools.Dnf, 'install', '-y', APackage]
    else if ATools.Yum <> '' then
        Result := [ATools.Pkexec, ATools.Yum, 'install', '-y', APackage]
    else if ATools.Rpm <> '' then
        Result := [ATools.Pkexec, ATools.Rpm, '-Uvh', '--replacepkgs', APackage];
end;

end.
