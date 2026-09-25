// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which file formats this build can open, and what opens them.)

THE DEFECT THIS ENDS. Choosing a loader was an if-chain over two extensions, and
a loader could not say which extensions it handled - the injector said it, on the
loader's behalf, in a different unit. So adding a format meant editing shared
code, and a loader shipped in a build whose injector had not been taught about it
was simply unreachable, with nothing to say so.

CAPABILITIES, NOT ENUMERATION (D18). A loader class declares the extensions it
opens and what to call the format; one central rule - Find below - derives which
loader opens a given file. Adding a format is then a new unit plus one
registration line, and every future question of the form "what can this build
read?" has one place to ask.

The shape deliberately mirrors curve_type_registration: registration is explicit
and VERIFIED at start-up, in the binary that is actually running, because linking
is a build-time property that no test running inside another binary can check on
its behalf.
}
unit data_loader_registry;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_loader, int_data_loader;

type
    { Raised when a build's loader set is self-contradictory - no class, no
      extension, or two loaders claiming the same one. A registration fault is a
      programming error in this build, not a user error, so it stops start-up
      rather than being logged and carried past. }
    EDataLoaderRegistration = class(Exception);

    TDataLoaderClass = class of TDataLoader;

    { What a loader says about itself. }
    TDataLoaderInfo = record
        { Class to instantiate. }
        LoaderClass: TDataLoaderClass;
        { Extensions it opens, upper-case and dot-prefixed, separated by ';' -
          '.DAT' or '.XLS;.XLSX'. Upper-case because the lookup upper-cases what
          it is given, and a mismatch here would fail only on some file names. }
        Extensions:  string;
        { What to call the format in a file dialog or an error message. }
        FormatName:  string;
        { What a file of this format holds, as axis modes (axis_mode_registry):
          '' where the format does not say. A diffraction profile is 2 Theta
          against intensity; a plain two-column file is nothing in particular,
          though the same reader reads both - which is why this belongs to the
          registration and not to the reader. }
        ArgumentMode: string;
        ValueMode:    string;
    end;

    TDataLoaderInfoArray = array of TDataLoaderInfo;

{ Registers a loader. Raises when the class is nil, when it claims no extension,
  or when an extension is already claimed - a second claim on '.CSV' would
  otherwise be resolved by registration order, which is nobody's decision.

  AN IDENTICAL REGISTRATION IS A NO-OP - same class, same extensions, same format
  name. A module registers its own loaders from its front door, and a front door
  must be callable twice: a host that re-initialises, and every test that starts
  from a known state, does exactly that. Without this the guard meant to catch
  two DIFFERENT loaders claiming one extension would refuse a module's own second
  call, and the framework's set only escaped it because RegisterAllDataLoaders
  keeps a flag of its own.

  A CHANGED registration is still refused, including one differing only in the
  format name: that is two intentions, not a repeat, and which name the file
  dialog would then show is nobody's decision either. }
procedure RegisterDataLoader(ALoaderClass: TDataLoaderClass;
    const AExtensions, AFormatName: string;
    const AArgumentMode: string = ''; const AValueMode: string = '');

{ The registration AFileName's extension falls under. False when nothing claims
  it. }
function FindDataLoaderInfo(const AFileName: string;
    out AInfo: TDataLoaderInfo): boolean;

{ The loader for AFileName, or nil when nothing claims its extension. Nil rather
  than an exception: the caller knows whether an unreadable file is a user error
  (it usually is) and can say so in its own words. }
function FindDataLoaderClass(const AFileName: string): TDataLoaderClass;

{ Everything registered, in registration order. Used by the file dialog to build
  its filter and by the start-up check - so what the user is offered and what the
  build can actually open cannot disagree. }
function RegisteredDataLoaders: TDataLoaderInfoArray;

{ How many loaders are registered. }
function DataLoaderCount: longint;

{ Every extension a registered loader reads, once each, ';'-separated in
  registration order - '.DAT;.XY;.TXT'. What a source that offers whatever this
  build can read declares it produces. }
function RegisteredExtensions: string;

implementation

var
    Registry: TDataLoaderInfoArray;

function NormaliseExtension(const AExt: string): string;
begin
    Result := UpperCase(Trim(AExt));
    if (Result <> '') and (Result[1] <> '.') then
        Result := '.' + Result;
end;

function DataLoaderCount: longint;
begin
    Result := Length(Registry);
end;

function RegisteredDataLoaders: TDataLoaderInfoArray;
begin
    Result := Registry;
end;

function RegisteredExtensions: string;
var
    Seen, Own: TStringList;
    i, j: longint;
begin
    Seen := TStringList.Create;
    Own := TStringList.Create;
    try
        Own.Delimiter := ';';
        Own.StrictDelimiter := True;
        for i := 0 to High(Registry) do
        begin
            Own.DelimitedText := Registry[i].Extensions;
            for j := 0 to Own.Count - 1 do
                if (Trim(Own[j]) <> '') and
                    (Seen.IndexOf(NormaliseExtension(Own[j])) < 0) then
                    Seen.Add(NormaliseExtension(Own[j]));
        end;
        Seen.Delimiter := ';';
        Seen.StrictDelimiter := True;
        Result := Seen.DelimitedText;
    finally
        Own.Free;
        Seen.Free;
    end;
end;

function IndexOfExtension(const AExt: string): longint;
var
    i: longint;
    Claimed: TStringList;
begin
    Result := -1;
    for i := 0 to High(Registry) do
    begin
        Claimed := TStringList.Create;
        try
            Claimed.Delimiter := ';';
            Claimed.StrictDelimiter := True;
            Claimed.DelimitedText := Registry[i].Extensions;
            if Claimed.IndexOf(AExt) >= 0 then
                Exit(i);
        finally
            Claimed.Free;
        end;
    end;
end;

{ Whether two registrations say the same thing. Compared field by field rather
  than by the class alone: a loader may legitimately be registered for two
  different formats - the two-column reader serves both '.DAT' and '.XY' - so
  "the same class" is not the same registration. }
function SameRegistration(const A, B: TDataLoaderInfo): boolean;
begin
    Result := (A.LoaderClass = B.LoaderClass) and
        (A.Extensions = B.Extensions) and (A.FormatName = B.FormatName) and
        (A.ArgumentMode = B.ArgumentMode) and (A.ValueMode = B.ValueMode);
end;

procedure RegisterDataLoader(ALoaderClass: TDataLoaderClass;
    const AExtensions, AFormatName: string;
    const AArgumentMode: string; const AValueMode: string);
var
    Parts: TStringList;
    Normalised: TStringList;
    i, Existing: longint;
    Info: TDataLoaderInfo;
begin
    if not Assigned(ALoaderClass) then
        raise EDataLoaderRegistration.Create(
            'a data loader was registered with no class');
    if Trim(AExtensions) = '' then
        raise EDataLoaderRegistration.Create(ALoaderClass.ClassName +
            ' was registered without naming a file extension, so nothing could ' +
            'ever reach it');

    Parts := TStringList.Create;
    Normalised := TStringList.Create;
    try
        Parts.Delimiter := ';';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := AExtensions;
        for i := 0 to Parts.Count - 1 do
        begin
            if NormaliseExtension(Parts[i]) = '' then
                Continue;
            Normalised.Add(NormaliseExtension(Parts[i]));
        end;

        if Normalised.Count = 0 then
            raise EDataLoaderRegistration.Create(ALoaderClass.ClassName +
                ' named only empty file extensions');

        Normalised.Delimiter := ';';
        Normalised.StrictDelimiter := True;

        Info.LoaderClass := ALoaderClass;
        Info.Extensions := Normalised.DelimitedText;
        Info.FormatName := AFormatName;
        Info.ArgumentMode := Trim(AArgumentMode);
        Info.ValueMode := Trim(AValueMode);

        for i := 0 to Normalised.Count - 1 do
        begin
            Existing := IndexOfExtension(Normalised[i]);
            if Existing < 0 then
                Continue;
            //  The same registration arriving again - see the header.
            if SameRegistration(Registry[Existing], Info) then
                Exit;
            //  Which of the two wins would otherwise depend on registration
            //  order - i.e. on a uses clause somewhere - and the loser would
            //  be dead code that still looks installed.
            raise EDataLoaderRegistration.Create(
                Normalised[i] + ' is claimed by both ' +
                Registry[Existing].LoaderClass.ClassName + ' and ' +
                ALoaderClass.ClassName);
        end;

        SetLength(Registry, Length(Registry) + 1);
        Registry[High(Registry)] := Info;
    finally
        Normalised.Free;
        Parts.Free;
    end;
end;

function FindDataLoaderInfo(const AFileName: string;
    out AInfo: TDataLoaderInfo): boolean;
var
    Index: longint;
begin
    AInfo := Default(TDataLoaderInfo);
    Index := IndexOfExtension(NormaliseExtension(ExtractFileExt(AFileName)));
    Result := Index >= 0;
    if Result then
        AInfo := Registry[Index];
end;

function FindDataLoaderClass(const AFileName: string): TDataLoaderClass;
var
    Info: TDataLoaderInfo;
begin
    Result := nil;
    if FindDataLoaderInfo(AFileName, Info) then
        Result := Info.LoaderClass;
end;

end.
