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
    end;

    TDataLoaderInfoArray = array of TDataLoaderInfo;

{ Registers a loader. Raises when the class is nil, when it claims no extension,
  or when an extension is already claimed - a second claim on '.CSV' would
  otherwise be resolved by registration order, which is nobody's decision. }
procedure RegisterDataLoader(ALoaderClass: TDataLoaderClass;
    const AExtensions, AFormatName: string);

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

procedure RegisterDataLoader(ALoaderClass: TDataLoaderClass;
    const AExtensions, AFormatName: string);
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
            Existing := IndexOfExtension(NormaliseExtension(Parts[i]));
            if Existing >= 0 then
                //  Which of the two wins would otherwise depend on registration
                //  order - i.e. on a uses clause somewhere - and the loser would
                //  be dead code that still looks installed.
                raise EDataLoaderRegistration.Create(
                    NormaliseExtension(Parts[i]) + ' is claimed by both ' +
                    Registry[Existing].LoaderClass.ClassName + ' and ' +
                    ALoaderClass.ClassName);
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

        SetLength(Registry, Length(Registry) + 1);
        Registry[High(Registry)] := Info;
    finally
        Normalised.Free;
        Parts.Free;
    end;
end;

function FindDataLoaderClass(const AFileName: string): TDataLoaderClass;
var
    Index: longint;
begin
    Result := nil;
    Index := IndexOfExtension(NormaliseExtension(ExtractFileExt(AFileName)));
    if Index >= 0 then
        Result := Registry[Index].LoaderClass;
end;

end.
