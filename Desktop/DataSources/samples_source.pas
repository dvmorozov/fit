// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The data files that ship with this program, offered as a source.)

WHY A LOCAL DIRECTORY IS A DATA SOURCE. Two reasons, and neither is decoration.
A new user has nothing to fit and no reason to trust a remote service yet; the
samples are what makes the wizard useful in the first minute. And this source
needs NO NETWORK, so it is the one the end-to-end test drives through the real
import - which means the path a user takes is exercised in a build with no
connection at all.

IT IS ALSO THE PROOF THAT THE SEAM IS NOT AN HTTP SEAM. A source finds and
fetches; where it fetches from is its own business, and this one copies a file.

WHERE THE SAMPLES ARE is a question with several right answers - a source tree,
an installed package, an application bundle - so it is a rule over strings with
the existence check passed in, and the candidates are tried in order. When none
of them is there the source says so in words rather than offering an empty list,
because an empty list reads as "this build has no samples in it", which is a
different and more alarming thing.
}
unit samples_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

type
    { Whether a directory is really there - passed in, for the reason
      download_cache.TPathExists gives: in Delphi syntax mode the address of an
      RTL routine with the wrong signature binds in silence. }
    TDirExists = function(const APath: string): boolean;

    TSamplesSource = class(TDataSource)
    protected
        { Where the installed samples are. Virtual so a test can point it at a
          fixture directory without installing anything. }
        function SamplesDirectory: string; virtual;
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            override;
    end;

const
    SamplesSourceId = 'samples';
    SamplesTopic = 'data/source-samples';

{ Where the samples are, given where the program itself is, or '' when none of
  the places they are installed to exists. }
function SamplesDirectoryFor(const AExecutableDir: string;
    ADirExists: TDirExists): string;

{ The candidates, in order, for a program at AExecutableDir. Public so the test
  reads as the rule rather than as a list of strings it repeats. }
function SampleDirectoryCandidates(const AExecutableDir: string): TStringList;

{ The same, for a program of a given name - which is what decides the share
  directory a Linux package installs into: /usr/share/fit for one product and
  /usr/share/fit-pro for the other. Taking the name from the running binary
  rather than writing 'fit' out is what keeps the second product from finding
  the first's samples, or none. }
function SampleDirectoryCandidatesFor(const AExecutableDir,
    AProgramName: string): TStringList;

{ Whether a directory is really there. The application's own check. }
function DefaultDirExists(const APath: string): boolean;

implementation

uses
    data_loader_registry;

function DefaultDirExists(const APath: string): boolean;
begin
    Result := DirectoryExists(APath);
end;

function SampleDirectoryCandidates(const AExecutableDir: string): TStringList;
begin
    Result := SampleDirectoryCandidatesFor(AExecutableDir,
        ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
end;

function SampleDirectoryCandidatesFor(const AExecutableDir,
    AProgramName: string): TStringList;
var
    Dir, Name_: string;
begin
    Dir := IncludeTrailingPathDelimiter(AExecutableDir);
    Name_ := LowerCase(Trim(AProgramName));
    if Name_ = '' then
        Name_ := 'fit';
    Result := TStringList.Create;
    //  Beside the program: what a portable archive unpacks to.
    Result.Add(Dir + 'Data');
    //  One above it: the source tree, where the binary is in Desktop/o or
    //  Worker/o, and the macOS bundle's Contents/MacOS.
    Result.Add(ExpandFileName(Dir + '..' + PathDelim + 'Data'));
    //  Two above it: the source tree again when the binary is nested deeper.
    Result.Add(ExpandFileName(Dir + '..' + PathDelim + '..' + PathDelim + 'Data'));
    //  Inside a macOS bundle: the binary is in Contents/MacOS and its
    //  read-only data in Contents/Resources, which is where the packaging puts
    //  it. Looking one directory up finds Contents and nothing in it.
    Result.Add(ExpandFileName(Dir + '..' + PathDelim + 'Resources' +
        PathDelim + 'Data'));
    //  Where a Unix package puts read-only program data, whether the program
    //  itself is in /usr/bin or /usr/local/bin - UNDER ITS OWN NAME, because
    //  the two products install side by side.
    Result.Add(ExpandFileName(Dir + '..' + PathDelim + 'share' + PathDelim +
        Name_ + PathDelim + 'Data'));
    Result.Add('/usr/share/' + Name_ + '/Data');
end;

function SamplesDirectoryFor(const AExecutableDir: string;
    ADirExists: TDirExists): string;
var
    Candidates: TStringList;
    i: longint;
begin
    Result := '';
    if not Assigned(ADirExists) then
        Exit;
    Candidates := SampleDirectoryCandidates(AExecutableDir);
    try
        for i := 0 to Candidates.Count - 1 do
            if ADirExists(Candidates[i]) then
                Exit(Candidates[i]);
    finally
        Candidates.Free;
    end;
end;

function TSamplesSource.SamplesDirectory: string;
begin
    Result := SamplesDirectoryFor(ExtractFilePath(ParamStr(0)), @DefaultDirExists);
end;

class function TSamplesSource.Info: TDataSourceInfo;
begin
    Result.Id := SamplesSourceId;
    Result.Title := 'Sample data';
    Result.Category := 'General';
    Result.Summary := 'The example measurements and series that ship with Fit.';
    Result.Topic := SamplesTopic;
    //  WHATEVER THIS BUILD CAN READ, derived rather than listed: the samples
    //  are chosen from the installed folder by whether a registered loader
    //  reads them, and a module that adds a format adds its samples with it.
    //  The fixed list this replaced named .CSV, which only a module reads.
    Result.ProducesExtensions := RegisteredExtensions;
    Result.NeedsNetwork := False;
    Result.QueryFields := nil;
    Result.HasContainers := False;
end;

function TSamplesSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Dir: string;
    Found: TSearchRec;
    Item: TDataSourceItem;
    Loader: TDataLoaderClass;
begin
    Result := nil;
    Dir := SamplesDirectory;
    if Dir = '' then
        raise EDataSourceError.Create(
            'This installation has no sample data directory. The samples are ' +
            'optional: any file you have can be opened with File > Import ' +
            'Profile instead.');

    Dir := IncludeTrailingPathDelimiter(Dir);
    if FindFirst(Dir + '*', faAnyFile, Found) = 0 then
        try
            repeat
                if (Found.Attr and faDirectory) <> 0 then
                    Continue;
                //  Offer only what this build can actually read: the directory
                //  also holds READMEs and fixtures for other purposes, and a
                //  sample that refuses to open is worse than one that was
                //  never offered.
                Loader := FindDataLoaderClass(Found.Name);
                if Loader = nil then
                    Continue;

                Item.Id := Found.Name;
                Item.Title := Found.Name;
                Item.Details := 'Installed with Fit';
                Item.FileName := Found.Name;
                Item.Ref := Dir + Found.Name;
                Item.Size := Found.Size;
                Item.IsLeaf := True;
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := Item;
            until FindNext(Found) <> 0;
        finally
            FindClose(Found);
        end;

    if Length(Result) = 0 then
        //  THE DIRECTORY IS THERE AND HOLDS NOTHING THIS BUILD READS - an
        //  installation that copied it empty, or one whose samples are all of
        //  a kind only a module that is not built in can read. An empty list
        //  would read as "this build cannot list them", which is a different
        //  and more alarming thing than "there are none here".
        raise EDataSourceError.Create('There are no sample files this build ' +
            'can read in ' + Dir + '. Any data file you have can be opened ' +
            'with File > Import Profile instead.');
end;

function TSamplesSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
var
    Source: TFileStream;
begin
    if not FileExists(AItem.Ref) then
        raise EDataSourceError.Create('The sample ' + AItem.Title +
            ' is no longer where it was installed (' + AItem.Ref + ').');
    Source := TFileStream.Create(AItem.Ref, fmOpenRead or fmShareDenyNone);
    try
        ADest.CopyFrom(Source, Source.Size);
    finally
        Source.Free;
    end;
    Result := AItem.FileName;
end;

end.
