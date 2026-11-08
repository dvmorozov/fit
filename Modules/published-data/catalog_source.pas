// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The data catalog: Zenodo's and figshare's datasets, browsed by the
kind of data Fit can read - nothing to type, nothing kept in a list here.)

ASKED, NEVER KEPT (the user, 2026-10-20: "I don't need any hardcoded set").
Every level is asked of the services when it is opened, and filtered by the
one rule (repository_filter): what reaches the list is what this build reads.

ORGANISED BY KIND OF DATA, NOT BY SUBJECT (the user, 2026-10-20, after seeing
the first version). That one listed figshare's subject taxonomy - 213 research
fields from "Aboriginal and Torres Strait Islander culture" to "Zoology" -
almost none of which hold anything Fit can fit, and opening one failed. What a
user of THIS program can browse is what it can read, so the top level is the
readers this build has, each a row: "PANalytical XRDML scan (.xrdml)",
"Galactic SPC spectrum (.spc)", "Table of numbers (.csv, .tsv)"... and the
archives. Derived from the loader registry, so a module that adds a reader
adds a row, and a product without a module has none of its rows - nothing
here names a format.

A KIND LISTS THE NEWEST DATASETS holding such a file on both services, each
opening to its files, an archive among them a folder.

.txt IS NOT A KIND OF ITS OWN HERE, although the two-column reader reads it:
nearly every dataset holds a README.txt, so a row of ".txt files" would be a
row of every dataset there is. A .txt is still listed inside a record, and a
word search still finds records by their .txt files.
}
unit catalog_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, repository_source, published_repositories;

const
    CatalogSourceId = 'catalog';
    { A kind row's reference: the prefix, its extensions, its name. }
    KindPrefix = 'kind:';

type
    { One row of the catalog's top level. }
    TDataKind = record
        Title: string;
        { Lower case, no dot, comma-separated: 'xrdml'. }
        Extensions: string;
        { What such a file holds, as its registrant described the format
          (DescribeDataFormat). }
        Description: string;
    end;

    TDataKinds = array of TDataKind;

    TCatalogSource = class(TRepositorySource)
    public
        class function Info: TDataSourceInfo; override;
        { The kinds of data this build reads. }
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        { A kind's datasets; anything else as every repository source. }
        function Children(const AItem: TDataSourceItem): TDataSourceItems; override;
    end;

{ The kinds of data the readers registered now read, in registration order, one
  per format name, and the archives last. }
function DataKinds: TDataKinds;

{ A kind row's reference, and what it is read back into. }
function KindRef(const AKind: TDataKind): string;
function ReadKindRef(const ARef: string; out AKind: TDataKind): boolean;

implementation

uses
    StrUtils, log, data_loader_registry, published_data_explanations;

const
    { Read by the two-column reader, and not a kind: see the header. }
    NotAKind = 'txt';

function DataKinds: TDataKinds;
var
    Loaders: TDataLoaderInfoArray;
    Parts: TStringArray;
    i, j, k, Found: longint;
    Ext, Shown: string;
begin
    Result := nil;
    Loaders := RegisteredDataLoaders;
    for i := 0 to High(Loaders) do
    begin
        Parts := Loaders[i].Extensions.Split([';'], TStringSplitOptions.ExcludeEmpty);
        //  ONE ROW PER FORMAT NAME: two registrations of one reader under one
        //  name are one kind to a user.
        Found := -1;
        for k := 0 to High(Result) do
            if Result[k].Title = Loaders[i].FormatName then
                Found := k;
        if Found < 0 then
        begin
            SetLength(Result, Length(Result) + 1);
            Found := High(Result);
            Result[Found].Title := Loaders[i].FormatName;
            Result[Found].Extensions := '';
            Result[Found].Description :=
                DataFormatDescription(Loaders[i].FormatName);
        end;
        for j := 0 to High(Parts) do
        begin
            Ext := LowerCase(Trim(Parts[j]));
            if StartsStr('.', Ext) then
                Delete(Ext, 1, 1);
            if (Ext = '') or (Ext = NotAKind) then
                Continue;
            if Result[Found].Extensions <> '' then
                Result[Found].Extensions := Result[Found].Extensions + ',';
            Result[Found].Extensions := Result[Found].Extensions + Ext;
        end;
    end;
    //  A kind left with no extension (only .txt) is no row; the others are
    //  named with what they read.
    k := 0;
    for i := 0 to High(Result) do
        if Result[i].Extensions <> '' then
        begin
            Shown := '.' + StringReplace(Result[i].Extensions, ',', ', .',
                [rfReplaceAll]);
            Result[k].Title := Result[i].Title + ' (' + Shown + ')';
            Result[k].Extensions := Result[i].Extensions;
            Result[k].Description := Result[i].Description;
            Inc(k);
        end;
    SetLength(Result, k + 1);
    Result[k].Title := 'Archives (.zip, .gz, .tar, .tgz)';
    Result[k].Extensions := 'zip,gz,tar,tgz';
    Result[k].Description := 'compressed files, opened like folders to the ' +
        'files inside that Fit can read';
end;

function KindRef(const AKind: TDataKind): string;
begin
    Result := KindPrefix + AKind.Extensions + ':' + AKind.Title;
end;

function ReadKindRef(const ARef: string; out AKind: TDataKind): boolean;
var
    Rest: string;
    P: longint;
begin
    AKind := Default(TDataKind);
    Result := StartsStr(KindPrefix, ARef);
    if not Result then
        Exit;
    Rest := Copy(ARef, Length(KindPrefix) + 1, MaxInt);
    P := Pos(':', Rest);
    Result := P > 1;
    if not Result then
        Exit;
    AKind.Extensions := Copy(Rest, 1, P - 1);
    AKind.Title := Copy(Rest, P + 1, MaxInt);
end;

class function TCatalogSource.Info: TDataSourceInfo;
begin
    Result := Default(TDataSourceInfo);
    Result.Id := CatalogSourceId;
    Result.Title := 'Data catalog';
    Result.Category := 'Repositories';
    Result.Summary := 'Browse the newest datasets of ' + SearchedRepositories +
        ' by the kind of data Fit can read.';
    Result.Topic := CatalogTopic;
    Result.ProducesExtensions := RegisteredExtensions;
    Result.NeedsNetwork := True;
    //  NOTHING TO ASK: the wizard skips the question and lists the kinds as
    //  soon as the source is chosen.
    Result.QueryFields := nil;
    Result.HasContainers := True;
end;

function TCatalogSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Kinds: TDataKinds;
    i: longint;
begin
    Result := nil;
    Kinds := DataKinds;
    SetLength(Result, Length(Kinds));
    for i := 0 to High(Kinds) do
    begin
        Result[i] := Default(TDataSourceItem);
        Result[i].Title := Kinds[i].Title;
        Result[i].Ref := KindRef(Kinds[i]);
        Result[i].Id := Result[i].Ref;
        //  WHAT SUCH A FILE HOLDS, as whoever registered the reader said; a
        //  format nobody described says what opening it gives.
        Result[i].Details := Kinds[i].Description;
        if Result[i].Details = '' then
            Result[i].Details := 'the newest datasets holding such files';
        Result[i].IsLeaf := False;
    end;
    WriteLog('Published data: the catalog lists ' + IntToStr(Length(Result)) +
        ' kind(s) of data', Debug);
end;

function TCatalogSource.Children(const AItem: TDataSourceItem): TDataSourceItems;
var
    Kind: TDataKind;
    Query: TDatasetQuery;
begin
    if ReadKindRef(AItem.Ref, Kind) then
    begin
        Query := Default(TDatasetQuery);
        Query.Kind := dqKind;
        Query.Words := Kind.Title;
        Query.Extensions := Kind.Extensions;
        Exit(FRepositories.Datasets(Query, 1, 1));
    end;
    Result := inherited Children(AItem);
end;

end.
