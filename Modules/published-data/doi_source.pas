// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Data published with a DOI - a Zenodo or figshare record and its files.)

WHY A DOI AND NOT A SITE. A DOI is what a paper cites, what a reviewer is given
and what outlives the page it currently resolves to. The user has the DOI in
front of them; resolving it, finding which repository holds it and listing what
is inside is the work this saves, and doing it by hand is where the wrong file
gets downloaded.

TWO REPOSITORIES, NAMED. Zenodo and figshare both publish a file list as JSON
under a stable API, which is what makes this a source rather than a scraper.
Anything else the DOI resolves to is REFUSED BY NAME - "this DOI is at X, which
Fit cannot list" - because falling through to a download of an HTML landing page
produces a file that parses to nothing, which is the quiet failure a refusal
exists to prevent.

FREE TEXT IS A SEARCH, A DOI IS A LOOKUP, and one box takes both: a string
holding '10.' followed by a '/' is a DOI, and everything else is words to search
Zenodo and figshare for (published_repositories, since 2026-10-20 - Zenodo
alone before). Two boxes would make the user decide which kind of thing they
have before they have decided what they want.

WHAT A LIST SHOWS IS WHAT CAN BE USED (2026-10-20): only datasets, only those
holding a file this build reads, and of a record only those files - an archive
among them opened like a folder. Everything after the first list is
repository_source's, shared with the catalog.

STRICTLY DATA ACCESS. The source lists and fetches. It does not interpret, match
or rank what it finds; that boundary is deliberate (docs/internal/decisions.md, "Strictly data access").
}
unit doi_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, repository_source, published_repositories;

type
    TDoiSource = class(TRepositorySource)
    private
        { One record, reached by resolving a DOI. }
        function RecordForDoi(const ADoi: string): TDataSourceItems;
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
    end;

const
    DoiSourceId = 'doi';
    DoiFieldId = 'doi';

{ Whether the text the user typed is a DOI rather than words to search for. }
function LooksLikeDoi(const AText: string): boolean;

{ The DOI inside whatever the user pasted: a bare DOI, a doi.org link, or a
  'doi:' prefix all name the same thing. }
function DoiOf(const AText: string): string;

{ Which repository a resolved URL belongs to, as a Ref prefix, or '' when it is
  one this cannot list. Separated from the fetching so the whole decision is
  testable against a URL string. }
function RepositoryRef(const AResolvedUrl: string): string;

implementation

uses
    fpjson, StrUtils, data_loader_registry, published_data_explanations;

function LooksLikeDoi(const AText: string): boolean;
var
    P: longint;
    Text: string;
begin
    Text := Trim(LowerCase(AText));
    P := Pos('10.', Text);
    //  A DOI is '10.' + a registrant + '/' + a suffix. The '/' must come after
    //  the prefix: 'gamma-10.5 spectra' is words, not an identifier.
    Result := (P > 0) and (Pos('/', Copy(Text, P, MaxInt)) > 0);
end;

function DoiOf(const AText: string): string;
var
    Text: string;
    P: longint;
begin
    Text := Trim(AText);
    Text := StringReplace(Text, 'https://doi.org/', '', [rfIgnoreCase]);
    Text := StringReplace(Text, 'http://doi.org/', '', [rfIgnoreCase]);
    Text := StringReplace(Text, 'https://dx.doi.org/', '', [rfIgnoreCase]);
    Text := StringReplace(Text, 'doi:', '', [rfIgnoreCase]);
    Text := Trim(Text);
    P := Pos('10.', Text);
    if P > 1 then
        Text := Copy(Text, P, MaxInt);
    Result := Text;
end;

function RepositoryRef(const AResolvedUrl: string): string;
var
    Lower, Tail: string;
    P: longint;
begin
    Result := '';
    Lower := LowerCase(AResolvedUrl);
    if Pos('zenodo.org', Lower) > 0 then
    begin
        //  .../record/1234567 and .../records/1234567 are both in use, and old
        //  DOIs still resolve to the singular. Newer ones resolve to
        //  .../doi/10.5281/zenodo.1234567, whose last segment carries the
        //  number after "zenodo." - so the record is the DIGITS the segment
        //  ends with, never the segment whole: "zenodo.1234567" is no record
        //  and Zenodo answers it with 404.
        P := LastDelimiter('/', Lower);
        Tail := Copy(AResolvedUrl, P + 1, MaxInt);
        P := Length(Tail);
        while (P > 0) and (Tail[P] in ['0'..'9']) do
            Dec(P);
        Tail := Copy(Tail, P + 1, MaxInt);
        if Tail <> '' then
            Result := ZenodoPrefix + Tail;
    end
    else if Pos('figshare.com', Lower) > 0 then
    begin
        //  figshare's article id is the last all-digit segment: the URL carries
        //  a slug and often a version after it.
        Tail := '';
        P := Length(AResolvedUrl);
        while P > 0 do
        begin
            if AResolvedUrl[P] = '/' then
            begin
                if (Tail <> '') and (StrToIntDef(Tail, -1) > 0) then
                    Break;
                Tail := '';
            end
            else
                Tail := AResolvedUrl[P] + Tail;
            Dec(P);
        end;
        if StrToIntDef(Tail, -1) > 0 then
            Result := FigsharePrefix + Tail;
    end;
end;

class function TDoiSource.Info: TDataSourceInfo;
var
    Field: TInputFieldDecl;
begin
    Result.Id := DoiSourceId;
    Result.Title := 'Published data (DOI)';
    //  A heading of its own, beside the core's General: the repositories are
    //  this module's, and a product without it has no such heading at all.
    Result.Category := 'Repositories';
    //  THE SERVICES SEARCHED ARE NAMED, from the one list that searches them,
    //  so a service added there is named here too.
    Result.Summary := 'Open a Zenodo or figshare record by its DOI, or search ' +
        SearchedRepositories + ' for datasets Fit can read.';
    Result.Topic := DoiTopic;
    //  Whatever this build reads, derived from the loaders as the Web address
    //  source's is: a record may hold any of them.
    Result.ProducesExtensions := RegisteredExtensions;
    Result.NeedsNetwork := True;
    Field := TextField(DoiFieldId, 'DOI or search words',
        'A DOI such as 10.5281/zenodo.1234567, or words to search ' +
        SearchedRepositories + ' for');
    Field.Topic := DoiTopic;
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := Field;
    //  A record holds several files, and which one is the data is the user's
    //  choice - so the wizard asks it rather than guessing the first.
    Result.HasContainers := True;
end;

function TDoiSource.RecordForDoi(const ADoi: string): TDataSourceItems;
var
    Reply, Url, Ref: string;
    Root, Values, Entry: TJSONData;
    i: longint;
    Item: TDataSourceItem;
begin
    Result := nil;
    //  The handle system answers what a DOI currently points at, in JSON, with
    //  no key and no terms. Following the redirect in a browser would answer
    //  the same question with an HTML page nobody can parse reliably.
    Reply := FWeb.GetText('https://doi.org/api/handles/' + ADoi);
    Url := '';
    Root := ParsedReply(Reply, 'the DOI system');
    try
        Values := Root.FindPath('values');
        if Values <> nil then
            for i := 0 to Values.Count - 1 do
            begin
                Entry := Values.Items[i];
                if SameText(Entry.FindPath('type').AsString, 'URL') then
                begin
                    Url := Entry.FindPath('data.value').AsString;
                    Break;
                end;
            end;
    finally
        Root.Free;
    end;

    if Url = '' then
        raise EDataSourceError.Create('The DOI ' + ADoi +
            ' does not resolve to anything. Check that it was copied whole.');

    Ref := RepositoryRef(Url);
    if Ref = '' then
        raise EDataSourceError.Create('The DOI ' + ADoi + ' is published at ' +
            Url + '. Fit can list the files of Zenodo and figshare records; ' +
            'for anything else, open the page, copy the address of the data ' +
            'file and use the Web address source.');

    Item.Id := Ref;
    Item.Title := ADoi;
    Item.Details := Url;
    Item.FileName := '';
    Item.Ref := Ref;
    Item.Size := 0;
    //  A record, not a file: the next step lists what is inside it.
    Item.IsLeaf := False;
    SetLength(Result, 1);
    Result[0] := Item;
end;

function TDoiSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Text: string;
    Words: TDatasetQuery;
begin
    Text := Trim(QueryValue(AQuery, DoiFieldId));
    if Text = '' then
        raise EDataSourceError.Create('Enter a DOI, or words to search ' +
            SearchedRepositories + ' for.');
    if LooksLikeDoi(Text) then
        Exit(RecordForDoi(DoiOf(Text)));
    Words := Default(TDatasetQuery);
    Words.Kind := dqWords;
    Words.Words := Text;
    Result := FRepositories.Datasets(Words, 1, 1);
end;

end.
