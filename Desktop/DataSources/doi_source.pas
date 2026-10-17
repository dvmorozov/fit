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
Zenodo for. Two boxes would make the user decide which kind of thing they have
before they have decided what they want.

STRICTLY DATA ACCESS. The source lists and fetches. It does not interpret, match
or rank what it finds; that boundary is the roadmap's and it is deliberate.
}
unit doi_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

type
    TDoiSource = class(TDataSource)
    private
        { The files inside a Zenodo record, as items. }
        function ZenodoFiles(const AJson, ARecordId: string): TDataSourceItems;
        { The files inside a figshare article, as items. }
        function FigshareFiles(const AJson: string): TDataSourceItems;
        { Zenodo's free-text search, as container items. }
        function ZenodoSearch(const AWords: string): TDataSourceItems;
        { One record, reached by resolving a DOI. }
        function RecordForDoi(const ADoi: string): TDataSourceItems;
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Children(const AItem: TDataSourceItem): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            override;
    end;

const
    DoiSourceId = 'doi';
    DoiTopic = 'data/source-doi';
    DoiFieldId = 'doi';

    { How a container's Ref says which repository it is in. A record's identity
      is the repository AND its number: the numbers collide between them. }
    ZenodoPrefix = 'zenodo:';
    FigsharePrefix = 'figshare:';

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
    fpjson, jsonparser, StrUtils, data_loader_registry;

{ A reply parsed, or a refusal naming what was being read.

  A SERVICE THAT ANSWERS WITH SOMETHING ELSE - a maintenance page, a login
  form, an error in HTML - would otherwise reach the user as a parser's
  complaint about a character at a position, which names nothing they can act
  on and reads as a defect in this program. }
function ParsedReply(const AText, AWhat: string): TJSONData;
begin
    try
        Result := GetJSON(AText);
    except
        on E: Exception do
            raise EDataSourceError.Create('The answer to ' + AWhat +
                ' was not what this program expected, so nothing could be ' +
                'read from it. The service may be having trouble, or may ' +
                'have changed what it publishes.');
    end;
    if Result = nil then
        raise EDataSourceError.Create('The answer to ' + AWhat + ' was empty.');
end;

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
        //  DOIs still resolve to the singular.
        P := LastDelimiter('/', Lower);
        Tail := Copy(AResolvedUrl, P + 1, MaxInt);
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
    Result.Category := 'General';
    Result.Summary :=
        'Open a Zenodo or figshare record by its DOI, or search Zenodo.';
    Result.Topic := DoiTopic;
    //  Whatever this build reads, derived from the loaders as the Web address
    //  source's is: a record may hold any of them.
    Result.ProducesExtensions := RegisteredExtensions;
    Result.NeedsNetwork := True;
    Field := TextField(DoiFieldId, 'DOI or search words',
        'A DOI such as 10.5281/zenodo.1234567, or words to search Zenodo for');
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

function TDoiSource.ZenodoSearch(const AWords: string): TDataSourceItems;
var
    Reply: string;
    Root, Hits, Entry: TJSONData;
    i: longint;
    Item: TDataSourceItem;
begin
    Result := nil;
    Reply := FWeb.GetText('https://zenodo.org/api/records?size=20&q=' +
        UrlEncoded(AWords));
    Root := ParsedReply(Reply, 'the Zenodo search');
    try
        Hits := Root.FindPath('hits.hits');
        if (Hits = nil) or (Hits.Count = 0) then
            raise EDataSourceError.Create('Zenodo found nothing for "' +
                AWords + '". Try fewer or different words, or paste a DOI.');
        for i := 0 to Hits.Count - 1 do
        begin
            Entry := Hits.Items[i];
            Item.Id := ZenodoPrefix + Entry.FindPath('id').AsString;
            Item.Title := Entry.FindPath('metadata.title').AsString;
            Item.Details := Entry.FindPath('metadata.publication_date').AsString;
            Item.FileName := '';
            Item.Ref := Item.Id;
            Item.Size := 0;
            Item.IsLeaf := False;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Item;
        end;
    finally
        Root.Free;
    end;
end;

function TDoiSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Text: string;
begin
    Text := Trim(QueryValue(AQuery, DoiFieldId));
    if Text = '' then
        raise EDataSourceError.Create(
            'Enter a DOI, or words to search Zenodo for.');
    if LooksLikeDoi(Text) then
        Result := RecordForDoi(DoiOf(Text))
    else
        Result := ZenodoSearch(Text);
end;

function TDoiSource.ZenodoFiles(const AJson, ARecordId: string): TDataSourceItems;
var
    Root, Files, Entry: TJSONData;
    i: longint;
    Item: TDataSourceItem;
begin
    Result := nil;
    Root := ParsedReply(AJson, 'the Zenodo record');
    try
        Files := Root.FindPath('files');
        if Files = nil then
            Exit;
        for i := 0 to Files.Count - 1 do
        begin
            Entry := Files.Items[i];
            //  'key' is the file name; 'links.self' is where the bytes are.
            Item.Title := Entry.FindPath('key').AsString;
            Item.FileName := Item.Title;
            Item.Id := ARecordId + '/' + Item.Title;
            Item.Ref := Entry.FindPath('links.self').AsString;
            if Entry.FindPath('size') <> nil then
                Item.Size := Entry.FindPath('size').AsInt64
            else
                Item.Size := 0;
            Item.Details := '';
            Item.IsLeaf := True;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Item;
        end;
    finally
        Root.Free;
    end;
end;

function TDoiSource.FigshareFiles(const AJson: string): TDataSourceItems;
var
    Root, Files, Entry: TJSONData;
    i: longint;
    Item: TDataSourceItem;
begin
    Result := nil;
    Root := ParsedReply(AJson, 'the figshare article');
    try
        Files := Root.FindPath('files');
        if Files = nil then
            Exit;
        for i := 0 to Files.Count - 1 do
        begin
            Entry := Files.Items[i];
            Item.Title := Entry.FindPath('name').AsString;
            Item.FileName := Item.Title;
            Item.Id := Entry.FindPath('id').AsString;
            Item.Ref := Entry.FindPath('download_url').AsString;
            if Entry.FindPath('size') <> nil then
                Item.Size := Entry.FindPath('size').AsInt64
            else
                Item.Size := 0;
            Item.Details := '';
            Item.IsLeaf := True;
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := Item;
        end;
    finally
        Root.Free;
    end;
end;

function TDoiSource.Children(const AItem: TDataSourceItem): TDataSourceItems;
var
    Id: string;
begin
    if StartsStr(ZenodoPrefix, AItem.Ref) then
    begin
        Id := Copy(AItem.Ref, Length(ZenodoPrefix) + 1, MaxInt);
        Result := ZenodoFiles(
            FWeb.GetText('https://zenodo.org/api/records/' + Id), AItem.Ref);
    end
    else if StartsStr(FigsharePrefix, AItem.Ref) then
    begin
        Id := Copy(AItem.Ref, Length(FigsharePrefix) + 1, MaxInt);
        Result := FigshareFiles(
            FWeb.GetText('https://api.figshare.com/v2/articles/' + Id));
    end
    else
        Result := nil;

    if Length(Result) = 0 then
        raise EDataSourceError.Create('That record publishes no files that ' +
            'can be downloaded directly. Open it in a browser to see what it ' +
            'contains.');
end;

function TDoiSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
var
    Suggested: string;
begin
    Suggested := FWeb.Download(AItem.Ref, ADest);
    Result := AItem.FileName;
    if Result = '' then
        Result := Suggested;
end;

end.
