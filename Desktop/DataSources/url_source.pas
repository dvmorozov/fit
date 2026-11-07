// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A file the user already knows the address of.)

WHY THIS EXISTS WHEN THERE ARE CATALOGUE SOURCES. Most data a user wants is
behind a link someone sent them, a supplementary file on a journal page, or a
service nobody has written a source for. Without this, every one of those is a
trip through a web browser, a download folder and File > Import Profile - and
the provenance of what was imported is then lost entirely.

WHAT DECIDES THE READER is the file name, and three things can supply one: the
address, what the server says in Content-Disposition, and nothing at all. The
first two are tried in that order because the address is what the user can see
and reason about; when neither names a readable extension the refusal says so
rather than guessing a format.
}
unit url_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

type
    TUrlSource = class(TDataSource)
    public
        class function Info: TDataSourceInfo; override;
        function Search(const AQuery: TDataSourceQuery): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            override;
    end;

const
    UrlSourceId = 'url';
    UrlTopic = 'data/source-url';
    UrlFieldId = 'url';

{ The file name an address implies, or '' when it implies none: the last segment
  of the path, without the query string or fragment. }
function FileNameFromUrl(const AUrl: string): string;

{ Whether AUrl is something this can fetch at all, and what to say when it is
  not. A user pasting a page address, a 'file://' path or a bare word gets a
  sentence about what was wrong with it rather than a failed connection. }
function UrlRefusal(const AUrl: string): string;

implementation

uses
    StrUtils, data_loader_registry;

function FileNameFromUrl(const AUrl: string): string;
var
    Path, Segment: string;
    P: longint;
begin
    Path := Trim(AUrl);
    //  A query string and a fragment are not part of the name. The query often
    //  IS what selects the file ('?id=SP500'), which is exactly why a name
    //  taken from it would be neither readable nor stable.
    P := Pos('#', Path);
    if P > 0 then
        Path := Copy(Path, 1, P - 1);
    P := Pos('?', Path);
    if P > 0 then
        Path := Copy(Path, 1, P - 1);
    Segment := Path;
    P := LastDelimiter('/', Segment);
    if P > 0 then
        Segment := Copy(Segment, P + 1, MaxInt);
    Result := Trim(Segment);
    //  A SEGMENT WITH NO EXTENSION IS NOT A FILE NAME. '.../download?id=SP500'
    //  ends in a word that names an endpoint, not a file - and taking it would
    //  leave the download with no extension AND ignore the name the server
    //  goes on to suggest, which is the only thing that then says what the
    //  file is. Answering '' lets that suggestion be used.
    if Pos('.', Result) = 0 then
        Result := '';
end;

function UrlRefusal(const AUrl: string): string;
var
    Lower: string;
begin
    Lower := LowerCase(Trim(AUrl));
    if Lower = '' then
        Exit('Enter the address of a data file.');
    if StartsStr('file:', Lower) then
        //  Not a snub: a local file needs no download at all, and the command
        //  that opens one is right there in the same menu.
        Exit('That is an address on this computer. Use File > Import Profile ' +
            'to open a file you already have.');
    if not (StartsStr('http://', Lower) or StartsStr('https://', Lower)) then
        Exit('Enter a full web address beginning with http:// or https://.');
    Result := '';
end;

class function TUrlSource.Info: TDataSourceInfo;
var
    Field: TInputFieldDecl;
begin
    Result.Id := UrlSourceId;
    Result.Title := 'Web address';
    Result.Category := 'General';
    Result.Summary :=
        'Download a data file from an address you already have.';
    Result.Topic := UrlTopic;
    //  Anything this build can read - derived from the loaders, so a module's
    //  format counts in a build that has the module and in no other. The
    //  address decides which it is, and a file whose kind nothing here reads is
    //  refused by name.
    Result.ProducesExtensions := RegisteredExtensions;
    Result.NeedsNetwork := True;
    Field := TextField(UrlFieldId, 'Address',
        'The full address of the file, for example ' +
        'https://example.org/data/spectrum.xy');
    Field.Topic := UrlTopic;
    Result.QueryFields := nil;
    SetLength(Result.QueryFields, 1);
    Result.QueryFields[0] := Field;
    Result.HasContainers := False;
end;

function TUrlSource.Search(const AQuery: TDataSourceQuery): TDataSourceItems;
var
    Url, Refusal, Name_: string;
    Item: TDataSourceItem;
begin
    Result := nil;
    Url := Trim(QueryValue(AQuery, UrlFieldId));
    Refusal := UrlRefusal(Url);
    if Refusal <> '' then
        raise EDataSourceError.Create(Refusal);

    //  NOTHING IS FETCHED HERE. The address is one result, and the download
    //  happens when the user goes on to the preview - so a mistyped address
    //  costs nothing and the wizard's steps mean the same thing for every
    //  source.
    Name_ := FileNameFromUrl(Url);
    Item.Id := Url;
    Item.Title := Name_;
    if Item.Title = '' then
        Item.Title := Url;
    Item.Details := Url;
    Item.FileName := Name_;
    Item.Ref := Url;
    Item.Size := 0;
    Item.IsLeaf := True;
    SetLength(Result, 1);
    Result[0] := Item;
end;

function TUrlSource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
var
    Suggested: string;
begin
    Suggested := FWeb.Download(AItem.Ref, ADest);
    //  The server's suggestion is used only when the address named nothing:
    //  the address is what the user can see, and a service that renames every
    //  download 'attachment' should not decide how a file is read.
    Result := AItem.FileName;
    if Result = '' then
        Result := Suggested;
end;

end.
