// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(What every source of this module does once a list is on screen:
open a record, an archive, the next page, and fetch a file.)

The DOI source and the catalog differ only in how they START - a DOI or words,
or a subject - and from the first list on they are the same thing: rows that
are records, archives, "More" pages or a notice that a service could not be
asked, and files. So they share this, and a record opened from a search reads
exactly as one opened from the catalog, filtered by the same rule.
}
unit repository_source;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source, int_web_client, archive_browser,
    published_repositories;

type
    TRepositorySource = class(TDataSource)
    protected
        FArchives: TArchiveBrowser;
        FRepositories: TPublishedRepositories;
    public
        constructor Create(AWeb: IWebClient); override;
        destructor Destroy; override;
        { A record's readable files (archives among them as folders), an
          archive's readable members, the next page a More row names, or the
          reason a notice row stands for. }
        function Children(const AItem: TDataSourceItem): TDataSourceItems; override;
        function Download(const AItem: TDataSourceItem; ADest: TStream): string;
            override;
    end;

implementation

uses
    StrUtils, log, repository_filter;

constructor TRepositorySource.Create(AWeb: IWebClient);
begin
    inherited Create(AWeb);
    FArchives := TArchiveBrowser.Create(AWeb);
    FRepositories := TPublishedRepositories.Create(AWeb);
end;

destructor TRepositorySource.Destroy;
begin
    FRepositories.Free;
    FArchives.Free;
    inherited Destroy;
end;

function TRepositorySource.Children(const AItem: TDataSourceItem): TDataSourceItems;
var
    Listed: TDataSourceItems;
    Dropped, i: longint;
begin
    //  AN ARCHIVE in a record, opened: its members.
    if FArchives.Holds(AItem) then
        Exit(FArchives.Children(AItem));
    //  THE NEXT PAGE of one service's records.
    if StartsStr(MorePrefix, AItem.Ref) then
        Exit(FRepositories.MoreDatasets(AItem.Ref));
    //  A SERVICE THAT COULD NOT BE ASKED: opening its row says why.
    if StartsStr(NoticePrefix, AItem.Ref) then
        raise EDataSourceFault.Create(Copy(AItem.Ref, Length(NoticePrefix) + 1,
            MaxInt));

    Result := FRepositories.RecordFiles(AItem.Ref);
    if Length(Result) = 0 then
        raise EDataSourceError.Create('That record publishes no files that ' +
            'can be downloaded directly. Open it in a browser to see what it ' +
            'contains.');

    //  An archive among them is a folder to open, not a file to read.
    for i := 0 to High(Result) do
        Result[i] := FArchives.AsContainer(Result[i]);
    //  ONLY WHAT THIS BUILD CAN READ is listed (repository_filter says why),
    //  and a record holding nothing of the kind says how much it did hold.
    Listed := Result;
    Result := UsableItems(Listed, Dropped);
    WriteLog('Published data: ' + AItem.Ref + ' holds ' +
        IntToStr(Length(Listed)) + ' file(s), ' + IntToStr(Dropped) +
        ' left out as unreadable', Debug);
    if Length(Result) = 0 then
        raise EDataSourceError.Create(NothingUsableMessage(Length(Listed),
            'files in that record'));
end;

function TRepositorySource.Download(const AItem: TDataSourceItem;
    ADest: TStream): string;
var
    Suggested: string;
begin
    if FArchives.Holds(AItem) then
        Exit(FArchives.Download(AItem, ADest));
    Suggested := FWeb.Download(AItem.Ref, ADest);
    Result := AItem.FileName;
    if Result = '' then
        Result := Suggested;
end;

end.
