// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which of what a repository holds this build can use - the one rule
every list this module shows is filtered by.)

NOTHING REACHES THE WIZARD THAT FIT CANNOT USE (decided with the user,
2026-10-20). A record's files used to be listed whole, the unreadable ones
greyed with the reason - "what the record contains is part of what the user
came to see". That holds for a record a user named by its DOI and fails for a
search: a page of pdfs, images and 3-D models, most of every repository, buries
the one profile a user is looking for. So a file nothing here reads is not
listed, a record with no such file is not listed, and a subject with no such
record is not listed - and when that leaves nothing, the refusal says how much
was found and why none of it is shown, so an empty list never reads as "the
search is broken".

ONE RULE, ASKED OF THE LOADER REGISTRY, never a list of extensions kept here:
a module that adds a reader (a field's instrument format) widens every search
and every catalog page with no edit to this module. That is capabilities, not
enumeration (non-negotiable 5).
}
unit repository_filter;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, data_source;

{ Whether a file of this name is one this build can make a profile of - or an
  archive, which may hold one. }
function IsUsableFileName(const AName: string): boolean;

{ The usable items of AItems, in order: every container, and each file
  IsUsableFileName accepts. ADropped says how many were left out, which is what
  the refusal and the log both need. }
function UsableItems(const AItems: TDataSourceItems;
    out ADropped: longint): TDataSourceItems;

{ Whether any of the file names is usable - what decides whether a record is
  listed at all. }
function AnyUsable(const ANames: array of string): boolean;

{ What the user is told when filtering left nothing: how many AWHAT ('files',
  'records') were found, that none holds anything Fit can read, and what to
  do. }
function NothingUsableMessage(AFound: longint; const AWhat: string): string;

implementation

uses
    data_loader_registry, archive_members;

function IsUsableFileName(const AName: string): boolean;
begin
    //  AN ARCHIVE IS A FOLDER (archive_browser): usable when it is opened and
    //  something in it is, which only opening it can tell - so it is listed,
    //  and says so when it holds nothing readable.
    Result := (Trim(AName) <> '') and
        ((FindDataLoaderClass(AName) <> nil) or IsArchiveName(AName));
end;

function UsableItems(const AItems: TDataSourceItems;
    out ADropped: longint): TDataSourceItems;
var
    i: longint;
begin
    Result := nil;
    ADropped := 0;
    for i := 0 to High(AItems) do
        if not AItems[i].IsLeaf or IsUsableFileName(AItems[i].FileName) then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := AItems[i];
        end
        else
            Inc(ADropped);
end;

function AnyUsable(const ANames: array of string): boolean;
var
    i: longint;
begin
    for i := 0 to High(ANames) do
        if IsUsableFileName(ANames[i]) then
            Exit(True);
    Result := False;
end;

function NothingUsableMessage(AFound: longint; const AWhat: string): string;
begin
    Result := 'Found ' + IntToStr(AFound) + ' ' + AWhat + ', none of them ' +
        'holding data Fit can read, so none is listed: Fit lists only what it ' +
        'can make a profile of. Try other words, or open the record in a ' +
        'browser to see what it holds.';
end;

end.
