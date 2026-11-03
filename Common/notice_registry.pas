// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Notices: terms a module states, listed in the About box, and the ones
the user must acknowledge.)

A NOTICE IS AN EXPLANATION TOPIC. Its words live where every other explanation
lives (explanation_registry), in the chapter of the module that owns it, so a
notice is read, translated and checked for completeness like the rest; this
unit only records which topics are notices and whether each requires
acknowledgement.

ONCE PER MAJOR VERSION. An acknowledgement is stored as topic=major, so a new
build of the same major version does not ask again and a new major version -
which may bring new terms - does. The window asks at start-up and closes the
application when a notice is declined: a notice that could be dismissed without
accepting it would not have been acknowledged.

Everything deciding here is a plain function over plain values; the registry
itself is one process-wide list, like the other registries.

Copyright (C) Dmitry Morozov
}
unit notice_registry;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, explanation;

type
    TNotice = record
        { The explanation topic that holds the notice's words. }
        Topic: string;
        { Shown at start-up until accepted, once per major version. }
        RequiresAcknowledgement: boolean;
    end;
    TNotices = array of TNotice;

    TTopicResolves = function(const ATopic: string): boolean;
    TFindNoticeWords = function(const ATopic: string;
        out AExplanation: TExplanation): boolean;
    { Shows a notice and says whether the user accepted it. }
    TAskAcceptance = function(const ATitle, AText: string): boolean;

{ Idempotent by topic: a front door may be called twice. The first
  registration's flag stands. }
procedure RegisterNotice(const ATopic: string; ARequiresAcknowledgement: boolean);
function RegisteredNotices: TNotices;

{ '1' from '1.2.0.1980'; '' for a build that cannot name its version. }
function MajorVersionOf(const AVersion: string): string;

{ The topics of ANotices still to be acknowledged at major version AMajor,
  given the stored acknowledgements AAcknowledged ("topic=major;..."), in
  registration order. }
function NoticesToAcknowledge(const ANotices: TNotices;
    const AAcknowledged, AMajor: string): TStringArray;

{ AAcknowledged with ATopic acknowledged at AMajor, every other entry kept. }
function WithAcknowledgement(const AAcknowledged, ATopic, AMajor: string): string;

{ A notice as the About box and the acknowledgement dialog write it: the title,
  the summary, then each paragraph, separated by blank lines. }
function NoticeText(const AExplanation: TExplanation): string;

{ Asks for every notice of ANotices still to be acknowledged at AMajor, in
  order, through AAsk; each accepted one is added to AStored as it is accepted.
  False as soon as one is declined - the caller closes the application - and
  nothing after it is asked. A notice with no words is skipped: it cannot be
  accepted, and NoticeFindings fails for it by name before a release. }
function AcknowledgeNotices(const ANotices: TNotices; const AMajor: string;
    AFind: TFindNoticeWords; AAsk: TAskAcceptance; var AStored: string): boolean;

const
    { The preference key the acknowledgements are kept under
      (module_preferences): the framework's own, prefixed 'fit.' as a module's
      keys are by its name. }
    AcknowledgedNoticesKey = 'fit.acknowledged-notices';

{ One line per notice whose topic does not resolve - a notice with no words. }
function NoticeFindings(const ANotices: TNotices;
    AResolves: TTopicResolves): TStringArray;

implementation

var
    Notices: TNotices;

procedure RegisterNotice(const ATopic: string; ARequiresAcknowledgement: boolean);
var
    i: integer;
begin
    for i := 0 to High(Notices) do
        if Notices[i].Topic = ATopic then
            Exit;
    SetLength(Notices, Length(Notices) + 1);
    Notices[High(Notices)].Topic := ATopic;
    Notices[High(Notices)].RequiresAcknowledgement := ARequiresAcknowledgement;
end;

function RegisteredNotices: TNotices;
begin
    Result := Copy(Notices);
end;

function MajorVersionOf(const AVersion: string): string;
var
    Dot: integer;
begin
    Dot := Pos('.', AVersion);
    if Dot = 0 then
        Result := AVersion
    else
        Result := Copy(AVersion, 1, Dot - 1);
end;

{ The major version ATopic was acknowledged at in AAcknowledged, or ''.
  An entry that is not topic=major is skipped: the settings may have been
  written by another build. }
function AcknowledgedAt(const AAcknowledged, ATopic: string): string;
var
    Entries: TStringArray;
    Entry: string;
    Eq: integer;
begin
    Result := '';
    Entries := AAcknowledged.Split([';']);
    for Entry in Entries do
    begin
        Eq := Pos('=', Entry);
        if (Eq > 1) and (Copy(Entry, 1, Eq - 1) = ATopic) then
            Result := Copy(Entry, Eq + 1, MaxInt);
    end;
end;

function NoticesToAcknowledge(const ANotices: TNotices;
    const AAcknowledged, AMajor: string): TStringArray;
var
    i: integer;
begin
    Result := nil;
    for i := 0 to High(ANotices) do
        if ANotices[i].RequiresAcknowledgement and
            (AcknowledgedAt(AAcknowledged, ANotices[i].Topic) <> AMajor) then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := ANotices[i].Topic;
        end;
end;

function WithAcknowledgement(const AAcknowledged, ATopic, AMajor: string): string;
var
    Entries: TStringArray;
    Entry: string;
    Eq: integer;
begin
    Result := '';
    Entries := AAcknowledged.Split([';']);
    for Entry in Entries do
    begin
        Eq := Pos('=', Entry);
        //  Kept as they were, unknown ones too; only ATopic's is replaced.
        if (Entry = '') or ((Eq > 1) and (Copy(Entry, 1, Eq - 1) = ATopic)) then
            Continue;
        if Result <> '' then
            Result := Result + ';';
        Result := Result + Entry;
    end;
    if Result <> '' then
        Result := Result + ';';
    Result := Result + ATopic + '=' + AMajor;
end;

function NoticeText(const AExplanation: TExplanation): string;
const
    Para = LineEnding + LineEnding;
var
    i: integer;
begin
    Result := AExplanation.Title + Para + AExplanation.Summary;
    for i := 0 to High(AExplanation.Body) do
        Result := Result + Para + AExplanation.Body[i];
end;

function AcknowledgeNotices(const ANotices: TNotices; const AMajor: string;
    AFind: TFindNoticeWords; AAsk: TAskAcceptance; var AStored: string): boolean;
var
    Topic: string;
    E: TExplanation;
begin
    for Topic in NoticesToAcknowledge(ANotices, AStored, AMajor) do
    begin
        if not AFind(Topic, E) then
            Continue;
        if not AAsk(E.Title, NoticeText(E)) then
            Exit(False);
        AStored := WithAcknowledgement(AStored, Topic, AMajor);
    end;
    Result := True;
end;

function NoticeFindings(const ANotices: TNotices;
    AResolves: TTopicResolves): TStringArray;
var
    i: integer;
begin
    Result := nil;
    for i := 0 to High(ANotices) do
        if not AResolves(ANotices[i].Topic) then
        begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := 'notice ' + ANotices[i].Topic +
                ' names a topic no provider explains';
        end;
end;

end.
