// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The model history as parts of the project file, and the two hashes that
say whether two models - or two profiles - are the same one.)

PARTS, NOT A SECTION. The project file is a container of named parts so that a
feature adds parts and edits nothing else (fit_project_archive): the history is

    history/index.json              the lineage, the names, which one is current
    history/entries/<id>.json       one model, in the project's own sections
    history/profiles/<hash>.json    one profile, however many models share it

so a project with no history is byte-for-byte what it was before the feature
existed, and an older build carries the parts through untouched.

A RECORDED MODEL NEVER CHANGES, and the writer leans on that: an entry or a
profile part already in the file is written back exactly as it was read - so a
member a newer build put there survives an older build's save without this unit
knowing it exists. Only the index is rewritten, because a name and the current
entry do change; it keeps every member it does not know, at the top and inside
each entry (fit_project_json's second preservation rule, applied here).

WHAT THE CONTENT HASH LEAVES OUT. Two models are the same model when what they are
built from and the values they hold are the same. The R-factor and the statistics
are MEASURED from that, and a measurement repeated after a restore may differ in
the last bit; the provenance and the stamps say where and when, not what. Leaving
them in made a model put back from the history look different from itself, and
so be recorded again as an edit nobody made.
}
unit model_history_json;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpjson, md5,
    fit_points_json, fit_project_archive, fit_project_document,
    fit_project_json, fit_statistics, model_history;

const
    HistoryPartPrefix = 'history/';
    HistoryIndexPart = 'history/index.json';

{ The part holding the model of the entry AId. }
function HistoryEntryPartName(const AId: string): string;
{ The part holding the profile AHash names. }
function HistoryProfilePartName(const AHash: string): string;

{ A profile reduced to one string that changes when - and only when - a point
  of it does. }
function PointsHash(const APoints: TPointsData): string;
{ A model reduced to one string, equal for two models built from the same
  inputs with the same values - see the unit comment for what it leaves out. }
function ModelContentHash(const AModel: TProjectDocument;
    const AProfileHash: string): string;

{ The history entry for a model just captured from the engine (ACapture),
  recorded at ANow (UTC). AProfile receives the profile, which the entry
  itself only names: see model_history. }
function NewHistoryEntry(const ACapture: TProjectDocument; const AId: string;
    ARunKind: THistoryRunKind; AStopped: boolean; ANow: TDateTime;
    out AProfile: TPointsData): THistoryEntry;

{ AParts with the history in it: every history part AHistory no longer needs
  removed, the ones it holds kept or added, the index rewritten. AHistory nil
  or empty leaves no history part at all. }
function HistoryToParts(const AParts: TProjectParts;
    AHistory: TModelHistory): TProjectParts;

{ Reads the history AParts holds into AHistory, replacing what it held. An
  entry whose model part is missing or unreadable is left out; one whose
  profile is missing is kept (TModelHistory.IsRestorable says no). Never
  raises: a damaged history must not make the project unopenable. }
procedure HistoryFromParts(const AParts: TProjectParts;
    AHistory: TModelHistory);

implementation

const
    EntriesPrefix = 'history/entries/';
    ProfilesPrefix = 'history/profiles/';

{ The run kinds as the file names them. Words, not ordinals, so that inserting
  a kind cannot renumber the ones files already hold. }
function RunToken(AKind: THistoryRunKind): string;
begin
    case AKind of
        hrkMinimizeDifference:     Result := 'minimize-difference';
        hrkMinimizeNumberOfCurves: Result := 'minimize-number-of-curves';
        hrkAutomatically:          Result := 'automatically';
        hrkEdited:                 Result := 'edited';
    else
        Result := '';
    end;
end;

function RunKindOfToken(const AToken: string): THistoryRunKind;
var
    K: THistoryRunKind;
begin
    for K := Low(THistoryRunKind) to High(THistoryRunKind) do
        if (K <> hrkOther) and (RunToken(K) = AToken) then
            Exit(K);
    Result := hrkOther;
end;

function HistoryEntryPartName(const AId: string): string;
begin
    Result := EntriesPrefix + AId + '.json';
end;

function HistoryProfilePartName(const AHash: string): string;
begin
    Result := ProfilesPrefix + AHash + '.json';
end;

function PointsHash(const APoints: TPointsData): string;
var
    Bare: TPointsData;
begin
    //  The points alone: a title is a caption, not data.
    Bare := APoints;
    Bare.Title := '';
    Result := LowerCase(MD5Print(MD5String(PointsToJsonString(Bare))));
end;

{ What a model is, with what is measured from it and what says where and when
  taken out - see the unit comment. }
function Essence(const AModel: TProjectDocument): TProjectDocument;
begin
    Result := AModel;
    Result.RFactor := -1;
    Result.Statistics := EmptyFitStatistics;
    Result.Provenance := Default(TProjectProvenance);
    Result.CreatedUtc := '';
    Result.ModifiedUtc := '';
    Result.HasUi := False;
    Result.Ui := Default(TProjectUi);
    Result.AsRead := nil;
    Result.Profile := Default(TPointsData);
end;

function ModelContentHash(const AModel: TProjectDocument;
    const AProfileHash: string): string;
var
    E: TProjectDocument;
    Text: string;
    i: longint;
begin
    E := Essence(AModel);
    Text := AProfileHash + #0 + ProblemJson(E) + #0 + ResultsJson(E);
    for i := 0 to High(E.ModuleDocuments) do
        Text := Text + #0 + E.ModuleDocuments[i].Module + #0 +
            E.ModuleDocuments[i].Content;
    Result := LowerCase(MD5Print(MD5String(Text)));
end;

function NewHistoryEntry(const ACapture: TProjectDocument; const AId: string;
    ARunKind: THistoryRunKind; AStopped: boolean; ANow: TDateTime;
    out AProfile: TPointsData): THistoryEntry;
begin
    Result := Default(THistoryEntry);
    Result.Id := AId;
    Result.CreatedUtc := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', ANow);
    Result.RunKind := ARunKind;
    Result.Stopped := AStopped;
    AProfile := ACapture.Profile;
    Result.ProfileHash := PointsHash(AProfile);
    //  THE MEASUREMENT IS KEPT in the entry - it is what the History tab shows
    //  - and left out only of what decides whether two models are the same.
    Result.Model := Essence(ACapture);
    Result.Model.RFactor := ACapture.RFactor;
    Result.Model.Statistics := ACapture.Statistics;
    Result.ContentHash := ModelContentHash(Result.Model, Result.ProfileHash);
end;

{ ---- writing ---------------------------------------------------------------- }

function EntryJson(const AEntry: THistoryEntry): string;
var
    O, M: TJSONObject;
    Modules: TJSONArray;
    i: longint;
begin
    O := TJSONObject.Create;
    try
        //  THE PROJECT'S OWN SECTIONS, nested: a recorded model is read by the
        //  code that reads a project's model.
        O.Add('problem', AsObject(ProblemJson(AEntry.Model)));
        O.Add('results', AsObject(ResultsJson(AEntry.Model)));
        Modules := TJSONArray.Create;
        for i := 0 to High(AEntry.Model.ModuleDocuments) do
        begin
            M := TJSONObject.Create;
            M.Add('module', AEntry.Model.ModuleDocuments[i].Module);
            //  AS TEXT: a module's document is the module's business, and the
            //  framework does not parse it (fit_project_json says why).
            M.Add('content', AEntry.Model.ModuleDocuments[i].Content);
            Modules.Add(M);
        end;
        O.Add('modules', Modules);
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

{ ANew, plus every member of AOld that ANew does not name. }
procedure KeepUnknownMembers(ANew, AOld: TJSONObject);
var
    i: longint;
begin
    if not Assigned(AOld) then
        Exit;
    for i := 0 to AOld.Count - 1 do
        if ANew.IndexOfName(AOld.Names[i]) < 0 then
            ANew.Add(AOld.Names[i], AOld.Items[i].Clone);
end;

{ The entry object AId had in the index as read, or nil. }
function OldEntryObject(AOldIndex: TJSONObject; const AId: string): TJSONObject;
var
    D: TJSONData;
    i: longint;
begin
    Result := nil;
    if not Assigned(AOldIndex) then
        Exit;
    D := AOldIndex.Find('entries');
    if not (D is TJSONArray) then
        Exit;
    for i := 0 to TJSONArray(D).Count - 1 do
        if (TJSONArray(D).Items[i] is TJSONObject) and
            (TJSONObject(TJSONArray(D).Items[i]).Get('id', '') = AId) then
            Exit(TJSONObject(TJSONArray(D).Items[i]));
end;

function IndexJson(AHistory: TModelHistory; const AOldText: string): string;
var
    O, E, Old: TJSONObject;
    Entries: TJSONArray;
    i: longint;
begin
    Old := AsObject(AOldText);
    O := TJSONObject.Create;
    try
        O.Add('current', AHistory.CurrentId);
        Entries := TJSONArray.Create;
        for i := 0 to AHistory.Count - 1 do
        begin
            E := TJSONObject.Create;
            E.Add('id', AHistory[i].Id);
            E.Add('parent', AHistory[i].ParentId);
            E.Add('created', AHistory[i].CreatedUtc);
            //  A KIND THIS BUILD CANNOT NAME is not written as one: the
            //  word the newer build wrote comes back with the other members
            //  this build did not read.
            if AHistory[i].RunKind <> hrkOther then
                E.Add('run', RunToken(AHistory[i].RunKind));
            E.Add('stopped', AHistory[i].Stopped);
            E.Add('name', AHistory[i].Name);
            E.Add('profile', AHistory[i].ProfileHash);
            KeepUnknownMembers(E, OldEntryObject(Old, AHistory[i].Id));
            Entries.Add(E);
        end;
        O.Add('entries', Entries);
        KeepUnknownMembers(O, Old);
        Result := O.AsJSON;
    finally
        O.Free;
        Old.Free;
    end;
end;

function HistoryToParts(const AParts: TProjectParts;
    AHistory: TModelHistory): TProjectParts;
var
    Wanted: TProjectParts;
    Profiles: THistoryProfiles;
    OldIndex, Existing: string;
    i, n: longint;
begin
    if not PartContent(AParts, HistoryIndexPart, OldIndex) then
        OldIndex := '';

    //  WHAT THE HISTORY NEEDS, keeping a part already there exactly as it was
    //  read: a recorded model never changes, and writing it back from what was
    //  parsed would drop whatever a newer build added to it.
    Wanted := nil;
    if Assigned(AHistory) and (AHistory.Count > 0) then
    begin
        Wanted := WithPart(Wanted, HistoryIndexPart,
            IndexJson(AHistory, OldIndex));
        for i := 0 to AHistory.Count - 1 do
            if PartContent(AParts, HistoryEntryPartName(AHistory[i].Id),
                Existing) then
                Wanted := WithPart(Wanted, HistoryEntryPartName(AHistory[i].Id),
                    Existing)
            else
                Wanted := WithPart(Wanted, HistoryEntryPartName(AHistory[i].Id),
                    EntryJson(AHistory[i]));
        Profiles := AHistory.Profiles;
        for i := 0 to High(Profiles) do
            if PartContent(AParts, HistoryProfilePartName(Profiles[i].Hash),
                Existing) then
                Wanted := WithPart(Wanted,
                    HistoryProfilePartName(Profiles[i].Hash), Existing)
            else
                Wanted := WithPart(Wanted,
                    HistoryProfilePartName(Profiles[i].Hash),
                    PointsToJsonString(Profiles[i].Points));
    end;

    //  EVERY OTHER HISTORY PART GOES: one the user deleted would otherwise be
    //  read back - and reappear - the next time the project is opened.
    Result := nil;
    n := 0;
    for i := 0 to High(AParts) do
        if Pos(HistoryPartPrefix, AParts[i].Name) <> 1 then
        begin
            SetLength(Result, n + 1);
            Result[n] := AParts[i];
            Inc(n);
        end;
    for i := 0 to High(Wanted) do
        Result := WithPart(Result, Wanted[i].Name, Wanted[i].Content);
end;

{ ---- reading ---------------------------------------------------------------- }

{ The model in an entry's part; False when it cannot be read. }
function ModelOfEntryPart(const AText: string;
    out AModel: TProjectDocument): boolean;
var
    O, M: TJSONObject;
    D: TJSONData;
    i, n: longint;
begin
    AModel := EmptyProjectDocument;
    O := AsObject(AText);
    Result := Assigned(O);
    if not Result then
        Exit;
    try
        D := O.Find('problem');
        if D is TJSONObject then
            ProblemFromJson(D.AsJSON, AModel);
        D := O.Find('results');
        if D is TJSONObject then
            ResultsFromJson(D.AsJSON, AModel);
        D := O.Find('modules');
        if D is TJSONArray then
        begin
            n := 0;
            for i := 0 to TJSONArray(D).Count - 1 do
                if TJSONArray(D).Items[i] is TJSONObject then
                begin
                    M := TJSONObject(TJSONArray(D).Items[i]);
                    SetLength(AModel.ModuleDocuments, n + 1);
                    AModel.ModuleDocuments[n].Module := M.Get('module', '');
                    AModel.ModuleDocuments[n].Content := M.Get('content', '');
                    Inc(n);
                end;
        end;
    finally
        O.Free;
    end;
end;

procedure HistoryFromParts(const AParts: TProjectParts;
    AHistory: TModelHistory);
var
    Index, E: TJSONObject;
    D: TJSONData;
    Text: string;
    Entries: THistoryEntries;
    Profiles: THistoryProfiles;
    Entry: THistoryEntry;
    P: TPointsData;
    i, n: longint;
begin
    AHistory.Clear;
    if not PartContent(AParts, HistoryIndexPart, Text) then
        Exit;
    Index := AsObject(Text);
    if not Assigned(Index) then
        Exit;
    try
        Entries := nil;
        n := 0;
        D := Index.Find('entries');
        if D is TJSONArray then
            for i := 0 to TJSONArray(D).Count - 1 do
            begin
                if not (TJSONArray(D).Items[i] is TJSONObject) then
                    Continue;
                E := TJSONObject(TJSONArray(D).Items[i]);
                Entry := Default(THistoryEntry);
                Entry.Id := E.Get('id', '');
                if (Entry.Id = '') or not PartContent(AParts,
                    HistoryEntryPartName(Entry.Id), Text) or
                    not ModelOfEntryPart(Text, Entry.Model) then
                    //  NO MODEL, NO ENTRY: a row that could be neither
                    //  shown truthfully nor made current.
                    Continue;
                Entry.ParentId := E.Get('parent', '');
                Entry.CreatedUtc := E.Get('created', '');
                Entry.RunKind := RunKindOfToken(E.Get('run', ''));
                Entry.Stopped := E.Get('stopped', False);
                Entry.Name := E.Get('name', '');
                Entry.ProfileHash := E.Get('profile', '');
                Entry.ContentHash := ModelContentHash(Entry.Model,
                    Entry.ProfileHash);
                SetLength(Entries, n + 1);
                Entries[n] := Entry;
                Inc(n);
            end;

        //  THE PROFILES THE ENTRIES NAME, each once. One that is missing or
        //  unreadable is simply not there: its entries stay, refused.
        Profiles := nil;
        for i := 0 to High(Entries) do
        begin
            if (Entries[i].ProfileHash = '') then
                Continue;
            n := 0;
            while (n <= High(Profiles)) and
                (Profiles[n].Hash <> Entries[i].ProfileHash) do
                Inc(n);
            if n <= High(Profiles) then
                Continue;
            if PartContent(AParts, HistoryProfilePartName(
                Entries[i].ProfileHash), Text) and
                PointsFromJsonString(Text, P) then
            begin
                SetLength(Profiles, Length(Profiles) + 1);
                Profiles[High(Profiles)].Hash := Entries[i].ProfileHash;
                Profiles[High(Profiles)].Points := P;
            end;
        end;

        AHistory.Load(Entries, Profiles, Index.Get('current', ''));
    finally
        Index.Free;
    end;
end;

end.
