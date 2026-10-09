// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The models a project has been through: every run's result, which one
is current, and how each descends from the one it started from.)

WHAT AN ENTRY IS. A model as a project file holds one - the inputs plus the values
a fit found (fit_project_document) - recorded when a run ends. Not a new kind of
record: making an entry current is the restore Open Project already does
(fit_project_session.ApplyProject), so a model that comes back from the history
is indistinguishable from one that comes back from a file. A second, parallel way
to put a model back would be the bypass AGENTS.md rule 6 forbids.

WHY THE PROFILE IS KEPT APART. Smoothing rewrites the profile in place, so a model
is only meaningful against the profile it was fitted to - an entry has to name
it. Stored inside every entry it would multiply the largest thing in the project
by the number of runs; stored once under its hash, most entries share one.

THE LINEAGE. An entry's parent is the entry that was current when it was
recorded - the model the run started from. Making an older entry current and
fitting again therefore branches, which is what the History tab draws. Deleting an
entry hands its children to its own parent, so the lineage stays connected rather
than leaving orphans that claim to start from nothing.

WHY NOTHING HERE TALKS TO THE ENGINE. Every decision - what counts as a new model,
what deletion does to the lineage, which entry is the best - is a function of
records, so it is tested exhaustively without a service. Capturing and applying is
model_history_session's job, one layer up; the file format is model_history_json's.
}
unit model_history;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_points_json, fit_project_document;

type
    { What ended in the model an entry records.

      THE MENU'S OWN NAMES, so the History tab says what the user pressed.
      hrkEdited is the model as the user left it by hand: recorded only when
      making another entry current would otherwise lose it. hrkOther is a kind a
      newer build wrote and this one does not know - kept, and shown as a run. }
    THistoryRunKind = (hrkMinimizeDifference, hrkMinimizeNumberOfCurves,
        hrkAutomatically, hrkEdited, hrkOther);

    THistoryEntry = record
        { A GUID as text, lower case and without braces: it names a part of
          the project file, and a brace is a character no tool expects there. }
        Id: string;
        { The entry that was current when this one was recorded; '' for a
          root. }
        ParentId: string;
        { When, as ISO 8601 UTC - the form the project file writes every time
          in, and one that sorts as text. }
        CreatedUtc: string;
        RunKind: THistoryRunKind;
        { The run was cut short by Stop. What it reached is still a model
          (AGENTS.md, "Runs are incremental"), so it is recorded - and said. }
        Stopped: boolean;
        { The user's own name for it; '' when they gave none. }
        Name: string;
        { Which profile the model was fitted to: a key into the history's
          profiles. }
        ProfileHash: string;
        { What the model holds, reduced to one string: equal for two entries
          holding the same model however they were reached. Not stored - it is
          a function of the model, recomputed on reading. }
        ContentHash: string;
        { The model itself, with its Profile EMPTY - see the unit comment. }
        Model: TProjectDocument;
    end;
    THistoryEntries = array of THistoryEntry;

    THistoryProfile = record
        Hash: string;
        Points: TPointsData;
    end;
    THistoryProfiles = array of THistoryProfile;

    TModelHistory = class
    private
        FEntries: THistoryEntries;
        FProfiles: THistoryProfiles;
        FCurrentId: string;
        function GetEntry(AIndex: longint): THistoryEntry;
        function IndexOfProfile(const AHash: string): longint;
        procedure DropUnusedProfiles;
    public
        procedure Clear;
        function Count: longint;
        { In the order they were recorded, oldest first. }
        property Entries[AIndex: longint]: THistoryEntry read GetEntry; default;
        function IndexOf(const AId: string): longint;
        { The entry the live model was last made from or recorded as; '' when
          there is none - nothing recorded yet, or the current one deleted. }
        property CurrentId: string read FCurrentId;
        { The first entry holding the model AContentHash describes, or -1. }
        function IndexOfContent(const AContentHash: string): longint;

        { Records AEntry - with AProfile, the profile it was fitted to - as a
          child of the current entry, and makes it current.

          FALSE, AND NOTHING RECORDED, when it holds exactly what the current
          entry holds: a run that changed nothing is not a new model, and a
          list that grows by one identical row per press of Fit is a list
          nobody can find anything in. }
        function Add(const AEntry: THistoryEntry;
            const AProfile: TPointsData): boolean;
        { Removes the entry AId names. Its children become children of its
          parent. When it was current, nothing is current any more: the live
          model is unchanged, it has simply lost its record. }
        procedure Delete(const AId: string);
        { Marks AId current - after the session has put its model back. '' is
          allowed and means none. }
        procedure SetCurrent(const AId: string);
        procedure Rename(const AId, AName: string);

        { Whether the entry at AIndex can be made current: its profile is
          there. A project damaged by hand, or written by a build that lost it,
          may hold an entry whose profile is gone - kept, shown, refused. }
        function IsRestorable(AIndex: longint): boolean;
        { The model at AIndex as a restore takes it: with its profile put
          back. False when IsRestorable is. }
        function ModelToRestore(AIndex: longint;
            out ADoc: TProjectDocument): boolean;

        { The entry with the lowest R-factor among those comparable with the
          current one, or -1.

          COMPARABLE MEANS THE SAME DATA AND THE SAME OBJECTIVE. An R-factor
          over a smoothed profile, or a different loss, is a different number
          about a different question - ranking it beside the others would
          present a choice of objective as an improvement in the fit. With no
          current entry, the newest one is the reference. }
        function BestIndex: longint;

        { What the file format reads and writes. }
        function Profiles: THistoryProfiles;
        procedure Load(const AEntries: THistoryEntries;
            const AProfiles: THistoryProfiles; const ACurrentId: string);
    end;

{ The run kind's name as the menu says it. }
function HistoryRunKindName(AKind: THistoryRunKind): string;

implementation

function HistoryRunKindName(AKind: THistoryRunKind): string;
begin
    case AKind of
        hrkMinimizeDifference:     Result := 'Minimize Difference';
        hrkMinimizeNumberOfCurves: Result := 'Minimize Number of Curves';
        hrkAutomatically:          Result := 'Automatically';
        hrkEdited:                 Result := 'Edited, not fitted';
    else
        Result := 'Run';
    end;
end;

{ TModelHistory }

procedure TModelHistory.Clear;
begin
    FEntries := nil;
    FProfiles := nil;
    FCurrentId := '';
end;

function TModelHistory.Count: longint;
begin
    Result := Length(FEntries);
end;

function TModelHistory.GetEntry(AIndex: longint): THistoryEntry;
begin
    Result := FEntries[AIndex];
end;

function TModelHistory.IndexOf(const AId: string): longint;
var
    i: longint;
begin
    for i := 0 to High(FEntries) do
        if FEntries[i].Id = AId then
            Exit(i);
    Result := -1;
end;

function TModelHistory.IndexOfProfile(const AHash: string): longint;
var
    i: longint;
begin
    for i := 0 to High(FProfiles) do
        if FProfiles[i].Hash = AHash then
            Exit(i);
    Result := -1;
end;

function TModelHistory.IndexOfContent(const AContentHash: string): longint;
var
    i: longint;
begin
    for i := 0 to High(FEntries) do
        if FEntries[i].ContentHash = AContentHash then
            Exit(i);
    Result := -1;
end;

function TModelHistory.Add(const AEntry: THistoryEntry;
    const AProfile: TPointsData): boolean;
var
    Current, n: longint;
begin
    Current := IndexOf(FCurrentId);
    if (Current >= 0) and (FEntries[Current].ContentHash = AEntry.ContentHash)
    then
        Exit(False);
    n := Length(FEntries);
    SetLength(FEntries, n + 1);
    FEntries[n] := AEntry;
    //  THE MODEL THE RUN STARTED FROM, whatever the caller thought: the
    //  lineage is this object's to keep, and one place deciding it is what
    //  stops two callers disagreeing about it.
    FEntries[n].ParentId := FCurrentId;
    if IndexOfProfile(AEntry.ProfileHash) < 0 then
    begin
        SetLength(FProfiles, Length(FProfiles) + 1);
        FProfiles[High(FProfiles)].Hash := AEntry.ProfileHash;
        FProfiles[High(FProfiles)].Points := AProfile;
    end;
    FCurrentId := AEntry.Id;
    Result := True;
end;

procedure TModelHistory.DropUnusedProfiles;
var
    Kept: THistoryProfiles;
    i, j, n: longint;
    Used: boolean;
begin
    Kept := nil;
    n := 0;
    for i := 0 to High(FProfiles) do
    begin
        Used := False;
        for j := 0 to High(FEntries) do
            if FEntries[j].ProfileHash = FProfiles[i].Hash then
            begin
                Used := True;
                Break;
            end;
        if Used then
        begin
            SetLength(Kept, n + 1);
            Kept[n] := FProfiles[i];
            Inc(n);
        end;
    end;
    FProfiles := Kept;
end;

procedure TModelHistory.Delete(const AId: string);
var
    Index, i: longint;
    Parent: string;
begin
    Index := IndexOf(AId);
    if Index < 0 then
        Exit;
    Parent := FEntries[Index].ParentId;
    for i := 0 to High(FEntries) do
        if FEntries[i].ParentId = AId then
            FEntries[i].ParentId := Parent;
    for i := Index to High(FEntries) - 1 do
        FEntries[i] := FEntries[i + 1];
    SetLength(FEntries, Length(FEntries) - 1);
    if FCurrentId = AId then
        FCurrentId := '';
    DropUnusedProfiles;
end;

procedure TModelHistory.SetCurrent(const AId: string);
begin
    if (AId = '') or (IndexOf(AId) >= 0) then
        FCurrentId := AId;
end;

procedure TModelHistory.Rename(const AId, AName: string);
var
    Index: longint;
begin
    Index := IndexOf(AId);
    if Index >= 0 then
        FEntries[Index].Name := AName;
end;

function TModelHistory.IsRestorable(AIndex: longint): boolean;
begin
    Result := (AIndex >= 0) and (AIndex < Length(FEntries)) and
        (IndexOfProfile(FEntries[AIndex].ProfileHash) >= 0);
end;

function TModelHistory.ModelToRestore(AIndex: longint;
    out ADoc: TProjectDocument): boolean;
begin
    ADoc := EmptyProjectDocument;
    Result := IsRestorable(AIndex);
    if not Result then
        Exit;
    ADoc := FEntries[AIndex].Model;
    ADoc.Profile := FProfiles[IndexOfProfile(FEntries[AIndex].ProfileHash)].Points;
end;

function TModelHistory.BestIndex: longint;
var
    Reference, i: longint;
    Best: double;
begin
    Result := -1;
    Reference := IndexOf(FCurrentId);
    if Reference < 0 then
        Reference := High(FEntries);
    if Reference < 0 then
        Exit;
    Best := 0;
    for i := 0 to High(FEntries) do
        if (FEntries[i].Model.RFactor >= 0) and
            (FEntries[i].ProfileHash = FEntries[Reference].ProfileHash) and
            (FEntries[i].Model.Settings.LossKind =
                FEntries[Reference].Model.Settings.LossKind) and
            ((Result < 0) or (FEntries[i].Model.RFactor < Best)) then
        begin
            Result := i;
            Best := FEntries[i].Model.RFactor;
        end;
end;

function TModelHistory.Profiles: THistoryProfiles;
begin
    Result := Copy(FProfiles);
end;

procedure TModelHistory.Load(const AEntries: THistoryEntries;
    const AProfiles: THistoryProfiles; const ACurrentId: string);
begin
    FEntries := Copy(AEntries);
    FProfiles := Copy(AProfiles);
    FCurrentId := '';
    SetCurrent(ACurrentId);
end;

end.
