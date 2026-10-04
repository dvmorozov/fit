// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Recording the model a run reached, and making a recorded model the
live one again - through the same capture and restore a project file uses.)

NOTHING HERE IS A NEW WAY TO TALK TO THE ENGINE. Recording is CaptureProject;
making current is ApplyProject, in the order fit_project_restore plans, ending
with the measurement that lets the restored model report its R-factor. A model
from the history is therefore exactly what a model from a file is, and the two
cannot drift apart - which a history-specific restore, one verb at a time, would
have started doing the first time the restore order changed.

MAKING AN ENTRY CURRENT NEVER LOSES WORK. The live model may hold edits made
since the last run - a pick moved, a value typed - which no entry records.
Before it is replaced it is recorded as an entry of its own ("Edited, not
fitted"), unless some entry already holds exactly it. Asking instead would put a
question in front of the one gesture this feature exists to make cheap; keeping
it costs a row the user can delete.
}
unit model_history_session;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, int_fit_service, fit_points_json, fit_project_document,
    fit_project_session, model_history, model_history_json;

{ A new entry id: a GUID, lower case, without braces. }
function NewHistoryId: string;

{ Records the model AService holds as the result of a run of ARunKind, at ANow
  (UTC). False when nothing was recorded: the run left the current entry's model
  as it was. }
function RecordRun(AService: IFitService; const AContext: TProjectClientContext;
    AHistory: TModelHistory; ARunKind: THistoryRunKind; AStopped: boolean;
    ANow: TDateTime; const AId: string): boolean;

{ Makes the entry AId the live model, keeping the model it replaces as an entry
  AKeptId when no entry holds it yet - see the unit comment.

  False, with AFault in words for the user, when the entry cannot be made
  current or the restore failed; AFault then names the step, as Open Project
  does. }
function MakeHistoryEntryCurrent(AService: IFitService;
    const AContext: TProjectClientContext; AHistory: TModelHistory;
    const AId: string; ANow: TDateTime; const AKeptId: string;
    out AFault: string): boolean;

implementation

function NewHistoryId: string;
var
    G: TGuid;
begin
    CreateGUID(G);
    Result := LowerCase(Copy(GUIDToString(G), 2, 36));
end;

{ The live model as an entry would hold it. }
function CaptureEntry(AService: IFitService;
    const AContext: TProjectClientContext; ARunKind: THistoryRunKind;
    AStopped: boolean; ANow: TDateTime; const AId: string;
    out AProfile: TPointsData): THistoryEntry;
begin
    //  FROM NOTHING, not from the document as read: an entry is a model, and
    //  carrying the file's unknown parts into it would make every entry a
    //  copy of the project it was recorded in.
    Result := NewHistoryEntry(CaptureProject(AService, AContext,
        EmptyProjectDocument), AId, ARunKind, AStopped, ANow, AProfile);
end;

function RecordRun(AService: IFitService; const AContext: TProjectClientContext;
    AHistory: TModelHistory; ARunKind: THistoryRunKind; AStopped: boolean;
    ANow: TDateTime; const AId: string): boolean;
var
    Entry: THistoryEntry;
    Profile: TPointsData;
begin
    Entry := CaptureEntry(AService, AContext, ARunKind, AStopped, ANow, AId,
        Profile);
    Result := AHistory.Add(Entry, Profile);
end;

function MakeHistoryEntryCurrent(AService: IFitService;
    const AContext: TProjectClientContext; AHistory: TModelHistory;
    const AId: string; ANow: TDateTime; const AKeptId: string;
    out AFault: string): boolean;
var
    Index: longint;
    Live: THistoryEntry;
    LiveProfile: TPointsData;
    Model: TProjectDocument;
begin
    AFault := '';
    Result := False;
    Index := AHistory.IndexOf(AId);
    if Index < 0 then
    begin
        AFault := 'That model is no longer in the history.';
        Exit;
    end;
    //  ASKED BEFORE ANYTHING IS TOUCHED: a refusal leaves the live model
    //  exactly as it was.
    if not AHistory.ModelToRestore(Index, Model) then
    begin
        AFault := 'This model cannot be made current: the profile it was ' +
            'fitted to is not in the project, and putting it back onto ' +
            'another one would fit it to data it was never fitted to.';
        Exit;
    end;

    //  KEPT FIRST - see the unit comment. Only a model with something in it:
    //  an empty session is not work anybody would want back.
    Live := CaptureEntry(AService, AContext, hrkEdited, False, ANow, AKeptId,
        LiveProfile);
    if (AHistory.IndexOfContent(Live.ContentHash) < 0) and
        ((Length(Live.Model.Curves) > 0) or
        (Length(Live.Model.Positions.X) > 0)) then
        AHistory.Add(Live, LiveProfile);

    Result := ApplyProject(AService, Model, AFault);
    //  MARKED CURRENT ONLY ONCE IT IS: a restore that stopped half way has
    //  left a model that is neither, and the entry the user can go back to is
    //  the edit just kept.
    if Result then
        AHistory.SetCurrent(AId);
end;

end.
