// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(The wire form of a running fit's progress.)

WHAT IT CARRIES. While a fit runs, a client polls GET /problems/<id>/progress and
draws the answer: the loss the engine has reached, one sample per improvement,
and - only for a client that asked - a SNAPSHOT of the model as it stood at one
of those samples. The same record is what IFitService.GetFitProgress returns in
process, so the in-process engine and the HTTP client hand the viewer one shape.

    busy      bool
    elapsed   seconds
    nextSeq   n
    samples   array of objects: seq, t (seconds), value
    snapshot  object, optional: seq, revision, profile (points), delta (points),
              curves (array of objects: id, points, group), groups (array of
              objects: id, rev, n), partial, base - see "A FRAME IS SENT
              AS WHAT MOVED" below
    events    array, optional: seq, t, source, section, topic, title, detail -
              the run's decisions since the seq asked from (the Run Log)

The points are fit_points_json's own shape, so a snapshot curve is read exactly
as GET /curves/<cid>/points is.

A FRAME IS SENT AS WHAT MOVED. A model of a thousand curves is twenty
megabytes of points, and a window polling it twice a second was sent all of it
each time - the reply took the server ten seconds under load, the client gave
up after two, and nothing reached the screen. The curves of a frame belong to
GROUPS, the fit intervals they were drawn by (the background is one more), and
each group carries the REVISION of the frame it last changed in. A poll names
the revision it holds (snapshotSince); the frame it is sent carries only the
curves of the groups changed since, says so (partial, base) and lists every
group (id, rev, n: how many curves it holds) so the reader can keep the others
from what it holds and drop the ones that are gone. A poll from nothing, or a
frame that is not grouped (a server that does not group, or a single fit), is
sent whole, as it always was - the fields are additive, and a reader that does
not know them reads a partial frame as a frame of fewer curves, which is why a
server sends one only to a poll that names a revision.

WHY THE SNAPSHOT IS OPTIONAL AND FORGIVEN. It is ABSENT when there is none rather
than empty, because an empty profile would read as a model with no points; and a
snapshot that cannot be read is DROPPED rather than failing the reply, because
the samples beside it are still a perfectly good loss curve and refusing them
would blank the chart over a decoration. Samples that cannot be read are
refused: without them there is nothing to draw.

A sample with no value is refused rather than read as zero. Zero is a perfect
fit, which is the one reading of a missing number that is certainly wrong.
}
unit fit_progress_json;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, fpjson, jsonparser, fit_points_json,
    difference_structure;

const
    { How many of a run's R-factor samples are kept - by the server's log, by
      the window watching the run, and by the History entry recording it. }
    FIT_PROGRESS_SAMPLES_KEPT = 4000;

type
    { One improvement the engine reported. }
    TFitProgressSample = record
        { Issued in order and never reused, so a client asks for what arrived
          since the last one it has. }
        Seq: longint;
        { Seconds since the operation started. }
        Elapsed: double;
        { The loss the engine reached - the total R-factor over every interval,
          which is the number the status bar shows when the fit is over. }
        Value: double;
    end;
    TFitProgressSamples = array of TFitProgressSample;

    { ONE DECISION A RUN MADE, and why - what the Run Log shows. Numbered with
      the samples, from the same counter, so the one 'since' a poll names
      covers both. }
    TRunEvent = record
        Seq: longint;
        { Seconds since the operation started. }
        Elapsed: double;
        { Who decided: 'framework', or the module's name. }
        Source: string;
        { The stage of the run it belongs to, a heading in the log. }
        Section: string;
        { The guide topic that explains the rule it applied. }
        Topic: string;
        { One line, and the reason in full. }
        Title, Detail: string;
    end;
    TRunEvents = array of TRunEvent;

    { One curve of a snapshot, under the handle the model addresses it by. }
    TFitProgressCurve = record
        Id: string;
        { Title carries the curve type's name, as GET /curves/<cid>/points does. }
        Points: TPointsData;
        { The group the curve belongs to - the fit interval whose worker drew
          it (TFitProgressGroup.Id). Meaningful only in a snapshot that lists
          its groups. }
        Group: longint;
    end;
    TFitProgressCurves = array of TFitProgressCurve;

    { ONE FIT INTERVAL OF A FRAME: which it is, the revision of the frame it
      last changed in, and how many curves it holds - all of them, whether or
      not this reply carries them. }
    TFitProgressGroup = record
        Id: longint;
        Revision: longint;
        Count: longint;
    end;
    TFitProgressGroups = array of TFitProgressGroup;
    { Which groups changed in a frame, by position in its group list. }
    TFrameGroupFlags = array of boolean;

    { The model as it stood at one sample. }
    TFitProgressSnapshot = record
        { The last sample recorded when it was taken. }
        Seq: longint;
        { WHICH FRAME THIS IS: rises with every frame stored, across
          operations, so a client that names the one it drew is sent the next
          one only (fit-performance.md, "polling"). Seq cannot say it - a new
          operation's first frame repeats the last seq of the one before.
          0 from a server that does not number its frames. }
        Revision: longint;
        ComputedProfile: TPointsData;
        DeltaProfile: TPointsData;
        { The curves - only those of the groups changed since BaseRevision when
          Partial. }
        Curves: TFitProgressCurves;
        { The groups the curves belong to, in the order the model lists them;
          empty for a frame that is not grouped. }
        Groups: TFitProgressGroups;
        { Curves holds only the groups changed since the frame of revision
          BaseRevision: the rest are what the reader holds. }
        Partial: boolean;
        BaseRevision: longint;
        { THE FIT INTERVALS THE FRAME WAS TAKEN OVER, as GET /rfactor-bounds
          sends them: an automatic run sets them while it runs, and a client
          holding none judged and drew the whole profile until the run ended.
          Empty from a server that does not send them - the client keeps the
          ones it holds. }
        FitBounds: TPointsData;
        { WHERE THE DIFFERENCE IS NOT NOISE in that frame, computed by the
          server over those intervals (GET /difference-structure's form), for
          the strip the window draws under the moving model. HasStructure is
          False from a server that does not send it; an empty structure is
          one that judged nothing, and clears the strip. }
        HasStructure: boolean;
        Structure: TDifferenceStructure;
    end;

    TFitProgressReport = record
        { An operation is running. }
        Busy: boolean;
        { Seconds it has run - or, once it is over, how long it took. }
        Elapsed: double;
        { One past the last seq issued; what to ask from next time. }
        NextSeq: longint;
        Samples: TFitProgressSamples;
        { What the operation is minimising, and what is minimising it, in the
          words the user chose them by. Empty from a server that does not say -
          the header then reads as it did before either was carried. }
        LossName: string;
        EngineName: string;
        { WHAT THE OPERATION IS DOING BEFORE IT CAN REPORT A VALUE, in the
          user's words - "Starting the Python engine..." - or empty. Without it
          a start of minutes read as "Starting the fit", and then as an engine
          that reports no progress, neither of which was true. }
        Stage: string;
        HasSnapshot: boolean;
        Snapshot: TFitProgressSnapshot;
        { THE PROFILE AS THE OPERATION LEFT IT, when it changed it - Fit >
          Automatically takes the background off before it fits. The engine
          says so through a callback that reaches no client across HTTP, so it
          travels here, with the frames, and the client draws the curves
          against the profile they are fitted to. ProfileRevision counts the
          changes within the operation, so a client redraws only a new one. }
        HasProfile: boolean;
        ProfileRevision: longint;
        Profile: TPointsData;
        { The decisions recorded since the seq asked from; none from a server
          that records none. }
        Events: TRunEvents;
    end;

function FitProgressToJson(const R: TFitProgressReport): TJSONObject;
function FitProgressFromJson(O: TJSONObject; out R: TFitProgressReport): boolean;

{ ASnapshot as a poll that holds the frame of revision ASince is sent it: the
  curves of the groups changed since, and only when some group did not change -
  a frame that is not grouped, a poll from nothing (ASince 0) and a frame in
  which everything moved are sent whole. Nothing is recomputed: the groups carry
  the revisions. }
function SnapshotChangesSince(const ASnapshot: TFitProgressSnapshot;
    ASince: longint): TFitProgressSnapshot;

{ A partial ANew put over AHeld, the frame the reader holds, so ANew is the
  whole frame: for each group in the order ANew lists them, the curves ANew
  carries when the group changed since ANew.BaseRevision, the ones AHeld holds
  otherwise. False - and ANew left as it was - when AHeld does not hold what
  ANew says is unchanged: the reader must then ask from nothing. A frame that
  is not partial is whole already and answers True. }
function MergeSnapshotChanges(const AHeld: TFitProgressSnapshot;
    var ANew: TFitProgressSnapshot): boolean;

type
    { A SNAPSHOT AS TEXT, kept for the revision it was made from. A frame is the
      bulk of a progress reply - every curve's points - and it changes about
      once a second while polls arrive every few hundred milliseconds from each
      client watching: without this each poll walked and wrote it all again.
      Thread safe: the polls come from the server's connection threads.

      A POLL NEVER WAITS FOR A BUILD. Writing a frame of a few hundred curves
      took seconds while the fit held every processor, the client gave up after
      two, the frame it never received made the next one larger, and the window
      stood still. A frame that is not ready is written on a thread of its own
      and the poll is answered without it, as one with nothing new; the next
      poll is handed the finished text. A poll waits at most SNAPSHOT_WAIT_MS
      for it, so a small frame is still answered by the poll that asked. }
    TSnapshotTextCache = class
    private
        FLock: TRTLCriticalSection;
        FRevision: longint;
        { The frame this text answers: a poll from nothing is sent the whole
          frame, one that holds a frame the changes since it. }
        FBase: longint;
        FHasText: boolean;
        FText: string;
        FSerializations: longint;
        { The thread writing a frame out and which frame it is (revision 0 when
          none is being written). }
        FBuilder: TThread;
        FBuildingRevision, FBuildingBase: longint;
    public
        constructor Create;
        destructor Destroy; override;
        { Called by the builder thread. }
        procedure Publish(ARevision, ABase: longint; const AText: string);
        procedure BuildFinished;
        { The JSON of ASnapshot, written on the calling thread. Revision 0 is a
          server that does not number its frames, so two different frames would
          share it: never cached. }
        function TextOf(const ASnapshot: TFitProgressSnapshot): string;
        { The JSON of ASnapshot if it is ready, or becomes ready within AWaitMs;
          False when it is not, with its writing going on behind. WHAT IS READY
          IS SENT WHEN THE ONE ASKED FOR IS NOT: writing a frame takes longer
          than the fit takes to make the next, so waiting for the newest would
          never send any. The text last written stands in for it when it is
          newer than the frame the poll holds (ASince) and is a whole frame or
          the changes since that very frame. A frame that
          is not numbered cannot be kept, and is written here and now. }
        function TryTextOf(const ASnapshot: TFitProgressSnapshot;
            ASince, AWaitMs: longint; out AText: string): boolean;
        { How many times a snapshot was written out: what a test counts. }
        property Serializations: longint read FSerializations;
    end;

const
    { How long a poll waits for the frame it asked for to be written out. }
    SNAPSHOT_WAIT_MS = 100;

{ The whole reply of GET /progress - what OkResponse(FitProgressToJson(R))
  writes, byte for byte - with the snapshot's text taken from ACache. A poll
  that holds the frame of revision ASnapshotSince is sent what moved since it
  (SnapshotChangesSince). }
function FitProgressToResponse(const R: TFitProgressReport;
    ACache: TSnapshotTextCache; ASnapshotSince: longint = 0): string;
{ A run's events as the wire writes them, and back: the one form, kept by a
  History entry too (model_history_json). Anything but an array reads as
  none; an item that is not an object is dropped. }
function RunLogToJson(const AEvents: TRunEvents): TJSONArray;
function RunLogFromJson(AData: TJSONData): TRunEvents;
{ The loss samples, as /progress sends them and a History entry keeps them
  (model_history_json): {seq, t, value} each. False for anything but an array
  of such objects, ASamples then empty. }
function SamplesToJson(const ASamples: TFitProgressSamples): TJSONArray;
function SamplesFromJson(AData: TJSONData; out ASamples: TFitProgressSamples): boolean;
{ ASamples within ACapacity, as a TSampleLog fed them one by one keeps them;
  whole when they fit. A bound below four is taken as four. }
function BoundedSamples(const ASamples: TFitProgressSamples;
    ACapacity: longint = FIT_PROGRESS_SAMPLES_KEPT): TFitProgressSamples;
type
    { The lowest and the highest sample of one stretch of the run, and the
      latest, which is shown only while the stretch is the last one. }
    TSampleBucket = record
        { Which stretch: the sample's time over the log's Width, floored. }
        Index: Int64;
        Lo, Hi, Last: TFitProgressSample;
    end;

    { A RUN'S SAMPLES AS THEY ARRIVE, KEPT WITHIN A BOUND - by the server's
      log, by the window watching the run, and by the History entry recording
      it - by MIN-MAX DECIMATION, the standard way a line or bar chart squeezes
      more samples than it can draw (the core of M4, Jugel et al., VLDB 2014):
      the run's time is cut into equal stretches, and each keeps its lowest
      and its highest sample. When the samples kept would pass Capacity the
      stretches double in width and neighbours merge, lowest with lowest and
      highest with highest. The first sample and the latest are always kept.

      WHY NOT HALVING THE WHOLE LOG, which this replaced: done each time the
      log filled, it halved the oldest part again at every fill, so the first
      minutes of a long fit thinned to two points and a straight line, and
      every peak in them went (reported 2026-10-26). Keeping, at each halving,
      the sample farthest off its neighbours' line kept a spike only until its
      stretch held more spikes than points. Equal stretches of time thin every
      part of the run alike, and the lowest and highest of each are what a
      squeezed chart must still show.

      The fields are the log's own; use the routines below. }
    TSampleLog = record
        Capacity: longint;
        { How many seconds one stretch covers. }
        Width: double;
        HasFirst: boolean;
        First: TFitProgressSample;
        Buckets: array of TSampleBucket;
        Count: longint;
        { How many samples SampleLogSamples answers. }
        Shown: longint;
    end;

procedure SampleLogStart(out ALog: TSampleLog;
    ACapacity: longint = FIT_PROGRESS_SAMPLES_KEPT);
procedure SampleLogAdd(var ALog: TSampleLog; const ASample: TFitProgressSample);
function SampleLogSamples(const ALog: TSampleLog): TFitProgressSamples;

{ Convenience: whole-message encode/decode. }
function FitProgressToJsonString(const R: TFitProgressReport): string;
function FitProgressFromJsonString(const S: string;
    out R: TFitProgressReport): boolean;

implementation

uses
    Math, fit_worker_protocol;

{ The splice cannot miss: the object it was written into holds the placeholder
  by construction. A miss would send a reply without its frame, silently. }
procedure CheckSplice(APosition: longint);
begin
    if APosition < 1 then
        raise Exception.Create('the progress reply lost its snapshot marker');
end;

function GroupsToJson(const AGroups: TFitProgressGroups): TJSONArray;
var
    G: TJSONObject;
    i: longint;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(AGroups) do
    begin
        G := TJSONObject.Create;
        G.Add('id', AGroups[i].Id);
        G.Add('rev', AGroups[i].Revision);
        G.Add('n', AGroups[i].Count);
        Result.Add(G);
    end;
end;

function SnapshotToJson(const S: TFitProgressSnapshot): TJSONObject;
var
    Curves: TJSONArray;
    Curve: TJSONObject;
    i: integer;
begin
    Result := TJSONObject.Create;
    Result.Add('seq', S.Seq);
    Result.Add('revision', S.Revision);
    Result.Add('profile', PointsToJson(S.ComputedProfile));
    Result.Add('delta', PointsToJson(S.DeltaProfile));
    Curves := TJSONArray.Create;
    for i := 0 to High(S.Curves) do
    begin
        Curve := TJSONObject.Create;
        Curve.Add('id', S.Curves[i].Id);
        Curve.Add('points', PointsToJson(S.Curves[i].Points));
        //  ONLY IN A FRAME THAT LISTS ITS GROUPS: an ungrouped frame is what it
        //  always was.
        if Length(S.Groups) > 0 then
            Curve.Add('group', S.Curves[i].Group);
        Curves.Add(Curve);
    end;
    Result.Add('curves', Curves);
    if Length(S.Groups) > 0 then
    begin
        Result.Add('groups', GroupsToJson(S.Groups));
        if S.Partial then
        begin
            Result.Add('partial', True);
            Result.Add('base', S.BaseRevision);
        end;
    end;
    //  ABSENT RATHER THAN EMPTY, both: an older reader ignores them, and a
    //  reader of this one keeps what it holds when they are missing.
    if Length(S.FitBounds.X) > 0 then
        Result.Add('rFactorBounds', PointsToJson(S.FitBounds));
    if S.HasStructure then
        Result.Add('structure', DifferenceStructureToJson(S.Structure));
end;

{ What stands in a reply's object where the cached snapshot text goes. }
const
    SNAPSHOT_PLACEHOLDER = '{snapshot}';

function ProgressObject(const R: TFitProgressReport;
    APlaceholderForSnapshot: boolean): TJSONObject;
begin
    Result := TJSONObject.Create;
    Result.Add('busy', R.Busy);
    Result.Add('elapsed', R.Elapsed);
    //  ABSENT RATHER THAN EMPTY when the server does not name them, so a reader
    //  cannot tell "unknown" from "named as nothing" the wrong way round.
    if R.LossName <> '' then
        Result.Add('lossName', R.LossName);
    if R.EngineName <> '' then
        Result.Add('engineName', R.EngineName);
    if R.Stage <> '' then
        Result.Add('stage', R.Stage);
    Result.Add('nextSeq', R.NextSeq);
    Result.Add('samples', SamplesToJson(R.Samples));
    if R.HasSnapshot then
        if APlaceholderForSnapshot then
            Result.Add('snapshot', SNAPSHOT_PLACEHOLDER)
        else
            Result.Add('snapshot', SnapshotToJson(R.Snapshot));
    //  ABSENT RATHER THAN EMPTY, like the snapshot: an empty profile would read
    //  as one with no points and take the data off the chart.
    if R.HasProfile then
    begin
        Result.Add('profileRevision', R.ProfileRevision);
        Result.Add('experimentalProfile', PointsToJson(R.Profile));
    end;
    //  ONLY WHEN THERE ARE ANY, so a reply with none is what it always was.
    if Length(R.Events) > 0 then
        Result.Add('events', RunLogToJson(R.Events));
end;

function FitProgressToJson(const R: TFitProgressReport): TJSONObject;
begin
    Result := ProgressObject(R, False);
end;

type
    { Writes one frame out and hands the text to the cache. }
    TSnapshotBuilder = class(TThread)
    private
        FCache: TSnapshotTextCache;
        FSnapshot: TFitProgressSnapshot;
    protected
        procedure Execute; override;
    public
        constructor Create(ACache: TSnapshotTextCache;
            const ASnapshot: TFitProgressSnapshot);
    end;

function SnapshotText(const ASnapshot: TFitProgressSnapshot): string;
var
    O: TJSONObject;
begin
    O := SnapshotToJson(ASnapshot);
    try
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

constructor TSnapshotBuilder.Create(ACache: TSnapshotTextCache;
    const ASnapshot: TFitProgressSnapshot);
begin
    //  The arrays are taken by reference and never written again (the log's
    //  StoreSnapshot), so the copy of the record is the frame as it was.
    FCache := ACache;
    FSnapshot := ASnapshot;
    inherited Create(False);
end;

procedure TSnapshotBuilder.Execute;
var
    Text: string;
begin
    try
        Text := SnapshotText(FSnapshot);
        FCache.Publish(FSnapshot.Revision, FSnapshot.BaseRevision, Text);
    except
        //  A frame that cannot be written is one poll without it; the next
        //  asks again.
    end;
    FCache.BuildFinished;
end;

constructor TSnapshotTextCache.Create;
begin
    inherited Create;
    InitCriticalSection(FLock);
end;

destructor TSnapshotTextCache.Destroy;
begin
    if Assigned(FBuilder) then
    begin
        FBuilder.WaitFor;
        FBuilder.Free;
    end;
    DoneCriticalSection(FLock);
    inherited Destroy;
end;

procedure TSnapshotTextCache.Publish(ARevision, ABase: longint;
    const AText: string);
begin
    EnterCriticalSection(FLock);
    try
        Inc(FSerializations);
        FRevision := ARevision;
        FBase := ABase;
        FText := AText;
        FHasText := ARevision <> 0;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

procedure TSnapshotTextCache.BuildFinished;
begin
    EnterCriticalSection(FLock);
    try
        FBuildingRevision := 0;
    finally
        LeaveCriticalSection(FLock);
    end;
end;

function TSnapshotTextCache.TextOf(const ASnapshot: TFitProgressSnapshot): string;
begin
    EnterCriticalSection(FLock);
    try
        if FHasText and (ASnapshot.Revision <> 0) and
           (ASnapshot.Revision = FRevision) and
           (ASnapshot.BaseRevision = FBase) then
            Exit(FText);
    finally
        LeaveCriticalSection(FLock);
    end;
    Result := SnapshotText(ASnapshot);
    Publish(ASnapshot.Revision, ASnapshot.BaseRevision, Result);
end;

function TSnapshotTextCache.TryTextOf(const ASnapshot: TFitProgressSnapshot;
    ASince, AWaitMs: longint; out AText: string): boolean;
var
    Deadline: QWord;
begin
    AText := '';
    if ASnapshot.Revision = 0 then
    begin
        AText := TextOf(ASnapshot);
        Exit(True);
    end;
    Deadline := GetTickCount64 + QWord(AWaitMs);
    repeat
        EnterCriticalSection(FLock);
        try
            if FHasText and (ASnapshot.Revision = FRevision) and
               (ASnapshot.BaseRevision = FBase) then
            begin
                AText := FText;
                Exit(True);
            end;
            //  ONE WRITING AT A TIME: a poll for another frame than the one
            //  being written starts its own when that one is done. The thread
            //  of a finished one has published and is leaving, so it is put
            //  away without a wait.
            if FBuildingRevision = 0 then
            begin
                if Assigned(FBuilder) then
                begin
                    FBuilder.WaitFor;
                    FreeAndNil(FBuilder);
                end;
                FBuildingRevision := ASnapshot.Revision;
                FBuildingBase := ASnapshot.BaseRevision;
                FBuilder := TSnapshotBuilder.Create(Self, ASnapshot);
            end;
        finally
            LeaveCriticalSection(FLock);
        end;
        if GetTickCount64 >= Deadline then
        begin
            EnterCriticalSection(FLock);
            try
                Result := FHasText and (FRevision > ASince) and
                    ((FBase = 0) or (FBase = ASince));
                if Result then
                    AText := FText;
            finally
                LeaveCriticalSection(FLock);
            end;
            Exit;
        end;
        Sleep(1);
    until False;
end;

function SnapshotChangesSince(const ASnapshot: TFitProgressSnapshot;
    ASince: longint): TFitProgressSnapshot;
var
    i, k, Unmoved: longint;
    Moves: boolean;
begin
    Result := ASnapshot;
    if (ASince <= 0) or (Length(ASnapshot.Groups) = 0) then
        Exit;
    Unmoved := 0;
    for i := 0 to High(ASnapshot.Groups) do
        if ASnapshot.Groups[i].Revision <= ASince then
            Inc(Unmoved);
    if Unmoved = 0 then
        Exit;
    Result.Curves := nil;
    for i := 0 to High(ASnapshot.Curves) do
    begin
        //  By the group's id, not its place in the list.
        Moves := False;
        for k := 0 to High(ASnapshot.Groups) do
            if ASnapshot.Groups[k].Id = ASnapshot.Curves[i].Group then
            begin
                Moves := ASnapshot.Groups[k].Revision > ASince;
                Break;
            end;
        if Moves then
        begin
            SetLength(Result.Curves, Length(Result.Curves) + 1);
            Result.Curves[High(Result.Curves)] := ASnapshot.Curves[i];
        end;
    end;
    Result.Partial := True;
    Result.BaseRevision := ASince;
end;

function MergeSnapshotChanges(const AHeld: TFitProgressSnapshot;
    var ANew: TFitProgressSnapshot): boolean;
var
    Merged: TFitProgressCurves;
    g, i, Taken: longint;
    Fresh: boolean;
begin
    Result := True;
    if not ANew.Partial then
        Exit;
    Merged := nil;
    for g := 0 to High(ANew.Groups) do
    begin
        Fresh := ANew.Groups[g].Revision > ANew.BaseRevision;
        Taken := 0;
        if Fresh then
        begin
            for i := 0 to High(ANew.Curves) do
                if ANew.Curves[i].Group = ANew.Groups[g].Id then
                begin
                    SetLength(Merged, Length(Merged) + 1);
                    Merged[High(Merged)] := ANew.Curves[i];
                    Inc(Taken);
                end;
        end
        else
            for i := 0 to High(AHeld.Curves) do
                if AHeld.Curves[i].Group = ANew.Groups[g].Id then
                begin
                    SetLength(Merged, Length(Merged) + 1);
                    Merged[High(Merged)] := AHeld.Curves[i];
                    Inc(Taken);
                end;
        //  WHAT THE SERVER SAYS THE GROUP HOLDS, or the reader and the server
        //  disagree about the frame the poll named.
        if Taken <> ANew.Groups[g].Count then
            Exit(False);
    end;
    ANew.Curves := Merged;
    ANew.Partial := False;
    ANew.BaseRevision := 0;
end;

function FitProgressToResponse(const R: TFitProgressReport;
    ACache: TSnapshotTextCache; ASnapshotSince: longint): string;
var
    Marked, Text: string;
    At: longint;
    Trimmed: TFitProgressReport;
begin
    if not R.HasSnapshot then
        Exit(OkResponse(ProgressObject(R, False)));
    //  THE POLL'S OWN VIEW OF THE FRAME: what moved since the one it holds.
    Trimmed := R;
    Trimmed.Snapshot := SnapshotChangesSince(R.Snapshot, ASnapshotSince);
    Marked := OkResponse(ProgressObject(Trimmed, True));
    //  THE ONE PLACE the snapshot's text is spliced in: it stood as a string
    //  of its own in the object, which a plain string cannot be mistaken for
    //  anywhere else in a reply of numbers and names.
    At := Pos('"' + SNAPSHOT_PLACEHOLDER + '"', Marked);
    CheckSplice(At);
    if not ACache.TryTextOf(Trimmed.Snapshot, ASnapshotSince,
        SNAPSHOT_WAIT_MS, Text) then
    begin
        //  NOT WRITTEN YET: the poll is answered as one with no new frame, and
        //  the next is handed it.
        Trimmed.HasSnapshot := False;
        Exit(OkResponse(ProgressObject(Trimmed, False)));
    end;
    Result := Copy(Marked, 1, At - 1) + Text +
        Copy(Marked, At + Length(SNAPSHOT_PLACEHOLDER) + 2, MaxInt);
end;

function RunLogToJson(const AEvents: TRunEvents): TJSONArray;
var
    Event: TJSONObject;
    i: longint;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(AEvents) do
    begin
        Event := TJSONObject.Create;
        Event.Add('seq', AEvents[i].Seq);
        Event.Add('t', AEvents[i].Elapsed);
        Event.Add('source', AEvents[i].Source);
        Event.Add('section', AEvents[i].Section);
        Event.Add('topic', AEvents[i].Topic);
        Event.Add('title', AEvents[i].Title);
        Event.Add('detail', AEvents[i].Detail);
        Result.Add(Event);
    end;
end;

function RunLogFromJson(AData: TJSONData): TRunEvents;
var
    Item: TJSONObject;
    i, n: longint;
begin
    Result := nil;
    if not (AData is TJSONArray) then
        Exit;
    for i := 0 to TJSONArray(AData).Count - 1 do
        if TJSONArray(AData).Items[i] is TJSONObject then
        begin
            Item := TJSONObject(TJSONArray(AData).Items[i]);
            n := Length(Result);
            SetLength(Result, n + 1);
            Result[n].Seq := Item.Get('seq', 0);
            Result[n].Elapsed := Item.Get('t', 0.0);
            Result[n].Source := Item.Get('source', '');
            Result[n].Section := Item.Get('section', '');
            Result[n].Topic := Item.Get('topic', '');
            Result[n].Title := Item.Get('title', '');
            Result[n].Detail := Item.Get('detail', '');
        end;
end;

{ A number member, or False when it is missing or not a number. }
function ReadNumber(O: TJSONObject; const AName: string; out AValue: double): boolean;
var
    D: TJSONData;
begin
    AValue := 0;
    D := O.Find(AName);
    Result := (D <> nil) and (D.JSONType = jtNumber);
    if Result then
        AValue := D.AsFloat;
end;

function PointsMember(O: TJSONObject; const AName: string;
    out P: TPointsData): boolean;
var
    D: TJSONData;
begin
    P := Default(TPointsData);
    D := O.Find(AName);
    Result := (D is TJSONObject) and PointsFromJson(TJSONObject(D), P);
end;

function SnapshotFromJson(O: TJSONObject; out S: TFitProgressSnapshot): boolean;
var
    D: TJSONData;
    Curves: TJSONArray;
    Curve: TJSONObject;
    i: integer;
begin
    S := Default(TFitProgressSnapshot);
    Result := False;
    S.Seq := O.Get('seq', -1);
    S.Revision := O.Get('revision', 0);
    if not PointsMember(O, 'profile', S.ComputedProfile) then
        Exit;
    //  The difference is drawn beside the model but is not the model: a sender
    //  that has none still sends a picture worth drawing.
    PointsMember(O, 'delta', S.DeltaProfile);
    D := O.Find('curves');
    if not (D is TJSONArray) then
        Exit;
    Curves := TJSONArray(D);
    SetLength(S.Curves, Curves.Count);
    for i := 0 to Curves.Count - 1 do
    begin
        if not (Curves.Items[i] is TJSONObject) then
            Exit;
        Curve := TJSONObject(Curves.Items[i]);
        S.Curves[i].Id := Curve.Get('id', '');
        S.Curves[i].Group := Curve.Get('group', 0);
        if not PointsMember(Curve, 'points', S.Curves[i].Points) then
            Exit;
    end;
    //  FORGIVEN: a frame that names no groups is an ungrouped one, whole.
    D := O.Find('groups');
    if D is TJSONArray then
    begin
        SetLength(S.Groups, TJSONArray(D).Count);
        for i := 0 to High(S.Groups) do
            if TJSONArray(D).Items[i] is TJSONObject then
            begin
                S.Groups[i].Id := TJSONObject(TJSONArray(D).Items[i]).Get('id', i);
                S.Groups[i].Revision :=
                    TJSONObject(TJSONArray(D).Items[i]).Get('rev', 0);
                S.Groups[i].Count :=
                    TJSONObject(TJSONArray(D).Items[i]).Get('n', 0);
            end;
        S.Partial := O.Get('partial', False);
        S.BaseRevision := O.Get('base', 0);
    end;
    //  FORGIVEN, both: the model is the frame, and a frame whose intervals or
    //  strip cannot be read is still worth drawing - the client keeps the
    //  intervals and the strip it holds.
    PointsMember(O, 'rFactorBounds', S.FitBounds);
    D := O.Find('structure');
    S.HasStructure := (D <> nil) and DifferenceStructureFromJson(D, S.Structure);
    Result := True;
end;

function FitProgressFromJson(O: TJSONObject; out R: TFitProgressReport): boolean;
var
    D: TJSONData;
begin
    R := Default(TFitProgressReport);
    Result := False;
    if not Assigned(O) then
        Exit;
    R.Busy := O.Get('busy', False);
    R.Elapsed := O.Get('elapsed', 0.0);
    R.LossName := O.Get('lossName', '');
    R.EngineName := O.Get('engineName', '');
    R.Stage := O.Get('stage', '');
    R.NextSeq := O.Get('nextSeq', 0);

    if not SamplesFromJson(O.Find('samples'), R.Samples) then
        Exit;

    D := O.Find('snapshot');
    if D is TJSONObject then
        R.HasSnapshot := SnapshotFromJson(TJSONObject(D), R.Snapshot);
    if not R.HasSnapshot then
        R.Snapshot := Default(TFitProgressSnapshot);
    R.HasProfile := PointsMember(O, 'experimentalProfile', R.Profile);
    if R.HasProfile then
        R.ProfileRevision := O.Get('profileRevision', 0);
    //  FORGIVEN, like the snapshot: an event that cannot be read is dropped,
    //  and the loss curve beside it still drawn.
    R.Events := RunLogFromJson(O.Find('events'));
    Result := True;
end;

function SamplesToJson(const ASamples: TFitProgressSamples): TJSONArray;
var
    Sample: TJSONObject;
    i: integer;
begin
    Result := TJSONArray.Create;
    for i := 0 to High(ASamples) do
    begin
        Sample := TJSONObject.Create;
        Sample.Add('seq', ASamples[i].Seq);
        Sample.Add('t', ASamples[i].Elapsed);
        Sample.Add('value', ASamples[i].Value);
        Result.Add(Sample);
    end;
end;

const
    { The first stretches: a thousandth of a second, so a short run is kept
      sample by sample until it needs thinning at all. }
    FirstBucketWidth = 1e-3;
    { The two ends and one stretch's lowest and highest. }
    LeastSampleCapacity = 4;

{ How many samples ABucket shows: its lowest and highest, and its latest
  while it is the last stretch - each once. }
function BucketShown(const ABucket: TSampleBucket; AIsLast: boolean): longint;
begin
    Result := 1;
    if ABucket.Hi.Seq <> ABucket.Lo.Seq then
        Inc(Result);
    if AIsLast and (ABucket.Last.Seq <> ABucket.Lo.Seq) and
        (ABucket.Last.Seq <> ABucket.Hi.Seq) then
        Inc(Result);
end;

{ ALog's stretches twice as wide, neighbours merged. }
procedure MergeBuckets(var ALog: TSampleLog);
var
    i, n: longint;
    Idx: Int64;
begin
    ALog.Width := ALog.Width * 2;
    n := 0;
    for i := 0 to ALog.Count - 1 do
    begin
        //  Floored, so -1 and -2 are one stretch as 0 and 1 are.
        Idx := ALog.Buckets[i].Index;
        if Idx >= 0 then
            Idx := Idx div 2
        else
            Idx := -((1 - Idx) div 2);
        if (n > 0) and (ALog.Buckets[n - 1].Index = Idx) then
        begin
            //  Lower than the lowest, higher than the highest; of two equal,
            //  the earlier, as within a stretch.
            if ALog.Buckets[i].Lo.Value < ALog.Buckets[n - 1].Lo.Value then
                ALog.Buckets[n - 1].Lo := ALog.Buckets[i].Lo;
            if ALog.Buckets[i].Hi.Value > ALog.Buckets[n - 1].Hi.Value then
                ALog.Buckets[n - 1].Hi := ALog.Buckets[i].Hi;
            ALog.Buckets[n - 1].Last := ALog.Buckets[i].Last;
        end
        else
        begin
            ALog.Buckets[n] := ALog.Buckets[i];
            ALog.Buckets[n].Index := Idx;
            Inc(n);
        end;
    end;
    ALog.Count := n;
    ALog.Shown := 1;
    for i := 0 to n - 1 do
        Inc(ALog.Shown, BucketShown(ALog.Buckets[i], i = n - 1));
end;

procedure SampleLogStart(out ALog: TSampleLog; ACapacity: longint);
begin
    ALog := Default(TSampleLog);
    ALog.Capacity := Max(ACapacity, LeastSampleCapacity);
    ALog.Width := FirstBucketWidth;
end;

procedure SampleLogAdd(var ALog: TSampleLog; const ASample: TFitProgressSample);
var
    Idx: Int64;
    Last: longint;
begin
    if not ALog.HasFirst then
    begin
        //  KEPT APART: where the improvement is measured from, whatever the
        //  stretch it falls in comes to hold.
        ALog.HasFirst := True;
        ALog.First := ASample;
        ALog.Shown := 1;
        Exit;
    end;
    Idx := Floor(ASample.Elapsed / ALog.Width);
    Last := ALog.Count - 1;
    //  AT OR BEFORE THE LAST STRETCH: samples come in time order, but one at
    //  the moment of the last may round below it.
    if (Last >= 0) and (Idx <= ALog.Buckets[Last].Index) then
    begin
        Dec(ALog.Shown, BucketShown(ALog.Buckets[Last], True));
        if ASample.Value < ALog.Buckets[Last].Lo.Value then
            ALog.Buckets[Last].Lo := ASample;
        if ASample.Value > ALog.Buckets[Last].Hi.Value then
            ALog.Buckets[Last].Hi := ASample;
        ALog.Buckets[Last].Last := ASample;
        Inc(ALog.Shown, BucketShown(ALog.Buckets[Last], True));
    end
    else
    begin
        if Last >= 0 then
        begin
            //  No longer the last: its latest is not shown any more.
            Dec(ALog.Shown, BucketShown(ALog.Buckets[Last], True));
            Inc(ALog.Shown, BucketShown(ALog.Buckets[Last], False));
        end;
        if ALog.Count = Length(ALog.Buckets) then
            SetLength(ALog.Buckets, Max(16, 2 * Length(ALog.Buckets)));
        ALog.Buckets[ALog.Count].Index := Idx;
        ALog.Buckets[ALog.Count].Lo := ASample;
        ALog.Buckets[ALog.Count].Hi := ASample;
        ALog.Buckets[ALog.Count].Last := ASample;
        Inc(ALog.Count);
        Inc(ALog.Shown);
    end;
    //  One stretch is the least there can be: merging it with nothing never
    //  brings it within a bound smaller than itself.
    while (ALog.Shown > ALog.Capacity) and (ALog.Count > 1) do
        MergeBuckets(ALog);
end;

function SampleLogSamples(const ALog: TSampleLog): TFitProgressSamples;
var
    i, n, m, a, b: longint;
    Kept: array[0..2] of TFitProgressSample;
    T: TFitProgressSample;
begin
    Result := nil;
    if not ALog.HasFirst then
        Exit;
    SetLength(Result, ALog.Shown);
    Result[0] := ALog.First;
    n := 1;
    for i := 0 to ALog.Count - 1 do
    begin
        //  The stretch's samples in the order they came, each once.
        m := 1;
        Kept[0] := ALog.Buckets[i].Lo;
        if ALog.Buckets[i].Hi.Seq <> ALog.Buckets[i].Lo.Seq then
        begin
            Kept[m] := ALog.Buckets[i].Hi;
            Inc(m);
        end;
        if (i = ALog.Count - 1) and
            (ALog.Buckets[i].Last.Seq <> ALog.Buckets[i].Lo.Seq) and
            (ALog.Buckets[i].Last.Seq <> ALog.Buckets[i].Hi.Seq) then
        begin
            Kept[m] := ALog.Buckets[i].Last;
            Inc(m);
        end;
        for a := 1 to m - 1 do
            for b := a downto 1 do
                if Kept[b].Seq < Kept[b - 1].Seq then
                begin
                    T := Kept[b];
                    Kept[b] := Kept[b - 1];
                    Kept[b - 1] := T;
                end;
        for a := 0 to m - 1 do
        begin
            Result[n] := Kept[a];
            Inc(n);
        end;
    end;
    SetLength(Result, n);
end;

function BoundedSamples(const ASamples: TFitProgressSamples;
    ACapacity: longint): TFitProgressSamples;
var
    Log: TSampleLog;
    i: longint;
begin
    //  WHOLE WHEN THEY FIT: a History entry within its bound reads back as it
    //  was saved.
    if Length(ASamples) <= Max(ACapacity, LeastSampleCapacity) then
        Exit(Copy(ASamples));
    SampleLogStart(Log, ACapacity);
    for i := 0 to High(ASamples) do
        SampleLogAdd(Log, ASamples[i]);
    Result := SampleLogSamples(Log);
end;

function SamplesFromJson(AData: TJSONData; out ASamples: TFitProgressSamples): boolean;
var
    Samples: TJSONArray;
    Sample: TJSONObject;
    i: integer;
    T, V: double;
begin
    ASamples := nil;
    if not (AData is TJSONArray) then
        Exit(False);
    Samples := TJSONArray(AData);
    SetLength(ASamples, Samples.Count);
    for i := 0 to Samples.Count - 1 do
    begin
        //  ALL OR NOTHING, as the reply was read: a sample that cannot be
        //  read is a list nobody can vouch for.
        if not (Samples.Items[i] is TJSONObject) then
        begin
            ASamples := nil;
            Exit(False);
        end;
        Sample := TJSONObject(Samples.Items[i]);
        if not (ReadNumber(Sample, 't', T) and ReadNumber(Sample, 'value', V)) then
        begin
            ASamples := nil;
            Exit(False);
        end;
        ASamples[i].Seq := Sample.Get('seq', 0);
        ASamples[i].Elapsed := T;
        ASamples[i].Value := V;
    end;
    Result := True;
end;

function FitProgressToJsonString(const R: TFitProgressReport): string;
var
    O: TJSONObject;
begin
    O := FitProgressToJson(R);
    try
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

function FitProgressFromJsonString(const S: string;
    out R: TFitProgressReport): boolean;
var
    D: TJSONData;
begin
    R := Default(TFitProgressReport);
    Result := False;
    D := nil;
    try
        try
            D := GetJSON(S);
        except
            D := nil;
        end;
        if not (D is TJSONObject) then
            Exit;
        Result := FitProgressFromJson(TJSONObject(D), R);
    finally
        D.Free;
    end;
end;

end.
