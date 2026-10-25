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
    snapshot  object, optional: seq, profile (points), delta (points),
              curves (array of objects: id, points)
    events    array, optional: seq, t, source, section, topic, title, detail -
              the run's decisions since the seq asked from (the Run Log)

The points are fit_points_json's own shape, so a snapshot curve is read exactly
as GET /curves/<cid>/points is.

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
    SysUtils, Classes, fpjson, jsonparser, fit_points_json;

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
    end;
    TFitProgressCurves = array of TFitProgressCurve;

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
        Curves: TFitProgressCurves;
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
{ ASamples halved: the first, every second one after it, and the latest. }
function HalvedSamples(const ASamples: TFitProgressSamples): TFitProgressSamples;
{ ASamples halved until at most ACapacity are left; whole when they fit. }
function BoundedSamples(const ASamples: TFitProgressSamples;
    ACapacity: longint = FIT_PROGRESS_SAMPLES_KEPT): TFitProgressSamples;
{ Convenience: whole-message encode/decode. }
function FitProgressToJsonString(const R: TFitProgressReport): string;
function FitProgressFromJsonString(const S: string;
    out R: TFitProgressReport): boolean;

implementation

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
        Curves.Add(Curve);
    end;
    Result.Add('curves', Curves);
end;

function FitProgressToJson(const R: TFitProgressReport): TJSONObject;
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
        if not PointsMember(Curve, 'points', S.Curves[i].Points) then
            Exit;
    end;
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

function HalvedSamples(const ASamples: TFitProgressSamples): TFitProgressSamples;
var
    i, Kept: longint;
begin
    Result := nil;
    if Length(ASamples) = 0 then
        Exit;
    //  THE FIRST AND THE LATEST ALWAYS SURVIVE: the first is where the
    //  improvement is measured from and the latest is what the run reached.
    //  Every second one between keeps the chart's shape at any length.
    SetLength(Result, Length(ASamples) div 2 + 1);
    Kept := 0;
    i := 0;
    while i < High(ASamples) do
    begin
        Result[Kept] := ASamples[i];
        Inc(Kept);
        Inc(i, 2);
    end;
    Result[Kept] := ASamples[High(ASamples)];
    SetLength(Result, Kept + 1);
end;

function BoundedSamples(const ASamples: TFitProgressSamples;
    ACapacity: longint): TFitProgressSamples;
begin
    //  THE SERVER'S RULE, applied wherever samples pile up. The server halves
    //  its log at FIT_PROGRESS_SAMPLES_KEPT, but a client polling a long run
    //  received every sample before each halving and kept them all: an hour of
    //  improvements came to 322 923 samples, a 28 MB History index that every
    //  close then re-encoded to decide whether anything had changed (the
    //  S&P 500 project, 2026-10-24). Rejected: thinning by the chart's pixels,
    //  a second rule to keep in step with the server's for no visible gain.
    Result := Copy(ASamples);
    if ACapacity < 2 then
        ACapacity := 2;
    while Length(Result) > ACapacity do
        Result := HalvedSamples(Result);
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
