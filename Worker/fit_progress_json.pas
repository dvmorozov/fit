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
    end;

function FitProgressToJson(const R: TFitProgressReport): TJSONObject;
function FitProgressFromJson(O: TJSONObject; out R: TFitProgressReport): boolean;
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
var
    Samples: TJSONArray;
    Sample: TJSONObject;
    i: integer;
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
    Result.Add('nextSeq', R.NextSeq);
    Samples := TJSONArray.Create;
    for i := 0 to High(R.Samples) do
    begin
        Sample := TJSONObject.Create;
        Sample.Add('seq', R.Samples[i].Seq);
        Sample.Add('t', R.Samples[i].Elapsed);
        Sample.Add('value', R.Samples[i].Value);
        Samples.Add(Sample);
    end;
    Result.Add('samples', Samples);
    if R.HasSnapshot then
        Result.Add('snapshot', SnapshotToJson(R.Snapshot));
    //  ABSENT RATHER THAN EMPTY, like the snapshot: an empty profile would read
    //  as one with no points and take the data off the chart.
    if R.HasProfile then
    begin
        Result.Add('profileRevision', R.ProfileRevision);
        Result.Add('experimentalProfile', PointsToJson(R.Profile));
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
    Samples: TJSONArray;
    Sample: TJSONObject;
    i: integer;
    T, V: double;
begin
    R := Default(TFitProgressReport);
    Result := False;
    if not Assigned(O) then
        Exit;
    R.Busy := O.Get('busy', False);
    R.Elapsed := O.Get('elapsed', 0.0);
    R.LossName := O.Get('lossName', '');
    R.EngineName := O.Get('engineName', '');
    R.NextSeq := O.Get('nextSeq', 0);

    D := O.Find('samples');
    if not (D is TJSONArray) then
        Exit;
    Samples := TJSONArray(D);
    SetLength(R.Samples, Samples.Count);
    for i := 0 to Samples.Count - 1 do
    begin
        if not (Samples.Items[i] is TJSONObject) then
            Exit;
        Sample := TJSONObject(Samples.Items[i]);
        if not (ReadNumber(Sample, 't', T) and ReadNumber(Sample, 'value', V)) then
            Exit;
        R.Samples[i].Seq := Sample.Get('seq', 0);
        R.Samples[i].Elapsed := T;
        R.Samples[i].Value := V;
    end;

    D := O.Find('snapshot');
    if D is TJSONObject then
        R.HasSnapshot := SnapshotFromJson(TJSONObject(D), R.Snapshot);
    if not R.HasSnapshot then
        R.Snapshot := Default(TFitProgressSnapshot);
    R.HasProfile := PointsMember(O, 'experimentalProfile', R.Profile);
    if R.HasProfile then
        R.ProfileRevision := O.Get('profileRevision', 0);
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
