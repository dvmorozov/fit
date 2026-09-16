// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(JSON codec for point sets exchanged between client and compute server.)

Point sets are the currency of the IFitService surface (profile, background,
curve positions, R-factor bounds, calculated/delta profiles), so they get one
codec used by every endpoint. A set is encoded as

    "title": ..., "x": [...], "y": [...]  (a JSON object)

This unit is engine-free on purpose (plain arrays + fpjson) so it can be tested
in isolation; the callers convert to/from TPointsSet.
}
unit fit_points_json;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, Classes, fpjson, jsonparser;

type
    TPointsData = record
        Title: string;
        X, Y:  array of double;
        { THE HANDLE EACH POINT CARRIES, parallel to X and Y exactly as Y is
          parallel to X - and OPTIONAL, so every message that has no handles is
          byte-identical to what this unit sent before the field existed.

          Only ONE set has handles: the curve positions. A curve's identity is
          issued to the pick it is seeded from (curve_instance_id,
          curve_identity_registry), so a pick can be named and a profile sample
          cannot. That is enforced by the caller, which refuses ids BY NAME on
          any other set rather than ignoring them - being ignored is how a
          client comes to believe it restored something it did not.

          Text, not a GUID: this unit is engine-free and stays that way, and the
          wire form of a handle is text on every other route already. }
        Ids:   array of string;
    end;

function PointsToJson(const P: TPointsData): TJSONObject;
function PointsFromJson(O: TJSONObject; out P: TPointsData): boolean;
{ Convenience: whole-message encode/decode. }
function PointsToJsonString(const P: TPointsData): string;
function PointsFromJsonString(const S: string; out P: TPointsData): boolean;

implementation

function PointsToJson(const P: TPointsData): TJSONObject;
var
    AX, AY, AIds: TJSONArray;
    i: integer;
begin
    Result := TJSONObject.Create;
    Result.Add('title', P.Title);
    AX := TJSONArray.Create;
    AY := TJSONArray.Create;
    for i := 0 to High(P.X) do
        AX.Add(P.X[i]);
    for i := 0 to High(P.Y) do
        AY.Add(P.Y[i]);
    Result.Add('x', AX);
    Result.Add('y', AY);
    //  ABSENT WHEN THERE ARE NONE, rather than an empty array. A profile of
    //  100k points crosses this codec on every load, and an empty array per
    //  message would be a cost paid by every set to describe the one that has
    //  handles. It also keeps "this sender knows nothing about handles" and
    //  "this set has none" the same message, which is what makes the field
    //  additive.
    if Length(P.Ids) > 0 then
    begin
        AIds := TJSONArray.Create;
        for i := 0 to High(P.Ids) do
            AIds.Add(P.Ids[i]);
        Result.Add('ids', AIds);
    end;
end;

function ReadArray(O: TJSONObject; const Name: string): TJSONArray;
var
    D: TJSONData;
begin
    Result := nil;
    D := O.Find(Name);
    if D is TJSONArray then
        Result := TJSONArray(D);
end;

function PointsFromJson(O: TJSONObject; out P: TPointsData): boolean;
var
    AX, AY: TJSONArray;
    AIds: TJSONData;
    i, n: integer;
begin
    P := Default(TPointsData);
    Result := False;
    if not Assigned(O) then
        Exit;
    P.Title := O.Get('title', '');
    AX := ReadArray(O, 'x');
    AY := ReadArray(O, 'y');
    if (AX = nil) or (AY = nil) then
        Exit;
    //  A point needs both coordinates; ignore any ragged tail.
    n := AX.Count;
    if AY.Count < n then
        n := AY.Count;
    SetLength(P.X, n);
    SetLength(P.Y, n);
    for i := 0 to n - 1 do
    begin
        P.X[i] := AX.Items[i].AsFloat;
        P.Y[i] := AY.Items[i].AsFloat;
    end;

    //  THE HANDLES, and the one place this codec REFUSES where it elsewhere
    //  truncates. A ragged x/y tail drops a point nobody could place; a ragged
    //  ids array would keep every point and hand the handles to the WRONG ones,
    //  because nothing here can know whether the missing entry was meant to be
    //  the first pick or the last. An off-by-one there is not a decode failure
    //  at either end - it is a fit that silently resumes another curve's shape.
    //  So the count must match the points that SURVIVED the truncation above.
    AIds := O.Find('ids');
    if Assigned(AIds) then
    begin
        if not (AIds is TJSONArray) then
            Exit;
        if TJSONArray(AIds).Count <> n then
            Exit;
        SetLength(P.Ids, n);
        for i := 0 to n - 1 do
        begin
            //  A handle is opaque TEXT. A number here is the mistake the `kind`
            //  field exists to prevent on the curves route - a GUID written as a
            //  JSON number arrives as 0 - so it is refused, not coerced.
            if TJSONArray(AIds).Items[i].JSONType <> jtString then
            begin
                SetLength(P.Ids, 0);
                Exit;
            end;
            P.Ids[i] := TJSONArray(AIds).Items[i].AsString;
        end;
    end;

    Result := True;
end;

function PointsToJsonString(const P: TPointsData): string;
var
    O: TJSONObject;
begin
    O := PointsToJson(P);
    try
        Result := O.AsJSON;
    finally
        O.Free;
    end;
end;

function PointsFromJsonString(const S: string; out P: TPointsData): boolean;
var
    D: TJSONData;
begin
    P := Default(TPointsData);
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
        Result := PointsFromJson(TJSONObject(D), P);
    finally
        D.Free;
    end;
end;

end.
