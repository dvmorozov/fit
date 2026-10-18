// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A point set and a curve, rebuilt from what the wire carries - and
what the wire carries of one.)

TWO PATHS BUILD THE SAME OBJECTS, and must build them alike. The finished model
arrives curve by curve over GET /curves/<cid>/points (THttpFitService.GetCurves);
an animated frame arrives as a snapshot in one progress reply (TFitClient). A
frame and the result that follows it have to name each curve the same way - the
same type name, the same handle - or a highlight or a deletion loses track of the
curve between the two. So both call these, and neither writes its own copy.

AND THE SERVER SENDS THROUGH ONE, PointsDataOf, for the same reason in the other
direction: the REST layer and the progress snapshot each had a copy of it, and a
field added to one - the drawn baseline - would have reached the finished model
and not the frames before it.
}
unit wire_point_sets;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_points_json, points_set, title_points_set, named_points_set,
    curve_instance_id;

{ A point set holding P's title and points. The caller owns it. }
function TitlePointsSetOf(const P: TPointsData): TTitlePointsSet;
{ A curve holding P's points, typed by P's title and carrying the handle AId.
  An AId that is not a handle leaves the curve without one: whether a handle is
  REQUIRED is the caller's rule. The caller owns the result. }
function NamedCurveOf(const AId: string; const P: TPointsData): TNamedPointsSet;
{ What the wire carries of APoints, titled ATitle. }
function PointsDataOf(APoints: TPointsSet; const ATitle: string): TPointsData;

implementation

function TitlePointsSetOf(const P: TPointsData): TTitlePointsSet;
var
    i: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    Result.FTitle := P.Title;
    for i := 0 to High(P.X) do
        Result.AddNewPoint(P.X[i], P.Y[i]);
end;

function NamedCurveOf(const AId: string; const P: TPointsData): TNamedPointsSet;
var
    i: longint;
begin
    Result := TNamedPointsSet.Create(nil);
    //  Carried, so the view can tell one instance from another and address it
    //  back. The points alone cannot: two curves of one type differ only in
    //  where they sit.
    TryStrToCurveInstanceId(AId, Result.FInstanceId);
    Result.SetCurveTypeName(P.Title);
    Result.FTitle := P.Title;
    for i := 0 to High(P.X) do
        Result.AddNewPoint(P.X[i], P.Y[i]);
    //  What it is drawn on, for the chart; the codec has already refused one
    //  out of step with the points.
    Result.FDrawnBaseline := Copy(P.Baseline);
end;

function PointsDataOf(APoints: TPointsSet; const ATitle: string): TPointsData;
var
    i: longint;
    S: TTitlePointsSet;
begin
    Result := Default(TPointsData);
    Result.Title := ATitle;
    if not Assigned(APoints) then
        Exit;
    SetLength(Result.X, APoints.PointsCount);
    SetLength(Result.Y, APoints.PointsCount);
    for i := 0 to APoints.PointsCount - 1 do
    begin
        Result.X[i] := APoints.PointXCoord[i];
        Result.Y[i] := APoints.PointYCoord[i];
    end;
    //  Only in step: the reader refuses a ragged baseline, and refusing loses
    //  the whole curve where leaving it out draws the curve on zero.
    if APoints is TTitlePointsSet then
    begin
        S := TTitlePointsSet(APoints);
        if (Length(S.FDrawnBaseline) > 0) and
           (Length(S.FDrawnBaseline) = S.PointsCount) then
            Result.Baseline := Copy(S.FDrawnBaseline);
    end;
end;

end.
