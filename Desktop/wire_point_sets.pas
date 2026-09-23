// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A point set and a curve, rebuilt from what the wire carries.)

TWO PATHS BUILD THE SAME OBJECTS, and must build them alike. The finished model
arrives curve by curve over GET /curves/<cid>/points (THttpFitService.GetCurves);
an animated frame arrives as a snapshot in one progress reply (TFitClient). A
frame and the result that follows it have to name each curve the same way - the
same type name, the same handle - or a highlight or a deletion loses track of the
curve between the two. So both call these, and neither writes its own copy.
}
unit wire_point_sets;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fit_points_json, title_points_set, named_points_set,
    curve_instance_id;

{ A point set holding P's title and points. The caller owns it. }
function TitlePointsSetOf(const P: TPointsData): TTitlePointsSet;
{ A curve holding P's points, typed by P's title and carrying the handle AId.
  An AId that is not a handle leaves the curve without one: whether a handle is
  REQUIRED is the caller's rule. The caller owns the result. }
function NamedCurveOf(const AId: string; const P: TPointsData): TNamedPointsSet;

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
end;

end.
