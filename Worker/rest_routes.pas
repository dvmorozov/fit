// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Which route a request names.)

WHAT THIS TAKES OUT OF THE ROUTER. TFitRestApi.HandleRoute is the largest routine
in the counted half of this program - four hundred and fifty-five lines - and it
answers two quite different questions in one chain: WHICH ROUTE IS THIS, and WHAT
DOES THAT ROUTE DO. The first is a function of the method and the path and
nothing else. The second needs a session, a service, an engine and sometimes a
Python sidecar.

Keeping them together meant the first could only be asked through the second.
"Does GET /problems/1/curves/7/points name a route?" required standing up a
problem, and "is PUT accepted here as well as POST?" could only be answered by
sending one. Twenty-one route shapes, each spelled out as a conjunction of a
method, a segment count and one or two segment names, and no way to see the table
as a table.

THE SHAPES ARE PAIRWISE DISJOINT, which is why this can be a flat classifier
rather than an ordered chain. Nothing here depends on being asked in a particular
order, and the tests below assert that: every route is recognised, and no path is
recognised as two things.

WHAT IS DELIBERATELY NOT HERE. The 404s that guard the SESSION LOOKUP stay in the
router, and so does their order. A request naming a problem that does not exist
is answered "no such problem" even when the rest of its path is nonsense, because
the id is checked before the leaf; classifying first and refusing unknown routes
first would change that answer. It is a small thing, and it is a thing a caller
may already rely on.

}
unit rest_routes;

{$MODE Delphi}

interface

type
    { Every route this server answers. The comment on each is the shape it
      matches; the router holds what it does. }
    TRestRoute = (
        { Nothing this server answers. }
        rtUnknown,
        { GET /health }
        rtHealth,
        { POST /problems }
        rtCreateProblem,
        { DELETE /problems/<id> }
        rtDiscardProblem,
        { GET /problems/<id>/state }
        rtState,
        { PUT /problems/<id>/<profile|background|positions|rfactor-bounds> }
        rtPutPointsSet,
        { GET /problems/<id>/<profile|calc-profile|delta-profile|background|
          positions|calc-positions|rfactor-bounds|rfactor> }
        rtGetPointsSet,
        { GET /problems/<id>/settings }
        rtGetSettings,
        { PUT /problems/<id>/settings }
        rtPutSettings,
        { GET /problems/<id>/async }
        rtAsync,
        { GET /problems/<id>/stats }
        rtStats,
        { GET /problems/<id>/selected-interval }
        rtSelectedInterval,
        { GET /problems/<id>/curves }
        rtCurves,
        { PUT /problems/<id>/curves - the whole model's fitted values in one
          request, the write side of the GET above and the same body shape.

          Disjoint from rtCurves by METHOD, so nothing about the path changes
          and the router's "no path is recognised as two routes" check covers
          it without a new case. One request rather than one per parameter
          because each PUT of a parameter rebuilds the whole model - and
          because only a whole-model write can say that an OPTIMISER produced
          the values, which cannot be derived from the values themselves. }
        rtPutCurves,
        { GET /problems/<id>/special-params }
        rtGetSpecialParams,
        { PUT /problems/<id>/special-params }
        rtPutSpecialParams,
        { DELETE /problems/<id>/special-params }
        rtDeleteSpecialParams,
        { GET /problems/<id>/curves/<cid>/points }
        rtCurvePoints,
        { PUT /problems/<id>/curves/<cid>/params/<j> }
        rtCurveParam,
        { POST /problems/<id>/points/<set> }
        rtAddPoint,
        { PUT /problems/<id>/points/<set> }
        rtMovePoint,
        { DELETE /problems/<id>/points/<set>/<pid> - ONE MEMBER, by the handle
          that names it. What /points/<set> always implied and could not
          express, because a point used to be only a coordinate pair, so the
          only way to remove one was to post the same coordinates again and let
          AddPoint's toggle do it.

          DELETE alone for now. Reading the members with their handles and
          moving one by handle are the same idea and want the same shape, and
          both need the identity registry exposed through IFitService, which it
          is not. The shape is left free rather than half-filled. }
        rtDeletePoint,
        { GET /problems/<id>/module-states - every module's project-state
          document in one answer.

          NOT A BYPASS OF rtModule, on the same reading as GET /curves beside
          GET /curves/<cid>/points: that route addresses ONE module's resource,
          this one is the framework collecting across modules for the document.
          A client cannot do it itself, because the modules that matter are the
          SERVER's and it may not have the same ones linked. }
        rtModuleStates,
        { GET | POST | PUT /problems/<id>/modules/<vendor>/<resource...> }
        rtModule,
        { POST /problems/<id>/actions/<name> }
        rtAction);

{ The route AMethod and APath name, or rtUnknown.

  Accepts a bare path; a query string and any trailing slash are ignored, and
  empty segments are dropped, so '/problems//12/state/' names the same route as
  '/problems/12/state'. }
function RouteOf(const AMethod, APath: string): TRestRoute;

implementation

uses
    SysUtils, Classes, Types;

{ Splits '/problems/12/profile' into ['problems','12','profile'], dropping
  empty segments and anything after a '?'. }
function Segments(const APath: string): TStringArray;
var
    Parts: TStringList;
    i, q: integer;
    P, S: string;
begin
    P := APath;
    q := Pos('?', P);
    if q > 0 then
        P := Copy(P, 1, q - 1);
    Parts := TStringList.Create;
    try
        Parts.Delimiter := '/';
        Parts.StrictDelimiter := True;
        Parts.DelimitedText := P;
        SetLength(Result, 0);
        for i := 0 to Parts.Count - 1 do
        begin
            S := Trim(Parts[i]);
            if S <> '' then
            begin
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := S;
            end;
        end;
    finally
        Parts.Free;
    end;
end;

{ One of the four sets a PUT may replace wholesale. }
function IsWritablePointsSet(const AName: string): boolean;
begin
    Result := (AName = 'profile') or (AName = 'background') or
        (AName = 'positions') or (AName = 'rfactor-bounds');
end;

{ One of the eight a GET may read. Four more than the writable set: the computed
  ones are answers rather than inputs, so they can be read and not replaced. }
function IsReadablePointsSet(const AName: string): boolean;
begin
    Result := IsWritablePointsSet(AName) or
        (AName = 'calc-profile') or (AName = 'delta-profile') or
        (AName = 'calc-positions') or (AName = 'rfactor');
end;

function RouteOf(const AMethod, APath: string): TRestRoute;
var
    Seg: TStringArray;
    N: integer;
    Leaf: string;
begin
    Result := rtUnknown;
    Seg := Segments(APath);
    N := Length(Seg);
    if N = 0 then
        Exit;

    if (AMethod = 'GET') and (N = 1) and (Seg[0] = 'health') then
        Exit(rtHealth);

    //  Everything else this server answers hangs off /problems.
    if Seg[0] <> 'problems' then
        Exit;

    if (AMethod = 'POST') and (N = 1) then
        Exit(rtCreateProblem);
    if N < 2 then
        Exit;

    if (AMethod = 'DELETE') and (N = 2) then
        Exit(rtDiscardProblem);
    if N < 3 then
        Exit;

    Leaf := Seg[2];

    //  N >= 4, and the only route whose path may be longer than its shape: a
    //  module's resource name can carry slashes, so everything past 'modules'
    //  is the resource rather than more route.
    if (N >= 4) and (Leaf = 'modules') and
        ((AMethod = 'GET') or (AMethod = 'POST') or (AMethod = 'PUT')) then
        Exit(rtModule);

    if N = 3 then
    begin
        if AMethod = 'GET' then
        begin
            if Leaf = 'state' then Exit(rtState);
            if Leaf = 'settings' then Exit(rtGetSettings);
            if Leaf = 'async' then Exit(rtAsync);
            if Leaf = 'stats' then Exit(rtStats);
            if Leaf = 'selected-interval' then Exit(rtSelectedInterval);
            if Leaf = 'curves' then Exit(rtCurves);
            if Leaf = 'module-states' then Exit(rtModuleStates);
            if Leaf = 'special-params' then Exit(rtGetSpecialParams);
            if IsReadablePointsSet(Leaf) then Exit(rtGetPointsSet);
        end
        else if AMethod = 'PUT' then
        begin
            if Leaf = 'settings' then Exit(rtPutSettings);
            if Leaf = 'special-params' then Exit(rtPutSpecialParams);
            if Leaf = 'curves' then Exit(rtPutCurves);
            if IsWritablePointsSet(Leaf) then Exit(rtPutPointsSet);
        end
        else if AMethod = 'DELETE' then
        begin
            if Leaf = 'special-params' then Exit(rtDeleteSpecialParams);
        end;
        Exit;
    end;

    if N = 4 then
    begin
        if (AMethod = 'POST') and (Leaf = 'points') then Exit(rtAddPoint);
        if (AMethod = 'PUT') and (Leaf = 'points') then Exit(rtMovePoint);
        if (AMethod = 'POST') and (Leaf = 'actions') then Exit(rtAction);
        Exit;
    end;

    if (AMethod = 'GET') and (N = 5) and (Leaf = 'curves') and
        (Seg[4] = 'points') then
        Exit(rtCurvePoints);

    //  ONE MEMBER OF A POINT SET. Disjoint from the route above at the same
    //  depth: that one's Leaf is 'curves' and this one's is 'points', so the
    //  classifier stays flat.
    if (AMethod = 'DELETE') and (N = 5) and (Leaf = 'points') then
        Exit(rtDeletePoint);

    if (AMethod = 'PUT') and (N = 6) and (Leaf = 'curves') and
        (Seg[4] = 'params') then
        Exit(rtCurveParam);
end;

end.
