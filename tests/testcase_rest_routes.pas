// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which route a request names.)

TWENTY-ONE ROUTE SHAPES that were a conjunction of a method, a segment count and
one or two segment names, spelled out inline in the largest routine in the
counted half of this program. Asking "does this path name a route?" meant
standing up a problem and sending a request; asking "is PUT accepted here as well
as POST?" meant sending one of each.

WHAT THESE TESTS ARE FOR. Not the handlers - those need a session, an engine and
sometimes a Python sidecar, and they have their own tests through the router.
What is checked here is the TABLE: that every route is recognised by its own
path, that nothing else is, and that the near-misses are refused rather than
mistaken for a neighbour.

THE NEAR-MISSES ARE THE POINT. A route recognised one segment too loosely
answers a request nobody made; one recognised too strictly returns "unknown
endpoint" for a call the client is entitled to make, which reads as the feature
not existing. Both are silent from the server's side, and both are one character
away from correct in a chain of twenty-one conjunctions.

AND THE TWO SETS DIFFER BY FOUR NAMES. A PUT may replace the four point sets the
user supplies; a GET may read those and the four the engine computes. Mixing
them up would offer to overwrite a computed answer, or refuse to read one.
}
unit testcase_rest_routes;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, rest_routes;

type
    TRestRoutesTest = class(TTestCase)
    private
        procedure CheckRoute(const AMethod, APath: string;
            AExpected: TRestRoute);
        { Every route reachable by some request, for the walks below. }
        function EveryRoute: TStringList;
    published
        //  Each route by its own path.
        procedure HealthIsItsOwnRoute;
        procedure CreatingAndDiscardingAProblem;
        procedure TheStateRoute;
        procedure TheSettingsRoutes;
        procedure ThePollingRoutes;
        procedure TheModelReadingRoutes;
        procedure TheSpecialParameterRoutes;
        procedure TheCurveRoutes;
        procedure ThePointRoutes;
        procedure OneMemberOfAPointSetIsItsOwnRoute;
        procedure AMemberRouteIsNotACurvesRoute;
        procedure TheActionRoute;

        //  The two point-set vocabularies.
        procedure EveryUserSuppliedSetCanBeReplaced;
        procedure AndEveryComputedOneCanOnlyBeRead;

        //  Modules.
        procedure AModuleResourceIsReadableAndWritable;
        procedure AModuleResourceMayCarrySlashes;
        procedure AModuleRouteRefusesOtherMethods;

        //  What is not a route.
        procedure AnEmptyPathIsNothing;
        procedure APathOutsideProblemsIsNothing;
        procedure AKnownLeafWithTheWrongMethodIsNothing;
        procedure AKnownLeafAtTheWrongDepthIsNothing;
        procedure AnUnknownLeafIsNothing;
        procedure TheCurvePointsRouteNeedsItsLastSegment;

        //  Shapes of the same request.
        procedure ATrailingSlashChangesNothing;
        procedure AnEmptySegmentChangesNothing;
        procedure AQueryStringChangesNothing;

        //  Which routes address a problem.
    end;

implementation

const
    { The four a PUT may replace: what the user supplies. }
    Writable: array[0..3] of string = (
        'profile', 'background', 'positions', 'rfactor-bounds');
    { The four more a GET may read: what the engine computes. }
    ComputedOnly: array[0..3] of string = (
        'calc-profile', 'delta-profile', 'calc-positions', 'rfactor');

procedure TRestRoutesTest.CheckRoute(const AMethod, APath: string;
    AExpected: TRestRoute);
begin
    AssertEquals(AMethod + ' ' + APath, Ord(AExpected),
        Ord(RouteOf(AMethod, APath)));
end;

function TRestRoutesTest.EveryRoute: TStringList;
begin
    //  Only used for its Count in the walks below; the routes themselves are
    //  named individually so a rename is a compile error rather than a silent
    //  gap.
    Result := TStringList.Create;
end;

{ ------------------------- each route by its own path ----------------------- }

procedure TRestRoutesTest.HealthIsItsOwnRoute;
begin
    //  The one route that is not under /problems, because it answers before
    //  there is anything to address.
    CheckRoute('GET', '/health', rtHealth);
end;

procedure TRestRoutesTest.CreatingAndDiscardingAProblem;
begin
    CheckRoute('POST', '/problems', rtCreateProblem);
    CheckRoute('DELETE', '/problems/12', rtDiscardProblem);
end;

procedure TRestRoutesTest.TheStateRoute;
begin
    CheckRoute('GET', '/problems/12/state', rtState);
end;

procedure TRestRoutesTest.TheSettingsRoutes;
begin
    //  Read and written at one path by two methods, which is the shape most
    //  likely to be collapsed into one route by mistake.
    CheckRoute('GET', '/problems/12/settings', rtGetSettings);
    CheckRoute('PUT', '/problems/12/settings', rtPutSettings);
end;

procedure TRestRoutesTest.ThePollingRoutes;
begin
    //  The three the client asks for twice a second for as long as it is open.
    CheckRoute('GET', '/problems/12/state', rtState);
    CheckRoute('GET', '/problems/12/async', rtAsync);
    CheckRoute('GET', '/problems/12/rfactor', rtGetPointsSet);
end;

procedure TRestRoutesTest.TheModelReadingRoutes;
begin
    CheckRoute('GET', '/problems/12/stats', rtStats);
    CheckRoute('GET', '/problems/12/selected-interval', rtSelectedInterval);
    CheckRoute('GET', '/problems/12/curves', rtCurves);
end;

procedure TRestRoutesTest.TheSpecialParameterRoutes;
begin
    //  Three methods at one path, and the only route trio in the table.
    CheckRoute('GET', '/problems/12/special-params', rtGetSpecialParams);
    CheckRoute('PUT', '/problems/12/special-params', rtPutSpecialParams);
    CheckRoute('DELETE', '/problems/12/special-params',
        rtDeleteSpecialParams);
end;

procedure TRestRoutesTest.TheCurveRoutes;
begin
    //  The deepest two shapes in the table, at five and six segments.
    CheckRoute('GET', '/problems/12/curves/7/points', rtCurvePoints);
    CheckRoute('PUT', '/problems/12/curves/7/params/3', rtCurveParam);
    //  READING AND WRITING THE WHOLE MODEL ARE ONE PATH AND TWO ROUTES,
    //  separated by method alone - which is the shape most easily broken by a
    //  later edit to the table, and the one a restore depends on: the write is
    //  how a project's fitted values reach the model in a single rebuild.
    CheckRoute('GET', '/problems/12/curves', rtCurves);
    CheckRoute('PUT', '/problems/12/curves', rtPutCurves);
end;

procedure TRestRoutesTest.ThePointRoutes;
begin
    //  Same path, two methods, two quite different operations: appending a
    //  point and moving one.
    CheckRoute('POST', '/problems/12/points/background', rtAddPoint);
    CheckRoute('PUT', '/problems/12/points/background', rtMovePoint);
end;

procedure TRestRoutesTest.OneMemberOfAPointSetIsItsOwnRoute;
begin
    //  THE MEMBER ADDRESS the set always implied. Until a pick carried a
    //  handle there was nothing to put in the last segment, so the only way to
    //  remove one point was to post its coordinates again and let AddPoint's
    //  toggle do it.
    CheckRoute('DELETE', '/problems/12/points/positions/A1B2C3',
        rtDeletePoint);
end;

procedure TRestRoutesTest.AMemberRouteIsNotACurvesRoute;
begin
    //  Both are five segments. What tells them apart is the third: 'points'
    //  addresses a member of a set, 'curves' addresses a curve. Recognising one
    //  as the other would delete a curve when a point was asked for.
    CheckRoute('GET', '/problems/12/curves/7/points', rtCurvePoints);
    CheckRoute('DELETE', '/problems/12/points/positions/7', rtDeletePoint);
    //  And neither answers the other's method.
    CheckRoute('DELETE', '/problems/12/curves/7/points', rtUnknown);
    CheckRoute('GET', '/problems/12/points/positions/7', rtUnknown);
end;

procedure TRestRoutesTest.TheActionRoute;
begin
    CheckRoute('POST', '/problems/12/actions/fit', rtAction);
end;

{ ------------------------ the two point-set vocabularies -------------------- }

procedure TRestRoutesTest.EveryUserSuppliedSetCanBeReplaced;
var
    i: longint;
begin
    for i := 0 to High(Writable) do
    begin
        CheckRoute('PUT', '/problems/12/' + Writable[i], rtPutPointsSet);
        CheckRoute('GET', '/problems/12/' + Writable[i], rtGetPointsSet);
    end;
end;

procedure TRestRoutesTest.AndEveryComputedOneCanOnlyBeRead;
var
    i: longint;
begin
    //  THE FOUR THAT DIFFER. Offering to replace a computed answer would let a
    //  caller overwrite the engine's own output with whatever it liked, and the
    //  next fit would silently start from it.
    for i := 0 to High(ComputedOnly) do
    begin
        CheckRoute('GET', '/problems/12/' + ComputedOnly[i], rtGetPointsSet);
        CheckRoute('PUT', '/problems/12/' + ComputedOnly[i], rtUnknown);
    end;
end;

{ ---------------------------------- modules --------------------------------- }

procedure TRestRoutesTest.AModuleResourceIsReadableAndWritable;
begin
    //  PUT is accepted alongside POST because replacing a resource wholesale is
    //  what PUT means, and the bulk verb this route replaced was a PUT.
    CheckRoute('GET', '/problems/12/modules/vendor/thing', rtModule);
    CheckRoute('POST', '/problems/12/modules/vendor/thing', rtModule);
    CheckRoute('PUT', '/problems/12/modules/vendor/thing', rtModule);
end;

procedure TRestRoutesTest.AModuleResourceMayCarrySlashes;
begin
    //  THE ONLY ROUTE WHOSE PATH MAY BE LONGER THAN ITS SHAPE. A module names
    //  its resources with a prefix, so everything past 'modules' is the
    //  resource rather than more route - and a classifier that fixed the depth
    //  here would refuse every module resource this program actually uses.
    CheckRoute('GET', '/problems/12/modules/vendor/a/b/c', rtModule);
    //  And a single-segment resource is a resource: four segments is the
    //  shortest module path, so 'modules/vendor' names the resource 'vendor'.
    CheckRoute('POST', '/problems/12/modules/vendor', rtModule);
    //  Three is one too few - there is no resource named at all.
    CheckRoute('POST', '/problems/12/modules', rtUnknown);
end;

procedure TRestRoutesTest.AModuleRouteRefusesOtherMethods;
begin
    CheckRoute('DELETE', '/problems/12/modules/vendor/thing', rtUnknown);
end;

{ ------------------------------ not a route --------------------------------- }

procedure TRestRoutesTest.AnEmptyPathIsNothing;
begin
    CheckRoute('GET', '', rtUnknown);
    CheckRoute('GET', '/', rtUnknown);
end;

procedure TRestRoutesTest.APathOutsideProblemsIsNothing;
begin
    //  Everything but /health hangs off /problems, so a plausible-looking path
    //  elsewhere must not be mistaken for one of these.
    CheckRoute('GET', '/state', rtUnknown);
    CheckRoute('GET', '/fit/12/state', rtUnknown);
    CheckRoute('POST', '/problem', rtUnknown);
end;

procedure TRestRoutesTest.AKnownLeafWithTheWrongMethodIsNothing;
begin
    //  A leaf that exists does not make the request legal. Recognising it
    //  anyway would run a read as a write or the reverse.
    CheckRoute('DELETE', '/problems/12/state', rtUnknown);
    CheckRoute('POST', '/problems/12/settings', rtUnknown);
    //  PUT /curves USED TO BE HERE, and is now a route of its own - the write
    //  side of the GET, carrying the whole model's fitted values at once. It is
    //  still the case that the leaf alone decides nothing: POST of the same
    //  path is as meaningless as it ever was.
    CheckRoute('POST', '/problems/12/curves', rtUnknown);
    //  A SET IS NOT DELETED BY THE MEMBER ROUTE. Removing one member needs the
    //  member's own address, one segment deeper; without it there is nothing to
    //  say which point was meant.
    CheckRoute('DELETE', '/problems/12/points/background', rtUnknown);
end;

procedure TRestRoutesTest.AKnownLeafAtTheWrongDepthIsNothing;
begin
    //  The same names appear at several depths, and the depth is what tells
    //  'the curves collection' from 'one curve's points'.
    CheckRoute('GET', '/problems/12/curves/7', rtUnknown);
    CheckRoute('GET', '/problems/12/state/extra', rtUnknown);
    CheckRoute('POST', '/problems/12/points', rtUnknown);
    CheckRoute('POST', '/problems/12/actions', rtUnknown);
end;

procedure TRestRoutesTest.AnUnknownLeafIsNothing;
begin
    CheckRoute('GET', '/problems/12/nonsense', rtUnknown);
    CheckRoute('PUT', '/problems/12/nonsense', rtUnknown);
end;

procedure TRestRoutesTest.TheCurvePointsRouteNeedsItsLastSegment;
begin
    //  Five segments with the wrong last one is not this route; nor is the
    //  parameter route with the wrong fifth.
    CheckRoute('GET', '/problems/12/curves/7/nonsense', rtUnknown);
    CheckRoute('PUT', '/problems/12/curves/7/nonsense/3', rtUnknown);
end;

{ ------------------------- shapes of the same request ----------------------- }

procedure TRestRoutesTest.ATrailingSlashChangesNothing;
begin
    CheckRoute('GET', '/problems/12/state/', rtState);
end;

procedure TRestRoutesTest.AnEmptySegmentChangesNothing;
begin
    //  An empty segment is dropped rather than counted, or a doubled slash
    //  would shift every segment index by one and name a different route.
    CheckRoute('GET', '/problems//12/state', rtState);
end;

procedure TRestRoutesTest.AQueryStringChangesNothing;
begin
    CheckRoute('GET', '/problems/12/state?since=3', rtState);
end;

initialization
    //  Unit tests: one function of two strings. No session, no service, no
    //  server - which is the whole reason for taking the table out of one.
    RegisterTest('unit', TRestRoutesTest);
end.
