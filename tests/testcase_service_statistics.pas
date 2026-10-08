// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The goodness-of-fit statistics computed off a live service.)

WHAT IS BEING TESTED HERE, and what is not. fit_statistics holds the formulas and
has its own tests. fit_service_statistics is the bridge: it reads the two profiles
and the R-factor bounds through IFitService, decides which points are inside the
fitting window, counts the varying parameters, and refuses with a stated reason
when any of it is missing. Every one of those is a decision, and none of them was
covered - the unit measured 0 of 54 lines, reachable only by running a fit to
convergence against a real server.

The service is a THttpFitService whose replies come from a table (see
tests/mocks/mock_http_transport), so nothing here opens a socket. That the bridge
reads its inputs through the same wire the desktop uses is part of what is
asserted: a route renamed on one side shows up here.

WHY THE REFUSALS MATTER MORE THAN THE NUMBERS. An invalid statistics record is
indistinguishable from a bad fit at the call site, and the candidate ranking
depends on it - when the record comes back invalid every count shows as "not
scored" and the complexity penalty quietly does nothing (D26). So each
precondition gets its own test.
}
unit testcase_service_statistics;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    mock_http_transport, fit_service_statistics, fit_statistics,
    int_fit_service;

type
    TServiceStatisticsTest = class(TTestCase)
    private
        FSvc: TMockHttpService;
        { Replies for a fit of AN points over one bound pair covering all of
          them, with ACurves curves of AParams parameters each. }
        procedure StubAFit;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  When it refuses, and whether the reason is distinguishable.
        procedure WithNoDataThereAreNoStatistics;
        procedure WithNothingComputedThereAreNoStatistics;
        procedure WithNoBoundsThereAreNoStatistics;
        procedure ASingleBoundPointIsNotAnInterval;
        procedure ProfilesOfDifferentLengthsAreRefused;

        //  When it computes.
        procedure AFittedProfileHasStatistics;
        procedure OnlyThePointsInsideTheWindowAreCounted;
        procedure PointsOutsideEveryIntervalAreExcluded;
        procedure SeveralIntervalsAreAllIncluded;
        procedure BoundsAreReadAsConsecutivePairs;
        procedure AnIntervalIncludesItsEndpoints;

        //  What the parameter count is taken from.
        procedure TheParameterCountComesFromEveryCurve;
        procedure WithNoCurvesTheDegreesOfFreedomAreEveryPoint;

        //  The perfect and the hopeless fit, where the numbers are known.
        procedure AnExactFitHasNoResidual;
        procedure MoreParametersLeaveFewerDegreesOfFreedom;
    end;

implementation

const
    BASE = 'http://compute.example:8080';

procedure TServiceStatisticsTest.SetUp;
begin
    FSvc := TMockHttpService.Create(BASE);
end;

procedure TServiceStatisticsTest.TearDown;
begin
    FreeAndNil(FSvc);
end;

procedure TServiceStatisticsTest.StubAFit;
begin
    //  Six points, a calculated profile that misses each by one, and a single
    //  interval covering the lot. Distinct y values so a swap of data and fit
    //  would change the answer.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3,4,5,6],"y":[10,20,30,40,50,60]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3,4,5,6],"y":[11,21,31,41,51,61]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,6],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
end;

{ ---- when it refuses ------------------------------------------------------- }

procedure TServiceStatisticsTest.WithNoDataThereAreNoStatistics;
var
    S: TFitStatistics;
begin
    //  Nothing loaded at all. The reply carries no points, so the service
    //  answers nil and the bridge must not dereference it.
    FSvc.Reply('profile', '{"ok":true}');
    FSvc.Reply('calc-profile', '{"title":"c","x":[1],"y":[1]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertFalse('no statistics', S.Valid);
end;

procedure TServiceStatisticsTest.WithNothingComputedThereAreNoStatistics;
var
    S: TFitStatistics;
begin
    //  Data loaded, no fit run. This is the ordinary state of a freshly opened
    //  file, so it must be a refusal and not a fault.
    FSvc.Reply('profile', '{"title":"e","x":[1,2],"y":[1,2]}');
    FSvc.Reply('calc-profile', '{"ok":true}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,2],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertFalse('no statistics', S.Valid);
end;

procedure TServiceStatisticsTest.WithNoBoundsThereAreNoStatistics;
var
    S: TFitStatistics;
begin
    //  No fitting window. Computing over the whole profile instead would let the
    //  zero tails of the calculated profile swamp the residuals and report a
    //  hopeless fit as a bad one.
    FSvc.Reply('profile', '{"title":"e","x":[1,2],"y":[1,2]}');
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2],"y":[1,2]}');
    FSvc.Reply('rfactor-bounds', '{"ok":true}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertFalse('no statistics', S.Valid);
end;

procedure TServiceStatisticsTest.ASingleBoundPointIsNotAnInterval;
var
    S: TFitStatistics;
begin
    //  Bounds arrive as consecutive (start, end) pairs, so an odd count means a
    //  half-picked interval. Reading it as an interval would take the pair from
    //  past the end of the set.
    FSvc.Reply('profile', '{"title":"e","x":[1,2],"y":[1,2]}');
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2],"y":[1,2]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1],"y":[0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertFalse('no statistics', S.Valid);
end;

procedure TServiceStatisticsTest.ProfilesOfDifferentLengthsAreRefused;
var
    S: TFitStatistics;
begin
    //  The two profiles are sampled at the same points by construction, so
    //  different lengths mean one of them is stale. Pairing them anyway would
    //  compute residuals between unrelated abscissae - a plausible-looking
    //  number that means nothing.
    FSvc.Reply('profile', '{"title":"e","x":[1,2,3],"y":[1,2,3]}');
    FSvc.Reply('calc-profile', '{"title":"c","x":[1,2],"y":[1,2]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,3],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertFalse('no statistics', S.Valid);
end;

{ ---- when it computes ------------------------------------------------------ }

procedure TServiceStatisticsTest.AFittedProfileHasStatistics;
var
    S: TFitStatistics;
begin
    StubAFit;
    S := ServiceStatistics(FSvc);
    AssertTrue('statistics were computed', S.Valid);
    AssertEquals('every point was in the window', 6, S.DataPoints);
    AssertTrue('and there is a residual', S.ChiSquare > 0);
end;

procedure TServiceStatisticsTest.OnlyThePointsInsideTheWindowAreCounted;
var
    S: TFitStatistics;
begin
    //  THE WHOLE POINT of the bounds. The window is the region the engine's own
    //  R-factor is taken over, and a statistic taken over a wider one is not
    //  comparable with the R-factor shown beside it.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3,4,5,6],"y":[10,20,30,40,50,60]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3,4,5,6],"y":[11,21,31,41,51,61]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[2,4],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertTrue('computed', S.Valid);
    AssertEquals('three of the six points', 3, S.DataPoints);
end;

procedure TServiceStatisticsTest.PointsOutsideEveryIntervalAreExcluded;
var
    S: TFitStatistics;
begin
    //  An interval covering nothing in the data. Two points are the minimum the
    //  formulas need, so this comes back invalid rather than as a division by a
    //  zero count.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3],"y":[10,20,30]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3],"y":[11,21,31]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[100,200],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertFalse('nothing was in the window, so nothing was computed', S.Valid);
end;

procedure TServiceStatisticsTest.SeveralIntervalsAreAllIncluded;
var
    S: TFitStatistics;
begin
    //  Two disjoint windows - which is what picking two curve regions gives -
    //  and both contribute. Taking only the first is the mistake this catches,
    //  and it would report a statistic over half the fitted data.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3,4,5,6],"y":[10,20,30,40,50,60]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3,4,5,6],"y":[11,21,31,41,51,61]}');
    FSvc.Reply('rfactor-bounds',
        '{"title":"r","x":[1,2,5,6],"y":[0,0,0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertTrue('computed', S.Valid);
    AssertEquals('two points from each interval', 4, S.DataPoints);
end;

procedure TServiceStatisticsTest.BoundsAreReadAsConsecutivePairs;
var
    S: TFitStatistics;
begin
    //  PAIRS, not a run of alternating edges: (1,2) and (5,6) are intervals and
    //  (2,5) is the gap between them. Reading every adjacent pair instead would
    //  include the gap and count all six points.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3,4,5,6],"y":[10,20,30,40,50,60]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3,4,5,6],"y":[11,21,31,41,51,61]}');
    FSvc.Reply('rfactor-bounds',
        '{"title":"r","x":[1,2,5,6],"y":[0,0,0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertEquals('the gap between the pairs is not a window', 4, S.DataPoints);
end;

procedure TServiceStatisticsTest.AnIntervalIncludesItsEndpoints;
var
    S: TFitStatistics;
begin
    //  Closed, not open. The user picked those two abscissae as the region to
    //  fit; excluding them would drop a point at each end of every interval.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3],"y":[10,20,30]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3],"y":[11,21,31]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,3],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertEquals('all three, endpoints included', 3, S.DataPoints);
end;

{ ---- the parameter count --------------------------------------------------- }

procedure TServiceStatisticsTest.TheParameterCountComesFromEveryCurve;
var
    S: TFitStatistics;
begin
    //  SUMMED ACROSS CURVES. The degrees of freedom, and so the reduced
    //  chi-squared and both information criteria, are wrong by the parameters of
    //  every curve after the first if this counts only one.
    StubAFit;
    FSvc.Reply('curves', '{"ok":true,"curves":[' +
        '{"params":[{"name":"A","value":1},{"name":"S","value":2}]},' +
        '{"params":[{"name":"A","value":3},{"name":"S","value":4},' +
        '{"name":"P","value":5}]}]}');
    S := ServiceStatistics(FSvc);
    AssertTrue('computed', S.Valid);
    AssertEquals('two curves, five parameters', 5, S.Params);
    AssertEquals('six points less five parameters', 1, S.DegreesOfFreedom);
end;

procedure TServiceStatisticsTest.WithNoCurvesTheDegreesOfFreedomAreEveryPoint;
var
    S: TFitStatistics;
begin
    StubAFit;
    S := ServiceStatistics(FSvc);
    AssertEquals('no parameters', 0, S.Params);
    AssertEquals('so every point is a degree of freedom',
        S.DataPoints, S.DegreesOfFreedom);
end;

{ ---- the two ends of the scale --------------------------------------------- }

procedure TServiceStatisticsTest.AnExactFitHasNoResidual;
var
    S: TFitStatistics;
begin
    //  The model reproduces the data exactly. Chi-squared is zero and R^2 is
    //  one; anything else here means the two profiles are not being paired
    //  point for point.
    FSvc.Reply('profile',
        '{"title":"e","x":[1,2,3,4],"y":[10,20,30,40]}');
    FSvc.Reply('calc-profile',
        '{"title":"c","x":[1,2,3,4],"y":[10,20,30,40]}');
    FSvc.Reply('rfactor-bounds', '{"title":"r","x":[1,4],"y":[0,0]}');
    FSvc.Reply('curves', '{"ok":true,"curves":[]}');
    S := ServiceStatistics(FSvc);
    AssertTrue('computed', S.Valid);
    AssertEquals('no residual', 0.0, S.ChiSquare, 1E-12);
    AssertEquals('and all the variance explained', 1.0, S.RSquared, 1E-12);
end;

procedure TServiceStatisticsTest.MoreParametersLeaveFewerDegreesOfFreedom;
var
    Few, Many: TFitStatistics;
begin
    //  The complexity penalty, which is the whole reason the parameter count is
    //  read at all: two models that fit equally well must not score equally.
    StubAFit;
    Few := ServiceStatistics(FSvc);
    FSvc.Reply('curves', '{"ok":true,"curves":[' +
        '{"params":[{"name":"A","value":1},{"name":"S","value":2},' +
        '{"name":"P","value":3}]}]}');
    Many := ServiceStatistics(FSvc);
    AssertTrue('both computed', Few.Valid and Many.Valid);
    AssertEquals('the same data', Few.DataPoints, Many.DataPoints);
    AssertTrue('but fewer degrees of freedom',
        Many.DegreesOfFreedom < Few.DegreesOfFreedom);
    AssertTrue('and a worse information criterion', Many.AIC > Few.AIC);
end;

initialization
    //  A unit test: the service is a real THttpFitService over a table of
    //  replies, so no socket is opened and no fit is run.
    RegisterTest('unit', TServiceStatisticsTest);
end.
