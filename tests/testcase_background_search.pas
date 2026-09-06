// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where the background is, given the data alone.)

WHAT THIS DECIDES. When the user has not marked background points, this proposes
them - and what it proposes is subtracted from every ordinate before a single
curve is fitted. A wrong background is not a wrong number in a corner: it is a
profile that is not the measurement, with curves fitted to it and an R-factor
computed against it.

HALF THE ALGORITHM HAD NEVER RUN. Eighty-two lines on the compute service,
touching none of its state, and the only test that reached them fed data whose
minimum sits at index 0 - so the entire LEFTWARD walk was dead, and with it the
concave-background assumption the whole thing rests on. Every profile it was
written for has its minimum in the interior.

THE ASSUMPTION IS THE ALGORITHM, not a caveat on it: start from the lowest point,
then walk outward both ways, each step taking the lowest point not below the one
last taken. On a bowl that traces the bowl. What it does on a shape that is NOT
a bowl is characterised here rather than promised anywhere - because it does
something, silently, and somebody will one day feed it a profile that slopes.

WHY THE PROPOSALS MUST BE THE DATA'S OWN POINTS. The caller looks each one up in
the profile by its x and asserts the lookup succeeded, so an interpolated point
would fail there - inside the service, after the proposal, as an assertion rather
than as a reasoned refusal. Pinned here where it can be read.
}
unit testcase_background_search;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    background_search, points_set;

type
    TBackgroundSearchTest = class(TTestCase)
    private
        FData: TPointsSet;
        FFound: TPointsSet;
        { Fills FData with the given ordinates at x = 0, 1, 2 ... }
        procedure Given(const AY: array of double);
        { Runs the search over FData into FFound. }
        procedure Search;
        { The ordinate FFound proposes at AX, or NaN when it proposes none. }
        function ProposedAt(const AX: double): double;
        { True when FFound proposes a point at AX. }
        function Proposes(const AX: double): boolean;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        //  What it always does, whatever the shape.
        procedure ItAlwaysProposesSomething;
        procedure TheFirstProposalIsTheLowestPointInTheData;
        procedure EveryProposalIsOneOfTheDataOwnPoints;
        procedure NoPointIsProposedTwice;
        procedure ItTerminatesOnAFlatProfile;

        //  A bowl, which is what it is written for.
        procedure OnABowlItTracesBothFlanks;
        procedure IncludingTheLeftFlankThatHadNeverRun;
        procedure AndItReachesBothEdges;
        procedure TheProposalsRiseAwayFromTheMinimum;
        procedure ItDoesNotProposeThePeakItself;

        //  The minimum at an edge, which is what the old test used.
        procedure WithTheMinimumAtTheLeftEdgeOnlyTheRightFlankIsWalked;
        procedure AndTheMirrorAtTheRightEdge;

        //  Outside its stated domain - characterised, not endorsed.
        procedure OnARisingProfileItProposesTheWholeThing;
        procedure OnATwoPeakedProfileItStopsAtTheFirstRise;

        //  Degenerate inputs.
        procedure OnePointIsItsOwnBackground;
        procedure TwoPointsGiveBoth;
        procedure AnAbsentDataSetIsRefused;
    end;

implementation

procedure TBackgroundSearchTest.SetUp;
begin
    FData := TPointsSet.Create(nil);
end;

procedure TBackgroundSearchTest.TearDown;
begin
    FreeAndNil(FFound);
    FreeAndNil(FData);
end;

procedure TBackgroundSearchTest.Given(const AY: array of double);
var
    i: longint;
begin
    for i := 0 to High(AY) do
        FData.AddNewPoint(i, AY[i]);
end;

procedure TBackgroundSearchTest.Search;
begin
    FreeAndNil(FFound);
    FFound := ProposeBackgroundPoints(FData);
end;

function TBackgroundSearchTest.ProposedAt(const AX: double): double;
var
    i: longint;
begin
    Result := NaN;
    for i := 0 to FFound.PointsCount - 1 do
        if Abs(FFound.PointXCoord[i] - AX) < 1e-9 then
            Exit(FFound.PointYCoord[i]);
end;

function TBackgroundSearchTest.Proposes(const AX: double): boolean;
begin
    Result := not IsNan(ProposedAt(AX));
end;

{ ------------------- what it always does, whatever the shape ---------------- }

procedure TBackgroundSearchTest.ItAlwaysProposesSomething;
begin
    //  THE CALLER HAS NO FALLBACK. It asserts the answer is assigned and then
    //  looks up its first point in the profile, so an empty proposal is an
    //  assertion failure inside the service rather than "no background found".
    Given([5, 3, 1, 3, 5]);
    Search;
    AssertTrue('at least one point', FFound.PointsCount > 0);
end;

procedure TBackgroundSearchTest.TheFirstProposalIsTheLowestPointInTheData;
begin
    //  THE ORDER MATTERS TO THE CALLER: it takes the first proposal as the
    //  start of the background and looks it up by x. The walk grows outward
    //  from the minimum, so the minimum is necessarily first - and a proposal
    //  list beginning anywhere else would anchor the background off the floor.
    Given([9, 7, 2, 6, 8]);
    Search;
    AssertEquals('the first x', 2.0, FFound.PointXCoord[0], 1e-12);
    AssertEquals('the first y', 2.0, FFound.PointYCoord[0], 1e-12);
end;

procedure TBackgroundSearchTest.EveryProposalIsOneOfTheDataOwnPoints;
var
    i, j: longint;
    Found: boolean;
begin
    //  NEVER AN INTERPOLATION. The caller looks each x up in the profile and
    //  asserts it is there; a computed midpoint would fail that assertion after
    //  the fact, which reads as a corrupt profile rather than a bad proposal.
    Given([9, 7, 2, 6, 8]);
    Search;
    for i := 0 to FFound.PointsCount - 1 do
    begin
        Found := False;
        for j := 0 to FData.PointsCount - 1 do
            if (Abs(FFound.PointXCoord[i] - FData.PointXCoord[j]) < 1e-12) and
                (Abs(FFound.PointYCoord[i] - FData.PointYCoord[j]) < 1e-12) then
                Found := True;
        AssertTrue(Format('proposal %d is a data point', [i]), Found);
    end;
end;

procedure TBackgroundSearchTest.NoPointIsProposedTwice;
var
    i, j: longint;
begin
    //  THE CALLER CLEARS ITS LIST rather than checking for duplicates - its own
    //  comment says so - so a repeated proposal becomes a repeated background
    //  point, and the background is then interpolated through the same x twice.
    Given([9, 7, 2, 6, 8]);
    Search;
    for i := 0 to FFound.PointsCount - 1 do
        for j := i + 1 to FFound.PointsCount - 1 do
            AssertTrue(Format('proposals %d and %d are different points',
                [i, j]),
                Abs(FFound.PointXCoord[i] - FFound.PointXCoord[j]) > 1e-12);
end;

procedure TBackgroundSearchTest.ItTerminatesOnAFlatProfile;
begin
    //  EVERY ORDINATE EQUAL is the case where "the lowest point not below the
    //  last one taken" is satisfied by every remaining point, so a walk that
    //  did not advance its limit would loop forever - and the fit would hang
    //  with no message, on data a user might well load.
    Given([4, 4, 4, 4, 4, 4]);
    Search;
    AssertTrue('it finished, and proposed something', FFound.PointsCount > 0);
end;

{ ------------------------------- a bowl ------------------------------------- }

procedure TBackgroundSearchTest.OnABowlItTracesBothFlanks;
begin
    //  THE SHAPE IT IS WRITTEN FOR: a concave background with its minimum in the
    //  interior. Both flanks rise away from it, which is what makes each step's
    //  "not below the last" satisfiable outward and nowhere else.
    Given([9, 6, 4, 1, 3, 5, 8]);
    Search;
    AssertTrue('something to the left of the minimum',
        Proposes(0) or Proposes(1) or Proposes(2));
    AssertTrue('and something to the right',
        Proposes(4) or Proposes(5) or Proposes(6));
end;

procedure TBackgroundSearchTest.IncludingTheLeftFlankThatHadNeverRun;
begin
    //  THE TEST THIS FIXTURE EXISTS FOR. Every proposal left of the minimum
    //  comes from code that no test had executed: the only one that reached the
    //  algorithm used data whose minimum was at index 0, leaving nothing to the
    //  left at all.
    //
    //  Named individually rather than counted, because the whole flank going
    //  missing is what was being missed - and a count would be satisfied by one
    //  point of it.
    Given([9, 6, 4, 1, 3, 5, 8]);
    Search;
    AssertTrue('the sample next to the minimum on the left', Proposes(2));
    AssertTrue('the one beyond it', Proposes(1));
    AssertTrue('and the left edge', Proposes(0));
end;

procedure TBackgroundSearchTest.AndItReachesBothEdges;
begin
    //  ALL THE WAY OUT. A walk that stopped early leaves the background
    //  undefined over the ends of the profile, and the caller interpolates
    //  between what it was given - so the ends get the nearest proposal's level
    //  rather than their own.
    Given([9, 6, 4, 1, 3, 5, 8]);
    Search;
    AssertTrue('the left edge', Proposes(0));
    AssertTrue('the right edge', Proposes(6));
end;

procedure TBackgroundSearchTest.TheProposalsRiseAwayFromTheMinimum;
var
    i: longint;
    Prev: double;
begin
    //  THE FLOOR RULE, observed rather than restated: each step takes a point
    //  not below the one before, so reading the proposals outward on either
    //  side gives a non-decreasing sequence. That is what "traces the inside of
    //  a bowl" means, and without it the walk would wander back down into the
    //  minimum and never leave.
    Given([9, 6, 4, 1, 3, 5, 8]);
    Search;
    //  Leftward from the minimum at x=3.
    Prev := ProposedAt(3);
    for i := 2 downto 0 do
        if Proposes(i) then
        begin
            AssertTrue(Format('x=%d is not below its inward neighbour', [i]),
                ProposedAt(i) >= Prev - 1e-12);
            Prev := ProposedAt(i);
        end;
    //  And rightward.
    Prev := ProposedAt(3);
    for i := 4 to 6 do
        if Proposes(i) then
        begin
            AssertTrue(Format('x=%d is not below its inward neighbour', [i]),
                ProposedAt(i) >= Prev - 1e-12);
            Prev := ProposedAt(i);
        end;
end;

procedure TBackgroundSearchTest.ItDoesNotProposeThePeakItself;
begin
    //  A PEAK IS SIGNAL, NOT BACKGROUND. Proposed as background it would be
    //  subtracted from the data, and the curve meant to model it would be
    //  fitted to what was left - which is a hole.
    //
    //  The bowl here has a spike on its right flank: the walk climbs past the
    //  spike's foot and must not take its apex, because the apex is far above
    //  the flank the walk is following.
    Given([9, 6, 4, 1, 3, 40, 8]);
    Search;
    AssertFalse('the spike apex at x=5 is not background', Proposes(5));
end;

{ --------------------- the minimum at an edge (the old case) ---------------- }

procedure TBackgroundSearchTest.WithTheMinimumAtTheLeftEdgeOnlyTheRightFlankIsWalked;
begin
    //  THE CASE THE SUITE USED TO HAVE, kept because it is also real - a profile
    //  that only rises - and because it is what made the other flank invisible.
    //  Nothing is left of the minimum, so nothing can be proposed there.
    Given([1, 3, 5, 7, 9]);
    Search;
    AssertEquals('the minimum is first', 0.0, FFound.PointXCoord[0], 1e-12);
    AssertTrue('the right flank was walked', Proposes(4));
end;

procedure TBackgroundSearchTest.AndTheMirrorAtTheRightEdge;
begin
    //  THE MIRROR, which no test had either. The right-hand walk has its own
    //  boundary guard - it checks there is a point beyond the limit before
    //  seeding - and the left-hand one does not, so the two are not symmetric
    //  code and cannot be assumed symmetric in behaviour.
    Given([9, 7, 5, 3, 1]);
    Search;
    AssertEquals('the minimum is first', 4.0, FFound.PointXCoord[0], 1e-12);
    AssertTrue('the left flank was walked', Proposes(0));
end;

{ ------------------- outside its stated domain: characterised --------------- }

procedure TBackgroundSearchTest.OnARisingProfileItProposesTheWholeThing;
begin
    //  NOT A BOWL, and the unit's own comment says it will not work for one.
    //  What it actually does: every point qualifies, because each is above the
    //  last, so the whole profile is proposed as background - and subtracting
    //  that leaves nothing to fit.
    //
    //  Characterised so that the failure has a name. A user who loads a
    //  monotone profile and asks for an automatic background gets an empty
    //  model, and there is nothing in the program that says why.
    Given([1, 2, 3, 4, 5]);
    Search;
    AssertEquals('every point was proposed', FData.PointsCount,
        FFound.PointsCount);
end;

procedure TBackgroundSearchTest.OnATwoPeakedProfileItStopsAtTheFirstRise;
var
    Proposed: longint;
begin
    //  A VALLEY BETWEEN TWO PEAKS is not concave, and this is the shape a real
    //  diffractogram with two well-separated peaks presents. The walk climbs out
    //  of the global minimum and stops where the profile turns back down,
    //  because a lower point beyond the turn is below the floor.
    //
    //  So the background is proposed over part of the range and not the rest.
    //  Recorded rather than fixed: what SHOULD happen is a product question -
    //  refuse, warn, or propose per valley - and the honest first step is that
    //  the behaviour is written down.
    Given([8, 3, 9, 2, 7]);
    Search;
    Proposed := FFound.PointsCount;
    AssertTrue('it proposed something', Proposed > 0);
    AssertTrue('but not the whole profile', Proposed < FData.PointsCount);
    AssertTrue('starting from the global minimum at x=3', Proposes(3));
end;

{ ----------------------------- degenerate inputs ---------------------------- }

procedure TBackgroundSearchTest.OnePointIsItsOwnBackground;
begin
    Given([7]);
    Search;
    AssertEquals('one proposal', 1, FFound.PointsCount);
    AssertEquals('and it is the point', 7.0, FFound.PointYCoord[0], 1e-12);
end;

procedure TBackgroundSearchTest.TwoPointsGiveBoth;
begin
    //  The smallest input with a flank at all. Both are proposed: the lower is
    //  the minimum, the higher is one step out from it.
    Given([2, 5]);
    Search;
    AssertEquals('two proposals', 2, FFound.PointsCount);
    AssertTrue('the minimum', Proposes(0));
    AssertTrue('and its neighbour', Proposes(1));
end;

procedure TBackgroundSearchTest.AnAbsentDataSetIsRefused;
var
    Raised: boolean;
begin
    //  REFUSED, not answered empty. The caller has already checked its data is
    //  assigned, so reaching here with nothing is a programming error - and an
    //  empty proposal would fail later, inside the service, as an assertion
    //  about the profile.
    Raised := False;
    try
        ProposeBackgroundPoints(nil).Free;
    except
        on Exception do
            Raised := True;
    end;
    AssertTrue('nothing to search is refused', Raised);
end;

initialization
    //  A unit test: a list of ordinates in, a list of proposals out. No service,
    //  no profile object of the engine's, no fit - which is what eighty-two
    //  lines on a four-thousand-line class could not be asked without.
    RegisterTest('unit', TBackgroundSearchTest);
end.
