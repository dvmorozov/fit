// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Proposing the background points of a diffractogram.)

WHERE THE BACKGROUND IS, given the data alone. The user may mark background
points by hand; when they do not, this proposes them, and what it proposes
decides the shape that gets subtracted from every ordinate before a single curve
is fitted. Get it wrong and the curves are fitted to a profile that is not the
measurement.

IT ASSUMES A CONCAVE BACKGROUND, which is the shape a neutron diffractogram has,
and it says so - the assumption is the algorithm rather than a caveat on it. Start
from the lowest point in the data; then walk outward in both directions, each
step taking the lowest point not lower than the last one taken. On a bowl that
traces the bowl. On any other shape it does something, and what it does is
characterised by the tests rather than promised here.

IT WAS AN EIGHTY-TWO-LINE METHOD ON THE COMPUTE SERVICE, touching no field of it
and calling nothing else on it - a free function wearing a class's name. Half of
it had never run: the one test that reached it fed data whose minimum sits at the
left edge, so the entire leftward walk was dead, and with it the assumption the
comment is about. An algorithm whose stated domain has never been exercised is a
claim, not a guarantee.

OWNERSHIP. The caller owns the answer. It is a valid set or an exception - never
nil - and a failure part-way through frees what had been built rather than
leaking it, which is why the body is wrapped rather than written straight.
}
unit background_search;

{$mode objfpc}{$H+}

interface

uses
    points_set;

{ The background points AData suggests, as (x, y) taken from AData itself.

  EVERY PROPOSED POINT IS ONE OF THE DATA'S OWN, never an interpolation: what is
  subtracted has to be anchored to something measured, and the caller looks each
  one up in the profile by its x - a point that was not there would fail that
  lookup rather than being noticed here.

  The first point is the global minimum. Those after it come outward from it,
  alternating sides while either side still has one to give, so the order is
  neither left-to-right nor sorted - the caller sorts.

  Raises rather than answering nil. }
function ProposeBackgroundPoints(AData: TPointsSet): TPointsSet;

{ TAKES THE BACKGROUND OFF AData, in place: the piecewise chord through the
  data at ABackground's x values is subtracted from every sample between the
  first of them and the last, and nothing outside that stretch is touched.
  Every background point comes out at zero.

  THE KNOTS ARE THE DATA'S OWN ORDINATES. A background point says WHERE the
  background is touched; the data says how high it is there - which is what
  Model > Background > Subtract has always done with the user's picks.

  Both sets are sorted by x first. A background point that is not a sample of
  AData fails an internal check: the caller looks them up in the profile, and
  the points it hands over are picks on the data or proposals from it.

  Moved here from TFitService.SubtractBackground, where it could only be
  reached through a problem, so the automatic run can apply it to a COPY
  (BackgroundRemovedCopy) while the manual command keeps applying it to the
  profile itself. Fewer than two background points take nothing off. }
procedure SubtractPiecewiseLinear(AData, ABackground: TPointsSet);

{ A new copy of AData with the background background_search proposes for it
  taken off - what automatic subtraction would leave, without touching AData.
  The automatic run searches this for peaks: the search stops at a fraction of
  the highest point, so on a raised baseline every sample would be a "peak".
  The caller owns the answer. }
function BackgroundRemovedCopy(AData: TPointsSet): TPointsSet;

implementation

uses
    checks;

{ The index of the lowest ordinate in AData. }
function IndexOfLowest(AData: TPointsSet): longint;
var
    i: longint;
    Min: double;
begin
    Result := 0;
    Min := AData.PointYCoord[0];
    for i := 1 to AData.PointsCount - 1 do
        if AData.PointYCoord[i] < Min then
        begin
            Min := AData.PointYCoord[i];
            Result := i;
        end;
end;

{ The next background point to the LEFT of ALimit: the lowest ordinate strictly
  before it that is not below AFloor.

  NOT BELOW AFLOOR is what makes the walk climb. Each step's floor is the
  ordinate the previous step took, so the sequence rises away from the minimum -
  which is what tracing the inside of a bowl means. Without the floor the search
  would keep finding the minimum's neighbours and never move.

  Answers -1 when there is nothing to take. }
function NextToTheLeft(AData: TPointsSet; ALimit: longint;
    const AFloor: double): longint;
var
    i: longint;
    CurMin: double;
begin
    //  SEEDED FROM POINT ZERO, AND THE SEED IS NOT FLOOR-CHECKED. The loop
    //  below starts at 1, so point 0 can only enter as the seed - and it enters
    //  whatever its ordinate is, including one BELOW the floor. So the leftmost
    //  sample is always eligible while anything remains to the left of the
    //  limit, even when it dips under the point the walk last took.
    //
    //  That is the behaviour as it stands and it is what the tests pin. On the
    //  concave data this is written for it is harmless - the flank rises towards
    //  the edge, so the edge is above the floor anyway - and it is the reason a
    //  profile that falls away at its left edge gets a background point there.
    Result := 0;
    CurMin := AData.PointYCoord[0];
    for i := 1 to ALimit - 1 do
        if (AData.PointYCoord[i] < CurMin) and
            (AData.PointYCoord[i] >= AFloor) then
        begin
            CurMin := AData.PointYCoord[i];
            Result := i;
        end;
    //  Nothing strictly to the left of the limit.
    if Result >= ALimit then
        Result := -1;
end;

{ The mirror of NextToTheLeft: the lowest ordinate strictly after ALimit that is
  not below AFloor, or -1. }
function NextToTheRight(AData: TPointsSet; ALimit: longint;
    const AFloor: double): longint;
var
    i: longint;
    CurMin: double;
begin
    Result := -1;
    if ALimit + 1 > AData.PointsCount - 1 then
        Exit;
    Result := ALimit + 1;
    CurMin := AData.PointYCoord[ALimit + 1];
    for i := ALimit + 2 to AData.PointsCount - 1 do
        if (AData.PointYCoord[i] < CurMin) and
            (AData.PointYCoord[i] >= AFloor) then
        begin
            CurMin := AData.PointYCoord[i];
            Result := i;
        end;
    if Result <= ALimit then
        Result := -1;
end;

function ProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
var
    LeftIndex, RightIndex, Next: longint;
    LeftFloor, RightFloor: double;
    Moved: boolean;
begin
    CheckAssigned(AData, 'the data record');

    Result := TPointsSet.Create(nil);
    try
        LeftIndex := IndexOfLowest(AData);
        RightIndex := LeftIndex;
        LeftFloor := AData.PointYCoord[LeftIndex];
        RightFloor := LeftFloor;
        Result.AddNewPoint(AData.PointXCoord[LeftIndex], LeftFloor);

        //  BOTH SIDES EACH TIME ROUND, and the loop ends when neither side
        //  moved. Ending on the first side that runs out would stop the other
        //  flank half-traced, and the background would be proposed for one half
        //  of the profile.
        Moved := True;
        while Moved do
        begin
            Moved := False;

            Next := NextToTheLeft(AData, LeftIndex, LeftFloor);
            if Next >= 0 then
            begin
                LeftIndex := Next;
                LeftFloor := AData.PointYCoord[Next];
                Result.AddNewPoint(AData.PointXCoord[Next], LeftFloor);
                Moved := True;
            end;

            Next := NextToTheRight(AData, RightIndex, RightFloor);
            if Next >= 0 then
            begin
                RightIndex := Next;
                RightFloor := AData.PointYCoord[Next];
                Result.AddNewPoint(AData.PointXCoord[Next], RightFloor);
                Moved := True;
            end;
        end;
    except
        //  The caller owns the answer or gets an exception, never a half-built
        //  set and never nil.
        Result.Free;
        raise;
    end;
end;

{ Subtracts the straight line from sample AStart to sample AEnd, both
  included, measured from the data's own ordinates at those two samples. }
procedure SubtractChordBetween(AData: TPointsSet; AStart, AEnd: longint);
var
    i: longint;
    Delta, I0: double;
begin
    CheckThat(AEnd > AStart, 'a background stretch must end after it begins');
    Delta := (AData.PointYCoord[AEnd] - AData.PointYCoord[AStart]) /
        (AData.PointXCoord[AEnd] - AData.PointXCoord[AStart]);
    I0 := AData.PointYCoord[AStart];
    for i := AStart to AEnd do
        AData.PointYCoord[i] := AData.PointYCoord[i] - I0 -
            (AData.PointXCoord[i] - AData.PointXCoord[AStart]) * Delta;
end;

procedure SubtractPiecewiseLinear(AData, ABackground: TPointsSet);
var
    i, StartIndex, EndIndex: longint;
    SavedI: double;
begin
    CheckAssigned(AData, 'the data record');
    CheckAssigned(ABackground, 'the background points to subtract');
    if ABackground.PointsCount < 2 then
        Exit;
    AData.Sort;
    ABackground.Sort;

    StartIndex := AData.IndexOfValueX(ABackground.PointXCoord[0]);
    CheckThat(StartIndex <> -1,
        'the first background point must fall on a sample of the profile');
    EndIndex := StartIndex;
    for i := 1 to ABackground.PointsCount - 1 do
    begin
        EndIndex := AData.IndexOfValueX(ABackground.PointXCoord[i]);
        CheckThat(EndIndex <> -1,
            'every background point must fall on a sample of the profile');
        //  EVERY STRETCH INCLUDES BOTH ITS ENDS, so the end's ordinate is kept
        //  and put back - otherwise the next stretch would start from a point
        //  that can only ever come out as zero.
        SavedI := AData.PointYCoord[EndIndex];
        SubtractChordBetween(AData, StartIndex, EndIndex);
        StartIndex := EndIndex;
        AData.PointYCoord[EndIndex] := SavedI;
    end;
    //  Undoes that restoration for the very last point.
    AData.PointYCoord[EndIndex] := 0;
end;

function BackgroundRemovedCopy(AData: TPointsSet): TPointsSet;
var
    Back: TPointsSet;
begin
    CheckAssigned(AData, 'the data record');
    Result := TPointsSet(AData.GetCopy);
    try
        Back := ProposeBackgroundPoints(Result);
        try
            SubtractPiecewiseLinear(Result, Back);
        finally
            Back.Free;
        end;
    except
        Result.Free;
        raise;
    end;
end;

end.
