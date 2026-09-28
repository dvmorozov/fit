// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The small tables beside the chart: how many rows, and what is in them.)

FOUR GRIDS SHOW A POINT SET each - the picked positions, the background points,
the profile itself - and a fifth shows the fitting intervals. All five were
filled by a method that set `RowCount` from an expression and then wrote cells in
a loop, inside a unit that reaches into the main form by name. The arithmetic
was never reachable by a test.

THE INTERVALS TABLE IS THE ONE THAT IS NOT OBVIOUS. Bounds are picked in pairs
and a user is very often halfway through picking one, so the count can be odd -
and the table then needs a row for the half-made interval with its second cell
left blank. Getting the row count wrong by one either hides the interval the user
is in the middle of marking, or leaves a blank row at the bottom that looks like
one they have not started.
}
unit points_tables;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, points_set, title_points_set,
    //  The number formatting the tables share.
    summary_table;

type
    { Which set of positions the positions table should show. }
    TPositionsSource = (
        //  Nothing to show: no picks, and no model to have built any.
        psNone,
        //  What the user PICKED, which is what the table has always shown.
        psPicked,
        //  Where the model's curves actually sit. Shown when nothing was
        //  picked, which is not an edge case: an analysis pack places its
        //  instances from its own markup and picks nothing at all, so such
        //  a model drew its positions on the chart and left the table
        //  empty - the model was there and the table said there was none.
        psAchieved);

{ Which set the positions table should show, given how many of each there are.

  PICKS FIRST when there are any, because they are what the user put there and
  what they can edit; the achieved positions are the model's answer, and showing
  those instead would quietly replace the input with the output. }
function PositionsForTable(APickCount, AAchievedCount: longint):
    TPositionsSource;

{ How many rows a plain (x, y) table needs for APointCount points, including its
  heading row. }
function PointsTableRowCount(APointCount: longint): longint;

{ The cell of a plain (x, y) table, or '' for anything outside it.

  ARow counts the heading, so row 1 is the first point. Column 0 is the
  abscissa, column 1 the ordinate. }
function PointsCellText(APoints: TPointsSet; ACol, ARow: longint): string;

{ How many rows an EDITABLE (x, y) table needs, including its heading and one
  blank row at the end for a value the user is about to type.

  THE SPARE ROW IS THE AFFORDANCE. The profile grid is the one table a value can
  be typed into, and a grid with no empty row at the bottom offers nowhere to
  type it. The other three deliberately have none, because entering a point by
  hand is not supported there and a blank row invites someone to try. }
function EditablePointsTableRowCount(APointCount: longint): longint;

{ How many rows the intervals table needs for APointCount picked bounds,
  including its heading row.

  ONE ROW PER PAIR, PLUS ONE FOR AN ODD REMAINDER. The count is odd whenever the
  user is halfway through picking an interval, which is a state they are in every
  time they mark one - so it is the ordinary case rather than an edge. }
function IntervalTableRowCount(APointCount: longint): longint;

{ The cell of the intervals table, or '' for anything outside it - which
  includes the SECOND cell of an interval that has only been half picked. Both
  columns hold abscissae: an interval is a stretch of the axis, so its ordinates
  mean nothing and are not shown. }
function IntervalCellText(APoints: TPointsSet; ACol, ARow: longint): string;

type
    { A pick to move, from where to where. }
    TPickMove = record
        PrevX, PrevY, NewX, NewY: double;
    end;

{ The pick a row of the Curve Positions table shows: row 1 is pick 0. -1 for
  the heading. }
function PositionsCellPick(ARow: longint): longint;

{ The bound a cell of the Fit Intervals table shows: each row is one interval,
  its start in column 0 and its end in column 1. -1 for the heading. }
function IntervalCellPick(ACol, ARow: longint): longint;

{ Moving pick AIndex of APicks to the position typed as ATyped.

  A PICK IS A REAL SAMPLE of the profile, so the typed value is taken to the
  nearest sample, and the pick takes that sample's value. True, with the move
  in AMove, when there is one; False otherwise - with AWhy saying why in words
  when the typed text is refused (not a number, outside the profile, another
  pick's sample), and AWhy empty when there is simply nothing to do (the same
  sample, or no pick at AIndex). }
function PlanPickMove(AProfile, APicks: TPointsSet; AIndex: longint;
    const ATyped: string; out AMove: TPickMove; out AWhy: string): boolean;

{ Whether bound AIndex of ABounds - fit interval bounds, which pair in order -
  may move to ANewX: strictly between the bounds before and after it. Past a
  neighbour the pairs would re-form over stretches nobody chose. }
function BoundMoveKeepsOrder(ABounds: TPointsSet; AIndex: longint;
    ANewX: double): boolean;

implementation

function PositionsForTable(APickCount, AAchievedCount: longint):
    TPositionsSource;
begin
    if APickCount > 0 then
        Result := psPicked
    else if AAchievedCount > 0 then
        Result := psAchieved
    else
        Result := psNone;
end;

function PointsTableRowCount(APointCount: longint): longint;
begin
    if APointCount < 0 then
        APointCount := 0;
    //  No spare row: entering a point by hand is not supported, and an empty
    //  row at the bottom of a table invites someone to try.
    Result := APointCount + 1;
end;

function PointsCellText(APoints: TPointsSet; ACol, ARow: longint): string;
var
    Index: longint;
begin
    Result := '';
    if not Assigned(APoints) then
        Exit;
    if (ACol < 0) or (ACol > 1) then
        Exit;
    Index := ARow - 1;
    if (Index < 0) or (Index >= APoints.PointsCount) then
        Exit;
    if ACol = 0 then
        Result := CurveValueText(APoints.PointXCoord[Index])
    else
        Result := CurveValueText(APoints.PointYCoord[Index]);
end;

function EditablePointsTableRowCount(APointCount: longint): longint;
begin
    Result := PointsTableRowCount(APointCount) + 1;
end;

function IntervalTableRowCount(APointCount: longint): longint;
begin
    if APointCount < 0 then
        APointCount := 0;
    Result := APointCount div 2 + APointCount mod 2 + 1;
end;

function IntervalCellText(APoints: TPointsSet; ACol, ARow: longint): string;
var
    Index: longint;
begin
    Result := '';
    if not Assigned(APoints) then
        Exit;
    if (ACol < 0) or (ACol > 1) then
        Exit;
    //  Row 1 is the first pair, so its two cells are points 0 and 1.
    Index := (ARow - 1) * 2 + ACol;
    if (ARow < 1) or (Index < 0) or (Index >= APoints.PointsCount) then
        Exit;
    Result := CurveValueText(APoints.PointXCoord[Index]);
end;

function PositionsCellPick(ARow: longint): longint;
begin
    Result := ARow - 1;
    if Result < 0 then
        Result := -1;
end;

function IntervalCellPick(ACol, ARow: longint): longint;
begin
    if (ARow < 1) or (ACol < 0) or (ACol > 1) then
        Exit(-1);
    Result := (ARow - 1) * 2 + ACol;
end;

function PlanPickMove(AProfile, APicks: TPointsSet; AIndex: longint;
    const ATyped: string; out AMove: TPickMove; out AWhy: string): boolean;
var
    Typed: double;
    Sample, Other: longint;
begin
    Result := False;
    AWhy := '';
    AMove := Default(TPickMove);
    if (not Assigned(AProfile)) or (not Assigned(APicks)) or (AIndex < 0) or
        (AIndex >= APicks.PointsCount) or (AProfile.PointsCount = 0) then
        Exit;
    if not TryStrToFloat(Trim(ATyped), Typed) then
    begin
        AWhy := '"' + Trim(ATyped) + '" is not a number, so the point was not ' +
            'moved. Type the new position as a number.';
        Exit;
    end;
    //  NOT TAKEN TO THE END: a value beyond the profile is a typo or a unit,
    //  and moving the pick to the last sample would hide that.
    if (Typed < AProfile.PointXCoord[0]) or
        (Typed > AProfile.PointXCoord[AProfile.PointsCount - 1]) then
    begin
        AWhy := Format('%s is outside the profile, which runs from %s to %s, ' +
            'so the point was not moved.', [Trim(ATyped),
            FloatToStr(AProfile.PointXCoord[0]),
            FloatToStr(AProfile.PointXCoord[AProfile.PointsCount - 1])]);
        Exit;
    end;
    Sample := AProfile.IndexOfNearestToX(Typed);
    AMove.PrevX := APicks.PointXCoord[AIndex];
    AMove.PrevY := APicks.PointYCoord[AIndex];
    AMove.NewX := AProfile.PointXCoord[Sample];
    AMove.NewY := AProfile.PointYCoord[Sample];
    if AMove.NewX = AMove.PrevX then
        Exit;
    //  ONE PICK PER SAMPLE: two on one sample are one pick, and the curve the
    //  moved one seeds would disappear without a word.
    Other := APicks.IndexOfValueX(AMove.NewX);
    if (Other >= 0) and (Other <> AIndex) then
    begin
        AWhy := Format('There is already a point at %s, so this one was not ' +
            'moved onto it.', [FloatToStr(AMove.NewX)]);
        Exit;
    end;
    Result := True;
end;

function BoundMoveKeepsOrder(ABounds: TPointsSet; AIndex: longint;
    ANewX: double): boolean;
begin
    Result := (AIndex >= 0) and (AIndex < ABounds.PointsCount);
    if Result and (AIndex > 0) then
        Result := ANewX > ABounds.PointXCoord[AIndex - 1];
    if Result and (AIndex < ABounds.PointsCount - 1) then
        Result := ANewX < ABounds.PointXCoord[AIndex + 1];
end;

end.
