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

end.
