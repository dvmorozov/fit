// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of generic points set.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit points_set;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, Math, self_copied_component, SimpMath, SysUtils;

type
    { Generic point set. }
    TPointsSet = class(TSelfCopiedComponent)
    protected
        FPoints: TwoDimArray;

        function GetPointsCount: longint;
        function GetPointXCoord(index: longint): double; virtual;
        procedure SetPointXCoord(index: longint; Value: double); virtual;
        function GetPointYCoord(index: longint): double; virtual;
        procedure SetPointYCoord(index: longint; Value: double); virtual;
        function GetMaxXCoord: double;
        function GetMaxYCoord: double;

    public
        procedure CopyParameters(Dest: TObject); override;


        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        { Virtual so a curve can refuse it: a curve's points are its extent,
          decided when it is built, and one that grew afterwards would be summed
          into the profile at the wrong place from then on. }
        procedure AddNewPoint(XValue, YValue: double); virtual;
        procedure ReplacePoint(PrevXValue, PrevYValue, NewXValue,
            NewYValue: double);
        procedure DeletePoint(XValue: double);
        procedure Clear;
        procedure Sort; virtual;
        { Returns index of point with given X, -1 if point not found. }
        function IndexOfValueX(XValue: double): longint;
        { Returns index of point having X closest to the given value. }
        function IndexOfNearestToX(XValue: double): longint;

        property PointsCount: longint read GetPointsCount;
        property PointXCoord[index: longint]: double
            read GetPointXCoord write SetPointXCoord;
        property PointYCoord[index: longint]: double
            read GetPointYCoord write SetPointYCoord;
        property MaxXCoord: double read GetMaxXCoord;
        property MaxYCoord: double read GetMaxYCoord;
    end;

implementation

uses
    checks;

{============================== TPointsSet =================================}

function TPointsSet.GetPointsCount: longint;
begin
    Result := Length(FPoints);
end;

function TPointsSet.GetPointXCoord(index: longint): double;
begin
    CheckIndex(index, PointsCount, 'the X coordinates of this points set');

    Result := FPoints[index][1];
end;

function TPointsSet.GetPointYCoord(index: longint): double;
begin
    CheckIndex(index, PointsCount, 'the Y coordinates of this points set');

    Result := FPoints[index][2];
end;

procedure TPointsSet.SetPointXCoord(index: longint; Value: double);
begin
    CheckIndex(index, PointsCount, 'the X coordinates of this points set');

    FPoints[index][1] := Value;
end;

procedure TPointsSet.SetPointYCoord(index: longint; Value: double);
begin
    CheckIndex(index, PointsCount, 'the Y coordinates of this points set');

    FPoints[index][2] := Value;
end;

function TPointsSet.GetMaxXCoord: double;
var
    i:    longint;
    MaxX: double;
begin
    MaxX := PointXCoord[0];
    for i := 1 to PointsCount - 1 do
        if PointXCoord[i] > MaxX then
            MaxX := PointXCoord[i];
    Result := MaxX;
end;

function TPointsSet.GetMaxYCoord: double;
var
    i:    longint;
    MaxY: double;
begin
    MaxY := PointYCoord[0];
    for i := 1 to PointsCount - 1 do
        if PointYCoord[i] > MaxY then
            MaxY := PointYCoord[i];
    Result := MaxY;
end;

procedure TPointsSet.CopyParameters(Dest: TObject);
var
    i: longint;
begin
    CheckThat(Dest.ClassType = Self.ClassType, 'a points set may only copy itself into a set of its own class');

    TPointsSet(Dest).Clear;
    inherited;
    { TODO: optimize by copying entire array. }
    for i := 0 to PointsCount - 1 do
        TPointsSet(Dest).AddNewPoint(PointXCoord[i], PointYCoord[i]);
end;

procedure TPointsSet.AddNewPoint(XValue, YValue: double);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[PointsCount - 1][1] := XValue;
    FPoints[PointsCount - 1][2] := YValue;
end;

procedure TPointsSet.ReplacePoint(PrevXValue, PrevYValue, NewXValue,
    NewYValue: double);
var
    i: longint;
begin
    { Search the point with given argument and value in the selected list of points. }
    for i := 0 to PointsCount - 1 do
    begin
        if (Abs(PrevXValue - PointXCoord[i]) <= TINY) and
            (Abs(PrevYValue - PointYCoord[i]) <= TINY) then
        begin
            PointXCoord[i] := NewXValue;
            PointYCoord[i] := NewYValue;
            Sort;
            Exit;
        end;
        { The condition must be checked because as previous coordinates
          zeros can be passed what means that new point must be added. }
        if Abs(NewXValue - PointXCoord[i]) <= TINY then
            if Abs(NewYValue - PointYCoord[i]) <= TINY then
                { Ignores duplicates by argument and value. }
                Exit
            else
            begin
                { Replaces value of function for point with given argument. }
                PointYCoord[i] := NewYValue;
                Exit;
            end;
    end;
    { Point not found - add a new one. }
    AddNewPoint(NewXValue, NewYValue);
    Sort;
end;

procedure TPointsSet.Clear;
begin
    { Terminates dynamic array controlled by reference counter. }
    SetLength(FPoints, 0);
end;

procedure TPointsSet.DeletePoint(XValue: double);
var
    j, Index:  longint;
    NewPoints: TwoDimArray;
    Found:     boolean;
begin
    //  Looks for the point; its absence is not an error.
    //
    //  ROOM FOR THE WORST CASE, which is nothing matching. Sized at
    //  PointsCount - 1 the loop copied every point into an array one short and
    //  the assertion below fired - so the "not found" branch at the end was
    //  unreachable and the no-op this method documents was a fault instead.
    //  The array is trimmed once the answer is known.
    SetLength(NewPoints, PointsCount);
    Found := False;
    try
        Index := 0;
        for j := 0 to PointsCount - 1 do
            if (Abs(XValue - PointXCoord[j]) <= TINY) and (not Found) then
                Found := True
            else
            begin
                CheckThat(Index < PointsCount, 'the copy made while deleting a point cannot hold more points than the set it came from');

                NewPoints[Index][1] := PointXCoord[j];
                NewPoints[Index][2] := PointYCoord[j];
                Inc(Index);
                //  No Break here: whether or not the result array is already
                //  full, Found still has to end up correct. Breaking out made
                //  the last point impossible to delete.
            end;
    except
        SetLength(NewPoints, 0);
        raise;
    end;
    if Found then
    begin
        //  One shorter than it was, now that the answer is known.
        SetLength(NewPoints, PointsCount - 1);
        Clear;
        FPoints := NewPoints;
    end
    else
        //  Nothing matched, so the set is left exactly as it was.
        SetLength(NewPoints, 0);
end;

{ ASCENDING BY X, CORRECT FOR ANY INPUT.

  What was here was a selection sort that consumed one DISTINCT x per output slot
  while running exactly PointsCount-1 times, so it was only well defined for a set
  whose x values were pairwise distinct. On a repeated x the scan for "the
  smallest x greater than the last" found nothing, the index stayed -1, and an
  assertion caught it - in a debug build. With assertions off the next two lines
  read FPoints[-1]: an out-of-bounds read on a dynamic array, a garbage point
  written into the result, and no error anywhere. A sort is the wrong place to
  discover a duplicate, and memory corruption is the wrong way to report one.

  So this no longer polices uniqueness; it just sorts. Where uniqueness genuinely
  matters it is enforced where it can be explained: the picked positions must name
  real samples of the profile, and TFitService.CreateTasks says so with a
  CheckThat on the grid lookup. TFitService.AddPoint is what keeps the pick sets
  free of duplicates in the first place.

  STABLE, and that is load-bearing rather than tidy: points sharing an x keep the
  order they were added in, so a caller that sorts a set twice gets the same
  answer both times, and the derived curve-position markers of two instances that
  converged on one x0 stay in curve order.

  A merge sort, not insertion: Sort is called on the whole profile by CreateTasks,
  and the old O(n^2) scan over a few thousand samples was paid on every rebuild -
  which is every model edit. }
procedure TPointsSet.Sort;
var
    Buffer: TwoDimArray;

    procedure Merge(Lo, Mid, Hi: longint);
    var
        i, j, k: longint;
    begin
        for i := Lo to Hi do
        begin
            Buffer[i][1] := FPoints[i][1];
            Buffer[i][2] := FPoints[i][2];
        end;

        i := Lo;
        j := Mid + 1;
        for k := Lo to Hi do
        begin
            if i > Mid then
            begin
                FPoints[k][1] := Buffer[j][1];
                FPoints[k][2] := Buffer[j][2];
                Inc(j);
            end
            else
            if j > Hi then
            begin
                FPoints[k][1] := Buffer[i][1];
                FPoints[k][2] := Buffer[i][2];
                Inc(i);
            end
            else
            //  <= rather than <: taking from the LEFT run when the keys are
            //  equal is exactly what makes the sort stable.
            if Buffer[i][1] <= Buffer[j][1] then
            begin
                FPoints[k][1] := Buffer[i][1];
                FPoints[k][2] := Buffer[i][2];
                Inc(i);
            end
            else
            begin
                FPoints[k][1] := Buffer[j][1];
                FPoints[k][2] := Buffer[j][2];
                Inc(j);
            end;
        end;
    end;

    procedure SortRange(Lo, Hi: longint);
    var
        Mid: longint;
    begin
        if Lo >= Hi then
            Exit;
        //  Lo + (Hi - Lo) div 2, not (Lo + Hi) div 2, which can overflow.
        Mid := Lo + (Hi - Lo) div 2;
        SortRange(Lo, Mid);
        SortRange(Mid + 1, Hi);
        //  The two runs are already in order end to end - which is the common
        //  case here, since most sets are sorted or nearly so when they arrive.
        if FPoints[Mid][1] <= FPoints[Mid + 1][1] then
            Exit;
        Merge(Lo, Mid, Hi);
    end;

begin
    if PointsCount < 2 then
        Exit;

    SetLength(Buffer, PointsCount);
    try
        SortRange(0, PointsCount - 1);
    finally
        SetLength(Buffer, 0);
    end;
end;

function TPointsSet.IndexOfValueX(XValue: double): longint;
var
    i: longint;
begin
    Result := -1;
    for i := 0 to PointsCount - 1 do
        if (Abs(XValue - PointXCoord[i]) <= TINY) then
        begin
            Result := i;
            Exit;
        end;
end;

function TPointsSet.IndexOfNearestToX(XValue: double): longint;
var
    i: longint;
    Min, Cur: double;
begin
    Result := -1;
    for i := 0 to PointsCount - 1 do
        if i = 0 then
        begin
            Min    := Abs(XValue - PointXCoord[i]);
            Result := 0;
        end
        else
        begin
            Cur := Abs(XValue - PointXCoord[i]);
            if Cur < Min then
            begin
                Min    := Cur;
                Result := i;
            end;
        end;
end;

constructor TPointsSet.Create(AOwner: TComponent);
begin
    inherited;
    Clear;
end;





destructor TPointsSet.Destroy;
begin
    Clear;
    inherited;
end;

end.
