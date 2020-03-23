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

uses Classes, SysUtils, SimpMath, self_copied_component;

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
        procedure CopyParameters(const Dest: TObject); override;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AddNewPoint(XValue, YValue: double);
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
        property Points: TwoDimArray read FPoints;
        property PointXCoord[index: longint]: double
            read GetPointXCoord write SetPointXCoord;
        property PointYCoord[index: longint]: double
            read GetPointYCoord write SetPointYCoord;
        property MaxXCoord: double read GetMaxXCoord;
        property MaxYCoord: double read GetMaxYCoord;
    end;

implementation

{============================== TPointsSet =================================}

function TPointsSet.GetPointsCount: longint;
begin
    Result := Length(FPoints);
end;

function TPointsSet.GetPointXCoord(index: longint): double;
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    Result := FPoints[index][1];
end;

function TPointsSet.GetPointYCoord(index: longint): double;
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    Result := FPoints[index][2];
end;

procedure TPointsSet.SetPointXCoord(index: longint; Value: double);
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    FPoints[index][1] := Value;
end;

procedure TPointsSet.SetPointYCoord(index: longint; Value: double);
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
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

procedure TPointsSet.CopyParameters(const Dest: TObject);
var
    i: longint;
begin
    inherited;
    TPointsSet(Dest).Clear;
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
    FPoints := nil;
end;

procedure TPointsSet.DeletePoint(XValue: double);
var
    j, Index:  longint;
    NewPoints: TwoDimArray;
    Found:     boolean;
begin
    //  poisk tochki v nabore; otsutstvie ne schitat' oschibkoy
    SetLength(NewPoints, PointsCount - 1);
    Found := False;
    try
        Index := 0;
        for j := 0 to PointsCount - 1 do
            if (Abs(XValue - PointXCoord[j]) <= TINY) and (not Found) then
                Found := True
            else
            begin
                Assert(Index < PointsCount - 1);
                NewPoints[Index][1] := PointXCoord[j];
                NewPoints[Index][2] := PointYCoord[j];
                Inc(Index);
                //  Break delat' nel'zya, potomu chto nezavisimo ot togo,
                //  zapolnen ves' massiv resul'tata, ili net nuzhno
                //  pravil'no ustanovit' Found; Break privodit k
                //  nevozmozhnosti udalit' poslednyuyu tochku...
            end;
    except
        NewPoints := nil;
        raise;
    end;
    if Found then
    begin
        FPoints := nil;
        FPoints := NewPoints;
    end
    else
        NewPoints := nil;
end;

procedure TPointsSet.Sort;
var
    NewPoints: TwoDimArray;
    i, j:      longint;
    MinValueX, MaxValueX: double;
    CurMaxValueX: double;
    index:     longint;
begin
    if PointsCount = 0 then
        Exit;
    SetLength(NewPoints, PointsCount);
    try
        //  poisk indeksa tochki s min. X
        MinValueX := PointXCoord[0];
        index     := 0;
        for j := 1 to PointsCount - 1 do
            if PointXCoord[j] < MinValueX then
            begin
                MinValueX := PointXCoord[j];
                index     := j;
            end;
        NewPoints[0][1] := FPoints[index][1];
        NewPoints[0][2] := FPoints[index][2];
        //  poisk maksimal'nogo X
        MaxValueX := PointXCoord[0];
        for j := 1 to PointsCount - 1 do
            if PointXCoord[j] > MaxValueX then
                MaxValueX := PointXCoord[j];
        //  tsikl po vse ostavshimsya tochkam novogo massiva
        for i := 1 to PointsCount - 1 do
        begin
            CurMaxValueX := MaxValueX;
            index := -1;
            //  nahodim naimen'shuyu koord. X bol'shuyu zadannoy
            for j := 0 to PointsCount - 1 do
                if (PointXCoord[j] > MinValueX) and
                    (PointXCoord[j] <= CurMaxValueX) then
                begin
                    CurMaxValueX := PointXCoord[j];
                    index := j;
                end;
            Assert(index <> -1);    //  ne m.b. takogo, poskol'ku ne
            //  d.b. tochek s odinakovym X
            NewPoints[i][1] := FPoints[index][1];
            NewPoints[i][2] := FPoints[index][2];
            MinValueX := CurMaxValueX;
        end;
    except
        NewPoints := nil;
        raise;
    end;

    FPoints := nil;
    FPoints := NewPoints;
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
    FPoints := nil;
end;

destructor TPointsSet.Destroy;
begin
    FPoints := nil;
    inherited Destroy;
end;

end.
