{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of generic points set.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit points_set;

interface

uses Classes, SysUtils, SimpMath, self_copied_component;

type
    { Generic point set. }
    TPointsSet = class(TSelfCopiedComponent)
    protected
        FPoints: TwoDimArray;
        
        function GetPointsCount: LongInt;
        function GetPointXCoord(index: LongInt): Double; virtual;
        procedure SetPointXCoord(index: LongInt; Value: Double); virtual;
        function GetPointYCoord(index: LongInt): Double; virtual;
        procedure SetPointYCoord(index: LongInt; Value: Double); virtual;
        function GetMaxXCoord: Double;
        function GetMaxYCoord: Double;

    public
        procedure CopyParameters(const Dest: TObject); override;
        
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure AddNewPoint(XValue, YValue: Double);
        procedure ReplacePoint(
            PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
        procedure DeletePoint(XValue: Double);
        procedure Clear;
        procedure Sort; virtual;
        { Returns index of point with given X, -1 if point not found. }
        function IndexOfValueX(XValue: Double): LongInt;
        { Returns index of point having X closest to the given value. }
        function IndexOfNearestToX(XValue: Double): LongInt;

        property PointsCount: LongInt read GetPointsCount;
        property Points: TwoDimArray read FPoints;
        property PointXCoord[index: LongInt]: Double
            read GetPointXCoord write SetPointXCoord;
        property PointYCoord[index: LongInt]: Double
            read GetPointYCoord write SetPointYCoord;
        property MaxXCoord: Double read GetMaxXCoord;
        property MaxYCoord: Double read GetMaxYCoord;
    end;

implementation

{============================== TPointsSet =================================}

function TPointsSet.GetPointsCount: LongInt;
begin
    Result := Length(FPoints);
end;

function TPointsSet.GetPointXCoord(index: LongInt): Double;
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    Result := FPoints[index][1];
end;

function TPointsSet.GetPointYCoord(index: LongInt): Double;
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    Result := FPoints[index][2];
end;

procedure TPointsSet.SetPointXCoord(index: LongInt; Value: Double);
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    FPoints[index][1] := Value;
end;

procedure TPointsSet.SetPointYCoord(index: LongInt; Value: Double);
begin
    Assert(Assigned(FPoints));
    Assert(index >= 0);
    Assert(index < PointsCount);
    FPoints[index][2] := Value;
end;

function TPointsSet.GetMaxXCoord: Double;
var i: LongInt;
    MaxX: Double;
begin
    MaxX := PointXCoord[0];
    for i := 1 to PointsCount - 1 do
        if PointXCoord[i] > MaxX then MaxX := PointXCoord[i];
    Result := MaxX;
end;

function TPointsSet.GetMaxYCoord: Double;
var i: LongInt;
    MaxY: Double;
begin
    MaxY := PointYCoord[0];
    for i := 1 to PointsCount - 1 do
        if PointYCoord[i] > MaxY then MaxY := PointYCoord[i];
    Result := MaxY;
end;

procedure TPointsSet.CopyParameters(const Dest: TObject);
var i: LongInt;
begin
    inherited;
    TPointsSet(Dest).Clear;
    for i := 0 to PointsCount - 1 do
        TPointsSet(Dest).AddNewPoint(PointXCoord[i], PointYCoord[i]);
end;

procedure TPointsSet.AddNewPoint(XValue,YValue: Double);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[PointsCount - 1][1] := XValue;
    FPoints[PointsCount - 1][2] := YValue;
end;

procedure TPointsSet.ReplacePoint(
    PrevXValue, PrevYValue, NewXValue, NewYValue: Double);
var i: LongInt;
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
        begin
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
    end;
    { Point not found - add a new one. }
    AddNewPoint(NewXValue, NewYValue);
    Sort;
end;

procedure TPointsSet.Clear;
begin
    FPoints := nil;
end;

procedure TPointsSet.DeletePoint(XValue: Double);
var j, Index: LongInt;
    NewPoints: TwoDimArray;
    Found: Boolean;
begin
    //  poisk tochki v nabore; otsutstvie ne schitat' oschibkoy
    SetLength(NewPoints, PointsCount - 1);
    Found := False;
    try
        Index := 0;
        for j := 0 to PointsCount - 1 do
        begin
            if (Abs(XValue - PointXCoord[j]) <= TINY) and
                (not Found) then Found := True
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
        end;
    except
        NewPoints := nil;
        raise;
    end;
    if Found then
    begin
        FPoints := nil;
        FPoints := NewPoints;
    end else NewPoints := nil;
end;

procedure TPointsSet.Sort;
var NewPoints: TwoDimArray;
    i, j: LongInt;
    MinValueX, MaxValueX: Double;
    CurMaxValueX: Double;
    index: LongInt;
begin
    if PointsCount = 0 then Exit;
    SetLength(NewPoints, PointsCount);
    try
        //  poisk indeksa tochki s min. X
        MinValueX := PointXCoord[0]; index := 0;
        for j := 1 to PointsCount - 1 do
        begin
            if PointXCoord[j] < MinValueX then
            begin
                MinValueX := PointXCoord[j];
                index := j;
            end;
        end;
        NewPoints[0][1] := FPoints[index][1];
        NewPoints[0][2] := FPoints[index][2];
        //  poisk maksimal'nogo X
        MaxValueX := PointXCoord[0];
        for j := 1 to PointsCount - 1 do
            if PointXCoord[j] > MaxValueX then MaxValueX := PointXCoord[j];
        //  tsikl po vse ostavshimsya tochkam novogo massiva
        for i := 1 to PointsCount - 1 do
        begin
            CurMaxValueX := MaxValueX;
            index := -1;
            //  nahodim naimen'shuyu koord. X bol'shuyu zadannoy
            for j := 0 to PointsCount - 1 do
            begin
                if (PointXCoord[j] > MinValueX) and
                   (PointXCoord[j] <= CurMaxValueX) then
                begin
                    CurMaxValueX := PointXCoord[j];
                    index := j;
                end;
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

function TPointsSet.IndexOfValueX(XValue: Double): LongInt;
var i: LongInt;
begin
    Result := -1;
    for i := 0 to PointsCount - 1 do
    begin
        if (Abs(XValue - PointXCoord[i]) <= TINY) then
        begin Result := i; Exit end;
    end;
end;

function TPointsSet.IndexOfNearestToX(XValue: Double): LongInt;
var i: LongInt;
    Min, Cur: Double;
begin
    Result := -1;
    for i := 0 to PointsCount - 1 do
    begin
        if i = 0 then
        begin
            Min := Abs(XValue - PointXCoord[i]);
            Result := 0;
        end
        else
        begin
            Cur := Abs(XValue - PointXCoord[i]);
            if Cur < Min then
            begin
                Min := Cur;
                Result := i;
            end;
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


