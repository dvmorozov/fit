// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A background arithmetic and a peak search for the framework's own
tests.)

The engine asks for both by contract (background_algorithms,
peak_search_registry) and a build without a module that provides them refuses
the commands that need them - rightly. But what the ENGINE does around them -
computes positions inside the selected interval, pairs the bounds, carries the
background into a fit, refuses a command in a state that forbids it - is the
framework's, and its tests need something registered to do it with. Naming a
module's algorithms made the framework's suite a module's suite; these stand
in for them.

DELIBERATELY PLAIN: the background is the chord through the profile's lowest
point and its two ends, the peaks are the local maxima standing clear of the
floor, and a peak's interval runs down each flank as far as the data keeps
falling. What a field's search does better is that module's to test.

REGISTERED ONLY WHERE NOTHING IS: the registries replace on registration, and a
suite that links a module as well must fit with the module's - whichever of the
two initializes first, the module's ends up registered.
}
unit test_algorithms;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, points_set, title_points_set, named_points_set;

{ The profile's lowest point and its two ends, lowest first. }
function TestProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
{ Takes off the straight lines through ABackground, sorted by x; level beyond
  its ends. }
procedure TestSubtractPiecewiseLinear(AData, ABackground: TPointsSet);
function TestBackgroundRemovedCopy(AData: TPointsSet): TPointsSet;

{ Every local maximum - minimum when ASearchMinimums - standing more than a
  thirtieth of the profile's range clear of the other extreme, highest (lowest)
  first. }
function TestFindPeakPoints(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
procedure TestFindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
{ Each peak's interval, from where its flanks stop falling; intervals that
  meet or overlap are one. }
procedure TestFindPeakBounds(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);

implementation

uses
    background_algorithms, peak_search_registry, implementation_registry;

function TestProposeBackgroundPoints(AData: TPointsSet): TPointsSet;
var
    i, Lowest, Last: longint;
begin
    Result := TPointsSet.Create(nil);
    if AData.PointsCount = 0 then
        Exit;
    Lowest := 0;
    for i := 1 to AData.PointsCount - 1 do
        if AData.PointYCoord[i] < AData.PointYCoord[Lowest] then
            Lowest := i;
    Last := AData.PointsCount - 1;
    Result.AddNewPoint(AData.PointXCoord[Lowest], AData.PointYCoord[Lowest]);
    if Lowest <> 0 then
        Result.AddNewPoint(AData.PointXCoord[0], AData.PointYCoord[0]);
    if (Lowest <> Last) and (Last <> 0) then
        Result.AddNewPoint(AData.PointXCoord[Last], AData.PointYCoord[Last]);
end;

procedure TestSubtractPiecewiseLinear(AData, ABackground: TPointsSet);
var
    Line: TPointsSet;
    i, j: longint;
    X, Level: double;
begin
    if ABackground.PointsCount = 0 then
        Exit;
    Line := TPointsSet.Create(nil);
    try
        Line.CopyPointsFrom(ABackground);
        Line.Sort;
        for i := 0 to AData.PointsCount - 1 do
        begin
            X := AData.PointXCoord[i];
            if X <= Line.PointXCoord[0] then
                Level := Line.PointYCoord[0]
            else if X >= Line.PointXCoord[Line.PointsCount - 1] then
                Level := Line.PointYCoord[Line.PointsCount - 1]
            else
            begin
                j := 1;
                while Line.PointXCoord[j] < X do
                    Inc(j);
                Level := Line.PointYCoord[j - 1] +
                    (Line.PointYCoord[j] - Line.PointYCoord[j - 1]) *
                    (X - Line.PointXCoord[j - 1]) /
                    (Line.PointXCoord[j] - Line.PointXCoord[j - 1]);
            end;
            AData.PointYCoord[i] := AData.PointYCoord[i] - Level;
        end;
    finally
        Line.Free;
    end;
end;

function TestBackgroundRemovedCopy(AData: TPointsSet): TPointsSet;
var
    Background: TPointsSet;
begin
    Result := TPointsSet.Create(nil);
    Result.CopyPointsFrom(AData);
    Background := TestProposeBackgroundPoints(AData);
    try
        TestSubtractPiecewiseLinear(Result, Background);
    finally
        Background.Free;
    end;
end;

{ Whether sample i is a local extremum of the kind asked: above (below) the
  sample before it and not below (above) the one after. An end counts against
  its one neighbour. }
function IsExtremum(AData: TPointsSet; i: longint; AMinimum: boolean): boolean;
var
    Y, S: double;
begin
    S := 1;
    if AMinimum then
        S := -1;
    Y := S * AData.PointYCoord[i];
    Result := ((i = 0) or (Y > S * AData.PointYCoord[i - 1])) and
        ((i = AData.PointsCount - 1) or (Y >= S * AData.PointYCoord[i + 1]));
end;

function TestFindPeakPoints(AData: TPointsSet; ASearchMinimums: boolean;
    AHost: TObject): TTitlePointsSet;
var
    i, j: longint;
    Lo, Hi, Floor: double;
    Order: array of longint;
    Tmp: longint;
begin
    Result := TTitlePointsSet.Create(nil);
    if AData.PointsCount < 2 then
        Exit;
    Lo := AData.PointYCoord[0];
    Hi := Lo;
    for i := 1 to AData.PointsCount - 1 do
    begin
        if AData.PointYCoord[i] < Lo then
            Lo := AData.PointYCoord[i];
        if AData.PointYCoord[i] > Hi then
            Hi := AData.PointYCoord[i];
    end;
    if Hi <= Lo then
        Exit;
    Floor := (Hi - Lo) / 30;
    Order := nil;
    begin
        for i := 0 to AData.PointsCount - 1 do
            if IsExtremum(AData, i, ASearchMinimums) and
                ((not ASearchMinimums and (AData.PointYCoord[i] - Lo > Floor)) or
                (ASearchMinimums and (Hi - AData.PointYCoord[i] > Floor))) then
            begin
                SetLength(Order, Length(Order) + 1);
                Order[High(Order)] := i;
            end;
        //  Highest first (lowest first for troughs): a handful, so a plain
        //  insertion sort.
        for i := 1 to High(Order) do
        begin
            j := i;
            while (j > 0) and (
                (not ASearchMinimums and (AData.PointYCoord[Order[j]] >
                    AData.PointYCoord[Order[j - 1]])) or
                (ASearchMinimums and (AData.PointYCoord[Order[j]] <
                    AData.PointYCoord[Order[j - 1]]))) do
            begin
                Tmp := Order[j];
                Order[j] := Order[j - 1];
                Order[j - 1] := Tmp;
                Dec(j);
            end;
        end;
        for i := 0 to High(Order) do
            Result.AddNewPoint(AData.PointXCoord[Order[i]],
                AData.PointYCoord[Order[i]]);
    end;
end;

procedure AddFound(AData: TPointsSet; AMinimums: boolean; APositions: TPointsSet);
var
    Found: TTitlePointsSet;
    i: longint;
begin
    Found := TestFindPeakPoints(AData, AMinimums, nil);
    try
        for i := 0 to Found.PointsCount - 1 do
            if APositions.IndexOfValueX(Found.PointXCoord[i]) < 0 then
                APositions.AddNewPoint(Found.PointXCoord[i], Found.PointYCoord[i]);
    finally
        Found.Free;
    end;
end;

{$hints off}
procedure TestFindCurvePositions(AData: TPointsSet; AMode: TExtremumMode;
    AHost: TObject; APositions: TPointsSet);
begin
    if AMode in [OnlyMaximums, MaximumsAndMinimums] then
        AddFound(AData, False, APositions);
    if AMode in [OnlyMinimums, MaximumsAndMinimums] then
        AddFound(AData, True, APositions);
end;

procedure TestFindPeakBounds(AData: TPointsSet; AHost: TObject;
    ABounds: TPointsSet);
var
    Peaks: TTitlePointsSet;
    Starts, Ends: array of longint;
    i, j, k, L, R, Tmp: longint;
begin
    Peaks := TestFindPeakPoints(AData, False, nil);
    try
        Starts := nil;
        Ends := nil;
        for i := 0 to Peaks.PointsCount - 1 do
        begin
            k := AData.IndexOfValueX(Peaks.PointXCoord[i]);
            if k < 0 then
                Continue;
            L := k;
            while (L > 0) and (AData.PointYCoord[L - 1] < AData.PointYCoord[L]) do
                Dec(L);
            R := k;
            while (R < AData.PointsCount - 1) and
                (AData.PointYCoord[R + 1] < AData.PointYCoord[R]) do
                Inc(R);
            if L = R then
                Continue;
            SetLength(Starts, Length(Starts) + 1);
            SetLength(Ends, Length(Ends) + 1);
            Starts[High(Starts)] := L;
            Ends[High(Ends)] := R;
        end;
        //  In order of where they start, then merged where they meet.
        for i := 1 to High(Starts) do
        begin
            j := i;
            while (j > 0) and (Starts[j] < Starts[j - 1]) do
            begin
                Tmp := Starts[j]; Starts[j] := Starts[j - 1]; Starts[j - 1] := Tmp;
                Tmp := Ends[j]; Ends[j] := Ends[j - 1]; Ends[j - 1] := Tmp;
                Dec(j);
            end;
        end;
        i := 0;
        while i <= High(Starts) do
        begin
            L := Starts[i];
            R := Ends[i];
            while (i < High(Starts)) and (Starts[i + 1] <= R) do
            begin
                Inc(i);
                if Ends[i] > R then
                    R := Ends[i];
            end;
            ABounds.AddNewPoint(AData.PointXCoord[L], AData.PointYCoord[L]);
            ABounds.AddNewPoint(AData.PointXCoord[R], AData.PointYCoord[R]);
            Inc(i);
        end;
    finally
        Peaks.Free;
    end;
end;
{$hints on}

initialization
    if DefaultImplementationId(BackgroundContract) = '' then
        RegisterBackgroundAlgorithms(@TestProposeBackgroundPoints,
            @TestSubtractPiecewiseLinear, @TestBackgroundRemovedCopy);
    if not PeakSearchAvailable then
        RegisterPeakSearch(@TestFindPeakPoints, @TestFindCurvePositions,
            @TestFindPeakBounds);
end.
