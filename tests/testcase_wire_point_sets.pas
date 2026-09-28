// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A point set and a curve, rebuilt from what the wire carries.)

TWO PATHS BUILD THE SAME OBJECTS. The finished model is read curve by curve over
GET /curves/<cid>/points; an animated frame arrives as a snapshot in one
progress reply. Both must produce the same curve - the same type name, the same
handle - or a frame and the result that follows it would name one curve two ways,
and a highlight or a deletion would lose track of it between the two. So both
go through these functions, and these are tested once.
}
unit testcase_wire_point_sets;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    fit_points_json, title_points_set, named_points_set, curve_instance_id,
    wire_point_sets;

type
    TWirePointSetsTest = class(TTestCase)
    private
        function ThreePoints(const ATitle: string): TPointsData;
    published
        procedure APointSetKeepsItsTitleAndEveryPoint;
        procedure AnEmptyWireSetIsAnEmptyPointSet;
        procedure ACurveCarriesItsTypeName;
        procedure AndItsPoints;
        procedure AndTheHandleTheModelGaveIt;
        procedure ACurveWithNoReadableHandleIsStillACurve;
    end;

implementation

function TWirePointSetsTest.ThreePoints(const ATitle: string): TPointsData;
begin
    Result := Default(TPointsData);
    Result.Title := ATitle;
    SetLength(Result.X, 3);
    SetLength(Result.Y, 3);
    Result.X[0] := 1; Result.Y[0] := 10;
    Result.X[1] := 2; Result.Y[1] := 20;
    Result.X[2] := 3; Result.Y[2] := 30;
end;

procedure TWirePointSetsTest.APointSetKeepsItsTitleAndEveryPoint;
var
    S: TTitlePointsSet;
begin
    S := TitlePointsSetOf(ThreePoints('computed'));
    try
        AssertEquals('title', 'computed', S.FTitle);
        AssertEquals('points', 3, S.PointsCount);
        AssertEquals('x', 2.0, S.PointXCoord[1], 1e-12);
        AssertEquals('y', 30.0, S.PointYCoord[2], 1e-12);
    finally
        S.Free;
    end;
end;

procedure TWirePointSetsTest.AnEmptyWireSetIsAnEmptyPointSet;
var
    S: TTitlePointsSet;
begin
    S := TitlePointsSetOf(Default(TPointsData));
    try
        AssertEquals(0, S.PointsCount);
    finally
        S.Free;
    end;
end;

procedure TWirePointSetsTest.ACurveCarriesItsTypeName;
var
    C: TNamedPointsSet;
begin
    //  AS ITS TITLE, which is what the chart and the legend read. The type name
    //  proper is a class function, abstract on a curve rebuilt from the wire:
    //  such a curve is points under a name, not an instance of its type.
    C := NamedCurveOf('', ThreePoints('Gaussian'));
    try
        AssertEquals('the type, as its title', 'Gaussian', C.FTitle);
    finally
        C.Free;
    end;
end;

procedure TWirePointSetsTest.AndItsPoints;
var
    C: TNamedPointsSet;
begin
    C := NamedCurveOf('', ThreePoints('Gaussian'));
    try
        AssertEquals(3, C.PointsCount);
        AssertEquals(20.0, C.PointYCoord[1], 1e-12);
    finally
        C.Free;
    end;
end;

procedure TWirePointSetsTest.AndTheHandleTheModelGaveIt;
var
    C: TNamedPointsSet;
begin
    C := NamedCurveOf('0B0E4B7C-0000-0000-0000-000000000009',
        ThreePoints('Gaussian'));
    try
        AssertEquals('0b0e4b7c-0000-0000-0000-000000000009',
            LowerCase(CurveInstanceIdToWire(C.FInstanceId)));
    finally
        C.Free;
    end;
end;

procedure TWirePointSetsTest.ACurveWithNoReadableHandleIsStillACurve;
var
    C: TNamedPointsSet;
begin
    //  Whether a handle is REQUIRED is the caller's rule - the curves route
    //  refuses a curve without one. This only builds what it is given.
    C := NamedCurveOf('not a handle', ThreePoints('Gaussian'));
    try
        AssertEquals(3, C.PointsCount);
    finally
        C.Free;
    end;
end;

initialization
    RegisterTest('unit', TWirePointSetsTest);
end.
