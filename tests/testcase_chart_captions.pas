// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A chart series carries a caption for each of its points.)

A module hands the view a caption per point - a wave's pivots are numbered 1 to
5 or lettered A to C - and the view used to drop them, because the series had
nowhere to keep one. These tests hold the series to keeping them, point by
point, until they are replaced. Drawing them is the series' Draw, which needs a
chart on screen and is checked there.
}
unit testcase_chart_captions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, Graphics, TAGraph;

type
    TChartCaptionsTest = class(TTestCase)
    published
        procedure EachPointHasTheCaptionItWasGiven;
        procedure APointWithoutOneHasNone;
        procedure NewCaptionsReplaceTheOldOnes;
        procedure NoCaptionsLeavesEveryPointBare;
    end;

implementation

function Captions(const AItems: array of string): TStringList;
var
    i: longint;
begin
    Result := TStringList.Create;
    for i := 0 to High(AItems) do
        Result.Add(AItems[i]);
end;

procedure TChartCaptionsTest.EachPointHasTheCaptionItWasGiven;
var
    Serie: TTASerie;
    L: TStringList;
begin
    Serie := TTASerie.Create(nil);
    L := Captions(['1', '2', '3']);
    try
        Serie.SetCaptions(L);
        AssertEquals('1', Serie.PointCaption(0));
        AssertEquals('3', Serie.PointCaption(2));
    finally
        L.Free;
        Serie.Free;
    end;
end;

procedure TChartCaptionsTest.APointWithoutOneHasNone;
var
    Serie: TTASerie;
    L: TStringList;
begin
    //  Fewer captions than points is a series whose later points are bare,
    //  not an error - and never a caption borrowed from another point.
    Serie := TTASerie.Create(nil);
    L := Captions(['A']);
    try
        Serie.SetCaptions(L);
        AssertEquals('', Serie.PointCaption(1));
        AssertEquals('', Serie.PointCaption(-1));
    finally
        L.Free;
        Serie.Free;
    end;
end;

procedure TChartCaptionsTest.NewCaptionsReplaceTheOldOnes;
var
    Serie: TTASerie;
    L1, L2: TStringList;
begin
    Serie := TTASerie.Create(nil);
    L1 := Captions(['1', '2']);
    L2 := Captions(['A']);
    try
        Serie.SetCaptions(L1);
        Serie.SetCaptions(L2);
        AssertEquals('A', Serie.PointCaption(0));
        AssertEquals('', Serie.PointCaption(1));
    finally
        L1.Free;
        L2.Free;
        Serie.Free;
    end;
end;

procedure TChartCaptionsTest.NoCaptionsLeavesEveryPointBare;
var
    Serie: TTASerie;
    L: TStringList;
begin
    //  Nil is what every caller without captions passes.
    Serie := TTASerie.Create(nil);
    L := Captions(['1']);
    try
        Serie.SetCaptions(L);
        Serie.SetCaptions(nil);
        AssertEquals('', Serie.PointCaption(0));
    finally
        L.Free;
        Serie.Free;
    end;
end;

initialization
    //  A unit test: a series in memory, never drawn.
    RegisterTest('unit', TChartCaptionsTest);
end.
