// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(A series refilled with a curve's points changes once, not once a point.)

FOUND IN USE: switched to animation in the middle of a fit, the window stopped
answering. Every frame refills every curve's series, and the series were filled
one point at a time - each point a change the chart broadcast to its listeners
and answered with a repaint. A frame of a profile of thousands of points took
longer to draw than the next one took to arrive, so the window never caught up.
A refill is one change.
}
unit testcase_chart_refill;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Types, fpcunit, testregistry, fit_chart;

type
    TChartRefillTest = class(TTestCase)
    published
        procedure ARefillIsOneChangeHoweverManyPoints;
        procedure AndItHoldsExactlyThePointsGiven;
    end;

implementation

type
    { Counts the changes its source announces - what the chart repaints on. }
    TCountingSerie = class(TFitSerie)
    protected
        procedure SourceChanged(ASender: TObject); override;
    public
        Changes: longint;
    end;

procedure TCountingSerie.SourceChanged(ASender: TObject);
begin
    Inc(Changes);
    inherited SourceChanged(ASender);
end;

procedure Fill(out AX, AY: TDoubleDynArray; ACount: longint);
var
    i: longint;
begin
    SetLength(AX, ACount);
    SetLength(AY, ACount);
    for i := 0 to ACount - 1 do
    begin
        AX[i] := i * 0.5;
        AY[i] := Sqr(i);
    end;
end;

procedure TChartRefillTest.ARefillIsOneChangeHoweverManyPoints;
var
    Chart: TFitChart;
    S: TCountingSerie;
    X, Y: TDoubleDynArray;
begin
    Chart := TFitChart.Create(nil);
    try
        S := TCountingSerie.Create(nil);
        Chart.AddSeries(S);
        S.AddXY(0, 1);
        Fill(X, Y, 5000);
        S.Changes := 0;
        S.ReplacePoints(X, Y);
        AssertEquals('one change for the whole refill', 1, S.Changes);
    finally
        Chart.Free;
    end;
end;

procedure TChartRefillTest.AndItHoldsExactlyThePointsGiven;
var
    Chart: TFitChart;
    S: TFitSerie;
    X, Y: TDoubleDynArray;
    i: longint;
begin
    Chart := TFitChart.Create(nil);
    try
        S := TFitSerie.Create(nil);
        Chart.AddSeries(S);
        S.AddXY(100, 100);
        Fill(X, Y, 7);
        S.ReplacePoints(X, Y);
        AssertEquals('the points given, and only they', 7, S.Count);
        for i := 0 to 6 do
        begin
            AssertEquals('x', X[i], S.XValue[i], 0);
            AssertEquals('y', Y[i], S.YValue[i], 0);
        end;
    finally
        Chart.Free;
    end;
end;

initialization
    //  A UNIT test: a chart and its series in memory, never drawn.
    RegisterTest('unit', TChartRefillTest);
end.
