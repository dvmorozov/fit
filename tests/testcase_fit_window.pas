// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The samples a fit is measured over, and which interval each is in.)

The server's statistics and the window's spectrum both read the difference over
the fit intervals; they must read the same samples, so one function picks them.
}
unit testcase_fit_window;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_window;

type
    TFitWindowTest = class(TTestCase)
    published
        procedure OnlyTheSamplesInsideAnIntervalAreKept;
        procedure AnIntervalIncludesBothItsEnds;
        procedure EachIntervalIsASpanOfItsOwn;
        procedure BoundsAreReadAsConsecutivePairs;
        procedure WithNoIntervalNothingIsKept;
    end;

implementation

procedure TFitWindowTest.OnlyTheSamplesInsideAnIntervalAreKept;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3, 4, 5], [10, 20, 30, 40, 50], [11, 21, 31, 41, 51],
        [2, 4]);
    AssertEquals('three samples', 3, Length(W.X));
    AssertEquals('the data', 20, W.Data[0], 0);
    AssertEquals('the model beside it', 41, W.Fit[2], 0);
end;

procedure TFitWindowTest.AnIntervalIncludesBothItsEnds;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 3]);
    AssertEquals(3, Length(W.X));
end;

procedure TFitWindowTest.EachIntervalIsASpanOfItsOwn;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6],
        [1, 2, 5, 6]);
    AssertEquals('two spans', 2, Length(W.Spans));
    AssertEquals('the first', 1, W.Spans[0].Hi);
    AssertEquals('the second starts after it', 2, W.Spans[1].Lo);
    AssertEquals('and ends at the last', 3, W.Spans[1].Hi);
end;

procedure TFitWindowTest.BoundsAreReadAsConsecutivePairs;
var
    W: TFitWindow;
begin
    //  (1, 2) and (5, 6) - not (2, 5): the samples between are outside.
    W := FitWindowOf([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6],
        [1, 2, 5, 6]);
    AssertEquals(4, Length(W.X));
    AssertEquals('3 and 4 left out', 5, W.X[2], 0);
end;

procedure TFitWindowTest.WithNoIntervalNothingIsKept;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3], [1, 2, 3], [1, 2, 3], []);
    AssertEquals(0, Length(W.X));
    AssertEquals(0, Length(W.Spans));
end;

initialization
    RegisterTest('unit', TFitWindowTest);
end.
