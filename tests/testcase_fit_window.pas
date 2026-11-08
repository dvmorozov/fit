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
        procedure WithNoIntervalTheWholeProfileIsOneSpan;
        procedure TheNoiseIsPickedBesideTheData;
        procedure ANoiseOutOfStepIsNotPicked;
        procedure WithNoNoiseTheWindowHasNone;
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

{ The noise of the whole series is read over the window as the data are, so
  each difference meets the variance measured at its own sample. }
procedure TFitWindowTest.TheNoiseIsPickedBesideTheData;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3, 4, 5], [10, 20, 30, 40, 50], [11, 21, 31, 41, 51],
        [0.1, 0.2, 0.3, 0.4, 0.5], [2, 3, 5, 5]);
    AssertEquals('one variance per sample kept', 3, Length(W.Noise));
    AssertEquals('the first sample''s', 0.2, W.Noise[0], 0);
    AssertEquals('the second', 0.3, W.Noise[1], 0);
    AssertEquals('across the gap, the last''s', 0.5, W.Noise[2], 0);
end;

{ A variance for some other series would weigh samples by another sample's
  noise: the window holds none, which is counts. }
procedure TFitWindowTest.ANoiseOutOfStepIsNotPicked;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3], [1, 2, 3], [1, 2, 3], [4, 4], [1, 3]);
    AssertEquals('the samples', 3, Length(W.X));
    AssertEquals('no noise', 0, Length(W.Noise));
end;

procedure TFitWindowTest.WithNoNoiseTheWindowHasNone;
var
    W: TFitWindow;
begin
    W := FitWindowOf([1, 2, 3], [1, 2, 3], [1, 2, 3], [1, 3]);
    AssertEquals('no noise', 0, Length(W.Noise));
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

procedure TFitWindowTest.WithNoIntervalTheWholeProfileIsOneSpan;
var
    W: TFitWindow;
begin
    //  NO FIT INTERVAL MEANS THE WHOLE PROFILE, as the engine fits it
    //  (TFitService.CreateTasks). Read as nothing, the window's difference
    //  strip went blank through every animated frame of a run started with
    //  the intervals cleared - the engine had filled the whole profile in,
    //  and the window's copy stayed empty until the run ended (2026-10-18).
    W := FitWindowOf([1, 2, 3], [4, 5, 6], [7, 8, 9], []);
    AssertEquals('every sample', 3, Length(W.X));
    AssertEquals('the data beside it', 6, W.Data[2], 0);
    AssertEquals('the model beside it', 7, W.Fit[0], 0);
    AssertEquals('one span', 1, Length(W.Spans));
    AssertEquals('from the first', 0, W.Spans[0].Lo);
    AssertEquals('to the last', 2, W.Spans[0].Hi);
end;

initialization
    RegisterTest('unit', TFitWindowTest);
end.
