// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Where a fit interval ends, for the module state handed to it.)

TWO INTERVALS MAY MEET AT ONE SAMPLE. A module whose items run end to end - one
pattern ending where the next begins - proposes one interval per item, and the
bounds encoding cannot hold the same x twice. So the earlier interval ends one
sample short and the later one owns the shared sample, the rule the module's own
items follow between themselves. What slicing then needs is the earlier
interval's EXCLUSIVE right limit: the first sample of the next interval when it
begins right after this one, so the item ending there still belongs here
(docs/internal/fit-performance.md, stage 7).
}
unit testcase_fit_interval_limits;

{$mode objfpc}{$H+}

interface

uses
    SysUtils, fpcunit, testregistry, fit_interval_limits;

type
    TFitIntervalLimitsTest = class(TTestCase)
    published
        procedure AnIntervalFollowedByANeighbourReachesToItsFirstSample;
        procedure OneFollowedByAGapEndsAtItsOwnLastSample;
        procedure TheLastIntervalEndsAtItsOwnLastSample;
        procedure OneFollowedByAnOverlappingStartEndsAtItsOwnLastSample;
    end;

implementation

const
    X: array[0..7] of double = (0, 1, 2, 3, 4, 5, 6, 7);

procedure TFitIntervalLimitsTest.AnIntervalFollowedByANeighbourReachesToItsFirstSample;
begin
    //  [0..3] then [4..7]: the item ending at x = 4 belongs to the first.
    AssertEquals(4.0, IntervalHiLimitX(X, 3, 4), 0);
end;

procedure TFitIntervalLimitsTest.OneFollowedByAGapEndsAtItsOwnLastSample;
begin
    //  [0..3] then [5..7]: x = 4 is in no interval, so nothing ending there
    //  belongs to either.
    AssertEquals(3.0, IntervalHiLimitX(X, 3, 5), 0);
end;

procedure TFitIntervalLimitsTest.TheLastIntervalEndsAtItsOwnLastSample;
begin
    AssertEquals(7.0, IntervalHiLimitX(X, 7, -1), 0);
end;

procedure TFitIntervalLimitsTest.OneFollowedByAnOverlappingStartEndsAtItsOwnLastSample;
begin
    //  Bounds the user picked may overlap; nothing is lent across an overlap.
    AssertEquals(3.0, IntervalHiLimitX(X, 3, 2), 0);
end;

initialization
    RegisterTest('unit', TFitIntervalLimitsTest);
end.
