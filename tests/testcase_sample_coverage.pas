// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which samples of a fit interval no curve covers, as stretches of x.)

FOUND IN USE: a model of compactly supported patterns whose first one began one
sample after the data did scored 1.7E-5 over the whole profile and 2.5E-7 over
two intervals starting one sample later. The model was 0 at the uncovered sample
and its residual was 68 times all the others together - and the window said
nothing. These are the stretches the window now names.
}
unit testcase_sample_coverage;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fpjson, sample_coverage;

type
    TSampleCoverageTest = class(TTestCase)
    published
        procedure EverySampleCoveredLeavesNoStretch;
        procedure ALeadingUncoveredSampleIsOneStretch;
        procedure ConsecutiveUncoveredSamplesMergeIntoOneStretch;
        procedure SeparateRunsAreSeparateStretches;
        procedure ATrailingRunReachesTheLastSample;
        procedure TheCountIsTheSumOverStretches;
        procedure ASampleIsInsideAStretchByItsX;
        procedure AppendingKeepsBothListsInOrder;
        procedure NoStretchWritesNoField;
        procedure StretchesSurviveTheirJsonRoundTrip;
        procedure AbsentFieldsReadAsNoStretch;
    end;

implementation

function Ranges(const AX: array of double;
    const ACovered: array of boolean): TSampleRanges;
begin
    Result := UncoveredRanges(AX, ACovered);
end;

procedure TSampleCoverageTest.EverySampleCoveredLeavesNoStretch;
begin
    AssertEquals(0, Length(Ranges([0, 1, 2], [True, True, True])));
end;

procedure TSampleCoverageTest.ALeadingUncoveredSampleIsOneStretch;
var
    R: TSampleRanges;
begin
    R := Ranges([0, 1, 2], [False, True, True]);
    AssertEquals('one stretch', 1, Length(R));
    AssertEquals('from', 0, R[0].FromX, 0);
    AssertEquals('to', 0, R[0].ToX, 0);
    AssertEquals('count', 1, R[0].Count);
end;

procedure TSampleCoverageTest.ConsecutiveUncoveredSamplesMergeIntoOneStretch;
var
    R: TSampleRanges;
begin
    R := Ranges([5, 6, 7, 8], [True, False, False, False]);
    AssertEquals('one stretch', 1, Length(R));
    AssertEquals('from', 6, R[0].FromX, 0);
    AssertEquals('to', 8, R[0].ToX, 0);
    AssertEquals('count', 3, R[0].Count);
end;

procedure TSampleCoverageTest.SeparateRunsAreSeparateStretches;
var
    R: TSampleRanges;
begin
    R := Ranges([0, 1, 2, 3, 4], [False, True, False, False, True]);
    AssertEquals('two stretches', 2, Length(R));
    AssertEquals('first from', 0, R[0].FromX, 0);
    AssertEquals('first count', 1, R[0].Count);
    AssertEquals('second from', 2, R[1].FromX, 0);
    AssertEquals('second to', 3, R[1].ToX, 0);
    AssertEquals('second count', 2, R[1].Count);
end;

procedure TSampleCoverageTest.ATrailingRunReachesTheLastSample;
var
    R: TSampleRanges;
begin
    R := Ranges([0, 1, 2], [True, False, False]);
    AssertEquals('one stretch', 1, Length(R));
    AssertEquals('to the last sample', 2, R[0].ToX, 0);
end;

procedure TSampleCoverageTest.TheCountIsTheSumOverStretches;
begin
    AssertEquals(3, UncoveredSampleCount(
        Ranges([0, 1, 2, 3, 4], [False, True, False, False, True])));
    AssertEquals(0, UncoveredSampleCount(nil));
end;

procedure TSampleCoverageTest.ASampleIsInsideAStretchByItsX;
var
    R: TSampleRanges;
begin
    R := Ranges([0, 1, 2, 3, 4], [False, True, False, False, True]);
    AssertTrue('0', InRanges(R, 0));
    AssertFalse('1', InRanges(R, 1));
    AssertTrue('3', InRanges(R, 3));
    AssertFalse('4', InRanges(R, 4));
end;

procedure TSampleCoverageTest.AppendingKeepsBothListsInOrder;
var
    A: TSampleRanges;
begin
    A := Ranges([0, 1], [False, True]);
    AppendRanges(A, Ranges([10, 11], [True, False]));
    AssertEquals('both', 2, Length(A));
    AssertEquals('first', 0, A[0].FromX, 0);
    AssertEquals('second', 11, A[1].FromX, 0);
end;

procedure TSampleCoverageTest.NoStretchWritesNoField;
var
    O: TJSONObject;
begin
    O := TJSONObject.Create;
    try
        AddCoverageJson(O, nil, 0);
        AssertEquals('an existing reply stays as it was', 0, O.Count);
    finally
        O.Free;
    end;
end;

procedure TSampleCoverageTest.StretchesSurviveTheirJsonRoundTrip;
var
    O: TJSONObject;
    R: TSampleRanges;
    Share: double;
begin
    O := TJSONObject.Create;
    try
        AddCoverageJson(O,
            Ranges([0, 1, 2, 3], [False, True, False, False]), 0.985);
        AssertEquals('the count is on the wire', 3,
            O.Get('uncoveredSamples', 0));
        ReadCoverageJson(O, R, Share);
        AssertEquals('stretches', 2, Length(R));
        AssertEquals('second from', 2, R[1].FromX, 0);
        AssertEquals('second to', 3, R[1].ToX, 0);
        AssertEquals('second count', 2, R[1].Count);
        AssertEquals('share', 0.985, Share, 1e-12);
    finally
        O.Free;
    end;
end;

procedure TSampleCoverageTest.AbsentFieldsReadAsNoStretch;
var
    O: TJSONObject;
    R: TSampleRanges;
    Share: double;
begin
    O := TJSONObject.Create;
    try
        ReadCoverageJson(O, R, Share);
        AssertEquals('no stretch', 0, Length(R));
        AssertEquals('no share', 0, Share, 0);
    finally
        O.Free;
    end;
end;

initialization
    //  A UNIT test: arrays and a JSON object in memory.
    RegisterTest('unit', TSampleCoverageTest);
end.
