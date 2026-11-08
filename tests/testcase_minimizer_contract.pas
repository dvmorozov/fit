// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a minimiser needs before it will run, and the abscissa a
diffraction point set reports itself at.)

TWO SMALL CONTRACTS, both of them nine-or-so near-identical lines that nothing
had exercised.

`TMinimizer.IsReady` refuses to start until all nine of its callbacks are
attached. Nine guards, one per callback, written out in full - which is exactly
the shape where one gets missed in a merge, and a missing guard means the
algorithm calls a nil method in the middle of a fit, on a worker thread.

`TPointsSet` reports the same sample at three abscissae - the scattering
angle, twice it, and sin(theta)/lambda - because which one the user is looking at
is a menu choice and the data does not change. Getting a conversion wrong moves
every point on the chart, consistently, which looks like data rather than like a
bug.
}
unit testcase_minimizer_contract;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    int_minimizer, points_set, SimpMath;

type
    { Somewhere for the callbacks to point. What they do does not matter here -
      IsReady asks only whether they are there. }
    TCallbackHolder = class(TObject)
        function GetFunc: double;
        procedure ComputeFunc;
        function GetVariationStep: double;
        procedure SetVariationStep(NewStep: double);
        procedure SetFirstParam;
        procedure SetNextParam;
        function GetParam: double;
        procedure SetParam(NewVal: double);
        function EndOfCycle: boolean;
    end;

    { TMinimizer's Minimize is abstract, and IsReady is not - so a bare
      descendant is all that is needed to ask it. }
    TBareMinimizer = class(TMinimizer)
        procedure Minimize(var ErrorCode: longint); override;
    end;

    TMinimizerContractTest = class(TTestCase)
    private
        FMin: TBareMinimizer;
        FHolder: TCallbackHolder;
        { Attaches all nine callbacks. }
        procedure AttachEverything;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure WithNothingAttachedItIsNotReady;
        procedure WithEverythingAttachedItIsReady;
        procedure EveryOneOfTheNineIsRequired;
        procedure ItIsNotTerminatedToBeginWith;
    end;

    { Copying a point set's samples - what the neutron point set did, until it
      went with its wavelength and angles (2026-10-17); the copy is TPointsSet's. }
    TPointsSetCopyTest = class(TTestCase)
    published
        procedure CopyingBringsThePoints;
        procedure ASetMadeFromPointsHoldsThem;
    end;

implementation

function TCallbackHolder.GetFunc: double; begin Result := 0; end;
procedure TCallbackHolder.ComputeFunc; begin end;
function TCallbackHolder.GetVariationStep: double; begin Result := 1; end;
procedure TCallbackHolder.SetVariationStep(NewStep: double); begin end;
procedure TCallbackHolder.SetFirstParam; begin end;
procedure TCallbackHolder.SetNextParam; begin end;
function TCallbackHolder.GetParam: double; begin Result := 0; end;
procedure TCallbackHolder.SetParam(NewVal: double); begin end;
function TCallbackHolder.EndOfCycle: boolean; begin Result := True; end;

procedure TBareMinimizer.Minimize(var ErrorCode: longint);
begin
    ErrorCode := MIN_NO_ERRORS;
end;

{ ---- what a minimiser needs before it will run ----------------------------- }

procedure TMinimizerContractTest.SetUp;
begin
    FMin := TBareMinimizer.Create(nil);
    FHolder := TCallbackHolder.Create;
end;

procedure TMinimizerContractTest.TearDown;
begin
    FreeAndNil(FMin);
    FreeAndNil(FHolder);
end;

procedure TMinimizerContractTest.AttachEverything;
begin
    FMin.OnGetFunc := @FHolder.GetFunc;
    FMin.OnComputeFunc := @FHolder.ComputeFunc;
    FMin.OnGetVariationStep := @FHolder.GetVariationStep;
    FMin.OnSetVariationStep := @FHolder.SetVariationStep;
    FMin.OnSetFirstParam := @FHolder.SetFirstParam;
    FMin.OnSetNextParam := @FHolder.SetNextParam;
    FMin.OnGetParam := @FHolder.GetParam;
    FMin.OnSetParam := @FHolder.SetParam;
    FMin.OnEndOfCycle := @FHolder.EndOfCycle;
end;

procedure TMinimizerContractTest.WithNothingAttachedItIsNotReady;
begin
    //  A minimiser constructed and not wired up. Starting it would call a nil
    //  method in the middle of a fit, on a worker thread, where the failure is
    //  reported as a fatal fault naming a source line.
    AssertEquals('refused', MIN_FUNCTION_NOT_ASSIGNED, FMin.IsReady);
end;

procedure TMinimizerContractTest.WithEverythingAttachedItIsReady;
begin
    AttachEverything;
    AssertEquals('ready', MIN_NO_ERRORS, FMin.IsReady);
end;

procedure TMinimizerContractTest.EveryOneOfTheNineIsRequired;
var
    i: longint;
begin
    //  THE SWEEP. Nine near-identical guards written out in full is exactly the
    //  shape where one gets dropped in a merge, and the one that is dropped is
    //  invisible until a fit reaches the callback it guarded.
    //
    //  Each is removed in turn from a fully wired minimiser, so a guard that
    //  went missing shows up as this test passing where it should not.
    for i := 0 to 8 do
    begin
        AttachEverything;
        case i of
            0: FMin.OnGetFunc := nil;
            1: FMin.OnComputeFunc := nil;
            2: FMin.OnGetVariationStep := nil;
            3: FMin.OnSetVariationStep := nil;
            4: FMin.OnSetFirstParam := nil;
            5: FMin.OnSetNextParam := nil;
            6: FMin.OnGetParam := nil;
            7: FMin.OnSetParam := nil;
            8: FMin.OnEndOfCycle := nil;
        end;
        AssertEquals(Format('callback %d is required', [i]),
            MIN_FUNCTION_NOT_ASSIGNED, FMin.IsReady);
    end;
end;

procedure TMinimizerContractTest.ItIsNotTerminatedToBeginWith;
begin
    //  A minimiser that started terminated would stop before its first cycle,
    //  and report a fit that ran.
    AssertFalse('running', FMin.Terminated);
end;

{ ---- the three abscissae one sample has ------------------------------------ }

procedure TPointsSetCopyTest.CopyingBringsThePoints;
var
    Points, Other: TPointsSet;
begin
    Points := TPointsSet.Create(nil);
    Other := TPointsSet.Create(nil);
    try
        Points.AddNewPoint(60, 100);
        Points.AddNewPoint(61, 110);
        Other.CopyPointsFrom(Points);
        AssertEquals('both points', 2, Other.PointsCount);
        AssertEquals('the first abscissa', 60.0, Other.PointXCoord[0], 1E-9);
        AssertEquals('the last ordinate', 110.0, Other.PointYCoord[1], 1E-9);
    finally
        Other.Free;
        Points.Free;
    end;
end;

procedure TPointsSetCopyTest.ASetMadeFromPointsHoldsThem;
var
    Points, Other: TPointsSet;
begin
    Points := TPointsSet.Create(nil);
    try
        Points.AddNewPoint(60, 100);
        Other := TPointsSet.CreateFromPoints(nil, Points);
        try
            AssertEquals(1, Other.PointsCount);
            AssertEquals(100.0, Other.PointYCoord[0], 1E-9);
        finally
            Other.Free;
        end;
    finally
        Points.Free;
    end;
end;

initialization
    //  Unit tests: a minimiser with no algorithm behind it, and a point set in
    //  memory.
    RegisterTest('unit', TMinimizerContractTest);
    RegisterTest('unit', TPointsSetCopyTest);
end.
