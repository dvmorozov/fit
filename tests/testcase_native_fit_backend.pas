// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(The native backends: which minimizer each makes, and for which task.)

A native backend is named by the kind it was registered as and fits by the
minimizer its factory makes for the task (minimizer_registration). The fit
itself runs the optimiser and is tested elsewhere; what is decided here - the
name, the minimizer's class, and that the block-wise one asks THIS task which
block each parameter is in - was reached only by integration runs.
}
unit testcase_native_fit_backend;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, fit_task, int_minimizer,
    block_simplex_minimizer, native_fit_backend;

type
    TNativeFitBackendTest = class(TTestCase)
    private
        FTask: TFitTask;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure TheDefaultBackendIsNamedForTheDownhillSimplex;
        procedure ABackendIsNamedAsItWasRegistered;
        procedure TheBlockSimplexAsksThisTaskForItsBlocks;
        procedure TheRestartedSimplexIsTheRestartedOne;
    end;

implementation

procedure TNativeFitBackendTest.SetUp;
begin
    FTask := TFitTask.Create(nil, False, False);
end;

procedure TNativeFitBackendTest.TearDown;
begin
    FreeAndNil(FTask);
end;

procedure TNativeFitBackendTest.TheDefaultBackendIsNamedForTheDownhillSimplex;
var
    B: TNativeFitBackend;
begin
    B := TNativeFitBackend.Create;
    try
        AssertEquals('Native (Downhill Simplex)', B.Name);
    finally
        B.Free;
    end;
end;

procedure TNativeFitBackendTest.ABackendIsNamedAsItWasRegistered;
var
    B: TNativeFitBackend;
begin
    B := TNativeFitBackend.Create(@NewBlockDownhillSimplex, 'Block-wise');
    try
        AssertEquals('Block-wise', B.Name);
    finally
        B.Free;
    end;
end;

procedure TNativeFitBackendTest.TheBlockSimplexAsksThisTaskForItsBlocks;
var
    M: TMinimizer;
begin
    //  The blocks are the task's: one per curve and one for what they share.
    //  A minimizer wired to no task, or to another, sweeps blocks that are not
    //  this model's.
    M := NewBlockDownhillSimplex(FTask, False, 0);
    try
        AssertTrue('the block-wise minimizer', M is TBlockDownhillSimplexMinimizer);
        with TBlockDownhillSimplexMinimizer(M) do
        begin
            AssertTrue('asks which block a parameter is in',
                Assigned(OnGetParamBlock));
            AssertTrue('of this task', TMethod(OnGetParamBlock).Data = Pointer(FTask));
            AssertTrue('and how wide a block is',
                Assigned(OnGetBlockWidth));
            AssertTrue('of this task too',
                TMethod(OnGetBlockWidth).Data = Pointer(FTask));
        end;
    finally
        M.Free;
    end;
end;

procedure TNativeFitBackendTest.TheRestartedSimplexIsTheRestartedOne;
var
    M: TMinimizer;
begin
    M := NewRestartedDownhillSimplex(FTask, True, 1e-4);
    try
        AssertEquals(TRestartedDownhillSimplexMinimizer.ClassName, M.ClassName);
    finally
        M.Free;
    end;
end;

initialization
    //  A UNIT test: a task and a minimizer in memory; nothing is fitted.
    RegisterTest('unit', TNativeFitBackendTest);
end.
