// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Formulas evaluated on two threads at once.)

USER-DEFINED AND FORMULA CURVES evaluate through native_math_expr, and fit
intervals now fit side by side (docs/internal/fit-performance.md, stage 8). The
evaluator kept ONE parser and ONE cache of the last formula for the whole
process, so two intervals evaluating different formulas at once overwrote each
other's - a curve computed with the other interval's formula, or a crash. Each
thread now has its own; a lock would have made every model of such curves fit
one interval at a time.
}
unit testcase_expr_threads;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry, native_math_expr, job_pool;

type
    TExprThreadsTest = class(TTestCase)
    private
        FWrong: longint;
        FArrived: longint;
        procedure Evaluate(AIndex: longint);
    published
        procedure TwoThreadsEachGetTheirOwnFormulasValue;
    end;

implementation

procedure TExprThreadsTest.Evaluate(AIndex: longint);
var
    i: longint;
    r, Want: double;
    Params: string;
    Formula: string;
begin
    //  Both started before either computes, so they overlap.
    InterLockedIncrement(FArrived);
    while FArrived < 2 do
        Sleep(0);
    Params := 'x=3'#0#0;
    if AIndex = 0 then
    begin
        Formula := 'x*2';
        Want := 6;
    end
    else
    begin
        Formula := 'x+100';
        Want := 103;
    end;
    for i := 1 to 20000 do
    begin
        r := 0;
        ParseAndCalcExpression(PChar(Formula), PChar(Params), @r);
        if r <> Want then
            InterLockedIncrement(FWrong);
    end;
    ReleaseThreadExpressionEngine;
end;

procedure TExprThreadsTest.TwoThreadsEachGetTheirOwnFormulasValue;
begin
    FWrong := 0;
    FArrived := 0;
    RunJobs(2, 2, [], @Evaluate);
    AssertEquals('every value was its own formula''s', 0, FWrong);
end;

initialization
    RegisterTest('unit', TExprThreadsTest);
end.
