// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(What a fit task refuses outright, and as which kind of error.)

WHY THIS EXISTS. `TFitTask.SetSpecialCurve` used to wrap its two preconditions in

    except on E: EAssertionFailed do WriteLog(E.Message, Warning); else raise;

under a comment calling the failure "non-fatal". That handler could not fire -
`CheckThat` raises `EInternalCheckFailed`, which it never matched - so the real
behaviour was, and is, to raise. Nothing exercised the path, so the code went on
stating the opposite for as long as nobody looked.

Narrowing an `except` clause has no compiler consequence and no test consequence
either, unless something drives the failing path. This is that something.

WHY RAISING IS ALSO THE RIGHT ANSWER. Storing an empty expression leaves a
special curve that cannot be evaluated, and a logged warning about it is exactly
the silent degradation this codebase refuses: the fit would run and answer with a
curve nobody defined. See docs/contributing/no-silent-degradation.md.
}
unit testcase_task_preconditions;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fpcunit, testregistry,
    checks, MyExceptions, fit_task, persistent_curve_parameters;

type
    TTaskPreconditionsTest = class(TTestCase)
    published
        { An expression is the whole content of a special curve. }
        procedure ASpecialCurveWithNoExpressionIsRefused;
        { The expression names parameters; without them it cannot be evaluated. }
        procedure ASpecialCurveWithNoParametersIsRefused;
        { The refusal must be a defect, not a message aimed at the user - a
          catch-all written for user error must not be able to absorb it. }
        procedure TheRefusalIsADefectRatherThanUserError;
        { And a well-formed special curve is still accepted, so the checks
          above are not simply refusing everything. }
        procedure AWellFormedSpecialCurveIsAccepted;
    end;

implementation

procedure TTaskPreconditionsTest.ASpecialCurveWithNoExpressionIsRefused;
var
    Task: TFitTask;
    Refused: boolean;
begin
    Refused := False;
    Task := TFitTask.Create(nil, False, False);
    try
        try
            Task.SetSpecialCurve('', Curve_parameters.Create(nil));
        except
            on E: EInternalCheckFailed do
                Refused := True;
        end;
        AssertTrue('an empty expression was accepted', Refused);
    finally
        Task.Free;
    end;
end;

procedure TTaskPreconditionsTest.ASpecialCurveWithNoParametersIsRefused;
var
    Task: TFitTask;
    Refused: boolean;
begin
    Refused := False;
    Task := TFitTask.Create(nil, False, False);
    try
        try
            Task.SetSpecialCurve('A*exp(-x*x)', nil);
        except
            on E: EInternalCheckFailed do
                Refused := True;
        end;
        AssertTrue('a special curve with no parameters was accepted', Refused);
    finally
        Task.Free;
    end;
end;

procedure TTaskPreconditionsTest.TheRefusalIsADefectRatherThanUserError;
var
    Task: TFitTask;
    Kind: string;
begin
    Kind := 'nothing was raised';
    Task := TFitTask.Create(nil, False, False);
    try
        try
            Task.SetSpecialCurve('', nil);
        except
            //  ORDER MATTERS ONLY FOR THE MESSAGE: the two classes are
            //  unrelated, so this reports which one arrived rather than
            //  letting the wrong one escape as a test error.
            on E: EInternalCheckFailed do
                Kind := 'defect';
            on E: EUserException do
                Kind := 'user error';
        end;
        AssertEquals('a malformed special curve is the program being wrong ' +
            'about itself, not the user asking for something unsupported',
            'defect', Kind);
    finally
        Task.Free;
    end;
end;

procedure TTaskPreconditionsTest.AWellFormedSpecialCurveIsAccepted;
var
    Task: TFitTask;
begin
    Task := TFitTask.Create(nil, False, False);
    try
        //  The task takes ownership of the parameters, so this must not be freed
        //  here - the point of the test is that the call returns at all.
        Task.SetSpecialCurve('A*exp(-x*x)', Curve_parameters.Create(nil));
        AssertTrue('a well-formed special curve was accepted', True);
    finally
        Task.Free;
    end;
end;

initialization
    RegisterTest('unit', TTaskPreconditionsTest);
end.
