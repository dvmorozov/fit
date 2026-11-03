// SPDX-License-Identifier: GPL-3.0-or-later
{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(A Moffat fitted on the Python sidecar: the width cap that moves with
the shape the fit reaches.)

The adapter between the engine and the sidecar is the framework's, tested with
the framework's own test types (testcase_python_backend). What needs a shape
whose width per unit of sigma depends on another of its parameters - this
module's Moffat, through m - is here: the cap the sidecar is handed holds for
the m the fit starts from, and the backend asks again, a bounded number of
times, with the cap of the shape reached.

No socket and no optimiser: the sidecar's reply is canned
(mock_python_transport), so a unit test.
}
unit testcase_moffat_sidecar_cap;

{$MODE Delphi}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry,
    fit_task, fit_problem_json, fit_task_marshalling, SimpMath,
    mock_python_transport, curve_points_set,
    //  Linked so that its self-registration runs: the type the problem names.
    moffat_points_set;

type
    TMoffatSidecarCapTest = class(TTestCase)
    private
        FBackend: TMockPythonBackend;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure AResultHeldByAMovedCapIsFittedAgainWithinBoundedRounds;
    end;

implementation

procedure TMoffatSidecarCapTest.SetUp;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    FBackend := TMockPythonBackend.Create('http://127.0.0.1:9');
end;

procedure TMoffatSidecarCapTest.TearDown;
begin
    FreeAndNil(FBackend);
end;

procedure TMoffatSidecarCapTest.AResultHeldByAMovedCapIsFittedAgainWithinBoundedRounds;
var
    P: TFitProblem;
    Task: TFitTask;
    x: double;
    n: integer;
begin
    //  A MOFFAT'S WIDTH PER UNIT OF SIGMA DEPENDS ON m, so the cap the sidecar
    //  is handed is the one for the m the fit starts from (1.5: sigma up to
    //  20 / 1.53). Its answer raises m to 0.5 - where sigma may be no more
    //  than 20 / 3.46 - with a sigma of 10 the starting cap allowed. The task
    //  holds it at the new cap, and the backend asks again with the bounds
    //  the reached shape gives: here the answer never changes, so it stops
    //  after its bounded number of rounds rather than asking forever.
    P := Default(TFitProblem);
    n := 0;
    x := 0;
    while x <= 20 + 1E-9 do
    begin
        SetLength(P.ProfileX, n + 1);
        SetLength(P.ProfileY, n + 1);
        P.ProfileX[n] := x;
        P.ProfileY[n] := GaussPoint(100, 1.5, 10, x);
        Inc(n);
        x := x + 0.5;
    end;
    P.PositionsX := TDoubleArray.Create(10);
    P.PositionsY := TDoubleArray.Create(GaussPoint(100, 1.5, 10, 10));
    P.CurveTypeId := '{e6e51486-6970-4ab3-8010-908664116379}';
    Task := BuildTaskFromProblem(P);
    try
        FBackend.Reply('{"errorCode":0,"rFactor":0.1,"curves":[{"params":[' +
            '{"name":"A","value":50,"error":-1},' +
            '{"name":"x0","value":10,"error":-1},' +
            '{"name":"sigma","value":10,"error":-1},' +
            '{"name":"m","value":0.5,"error":-1}]}]}');
        FBackend.Fit(Task);
        AssertEquals('asked again, a bounded number of times', 3, FBackend.Calls);
        AssertTrue('and the sigma kept fits the shape reached: ' + FloatToStr(
            TCurvePointsSet(Task.GetCurves.Items[0]).ValuesByName['sigma']),
            TCurvePointsSet(Task.GetCurves.Items[0]).ValuesByName['sigma'] <=
            20 / (2 * Sqrt(3)) + 1e-9);
    finally
        Task.Free;
    end;
end;

initialization
    RegisterTest('unit', TMoffatSidecarCapTest);
end.
