// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(Which samples a curve is given, and the words when it covers none.)

FOUND IN USE: an automatic run over a real project was refused with "A curve was
placed at 2.89..2.93, which is outside the data (1..75)". It was not outside the
data: a component had become narrower than the spacing of the samples and sat
between two of them. A refusal that names the wrong cause sends the user looking
for a pattern off the edge of the chart that is not there.
}
unit testcase_curve_samples;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, points_set,
    gauss_points_set, fit_task, MyExceptions;

type
    TCurveSamplesTest = class(TTestCase)
    private
        FTask: TFitTask;
        function RefusalFor(ALo, AHi: double): string;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ACurveBetweenTwoSamplesIsNotCalledOutsideTheData;
        procedure ACurveOffTheDataIsCalledOutsideIt;
    end;

implementation

type
    { A Gaussian given a compact support, which no framework type has. }
    TSpanCurve = class(TGaussPointsSet)
    public
        Lo, Hi: double;
        function SupportMin: double; override;
        function SupportMax: double; override;
        function CoversSample(const AX: double): boolean; override;
    end;

function TSpanCurve.SupportMin: double;
begin
    Result := Lo;
end;

function TSpanCurve.SupportMax: double;
begin
    Result := Hi;
end;

function TSpanCurve.CoversSample(const AX: double): boolean;
begin
    Result := (AX >= Lo) and (AX <= Hi);
end;

procedure TCurveSamplesTest.SetUp;
var
    P: TPointsSet;
    i: longint;
begin
    FTask := TFitTask.Create(nil, False, False);
    P := TPointsSet.Create(nil);
    for i := 1 to 75 do
        P.AddNewPoint(i, 10);
    FTask.SetProfilePointsSet(P);
end;

procedure TCurveSamplesTest.TearDown;
begin
    FreeAndNil(FTask);
end;

function TCurveSamplesTest.RefusalFor(ALo, AHi: double): string;
var
    C: TSpanCurve;
begin
    Result := '';
    C := TSpanCurve.Create(nil);
    try
        C.Lo := ALo;
        C.Hi := AHi;
        try
            FTask.CreatePointsFor(C);
        except
            on E: EUserException do
                Result := E.Message;
        end;
    finally
        C.Free;
    end;
end;

procedure TCurveSamplesTest.ACurveBetweenTwoSamplesIsNotCalledOutsideTheData;
var
    Why: string;
begin
    Why := RefusalFor(2.89, 2.93);
    AssertTrue('refused', Why <> '');
    AssertEquals('not called outside the data: ' + Why, 0,
        Pos('outside the data', Why));
    AssertTrue('it says it lies between two samples: ' + Why,
        Pos('between the samples at 2 and 3', Why) > 0);
end;

procedure TCurveSamplesTest.ACurveOffTheDataIsCalledOutsideIt;
var
    Why: string;
begin
    Why := RefusalFor(80, 90);
    AssertTrue('called outside the data: ' + Why,
        Pos('outside the data', Why) > 0);
end;

initialization
    //  A UNIT test: a task and a profile in memory.
    RegisterTest('unit', TCurveSamplesTest);
end.
