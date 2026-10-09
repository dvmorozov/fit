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
    gauss_points_set, fit_task, MyExceptions, sample_coverage,
    span_gauss_points_set;

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
        procedure ACurveMarksOnlyTheSamplesItCovers;
        procedure AnUnboundedCurveMarksItsWholeWindow;
        procedure TheSamplesNoCurveCoversAreReportedPerTask;
        procedure ATaskHoldingNoCurveLeavesItsWholeIntervalUncovered;
        procedure AVariedBackgroundCoversEverySample;
        procedure ATypeBuiltThroughTheGenericPathStaysWhereItWasPicked;
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

function CoveredCount(const ACovered: array of boolean): longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to High(ACovered) do
        if ACovered[i] then
            Inc(Result);
end;

procedure TCurveSamplesTest.ACurveMarksOnlyTheSamplesItCovers;
var
    C: TSpanCurve;
    Covered: array of boolean;
begin
    C := TSpanCurve.Create(nil);
    try
        C.Lo := 10;
        C.Hi := 20;
        FTask.CreatePointsFor(C);
        SetLength(Covered, 75);
        C.MarkCoveredIn(Covered);
        AssertEquals('x = 10..20', 11, CoveredCount(Covered));
        AssertFalse('x = 9 is not its own', Covered[7]);
        AssertTrue('x = 10, the first sample of its own', Covered[9]);
        AssertTrue('x = 20, the last', Covered[19]);
        AssertFalse('x = 21 is not', Covered[20]);
    finally
        C.Free;
    end;
end;

procedure TCurveSamplesTest.AnUnboundedCurveMarksItsWholeWindow;
var
    C: TGaussPointsSet;
    Covered: array of boolean;
begin
    C := TGaussPointsSet.Create(nil, 40);
    try
        FTask.CreatePointsFor(C);
        SetLength(Covered, 75);
        C.MarkCoveredIn(Covered);
        AssertEquals('a Gaussian is never exactly zero', 75,
            CoveredCount(Covered));
    finally
        C.Free;
    end;
end;

{ One pick at 40 on 1..75, of the test type made to reach 30..50. }
procedure PlaceOneSpanCurveAt40(ATask: TFitTask);
var
    P: TPointsSet;
begin
    TSpanGaussPointsSet.SpanFrom := 30;
    TSpanGaussPointsSet.SpanTo := 50;
    ATask.CurveTypeId := TSpanGaussPointsSet.GetCurveTypeId;
    P := TPointsSet.Create(nil);
    P.AddNewPoint(40, 10);
    ATask.SetCurvePositions(P);
    ATask.RecreateCurves(nil);
end;

procedure TCurveSamplesTest.TheSamplesNoCurveCoversAreReportedPerTask;
var
    R: TSampleRanges;
begin
    PlaceOneSpanCurveAt40(FTask);
    R := FTask.UncoveredSamples;
    AssertEquals('one stretch each side', 2, Length(R));
    AssertEquals('from the first sample', 1, R[0].FromX, 0);
    AssertEquals('to the last before it', 29, R[0].ToX, 0);
    AssertEquals('after it', 51, R[1].FromX, 0);
    AssertEquals('to the end', 75, R[1].ToX, 0);
    AssertEquals('every other sample', 75 - 21, UncoveredSampleCount(R));
end;

procedure TCurveSamplesTest.ATaskHoldingNoCurveLeavesItsWholeIntervalUncovered;
var
    R: TSampleRanges;
begin
    R := FTask.UncoveredSamples;
    AssertEquals('one stretch', 1, Length(R));
    AssertEquals('every sample', 75, R[0].Count);
end;

procedure TCurveSamplesTest.AVariedBackgroundCoversEverySample;
var
    T: TFitTask;
    P: TPointsSet;
    i: longint;
begin
    //  The varied background is part of the model at every sample, so no
    //  sample is modelled as nothing.
    T := TFitTask.Create(nil, True, False);
    try
        P := TPointsSet.Create(nil);
        for i := 1 to 75 do
            P.AddNewPoint(i, 10);
        T.SetProfilePointsSet(P);
        PlaceOneSpanCurveAt40(T);
        AssertEquals(0, Length(T.UncoveredSamples));
    finally
        T.Free;
    end;
end;

procedure TCurveSamplesTest.ATypeBuiltThroughTheGenericPathStaysWhereItWasPicked;
begin
    //  FOUND WRITING THESE TESTS: a registered type is constructed at x0 = 0 and
    //  placed afterwards, and its position was then clamped to the first
    //  sample of its window - picked at 40, built at 30.
    PlaceOneSpanCurveAt40(FTask);
    AssertEquals('at its pick', 40,
        TSpanGaussPointsSet(FTask.GetCurves.Items[0]).x0, 1e-9);
end;

initialization
    //  A UNIT test: a task and a profile in memory.
    RegisterTest('unit', TCurveSamplesTest);
end.
