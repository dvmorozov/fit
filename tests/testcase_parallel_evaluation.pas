// SPDX-License-Identifier: GPL-3.0-or-later
{
@abstract(One fit's objective evaluation computed on the idle processors, to the
same bits.)

A few fit intervals over many curves leave most processors idle: the intervals
run side by side, one thread each, and each one's Downhill Simplex spends its
time computing the curves' own values, one curve after another. TFitTask now
shares those out (EvaluationWorkers) and still sums them into the profile in
list order. What that must not change is the answer - the minimizer, the order of
its evaluations and every addition are the serial loop's - and what it must
change is who computes: a model with enough to compute is spread over threads, a
small one, and any one under fitThreads 1, stays on the calling thread.

The threads are told apart by the test Gaussian, which notes the thread that
computed it (test_curve_types.TestGaussThreadsSeen).
}
unit testcase_parallel_evaluation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, fpcunit, testregistry, fpjson,
    fit_rest_api, fit_task, points_set, curve_points_set, test_curve_types,
    curve_types_singleton, int_curve_type_selector, int_fit_service, SimpMath,
    job_pool;

type
    TParallelEvaluationTest = class(TTestCase)
    private
        FApi: TFitRestApi;
        { AWorkers is EvaluationWorkers; ABudget the evaluations the fit may
          spend, so the fit is long enough to matter and always ends. }
        function BuildTask(ACurves: longint; AWorkers: longint;
            ABudget: int64): TFitTask;
        procedure AssertSameBits(const AWhat: string; A, B: TFitTask);
        procedure FitInThread(AIndex: longint);
        procedure CountRun(AIndex: longint);
        function TimeFullEvaluation(ATask: TFitTask): double;
        function Call(const M, P, B: string; out Code: longint): TJSONObject;
        function Num(const AValue: double): string;
        { Three picked Gaussians over a long profile, fitted on AThreads
          (fitThreads); answers every fitted value as text. }
        function FitThreePeaks(AThreads: longint): TStringList;
    private
        FSaturated: array of TFitTask;
        FRuns: array of longint;
    protected
        procedure SetUp; override;
        procedure TearDown; override;
    published
        procedure ParallelEvaluationReachesExactlyTheFitSerialEvaluationDoes;
        procedure SerialEvaluationNeverLeavesTheCallingThread;
        procedure ASmallModelStaysOnTheCallingThread;
        procedure ManyTasksEvaluatingAtOnceAllFinishWithTheSerialAnswer;
        procedure FitThreadsOneAndTheDefaultReachTheSameModelOverRest;
        procedure TheSpeedOfASharedOutEvaluationIsReported;
    end;

implementation

type
    TQuiet = class
        procedure NoOp;
    end;

procedure TQuiet.NoOp;
begin
end;

var
    Quiet: TQuiet;

const
    { Spacing and width of the peaks, and the profile's step: each curve spans
      the whole profile, so a model of N curves has N * samples points to
      compute. }
    SPACING = 2.6;
    SIGMA = 1.5;
    STEP = 0.5;
    { Evaluations a fit of a model of N curves may spend. The simplex builds its
      first vertices by moving ONE parameter each, 3 N of them for N curves, and
      an evaluation of those computes one curve; only after them does a step
      move every parameter and every curve need computing. So a budget below
      about 3 N proves nothing about sharing the curves out. }
    BUDGET_60 = 400;
    BUDGET_4 = 100;

procedure TParallelEvaluationTest.SetUp;
begin
    FApi := TFitRestApi.Create;
end;

procedure TParallelEvaluationTest.TearDown;
var
    i: longint;
begin
    for i := 0 to High(FSaturated) do
        FSaturated[i].Free;
    FSaturated := nil;
    FreeAndNil(FApi);
end;

function TParallelEvaluationTest.BuildTask(ACurves, AWorkers: longint;
    ABudget: int64): TFitTask;
var
    Profile, Positions: TPointsSet;
    Sel: ICurveTypeSelector;
    x, y: double;
    k: longint;
begin
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
    Sel := TCurveTypesSingleton.CreateCurveTypeSelector;
    Sel.SelectCurveType(TTestGaussPointsSet.GetCurveTypeId);
    Profile := TPointsSet.Create(nil);
    Positions := TPointsSet.Create(nil);
    x := 0;
    while x <= 10 + SPACING * ACurves + 10 do
    begin
        y := 0;
        for k := 0 to ACurves - 1 do
            y := y + GaussPoint(100 + (k mod 7), SIGMA, 10 + SPACING * k, x);
        Profile.AddNewPoint(x, y);
        x := x + STEP;
    end;
    //  A pick off each peak, so every curve has somewhere to go.
    for k := 0 to ACurves - 1 do
        Positions.AddNewPoint(10 + SPACING * k + 0.5,
            GaussPoint(100, SIGMA, 10 + SPACING * k, 10 + SPACING * k + 0.5));
    Result := TFitTask.Create(nil, False);
    Result.ServerShowCurMin := @Quiet.NoOp;
    Result.ServerDoneProc := @Quiet.NoOp;
    Result.SetProfilePointsSet(Profile);
    Result.SetCurvePositions(Positions);
    Result.RecreateCurves(nil);
    Result.BegIndex := 0;
    Result.EndIndex := Result.GetCalcProfile.PointsCount - 1;
    Result.MinimizerKind := MIN_KIND_DHS;
    Result.EvaluationBudget := ABudget;
    Result.EvaluationWorkers := AWorkers;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
end;

{ Every fitted value, every calculated sample and the figure of merit, compared
  by their bits - not within a tolerance, because a summation in another order
  is a different number and the fit that follows it a different fit. }
procedure TParallelEvaluationTest.AssertSameBits(const AWhat: string;
    A, B: TFitTask);
var
    i, j: longint;
    CA, CB: TCurvePointsSet;
    X, Y: double;
begin
    AssertEquals(AWhat + ': the same curves', A.GetCurves.Count,
        B.GetCurves.Count);
    AssertEquals(AWhat + ': the same evaluations', A.Evaluations,
        B.Evaluations);
    for i := 0 to A.GetCurves.Count - 1 do
    begin
        CA := TCurvePointsSet(A.GetCurves.Items[i]);
        CB := TCurvePointsSet(B.GetCurves.Items[i]);
        for j := 0 to CA.Parameters.Count - 1 do
        begin
            X := CA.Parameters[j].Value;
            Y := CB.Parameters[j].Value;
            AssertTrue(Format('%s: curve %d parameter %d, bit for bit',
                [AWhat, i, j]), CompareMem(@X, @Y, SizeOf(double)));
        end;
    end;
    for i := 0 to A.GetCalcProfile.PointsCount - 1 do
    begin
        X := A.GetCalcProfile.PointYCoord[i];
        Y := B.GetCalcProfile.PointYCoord[i];
        AssertTrue(Format('%s: calculated sample %d, bit for bit', [AWhat, i]),
            CompareMem(@X, @Y, SizeOf(double)));
    end;
    X := A.GetCurMin;
    Y := B.GetCurMin;
    AssertTrue(AWhat + ': the R-factor, bit for bit',
        CompareMem(@X, @Y, SizeOf(double)));
end;

{ THE CORE PROMISE. 60 curves over 352 samples is 21000 points an evaluation,
  enough for five workers; the fit it reaches must be the serial one's. }
procedure TParallelEvaluationTest.ParallelEvaluationReachesExactlyTheFitSerialEvaluationDoes;
var
    Serial, Parallel: TFitTask;
begin
    Serial := BuildTask(60, 1, BUDGET_60);
    Parallel := BuildTask(60, 64, BUDGET_60);
    try
        ForgetTestGaussThreads;
        Serial.MinimizeDifference;
        AssertEquals('serial: one thread computed', 1, TestGaussThreadsSeen);
        ForgetTestGaussThreads;
        Parallel.MinimizeDifference;
        if LogicalProcessorCount > 1 then
            AssertTrue('parallel: the curves were shared out, over ' +
                IntToStr(TestGaussThreadsSeen) + ' thread(s)',
                TestGaussThreadsSeen > 1);
        AssertTrue('and a fit was made', Serial.Evaluations > 1);
        AssertSameBits('60 curves', Serial, Parallel);
    finally
        Serial.Free;
        Parallel.Free;
    end;
end;

{ fitThreads 1 IS THE OLD LOOP: nothing parallel anywhere, however large. }
procedure TParallelEvaluationTest.SerialEvaluationNeverLeavesTheCallingThread;
var
    Task: TFitTask;
begin
    Task := BuildTask(60, 1, BUDGET_60);
    try
        ForgetTestGaussThreads;
        Task.MinimizeDifference;
        AssertEquals(1, TestGaussThreadsSeen);
    finally
        Task.Free;
    end;
end;

{ Threads cost more than a little work saves: 4 curves over 61 samples is 244
  points an evaluation, under what one extra thread is worth. }
procedure TParallelEvaluationTest.ASmallModelStaysOnTheCallingThread;
var
    Task: TFitTask;
begin
    Task := BuildTask(4, 64, BUDGET_4);
    try
        ForgetTestGaussThreads;
        Task.MinimizeDifference;
        AssertEquals('though it was allowed many', 1, TestGaussThreadsSeen);
    finally
        Task.Free;
    end;
end;

{ Milliseconds of one evaluation with every curve's parameters moved: the least
  of ten trials of three, because the machine is shared and a trial that
  another job disturbed says nothing of this code. }
function TParallelEvaluationTest.TimeFullEvaluation(ATask: TFitTask): double;
var
    Trial, k, i: longint;
    T0: QWord;
    This: double;
begin
    Result := MaxDouble;
    for Trial := 1 to 10 do
    begin
        T0 := GetTickCount64;
        for k := 1 to 3 do
        begin
            for i := 0 to ATask.GetCurves.Count - 1 do
                TCurvePointsSet(ATask.GetCurves.Items[i]).ValuesChanged;
            ATask.ComputeProfile;
        end;
        This := (GetTickCount64 - T0) / 3;
        if This < Result then
            Result := This;
    end;
end;

procedure TParallelEvaluationTest.CountRun(AIndex: longint);
begin
    InterLockedIncrement(FRuns[AIndex]);
end;

procedure TParallelEvaluationTest.FitInThread(AIndex: longint);
begin
    FSaturated[AIndex].MinimizeDifference;
end;

{ THE POOL SATURATED, AND NO DEADLOCK. Each interval is a thread of the pool and
  each asks for more for its own evaluation; the processors have only so many.
  Every task must still end - an inner run takes what is idle, or nothing - and
  with the serial answer. }
procedure TParallelEvaluationTest.ManyTasksEvaluatingAtOnceAllFinishWithTheSerialAnswer;
const
    TASKS = 6;
var
    Reference: TFitTask;
    i: longint;
begin
    Reference := BuildTask(60, 1, BUDGET_60);
    SetLength(FSaturated, TASKS);
    try
        for i := 0 to TASKS - 1 do
            FSaturated[i] := BuildTask(60, 64, BUDGET_60);
        Reference.MinimizeDifference;
        RunJobs(TASKS, TASKS, [], @FitInThread);
        for i := 0 to TASKS - 1 do
            AssertSameBits(Format('task %d', [i]), Reference, FSaturated[i]);
    finally
        Reference.Free;
    end;
end;

function TParallelEvaluationTest.Call(const M, P, B: string;
    out Code: longint): TJSONObject;
var
    Resp: string;
    D: TJSONData;
begin
    FApi.Handle(M, P, B, Code, Resp);
    D := GetJSON(Resp);
    if D is TJSONObject then
        Result := TJSONObject(D)
    else
    begin
        D.Free;
        Result := TJSONObject.Create;
    end;
end;

function TParallelEvaluationTest.Num(const AValue: double): string;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := FloatToStrF(AValue, ffGeneral, 17, 0, FS);
end;

function TParallelEvaluationTest.FitThreePeaks(AThreads: longint): TStringList;
const
    PEAKS = 3;
var
    Code, i, j, Id: longint;
    R: TJSONObject;
    XS, YS: string;
    Y, X: double;
    Curves, Params: TJSONArray;
    P: TJSONObject;
begin
    R := Call('POST', '/problems', '', Code);
    try
        Id := R.Get('id', 0);
    finally
        R.Free;
    end;
    //  The profile of testcase_parallel_fit: 61 samples, three peaks, a ripple
    //  the Gaussians cannot follow and a floor that rises every other sample
    //  - a fit on it ends in seconds. Small, so the sharing-out grain is
    //  lowered by the test (see below).
    XS := '';
    YS := '';
    for i := 0 to 60 do
    begin
        X := i;
        Y := 0;
        for j := 0 to PEAKS - 1 do
            Y := Y + (100 + 3 * Sin(i * 0.7)) * Exp(-Sqr((i - 10 - 20 * j) / 3));
        Y := Y + 0.1 * (i mod 2);
        if i > 0 then
        begin
            XS := XS + ',';
            YS := YS + ',';
        end;
        XS := XS + Num(X);
        YS := YS + Num(Y);
    end;
    Call('PUT', Format('/problems/%d/profile', [Id]),
        Format('{"x":[%s],"y":[%s]}', [XS, YS]), Code).Free;
    R := Call('PUT', Format('/problems/%d/settings', [Id]),
        Format('{"fitThreads":%d,"curveType":"%s"}',
        [AThreads, GUIDToString(TTestGaussPointsSet.GetCurveTypeId)]), Code);
    try
        AssertEquals('settings (' + R.Get('error', '') + ')', 200, Code);
    finally
        R.Free;
    end;
    for j := 0 to PEAKS - 1 do
        Call('POST', Format('/problems/%d/points/positions', [Id]),
            Format('{"x":%s,"y":90}', [Num(11 + 20 * j)]), Code).Free;
    R := Call('POST', Format('/problems/%d/actions/minimize-difference', [Id]),
        '', Code);
    try
        AssertEquals('fitted (' + R.Get('error', '') + ')', 200, Code);
    finally
        R.Free;
    end;
    Result := TStringList.Create;
    R := Call('GET', Format('/problems/%d/curves', [Id]), '', Code);
    try
        Curves := TJSONArray(R.Find('curves'));
        for i := 0 to Curves.Count - 1 do
        begin
            Params := TJSONArray(TJSONObject(Curves.Items[i]).Find('params'));
            for j := 0 to Params.Count - 1 do
            begin
                P := TJSONObject(Params.Items[j]);
                if P.Get('kind', '') = 'text' then
                    Continue;
                Result.Add(Num(P.Get('value', NaN)));
            end;
        end;
    finally
        R.Free;
    end;
end;

{ THROUGH THE SURFACE THE APPLICATION USES: a model big enough to be shared out,
  fitted by the verb the window calls, reaches the same model whatever fitThreads
  says. One interval, so the only parallelism there is is the evaluation's. }
procedure TParallelEvaluationTest.FitThreadsOneAndTheDefaultReachTheSameModelOverRest;
var
    One, Default_: TStringList;
    Threads, Single, Grain, i: longint;
begin
    //  Three curves over 61 samples are 183 points; the real grain would never
    //  share that out, and a fit of a model big enough to be shared out takes
    //  minutes.
    Grain := PointsPerExtraEvaluationWorker;
    PointsPerExtraEvaluationWorker := 90;
    try
        ForgetTestGaussThreads;
        One := FitThreePeaks(1);
        Single := TestGaussThreadsSeen;
        ForgetTestGaussThreads;
        Default_ := FitThreePeaks(0);
        Threads := TestGaussThreadsSeen;
    finally
        PointsPerExtraEvaluationWorker := Grain;
    end;
    try
        AssertEquals('fitThreads 1: one thread computed', 1, Single);
        if LogicalProcessorCount > 1 then
            AssertTrue('the default shared the evaluation out: ' +
                IntToStr(Threads) + ' thread(s)', Threads > 1);
        AssertTrue('a model was fitted', One.Count > 0);
        AssertEquals('as many values', One.Count, Default_.Count);
        for i := 0 to One.Count - 1 do
            AssertEquals(Format('value %d, bit for bit', [i]), One[i],
                Default_[i]);
    finally
        One.Free;
        Default_.Free;
    end;
end;

{ A MEASUREMENT, NOT A GATE: the time depends on the machine and on what else
  it is running, so it is written to the log for whoever wants the number and
  nothing is asserted about it. The model is the size of the opening fit that
  motivated this (150 curves over some hundreds of samples). }
procedure TParallelEvaluationTest.TheSpeedOfASharedOutEvaluationIsReported;
const
    EVALS = 700;
    WORKERS: array[0..4] of longint = (1, 2, 4, 8, 16);
var
    Serial, Parallel: TFitTask;
    T0, TSerial, TParallel, TSpawn: QWord;
    i, k: longint;
    Info: TStringList;
begin
    //  What making and joining the threads of one evaluation costs by itself:
    //  1000 runs of 16 jobs that do nothing.
    //  The machine is shared; what else it was doing is part of the figure.
    if FileExists('/proc/loadavg') then
    begin
        Info := TStringList.Create;
        try
            Info.LoadFromFile('/proc/loadavg');
            WriteLn('SPEED load average before: ', Info[0]);
        finally
            Info.Free;
        end;
    end;
    StopRecordingTestGaussThreads;
    SetLength(FRuns, 16);
    T0 := GetTickCount64;
    for i := 1 to 1000 do
        RunJobsOnIdleProcessors(16, 16, @CountRun);
    TSpawn := GetTickCount64 - T0;
    WriteLn(Format('SPEED an empty run of 16 jobs on up to 16 workers: %.3f ms',
        [TSpawn / 1000]));
    Serial := BuildTask(150, 1, EVALS);
    Parallel := BuildTask(150, 64, EVALS);
    try
        for k := 0 to High(WORKERS) do
        begin
            i := WORKERS[k];
            Parallel.EvaluationWorkers := i;
            WriteLn(Format('SPEED one full evaluation, all curves moved, on ' +
                'up to %d workers: %.2f ms', [i, TimeFullEvaluation(Parallel)]));
        end;
        Parallel.EvaluationWorkers := 64;
        T0 := GetTickCount64;
        Serial.MinimizeDifference;
        TSerial := GetTickCount64 - T0;
        T0 := GetTickCount64;
        Parallel.MinimizeDifference;
        TParallel := GetTickCount64 - T0;
        WriteLn(Format('SPEED 150 curves x %d samples, %d evaluations: serial ' +
            '%d ms (%.1f evaluations/s), shared out %d ms (%.1f evaluations/s) ' +
            'on %d processors', [Serial.GetCalcProfile.PointsCount,
            Serial.Evaluations, TSerial, 1000 * Serial.Evaluations / Max(1, TSerial),
            TParallel, 1000 * Parallel.Evaluations / Max(1, TParallel),
            LogicalProcessorCount]));
        AssertSameBits('150 curves', Serial, Parallel);
    finally
        Serial.Free;
        Parallel.Free;
    end;
end;

initialization
    Quiet := TQuiet.Create;
    //  INTEGRATION: it runs the optimiser.
    RegisterTest('integration', TParallelEvaluationTest);
finalization
    Quiet.Free;
end.
