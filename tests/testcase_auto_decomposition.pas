// SPDX-License-Identifier: GPL-3.0-or-later
{ Automatic decomposition of the twin-peaks profile, Data/2.dat, the way the
  application runs it: a background curve put under the peaks (the profile is
  no longer subtracted), a position on every peak point, 2-branch Pseudo-Voigt,
  maximum acceptable R-factor 0.01%. }
unit testcase_auto_decomposition;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, DateUtils, fpcunit, testregistry, points_set,
    self_copied_component,
    fit_service, fit_task, dat_file_loader, title_points_set,
    two_branches_pseudo_voigt_points_set, int_fit_service, named_points_set;

type
    { Runs the automatic algorithm on this thread instead of the calculation
      thread, so the test sees the result without polling. }
    TSyncFitService = class(TFitService)
    public
        procedure RunAutomatically;
        { The automatic run with Stop already pressed - as the server sees one
          that is stopped before its tasks begin: every task is built stopped. }
        procedure RunAutomaticallyStopped;
        function CurveCount: longint;
        { The curves that are peaks, and those that are the background. }
        function PeakCount: longint;
        function BackgroundCount: longint;
        function RFactor: double;
        function FirstProfileValue: double;
    end;

    TAutoDecompositionTest = class(TTestCase)
    private
        function LoadedTwinPeaks: TSyncFitService;
        procedure AssertDecomposedWell(ASvc: TSyncFitService);
    published
        procedure TwinPeaksDecomposesIntoAFewCurvesQuickly;
        procedure MarkingAnIntervalFirstStillFindsTheBackground;
        procedure AnIntervalWithNoCurveInItIsLeftEmptyNotFitted;
        procedure AStoppedAutomaticRunStartsFromTheModelItFound;
        procedure OneCurveIsReadWithoutCopyingTheModel;
    end;

implementation

procedure TSyncFitService.RunAutomatically;
begin
    SetState(AsyncOperation);
    DoAllAutomaticallyAlg;
end;

procedure TSyncFitService.RunAutomaticallyStopped;
begin
    SetState(AsyncOperation);
    //  Through the verb the stop route calls.
    StopAsyncOper;
    DoAllAutomaticallyAlg;
end;

function TSyncFitService.CurveCount: longint;
var
    i: longint;
begin
    Result := 0;
    for i := 0 to FTaskList.Count - 1 do
        Inc(Result, TFitTask(FTaskList.Items[i]).GetCurves.Count);
end;

function TSyncFitService.PeakCount: longint;
begin
    Result := CurveCount - BackgroundCount;
end;

function TSyncFitService.BackgroundCount: longint;
var
    i, j: longint;
begin
    Result := 0;
    for i := 0 to FTaskList.Count - 1 do
        for j := 0 to TFitTask(FTaskList.Items[i]).GetCurves.Count - 1 do
            if TNamedPointsSet(TFitTask(FTaskList.Items[i]).GetCurves.Items[j]).
                IsBackground then
                Inc(Result);
end;

function TSyncFitService.RFactor: double;
begin
    Result := GetTotalRFactor;
end;

function TSyncFitService.FirstProfileValue: double;
begin
    Result := FExpProfile.PointYCoord[0];
end;

function DataDir: string;
begin
    Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
        DirectorySeparator + 'Data' + DirectorySeparator);
end;

function TAutoDecompositionTest.LoadedTwinPeaks: TSyncFitService;
var
    Loader: TDATFileLoader;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    Result := TSyncFitService.Create;
    Loader := TDATFileLoader.Create(nil);
    try
        Loader.LoadDataSet(DataDir + '2.dat');
        Result.SetProfilePointsSet(Loader.GetPointsSetCopy);
    finally
        Loader.Free;
    end;
    Result.SetCurveType(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
    Result.MaxRFactor := 0.0001;
end;

{ A handful of curves for two peaks, in the time the algorithm took when it was
  written - not the forty-two curves it left on 2026-09-17. }
procedure TAutoDecompositionTest.AssertDecomposedWell(ASvc: TSyncFitService);
var
    Started: TDateTime;
    Seconds: double;
begin
    Started := Now;
    ASvc.RunAutomatically;
    Seconds := MilliSecondsBetween(Now, Started) / 1000;

    WriteLn(StdErr, Format('  auto decomposition of 2.dat: %d peaks and %d ' +
        'background, R-factor %.4g, %.1f s', [ASvc.PeakCount, ASvc.BackgroundCount,
        ASvc.RFactor, Seconds]));
    AssertTrue(Format('%d peaks remain', [ASvc.PeakCount]),
        ASvc.PeakCount <= 10);
    //  THE BASELINE IS A CURVE NOW, not a subtraction: one under the peaks of
    //  every interval the run marked.
    AssertTrue('the baseline is carried by a background curve',
        ASvc.BackgroundCount >= 1);
    AssertTrue(Format('R-factor %.4g', [ASvc.RFactor]),
        ASvc.RFactor <= 0.0001);
    //  150 s, raised from 120 on 2026-09-26: the run now ends with one more fit
    //  than it did - the surviving peaks and the background curve together over
    //  the measured profile - on top of the decomposition, which takes what it
    //  always took (about 100 s here). 120 left no room for a busy machine.
    //  300 s, raised again the same day: the run takes 114-130 s alone and went
    //  to 150.5 s beside the other suites running in parallel. What this bound
    //  exists to catch is an order of magnitude - the background left free
    //  during the reduction took 446 s, the strict reduction figure 2365 s - and
    //  300 still catches both without failing on a loaded machine.
    AssertTrue(Format('took %.1f s', [Seconds]), Seconds < 300);
end;

procedure TAutoDecompositionTest.TwinPeaksDecomposesIntoAFewCurvesQuickly;
var
    Svc: TSyncFitService;
begin
    Svc := LoadedTwinPeaks;
    try
        AssertDecomposedWell(Svc);
    finally
        Svc.Free;
    end;
end;

{ Marking a fit interval moves the service out of BackNotRemoved, which is what
  the automatic run once read as "the background is already subtracted". It
  then fitted the 780-count baseline of 2.dat with curves, one on every sample -
  which is how a project with its intervals saved came back as 42 curves.

  THE EXPECTATION CHANGED ON PURPOSE (2026-09-26): the run no longer subtracts
  anything. The profile keeps its 780-count baseline, and a background curve
  carries it - so what this now guards is that the baseline does not become
  peaks, which was the substance of the old failure. }
procedure TAutoDecompositionTest.MarkingAnIntervalFirstStillFindsTheBackground;
var
    Svc: TSyncFitService;
    Bounds: TTitlePointsSet;
begin
    Svc := LoadedTwinPeaks;
    try
        Bounds := TTitlePointsSet.Create(nil);
        Bounds.AddNewPoint(116, 773);
        Bounds.AddNewPoint(121, 745);
        Svc.SetRFactorBounds(Bounds);

        AssertDecomposedWell(Svc);
        AssertTrue(Format('the profile is as measured: it starts at %.0f',
            [Svc.FirstProfileValue]), Svc.FirstProfileValue > 700);
    finally
        Svc.Free;
    end;
end;

{ THE AUTOMATIC RUN KEEPS THE USER'S CURVE POSITIONS and finds its own fit
  intervals, so an interval can hold no curve at all. Its task was handed to
  the optimiser anyway, which asked for a parameter of a curve that did not
  exist: "a task must have built its curves before their parameters are read",
  and the whole run refused. Found by running Fit > Automatically over a model
  that already had picks. }
procedure TAutoDecompositionTest.AnIntervalWithNoCurveInItIsLeftEmptyNotFitted;
var
    Svc: TSyncFitService;
    Loader: TDATFileLoader;
    Data: TTitlePointsSet;
    Picks: TPointsSet;
    i, Top: longint;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    Svc := TSyncFitService.Create;
    Loader := TDATFileLoader.Create(nil);
    try
        Loader.LoadDataSet(DataDir + '1.dat');
        Data := Loader.GetPointsSetCopy;
        try
            Svc.SetProfilePointsSet(Data);
            //  One pick, on the highest sample: every other interval the run
            //  finds holds none.
            Top := 0;
            for i := 1 to Data.PointsCount - 1 do
                if Data.PointYCoord[i] > Data.PointYCoord[Top] then
                    Top := i;
            Picks := TPointsSet.Create(nil);
            Picks.AddNewPoint(Data.PointXCoord[Top], Data.PointYCoord[Top]);
        finally
            Data.Free;
        end;
        Svc.SetCurveType(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
        Svc.MaxRFactor := 0.0001;
        Svc.SetCurvePositions(Picks);

        Svc.RunAutomatically;
        AssertTrue('the run built intervals', Svc.FTaskList.Count > 1);
        AssertTrue(Format('the picked curve is there: %d peak(s)',
            [Svc.PeakCount]), Svc.PeakCount >= 1);
        AssertTrue('and the model, empty intervals included, reports its ' +
            'R-factor', Svc.GetRFactorStr <> RFactorStillNotCalculated);
    finally
        Loader.Free;
        Svc.Free;
    end;
end;

{ RUNS ARE INCREMENTAL, THE AUTOMATIC ONE TOO. It rebuilds its fit intervals every
  time it starts - and that must not rebuild the curves from their seeds: each is
  restored by its handle. So a run stopped before it fits anything leaves the
  model exactly as it found it. Compared as a whole model, by its R-factor over
  the profile as measured, which is the same objective both times. }
procedure TAutoDecompositionTest.AStoppedAutomaticRunStartsFromTheModelItFound;
var
    Svc: TSyncFitService;
    Loader: TDATFileLoader;
    Data: TTitlePointsSet;
    Picks: TPointsSet;
    i, Top: longint;
    Found, Left: double;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    Svc := TSyncFitService.Create;
    Loader := TDATFileLoader.Create(nil);
    try
        Loader.LoadDataSet(DataDir + '1.dat');
        Data := Loader.GetPointsSetCopy;
        try
            Svc.SetProfilePointsSet(Data);
            Top := 0;
            for i := 1 to Data.PointsCount - 1 do
                if Data.PointYCoord[i] > Data.PointYCoord[Top] then
                    Top := i;
            Picks := TPointsSet.Create(nil);
            Picks.AddNewPoint(Data.PointXCoord[Top], Data.PointYCoord[Top]);
        finally
            Data.Free;
        end;
        Svc.SetCurveType(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
        Svc.MaxRFactor := 0.0001;
        Svc.SetCurvePositions(Picks);

        Svc.RunAutomatically;
        Found := Svc.RFactor;
        Svc.RunAutomaticallyStopped;
        Left := Svc.RFactor;
        AssertEquals('the stopped run left the model it found', Found, Left,
            Found * 1E-9);
    finally
        Loader.Free;
        Svc.Free;
    end;
end;

{ ONE CURVE, READ ON ITS OWN. The points route copied every curve in the model to
  answer for one - quadratic in the model, 5 ms a request over 528 curves, and
  most of the five seconds a window waited after Stop left that many. What a
  curve reads on its own must be exactly what it reads in the whole copy. }
procedure TAutoDecompositionTest.OneCurveIsReadWithoutCopyingTheModel;
var
    Svc: TSyncFitService;
    Loader: TDATFileLoader;
    Data, One: TTitlePointsSet;
    All: TSelfCopiedCompList;
    Picks: TPointsSet;
    i, j, Top: longint;
    Whole: TNamedPointsSet;
begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
        exUnderflow, exPrecision]);
    Svc := TSyncFitService.Create;
    Loader := TDATFileLoader.Create(nil);
    try
        Loader.LoadDataSet(DataDir + '1.dat');
        Data := Loader.GetPointsSetCopy;
        try
            Svc.SetProfilePointsSet(Data);
            Top := 0;
            for i := 1 to Data.PointsCount - 1 do
                if Data.PointYCoord[i] > Data.PointYCoord[Top] then
                    Top := i;
            Picks := TPointsSet.Create(nil);
            Picks.AddNewPoint(Data.PointXCoord[Top], Data.PointYCoord[Top]);
        finally
            Data.Free;
        end;
        Svc.SetCurveType(T2BranchesPseudoVoigtPointsSet.GetCurveTypeId);
        Svc.MaxRFactor := 0.0001;
        Svc.SetCurvePositions(Picks);
        Svc.RunAutomatically;

        All := Svc.GetCurves;
        try
            AssertTrue('there are curves to read', All.Count > 0);
            for i := 0 to All.Count - 1 do
            begin
                Whole := TNamedPointsSet(All.Items[i]);
                One := Svc.CurvePointsCopy(i);
                try
                    AssertTrue('curve ' + IntToStr(i) + ' is read', Assigned(One));
                    AssertEquals('its title', Whole.GetCurveTypeName, One.FTitle);
                    AssertEquals('its point count', Whole.PointsCount,
                        One.PointsCount);
                    for j := 0 to Whole.PointsCount - 1 do
                    begin
                        AssertEquals('x', Whole.PointXCoord[j], One.PointXCoord[j]);
                        AssertEquals('y', Whole.PointYCoord[j], One.PointYCoord[j]);
                    end;
                finally
                    One.Free;
                end;
            end;
            AssertTrue('past the end reads nothing',
                Svc.CurvePointsCopy(All.Count) = nil);
        finally
            All.Free;
        end;
    finally
        Loader.Free;
        Svc.Free;
    end;
end;

initialization
    RegisterTest('integration', TAutoDecompositionTest);
end.
