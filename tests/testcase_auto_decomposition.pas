// SPDX-License-Identifier: GPL-3.0-or-later
{ Automatic decomposition of the twin-peaks profile, Data/2.dat, the way the
  application runs it: background subtracted, a position on every peak point,
  2-branch Pseudo-Voigt, maximum acceptable R-factor 0.01%. }
unit testcase_auto_decomposition;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math, DateUtils, fpcunit, testregistry,
    fit_service, fit_task, dat_file_loader, title_points_set,
    two_branches_pseudo_voigt_points_set, int_fit_service;

type
    { Runs the automatic algorithm on this thread instead of the calculation
      thread, so the test sees the result without polling. }
    TSyncFitService = class(TFitService)
    public
        procedure RunAutomatically;
        function CurveCount: longint;
        function RFactor: double;
        function FirstProfileValue: double;
    end;

    TAutoDecompositionTest = class(TTestCase)
    private
        function LoadedTwinPeaks: TSyncFitService;
        procedure AssertDecomposedWell(ASvc: TSyncFitService);
    published
        procedure TwinPeaksDecomposesIntoAFewCurvesQuickly;
        procedure MarkingAnIntervalFirstDoesNotKeepTheBackground;
    end;

implementation

procedure TSyncFitService.RunAutomatically;
begin
    SetState(AsyncOperation);
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

    WriteLn(StdErr, Format('  auto decomposition of 2.dat: %d curves, ' +
        'R-factor %.4g, %.1f s', [ASvc.CurveCount, ASvc.RFactor, Seconds]));
    AssertTrue(Format('%d curves remain', [ASvc.CurveCount]),
        ASvc.CurveCount <= 10);
    AssertTrue(Format('R-factor %.4g', [ASvc.RFactor]),
        ASvc.RFactor <= 0.0001);
    AssertTrue(Format('took %.1f s', [Seconds]), Seconds < 120);
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
  the automatic run used to read as "the background is already subtracted". It
  then fitted the 780-count baseline of 2.dat with curves, one on every sample -
  which is how a project with its intervals saved came back as 42 curves. }
procedure TAutoDecompositionTest.MarkingAnIntervalFirstDoesNotKeepTheBackground;
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
        AssertTrue(Format('the profile still starts at %.0f',
            [Svc.FirstProfileValue]), Svc.FirstProfileValue < 100);
    finally
        Svc.Free;
    end;
end;

initialization
    RegisterTest('integration', TAutoDecompositionTest);
end.
