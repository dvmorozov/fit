{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class executing optimization task in separate thread.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_task_with_thread;

interface

uses
    Classes, fit_task, log, main_calc_thread, SysUtils;

type
    { Executes task solution in separate thread. }
    TFitTaskWithThread = class(TFitTask)
    protected
        FMainCalcThread: TMainCalcThread;
        FDoneDisabled:   boolean;     //  Suppresses calling Done.

        procedure RecreateMainCalcThread(ATask: TThreadMethod;
            AAllDone: TThreadMethod);

    public
        { Waits for thread termination. }
        procedure DestroyMainCalcThread;

        procedure ShowCurMin; override;
        function GetCurMinInitialized: boolean; override;
        procedure Done; override;

        { Methods implement synchronization to work in multithreading environment. }

        function GetCurMin: double; override;
        function GetCurAbsMin: double; override;
        function GetCurSqrMin: double; override;
        function GetAllDone: boolean; override;

        {TODO: methods should be implemented. }

        procedure ShowCurMinSync;
        procedure ShowProfileSync;
        procedure DoneProcSync;
        procedure FindPeakBoundsDoneSync;
        procedure FindBackPointsDoneSync;
        procedure FindPeakPositionsDoneSync;

        constructor Create(AOwner: TComponent;
            AEnableBackgroundVariation: boolean;
            ACurveScalingEnabled: boolean); override;
        destructor Destroy; override;
        { Sets up termination flags and waits for
          actual termination of the thread. }
        procedure AbortAsyncOper;
        { Sets up termination flags and returns. The method doesn't wait for
          actual termination of the thread. }
        procedure StopAsyncOper; override;

        { Asynchronous long-term operations. }

        { Fits curves starting from given parameter set (initially or repeatedly). }
        procedure FindGausses; override;
        procedure FindGaussesAgain; override;
        { Searches set of pattern curves fitting exprerimental data with given accuracy
          sequentially decreasing number of such curves. }
        procedure MinimizeNumberOfCurves; override;
        property DoneDisabled: boolean read FDoneDisabled write FDoneDisabled;
    end;

implementation

uses
    app;

{$warnings off}
procedure TFitTaskWithThread.RecreateMainCalcThread(ATask: TThreadMethod;
    AAllDone: TThreadMethod);
begin
    if Assigned(FMainCalcThread) then
        AbortAsyncOper;

    FAllDone     := False;
    DoneDisabled := False;

    FMainCalcThread := TMainCalcThread.Create(True { CreateSuspended });
    if Assigned(FMainCalcThread.FatalException) then
        raise FMainCalcThread.FatalException;

    { Assignment of callbacks. }
    FMainCalcThread.SetSyncMethods(
        ATask, ShowCurMinSync, ShowProfileSync, DoneProcSync,
        FindPeakBoundsDoneSync, FindBackPointsDoneSync, FindPeakPositionsDoneSync,
        AAllDone);
    { Start thread. }
    FMainCalcThread.Resume;
end;

{$warnings on}

procedure TFitTaskWithThread.DestroyMainCalcThread;
begin
    if Assigned(FMainCalcThread) then
    begin
        FMainCalcThread.Terminate;
        FMainCalcThread.WaitFor;
        FMainCalcThread.Free;
        FMainCalcThread := nil;
    end;
end;

procedure TFitTaskWithThread.MinimizeNumberOfCurves;
begin
    RecreateMainCalcThread(FindGaussesSequentiallyAlg, Done);
end;

procedure TFitTaskWithThread.FindGaussesAgain;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyani
    // povtornaya initsializatsiya gaussianov
    RecreateCurveInstances(nil);
    CalculateProfile;
    RecreateMainCalcThread(Optimization, Done);
end;

procedure TFitTaskWithThread.FindGausses;
begin
    RecreateMainCalcThread(Optimization, Done);
end;

procedure TFitTaskWithThread.AbortAsyncOper;
begin
    DoneDisabled := True;
    StopAsyncOper;
    DestroyMainCalcThread;
end;

procedure TFitTaskWithThread.StopAsyncOper;
begin
    inherited;
    if Assigned(FMainCalcThread) then
        FMainCalcThread.Terminate;
end;

procedure TFitTaskWithThread.Done;
begin
    FMainCalcThread.Done;
end;

procedure TFitTaskWithThread.ShowCurMin;
begin
    FMainCalcThread.ShowCurMin;
end;

function TFitTaskWithThread.GetCurMin: double;
begin
    Result := FCurMin;
end;

function TFitTaskWithThread.GetCurAbsMin: double;
begin
    Result := FCurAbsMin;
end;

function TFitTaskWithThread.GetCurSqrMin: double;
begin
    Result := FCurSqrMin;
end;

function TFitTaskWithThread.GetAllDone: boolean;
begin
    Result := FAllDone;
end;

function TFitTaskWithThread.GetCurMinInitialized: boolean;
begin
    Result := FCurMinInitialized;
end;

constructor TFitTaskWithThread.Create(AOwner: TComponent;
    AEnableBackgroundVariation: boolean; ACurveScalingEnabled: boolean);
begin
    inherited;
end;

destructor TFitTaskWithThread.Destroy;
begin
    if Assigned(FMainCalcThread) then
        AbortAsyncOper;
    inherited;
end;

procedure TFitTaskWithThread.ShowCurMinSync;
begin
    inherited ShowCurMin;
end;

procedure TFitTaskWithThread.ShowProfileSync;
begin

end;

procedure TFitTaskWithThread.DoneProcSync;
begin
    FAllDone := True;
    if not DoneDisabled then
        ServerDoneProc;
end;

procedure TFitTaskWithThread.FindPeakBoundsDoneSync;
begin

end;

procedure TFitTaskWithThread.FindBackPointsDoneSync;
begin

end;

procedure TFitTaskWithThread.FindPeakPositionsDoneSync;
begin

end;

end.
