{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of multithreaded version of the server component.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}

unit fit_server_multithreaded;

interface

uses fit_server, fit_server_with_thread,
    fit_task, fit_task_with_thread, int_fit_service, MyExceptions, SysUtils;

type
    { Executes algorithms in separate threads. }
    TFitServerMultithreaded = class(TFitServerWithThread)
    protected
        function CreateTaskObject: TFitTask; override;

        { Algorithms are executed in separate threads. }

        procedure MinimizeNumberOfCurvesAlg; override;
        procedure MinimizeDifferenceAlg; override;
        procedure MinimizeDifferenceAgainAlg; override;

    public
        procedure AbortAsyncOper; override;
    end;

implementation

function TFitServerMultithreaded.CreateTaskObject: TFitTask;
begin
    Result := TFitTaskWithThread.Create(nil, FBackgroundVariationEnabled,
        FCurveScalingEnabled);
end;

procedure TFitServerMultithreaded.AbortAsyncOper;
var
    i: longint;
    FitTask: TFitTaskWithThread;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(FTaskList));
    Assert(Assigned(FMainCalcThread));
    //  bolee optimal'naya realizatsiya
    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask := TFitTaskWithThread(FTaskList.Items[i]);
        FitTask.DoneDisabled := True;
        FitTask.StopAsyncOper;
    end;
    for i := 0 to FTaskList.Count - 1 do
    begin
        FitTask := TFitTaskWithThread(FTaskList.Items[i]);
        FitTask.DestroyMainCalcThread;
    end;

    FMainCalcThread.Terminate;
    DestroyMainCalcThread;
    FState := FSavedState;
end;

procedure TFitServerMultithreaded.MinimizeNumberOfCurvesAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CreateTasks;
    InitTasks;
    //??? eto budet rabotat' tol'ko poka net obschey Sigma
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeNumberOfCurves;
    end;
end;

procedure TFitServerMultithreaded.MinimizeDifferenceAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CreateTasks;
    InitTasks;
    //??? eto budet rabotat' tol'ko poka net obschey Sigma
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeDifference;
    end;
end;

procedure TFitServerMultithreaded.MinimizeDifferenceAgainAlg;
var
    i:  longint;
    FT: TFitTask;
begin
    Assert(Assigned(FTaskList));
    //??? eto budet rabotat' tol'ko poka net obschey Sigma
    for i := 0 to FTaskList.Count - 1 do
    begin
        FT := TFitTask(FTaskList.Items[i]);
        FT.MinimizeDifferenceAgain;
    end;
end;

end.
