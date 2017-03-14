unit FitServerMultithreaded;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, FitTask, FitTaskWithThread, FitServerWithThread,
    FitServer, MyExceptions, CommonTypes;

type
    //  sobstvennyy potok nuzhen dlya zapuska
    TFitServerMultithreaded = class(TFitServerWithThread)
    protected
        function CreateTaskObject: TFitTask; override;
        //  !!! vypolnyayutsya v sobstvennom potoke,
        //  ne vyzyvayut inherited !!!
        procedure FindGaussesSequentiallyAlg; override;
        procedure FindGaussesAlg; override;
        procedure FindGaussesAgainAlg; override;

    public
        procedure AbortAsyncOper; override;
    end;
    
implementation

function TFitServerMultithreaded.CreateTaskObject: TFitTask;
begin
    Result := TFitTaskWithThread.Create(nil);
end;

procedure TFitServerMultithreaded.AbortAsyncOper;
var i: LongInt;
    FT: TFitTaskWithThread;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(MainCalcThread));
    //  bolee optimal'naya realizatsiya
    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTaskWithThread(TaskList.Items[i]);
        FT.DoneDisabled := True;
        FT.StopAsyncOper;
    end;
    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTaskWithThread(TaskList.Items[i]);
        FT.DestroyMainCalcThread;
    end;

    MainCalcThread.Terminate;
    DestroyMainCalcThread;
    FState := SavedState;
end;

procedure TFitServerMultithreaded.FindGaussesSequentiallyAlg;
var i: LongInt;
    FT: TFitTask;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CreateTasks;
    InitTasks;
    //??? eto budet rabotat' tol'ko poka net obschey Sigma
    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTask(TaskList.Items[i]);
        FT.FindGaussesSequentially;
    end;
end;

procedure TFitServerMultithreaded.FindGaussesAlg;
var i: LongInt;
    FT: TFitTask;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyaniya
    CreateTasks;
    InitTasks;
    //??? eto budet rabotat' tol'ko poka net obschey Sigma
    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTask(TaskList.Items[i]);
        FT.FindGausses;
    end;
end;

procedure TFitServerMultithreaded.FindGaussesAgainAlg;
var i: LongInt;
    FT: TFitTask;
begin
    Assert(Assigned(TaskList));
    //??? eto budet rabotat' tol'ko poka net obschey Sigma
    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTask(TaskList.Items[i]);
        FT.FindGaussesAgain;
    end;
end;

end.



