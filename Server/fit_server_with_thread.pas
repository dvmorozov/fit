{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of server component performing long-term operation in separate thread.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_server_with_thread;

interface

uses
    Classes, SysUtils, fit_server, common_types, main_calc_thread, MyExceptions,
    fit_task;

type
    { The server component performing long-term operation in separate thread. }
    TFitServerWithThread = class(TFitServer)
    protected
        { Pointer to the thread performing long-term method of the component. 
          It is used for synchronization with main thread of the application.
          If this pointer is not Nil then displaying methods are called
          synchronously. }
        FMainCalcThread: TMainCalcThread;
        { Temporarily saved value of current minimum. }
        FCurMin: Double;

        procedure RecreateMainCalcThread(
            ATask: TThreadMethod; AAllDone: TThreadMethod); override;
        { Waits for completion of the thread. Do not call from synchonized
          method, otherwise this will result in deadlock. }
        procedure DestroyMainCalcThread;
        
        function CreateTaskObject: TFitTask; override;

        { IClientCallback synchronized counterparts }
        { These methods are called synchronously from the worker thread.
          They can't have parameters and call inherited methods to perform
          actual processing. }

        procedure ShowCurMinSync;
        procedure ShowProfileSync;
        procedure DoneSync;
        procedure FindPeakBoundsDoneSync;
        procedure FindBackPointsDoneSync;
        procedure FindPeakPositionsDoneSync;

    public
        destructor Destroy; override;

        { IClientCallback }
        { All methods must be called synchronously with main application thread,
          because they update UI. These methods must not call inherited ones,
          instead call synchronized counterparts of FMainCalcThread. }

        procedure ShowCurMin(Min: Double); override;
        procedure ShowProfile; override;
        procedure Done; override;
        procedure FindPeakBoundsDone; override;
        procedure FindBackPointsDone; override;
        procedure FindPeakPositionsDone; override;

        { Control commands. }

        { Asynchronous termination of long-term operation with
          calling termination method. }
        procedure StopAsyncOper; override;
        { Synchronous termination of long-term operation without
          calling termination method. }
        procedure AbortAsyncOper; override;
    end;

implementation

destructor TFitServerWithThread.Destroy;
begin
    if State = AsyncOperation then AbortAsyncOper;
    inherited;
end;

{$warnings off}
procedure TFitServerWithThread.RecreateMainCalcThread(
    ATask: TThreadMethod; AAllDone: TThreadMethod);
begin
    if State = AsyncOperation then AbortAsyncOper;
    DoneDisabled := False;

    Assert(not Assigned(FMainCalcThread));

    FMainCalcThread := TMainCalcThread.Create(True { CreateSuspended });
    if Assigned(FMainCalcThread.FatalException) then
       raise FMainCalcThread.FatalException;

    { Assigns callbacks. }
    FMainCalcThread.SetSyncMethods(
        ATask, ShowCurMinSync, ShowProfileSync, DoneSync,
        FindPeakBoundsDoneSync, FindBackPointsDoneSync, FindPeakPositionsDoneSync,
        AAllDone);
    { Sets appropriate state befor starting thread. }
    SetState(AsyncOperation);
    { Starts thread. }
    FMainCalcThread.Resume;
end;
{$warnings on}

procedure TFitServerWithThread.DestroyMainCalcThread;
begin
    if Assigned(FMainCalcThread) then
    begin
        FMainCalcThread.Terminate;
        FMainCalcThread.WaitFor;
        FMainCalcThread.Free;
        FMainCalcThread := nil;
    end;
end;

procedure TFitServerWithThread.AbortAsyncOper;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(FMainCalcThread));

    DoneDisabled := True;

    FMainCalcThread.Terminate;
    DestroyMainCalcThread;
    FState := SavedState;
end;

procedure TFitServerWithThread.StopAsyncOper;
var
    i: LongInt;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(FMainCalcThread));

    for i := 0 to TaskList.Count - 1 do
    begin
        TFitTask(TaskList.Items[i]).StopAsyncOper;
    end;

    FMainCalcThread.Terminate;
end;

function TFitServerWithThread.CreateTaskObject: TFitTask;
begin
    Result := TFitTask.Create(nil,
        FBackgroundVariationEnabled, FCurveScalingEnabled);
end;

procedure TFitServerWithThread.ShowCurMin(Min: Double);
begin
    FCurMin := Min;
    FMainCalcThread.ShowCurMin;
end;

procedure TFitServerWithThread.ShowProfile;
begin
    FMainCalcThread.ShowProfile;
end;

procedure TFitServerWithThread.Done;
begin
    FMainCalcThread.Done;
end;

procedure TFitServerWithThread.FindPeakBoundsDone;
begin
    FMainCalcThread.FindPeakBoundsDone;
end;

procedure TFitServerWithThread.FindBackPointsDone;
begin
    FMainCalcThread.FindBackPointsDone;
end;

procedure TFitServerWithThread.FindPeakPositionsDone;
begin
    FMainCalcThread.FindPeakPositionsDone;
end;

procedure TFitServerWithThread.ShowCurMinSync;
begin
    inherited ShowCurMin(FCurMin);
end;

procedure TFitServerWithThread.ShowProfileSync;
begin
    inherited ShowProfile;
end;

procedure TFitServerWithThread.DoneSync;
begin
    inherited Done;
end;

procedure TFitServerWithThread.FindPeakBoundsDoneSync;
begin
    inherited FindPeakBoundsDone;
end;

procedure TFitServerWithThread.FindBackPointsDoneSync;
begin
    inherited FindBackPointsDone;
end;

procedure TFitServerWithThread.FindPeakPositionsDoneSync;
begin
    inherited FindPeakPositionsDone;
end;

end.



