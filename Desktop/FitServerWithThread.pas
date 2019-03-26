{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of server component performing long-term operation in separate thread.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit FitServerWithThread;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses SysUtils, FitServer, CommonTypes, MainCalcThread, MyExceptions,
    FitTask;

type
    { The server component performing long-term operation in separate thread. }
    TFitServerWithThread = class(TFitServer)
    protected
        { Pointer to the thread performing long-term method of the component. 
          It is used for synchronization with main thread of the application.
          If this pointer is not Nil then displaying methods are called synchronously. }
        MainCalcThread: TMainCalcThread;

        procedure RecreateMainCalcThread(
            ACurrentTask: TCurrentTask; ADoneProc: TDoneProc); override;
        { Waits for completion of the thread. Do not call from synchonized method - 
          this will result in deadlock. }
        procedure DestroyMainCalcThread;
        
        function CreateTaskObject: TFitTask; override;

    public
        destructor Destroy; override;
        
        { Control commands. }

        { Asynchronous termination of long-term operation with calling termination method. }
        procedure StopAsyncOper; override;
        { Synchronous termination of long-term operation without calling termination method. }
        procedure AbortAsyncOper; override;
    end;

implementation

destructor TFitServerWithThread.Destroy;
begin
    if State = AsyncOperation then AbortAsyncOper;
    inherited;
end;

procedure TFitServerWithThread.RecreateMainCalcThread(
    ACurrentTask: TCurrentTask; ADoneProc: TDoneProc);
begin
    if State = AsyncOperation then AbortAsyncOper;
    DoneDisabled := False;
    
    MainCalcThread := TMainCalcThread.Create(True (* CreateSuspended *));
    //  tak rekomenduet rukovodstvo
    if Assigned(MainCalcThread.FatalException) then
       raise MainCalcThread.FatalException;
    //  prisoedinenie zadachi
    MainCalcThread.SetCurrentTask(ACurrentTask);
    MainCalcThread.SetDoneProc(ADoneProc);
    //  ustanovka sostoyaniya d.b. do zapuska potoka
    SetState(AsyncOperation);
    //  zapusk potoka resheniya zadachi
    MainCalcThread.Resume;
end;

procedure TFitServerWithThread.DestroyMainCalcThread;
begin
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    (*
    try
        Assert(Assigned(MainCalcThread));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Surprising)
        else raise;
    end;
    *)
    if Assigned(MainCalcThread) then
    begin
        MainCalcThread.Terminate;
        MainCalcThread.WaitFor;
        MainCalcThread.Free;
        MainCalcThread := nil;
    end;
end;

procedure TFitServerWithThread.AbortAsyncOper;
var i: LongInt;
    FT: TFitTask;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(MainCalcThread));

    DoneDisabled := True;

    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTask(TaskList.Items[i]);
        FT.AbortAsyncOper;
    end;

    MainCalcThread.Terminate;
    DestroyMainCalcThread;
    FState := SavedState;
end;

procedure TFitServerWithThread.StopAsyncOper;
var i: LongInt;
    FT: TFitTask;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(MainCalcThread));
    
    for i := 0 to TaskList.Count - 1 do
    begin
        FT := TFitTask(TaskList.Items[i]);
        FT.StopAsyncOper;
    end;

    MainCalcThread.Terminate;
    //  ozhidaniya zdes' nikakogo byt' ne dolzhno,
    //  poskol'ku metod vypolnyaetsya sinhronno i
    //  dolzhen bystro vernut' upravlenie
end;

function TFitServerWithThread.CreateTaskObject: TFitTask;
begin
    Result := TFitTask.Create(nil);
end;

end.



