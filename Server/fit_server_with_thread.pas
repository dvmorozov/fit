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

uses SysUtils, fit_server, common_types, main_calc_thread, MyExceptions,
    fit_task;

type
    { The server component performing long-term operation in separate thread. }
    TFitServerWithThread = class(TFitServer)
    protected
        { Pointer to the thread performing long-term method of the component. 
          It is used for synchronization with main thread of the application.
          If this pointer is not Nil then displaying methods are called synchronously. }
        main_calc_thread: TMainCalcThread;

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

{$warnings off}
procedure TFitServerWithThread.RecreateMainCalcThread(
    ACurrentTask: TCurrentTask; ADoneProc: TDoneProc);
begin
    if State = AsyncOperation then AbortAsyncOper;
    DoneDisabled := False;
    
    main_calc_thread := TMainCalcThread.Create(True (* CreateSuspended *));
    //  tak rekomenduet rukovodstvo
    if Assigned(main_calc_thread.FatalException) then
       raise main_calc_thread.FatalException;
    //  prisoedinenie zadachi
    main_calc_thread.SetCurrentTask(ACurrentTask);
    main_calc_thread.SetDoneProc(ADoneProc);
    //  ustanovka sostoyaniya d.b. do zapuska potoka
    SetState(AsyncOperation);
    //  zapusk potoka resheniya zadachi
    main_calc_thread.Resume;
end;
{$warnings on}

procedure TFitServerWithThread.DestroyMainCalcThread;
begin
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    (*
    try
        Assert(Assigned(main_calc_thread));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Surprising)
        else raise;
    end;
    *)
    if Assigned(main_calc_thread) then
    begin
        main_calc_thread.Terminate;
        main_calc_thread.WaitFor;
        main_calc_thread.Free;
        main_calc_thread := nil;
    end;
end;

procedure TFitServerWithThread.AbortAsyncOper;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(main_calc_thread));

    DoneDisabled := True;

    main_calc_thread.Terminate;
    DestroyMainCalcThread;
    FState := SavedState;
end;

procedure TFitServerWithThread.StopAsyncOper;
begin
    if State <> AsyncOperation then
        raise EUserException.Create(InadmissibleServerState + CRLF +
            CalcNotStarted);

    Assert(Assigned(TaskList));
    Assert(Assigned(main_calc_thread));
    
    main_calc_thread.Terminate;
    //  ozhidaniya zdes' nikakogo byt' ne dolzhno,
    //  poskol'ku metod vypolnyaetsya sinhronno i
    //  dolzhen bystro vernut' upravlenie
end;

function TFitServerWithThread.CreateTaskObject: TFitTask;
begin
    Result := TFitTask.Create(nil, FEnableBackgroundVariation);
end;

end.



