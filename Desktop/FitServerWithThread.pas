unit FitServerWithThread;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, FitServer, CommonTypes, MainCalcThread, MyExceptions,
    FitTask;

type
    TFitServerWithThread = class(TFitServer)
    protected
        //  uk-l' na potok vypolnyayuschiy dlitel'nyy metod dannogo komponenta
        //  dlya vyzova metoda sinhronizatsii s osnovnym potokom programmy;
        //  !!! esli etot uk-l' ne raven nil, to metody otobrazheniya vyzyvayutsya
        //  sinhronno, potomu chto poka ne predusmotreno situatsiy kogda metody
        //  otobrazheniya mogut vyzyvat'sya iz osn. potoka programmy v to vremya
        //  kak vypolnyaetsya zadacha v dopolnitel'nom potoke; sootvetstvuyuschie
        //  el-ty menyu d.b. zaprescheny !!!
        MainCalcThread: TMainCalcThread;

        procedure RecreateMainCalcThread(
            ACurrentTask: TCurrentTask; ADoneProc: TDoneProc); override;
        //  ozhidaet zaversheniya vypolneniya potoka;
        //  nel'zya vyzyvat' iz sinhronizovannogo metoda - inache deadlock
        procedure DestroyMainCalcThread;
        
        function CreateTaskObject: TFitTask; override;

    public
        destructor Destroy; override;
        
        //  ======================= komandy upravleniya =========================
        //  asinhronnaya ostanovka dlitel'noy operatsii
        //  s vyzovom protsedury zaversheniya
        procedure StopAsyncOper; override;
        //  sinhronnoe preryvanie dlitel'noy operatsii
        //  bez vyzova protsedury zaversheniya
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



