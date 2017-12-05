{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of container class for callback methods.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit MainCalcThread;

{$MODE Delphi}

interface

uses Classes, SysUtils;

type
    TCurrentTask = procedure of object;
    TShowResultsProc = procedure of object;
    TDoneProc = procedure of object;

    //??? nuzhno sdelat' perehvat isklyucheniy i sohranenie soobscheniya
    //  v ob'ekte potoka dlya posleduyuschego chteniya
    TMainCalcThread = class(TThread)
    private
        CurrentTask: TCurrentTask;
        //  vyzovy etih metodov sinhroniziruyutsya
        //  s osnovnym potokom prilozheniya, iz
        //  kotorogo proishodit vyzov vseh operatsiy
        DoneProc: TDoneProc;
    public
        procedure Execute; override;

        procedure SetCurrentTask(ACurrentTask: TCurrentTask);
        procedure SetDoneProc(ADoneProc: TDoneProc);
    end;

implementation

uses Main;

procedure TMainCalcThread.SetCurrentTask(ACurrentTask: TCurrentTask);
begin
    CurrentTask := ACurrentTask;
end;

procedure TMainCalcThread.SetDoneProc(ADoneProc: TDoneProc);
begin
    DoneProc := ADoneProc;
end;

procedure TMainCalcThread.Execute;
begin
    //  !!! zdes' ostavim takuyu shemu obrabotki,
    //  potomu chto vypolnyaetsya v drugom potoke !!!
    try
        Assert(Assigned(CurrentTask));  //  proverka tol'ko pri otladke -
                                        //  isklyuchenie ne vyhodit naruzhu
        CurrentTask;
    except
        on E: Exception do WriteLog(E.Message, Fatal);
    end;
    if Assigned(DoneProc) then Synchronize(DoneProc);
end;

end.



