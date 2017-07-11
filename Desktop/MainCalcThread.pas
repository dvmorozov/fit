//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
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



