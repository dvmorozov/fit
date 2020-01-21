{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of container class for callback methods.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit main_calc_thread;

interface

uses
  Classes, SysUtils;

type
  TCurrentTask = procedure of object;
  TShowResultsProc = procedure of object;
  TDoneProc = procedure of object;

  TMainCalcThread = class(TThread)
  private
    CurrentTask : TCurrentTask;

    { These methods are synchronized with main application thread. }
    
    ShowResultsProc : TShowResultsProc;
    DoneProc : TDoneProc;
  public
    procedure Execute; override;
    procedure Synchronize(Method: TThreadMethod);
    procedure SetCurrentTask(ACurrentTask : TCurrentTask);
    procedure SetShowResultsProc(AShowResultsProc : TShowResultsProc);
    procedure SetDoneProc(ADoneProc : TDoneProc);
    procedure ShowResults;
  end;

implementation

procedure TMainCalcThread.SetCurrentTask(ACurrentTask : TCurrentTask);
begin
 CurrentTask := ACurrentTask;
end;

procedure TMainCalcThread.SetShowResultsProc(AShowResultsProc : TShowResultsProc);
begin
 ShowResultsProc := AShowResultsProc;
end;

procedure TMainCalcThread.SetDoneProc(ADoneProc : TDoneProc);
begin
 DoneProc := ADoneProc;
end;

procedure TMainCalcThread.Execute;
begin
    //  !!! zdes' ostavim takuyu shemu obrabotki,
    //  potomu chto vypolnyaetsya v drugom potoke !!!
    try
        Assert(Assigned(CurrentTask));
        CurrentTask;
        //  DoneProc sama d. zabotit'sya o sinhronizatsii
        //  inache mozhet proizoyti povtornyy vhod v Synchronize
        if Assigned(DoneProc) then DoneProc;
    except
    end;
end;

procedure TMainCalcThread.ShowResults;
begin
 if Assigned(ShowResultsProc) then Synchronize(ShowResultsProc);
end;

procedure TMainCalcThread.Synchronize(Method: TThreadMethod);
begin
 inherited Synchronize(Method);
end;

end.


