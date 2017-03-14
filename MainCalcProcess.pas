//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit MainCalcProcess;

{$MODE Delphi}

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
    // vyzovy etih metodov sinhroniziruyutsya
    // s osnovnym potokom prilozheniya
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


