{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of thread class executing server methods.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit main_calc_thread;

interface

uses
    Classes, SysUtils;

type
    { Must contain counterparts of IClientCallback methods withoud parameters
      for synchronous call from UI thread. }
    //  TODO: nuzhno sdelat' perehvat isklyucheniy i sohranenie soobscheniya
    //  v ob'ekte potoka dlya posleduyuschego chteniya.
    TMainCalcThread = class(TThread)
    private
        FTask: TThreadMethod;
        { These methods are synchronized with UI thread. }
        FShowCurMin: TThreadMethod;
        FShowProfile: TThreadMethod;
        FDone: TThreadMethod;
        FFindPeakBoundsDone: TThreadMethod;
        FFindBackPointsDone: TThreadMethod;
        FFindPeakPositionsDone: TThreadMethod;
        FDoneAll: TThreadMethod;

    public
        procedure Execute; override;

        procedure ShowCurMin;
        procedure ShowProfile;
        procedure Done;
        procedure FindPeakBoundsDone;
        procedure FindBackPointsDone;
        procedure FindPeakPositionsDone;

        procedure SetSyncMethods(ATask, AShowCurMin, AShowProfile,
            ADone, AFindPeakBoundsDone, AFindBackPointsDone,
            AFindPeakPositionsDone, AAllDone: TThreadMethod);
    end;

implementation

uses app;

procedure TMainCalcThread.SetSyncMethods(
    ATask, AShowCurMin, AShowProfile,
    ADone, AFindPeakBoundsDone, AFindBackPointsDone,
    AFindPeakPositionsDone, AAllDone: TThreadMethod);
begin
    Assert(Assigned(ATask);
    Assert(Assigned(AShowCurMin));
    Assert(Assigned(AShowProfile)):
    Assert(Assigned(ADone));
    Assert(Assigned(AFindPeakBoundsDone));
    Assert(Assigned(AFindBackPointsDone));
    Assert(Assigned(AFindPeakPositionsDone));
    Assert(Assigned(AAllDone));

    FTask := ATask;
    FShowCurMin := AShowCurMin;
    FShowProfile := AShowProfile;
    FDone := ADone;
    FFindPeakBoundsDone := AFindPeakBoundsDone;
    FFindPeakPositionsDone := AFindPeakPositionsDone;
    FDoneAll := AAllDone;
end;

procedure TMainCalcThread.Execute;
begin
    //  !!! zdes' ostavim takuyu shemu obrabotki,
    //  potomu chto vypolnyaetsya v drugom potoke !!!
    try
        FTask;
    except
        on E: Exception do WriteLog(E.Message, Fatal);
    end;
    Synchronize(FDoneAll);
end;

procedure TMainCalcThread.ShowCurMin;
begin
    Synchronize(FShowCurMin);
end;

procedure TMainCalcThread.ShowProfile;
begin
    Synchronize(FShowProfile);
end;

procedure TMainCalcThread.Done;
begin
    Synchronize(FDone);
end;

procedure TMainCalcThread.FindPeakBoundsDone;
begin
    Synchronize(FFindPeakBoundsDone);
end;

procedure TMainCalcThread.FindBackPointsDone;
begin
    Synchronize(FFindBackPointsDone);
end;

procedure TMainCalcThread.FindPeakPositionsDone;
begin
    Synchronize(FFindPeakPositionsDone);
end;

end.



