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
    Classes, log, SysUtils;

type
    { Must contain counterparts of IClientCallback methods withoud parameters
      for synchronous call from UI thread. }
    //  TODO: nuzhno sdelat' perehvat isklyucheniy i sohranenie soobscheniya
    //  v ob'ekte potoka dlya posleduyuschego chteniya.
    TMainCalcThread = class(TThread)
    private
        FTask:    TThreadMethod;
        { These methods are synchronized with UI thread. }
        FShowCurMin: TThreadMethod;
        FShowProfile: TThreadMethod;
        FDone:    TThreadMethod;
        FComputeCurveBoundsDone: TThreadMethod;
        FComputeBackgroundPointsDone: TThreadMethod;
        FComputeCurvePositionsDone: TThreadMethod;
        FAllDone: TThreadMethod;

    public
        procedure Execute; override;

        procedure ShowCurMin;
        procedure ShowProfile;
        procedure Done;
        procedure ComputeCurveBoundsDone;
        procedure ComputeBackgroundPointsDone;
        procedure ComputeCurvePositionsDone;

        procedure SetSyncMethods(ATask, AShowCurMin, AShowProfile,
            ADone, AComputeCurveBoundsDone, AComputeBackgroundPointsDone,
            AComputeCurvePositionsDone, AAllDone: TThreadMethod);
    end;

implementation

uses
    app;

procedure TMainCalcThread.SetSyncMethods(
    ATask, AShowCurMin, AShowProfile, ADone, AComputeCurveBoundsDone,
    AComputeBackgroundPointsDone, AComputeCurvePositionsDone, AAllDone: TThreadMethod);
begin
    Assert(Assigned(ATask));
    Assert(Assigned(AShowCurMin));
    Assert(Assigned(AShowProfile));
    Assert(Assigned(ADone));
    Assert(Assigned(AComputeCurveBoundsDone));
    Assert(Assigned(AComputeBackgroundPointsDone));
    Assert(Assigned(AComputeCurvePositionsDone));
    Assert(Assigned(AAllDone));

    FTask    := ATask;
    FShowCurMin := AShowCurMin;
    FShowProfile := AShowProfile;
    FDone    := ADone;
    FComputeCurveBoundsDone := AComputeCurveBoundsDone;
    FComputeCurvePositionsDone := AComputeCurvePositionsDone;
    FAllDone := AAllDone;
end;

procedure TMainCalcThread.Execute;
begin
    //  !!! zdes' ostavim takuyu shemu obrabotki,
    //  potomu chto vypolnyaetsya v drugom potoke !!!
    try
        FTask;
    except
        on E: Exception do
            WriteLog(E.Message, Fatal);
    end;
    Synchronize(FAllDone);
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

procedure TMainCalcThread.ComputeCurveBoundsDone;
begin
    Synchronize(FComputeCurveBoundsDone);
end;

procedure TMainCalcThread.ComputeBackgroundPointsDone;
begin
    Synchronize(FComputeBackgroundPointsDone);
end;

procedure TMainCalcThread.ComputeCurvePositionsDone;
begin
    Synchronize(FComputeCurvePositionsDone);
end;

end.
