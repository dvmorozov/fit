{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class executing optimization task in separate thread.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit fit_task_with_thread;

interface

uses
    Classes, SysUtils, fit_task, main_calc_thread, log;

type
    { Executes task solution in separate thread. }
    TFitTaskWithThread = class(TFitTask)
    protected
        FMainCalcThread: TMainCalcThread;
        CS: TRTLCriticalSection;    //  Use instead of TCriticalSection.
        FDoneDisabled: Boolean;     //  Suppresses calling DoneProc.
        
        procedure RecreateMainCalcThread(
            ACurrentTask: TThreadMethod; ADoneProc: TThreadMethod);

     public
        procedure ShowCurMin; override;
        function GetCurMinInitialized: Boolean; override;
        procedure DoneProc; override;
        
        { Methods implement synchronization to work in multithreading environment. }
        
        function GetCurMin: Double; override;
        function GetCurAbsMin: Double; override;
        function GetCurSqrMin: Double; override;
        function GetAllDone: Boolean; override;

        {TODO: methods should be implemented. }

        procedure ShowCurMinSync;
        procedure ShowProfileSync;
        procedure DoneSync;
        procedure FindPeakBoundsDoneSync;
        procedure FindBackPointsDoneSync;
        procedure FindPeakPositionsDoneSync;

        constructor Create(AOwner: TComponent;
            AEnableBackgroundVariation: Boolean;
            ACurveScalingEnabled: Boolean); override;
        destructor Destroy; override;
        { Sets up termination flags and waits for
          actual termination of the thread. }
        procedure AbortAsyncOper;
        { Sets up termination flags and returns. The method doesn't wait for
          actual termination of the thread. }
        procedure StopAsyncOper; override;
        { Waits for thread termination. }
        procedure DestroyMainCalcThread;

        { Asynchronous long-term operations. }
        
        { Fits pattern specimens starting from given parameter set (initially or repeatedly). }
        procedure FindGausses; override;
        procedure FindGaussesAgain; override;
        { Searches set of pattern specimens (curves) fitting exprerimental data with given accuracy
          sequentially decreasing number of curves. }
        procedure FindGaussesSequentially; override;
        property DoneDisabled: Boolean read FDoneDisabled write FDoneDisabled;
    end;

implementation

uses app;

{$warnings off}
procedure TFitTaskWithThread.RecreateMainCalcThread(
    ACurrentTask: TThreadMethod; ADoneProc: TThreadMethod);
begin
    if Assigned(FMainCalcThread) then AbortAsyncOper;

    FAllDone := False;
    DoneDisabled := False;
    
    FMainCalcThread := TMainCalcThread.Create(True (* CreateSuspended *));
    if Assigned(FMainCalcThread.FatalException) then
       raise FMainCalcThread.FatalException;
    { Assignment of callbacks. }
    FMainCalcThread.SetSyncMethods(
        ACurrentTask, ShowCurMinSync, ShowProfileSync, DoneSync,
        FindPeakBoundsDoneSync, FindBackPointsDoneSync, FindPeakPositionsDoneSync,
        ADoneProc);
    { Start thread. }
    FMainCalcThread.Resume;
end;
{$warnings on}

procedure TFitTaskWithThread.DestroyMainCalcThread;
begin
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    try
        Assert(Assigned(FMainCalcThread));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Warning)
        else raise;
    end;

    if Assigned(FMainCalcThread) then
    begin
        FMainCalcThread.Terminate;
        FMainCalcThread.WaitFor;
        FMainCalcThread.Free;
        FMainCalcThread := nil;
    end;
end;

procedure TFitTaskWithThread.FindGaussesSequentially;
begin
    RecreateMainCalcThread(FindGaussesSequentiallyAlg, DoneProc);
end;

procedure TFitTaskWithThread.FindGaussesAgain;
begin
    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyani
    // povtornaya initsializatsiya gaussianov
    RecreateCurveInstances(nil);
    CalculateProfile;
    RecreateMainCalcThread(Optimization, DoneProc);
end;

procedure TFitTaskWithThread.FindGausses;
//var    i: LongInt;
    //GP: TPointsSet;
begin
    // nachal'naya initsializatsiya neobhodima, kogda pri
    // vychislenii R-faktora predpolagaetsya, chto vse
    // tochki vychislennogo profilya ne d.b. ravny 0
    //Assert(Assigned(CurvesList));
    //with CurvesList do
    //  for i := 0 to Count - 1 do
    //  begin
    //      GP := TPointsSet(Items[i]);
    //      if GP is TGaussPointsSet then
    //          TGaussPointsSet(GP).Sigma := 0.6;
    //      TGaussPointsSet(GP).A := 100;
    //  end
    //CalcGaussProfile;

    //  metod vnutrenniy - ne vybrasyvaet isklyucheniya nedopustimogo sostoyani
    RecreateMainCalcThread(Optimization, DoneProc);
end;

procedure TFitTaskWithThread.AbortAsyncOper;
begin
    DoneDisabled := True;
    StopAsyncOper;
    DestroyMainCalcThread;
end;

procedure TFitTaskWithThread.StopAsyncOper;
begin
    inherited;
    try
        Assert(Assigned(FMainCalcThread));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Warning)
        else raise;
    end;

    if Assigned(FMainCalcThread) then FMainCalcThread.Terminate;
end;

procedure TFitTaskWithThread.DoneProc;
begin
    //  !!! vyzyvaetsya v osnovnom potoke !!!
    EnterCriticalsection(CS);
    FAllDone := True;
    LeaveCriticalsection(CS);
    if not DoneDisabled then DoneProcExternal;
end;

procedure TFitTaskWithThread.ShowCurMin;
var Flag: Boolean;
begin
    Flag := False;
    //  !!! vyzyvaetsya v dopolnitel'nom potoke !!!
    EnterCriticalsection(CS);
    //  !!! nuzhno pereschityvat' fakt. rash. potomu chto:
    //  1. vyvodimaya f-ya mozhet otlichat'sya ot toy, po kot.
    //  proizvoditsya optimizatsiya;
    //  2. parametry mogut izmenit'sya v rezul'tate raboty
    //  spets. algoritmov (naprimer udaleniya "lishnih" krivyh) !!!
    CurSqrMin := GetSqrRFactor;
    CurAbsMin := GetAbsRFactor;
    CurMin := CurSqrMin;    //  chtoby ne pereschityvat'
                            //  !!! dolzhno sootvetstvovat' GetRFactor !!!
    CurMinInitialized := True;
    Flag := True;
    
    LeaveCriticalsection(CS);
    //  dop. potok ostanavlivaetsya
    if Flag then
{$IFDEF FIT}
        FMainCalcThread.Synchronize(ShowCurMinExternal);
{$ELSE}
        FMainCalcThread.Synchronize(FMainCalcThread, ShowCurMinExternal);
{$ENDIF}
end;

function TFitTaskWithThread.GetCurMin: Double;
begin
    //  !!! krit. sektsiya trebuetsya potomu, chto
    //  zapis' znacheniya tipa Double ne delaetsya
    //  odnoy komandoy !!!
    EnterCriticalsection(CS);
    Result := CurMin;
    LeaveCriticalsection(CS);
end;

function TFitTaskWithThread.GetCurAbsMin: Double;
begin
    //  !!! krit. sektsiya trebuetsya potomu, chto
    //  zapis' znacheniya tipa Double ne delaetsya
    //  odnoy komandoy !!!
    EnterCriticalsection(CS);
    Result := CurAbsMin;
    LeaveCriticalsection(CS);
end;

function TFitTaskWithThread.GetCurSqrMin: Double;
begin
    //  !!! krit. sektsiya trebuetsya potomu, chto
    //  zapis' znacheniya tipa Double ne delaetsya
    //  odnoy komandoy !!!
    EnterCriticalsection(CS);
    Result := CurSqrMin;
    LeaveCriticalsection(CS);
end;

function TFitTaskWithThread.GetAllDone: Boolean;
begin
    EnterCriticalsection(CS);
    Result := FAllDone;
    LeaveCriticalsection(CS);
end;

function TFitTaskWithThread.GetCurMinInitialized: Boolean;
begin
    EnterCriticalsection(CS);
    Result := CurMinInitialized;
    LeaveCriticalsection(CS);
end;

constructor TFitTaskWithThread.Create(AOwner: TComponent;
    AEnableBackgroundVariation: Boolean;
    ACurveScalingEnabled: Boolean);
begin
    inherited;
    InitCriticalSection(CS);
end;

destructor TFitTaskWithThread.Destroy;
begin
    if Assigned(FMainCalcThread) then AbortAsyncOper;
    DoneCriticalSection(CS);
    inherited;
end;

procedure TFitTaskWithThread.ShowCurMinSync;
begin

end;

procedure TFitTaskWithThread.ShowProfileSync;
begin

end;

procedure TFitTaskWithThread.DoneSync;
begin

end;

procedure TFitTaskWithThread.FindPeakBoundsDoneSync;
begin

end;

procedure TFitTaskWithThread.FindBackPointsDoneSync;
begin

end;

procedure FindPeakPositionsDoneSync;
begin

end;

end.



