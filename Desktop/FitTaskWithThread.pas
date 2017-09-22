unit FitTaskWithThread;

//{$mode objfpc}{$H+}
{$MODE Delphi}

interface

uses Classes, SysUtils, FitTask, MainCalcThread;

type
    //  klass dlya asynhronnogo resheniya zadachi
    TFitTaskWithThread = class(TFitTask)
    protected
        MainCalcThread: TMainCalcThread;
        CS: TRTLCriticalSection;    //  vmesto TCriticalSection;
        FDoneDisabled: Boolean;     //  zapreschaet vyzov DoneProc
        
        procedure RecreateMainCalcThread(
            ACurrentTask: TCurrentTask; ADoneProc: TDoneProc);

        procedure ShowCurMin; override;
        function GetCurMinInitialized: Boolean; override;
        procedure DoneProc; override;
        //  !!! dlya raboty v mnogopotochnoy srede trebuetsya sinhronizatsiya !!!
        function GetCurMin: Double; override;
        function GetCurAbsMin: Double; override;
        function GetCurSqrMin: Double; override;
        function GetAllDone: Boolean; override;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        //  sinhronnoe preryvanie dlitel'noy operatsii
        //  bez vyzova protsedury zaversheniya
        procedure AbortAsyncOper; override;
        //  asinhronnaya ostanovka dlitel'noy operatsii
        //  s vyzovom protsedury zaversheniya
        procedure StopAsyncOper; override;
        //  ozhidaet zaversheniya vypolneniya potoka
        procedure DestroyMainCalcThread;
        //  =============== asinhronnye (dlitel'nye) operatsii ==================
        //  podgonka krivyh pri zadannyh dannyh (pervonachal'naya ili povtornaya)
        procedure FindGausses; override;
        procedure FindGaussesAgain; override;
        //  ischet nabor krivyh opisyvayuschiy profil' s zadannoy tochnost'yu
        //  posledovatel'no umen'shaya kolichestvo krivyh (poluavtomaticheskaya
        //  podgonka krivyh po zadannym dannym)
        procedure FindGaussesSequentially; override;
        //  !!! opublikovano dlya bolee optimal'noy realizatsii ostanovki !!!
        property DoneDisabled: Boolean read FDoneDisabled write FDoneDisabled;
    end;

implementation

uses Main;

procedure TFitTaskWithThread.RecreateMainCalcThread(
    ACurrentTask: TCurrentTask; ADoneProc: TDoneProc);
begin
    if Assigned(MainCalcThread) then AbortAsyncOper;

    AllDone := False;
    DoneDisabled := False;
    
    MainCalcThread := TMainCalcThread.Create(True (* CreateSuspended *));
    if Assigned(MainCalcThread.FatalException) then
       raise MainCalcThread.FatalException;
    //  prisoedinenie zadachi
    MainCalcThread.SetCurrentTask(ACurrentTask);
    MainCalcThread.SetDoneProc(ADoneProc);
    //  zapusk potoka resheniya zadachi
    MainCalcThread.Resume;
end;

procedure TFitTaskWithThread.DestroyMainCalcThread;
begin
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    try
        Assert(Assigned(MainCalcThread));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Surprising)
        else raise;
    end;

    if Assigned(MainCalcThread) then
    begin
        MainCalcThread.Terminate;
        MainCalcThread.WaitFor;
        MainCalcThread.Free;
        MainCalcThread := nil;
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
    UpdateCurves(nil);
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
    //  proverka neozhidannyh situatsiy;
    //  ne protivorechit semantike metoda - nefatal'n. oshibka
    try
        Assert(Assigned(MainCalcThread));
    except
        on E: EAssertionFailed do WriteLog(E.Message, Surprising)
        else raise;
    end;

    Terminated := True;
    if Assigned(Minimizer) then Minimizer.Terminated := True;
    if Assigned(MainCalcThread) then MainCalcThread.Terminate;
    //  ozhidaniya zdes' nikakogo byt' ne dolzhno,
    //  poskol'ku metod vypolnyaetsya v osn. potoke
    //  dolzhen bystro vernut' upravlenie
end;

procedure TFitTaskWithThread.DoneProc;
begin
    //  !!! vyzyvaetsya v osnovnom potoke !!!
    EnterCriticalsection(CS);
    AllDone := True;
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
        MainCalcThread.Synchronize(MainCalcThread, ShowCurMinExternal);
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
    Result := AllDone;
    LeaveCriticalsection(CS);
end;

function TFitTaskWithThread.GetCurMinInitialized: Boolean;
begin
    EnterCriticalsection(CS);
    Result := CurMinInitialized;
    LeaveCriticalsection(CS);
end;

constructor TFitTaskWithThread.Create(AOwner: TComponent);
begin
    inherited;
    //InitializeCriticalSection(CS);
    InitCriticalSection(CS);
end;

destructor TFitTaskWithThread.Destroy;
begin
    if Assigned(MainCalcThread) then AbortAsyncOper;
    //DeleteCriticalSection(CS);
    DoneCriticalSection(CS);
    inherited;
end;

end.


