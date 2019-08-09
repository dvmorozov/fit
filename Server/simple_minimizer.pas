{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of classes implementing optimization algorithms.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit simple_minimizer;

{$MODE Delphi}

interface

uses minimizer, SelfCheckedComponentList, SysUtils;

type
    { Implements simple downhill simplex algorithm. }
    TSimpleMinimizer = class(TMinimizer)
    public
        procedure Minimize(var ErrorCode: LongInt); override;
    end;

    { Implements simple downhill simplex algorithm having different steps for every parameter. }
    TSimpleMinimizer2 = class(TMinimizer)
    public
        { Divides all steps by 2. }
        DivideStepsBy2: procedure of object;
        { Returns flag terminating calculation. }
        EndOfCalculation: function: Boolean of object;
        procedure Minimize(var ErrorCode: LongInt); override;
    end;

    { Implements simple downhill simplex algorithm able to increase step size. Now this variant is used. }
    TSimpleMinimizer3 = class(TMinimizer)
    public
        MultipleSteps: procedure(Factor: Double) of object;
        { Returns flag terminating calculation. }
        EndOfCalculation: function: Boolean of object;
        procedure Minimize(var ErrorCode: LongInt); override;
    end;

procedure Register;

implementation

procedure Register;
begin
    //RegisterComponents('Minimax',[TSimpleMinimizer]);
    //RegisterComponents('Minimax',[TSimpleMinimizer2]);
    //RegisterComponents('Minimax',[TSimpleMinimizer3]);
end;

{============================== TSimpleMinimizer ==============================}
procedure TSimpleMinimizer.Minimize(var ErrorCode: LongInt);
var Step: Double;
    SaveParam: Double;
    MinimizeValue,MinimizeValue2: Double;
    MinIndex: LongInt;
    TotalMinimum: Double;
begin
    //  proverka prisoedineniya interfeysnyh funktsiy
    ErrorCode := IsReady;
    if ErrorCode <> MIN_NO_ERRORS then Exit;
    
    Step := OnGetStep;
    while (Step >= 0.001{!!!}) and (not Terminated) do
    begin
        OnSetFirstParam;
        Step := OnGetStep;
        OnCalcFunc;
        TotalMinimum := OnFunc;
        while (not OnEndOfCycle) and (not Terminated) do
        begin
            OnCalcFunc;
            SaveParam := OnGetParam;
            CurrentMinimum := OnFunc;

            OnSetParam(SaveParam + Step);
            OnCalcFunc;
            MinimizeValue := OnFunc;

            OnSetParam(SaveParam - Step);
            OnCalcFunc;
            MinimizeValue2 := OnFunc;

            OnSetParam(SaveParam);
            MinIndex := 0;

            if (MinimizeValue >= CurrentMinimum) and
               (MinimizeValue2 >= CurrentMinimum) then MinIndex := 0;
            if (MinimizeValue >= CurrentMinimum) and
               (MinimizeValue2 < CurrentMinimum) then MinIndex := 2;
            if (MinimizeValue < CurrentMinimum) and
               (MinimizeValue2 >= CurrentMinimum) then MinIndex := 1;
            if (MinimizeValue < CurrentMinimum) and
               (MinimizeValue2 < CurrentMinimum) then
            begin
                if MinimizeValue <= MinimizeValue2 then
                    MinIndex := 1 else MinIndex := 2;
            end;

            case MinIndex of
                1: OnSetParam(SaveParam + Step);
                2: OnSetParam(SaveParam - Step);
            end;

            OnCalcFunc;

            if Assigned(OnShowCurMin) then
                if OnFunc < CurrentMinimum then
                begin
                    CurrentMinimum := OnFunc;
                    OnShowCurMin;
                end;
            OnSetNextParam;
        end;
        if OnFunc >= TotalMinimum then OnSetStep(Step / 2);
    end;{while (Step > 1e-5) and (not Terminated) do...}
end;

{============================== TSimpleMinimizer2 =============================}
procedure TSimpleMinimizer2.Minimize(var ErrorCode: LongInt);
var Step: Double;
    SaveParam: Double;
    MinimizeValue, MinimizeValue2: Double;
    MinIndex: LongInt;
    TotalMinimum: Double;
    NewMinFound: Boolean;
begin
    //  proverka prisoedineniya interfeysnyh funktsiy
    ErrorCode := IsReady;
    if ErrorCode <> MIN_NO_ERRORS then Exit;
    CurrentMinimum := OnFunc;
    
    //??? vydavat' kod oshibki ili vybrasyvat' isklyuchenie
    Assert(Assigned(DivideStepsBy2));

    //  beskonechnyy tsikl optimizatsii
    while (not EndOfCalculation) and (not Terminated) do
    begin
        OnSetFirstParam;
        TotalMinimum := CurrentMinimum;
        //  tsikl optimizatsii po vsem parametram
        while (not OnEndOfCycle) and (not Terminated) do
        begin
            //  poluchenie shaga izmeneniya dlya ocherednogo parametra
            Step := OnGetStep;
            //  poluchenie znacheniya ocherednogo parametra
            SaveParam := OnGetParam;

            //  pervoe izmenenie parametra
            OnSetParam(SaveParam + Step);
            //  vychislenie novogo znacheniya funktsii
            OnCalcFunc;
            // poluchenie novogo znacheniya funktsii
            MinimizeValue := OnFunc;

            //  vtoroe izmenenie parametra
            OnSetParam(SaveParam - Step);
            //  vychislenie novogo znacheniya funktsii
            OnCalcFunc;
            //  poluchenie novogo znacheniya funktsii
            MinimizeValue2 := OnFunc;

            //  vosstanovlenie ishodnogo znacheniya parametra
            OnSetParam(SaveParam);
            MinIndex := 0;
            if (MinimizeValue >= CurrentMinimum) and
               (MinimizeValue2 >= CurrentMinimum) then MinIndex := 0;
            if (MinimizeValue >= CurrentMinimum) and
               (MinimizeValue2 < CurrentMinimum) then MinIndex := 2;
            if (MinimizeValue < CurrentMinimum) and
               (MinimizeValue2 >= CurrentMinimum) then MinIndex := 1;
            if (MinimizeValue < CurrentMinimum) and
               (MinimizeValue2 < CurrentMinimum) then
            begin
                if MinimizeValue <= MinimizeValue2 then
                    MinIndex := 1 else MinIndex := 2;
            end;

            NewMinFound := False;
            case MinIndex of
                1: begin OnSetParam(SaveParam + Step); NewMinFound := True; end;
                2: begin OnSetParam(SaveParam - Step); NewMinFound := True; end;
            end;

            if NewMinFound then
            begin
                OnCalcFunc; //  pereschet nuzhno delat', chtoby dopolnitel'nye dannye
                            //  imeli znacheniya, sootvetstvuyuschie minimal'nomu znacheniyu
                            //  funktsii
                CurrentMinimum := OnFunc;
                if Assigned(OnShowCurMin) then OnShowCurMin;
                //OutputDebugString(PChar(FloatToStr(CurrentMinimum) + Chr(10) + Chr(13)));
            end;
            OnSetNextParam;
        end;{while (not OnEndOfCycle) and (not Terminated) do...}
        if (TotalMinimum <> 0) and
           (Abs(CurrentMinimum - TotalMinimum) / TotalMinimum < 1e-5) then
                DivideStepsBy2;
    end;{while (not EndOfCalculation) and (not Terminated) do...}
end;

{============================== TSimpleMinimizer3 =============================}
procedure TSimpleMinimizer3.Minimize(var ErrorCode: LongInt);
var Step: Double;
    SaveParam: Double;
    MinimizeValue, MinimizeValue2: Double;
    MinIndex: LongInt;
    TotalMinimum: Double;
    NewMinFound: Boolean;
    DownCount: LongInt;
    
    debug: LongInt;//???
begin
    //  proverka prisoedineniya interfeysnyh funktsiy
    ErrorCode := IsReady;
    if ErrorCode <> MIN_NO_ERRORS then Exit;
    CurrentMinimum := OnFunc;
    TotalMinimum := CurrentMinimum;
    DownCount := 0;

    Assert(Assigned(MultipleSteps));
    
    debug := 0;
    
    //  beskonechnyy tsikl optimizatsii
    while (not EndOfCalculation) and (not Terminated) do
    begin
        OnSetFirstParam;
        //  tsikl optimizatsii po vsem parametram
        while (not OnEndOfCycle) and (not Terminated) do
        begin
            Inc(debug);
            //  poluchenie shaga izmeneniya dlya ocherednogo parametra
            Step := OnGetStep;
            //  poluchenie znacheniya ocherednogo parametra
            SaveParam := OnGetParam;

            //  pervoe izmenenie parametra
            OnSetParam(SaveParam + Step);
            //  vychislenie novogo znacheniya funktsii
            OnCalcFunc;
            // poluchenie novogo znacheniya funktsii
            MinimizeValue := OnFunc;

            //  vtoroe izmenenie parametra
            OnSetParam(SaveParam - Step);
            //  vychislenie novogo znacheniya funktsii
            OnCalcFunc;
            //  poluchenie novogo znacheniya funktsii
            MinimizeValue2 := OnFunc;

            //  vosstanovlenie ishodnogo znacheniya parametra
            OnSetParam(SaveParam);
            MinIndex := 0;
            if (MinimizeValue >= CurrentMinimum) and
               (MinimizeValue2 >= CurrentMinimum) then MinIndex := 0;
            if (MinimizeValue >= CurrentMinimum) and
               (MinimizeValue2 < CurrentMinimum) then MinIndex := 2;
            if (MinimizeValue < CurrentMinimum) and
               (MinimizeValue2 >= CurrentMinimum) then MinIndex := 1;
            if (MinimizeValue < CurrentMinimum) and
               (MinimizeValue2 < CurrentMinimum) then
            begin
                if MinimizeValue <= MinimizeValue2 then
                    MinIndex := 1 else MinIndex := 2;
            end;

            NewMinFound := False;
            case MinIndex of
                1: begin OnSetParam(SaveParam + Step); NewMinFound := True; end;
                2: begin OnSetParam(SaveParam - Step); NewMinFound := True; end;
            end;

            if NewMinFound then
            begin
                OnCalcFunc; //  pereschet nuzhno delat', chtoby dopolnitel'nye dannye
                            //  imeli znacheniya, sootvetstvuyuschie minimal'nomu znacheniyu
                            //  funktsii
                CurrentMinimum := OnFunc;
                if Assigned(OnShowCurMin) then OnShowCurMin;
                //OutputDebugString(PChar(FloatToStr(CurrentMinimum)));
            end;
            OnSetNextParam;
        end;{while (not OnEndOfCycle) and (not Terminated) do...}
        
        if (TotalMinimum <> 0) then
        begin
            //  CurrentMinimum m. stat' bol'she, chem TotalMinimum posle
            //  uvelicheniya shaga
            if (CurrentMinimum < TotalMinimum) and
                (Abs(CurrentMinimum - TotalMinimum) / TotalMinimum >= 1e-5) then
            begin
                TotalMinimum := CurrentMinimum;
                //  neskol'ko tsiklov idet vniz -
                //  shag uvelichivaetsya
                Inc(DownCount);
                if DownCount >= 10 then
                begin
                    MultipleSteps(1.01);
                    //OutputDebugString(PChar('Parameter steps increased...'));
                end;
            end
            else
            begin
                //  za posledniy tsikl suschestvenno luchshiy
                //  minimum ne nayden - shag umen'shaetsya
                MultipleSteps(0.99);
                DownCount := 0;
                //OutputDebugString(PChar('Parameter steps decreased...'));
            end;
            if CurrentMinimum < TotalMinimum then
                TotalMinimum := CurrentMinimum;
        end else Break;
    end;{while (not EndOfCalculation) and (not Terminated) do...}
end;

end.


