{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of base classes for optimization algorithms.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit int_minimizer;

{$MODE Delphi}

interface

uses Classes;

type
    TFunc     = function: double of object;
    TCalcFunc = procedure of object;
    TGetStep  = function: double of object;
    TSetStep  = procedure(NewStepValue: double) of object;
    TSetNextParam = procedure of object;
    TSetFirstParam = procedure of object;
    TGetParam = function: double of object;
    TSetParam = procedure(NewParamValue: double) of object;
    TEndOfCycle = function: boolean of object;
    TShowCurMin = procedure of object;

    { Defines interface of data provider for optimization algorithm. }
    IMinimizer = interface(IUnknown)
        { Returns result of function computation. }
        function Func: double;
        { Calculates function value. }
        procedure CalcFunc;
        { Returns current step value. }
        function GetStep: double;
        { Set new step value. }
        procedure SetStep(NewStepValue: double);
        { Selects next variable parameter. 
          Changes internal state of provider in such way to allow
          other methods to have access to parameter data. }
        procedure SetNextParam;
        { Selects the first variable parameter. }
        procedure SetFirstParam;
        { Returns value of selected parameter. }
        function GetParam: double;
        { Sets value of selected parameter. }
        procedure SetParam(NewParamValue: double);
        { Returns flag indicating end of calculation cycle (when the 
          last parameter has been varied and corresponding function 
          values were evaluated. }
        function EndOfCycle: boolean;
    end;

    { Defines component implementing IMinimizer interface (providing data for optimization algorithm). }
    TIntMinimizer = class(TComponent, IMinimizer)
        function Func: double; virtual; abstract;
        procedure CalcFunc; virtual; abstract;
        function GetStep: double; virtual; abstract;
        procedure SetStep(NewStepValue: double); virtual; abstract;
        procedure SetNextParam; virtual; abstract;
        procedure SetFirstParam; virtual; abstract;
        function GetParam: double; virtual; abstract;
        procedure SetParam(NewParamValue: double); virtual; abstract;
        function EndOfCycle: boolean; virtual; abstract;
    end;

    { Adapter supporting set of function pointers ("events") for any optimization task. }
    TMinimizer = class(TComponent)
    private
        FOnGetFunc:     TFunc;
        FOnComputeFunc: TCalcFunc;
        FOnGetVariationStep:  TGetStep;
        FOnSetVariationStep:  TSetStep;
        FOnSetNextParam: TSetNextParam;
        FOnSetFirstParam: TSetFirstParam;
        FOnGetParam: TGetParam;
        FOnSetParam: TSetParam;
        FOnEndOfCycle: TEndOfCycle;
        FOnShowCurMin: TShowCurMin;
        FTerminated: boolean;

    protected
        procedure SetTerminated(ATerminated: boolean); virtual;

    public
        FCurrentMinimum: double;

        procedure Minimize(var ErrorCode: longint); virtual; abstract;
        // vozvraschaet kod oshibki
        function IsReady: longint; virtual;
        constructor Create(AOwner: TComponent); override;

        property Terminated: boolean read FTerminated write SetTerminated;

    published
        //property MinInterface: IMinimizer
        //    read FMinInterface write FMinInterface;
        property OnGetFunc: TFunc read FOnGetFunc write FOnGetFunc;
        property OnComputeFunc: TCalcFunc read FOnComputeFunc write FOnComputeFunc;
        property OnGetVariationStep: TGetStep read FOnGetVariationStep write FOnGetVariationStep;
        property OnSetVariationStep: TSetStep read FOnSetVariationStep write FOnSetVariationStep;
        property OnSetNextParam: TSetNextParam
            read FOnSetNextParam write FOnSetNextParam;
        property OnSetFirstParam: TSetFirstParam
            read FOnSetFirstParam write FOnSetFirstParam;
        property OnGetParam: TGetParam read FOnGetParam write FOnGetParam;
        property OnSetParam: TSetParam read FOnSetParam write FOnSetParam;
        property OnEndOfCycle: TEndOfCycle read FOnEndOfCycle write FOnEndOfCycle;
        property OnShowCurMin: TShowCurMin read FOnShowCurMin write FOnShowCurMin;
    end;

const
    MIN_NO_ERRORS: longint = 0;
    MIN_FUNCTION_NOT_ASSIGNED: longint = 1;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('MSCR', [TMinimizer]);
    (*
    //RegisterPropertyEditor(TypeInfo(IMinimizer),
    //    TMinimizer, 'MinInterface', TInterfaceProperty);
    RegisterPropertyEditor(TypeInfo(TFunc),
        TMinimizer, 'OnFunc', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TCalcFunc),
        TMinimizer, 'OnCalcFunc', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TGetStep),
        TMinimizer, 'OnGetStep', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TSetStep),
        TMinimizer, 'OnSetStep', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TSetNextParam),
        TMinimizer, 'OnSetNextParam', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TSetFirstParam),
        TMinimizer, 'OnSetFirstParam', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TGetParam),
        TMinimizer, 'OnGetParam', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TSetParam),
        TMinimizer, 'OnSetParam', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TEndOfCycle),
        TMinimizer, 'OnEndOfCycle', TMethodProperty);
    RegisterPropertyEditor(TypeInfo(TShowCurMin),
        TMinimizer, 'OnShowCurMin', TMethodProperty);
    *)
end;

{================================ TMinimizer ==================================}

procedure TMinimizer.SetTerminated(ATerminated: boolean);
begin
    FTerminated := ATerminated;
end;

function TMinimizer.IsReady: longint;
begin
    Result := MIN_NO_ERRORS;
    if not Assigned(OnGetFunc) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnComputeFunc) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnGetVariationStep) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnSetVariationStep) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnSetNextParam) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnSetFirstParam) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnGetParam) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnSetParam) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
    if not Assigned(OnEndOfCycle) then
    begin
        Result := MIN_FUNCTION_NOT_ASSIGNED;
        Exit;
    end;
end;

constructor TMinimizer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FTerminated := False;
end;

end.
