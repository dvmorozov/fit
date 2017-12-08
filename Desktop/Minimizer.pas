{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of base classes for optimization algorithms.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit Minimizer;

{$MODE Delphi}

interface

uses Classes;

type
    TFunc = function: Double of object;
    TCalcFunc = procedure of object;
    TGetStep = function: Double of object;
    TSetStep = procedure(NewStepValue: Double) of object;
    TSetNextParam = procedure of object;
    TSetFirstParam = procedure of object;
    TGetParam = function: Double of object;
    TSetParam = procedure(NewParamValue: Double) of object;
    TEndOfCycle = function: Boolean of object;
    TShowCurMin = procedure of object;

    { Defines interface of data provider for optimization algorithm. }
    IMinimizer = interface(IUnknown)
        { Returns result of function computation. }
        function  Func: Double;
        { Calculates function value. }
        procedure CalcFunc;
        { Returns current step value. }
        function  GetStep: Double;
        { Set new step value. }
        procedure SetStep(NewStepValue: Double);
        { Selects next variable parameter. 
          Changes internal state of provider in such way to allow
          other methods to have access to parameter data. }
        procedure SetNextParam;
        { Selects the first variable parameter. }
        procedure SetFirstParam;
        { Returns value of selected parameter. }
        function  GetParam: Double;
        { Sets value of selected parameter. }
        procedure SetParam(NewParamValue: Double);
        { Returns flag indicating end of calculation cycle (when the 
          last parameter has been varied and corresponding function 
          values were evaluated. }
        function  EndOfCycle: Boolean;
    end;

    { Defines component implementing IMinimizer interface (providing data for optimization algorithm). }
    TIntMinimizer = class(TComponent, IMinimizer)
        function  Func: Double; virtual; abstract;
        procedure CalcFunc; virtual; abstract;
        function  GetStep: Double; virtual; abstract;
        procedure SetStep(NewStepValue: Double); virtual; abstract;
        procedure SetNextParam; virtual; abstract;
        procedure SetFirstParam; virtual; abstract;
        function  GetParam: Double; virtual; abstract;
        procedure SetParam(NewParamValue: Double); virtual; abstract;
        function  EndOfCycle: Boolean; virtual; abstract;
    end;

    { Implements interface for optimization of any function. }
    TMinimizer = class(TComponent)
    private
        FOnFunc: TFunc;
        FOnCalcFunc: TCalcFunc;
        FOnGetStep: TGetStep;
        FOnSetStep: TSetStep;
        FOnSetNextParam: TSetNextParam;
        FOnSetFirstParam: TSetFirstParam;
        FOnGetParam: TGetParam;
        FOnSetParam: TSetParam;
        FOnEndOfCycle: TEndOfCycle;
        FOnShowCurMin: TShowCurMin;
        //FMinInterface: IMinimizer;
        FTerminated: Boolean;

    protected
        procedure SetTerminated(ATerminated: Boolean); virtual;

    public
        CurrentMinimum: Double;
        
        procedure MinimizeStep; virtual; abstract;
        procedure Minimize(var ErrorCode: LongInt); virtual; abstract;
        // vozvraschaet kod oshibki
        function IsReady: LongInt; virtual;
        constructor Create(AOwner: TComponent); override;
        
        property Terminated: Boolean read FTerminated write SetTerminated;
        
    published
        //property MinInterface: IMinimizer
        //    read FMinInterface write FMinInterface;
        property OnFunc: TFunc read FOnFunc write FOnFunc;
        property OnCalcFunc: TCalcFunc read FOnCalcFunc write FOnCalcFunc;
        property OnGetStep: TGetStep read FOnGetStep write FOnGetStep;
        property OnSetStep: TSetStep read FOnSetStep write FOnSetStep;
        property OnSetNextParam: TSetNextParam
            read FOnSetNextParam write FOnSetNextParam;
        property OnSetFirstParam: TSetFirstParam
            read FOnSetFirstParam write FOnSetFirstParam;
        property OnGetParam: TGetParam read FOnGetParam write FOnGetParam;
        property OnSetParam: TSetParam read FOnSetParam write FOnSetParam;
        property OnEndOfCycle: TEndOfCycle
            read FOnEndOfCycle write FOnEndOfCycle;
        property OnShowCurMin: TShowCurMin
            read FOnShowCurMin write FOnShowCurMin;
    end;

const
    MIN_NO_ERRORS              : LongInt = 0;
    MIN_FUNCTION_NOT_ASSIGNED  : LongInt = 1;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('MSCR',[TMinimizer]);
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

procedure TMinimizer.SetTerminated(ATerminated: Boolean);
begin
    FTerminated := ATerminated;
end;

function TMinimizer.IsReady: LongInt;
begin
    Result := MIN_NO_ERRORS;
    if not Assigned(OnFunc) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnCalcFunc) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnGetStep) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnSetStep) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnSetNextParam) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnSetFirstParam) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnGetParam) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnSetParam) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
    if not Assigned(OnEndOfCycle) then
    begin Result := MIN_FUNCTION_NOT_ASSIGNED; Exit end;
end;

constructor TMinimizer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FTerminated := False;
end;

end.


