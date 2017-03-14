//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
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

    IMinimizer = interface(IUnknown)
        function  Func: Double;
        procedure CalcFunc;
        function  GetStep: Double;
        procedure SetStep(NewStepValue: Double);
        procedure SetNextParam;
        procedure SetFirstParam;
        function  GetParam: Double;
        procedure SetParam(NewParamValue: Double);
        function  EndOfCycle: Boolean;
    end;

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

    // realizuet intenrfeys dlya minimizatsii lyubyh funktsiy
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


