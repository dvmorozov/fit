//      dvoynoy kosoy chertoy kommentiruyutsya zamechaniya, sohranyaemye vo
//      vseh versiyah ishodnika; figurnymi skobkami kommentiruyutsya zamechaniya,
//      sohranyaemye tol'ko v versii ishodnika dlya besplatnogo rasprostraneniya
{------------------------------------------------------------------------------}
{       Copyright (C) 1999-2007 D.Morozov (dvmorozov@mail.ru)                  }
{------------------------------------------------------------------------------}
unit Minimizer_DS;

{$MODE Delphi}

interface

uses Minimizer, MSCRDataClasses, SelfCheckedComponentList, Classes,
    DownhillSimplexContainer, Tools, Algorithm, DownhillSimplexAlgorithm(*,
    Windows  ??? *);

type
    // perehodnik dlya podklyucheniya algoritma k dannoy programme
    TDownhillSimplexMinimizer = class(TMinimizer,
        IOptimizedFunction, IDownhillRealParameters, IUpdatingResults)
    private
        Container: TDownhillSimplexContainer;
        
    protected
        procedure SetTerminated(ATerminated: Boolean); override;

    public
        procedure Minimize(var ErrorCode: LongInt); override;

        {IOptimizedFunction}
        function GetOptimizedFunction: Double;

        {IDownhillRealParameters}
        procedure CreateParameters;
        procedure ParametersUpdated;

        {IDownhillSimplexParameters}
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        {IDiscretValue}
        function GetNumberOfValues: LongInt;
        function GetValueIndex: LongInt;
        procedure SetValueIndex(const AValueIndex: LongInt);

        {IUpdatingResults}
        procedure ShowCurJobProgress(Sender: TComponent;
            MinValue, MaxValue, CurValue: LongInt);
        procedure ResetCurJobProgress(Sender: TComponent);
        procedure ShowMessage(Sender: TComponent; Msg: string);
        procedure UpdatingResults(Sender: TComponent);

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
    end;

procedure Register;

implementation

procedure Register;
begin
end;

{========================== TDownhillSimplexMinimizer =========================}
procedure TDownhillSimplexMinimizer.Minimize(var ErrorCode: LongInt);
begin
    ErrorCode := IsReady;
    Terminated := False;
    if ErrorCode <> MIN_NO_ERRORS then Exit;

    with Container do Run;
end;

{IOptimizedFunction}
function TDownhillSimplexMinimizer.GetOptimizedFunction: Double;
begin
    OnCalcFunc;
    Result := OnFunc;
end;

{IDownhillRealParameters}
procedure TDownhillSimplexMinimizer.CreateParameters;
begin
end;

procedure TDownhillSimplexMinimizer.ParametersUpdated;
begin
end;

{IDownhillSimplexParameters}
function TDownhillSimplexMinimizer.GetParametersNumber: LongInt;
begin
    Result := 0;
    OnSetFirstParam;
    while not OnEndOfCycle do
    begin
        Inc(Result);
        OnSetNextParam;
    end;
end;

function TDownhillSimplexMinimizer.GetParameter(
    index: LongInt): TVariableParameter;
var i: LongInt;
begin
    i := 0;
    OnSetFirstParam;
    while not OnEndOfCycle do
    begin
        if i = index then Break;
        Inc(i);
        OnSetNextParam;
    end;
    Result.Limited := False;
    Result.Value := OnGetParam;
end;

procedure TDownhillSimplexMinimizer.SetParameter(
    index: LongInt; AParameter: TVariableParameter);
var i: LongInt;
begin
    i := 0;
    OnSetFirstParam;
    while not OnEndOfCycle do
    begin
        if i = index then Break;
        Inc(i);
        OnSetNextParam;
    end;
    OnSetParam(AParameter.Value);
end;

{IDiscretValue}
function TDownhillSimplexMinimizer.GetNumberOfValues: LongInt;
begin
    //  minimal'noe chislo znacheniy d.b. = 1
    Result := 1;
end;

function TDownhillSimplexMinimizer.GetValueIndex: LongInt;
begin
    Result := 0;
end;

procedure TDownhillSimplexMinimizer.SetValueIndex(const AValueIndex: LongInt);
begin
end;

procedure TDownhillSimplexMinimizer.ShowCurJobProgress(Sender: TComponent;
    MinValue, MaxValue, CurValue: LongInt);
begin
end;

procedure TDownhillSimplexMinimizer.ResetCurJobProgress(Sender: TComponent);
begin
end;

procedure TDownhillSimplexMinimizer.ShowMessage(Sender: TComponent; Msg: string);
begin
end;

procedure TDownhillSimplexMinimizer.UpdatingResults(Sender: TComponent);
begin
    if Assigned(OnShowCurMin) then
    begin
        CurrentMinimum := Container.TotalMinimum;
        OnShowCurMin;
    end;
end;

procedure TDownhillSimplexMinimizer.SetTerminated(ATerminated: Boolean);
begin
    inherited;
    if ATerminated then Container.StopAlgorithm;
end;

constructor TDownhillSimplexMinimizer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Container := TDownhillSimplexContainer.Create(nil);
    Container.UpdatingResults := Self;
    Container.OptimizedFunction := Self;
    //Container.FinalTolerance := 0.1;//0.5; //???
    Container.RestartDisabled := True;
    Container.AddIDSPToList(Self);
end;

destructor TDownhillSimplexMinimizer.Destroy;
begin
    UtilizeObject(Container);
    inherited Destroy;
end;

end.



