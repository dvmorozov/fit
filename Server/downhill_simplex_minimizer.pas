{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of downhill simplex algorithm.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit downhill_simplex_minimizer;

{$MODE Delphi}

interface

uses SysUtils, minimizer, SelfCheckedComponentList, Classes,
    downhill_simplex_container, Tools;

type
	{ Implements downhill simplex algorithm. }
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
    Assert(AValueIndex = 0);
end;

{$hints off}
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
{$hints on}

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



