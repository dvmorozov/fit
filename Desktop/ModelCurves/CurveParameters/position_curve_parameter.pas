unit position_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, points_set, special_curve_parameter, SysUtils;

type
    { The abciss coordinate of curve position (middle point). }
    TPositionCurveParameter = class(TSpecialCurveParameter)
    private
        { X0 variation boundaries. }
        Fx0Low, Fx0High: double;

    protected
        procedure SetValue(AValue: double); override;

    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure CopyTo(const Dest: TSpecialCurveParameter); override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
        procedure SetBoundaries(Value: double; PointsSet: TPointsSet);
    end;

implementation

const
    { The minimal allowed number. }
    MIN_VALUE: double = -1e100;
    { The maximal allowed number. }
    MAX_VALUE: double = 1e100;

constructor TPositionCurveParameter.Create;
begin
    inherited Create;
    FName      := 'x0';
    FType      := VariablePosition;
end;

procedure TPositionCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TPositionCurveParameter.InitValue;
begin
    //??? FValue := 0;
end;

function TPositionCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TPositionCurveParameter.Create;
    CopyTo(Result);
end;

procedure TPositionCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    inherited;
    TPositionCurveParameter(Dest).Fx0Low := Fx0Low;
    TPositionCurveParameter(Dest).Fx0High := Fx0High;
end;

procedure TPositionCurveParameter.SetBoundaries(Value: double; PointsSet: TPointsSet);
var
    i: longint;
    TempDouble: double;
    Highindex, Lowindex: longint;
begin
    Fx0Low    := MIN_VALUE;
    Fx0High   := MAX_VALUE;
    Highindex := -1;
    Lowindex  := -1;

    { Searches of curve points closest to the given position
      value and return them as  boundaries of variation. }
    for i := 0 to PointsSet.PointsCount - 1 do
    begin
        TempDouble := PointsSet.PointXCoord[i];
        if TempDouble < Value then
        begin
            if Abs(TempDouble - Value) < Abs(Fx0Low - Value) then
                Fx0Low := TempDouble;
            Lowindex   := i;
        end;
        if TempDouble > Value then
        begin
            if Abs(TempDouble - Value) < Abs(Fx0High - Value) then
                Fx0High := TempDouble;
            Highindex   := i;
        end;
    end;

    if Lowindex = -1 then
        Fx0Low := Value;
    if Highindex = -1 then
        Fx0High := Value;
end;

procedure TPositionCurveParameter.SetValue(AValue: double);
begin
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    FValue := Abs(AValue);

    if FValue < Fx0Low then
    begin
        FValue := Fx0Low;
        Exit;
    end;
    if FValue > Fx0High then
    begin
        FValue := Fx0High;
        Exit;
    end;
    FValue := AValue;

    WriteValueToLog(AValue);
end;

function TPositionCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
