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

        constructor Create; overload;
        procedure SetBoundaries(x0: double; PointsSet: TPointsSet);

    protected
        procedure SetValue(AValue: double); override;

    public
        constructor Create(x0: double; PointsSet: TPointsSet); overload;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure CopyTo(const Dest: TSpecialCurveParameter); override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;

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
    Fx0Low     := MIN_VALUE;
    Fx0High    := MAX_VALUE;
end;

constructor TPositionCurveParameter.Create(x0: double; PointsSet: TPointsSet);
begin
    inherited Create;
    FName      := 'x0';
    FType      := VariablePosition;
    SetBoundaries(x0, PointsSet);
end;

procedure TPositionCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TPositionCurveParameter.InitValue;
begin
    FValue := 0;
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

procedure TPositionCurveParameter.SetBoundaries(x0: double; PointsSet: TPointsSet);
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
      x0 and return them as  boundaries of variation. }
    for i := 0 to PointsSet.PointsCount - 1 do
    begin
        TempDouble := PointsSet.PointXCoord[i];
        if TempDouble < x0 then
        begin
            if Abs(TempDouble - x0) < Abs(Fx0Low - x0) then
                Fx0Low := TempDouble;
            Lowindex   := i;
        end;
        if TempDouble > x0 then
        begin
            if Abs(TempDouble - x0) < Abs(Fx0High - x0) then
                Fx0High := TempDouble;
            Highindex   := i;
        end;
    end;

    if Lowindex = -1 then
        Fx0Low := x0;
    if Highindex = -1 then
        Fx0High := x0;
end;

procedure TPositionCurveParameter.SetValue(AValue: double);
begin
    FValue := AValue;
    { Checks boundary conditions. }
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
