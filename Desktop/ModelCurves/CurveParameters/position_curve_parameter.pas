unit position_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, points_set, special_curve_parameter;

type
    { The abciss coordinate of curve position (middle point). }
    TPositionCurveParameter = class(TSpecialCurveParameter)
    private
        Fx0IsSet: Boolean;
        { X0 variation boundaries. }
        Fx0Low, Fx0High: Double;
        FPointsSet: TPointsSet;

    protected
        procedure SetValue(AValue: Double); override;

    public
        constructor Create(APointsSet: TPointsSet);
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: Boolean; override;
    end;

const
    { The minimal allowed number. }
    MIN_VALUE: Double = -1e100;
    { The maximal allowed number. }
    MAX_VALUE: Double =  1e100;

implementation

constructor TPositionCurveParameter.Create;
begin
    inherited Create;
    FName := 'x0';
    FType := VariablePosition;
    Fx0IsSet := False;
    FPointsSet := APointsSet;
end;

procedure TPositionCurveParameter.InitVariationStep;
begin
   FVariationStep := 0.01;
end;

procedure TPositionCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TPositionCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TPositionCurveParameter.Create(FPointsSet);
    CopyTo(Result);
end;

procedure TPositionCurveParameter.SetValue(AValue: Double);
var i: LongInt;
    TempDouble: Double;
    Highindex: LongInt;
    Lowindex: LongInt;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := 'SetValue: Name = ' + FName + ', Value = ' + FloatToStr(AValue);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    FValue := Abs(AValue);
    if not Fx0IsSet then
    begin
        //  pervaya ustanovka parametra
        Fx0IsSet := True;
        FValue := AValue;
        Fx0Low := MIN_VALUE;
        Fx0High := MAX_VALUE;
        Highindex := -1;
        Lowindex := -1;
        //  opredelenie granits variatsii parametra
        for i := 0 to FPointsSet.PointsCount - 1 do
        begin
            TempDouble := FPointsSet.PointXCoord[i];
            if TempDouble < FValue then
            begin
                if Abs(TempDouble - FValue) < Abs(Fx0Low - FValue) then
                    Fx0Low := TempDouble;
                Lowindex := i;
            end;
            if TempDouble > FValue then
            begin
                if Abs(TempDouble - FValue) < Abs(Fx0High - FValue) then
                    Fx0High := TempDouble;
                Highindex := i;
            end;
        end;
        if Lowindex = -1 then Fx0Low := FValue;
        if Highindex = -1 then Fx0High := FValue;
    end
    else
    begin
        if FValue < Fx0Low then begin FValue := Fx0Low; Exit end;
        if FValue > Fx0High then begin FValue := Fx0High; Exit end;
        FValue := AValue;
    end;
end;

function TPositionCurveParameter.MinimumStepAchieved: Boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
