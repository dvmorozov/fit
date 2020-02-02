unit position_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter;

type
    { The abciss coordinate of curve position (middle point). }
    TPositionCurveParameter = class(TSpecialCurveParameter)
    private
        Fx0IsSet: Boolean;
        { X0 variation boundaries. }
        Fx0Low, Fx0High: Double;

    protected
        procedure SetValue(AValue: Double); override;

    public
        constructor Create;
    end;

const
    { The minimal allowed number. }
    MIN_VALUE: Double = -1e100;
    { The maximal allowed number. }
    MAX_VALUE: Double =  1e100;

implementation

constructor TPositionCurveParameter.Create;
begin
    inherited;
    FName := 'x0';
    FValue := 0;
    FType := VariablePosition;
    Fx0IsSet := False;
end;

procedure TPositionCurveParameter.SetValue(AValue: Double);
var i: LongInt;
    TempDouble: Double;
    Highindex: LongInt;
    Lowindex: LongInt;
    P: TSpecialCurveParameter;
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetX0: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    //  nuzhno brat' po modulyu, potomu chto
    //  algoritm optimizatsii mozhet zagonyat'
    //  v oblast' otritsatel'nyh znacheniy
    Value := Abs(Value);
    if not Fx0IsSet then
    begin
        //  pervaya ustanovka parametra
        Fx0IsSet := True;
        P.Value := Value;
        Fx0Low := MIN_VALUE;
        Fx0High := MAX_VALUE;
        Highindex := -1;
        Lowindex := -1;
        //  opredelenie granits variatsii parametra
        for i := 0 to FOwner.PointsCount - 1 do
        begin
            TempDouble := PointXCoord[i];
            if TempDouble < P.Value then
            begin
                if Abs(TempDouble - P.Value) < Abs(Fx0Low - P.Value) then
                    Fx0Low := TempDouble;
                Lowindex := i;
            end;
            if TempDouble > P.Value then
            begin
                if Abs(TempDouble - P.Value) < Abs(Fx0High - P.Value) then
                    Fx0High := TempDouble;
                Highindex := i;
            end;
        end;
        if Lowindex = -1 then Fx0Low := P.Value;
        if Highindex = -1 then Fx0High := P.Value;
    end
    else
    begin
        if Value < Fx0Low then begin P.Value := Fx0Low; Exit end;
        if Value > Fx0High then begin P.Value := Fx0High; Exit end;
        P.Value := Value;
    end;
end;

end.
