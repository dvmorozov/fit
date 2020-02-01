unit amplitude_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter;

type
    { Represents curve amplitude (highest value).
      It can take only positive value. }
    TAmplitudeCurveParameter = class(TSpecialCurveParameter)
    protected
        procedure SetValue(AValue: Double); override;

    public
        constructor Create;
    end;

implementation

constructor TAmplitudeCurveParameter.Create;
begin
    inherited;
    FName := 'A';
    FValue := 0;
    FType := Variable;
end;

procedure TAmplitudeCurveParameter.SetValue(AValue: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := ' SetA: Value = ' + FloatToStr(Value);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    FValue := Abs(AValue);
end;

end.

