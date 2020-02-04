unit sigma_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, SimpMath, special_curve_parameter;

type
    { Represents curve width. It can take only positive value. }
    TSigmaCurveParameter = class(TSpecialCurveParameter)
    protected
        procedure SetValue(AValue: Double); override;

    public
        constructor Create;
    end;

implementation

constructor TSigmaCurveParameter.Create;
begin
    inherited;
    FName := 'sigma';
    FValue := 0.25;
    FType := Variable;
end;

procedure TSigmaCurveParameter.SetValue(AValue: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := 'SetValue: Name = ' + FName + ', Value = ' + FloatToStr(AValue);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    FValue := Abs(AValue);
    if FValue = 0 then FValue := TINY;
end;

end.

