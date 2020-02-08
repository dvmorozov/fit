unit eta_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter;

type
    { Represents curve width. It can take only positive value. }
    TEtaCurveParameter = class(TSpecialCurveParameter)
    protected
        procedure SetValue(AValue: Double); override;

    public
        constructor Create;
    end;

implementation

constructor TEtaCurveParameter.Create;
begin
    inherited;
    FName := 'eta';
    FValue := 0;
    FType := Variable;
end;

procedure TEtaCurveParameter.SetValue(AValue: Double);
{$IFDEF WRITE_PARAMS_LOG}
var LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := 'SetValue: Name = ' + FName + ', Value = ' + FloatToStr(AValue);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    FValue := Abs(AValue);
    if FValue > 1 then FValue := 1;
end;

end.

