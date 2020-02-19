unit sigma_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, SimpMath, special_curve_parameter, log;

type
    { Represents curve width. It can take only positive value. }
    TSigmaCurveParameter = class(TSpecialCurveParameter)
    protected
        procedure SetValue(AValue: Double); override;

    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: Boolean; override;
    end;

implementation

constructor TSigmaCurveParameter.Create;
begin
    inherited;
    FName := 'sigma';
    FType := Variable;
end;

procedure TSigmaCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TSigmaCurveParameter.InitValue;
begin
    FValue := 0.25;
end;

function TSigmaCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TSigmaCurveParameter.Create;
    CopyTo(Result);
end;

procedure TSigmaCurveParameter.SetValue(AValue: Double);
{$IFDEF WRITE_PARAMS_LOG}
var
    LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := 'SetValue: Name = ' + FName + ', Value = ' + FloatToStr(AValue);
    WriteLog(LogStr, Notification);
{$ENDIF}
    FValue := Abs(AValue);
    if FValue = 0 then FValue := TINY;
end;

function TSigmaCurveParameter.MinimumStepAchieved: Boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.

