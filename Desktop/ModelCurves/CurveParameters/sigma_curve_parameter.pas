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
        procedure SetValue(AValue: double); override;

    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
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

procedure TSigmaCurveParameter.SetValue(AValue: double);
begin
    FValue := Abs(AValue);
    if FValue = 0 then
        FValue := TINY;
    WriteValueToLog(AValue);
end;

function TSigmaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
