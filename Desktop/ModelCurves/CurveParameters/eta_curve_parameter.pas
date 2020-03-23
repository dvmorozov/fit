unit eta_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter, log;

type
    { Represents curve width. It can take only positive value. }
    TEtaCurveParameter = class(TSpecialCurveParameter)
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

constructor TEtaCurveParameter.Create;
begin
    inherited;
    FName := 'eta';
    FType := Variable;
end;

procedure TEtaCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TEtaCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TEtaCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TEtaCurveParameter.Create;
    CopyTo(Result);
end;

procedure TEtaCurveParameter.SetValue(AValue: double);
begin
    FValue := Abs(AValue);
    if FValue > 1 then
        FValue := 1;
    WriteValueToLog(AValue);
end;

function TEtaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
