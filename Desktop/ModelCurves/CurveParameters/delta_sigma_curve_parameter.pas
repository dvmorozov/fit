unit delta_sigma_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter, log;

type
    { Represents curve width. }
    TDeltaSigmaCurveParameter = class(TSpecialCurveParameter)
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

constructor TDeltaSigmaCurveParameter.Create;
begin
    inherited;
    FName := 'deltasigma';
    FType := Variable;
end;

procedure TDeltaSigmaCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TDeltaSigmaCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TDeltaSigmaCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TDeltaSigmaCurveParameter.Create;
    CopyTo(Result);
end;

procedure TDeltaSigmaCurveParameter.SetValue(AValue: double);
begin
    FValue := AValue;
    WriteValueToLog(AValue);
end;

function TDeltaSigmaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
