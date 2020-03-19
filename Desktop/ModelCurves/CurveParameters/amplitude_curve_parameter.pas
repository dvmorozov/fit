unit amplitude_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter, log;

type
    { Represents curve amplitude (highest value).
      It can take only positive value. }
    TAmplitudeCurveParameter = class(TSpecialCurveParameter)
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

constructor TAmplitudeCurveParameter.Create;
begin
    inherited;
    FName := 'A';
    FType := Variable;
end;

procedure TAmplitudeCurveParameter.InitVariationStep;
begin
    FVariationStep := 100;
end;

procedure TAmplitudeCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TAmplitudeCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TAmplitudeCurveParameter.Create;
    CopyTo(Result);
end;

procedure TAmplitudeCurveParameter.SetValue(AValue: Double);
begin
    FValue := Abs(AValue);
    WriteValueToLog(AValue);
end;

function TAmplitudeCurveParameter.MinimumStepAchieved: Boolean;
begin
    Result := FVariationStep < 0.0001;
end;

end.
