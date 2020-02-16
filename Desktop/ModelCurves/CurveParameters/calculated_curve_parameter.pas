unit calculated_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter;

type
    { Represents calculated parameter. }
    TCalculatedCurveParameter = class(TSpecialCurveParameter)
    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;

        function MinimumStepAchieved(): Boolean; override;
    end;

implementation

constructor TCalculatedCurveParameter.Create;
begin
    inherited;
    FType := Calculated;
end;

procedure TCalculatedCurveParameter.InitVariationStep;
begin
    FVariationStep := 0;
end;

procedure TCalculatedCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TCalculatedCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TCalculatedCurveParameter.Create;
    CopyTo(Result);
end;

function TCalculatedCurveParameter.MinimumStepAchieved(): Boolean;
begin
    Result := True;
end;

end.

