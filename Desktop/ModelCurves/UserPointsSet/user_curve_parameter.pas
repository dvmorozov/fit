unit user_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils, special_curve_parameter, log;

type
    { Represents parameter of user-defined curve. }
    TUserCurveParameter = class(TSpecialCurveParameter)
    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
    end;

implementation

constructor TUserCurveParameter.Create;
begin
    inherited;
end;

procedure TUserCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TUserCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TUserCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TUserCurveParameter.Create;
    CopyTo(Result);
end;

function TUserCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := True;
end;

end.
