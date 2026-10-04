// SPDX-License-Identifier: GPL-3.0-or-later
unit gamma_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, SimpMath, special_curve_parameter, SysUtils;

type
    { The Lorentzian half-width (gamma) of a Voigt profile: strictly positive,
      floored just above 0. }
    TGammaCurveParameter = class(TSpecialCurveParameter)
    protected
        procedure SetValue(AValue: double); override;

    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
        function GetMinValue: double; override;
    end;

implementation

constructor TGammaCurveParameter.Create;
begin
    inherited;
    FName := 'gamma';
    FType := Variable;
end;

procedure TGammaCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TGammaCurveParameter.InitValue;
begin
    FValue := 0.25;
end;

function TGammaCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TGammaCurveParameter.Create;
    CopyTo(Result);
end;

procedure TGammaCurveParameter.SetValue(AValue: double);
begin
    FValue := Abs(AValue);
    if FValue = 0 then
        FValue := TINY;
    WriteValueToLog(AValue);
end;

function TGammaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

function TGammaCurveParameter.GetMinValue: double;
begin
    Result := TINY;
end;

end.
