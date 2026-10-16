// SPDX-License-Identifier: GPL-3.0-or-later
unit gamma_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, SimpMath, special_curve_parameter, SysUtils,
    width_curve_parameter;

type
    { The Lorentzian half-width (gamma) of a Voigt profile: strictly positive,
      floored just above 0 and capped by the fit interval, as every width is
      (width_curve_parameter). }
    TGammaCurveParameter = class(TWidthCurveParameter)
    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
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

function TGammaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
