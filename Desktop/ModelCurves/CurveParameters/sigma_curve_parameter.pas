// SPDX-License-Identifier: GPL-3.0-or-later
unit sigma_curve_parameter;

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
    { Represents curve width: positive, and no wider than the fit interval
      the curve is fitted in (width_curve_parameter). }
    TSigmaCurveParameter = class(TWidthCurveParameter)
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

function TSigmaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
