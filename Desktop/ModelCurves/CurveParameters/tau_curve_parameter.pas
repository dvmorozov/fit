// SPDX-License-Identifier: GPL-3.0-or-later
unit tau_curve_parameter;

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
    { An exponential-relaxation time (the tau of an exponentially modified
      Gaussian): strictly positive, floored just above 0 so 1/tau stays finite,
      and capped by the fit interval as every width is - a tail is a width on
      one side (width_curve_parameter). }
    TTauCurveParameter = class(TWidthCurveParameter)
    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
    end;

implementation

constructor TTauCurveParameter.Create;
begin
    inherited;
    FName := 'tau';
    FType := Variable;
end;

procedure TTauCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TTauCurveParameter.InitValue;
begin
    FValue := 1.0;
end;

function TTauCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TTauCurveParameter.Create;
    CopyTo(Result);
end;

function TTauCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
