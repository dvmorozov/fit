// SPDX-License-Identifier: GPL-3.0-or-later
unit skew_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, special_curve_parameter, SysUtils;

type
    { A dimensionless skew parameter (the beta of a skewed Gaussian): a free real,
      positive or negative, unbounded - 0 is symmetric. Relies on the base
      unbounded SetValue / GetMinValue / GetMaxValue. }
    TSkewCurveParameter = class(TSpecialCurveParameter)
    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
    end;

implementation

constructor TSkewCurveParameter.Create;
begin
    inherited;
    FName := 'beta';
    FType := Variable;
end;

procedure TSkewCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TSkewCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TSkewCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TSkewCurveParameter.Create;
    CopyTo(Result);
end;

function TSkewCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
