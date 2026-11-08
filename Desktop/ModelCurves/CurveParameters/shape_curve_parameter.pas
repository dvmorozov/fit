// SPDX-License-Identifier: GPL-3.0-or-later
unit shape_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, special_curve_parameter, SysUtils;

type
    { A dimensionless shape/exponent parameter (e.g. Pearson VII's m, Moffat's
      beta): a free positive variable with no upper bound. Floored just above 0
      so an exponent like 2^(1/m) or a power (...)^m stays finite while the
      optimizer probes small values. }
    TShapeCurveParameter = class(TSpecialCurveParameter)
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

const
    { Smallest shape value; keeps 1/m and (...)^m finite. }
    ShapeMin: double = 0.1;

implementation

constructor TShapeCurveParameter.Create;
begin
    inherited;
    FName := 'm';
    FType := Variable;
end;

procedure TShapeCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TShapeCurveParameter.InitValue;
begin
    //  Between the Lorentzian limit (m = 1) and the Gaussian limit (m -> inf).
    FValue := 1.5;
end;

function TShapeCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TShapeCurveParameter.Create;
    CopyTo(Result);
end;

procedure TShapeCurveParameter.SetValue(AValue: double);
begin
    FValue := Abs(AValue);
    if FValue < ShapeMin then
        FValue := ShapeMin;
    WriteValueToLog(AValue);
end;

function TShapeCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

function TShapeCurveParameter.GetMinValue: double;
begin
    Result := ShapeMin;
end;

end.
