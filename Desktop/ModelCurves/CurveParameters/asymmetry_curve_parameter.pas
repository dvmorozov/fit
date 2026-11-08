// SPDX-License-Identifier: GPL-3.0-or-later
unit asymmetry_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, special_curve_parameter, SysUtils;

type
    { A line-asymmetry parameter in [0, 1] (e.g. the Doniach-Sunjic singularity
      index alpha): 0 is symmetric, larger values skew the tail. Clamped to
      [0, 1] like eta, but named 'alpha' so the formula reads conventionally. }
    TAsymmetryCurveParameter = class(TSpecialCurveParameter)
    protected
        procedure SetValue(AValue: double); override;

    public
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
        function GetMinValue: double; override;
        function GetMaxValue: double; override;
    end;

implementation

constructor TAsymmetryCurveParameter.Create;
begin
    inherited;
    FName := 'alpha';
    FType := Variable;
end;

procedure TAsymmetryCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.05;
end;

procedure TAsymmetryCurveParameter.InitValue;
begin
    //  Start symmetric; the optimizer skews as the data require.
    FValue := 0.1;
end;

function TAsymmetryCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TAsymmetryCurveParameter.Create;
    CopyTo(Result);
end;

procedure TAsymmetryCurveParameter.SetValue(AValue: double);
begin
    FValue := Abs(AValue);
    if FValue > 1 then
        FValue := 1;
    WriteValueToLog(AValue);
end;

function TAsymmetryCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

function TAsymmetryCurveParameter.GetMinValue: double;
begin
    Result := 0;
end;

function TAsymmetryCurveParameter.GetMaxValue: double;
begin
    Result := 1;
end;

end.
