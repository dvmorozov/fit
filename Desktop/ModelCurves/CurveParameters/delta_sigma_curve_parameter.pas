// SPDX-License-Identifier: GPL-3.0-or-later
unit delta_sigma_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, Math, special_curve_parameter, SysUtils,
    width_curve_parameter;

type
    { The DIFFERENCE of a curve's two side widths (sigma - deltasigma left,
      sigma + deltasigma right): signed, and once its curve has a fit interval,
      no larger either way than a width may be there - past it, one side is
      wider than the interval (width_curve_parameter). }
    TDeltaSigmaCurveParameter = class(TSpecialCurveParameter)
    protected
        { The largest magnitude allowed; +Infinity until a window gives one. }
        FWindowCap: double;
        procedure SetValue(AValue: double); override;

    public
        procedure CopyTo(const Dest: TSpecialCurveParameter); override;
        procedure LimitToWindow(const AWindow: TCurveWindow); override;
        function GetMinValue: double; override;
        function GetMaxValue: double; override;
        constructor Create;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
    end;

implementation

constructor TDeltaSigmaCurveParameter.Create;
begin
    //  Before inherited, which assigns the starting value through SetValue.
    FWindowCap := Infinity;
    inherited;
    FName := 'deltasigma';
    FType := Variable;
end;

procedure TDeltaSigmaCurveParameter.InitVariationStep;
begin
    FVariationStep := 0.1;
end;

procedure TDeltaSigmaCurveParameter.InitValue;
begin
    FValue := 0;
end;

function TDeltaSigmaCurveParameter.CreateCopy: TSpecialCurveParameter;
begin
    Result := TDeltaSigmaCurveParameter.Create;
    CopyTo(Result);
end;

procedure TDeltaSigmaCurveParameter.SetValue(AValue: double);
begin
    FValue := EnsureRange(AValue, -FWindowCap, FWindowCap);
    WriteValueToLog(AValue);
end;

procedure TDeltaSigmaCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    inherited;
    if Dest is TDeltaSigmaCurveParameter then
        TDeltaSigmaCurveParameter(Dest).FWindowCap := FWindowCap;
end;

procedure TDeltaSigmaCurveParameter.LimitToWindow(const AWindow: TCurveWindow);
begin
    FWindowCap := TWidthCurveParameter.CapFor(
        Abs(AWindow.LastX - AWindow.FirstX), AWindow.FullWidthPerUnit);
    SetValue(Value);
end;

function TDeltaSigmaCurveParameter.GetMinValue: double;
begin
    Result := -FWindowCap;
end;

function TDeltaSigmaCurveParameter.GetMaxValue: double;
begin
    Result := FWindowCap;
end;

function TDeltaSigmaCurveParameter.MinimumStepAchieved: boolean;
begin
    Result := FVariationStep < 0.00001;
end;

end.
