// SPDX-License-Identifier: GPL-3.0-or-later
unit user_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, Math, SimpMath, special_curve_parameter, SysUtils,
    width_curve_parameter;

type
    { Represents parameter of user-defined curve.

      LIMITED BY ITS ROLE, decided with the user: the role the user gives a
      parameter in the curve's dialog says what the quantity is, and it then
      keeps the range the program's own parameter of that role keeps. An
      Amplitude is folded to its magnitude (TAmplitudeCurveParameter); a Width
      is positive and no wider than the fit interval (TWidthCurveParameter) -
      at one unit of x per unit of it, since the program cannot know whether
      the user's formula reads it as a standard deviation or a full width. Any
      other role takes any value: the formula is the user's. }
    TUserCurveParameter = class(TSpecialCurveParameter)
    protected
        { The largest width allowed, for the Width role; +Infinity until a
          window gives one. }
        FWindowCap: double;
        procedure SetValue(AValue: double); override;
    public
        constructor Create;
        procedure CopyTo(const Dest: TSpecialCurveParameter); override;
        procedure LimitToWindow(const AWindow: TCurveWindow); override;
        function GetMinValue: double; override;
        function GetMaxValue: double; override;
        function CreateCopy: TSpecialCurveParameter; override;
        procedure InitVariationStep; override;
        procedure InitValue; override;
        function MinimumStepAchieved: boolean; override;
    end;

implementation

constructor TUserCurveParameter.Create;
begin
    //  Before inherited, which assigns the starting value through SetValue.
    FWindowCap := Infinity;
    inherited;
end;

procedure TUserCurveParameter.SetValue(AValue: double);
begin
    case Type_ of
        Amplitude:
            FValue := Abs(AValue);
        Width:
        begin
            FValue := Abs(AValue);
            if FValue = 0 then
                FValue := TINY;
            if FValue > FWindowCap then
                FValue := FWindowCap;
        end;
    else
        FValue := AValue;
    end;
    WriteValueToLog(AValue);
end;

procedure TUserCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    inherited;
    if Dest is TUserCurveParameter then
        TUserCurveParameter(Dest).FWindowCap := FWindowCap;
end;

procedure TUserCurveParameter.LimitToWindow(const AWindow: TCurveWindow);
begin
    //  Kept whatever the role, so a role given later still finds its window.
    FWindowCap := TWidthCurveParameter.CapFor(
        Abs(AWindow.LastX - AWindow.FirstX), AWindow.FullWidthPerUnit);
    if (Type_ = Width) and (Value > FWindowCap) then
        SetValue(Value);
end;

function TUserCurveParameter.GetMinValue: double;
begin
    case Type_ of
        Amplitude: Result := 0;
        Width: Result := TINY;
    else
        Result := inherited GetMinValue;
    end;
end;

function TUserCurveParameter.GetMaxValue: double;
begin
    if Type_ = Width then
        Result := FWindowCap
    else
        Result := inherited GetMaxValue;
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
