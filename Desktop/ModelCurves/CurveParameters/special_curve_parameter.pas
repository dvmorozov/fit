// SPDX-License-Identifier: GPL-3.0-or-later
unit special_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, Math, SysUtils, Variants;

type
    TParameterType = (
        { Created by user and non-variable. Such parameters are variated together for all
        instances in the given interval if it isn't disabled by special flag. }
        Shared,
        { Created by user and variable. }
        Variable,
        { Created by the application. }
        Calculated,
        { Argument of expression. Always variable. }
        Argument,
        { Non-variable parameter describing instance position. }
        InvariablePosition,
        { Variable parameter describing instance position. }
        VariablePosition,
        { Variable parameter designated (by role, not by name) as the peak
          amplitude - initialised from the data peak. }
        Amplitude,
        { Variable parameter designated as the peak width - initialised from the
          fitting interval. }
        Width
        );

    { Represents parameter of curve point set. It could be variable or not,
      depending on selected type. }
    TSpecialCurveParameter = class(TObject)
    protected
        FName:  string;
        { THE parameter's value - one member, carrying its own type.

          Variant, because a parameter may hold a quantity (every ordinary
          parameter), an identity, or a label, and the reader must be able to tell
          which without guessing. An earlier attempt bolted a second string field
          on beside a double: a parameter then had TWO values that could disagree,
          no single source of truth, and a parse-the-string guess on load to
          decide which one applied. The class contract has always been one value.

          `Value: double` below remains the ordinary accessor and is what the
          optimiser uses, so nothing about the numeric path changes. }
        FValue: Variant;
        FType:  TParameterType;
        FVariationDisabled: boolean;
        FVariationStep: double;

        FSavedValue: double;
        { Estimated standard error of the fitted value; < 0 when unavailable
          (the native engine does not estimate it; the Python backend does). }
        FError: double;


        { These methods are overriden in descendant classes
          to provide some special computation on parameter values. }

        function GetValue: double; virtual;
        procedure SetValue(AValue: double); virtual;
        procedure SetTypedValue(const AValue: Variant); virtual;
        procedure WriteValueToLog(AValue: double);

    public
        constructor Create;
        procedure CopyTo(const Dest: TSpecialCurveParameter); virtual;

        { The value with its type - the same single member `Value` reads, not a
          second one. Use this for anything that is not a quantity (identity,
          labels); use Value for numbers. Only Calculated parameters may hold a
          non-numeric value: the optimiser varies doubles. }
        property TypedValue: Variant read FValue write SetTypedValue;
        { True when the value is not a number, so a caller can tell without
          converting. }
        function IsNumeric: boolean;

        { Must be abstract because only instances of descendant classes could
          be created. https://github.com/dvmorozov/fit/issues/143 }
        function CreateCopy: TSpecialCurveParameter; virtual; abstract;
        procedure InitVariationStep; virtual; abstract;
        procedure InitValue; virtual; abstract;
        function MinimumStepAchieved: boolean; virtual; abstract;

        { The physical bounds this parameter enforces (via SetValue clamping),
          surfaced so a bounded backend fit (the Python engine) stays in the same
          feasible region the native engine does. Defaults are unbounded; the
          clamped parameters (amplitude, sigma, eta) override them. }
        function GetMinValue: double; virtual;
        function GetMaxValue: double; virtual;

        procedure MultiplyVariationStep(Factor: double);

        property SavedValue: double read FSavedValue write FSavedValue;
        property Value: double read GetValue write SetValue;
        property VariationDisabled: boolean
            read FVariationDisabled write FVariationDisabled;
        property VariationStep: double read FVariationStep write FVariationStep;

        property Name: string read FName write FName;
        property Type_: TParameterType read FType write FType;
        { Estimated standard error; < 0 = not estimated. }
        property Error: double read FError write FError;
    end;

var
    WriteParamsLog: boolean = False;

implementation

constructor TSpecialCurveParameter.Create;
begin
    inherited Create;
    //  A parameter is numeric until told otherwise. Without this the Variant
    //  would start Unassigned, and VarIsNumeric would report a brand-new
    //  parameter as non-numeric.
    FValue := 0.0;
    FType := Calculated;
    FError := -1;   //  no estimate until a backend supplies one
    InitValue;
    InitVariationStep;
end;

function TSpecialCurveParameter.GetMinValue: double;
begin
    Result := NegInfinity;
end;

function TSpecialCurveParameter.GetMaxValue: double;
begin
    Result := Infinity;
end;

procedure TSpecialCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    Dest.Name  := Name;
    //  The TYPED value, so a copy of an identity stays an identity rather than
    //  being flattened to a number. Faithful by design: a caller needing fresh
    //  identity mints it after copying.
    Dest.TypedValue := TypedValue;
    Dest.Type_ := Type_;
    Dest.SavedValue := SavedValue;
    Dest.VariationDisabled := VariationDisabled;
    Dest.VariationStep := VariationStep;
    Dest.Error := Error;
end;

function TSpecialCurveParameter.GetValue: double;
begin
    //  A non-numeric parameter has no meaningful number, and returning a
    //  conversion error mid-fit would be worse than returning nothing: the
    //  optimiser never varies such a parameter (they are Calculated), so 0 is
    //  never actually used as a quantity.
    if VarIsNumeric(FValue) then
        Result := FValue
    else
        Result := 0;
end;

procedure TSpecialCurveParameter.SetValue(AValue: double);
begin
    FValue := AValue;
    WriteValueToLog(AValue);
end;

procedure TSpecialCurveParameter.SetTypedValue(const AValue: Variant);
begin
    FValue := AValue;
    if VarIsNumeric(AValue) then
        WriteValueToLog(AValue);
end;

function TSpecialCurveParameter.IsNumeric: boolean;
begin
    Result := VarIsNumeric(FValue);
end;

procedure TSpecialCurveParameter.WriteValueToLog(AValue: double);
var
    LogStr: string;
begin
    if WriteParamsLog then
    begin
        LogStr :=
            'Set value: Name = ' + FName + ', Original value = ' +
            FloatToStr(AValue) + ', Assigned value = ' + FloatToStr(FValue);
        WriteLog(LogStr, Notification);
    end;
end;

procedure TSpecialCurveParameter.MultiplyVariationStep(Factor: double);
begin
    FVariationStep := FVariationStep * Factor;
end;

end.
