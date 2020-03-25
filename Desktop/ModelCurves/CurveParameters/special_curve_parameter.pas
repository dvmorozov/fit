unit special_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, log, SysUtils;

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
        VariablePosition
        );

    { Represents parameter of curve point set. It could be variable or not,
      depending on selected type. }
    TSpecialCurveParameter = class(TObject)
    protected
        FName:  string;
        FValue: double;
        FType:  TParameterType;
        FVariationDisabled: boolean;
        FVariationStep: double;

        FSavedValue: double;

        { These methods are overriden in descendant classes
          to provide some special computation on parameter values. }

        function GetValue: double; virtual;
        procedure SetValue(AValue: double); virtual;
        procedure WriteValueToLog(AValue: double);

    public
        constructor Create;
        procedure CopyTo(const Dest: TSpecialCurveParameter); virtual;

        { Must be abstract because only instances of descendant classes could
          be created. https://github.com/dvmorozov/fit/issues/143 }
        function CreateCopy: TSpecialCurveParameter; virtual; abstract;
        procedure InitVariationStep; virtual; abstract;
        procedure InitValue; virtual; abstract;
        function MinimumStepAchieved: boolean; virtual; abstract;

        procedure MultiplyVariationStep(Factor: double);

        property SavedValue: double read FSavedValue write FSavedValue;
        property Value: double read GetValue write SetValue;
        property VariationDisabled: boolean
            read FVariationDisabled write FVariationDisabled;
        property VariationStep: double read FVariationStep write FVariationStep;

        property Name: string read FName write FName;
        property Type_: TParameterType read FType write FType;
    end;

var
    WriteParamsLog: boolean = False;

implementation

constructor TSpecialCurveParameter.Create;
begin
    inherited Create;
    FType := Calculated;
    InitValue;
    InitVariationStep;
end;

procedure TSpecialCurveParameter.CopyTo(const Dest: TSpecialCurveParameter);
begin
    Dest.Name  := Name;
    Dest.Value := Value;
    Dest.Type_ := Type_;
    Dest.SavedValue := SavedValue;
    Dest.VariationDisabled := VariationDisabled;
    Dest.VariationStep := VariationStep;
end;

function TSpecialCurveParameter.GetValue: double;
begin
    Result := FValue;
end;

procedure TSpecialCurveParameter.SetValue(AValue: double);
begin
    FValue := AValue;
    WriteValueToLog(AValue);
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
