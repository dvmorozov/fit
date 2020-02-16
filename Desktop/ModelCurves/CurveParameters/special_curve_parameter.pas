unit special_curve_parameter;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses Classes, SysUtils;

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
        FName: string;
        FValue: Double;
        FType: TParameterType;
        FVariationDisabled: Boolean;
        FVariationStep: Double;

        FSavedValue: Double;

        { These methods are overriden in descendant classes
          to provide some special computation on parameter values. }

        function GetValue: Double; virtual;
        procedure SetValue(AValue: Double); virtual;

    public
        constructor Create;
        procedure CopyTo(const Dest: TSpecialCurveParameter); virtual;

        { Must be abstract because only instances of descendant classes could
          be created. https://github.com/dvmorozov/fit/issues/143 }
        function CreateCopy: TSpecialCurveParameter; virtual; abstract;
        procedure InitVariationStep; virtual; abstract;
        procedure InitValue; virtual; abstract;
        function MinimumStepAchieved: Boolean; virtual; abstract;

        procedure MultiplyVariationStep(Factor: Double);

        property SavedValue: Double read FSavedValue write FSavedValue;
        property Value: Double read GetValue write SetValue;
        property VariationDisabled: Boolean
            read FVariationDisabled write FVariationDisabled;
        property VariationStep: Double
            read FVariationStep write FVariationStep;

        property Name: string read FName write FName;
        property Type_: TParameterType read FType write FType;
    end;

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
    Dest.Name := Name;
    Dest.Value := Value;
    Dest.Type_ := Type_;
    Dest.SavedValue := SavedValue;
    Dest.VariationDisabled := VariationDisabled;
    Dest.VariationStep := VariationStep;
end;

function TSpecialCurveParameter.GetValue: Double;
begin
    Result := FValue;
end;

procedure TSpecialCurveParameter.SetValue(AValue: Double);
{$IFDEF WRITE_PARAMS_LOG}
    LogStr: string;
{$ENDIF}
begin
{$IFDEF WRITE_PARAMS_LOG}
    LogStr := 'SetValue: Name = ' + FName + ', Value = ' + FloatToStr(AValue);
    WriteLog(LogStr, Notification_);
{$ENDIF}
    FValue := AValue;
end;

procedure TSpecialCurveParameter.MultiplyVariationStep(Factor: Double);
begin
    FVariationStep := FVariationStep * Factor;
end;

end.
