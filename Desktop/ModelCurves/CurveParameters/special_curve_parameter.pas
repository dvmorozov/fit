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
    TSpecialCurveParameter = class(TCollectionItem)
    private
        FName: string;
        FValue: Double;
        FType: TParameterType;
        FVariationDisabled: Boolean;
        FVariationStep: Double;

        FSavedValue: Double;

        function GetValue_: string;
        procedure SetValue_(AValue: string);

    public
        constructor Create(Collection: TCollection); override;
        procedure CopyTo(const Dest: TSpecialCurveParameter);

        property SavedValue: Double read FSavedValue write FSavedValue;
        property Value: Double read FValue write FValue;
        property VariationDisabled: Boolean
            read FVariationDisabled write FVariationDisabled;
        property VariationStep: Double
            read FVariationStep write FVariationStep;

    published
        { Published for XML-serialization. }
        property Name: string read FName write FName;
        { String because some problem with XML-serialization as Double. }
        property Value_: string read GetValue_ write SetValue_;
        property Type_: TParameterType read FType write FType;
    end;

implementation

constructor TSpecialCurveParameter.Create(Collection: TCollection);
begin
    inherited;
    FType := Calculated;
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

function TSpecialCurveParameter.GetValue_: string;
begin
    Result := FloatToStr(FValue);
end;

procedure TSpecialCurveParameter.SetValue_(AValue: string);
begin
    FValue := StrToFloat(AValue);
end;

end.

