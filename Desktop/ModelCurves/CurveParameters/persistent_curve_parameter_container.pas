unit persistent_curve_parameter_container;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, SysUtils, special_curve_parameter;

type
    { An item of TCollection, it is used for persistent storaging of
      parameter attributes. }
    TPersistentCurveParameterContainer = class(TCollectionItem)
    private
        FSpecialCurveParameter: TSpecialCurveParameter;

        function GetName: string;
        procedure SetName(AName: string);
        function GetValue: string;
        procedure SetValue(AValue: string);
        function GetType: TParameterType;
        procedure SetType(AType: TParameterType);

    public
        destructor Destroy; override;

        property Parameter: TSpecialCurveParameter
            read FSpecialCurveParameter write FSpecialCurveParameter;

    published
        { Published for XML-serialization. }
        property Name: string read GetName write SetName;
        { String because some problem with XML-serialization as Double. }
        property Value_: string read GetValue write SetValue;
        property Type_: TParameterType read GetType write SetType;
    end;

implementation

destructor TPersistentCurveParameterContainer.Destroy;
begin
    if Assigned(FSpecialCurveParameter) then
        FSpecialCurveParameter.Free;
    inherited;
end;

function TPersistentCurveParameterContainer.GetName: string;
begin
    Assert(Assigned(FSpecialCurveParameter));
    Result := FSpecialCurveParameter.Name;
end;

procedure TPersistentCurveParameterContainer.SetName(AName: string);
begin
    Assert(Assigned(FSpecialCurveParameter));
    FSpecialCurveParameter.Name := AName;
end;

function TPersistentCurveParameterContainer.GetValue: string;
begin
    Result := FloatToStr(FSpecialCurveParameter.Value);
end;

procedure TPersistentCurveParameterContainer.SetValue(AValue: string);
begin
    FSpecialCurveParameter.Value := StrToFloat(AValue);
end;

function TPersistentCurveParameterContainer.GetType: TParameterType;
begin
    Result := FSpecialCurveParameter.Type_;
end;

procedure TPersistentCurveParameterContainer.SetType(AType: TParameterType);
begin
    FSpecialCurveParameter.Type_ := AType;
end;

end.
