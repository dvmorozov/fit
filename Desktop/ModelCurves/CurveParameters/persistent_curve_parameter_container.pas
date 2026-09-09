// SPDX-License-Identifier: GPL-3.0-or-later
unit persistent_curve_parameter_container;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, special_curve_parameter, user_curve_parameter, SysUtils, Variants;

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
        procedure SetParameter(AParameter: TSpecialCurveParameter);

    private
        { Streaming halves: the text and its kind can arrive in either order. }
        FPendingValue: string;
        FHasPendingValue: boolean;
        FKind: longint;
        FHasKind: boolean;
        procedure ApplyPending;
        function GetKind: longint;
        procedure SetKind(AKind: longint);

    public
        constructor Create(ACollection: TCollection); override;
        destructor Destroy; override;

        property Parameter: TSpecialCurveParameter
            read FSpecialCurveParameter write SetParameter;

    published
        { Published for XML-serialization. }
        property Name: string read GetName write SetName;
        { String because some problem with XML-serialization as Double. }
        property Value_: string read GetValue write SetValue;
        property Type_: TParameterType read GetType write SetType;
        { The VALUE's data type (a VarType code), as distinct from Type_ above,
          which is the parameter's ROLE (Variable/Calculated/...). Absent in files
          written before this existed; such a file loads numerically, as it did. }
        property Kind: longint read GetKind write SetKind;
    end;

implementation

uses
    checks;

constructor TPersistentCurveParameterContainer.Create(ACollection: TCollection);
begin
    inherited Create(ACollection);
    //  A concrete parameter must exist before the published Name/Value_/Type_
    //  properties are streamed in (they delegate to it). It is replaced when a
    //  real parameter is assigned via the Parameter property.
    FSpecialCurveParameter := TUserCurveParameter.Create;
end;

destructor TPersistentCurveParameterContainer.Destroy;
begin
    FSpecialCurveParameter.Free;
    inherited;
end;

procedure TPersistentCurveParameterContainer.SetParameter(
    AParameter: TSpecialCurveParameter);
begin
    if FSpecialCurveParameter <> AParameter then
    begin
        FSpecialCurveParameter.Free;
        FSpecialCurveParameter := AParameter;
    end;
end;

function TPersistentCurveParameterContainer.GetName: string;
begin
    CheckAssigned(FSpecialCurveParameter, 'the curve parameter this container was created around');

    Result := FSpecialCurveParameter.Name;
end;

procedure TPersistentCurveParameterContainer.SetName(AName: string);
begin
    CheckAssigned(FSpecialCurveParameter, 'the curve parameter this container was created around');

    FSpecialCurveParameter.Name := AName;
end;

function TPersistentCurveParameterContainer.GetValue: string;
begin
    //  Written verbatim. Value_ was ALWAYS a string on disk (see the property
    //  comment: XML serialisation of doubles was the problem), so a non-numeric
    //  value needs no format change - only a way to say what it is, which Kind
    //  below provides.
    Result := VarToStr(FSpecialCurveParameter.TypedValue);
end;

procedure TPersistentCurveParameterContainer.SetValue(AValue: string);
begin
    //  Held until Kind arrives. Published properties stream in declaration order
    //  and Kind is declared after Value_, so the text is parked and converted
    //  once the kind is known - rather than GUESSING from the text, which is what
    //  the previous implementation did and what broke for any text that happens
    //  to look like a number (a wave label "3").
    FPendingValue := AValue;
    FHasPendingValue := True;
    ApplyPending;
end;

function TPersistentCurveParameterContainer.GetKind: longint;
begin
    Result := longint(VarType(FSpecialCurveParameter.TypedValue));
end;

procedure TPersistentCurveParameterContainer.SetKind(AKind: longint);
begin
    FKind := AKind;
    FHasKind := True;
    ApplyPending;
end;

{ Converts the parked text once both halves have arrived.

  Order-independent on purpose: streaming order is a property of the XML writer,
  not something this class should depend on, and an older file has no Kind at all
  - which must still load, as a number, exactly as it always did. }
procedure TPersistentCurveParameterContainer.ApplyPending;
var
    Num: double;
begin
    if not FHasPendingValue then
        Exit;

    //  Any of the string variant types - FPC uses varString for a native string,
    //  varOleStr/varUString elsewhere. Testing one of them only would have let a
    //  label load back as a number on some builds and not others.
    //  Explicit comparisons, NOT a set: varString is 256 and varUString 258,
    //  and a Pascal set constructor silently wraps values above 255 - so
    //  `in [varString, ...]` quietly tested the wrong thing and a label loaded
    //  back as a number.
    if FHasKind and ((FKind = varOleStr) or (FKind = varString) or
                     (FKind = varUString)) then
    begin
        FSpecialCurveParameter.TypedValue := FPendingValue;
        Exit;
    end;

    //  No kind recorded (an older file) or a numeric kind: parse as before.
    if TryStrToFloat(FPendingValue, Num) then
        FSpecialCurveParameter.TypedValue := Num
    else
        //  Unparseable and unlabelled. Keeping the text beats losing it, and
        //  beats raising in the middle of loading a document.
        FSpecialCurveParameter.TypedValue := FPendingValue;
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
