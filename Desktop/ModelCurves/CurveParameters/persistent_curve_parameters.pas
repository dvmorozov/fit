{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definitions of generic container for point set of all calcuated curves.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit persistent_curve_parameters;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    Classes, SysUtils, self_copied_component,
    persistent_curve_parameter_container, special_curve_parameter;

type
    { Curve parameter container. It is used for persistent storage. }
    Curve_parameters = class(TSelfCopiedComponent)
    protected
        FParams: TCollection;

        function GetParameter(Index: LongInt): TSpecialCurveParameter;
        procedure SetParameter(Index: LongInt; Parameter: TSpecialCurveParameter);

        { Returns value of parameter with given name. }
        function GetValueByName(Name: string): Double; virtual;
        procedure SetValueByName(Name: string; Value: Double); virtual;

        function GetCount: LongInt;

    public
        { Initial parameters hash. Should be used for copying optimization. }
        SavedInitHash: Cardinal;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure CopyParameters(const Dest: TObject); override;
        { Parameter names aren't case sensitive. }

        property Parameters[Index: LongInt]: TSpecialCurveParameter
            read GetParameter write SetParameter; default;

        { Provides access to all parameters by name. }
        property ValuesByName[Name: string]: Double
            read GetValueByName write SetValueByName;

        property Count: LongInt read GetCount;

    published
        { Published for XML-serialization. Don't rename. }
        property Params: TCollection read FParams write FParams;
    end;

implementation

function Curve_parameters.GetValueByName(Name: string): Double;
var i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(FParams.Items[i]).Parameter;
        if UpperCase(Parameter.Name) = UpperCase(Name) then
        begin
            Result := Parameter.Value;
            Exit;
        end;
    end;
    Assert(False);
end;

procedure Curve_parameters.SetValueByName(Name: string; Value: Double);
var i: LongInt;
    Parameter: TSpecialCurveParameter;
begin
    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(FParams.Items[i]).Parameter;
        if UpperCase(Parameter.Name) = UpperCase(Name) then
        begin
            Parameter.Value := Value;
            Exit;
        end;
    end;
    Assert(False);
end;

constructor Curve_parameters.Create;
var Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
begin
    inherited;
    FParams := TCollection.Create(TPersistentCurveParameterContainer);
    { Collection should contain at least on item, otherwise is written
      incorrectly. TODO: check it. }
    Parameter := TSpecialCurveParameter.Create;
    Parameter.Name := 'x';
    Parameter.Type_ := Argument;
    Parameter.Value := 0;

    Container := TPersistentCurveParameterContainer(FParams.Add);
    Container.Parameter := Parameter;
end;

destructor Curve_parameters.Destroy;
begin
    FParams.Free;
    inherited;
end;

procedure Curve_parameters.CopyParameters(const Dest: TObject);
var i: LongInt;
    Parameter, NewParameter: TSpecialCurveParameter;
    NewContainer: TPersistentCurveParameterContainer;
begin
    inherited;

    Curve_parameters(Dest).Params.Clear;

    for i := 0 to Count - 1 do
    begin
        Parameter := Parameters[i];

        NewParameter := TSpecialCurveParameter.Create;
        Parameter.CopyTo(NewParameter);

        try
            NewContainer :=
                TPersistentCurveParameterContainer(Curve_parameters(Dest).Params.Add);
        except
            NewParameter.Free;
            raise;
        end;
        NewContainer.Parameter := NewParameter;
    end;
    Curve_parameters(Dest).SavedInitHash := SavedInitHash;
end;

function Curve_parameters.GetParameter(Index: LongInt): TSpecialCurveParameter;
begin
    Assert(Assigned(FParams));
    Result := TPersistentCurveParameterContainer(FParams.Items[Index]).Parameter;
end;

procedure Curve_parameters.SetParameter(Index: LongInt; Parameter: TSpecialCurveParameter);
begin
    Assert(Assigned(FParams));
    TPersistentCurveParameterContainer(FParams.Items[Index]).Parameter := Parameter;
end;

function Curve_parameters.GetCount: LongInt;
begin
    Assert(Assigned(FParams));
    Result := FParams.Count;
end;

begin
end.
