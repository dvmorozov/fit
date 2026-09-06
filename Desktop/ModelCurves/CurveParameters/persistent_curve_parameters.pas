// SPDX-License-Identifier: GPL-3.0-or-later
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
    amplitude_curve_parameter, Classes, curve_instance_id,
    persistent_curve_parameter_container,
    self_copied_component, special_curve_parameter, SysUtils, Variants;

type
    { Curve parameter container. It is used for persistent storage. }
    Curve_parameters = class(TSelfCopiedComponent)
    protected
        FParams: TCollection;

        function GetParameter(Index: longint): TSpecialCurveParameter;
        procedure SetParameter(Index: longint; Parameter: TSpecialCurveParameter);

        { Returns value of parameter with given name. }
        function GetValueByName(Name: string): double; virtual;
        procedure SetValueByName(Name: string; Value: double); virtual;

        function GetTypedByName(Name: string): Variant; virtual;
        procedure SetTypedByName(Name: string; const AValue: Variant); virtual;

        function GetCount: longint;

    public
        { WHICH INSTANCE these values belong to.

          The server rebuilds every curve instance from the picks on each model
          edit, so a set of fitted values outlives the object it was found for.
          This is the handle that says which instance to give it back to -
          issued once, to the pick, and inherited by whatever instance is built
          from it (see curve_identity_registry).

          It replaced a hash of the instance's INITIAL parameter values, which
          was derived rather than issued: it collided on permutations, and it
          changed whenever the seed moved, so moving a pick silently orphaned
          the values stored under it. }
        FInstanceId: TCurveInstanceId;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure CopyParameters(Dest: TObject); override;
        { Parameter names aren't case sensitive. }

        property Parameters[Index: longint]: TSpecialCurveParameter
            read GetParameter write SetParameter; default;

        { The parameter with this name, or NIL when this set has none.

          ValuesByName cannot answer that question: it asserts when the name is
          absent, which is right for a caller that knows the parameter must be
          there and useless for one asking WHETHER it is. Curves of different
          types hold different parameters - a motive pattern has k5 where a
          diagonal has c5, and a corrective one has neither - so anything
          displaying several curves side by side has to ask. }
        function FindByName(const AName: string): TSpecialCurveParameter;

        { Provides access to all parameters by name. }
        property ValuesByName[Name: string]: double
            read GetValueByName write SetValueByName;

        { The same single member as ValuesByName, but WITH its type - for values
          that are not quantities (identity, labels). Not a second storage slot:
          both properties read and write the one value the parameter holds. }
        property TypedByName[Name: string]: Variant
            read GetTypedByName write SetTypedByName;

        property Count: longint read GetCount;

        { The position of the first parameter allowed to vary, or Count when
          none is - a sentinel the callers compare against rather than a value
          to index with.

          IT IS A NAMED OPERATION because the obvious way to write it - a loop
          with a Break, then read the loop variable - answers "none" with a
          variable the language never promised to set, and an empty list never
          assigns it at all. That answer was a large negative number on some
          platforms, which passes an `index < Count` guard and indexes the
          collection from the wrong end. }
        function IndexOfFirstVarying: longint;

    published
        { Published for XML-serialization. Don't rename. }
        property Params: TCollection read FParams write FParams;
    end;

implementation

uses
    checks;

function Curve_parameters.IndexOfFirstVarying: longint;
var
    i: longint;
begin
    Result := Count;
    for i := 0 to Count - 1 do
        if not Parameters[i].VariationDisabled then
        begin
            Result := i;
            Exit;
        end;
end;

function Curve_parameters.GetTypedByName(Name: string): Variant;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
begin
    Result := Null;
    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(FParams.Items[i]).Parameter;
        if UpperCase(Parameter.Name) = UpperCase(Name) then
        begin
            Result := Parameter.TypedValue;
            Exit;
        end;
    end;
    CheckThat(False, Format('this curve has no parameter named "%s" to read a typed value from', [Name]));
end;

procedure Curve_parameters.SetTypedByName(Name: string; const AValue: Variant);
var
    i: longint;
    Parameter: TSpecialCurveParameter;
begin
    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(FParams.Items[i]).Parameter;
        if UpperCase(Parameter.Name) = UpperCase(Name) then
        begin
            Parameter.TypedValue := AValue;
            Exit;
        end;
    end;
    CheckThat(False, Format('this curve has no parameter named "%s" to write a typed value to', [Name]));
end;

function Curve_parameters.FindByName(const AName: string):
    TSpecialCurveParameter;
var
    i: longint;
    Parameter: TSpecialCurveParameter;
begin
    Result := nil;
    for i := 0 to FParams.Count - 1 do
    begin
        Parameter := TPersistentCurveParameterContainer(FParams.Items[i]).Parameter;
        //  Case-insensitively, as the rest of this class matches names.
        if UpperCase(Parameter.Name) = UpperCase(AName) then
            Exit(Parameter);
    end;
end;

function Curve_parameters.GetValueByName(Name: string): double;
var
    i: longint;
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
    CheckThat(False, Format('this curve has no parameter named "%s" to read a value from', [Name]));
end;

procedure Curve_parameters.SetValueByName(Name: string; Value: double);
var
    i: longint;
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
    CheckThat(False, Format('this curve has no parameter named "%s" to write a value to', [Name]));
end;

constructor Curve_parameters.Create;
var
    Parameter: TSpecialCurveParameter;
    Container: TPersistentCurveParameterContainer;
begin
    inherited;
    FParams   := TCollection.Create(TPersistentCurveParameterContainer);
    { Collection should contain at least on item, otherwise is written
      incorrectly. TODO: check it. }
    Parameter := TAmplitudeCurveParameter.Create;
    Parameter.Name := '?';
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

procedure Curve_parameters.CopyParameters(Dest: TObject);
var
    i: longint;
    Parameter, NewParameter: TSpecialCurveParameter;
    NewContainer: TPersistentCurveParameterContainer;
begin
    inherited;

    Curve_parameters(Dest).Params.Clear;

    for i := 0 to Count - 1 do
    begin
        Parameter    := Parameters[i];
        NewParameter := Parameter.CreateCopy;

        try
            NewContainer :=
                TPersistentCurveParameterContainer(Curve_parameters(Dest).Params.Add);
        except
            NewParameter.Free;
            raise;
        end;
        NewContainer.Parameter := NewParameter;
    end;
    Curve_parameters(Dest).FInstanceId := FInstanceId;
end;

function Curve_parameters.GetParameter(Index: longint): TSpecialCurveParameter;
begin
    CheckAssigned(FParams, 'the collection holding this curve''s parameters');

    Result := TPersistentCurveParameterContainer(FParams.Items[Index]).Parameter;
end;

procedure Curve_parameters.SetParameter(Index: longint;
    Parameter: TSpecialCurveParameter);
begin
    CheckAssigned(FParams, 'the collection holding this curve''s parameters');

    TPersistentCurveParameterContainer(FParams.Items[Index]).Parameter := Parameter;
end;

function Curve_parameters.GetCount: longint;
begin
    CheckAssigned(FParams, 'the collection holding this curve''s parameters');

    Result := FParams.Count;
end;

begin
end.
