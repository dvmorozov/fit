{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of class implementing ICurveTypeParametersFactory.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit curve_type_parameters_factory;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    app_settings, Classes, int_curve_type_parameters_factory,
    persistent_curve_parameters, SysUtils;

type
{$warnings off}
    { Class-factory implementing operation for creating custom curve type object. }
    TCurveTypeParametersFactory = class(TInterfacedObject,
        ICurveTypeParametersFactory)
    private
        constructor Init;

    public
        class function Create: ICurveTypeParametersFactory;
        function CreateUserCurveType(Name: string; Expression: string;
            Parameters: Curve_parameters): Curve_type;
    end;

{$warnings on}

implementation

{ Class members aren't supported by Lazarus 0.9.24, global variable are used instead. }
var
    CurveTypeParametersFactory: TCurveTypeParametersFactory;

constructor TCurveTypeParametersFactory.Init;
begin
    inherited;
end;

class function TCurveTypeParametersFactory.Create: ICurveTypeParametersFactory;
begin
    Result := ICurveTypeParametersFactory(CurveTypeParametersFactory);
end;

function TCurveTypeParametersFactory.CreateUserCurveType(Name: string;
    Expression: string; Parameters: Curve_parameters): Curve_type;
begin
    Result      := Curve_type.Create(nil);
    Result.Name := Name;
    Result.Expression := Expression;
    Result.Parameters := Parameters;
end;

initialization
    CurveTypeParametersFactory := TCurveTypeParametersFactory.Init;

finalization
    CurveTypeParametersFactory.Free;

end.
