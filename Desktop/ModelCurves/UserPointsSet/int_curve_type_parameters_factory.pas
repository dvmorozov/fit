{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating custom curve type object.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_curve_type_parameters_factory;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses
    app_settings, persistent_curve_parameters;

type
    { Interface defining operation for creating custom curve type object. }
    ICurveTypeParametersFactory = interface
        ['{3cb72d49-6e45-44b9-a200-edd50e945d02}']
        function CreateUserCurveType(Name: string; Expression: string;
            Parameters: Curve_parameters): Curve_type;
    end;

implementation

end.
