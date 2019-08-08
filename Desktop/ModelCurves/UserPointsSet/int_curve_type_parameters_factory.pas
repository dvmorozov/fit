{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating custom curve type object.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit int_curve_type_parameters_factory;

{$MODE Delphi}

interface

uses curve_points_set, app_settings;

type
    { Interface defining operation for creating custom curve type object. }
    ICurveTypeParametersFactory = interface
        function CreateUserCurveType(Name: string;
            Expression: string; Parameters: Curve_parameters): Curve_type;
    end;

implementation

end.


