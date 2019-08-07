{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for storing parameters of custom curve type.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit int_curve_type_storage;

{$MODE Delphi}

interface

uses Settings;

type
    { Interface defining basic operation for storing parameters of custom curve type. }
    ICurveTypeStorage = interface
        procedure AddCurveType(CurveType: Curve_type);
        procedure UpdateCurveType(CurveType: Curve_type);
        procedure DeleteCurveType(CurveType: Curve_type);
    end;

implementation

end.


