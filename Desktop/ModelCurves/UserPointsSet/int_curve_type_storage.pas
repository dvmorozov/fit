{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for storing parameters of custom curve type.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_curve_type_storage;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses app_settings;

type
    { Interface defining basic operation for storing parameters of custom curve type. }
    ICurveTypeStorage = interface
        ['{8a725ed0-9b41-45fa-bcdf-635e21dc88b6}']
        procedure AddCurveType(CurveType: Curve_type);
        procedure UpdateCurveType(CurveType: Curve_type);
        procedure DeleteCurveType(CurveType: Curve_type);
    end;

implementation

end.
