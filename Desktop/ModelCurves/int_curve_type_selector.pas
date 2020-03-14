{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for selecting curve type.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_curve_type_selector;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses named_points_set;

type
    { Interface defining basic operation for selecting curve type. }
    ICurveTypeSelector = interface
        ['{307b3235-97a7-45bb-a6cd-ec7b5e503e51}']
        procedure SelectCurveType(TypeId: TCurveTypeId);
        function GetSelectedCurveType: TCurveTypeId;
        function GetSelectedExtremumMode: TExtremumMode;
    end;

implementation

end.


