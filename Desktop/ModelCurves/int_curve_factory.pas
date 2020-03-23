{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve instances.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit int_curve_factory;

{$IF NOT DEFINED(FPC)}
{$DEFINE _WINDOWS}
{$ELSEIF DEFINED(WINDOWS)}
{$DEFINE _WINDOWS}
{$ENDIF}

interface

uses named_points_set;

type
    { Class-reference type for base curve type. }
    TCurveClass = class of TNamedPointsSet;

    { Class containing information about curve types. }
    TCurveType = class
    public
        FName:   string;
        FClass:  TCurveClass;
        FTypeId: TCurveTypeId;
        FTag:    integer;
        FExtremumMode: TExtremumMode;
    end;

    { Interface defining basic operation for creating curve instances. }
    ICurveFactory = interface
        ['{a627bac8-86ef-4d43-bcd5-5c2072ca5fc4}']
        //function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
        procedure RegisterCurveType(CurveClass: TCurveClass);
    end;

implementation

end.
