{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve instances.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit int_curve_factory;

interface

uses int_points_set, named_points_set;

type
    { Interface defining basic operation for creating curve instances. }
    ICurveFactory = interface
        function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
    end;

implementation

end.


