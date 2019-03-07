{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve type.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit IntPointsSet;

{$MODE Delphi}

interface

uses Classes, SysUtils;

type
    TCurveTypeId = TGuid;
    { Interface defining basic operation for creating curve type. }
    IPointsSet = interface
        { Sets unique identifier of curve type. }
        procedure SetCurveTypeId(CurveTypeId: TCurveTypeId);
        { Sets name of curve type. The method is used in deserializing
          objects received from server. }
        procedure SetCurveTypeName(Name: string);
        { Returns unique identifier of curve type. }
        function GetCurveTypeId: TCurveTypeId;
        { Returns name of curve type. It's better to use function
          instead of property because property assumes storing data
          in object, but storing any data is not necessary in this case. }
        function GetCurveTypeName: string;
    end;

implementation

end.


