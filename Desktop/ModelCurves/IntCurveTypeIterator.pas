{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for iterating through curve types.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit IntCurveTypeIterator;

{$MODE Delphi}

interface

uses IntPointsSet;

type
    { Interface defining basic operation for iterating through curve types. }
    ICurveTypeIterator = interface
        procedure FirstCurveType;
        procedure NextCurveType;
        function EndCurveType: Boolean;
        function GetCurveTypeName: string;
        function GetCurveTypeId: TCurveTypeId;
    end;

implementation

end.


