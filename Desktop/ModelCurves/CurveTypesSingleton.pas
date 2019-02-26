{
This software is distributed under GPL
in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the warranty of FITNESS FOR A PARTICULAR PURPOSE.

@abstract(Contains definition of interface for creating curve instances.)

@author(Dmitry Morozov dvmorozov@hotmail.com, 
LinkedIn https://ru.linkedin.com/pub/dmitry-morozov/59/90a/794, 
Facebook https://www.facebook.com/profile.php?id=100004082021870)
}
unit CurveTypesSingleton;

{$MODE Delphi}

interface

uses Classes, SysUtils, NamedPointsSet, IntCurveFactory, IntCurveTypeSelector,
  IntCurveTypeIterator;

type
    { Class-singleton containing information about curve types. }
    TCurveTypesSingleton = class
    private
        FCurveTypesSingleton: TCurveTypesSingleton;
        constructor Init;

    public
        class function Create: TCurveTypesSingleton;
    end;

implementation

var CurveTypesSingleton : TCurveTypesSingleton = nil;

constructor TCurveTypesSingleton.Init;
begin
    inherited Create;
end;

class function TCurveTypesSingleton.Create: TCurveTypesSingleton;
begin
    if CurveTypesSingleton = nil then
      CurveTypesSingleton := TCurveTypesSingleton.Init;
    Result := CurveTypesSingleton;
end;

end.


