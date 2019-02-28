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
    TCurveTypesSingleton = class(
        ICurveFactory, ICurveTypeIterator, ICurveTypeSelector)
    private
        class FCurveTypesSingleton: TCurveTypesSingleton;
        constructor Init;

    public
        class function Create: TCurveTypesSingleton;
        procedure RegitsterCurveType(CurveType: TNamedPointsSet);
        { Implementation of ICurveFactory. }
        function CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
        { Implementation of ICurveTypeIterator. }
        procedure FirstType;
        procedure NextType;
        function EndType: Boolean;
        function GetTypeName: string;
        function GetTypeId: TCurveTypeId;
        { Implementation of ICurveTypeSelector. }
        procedure SelectType(TypeId: TCurveTypeId);
        function GetSelectedType: TCurveTypeId;
    end;

implementation

constructor TCurveTypesSingleton.Init;
begin
    inherited Create;
end;

class function TCurveTypesSingleton.Create: TCurveTypesSingleton;
begin
    if FCurveTypesSingleton = nil then
      FCurveTypesSingleton := TCurveTypesSingleton.Init;
    Result := FCurveTypesSingleton;
end;

function TCurveTypesSingleton.CreatePointsSet(TypeId: TCurveTypeId): TNamedPointsSet;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.CreatePointsSet not implemented.');
end;

procedure TCurveTypesSingleton.RegitsterCurveType(CurveType: TNamedPointsSet);
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.RegitsterCurveType not implemented.');
end;

procedure TCurveTypesSingleton.FirstType;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.FirstType not implemented.');
end;

procedure TCurveTypesSingleton.NextType;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.NextType not implemented.');
end;

function TCurveTypesSingleton.EndType: Boolean;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.EndType not implemented.');
end;

function TCurveTypesSingleton.GetTypeName: string;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.GetTypeName not implemented.');
end;

function TCurveTypesSingleton.GetTypeId: TCurveTypeId;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.GetTypeId not implemented.');
end;

procedure TCurveTypesSingleton.SelectType(TypeId: TCurveTypeId);
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.SelectType not implemented.');
end;

function TCurveTypesSingleton.GetSelectedType: TCurveTypeId;
begin
    raise ENotImplemented.Create('TCurveTypesSingleton.GetSelectedType not implemented.');
end;

end.


